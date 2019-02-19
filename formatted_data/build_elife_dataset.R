# Load required libraries
library(geosphere)
library(tidyverse)
library(readr)
library(readxl)


############################################
# 
# Setup
#
############################################


#
# Define save paths
#
# First, the path to save the "people" file, contianing info on name/role combinations in the datase
path_to_save_people = "~/Dropbox/eLife/elife-analysis/formatted_data/elife_people.csv"

# Save the non-anonymized version of the data
path_to_save_non_anonymous = "~/Dropbox/eLife/data/non_anonymous_data.csv"

# Save the anaonymized version of the data
path_to_save_anonymous = "~/Dropbox/eLife/elife-analysis/formatted_data/formatted_elife.csv"

#
# Define load paths
#
# Load the main eLife data file. This is private, so the path has to be on your local machine
path_to_raw_elife_data = "~/Dropbox/eLife/data/elife_datafile.xlsx"

# This is the dropbox path to the institutional metadata
path_to_institution_data = "https://www.dropbox.com/s/5pcbzhb3v9prwhu/elife_institutions.csv?dl=1"

# And this is the dropbox path to the country-level metadata
path_to_country_data = "https://www.dropbox.com/s/o2t6724ugp8dzev/country_data_coords.csv?dl=1"


#
# Load the data
#
# Load the eLife data
elife <- read_excel(path_to_raw_elife_data)

# Load the country and institutional data infomration .csv file, which was manually created and available along with this file
institution_mapping <- read_csv(path_to_institution_data)[, 2:3]
country_data <- read_csv(path_to_country_data)

# Change the naems of the country metadata file to something a little more easy to work with. 
country_mapping <- country_data %>% select(Country, continent, name) %>% rename(Mapping = name, Continent = continent)


############################################
# 
# eLife Person-level dataset
#
############################################

# Get a single list of all the peer reviewers, which presently are represented as separate variables on each row. This method
# is ugly and can be improved, but it is simple and works, so I won't bother changing it
reviewer1 <- elife[, c("reviewer1_name", "reviewer1_gender", "reviewer1_institution", "reviewer1_country")]
reviewer1 <- reviewer1[with(reviewer1, !is.na(reviewer1_name)), ]
colnames(reviewer1) <- c("reviewer_name", "reviewer_gender", "reviewer_institution", "reviewer_country")

reviewer2 <- elife[, c("reviewer2_name", "reviewer2_gender", "reviewer2_institution", "reviewer2_country")]
reviewer2 <- reviewer2[with(reviewer2, !is.na(reviewer2_name)), ]
colnames(reviewer2) <- c("reviewer_name", "reviewer_gender", "reviewer_institution", "reviewer_country")

reviewer3 <- elife[, c("reviewer3_name", "reviewer3_gender", "reviewer3_institution", "reviewer3_country")]
reviewer3 <- reviewer2[with(reviewer3, !is.na(reviewer3_name)), ]
colnames(reviewer3) <- c("reviewer_name", "reviewer_gender", "reviewer_institution", "reviewer_country")

reviewer4 <- elife[, c("reviewer4_name", "reviewer4_gender", "reviewer4_institution", "reviewer4_country")]
reviewer4 <- reviewer2[with(reviewer4, !is.na(reviewer4_name)), ]
colnames(reviewer4) <- c("reviewer_name", "reviewer_gender", "reviewer_institution", "reviewer_country")

# Combine all of these reviewer datasets into one
reviewers <- rbind(reviewer1, reviewer2, reviewer3, reviewer4)

# proportion of reviewers by gender compared to proportions of author by gender
bres <- elife[, c("bre_gender", "bre_name", "bre_country")]
peer_reviewers <- reviewers[, c("reviewer_gender", "reviewer_name", "reviewer_country")]
editors <- elife[, c("se_gender", "se_name", "se_country")]
names(editors) <- c("gender", "name", "country")
names(bres) <- c("gender", "name", "country")
names(peer_reviewers) <- c("gender", "name", "country")

revs <- as.data.frame(rbind(bres, editors, peer_reviewers))
revs$type <- "Gatekeeper"
revs <- revs[!is.na(revs$country), ]

# now we add the authors
# First, add the information for the corresponding author
ca_authors <- elife[, c("ca_gender", "ca_name", "ca_country")]
names(ca_authors) <- c("gender", "name", "country")
ca_authors$type <- "Corr. Author"

# Then, the first authors
fa_authors <- elife[, c("fa_gender", "fa_name", "fa_country")]
names(fa_authors) <- c("gender", "name", "country")
fa_authors$type <- "First Author"

# And finally, the last authors
la_authors <- elife[, c("la_gender", "la_name", "la_country")]
names(la_authors) <- c("gender", "name", "country")
la_authors$type <- "Last Author"

# combine all these variables into a single data frame
people <- as.data.frame(rbind(revs, ca_authors, fa_authors, la_authors))
people <- people[!duplicated(people[, c("name", "country", "type")]), ]
people <- people[!is.na(people$name), ]

# Adjust the levels of the gender factor, limit to "Male/Female/UNK", the matching algorithm will assign naems as "unisex", which we can't do anything with. 
people$gender = with(people, relevel(factor(ifelse(gender %in% c("M", "F"), gender, 
                                                   ifelse(!is.na(name), "UNK", NA)), levels = c("M", "F", "UNK")), ref = "F"))

#
# Map the continent values onto this dataframe
#
people <- people %>%
  mutate(country = tolower(country)) %>%
  left_join(country_mapping, by = c("country" = "Country")) %>%
  mutate(
    country = Mapping,
    continent = Continent
  ) %>%
  select(-Mapping, -Continent, -name, -country)

#
# Now save...
#
write.csv(people, path_to_save_people)


############################################
# 
# eLife Submission-Level dataset
#
############################################

# Moving on, lets begin to work with the manuscript data. 
# The first thing that we will do is process the country names, and add the mapping information for the countries of each of the authors. 
elife_with_country_mapping <- elife %>%
  # This group_by is needed to make the mutate functions work propoerly
  group_by(MSNO) %>%
  # these vairbales need to be converted to lowercase for the next step...
  mutate(
    ca_country = tolower(ca_country),
    la_country = tolower(la_country),
    fa_country = tolower(fa_country),
    bre_country = tolower(bre_country),
    se_country = tolower(se_country),
    reviewer1_country = tolower(reviewer1_country),
    reviewer2_country = tolower(reviewer2_country),
    reviewer3_country = tolower(reviewer3_country),
    reviewer4_country = tolower(reviewer4_country)
  ) %>%
  # This series of left_joins + mutates + selects is ugly, but it seems one of the most straightforward ways of adding the country mapping values for 
  # each of the author and reviewer types. I am sure that there is some solution using gathers, but I was having difficulty
  left_join(country_mapping, by = c("ca_country" = "Country")) %>%
  mutate(
    ca_country = Mapping,
    ca_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("la_country" = "Country")) %>%
  mutate(
    la_country = Mapping,
    la_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("fa_country" = "Country")) %>%
  mutate(
    fa_country = Mapping,
    fa_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("se_country" = "Country")) %>%
  mutate(
    se_country = Mapping,
    se_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("bre_country" = "Country")) %>%
  mutate(
    bre_country = Mapping,
    bre_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("reviewer1_country" = "Country")) %>%
  mutate(
    reviewer1_country = Mapping,
    reviewer1_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("reviewer2_country" = "Country")) %>%
  mutate(
    reviewer2_country = Mapping,
    reviewer2_continent = Continent
  ) %>%
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("reviewer3_country" = "Country")) %>%
  mutate(
    reviewer3_country = Mapping,
    reviewer3_continent = Continent
  ) %>% 
  select(-Mapping, -Continent) %>%
  left_join(country_mapping, by = c("reviewer4_country" = "Country")) %>%
  mutate(
    reviewer4_country = Mapping,
    reviewer4_continent = Continent
  ) %>%
  select(-Mapping, -Continent)
  
# Then, we can repeat a similar process, but this time mapping the institutions rankings for 
# each author and reviewer role
elife_with_institition_rank <- elife_with_country_mapping %>%
  left_join(institution_mapping, by = c("ca_institution" = "elife_institution")) %>%
  mutate(
    ca_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("fa_institution" = "elife_institution")) %>%
  mutate(
    fa_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("la_institution" = "elife_institution")) %>%
  mutate(
    la_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("se_institution" = "elife_institution")) %>%
  mutate(
    se_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("reviewer1_institution" = "elife_institution")) %>%
  mutate(
    reviewer1_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("reviewer2_institution" = "elife_institution")) %>%
  mutate(
    reviewer2_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("reviewer3_institution" = "elife_institution")) %>%
  mutate(
    reviewer3_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank) %>%
  left_join(institution_mapping, by = c("reviewer4_institution" = "elife_institution")) %>%
  mutate(
    reviewer4_institution_rank = THE_rank
  ) %>% 
  select(-THE_rank)
  # End dplyr 


#
# Next we will modify the gender vairables to be a little more amenable for our analysis
#
elife_releveled_gender <- elife_with_institition_rank %>%
  # Now I will turn towards the other variables that we will be working with. 
  mutate(
    # convert NA assignments in gender assignment to a value of "Unknown" as well as setup factor levels
    ca_gender = relevel(factor(ifelse(ca_gender %in% c("M", "F"), ca_gender, ifelse(!is.na(ca_name), "UNK", NA)), levels = c("M", "F", "UNK")), ref = "F"),
    fa_gender = relevel(factor(ifelse(fa_gender %in% c("M", "F"), fa_gender, ifelse(!is.na(fa_name), "UNK", NA)), levels = c("M", "F", "UNK")), ref = "F"),
    la_gender = relevel(factor(ifelse(la_gender %in% c("M", "F"), la_gender, ifelse(!is.na(la_name), "UNK", NA)), levels = c("M", "F", "UNK")), ref = "F"),
    se_gender = relevel(factor(ifelse(se_gender %in% c("M", "F"), se_gender, ifelse(!is.na(se_name), "UNK", NA)), levels = c("M", "F", "UNK")), ref = "F"),
    bre_gender = relevel(factor(ifelse(se_gender %in% c("M", "F"), bre_gender, ifelse(!is.na(bre_name), "UNK", NA)), 
                                levels = c("M", "F", "UNK")), ref = "F"),
    reviewer1_gender = relevel(factor(ifelse(reviewer1_gender %in% c("M", "F"), reviewer1_gender, ifelse(!is.na(reviewer1_name), "UNK", NA)), 
                                      levels = c("M", "F", "UNK")), ref = "F"),
    reviewer2_gender = relevel(factor(ifelse(reviewer2_gender %in% c("M", "F"), reviewer2_gender, ifelse(!is.na(reviewer2_name), "UNK", NA)), 
                                      levels = c("M", "F", "UNK")), ref = "F"),
    reviewer3_gender = relevel(factor(ifelse(reviewer3_gender %in% c("M", "F"), reviewer3_gender, ifelse(!is.na(reviewer3_name), "UNK", NA)), 
                                      levels = c("M", "F", "UNK")), ref = "F"),
    reviewer4_gender = relevel(factor(ifelse(reviewer4_gender %in% c("M", "F"), reviewer4_gender, ifelse(!is.na(reviewer4_name), "UNK", NA)), 
                                      levels = c("M", "F", "UNK")), ref = "F")
  )
  # End dplyr 

#
# Now we will add infomration related to the submission itself
#
elife_with_submission_info <- elife_releveled_gender %>%
  mutate(
    submission_year = as.numeric(format(as.Date(initial_submission_time), "%Y")),
    # Flag the data if there is an initial submission but no decision has been made
    initial_decision_made = !is.na(initial_decision),
    
    # Flag is the manuscript had a full submission
    has_full_submission = !is.na(full_submission_date),
    
    # Flag data if full submission has been submitted, but no decision has been made (ie: still in revision or decision stage)
    full_decision_made = has_full_submission & dplyr::last(na.omit(c(full_decision, rev1_decision, rev2_decision))) != "Revise",
    
    # Flag the data if it was appealed at any state during its process
    appealed_any_stage = any(c(appeal_state1, appeal_state2, appeal_state3) == 4) & any(!is.na(c(appeal_state1, appeal_state2, appeal_state3))),
    
    # Flag the data if the initial decision of the manuscript was encouraged
    encouraged = initial_decision_made & initial_decision == "Encourage",
    
    # Flag the data if the full submission was eventually accepted
    accepted = full_decision_made & any(c(full_decision, rev1_decision, rev2_decision) == "Accept", na.rm = T),
    
    # Flag whether a final decision of accept or reject has been passed
    final_decision_made = any(c(full_decision, rev1_decision, rev2_decision) %in% c("Accept", "Reject"), na.rm = T),
    
    # Flag data if there are potential inconsistencies with the naming, ie: no last name for a full submittion but there is a first name
    #name_inconsistency = full_decision_made & ((!is.na(la_name) & is.na(elife$fa_name)) | ((is.na(la_name) & !is.na(fa_name)))),
    
    # This is a list of all the full decisions made (first decision or full submission + subsequent revisions)
    full_decisions = list(na.omit(c(full_decision, rev1_decision, rev2_decision))),
    
    # Add variables related to how long it takes to go through the review process
    deltatime_rev1 = as.Date(rev1_decision_date) - as.Date(rev1_submission_date),
    deltatime_rev2 = as.Date(rev2_decision_date) - as.Date(rev2_submission_date),
    
    # Store the final stage of the full-submission process in which the final decision was amde
    final_decision_stage = ifelse(full_decision_made, c("Full", "Rev1", "Rev2")[grep("Accept|Reject", unlist(full_decisions))], "Pending"),
    
    # Calculat the total amount of time spent deliberating revision decisions 
    revision_time = ifelse(final_decision_stage == "Full", NA, 
                           ifelse(final_decision_stage == "Rev1", deltatime_rev1,
                                  deltatime_rev1 + deltatime_rev2)),
    
    # Calculate total amount of time spent deliberating decisions from first full submittion to the final decision
    deltatime_final = ifelse(final_decision_stage == "Full", deltatime_full_submission_decision, 
                             ifelse(final_decision_stage == "Rev1", deltatime_rev1 + deltatime_full_submission_decision, 
                                    deltatime_rev2 + deltatime_rev1 + deltatime_full_submission_decision)),
    
    # just some simple boolean values for whether or not the full submission and decision dates are NA—useful once we scrub dates from analysis
    #!has_full_submission = is.na(full_submission_date),
    full_decision_date_isNA = is.na(full_decision_date),
    
    # Number of revisions
    num_revisions = length(unlist(full_decisions)) - 1
  ) %>% 
  select(-full_decisions)
  # End elife_with_submission_info <- ...

#
# Now we add the reviewer information to the dataset
#
elife_with_reviewer_info <- elife_with_submission_info %>%
  mutate(
    # Now add variables relating to the composition of the reviewer team
    # Quality of list variables—list of the names, countries, and genders of reviewers
    reviewer_names = list(tolower(na.omit(c(bre_name, reviewer1_name, reviewer2_name, reviewer3_name, reviewer4_name)))),
    reviewer_genders = list(na.omit(c(bre_gender, reviewer1_gender, reviewer2_gender, reviewer3_gender, reviewer4_gender))),
    reviewer_countries = list(tolower(na.omit(c(bre_country, reviewer1_country, reviewer2_country, reviewer3_country, reviewer4_country)))),
    reviewer_continents = list(tolower(na.omit(c(bre_continent, reviewer1_continent, reviewer2_continent, reviewer3_continent, reviewer4_continent)))),
    reviewer_institutions = list(tolower(na.omit(c(bre_institution, reviewer1_institution, reviewer2_institution, reviewer3_institution, reviewer4_institution)))),
    
    reviewer_names_nobre = list(tolower(na.omit(c(reviewer1_name, reviewer2_name, reviewer3_name, reviewer4_name)))),
    reviewer_genders_nobre = list(na.omit(c(reviewer1_gender, reviewer2_gender, reviewer3_gender, reviewer4_gender))),
    
    # Number of reviewers in the team
    num_reviewers = length(unlist(reviewer_names)),
    num_reviewers_nobre = length(unlist(reviewer_names_nobre)),
    
    # Number of reviewers on the team identified as female
    num_female_reviewers = sum(unlist(reviewer_genders) == 1),
    # NUmber of reviewers on the team identified as male
    num_male_reviewers = sum(unlist(reviewer_genders) == 2),
    
    num_female_reviewers_nobre = sum(unlist(reviewer_genders_nobre) == 1),
    # NUmber of reviewers on the team identified as male
    num_male_reviewers_nobre = sum(unlist(reviewer_genders_nobre) == 2),
    
    # Composition: three variables indicaitng whether all have a gender identified as male, all have a gender identified as female, or there is a mix
    # of at least one male and one female reviewer on the team
    composition = factor(ifelse(is.na(full_submission_date), NA, 
                                ifelse(all(unlist(reviewer_genders) == 2), "All Men", 
                                       ifelse(all(unlist(reviewer_genders) == 1), "All Women", 
                                              ifelse(any(unlist(reviewer_genders) == 1) & any(unlist(reviewer_genders) == 2), "Mixed", 
                                                     "Uncertain"
                                              )
                                       )
                                )
    ) # end first ifelse
    , levels = c("All Men", "All Women", "Mixed", "Uncertain")), # end factor
    
    # The corresponding author is from the same country as at least one reviewer 
    ca_country_homophily = !is.na(ca_country) & tolower(ca_country) %in% unlist(reviewer_countries),
    # The last author is from the same country as at least one reviewer
    la_country_homophily = !is.na(la_country) & tolower(la_country) %in% unlist(reviewer_countries),
    # The first author is from the same country as at least one reviewer
    fa_country_homophily = !is.na(fa_country) & tolower(fa_country) %in% unlist(reviewer_countries),
    
    # The corresponding author is from the same country as at least one reviewer 
    ca_continent_homophily = !is.na(ca_continent) & tolower(ca_continent) %in% unlist(reviewer_continents),
    # The last author is from the same country as at least one reviewer
    la_continent_homophily = !is.na(la_continent) & tolower(la_continent) %in% unlist(reviewer_continents),
    # The first author is from the same country as at least one reviewer
    fa_continent_homophily = !is.na(fa_continent) & tolower(fa_continent) %in% unlist(reviewer_continents),
    
    # Geographic distance between the country of the corresponding author and the senior editor
    ca_se_dist = distm(c(subset(country_data, name == ca_country)$Longitude[1], subset(country_data, name == ca_country)$Latitude[1]),
                       c(subset(country_data, name == se_country)$Longitude[1], subset(country_data, name == se_country)$Latitude[1]))[1],
    
    la_bre_dist = distm(c(subset(country_data, name == la_country)$Longitude[1], subset(country_data, name == la_country)$Latitude[1]),
                        c(subset(country_data, name == bre_country)$Longitude[1], subset(country_data, name == bre_country)$Latitude[1]))[1],
    
    # sum of geographic distance between country of last author and each of the countries of associated reviewers
    la_reviewer_dist = sum(unlist(sapply(unlist(reviewer_countries), function(rev_country) {
      #print(rev_country)
      if(is.na(rev_country) | is.na(la_country)) return(NA)
      
      la_coords = c(subset(country_data, name == la_country)$Longitude[1], subset(country_data, name == la_country)$Latitude[1])
      rev_coords = c(subset(country_data, tolower(name) == rev_country)$Longitude[1], subset(country_data, tolower(name) == rev_country)$Latitude[1])
      return(distm(la_coords, rev_coords)[1])
    })), na.rm = T),
    
    # Sum of geographic distance between country of first author and each of the countries of associated reviewers
    fa_reviewer_dist = sum(unlist(sapply(unlist(reviewer_countries), function(rev_country) {
      if(is.na(rev_country) | is.na(fa_country)) return(NA)
      
      la_coords = c(subset(country_data, name == fa_country)$Longitude[1], subset(country_data, name == fa_country)$Latitude[1])
      rev_coords = c(subset(country_data, tolower(name) == rev_country)$Longitude[1], subset(country_data, tolower(name) == rev_country)$Latitude[1])
      return(distm(la_coords, rev_coords)[1])
    })), na.rm = T),
    
    # Indicator variables about whether or not any of the above distances are zero
    ca_se_dist_zero = ifelse(is.na(ca_se_dist), NA, ifelse(ca_se_dist == 0, T, F)),
    la_reviewer_dist_zero = ifelse(is.na(la_reviewer_dist), NA, ifelse(la_reviewer_dist == 0, T, F)),
    fa_reviewer_dist_zero = ifelse(is.na(fa_reviewer_dist), NA, ifelse(fa_reviewer_dist == 0, T, F)),
    
    any_reviewer_from_africa = any(unlist(reviewer_continents) == "africa"),
    any_reviewer_from_north_america = any(unlist(reviewer_continents) == "north america"),
    any_reviewer_from_south_america = any(unlist(reviewer_continents) == "south america"),
    any_reviewer_from_europe = any(unlist(reviewer_continents) == "europe"),
    any_reviewer_from_asia = any(unlist(reviewer_continents) == "asia"),
    
    # The BRE is also listed as a reviewer
    bre_is_reviewer = tolower(bre_name) %in% unlist(reviewer_names_nobre),
    
    ca_bre_country_homophily = bre_country == ca_country,
    
    la_bre_country_homophily = bre_country == la_country,
    
    ca_bre_continent_homophily = bre_continent == ca_continent,
    
    la_bre_continent_homophily = bre_continent == la_continent,
    
    # A short series of variables specifying if the first and last authors are the same, or if the CA is also the first/last author
    ca_is_first = tolower(ca_name) == tolower(fa_name),
    ca_is_last = tolower(ca_name) == tolower(la_name),
    single_authored = tolower(fa_name) == tolower(la_name),
    
    # this just cleans up a poorly named variable earlier in the preprocessing step
    submission_type = type.x,
    
    # Recalculate the composition, but this time justed for the BRE
    num_female_reviewers_adj = ifelse(bre_is_reviewer == T & bre_gender == "F", num_female_reviewers_nobre - 1, num_female_reviewers_nobre),
    num_male_reviewers_adj = ifelse(bre_is_reviewer ==T & bre_gender == "M", num_male_reviewers_nobre - 1, num_male_reviewers_nobre),
    num_reviewers_adj = ifelse(bre_is_reviewer, num_reviewers_nobre - 1, num_reviewers_nobre),
    composition_adj = ifelse(!has_full_submission, NA, 
                             ifelse(num_male_reviewers_adj > 0 & num_female_reviewers_adj > 0, "Mixed",
                                    ifelse(num_male_reviewers_adj == num_reviewers_adj, "All Men", 
                                           ifelse(num_female_reviewers_adj == num_reviewers_adj, "All Women", "Uncertain"
                                           )
                                    )
                             )
    )
  ) %>%
  filter(submission_type != "RE") %>%
  select(-c(reviewer_names, reviewer_countries, reviewer_institutions, reviewer_genders, 
            reviewer_continents, reviewer_names_nobre, reviewer_genders_nobre))
  # End elife_with_reviewer_info <- ...

# Now we should save this (non-anonymized) dataset,
write.csv(elife_with_reviewer_info, file = path_to_save_non_anonymous)

############################################
# 
# eLife Anonymize dataset
#
############################################

elife_anonymous <- elife_with_reviewer_info %>%
  # Now lets remove any identifying information, ie: names and institutions. We also remove country/continent for gatekeepers, as the population is smaller
  # We also remove all dates and times, because thees could probably be used to identify information about a particular manuscript
  # Also, lets remove variables that we will not prefently be working with, just to keep the table clean
  select(-c(
    ca_name, la_name, fa_name,  se_name, bre_name, reviewer1_name, reviewer2_name, reviewer3_name, reviewer4_name, 
    ca_institution, la_institution, fa_institution, se_institution, bre_institution, 
    reviewer1_institution, reviewer2_institution, reviewer3_institution, reviewer4_institution,
    se_country, bre_country, reviewer1_country, reviewer2_country, reviewer3_country, reviewer4_country,
    initial_submission_time, full_submission_date, rev1_submission_date, rev2_submission_date,
    initial_decision_time, full_decision_date, rev1_decision_date, rev2_decision_date, submission_month,
    deltatime_initial_submission_decision,	deltatime_full_submission_decision,	deltatime_initial_submission_to_accept,	deltatime_full_submission_to_accept,
    deltatime_rev1,	deltatime_rev2,	revision_time,	deltatime_final,
    prop_male_reviewers, prop_female_reviewers, appeal_state1, appeal_state2, appeal_state3, type.x, type.y
    #reviewer_names, reviewer_countries, reviewer_continents, reviewer_genders, full_decisions, reviewer_institutions,
    #reviewer_names_nobre, reviewer_genders_nobre))
  ))

# write this file as output, so that future analysis can start from this file as a checkpoint
write.csv(elife_anonymous, path_to_save_anonymous)
