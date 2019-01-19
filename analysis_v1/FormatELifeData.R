library(plyr)
library(readxl)
library(WriteXLS)

article_history <- read_excel("~/Dropbox/eLife/OldData/Data20171015/A_eLife_query_Article History 2012_2017 v2.xls", skip = 3)
authors <- read_excel("~/Dropbox/eLife/OldData/Data20171015/B_eLife_query_Authors 2012_2017 v2.xls", skip = 3)
reviewers <- read_excel("~/Dropbox/eLife/OldData/Data20171015/C_eLife_query_Reviewers 2012_2017 v2.xls", skip = 3)
bres <- read_excel("~/Dropbox/eLife/OldData/Data20171015/D_eLife_query_BREs 2012_2017 v2.xls", skip = 3)


vincent_gender <- read_delim("~/Dropbox/eLife/OldData/Gender_Country.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
# We need to modify the raw country data, fixing inconsistencies. Fortunately, the number of mismatched countries
# is small enough that we can do it manually here in this script
vincent_gender$Name <- tolower(vincent_gender$Name)
vincent_gender <- vincent_gender[!duplicated(vincent_gender[1:2]), ]

# Function takes as input list of names and gender table to match against
# returns list of assigned genders, based on first name

# Function takes as input list of names and gender table to match against
# returns list of assigned genders, based on first name
assign_gender <- function(data, name_var, country_var, gender_table, new_gender_name) {
  countries <- data[[country_var]]
  # first we need to match the countries to make this work
  countries[countries == "Viet Nam"] <- "Vietnam"
  countries[countries == "Iran, Islamic Republic of"] <- "Iran"
  countries[countries == "Slovak Republic"] <- "Slovakia"
  countries[countries == "Russian Federation"] <- "Russia"
  countries[countries == "Burkina Faso"] <- "Burkina-Faso"
  countries[countries == "Bangladesh"] <- "Bangledesh"
  countries[countries == "French Guiana"] <- "French-Guiana"
  countries[countries == "French Polynesia"] <- "French-Polynesia"
  countries[countries == "Republic of Korea"] <- "South Korea"
  countries[countries == "Korea (South), Republic of"] <- "South Korea"
  countries[countries == "Hong Kong"] <- "Hong-Kong" 
  countries[countries == "Serbia and Montenegro"] <- "Serbie" 
  countries[countries == "Taiwan, Republic of China"] <- "Taiwan"
  countries[countries == "Puerto Rico"] <- "United States"
  countries[is.na(countries)] <- "Unknown"
  
  # take the first token of the names
  given = tolower(sapply(data[[name_var]], function(x) {
    return(unlist(strsplit(x, " "))[1])
  }))
  
  data$temp_country <- countries
  data$temp_name <- given
  
  merged <- merge(data, 
                  gender_table, 
                  by.x = c("temp_name", "temp_country"),
                  by.y = c("Name", "Country"), 
                  all.x = T
                  )
  
  unmatched <- is.na(merged$Gender)
  replacement <- (match(merged$temp_name[unmatched], gender_table$Name))
  merged[unmatched, "Gender"] <- gender_table[replacement, "Gender"]
  merged$Gender <- toupper(merged$Gender)
  
  merged[[new_gender_name]] <- merged$Gender
  merged <- merged %>% select(-temp_country, -temp_name, -Gender)
  
  return(merged)
}


#################
# First, we need to work with the article history data, and get it into a nice format
# First, the column names need some work, to be more descriptive and easier to work with.
colnames(article_history) <- c("MSNO", "type", 
                               "ca_name", "ca_institution", "ca_country", 
                               "se_name", "se_institution", "se_country",
                               "initial_submission_time", "initial_decision_time", "initial_decision",
                               "full_submission_date", "appeal_state1", "full_decision_date", "full_decision",
                               "rev1_submission_date", "appeal_state2", "rev1_decision_date", "rev1_decision",
                               "rev2_submission_date", "appeal_state3", "rev2_decision_date", "rev2_decision",
                               "deltatime_initial_submission_decision", "deltatime_full_submission_decision",
                               "deltatime_initial_submission_to_accept", "deltatime_full_submission_to_accept")

# Now lets simplify some of the values in each of these columns

article_history$initial_decision <- factor(revalue(article_history$initial_decision, 
                                                c("Encourage Full Submission"="Encourage", 
                                                  "Reject Initial Submission"="Reject", 
                                                  "Reject Full Submission"="Reject")))

article_history$full_decision <- factor(revalue(article_history$full_decision, 
                                         c("Reject Full Submission"="Reject", 
                                           "Revise Full Submission"="Revise", 
                                           "Accept Full Submission"="Accept")))

article_history$rev1_decision <- factor(revalue(article_history$rev1_decision, 
                                                c("Reject Full Submission"="Reject", 
                                                  "Revise Full Submission"="Revise", 
                                                  "Accept Full Submission"="Accept")))

article_history$rev2_decision <- factor(revalue(article_history$rev2_decision, 
                                                c("Reject Full Submission"="Reject", 
                                                  "Revise Full Submission"="Revise", 
                                                  "Accept Full Submission"="Accept")))

# Just if it was appealed at any point
article_history$appealed_any_stage <- with(article_history, appeal_state1 == 4 | appeal_state2 == 4 | appeal_state3 == 4)
article_history$appealed_any_stage <- !is.na(article_history$appealed_any_stage)

# Simple value just to show whether or not the document was accepted in the end
article_history$accepted <- !is.na(article_history$deltatime_initial_submission_to_accept)

article_history <- assign_gender(article_history, "ca_name", "ca_country", vincent_gender, "ca_gender")
article_history <- assign_gender(article_history, "se_name", "se_country", vincent_gender, "se_gender")



dates <- as.Date(article_history$initial_submission_time)
article_history$submission_year <- as.numeric(format(dates, "%Y"))
article_history$submission_month <- as.numeric(format(dates, "%m"))

###################
# Now we need to work with the authors file
colnames(authors) <- c("MSNO", "type",
                       "ca_name", "ca_institution", "ca_country",
                       "fa_name", "fa_institution", "fa_country",
                       "la_name", "la_institution", "la_country")

authors <- assign_gender(authors, "ca_name", "ca_country", vincent_gender, "ca_gender")
authors <- assign_gender(authors, "fa_name", "fa_country", vincent_gender, "fa_gender")
authors <- assign_gender(authors, "la_name", "la_country", vincent_gender, "la_gender")

# Lets also add some little binary vairbales to help filter cases where an author is filling multiple of these roles
authors$single_authored <- authors$fa_name == authors$la_name
authors$ca_is_first <- authors$ca_name == authors$fa_name
authors$ca_is_last <- authors$ca_name == authors$la_name

##################
# Next step, we will take a swing at the reviewers file, which should be fairly simple??
colnames(reviewers) <- c("MSNO",
                         "reviewer1_name", "reviewer1_institution", "reviewer1_country",
                         "reviewer2_name", "reviewer2_institution", "reviewer2_country",
                         "reviewer3_name", "reviewer3_institution", "reviewer3_country",
                         "reviewer4_name", "reviewer4_institution", "reviewer4_country")

reviewers <- assign_gender(reviewers, "reviewer1_name", "reviewer1_country", vincent_gender, "reviewer1_gender")
reviewers <- assign_gender(reviewers, "reviewer2_name", "reviewer2_country", vincent_gender, "reviewer2_gender")
reviewers <- assign_gender(reviewers, "reviewer3_name", "reviewer3_country", vincent_gender, "reviewer3_gender")
reviewers <- assign_gender(reviewers, "reviewer4_name", "reviewer4_country", vincent_gender, "reviewer4_gender")

# lest add a variable for the number of reviewers involved in the project
reviewers$num_reviewers <- with(reviewers, 4 - is.na(reviewer1_name) - is.na(reviewer2_name) - is.na(reviewer3_name) - is.na(reviewer4_name))

# Lets add a value for the number of known men and women among the reviewers, as a proportion of the total 
reviewers$prop_male_reviewers <- with(reviewers, ((reviewer1_gender == "M") + (reviewer2_gender == "M") + (reviewer3_gender == "M") + (reviewer4_gender == "M")) / num_reviewers)
reviewers$prop_female_reviewers <- with(reviewers, ((reviewer1_gender == "F") + (reviewer2_gender == "F") + (reviewer3_gender == "F") + (reviewer4_gender == "F")) / num_reviewers)


###################
# And finally, lets work with the BREs table
colnames(bres) <- c("MSNO", "bre_name", "bre_institution", "bre_country")
bres <- assign_gender(bres, "bre_name", "bre_country", vincent_gender, "bre_gender")

##########
# Now, merge them all together
merged <- merge(article_history, authors[-c(3:5, 12)], by=c("MSNO"), all.x=TRUE)
merged <- merge(merged, reviewers, by="MSNO", all.x=TRUE)
merged <- merge(merged, bres, by.x="MSNO", by.y = "MSNO", all.x=TRUE)

merged <- merged[!duplicated(merged$MSNO), ]

WriteXLS(merged, "~/Desktop/elife_datafile.xls")
