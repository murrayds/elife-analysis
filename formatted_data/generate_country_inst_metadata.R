library(tidyverse)
library(broom)
library(data.table)
library(readxl)
library(tm)
library(stringr)
library(fuzzyjoin)



# Load the metadata collected from the following URL
# https://worldmap.harvard.edu/data/geonode:country_centroids_az8
#metadata <- fread)"read.csv("~/Desktop/country_centroids_az8.csv")
metadata = fread("http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0")

metadata$lower_name <- tolower(metadata$name)
country_mapping <- read_csv("~/Dropbox/eLife/data/elife_countries.csv")


new <- country_mapping %>%
  left_join(metadata, by = c("Mapping" = "lower_name")) %>%
  dplyr::select(Country, continent, name, region_un,	subregion,	region_wb, Longitude, Latitude, economy,	income_grp, pop_est) %>%
  filter(!is.na(name))


write.csv(new, "~/Desktop/country_data.csv")


# Still need to fix the main file to get everything loaded properly
# Can we do fuzzy-string matching 
# rankings <- read.delim("~/Dropbox/eLife/elife-analysis/metadata/Shanghai_LIfeScience_2016.csv")
elife <- read_excel("~/Dropbox/eLife/data/elife_datafile.xlsx")

word_cut <- 4
# Normalize university names in eLife and select unique intitution
institutions <- elife %>% 
  select(ca_institution, la_institution, fa_institution, 
         se_institution, bre_institution, reviewer1_institution, 
         reviewer2_institution, reviewer3_institution, reviewer4_institution) %>% 
  gather() %>%
  group_by(value) %>% # Need to add the groupby first, just for the ifelse statemenet below
  mutate(count = n()) %>%
  filter(row_number() == 1) %>%  # remove obvious dupicates
  mutate(
    mapping = value,
    mapping = tolower(mapping), # convert all to lower case
    mapping = removeWords(mapping, c("the", "of", "at", "for", "and", "in")), # remove basic articles
    mapping = iconv(mapping, to="ASCII//TRANSLIT"), # try to replace accented characters as best as possible
    mapping = gsub( "[-—]", " ", mapping), # replace both em-dash and regular dash with a space
    mapping = gsub( "uc ", "university california ", mapping), # common UC abbreviation
    mapping = gsub( "howard hughes medical institute", "", mapping), # common 
    mapping = gsub( "institute", "ins", mapping), # sinplify to univ
    mapping = gsub("ueristes", "u", mapping), # french university title
    mapping = gsub("Universiteit", "u", mapping), #dutch university name
    mapping = gsub( "school", "sch", mapping), # sinplify to univ
    mapping = gsub( "national", "nat", mapping), # sinplify to univ
    mapping = gsub("ecole polytechnique federale", "ecole", mapping), # french and belgium
    mapping = gsub( "academic medical center", "", mapping), # common UC abbreviation
    mapping = gsub( "university", "u", mapping), # sinplify to univ
    mapping = gsub( "univ", "u", mapping), # sinplify to univ
    mapping = gsub( "uni", "u", mapping), # sinplify to univ
    mapping = gsub("Universidad", "u", mapping),
    mapping = gsub( "[[:digit:]]", "", mapping), # remove numbers
    mapping = removePunctuation(mapping), # remove rest of punctuation
    mapping = trimws(mapping), # trim leading and trailing whitespace
    mapping = gsub("\\s+"," ", mapping), # remove extra whitespace in betweeen words
    num_words = length(unlist(strsplit(mapping, split = " "))),
    mapping = word(mapping, start = 1, end = ifelse(num_words < word_cut, num_words, word_cut)) # select first three words
  ) %>%
  dplyr::select(-num_words, -key)

# Load the ranking information from dropbox
rankings <- read_csv("https://www.dropbox.com/s/wm8ymnzsw1c1uix/THE_world_rankings.csv?dl=1")

# Repeat with Times Higher Education rankings 
rankings <- rankings %>% 
  group_by(name) %>%
  mutate(
    mapping = name,
    mapping = tolower(mapping), # convert all to lower case
    mapping = removeWords(mapping, c("the", "of", "at", "for", "and", "in")), # remove basic articles
    mapping = iconv(mapping, to="ASCII//TRANSLIT"), # try to replace accented characters as best as possible
    mapping = gsub( "[-—]", " ", mapping), # replace both em-dash and regular dash with a space
    mapping = gsub( "university", "u", mapping), # sinplify to univ
    mapping = gsub( "institute", "ins", mapping), # sinplify to univ
    mapping = gsub( "national", "nat", mapping), # sinplify to univ
    mapping = gsub( "howard hughes medical institute", "", mapping), # common name
    mapping = gsub("ueristes", "u", mapping), # french university title
    mapping = gsub("Universiteit", "u", mapping), #dutch university name
    mapping = gsub("ecole polytechnique federale", "ecole", mapping), # french and belgium
    mapping = gsub( "school", "sch", mapping), # sinplify to univ
    mapping = gsub( "univ", "u", mapping), # sinplify to univ
    mapping = gsub( "uni", "u", mapping), # sinplify to univ
    mapping = gsub("Universidad", "u", mapping), # spanish name
    mapping = gsub( "[[:digit:]]", "", mapping), # remove numbers
    mapping = removePunctuation(mapping), # remove rest of punctuation
    mapping = trimws(mapping), # trim leading and trailing whitespace
    mapping = gsub("\\s+"," ", mapping), # remove extra whitespace in betweeen words
    num_words = length(unlist(strsplit(mapping, split = " "))),
    mapping = word(mapping, start = 1, end = ifelse(num_words < word_cut, num_words, word_cut)) # select first three words
  ) %>%
  dplyr::select(rank, name, mapping)


# After a fair bit of tuning, I decided on using the jaro distance with a max ddistance threshold of .15, this seems to produce reasonably good results (though still
# some mistakes), and produces a large number of matcheas
matched <- institutions %>%
  filter(!is.na(mapping)) %>%
  stringdist_join(rankings, by = c("mapping" = "mapping"), method = "jw", max_dist = .20, mode = "left", distance_col = "dist") %>%
  group_by(value) %>%
  arrange(dist) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-mapping.y, -mapping.x, -dist) %>%
  rename(elife_institution = value,
         THE_institution = name)


# count how many corresponding authors this matching accounts for
counted <- elife %>% left_join(matched, by = c("la_institution" = "elife_institution")) %>% group_by(THE_institution) %>% summarise(count = n())
sum(subset(counted, !is.na(THE_institution))$count) # the number of corresponding authors for which this accounts for.



# Now we assign a category based on the prestige. If it is in the top 50, it is rated as "top". If in the top 200, its ranked as "Middle", and otherwise
# is assigned "unranked"

matched_with_rank <- matched %>%
  group_by(elife_institution) %>%
  mutate(
    THE_rank = ifelse(!is.na(as.numeric(rank)) & as.numeric(rank) < 50, "Top", 
                      ifelse(!is.na(as.numeric(rank)) & as.numeric(rank) < 200, "Proficient", "Standard/Unranked/Unknown")
               )
  ) %>%
  select(-rank, -THE_institution, -count)


write.csv(matched_with_rank, "~/Desktop/elife_institutions.csv")

