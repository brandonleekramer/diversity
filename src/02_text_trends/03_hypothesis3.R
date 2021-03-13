
####################################################################################### install.packages (for slurm) 

#rm(list = ls())
#analysis_timeframe <- "1990"
#install.packages("readr", repos = "http://cran.us.r-project.org")
#install.packages("stringi", repos = "http://cran.us.r-project.org", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#install.packages("stringr", repos = "http://cran.us.r-project.org", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#install.packages("dplyr", repos = "http://cran.us.r-project.org")
#install.packages("tidytext", repos = "http://cran.us.r-project.org")
#install.packages("widyr", repos = "http://cran.us.r-project.org")
#install.packages("RPostgreSQL", repos = "http://cran.us.r-project.org")
#install.packages("data.table", repos = "http://cran.us.r-project.org")
#install.packages("maditr", repos = "http://cran.us.r-project.org")
#install.packages("purrr", repos = "http://cran.us.r-project.org")

library("readr")
library("dplyr") 
library("stringr")  
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("purrr")

######################################################################################################## testing h3 

test_h3 <- function(analysis_timeframe){

# this file produces 2 outputs 
# 1. counts all of the terms in a nested dictionary of population terms from around the world 
# 2. converts those counts into percentage of publications over time 

#rm(list = ls())
#analysis_timeframe <- "1990"

################################################################################################# ingestion/cleaning 
  
str_c("Starting data pull at: ", Sys.time())

# connect to postgresql to get our data
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))

# query the users_gh data (table of all github users) 
pubmed_data <- dbGetQuery(conn, str_c(
                          "SELECT DISTINCT(fk_pmid), year, abstract  
                          FROM pubmed_2021.biomedical_abstracts 
                          WHERE year =", analysis_timeframe, ";"))

# disconnect from postgresql database 
dbDisconnect(conn)

str_c("Finished data pull at: ", Sys.time())

# total articles each year 
# banking this df now to optimize our memory for later 
bycats_prc_counts <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ungroup() %>% 
  rename(total = n) 

################################################################################################# creating dictionaries

# lets pull in the dictionaries 
setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 
h3_dictionary <- read_csv("diversity_project - h3_dictionary.csv") 

# pulling in concatenated population terms from our dataset 
ancestry <- h3_dictionary %>% filter(category == "ancestry")
continental_terms <- h3_dictionary %>% filter(category == "continental")
subcontinental_terms <- h3_dictionary %>% filter(category == "subcontinental")
national_terms <- h3_dictionary %>% filter(category == "national")
subnational_terms <- h3_dictionary %>% filter(category == "subnational")
directional_terms <- h3_dictionary %>% filter(category == "directional")
race_ethnicity <- h3_dictionary %>% filter(category == "race/ethnicity")
omb_uscensus <- h3_dictionary %>% filter(category == "omb/us census")

# creating detection string patterns for all the categories
all_pop_terms <- paste(c("\\b(?i)(zcx", h3_dictionary$term, "zxc)\\b"), collapse = "|")
ancestry <- paste(c("\\b(?i)(zxz", ancestry$term, "zxz)\\b"), collapse = "|")
continental_terms <- paste(c("\\b(?i)(zxz", continental_terms$term, "zxz)\\b"), collapse = "|")
subcontinental_terms <- paste(c("\\b(?i)(zxz", subcontinental_terms$term, "zxz)\\b"), collapse = "|")
national_terms <- paste(c("\\b(?i)(zxz", national_terms$term, "zxz)\\b"), collapse = "|")
subnational_terms <- paste(c("\\b(?i)(zxz", subnational_terms$term, "zxz)\\b"), collapse = "|")
directional_terms <- paste(c("\\b(?i)(zxz", directional_terms$term, "zxz)\\b"), collapse = "|")
race_ethnicity <- paste(c("\\b(?i)(zxz", race_ethnicity$term, "zxz)\\b"), collapse = "|")
omb_uscensus <- paste(c("\\b(?i)(zxz", omb_uscensus$term, "zxz)\\b"), collapse = "|")

################################################################################################# preprocessing

str_c("Starting to preprocess at: ", Sys.time())

# pulls in the preprocessing dictionary 
preprocessing_terms <- read_csv("diversity_project - preprocessing.csv") %>%
  select(original_string, new_string) %>% tibble::deframe()

# convert text to lowercase 
pubmed_data <- pubmed_data %>% mutate(abstract = tolower(abstract))

# replaces all the hyphenated and compound words 
# helps to reduce false positives (e.g. double_blind)
pubmed_data$abstract <- pubmed_data$abstract %>% 
  str_replace_all(preprocessing_terms)

# animal exclusion + human inclusion clauses
in_exclusions <- read_csv("diversity_project - in_exclusions.csv")
human_inclusion_clause <- paste(c("\\b(?i)(zqx", na.omit(in_exclusions$humans), "zqx)\\b"), collapse = "|")
animal_exclusion_clause <- paste(c("\\b(?i)(zqx", na.omit(in_exclusions$animals), "zqx)\\b"), collapse = "|")

# remove abstracts with animals to reduce false positive animal studies 
pubmed_data <- pubmed_data %>% 
  dt_mutate(human_study = ifelse(test = str_detect(string = abstract, 
            pattern = human_inclusion_clause), yes = 1, no = 0)) %>% 
  dt_mutate(animal_study = ifelse(test = str_detect(string = abstract, 
            pattern = animal_exclusion_clause), yes = 1, no = 0)) %>% 
  filter(human_study == 1 | animal_study == 0)

str_c("Starting to unnest tokens at: ", Sys.time())

# tokenizing the abstract data into words 
pubmed_abstract_data <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  rename(id = fk_pmid) %>% 
  unnest_tokens(word, abstract) %>% 
  # run this since the dataset is so huge now 
  filter(word %in% na.omit(h3_dictionary$term))

# clean up memory 
# rm(pubmed_data)

str_c("Finished unnesting tokens at: ", Sys.time())

############################################################################################## counting terms over time

str_c("Started counting tokens at: ", Sys.time())

pop_terms_bycats <- pubmed_abstract_data %>% 
  as.data.table() %>% 
  dt_mutate(all_pop_terms = ifelse(test = str_detect(string = word, pattern = all_pop_terms), 1, 0)) %>%
  dt_mutate(continental = ifelse(test = str_detect(string = word, pattern = continental_terms), 1, 0)) %>%
  dt_mutate(subcontinental = ifelse(test = str_detect(string = word, pattern = subcontinental_terms), 1, 0)) %>%
  dt_mutate(national = ifelse(test = str_detect(string = word, pattern = national_terms), 1, 0)) %>%
  dt_mutate(subnational = ifelse(test = str_detect(string = word, pattern = subnational_terms), 1, 0)) %>%
  dt_mutate(directional = ifelse(test = str_detect(string = word, pattern = directional_terms), 1, 0)) %>%
  dt_mutate(race_ethnicity = ifelse(test = str_detect(string = word, pattern = race_ethnicity), 1, 0)) %>%
  dt_mutate(omb_uscensus = ifelse(test = str_detect(string = word, pattern = omb_uscensus), 1, 0)) %>%
  dt_mutate(ancestry = ifelse(test = str_detect(string = word, pattern = ancestry), 1, 0)) %>% 
  dt_mutate(word = if_else(directional == 1 & word == "north_america", "north", word),
            word = if_else(directional == 1 & word == "north_american", "north", word),
            word = if_else(directional == 1 & word == "north_americans", "north", word),
            word = if_else(directional == 1 & word == "south_america", "south", word),
            word = if_else(directional == 1 & word == "south_american", "south", word),
            word = if_else(directional == 1 & word == "south_americans", "south", word)) 

h3_counts <- pop_terms_bycats %>% 
  filter(all_pop_terms == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "all population terms") %>% 
  select(term, everything())

h3_counts <- pop_terms_bycats %>% 
  filter(continental == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "continental") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts)

h3_counts <- pop_terms_bycats %>% 
  filter(subcontinental == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "subcontinental") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts)

h3_counts <- pop_terms_bycats %>% 
  filter(national == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "national") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts)

h3_counts <- pop_terms_bycats %>% 
  filter(subnational == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "subnational") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts)

h3_counts <- pop_terms_bycats %>% 
  filter(directional == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "directional") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts)

h3_counts <- pop_terms_bycats %>% 
  filter(race_ethnicity == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "race/ethnicity") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts)

h3_counts <- pop_terms_bycats %>% 
  filter(omb_uscensus == 1) %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(term = "omb/us-census") %>% 
  select(term, everything()) %>% 
  bind_rows(h3_counts) %>% 
  # add the year and sort  
  mutate(year = analysis_timeframe) %>% 
  arrange(year, -n)

h3_counts_chk <- h3_counts %>% filter(term != "all population terms")

setwd("~/git/diversity/data/text_results/h3_results/")
write_rds(h3_counts, str_c("h3_counts_",analysis_timeframe,".rds"))

############################################################################################## sensitivity analyses

#words_of_interest = c("apache", "ree", "mono", "ho", "bia", "mayo", "tunica", 
#                      "bia", "het", "wa", "nat", "lu", "lak", "ava", "zo")

#sensitivity_chks = pop_terms_bycats %>% 
#  filter(word %in% words_of_interest) %>% 
#  rename(fk_pmid = id) %>%
#  inner_join(pubmed_data %>% select(-year), by = "fk_pmid") %>% 
#  select(fk_pmid, year, word, abstract, everything()) 

#second_chk = sensitivity_chks %>% 
#  filter(word == "wa") %>% 
#  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
#  count(bigram) %>% 
#  filter(grepl(paste(words_of_interest, collapse = "|"), bigram)) %>% 
#  arrange(-n)

############################################################################################## percentages 

percentages = h3_counts %>% 
  group_by(term) %>% 
  count(wt = n) %>% 
  mutate(year = analysis_timeframe,
         total = bycats_prc_counts$total,
         percentage = round(n / total * 100, digits = 2)) %>% 
  select(term, year, n, total, percentage) %>% 
  arrange(year, -percentage) 

setwd("~/git/diversity/data/text_results/h3_results/")
write_rds(percentages, str_c("h3_prc_trends_",analysis_timeframe,".rds"))

str_c("Finished all processes for ",analysis_timeframe, " at: ", Sys.time())

############################################################################################## distinct ids 

distinct_ids = pop_terms_bycats %>% 
  rename(fk_pmid = id) %>% 
  group_by(fk_pmid, year) %>% 
  summarise(across(all_pop_terms:omb_uscensus, sum)) %>% 
  arrange(fk_pmid, year)

# this saves all of the unique ids that mention h3 diversity sets 
# basically, this table of uniques ids can go back to postgresql 
setwd("~/git/diversity/data/text_results/h3_results/")
write_rds(distinct_ids, str_c("h3_population_ids_",analysis_timeframe,".rds"))

} # closing the function 

##################################################################################### for loop of all years 

for (year in 1992:2020) {
  test_h3(year)
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years 

setwd("~/git/diversity/data/text_results/h3_results/")

#chk <- read_rds("h3_all_prcs.rds")

# percentages for all sets 
h3_all_counts <- list.files(pattern="h3_counts_*") %>% 
  map_df(~read_rds(.)) 

# overall set counts 
h3_all_prcs <- list.files(pattern="h3_prc_trends_*") %>% 
  map_df(~read_rds(.))

# all the unique ids 
h3_allpop_ids <- list.files(pattern="h3_population_ids_*") %>% 
  map_df(~read_rds(.))

# need to do the matrix aggregation next 

setwd("~/git/diversity/data/text_results/h3_results/")
write_rds(h3_all_counts, "h3_all_counts.rds")
write_rds(h3_all_prcs, "h3_all_prcs.rds")
write_rds(h3_allpop_ids, "h3_all_population_ids.rds")

str_c("Aggregated data for all years at: ", Sys.time())
