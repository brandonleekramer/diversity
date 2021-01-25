
####################################################################################### install.packages (for slurm) 

#rm(list = ls())

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

######################################################################################################## testing h2 

test_h2 <- function(analysis_timeframe){

# this file produces 2 outputs 
# 1. counts all of the terms in a nested dictionary of population terms from around the world 
# 2. converts those counts into percentage of publications over time 
  
library("readr")
library("dplyr") 
library("stringr")  
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("purrr")
data(stop_words)

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

str_c("Starting to unnest tokens at: ", Sys.time())

# tokenizing the abstract data into words 
pubmed_abstract_data <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  rename(id = fk_pmid) %>% 
  unnest_tokens(word, abstract) %>% 
  # run this since the dataset is so huge now 
  anti_join(stop_words) 

# clean up memory 
rm(pubmed_data)

str_c("Finished unnesting tokens at: ", Sys.time())

################################################################################################# creating dictionaries

# lets pull in the dictionaries 
setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 
h2_dictionary <- read_csv("diversity_project - h2_dictionary.csv") 

# pulling in concatenated population terms from our dataset 
ancestry <- h2_dictionary %>% filter(category == "ancestry")
continental_terms <- h2_dictionary %>% filter(category == "continental")
subcontinental_terms <- h2_dictionary %>% filter(category == "subcontinental")
national_terms <- h2_dictionary %>% filter(category == "national")
subnational_terms <- h2_dictionary %>% filter(category == "subnational")
directional_terms <- h2_dictionary %>% filter(category == "directional")
race_ethnicity <- h2_dictionary %>% filter(category == "race/ethnicity")
omb_uscensus <- h2_dictionary %>% filter(category == "omb/us census")

# creating detection string patterns for all the categories
all_pop_terms <- paste(c("\\b(?i)(zcx", h2_dictionary$term, "zxc)\\b"), collapse = "|")
ancestry <- paste(c("\\b(?i)(zxz", ancestry$term, "zxz)\\b"), collapse = "|")
continental_terms <- paste(c("\\b(?i)(zxz", continental_terms$term, "zxz)\\b"), collapse = "|")
subcontinental_terms <- paste(c("\\b(?i)(zxz", subcontinental_terms$term, "zxz)\\b"), collapse = "|")
national_terms <- paste(c("\\b(?i)(zxz", national_terms$term, "zxz)\\b"), collapse = "|")
subnational_terms <- paste(c("\\b(?i)(zxz", subnational_terms$term, "zxz)\\b"), collapse = "|")
directional_terms <- paste(c("\\b(?i)(zxz", directional_terms$term, "zxz)\\b"), collapse = "|")
race_ethnicity <- paste(c("\\b(?i)(zxz", race_ethnicity$term, "zxz)\\b"), collapse = "|")
omb_uscensus <- paste(c("\\b(?i)(zxz", omb_uscensus$term, "zxz)\\b"), collapse = "|")

################################################################################################# filtering dataset 

filtered_diversity_terms <- h2_dictionary$term

pubmed_abstract_data <- pubmed_abstract_data %>% 
  filter(word %in% filtered_diversity_terms)

# this saves all of the unique ids that mention h1 diversity sets 
# basically, this table of uniques ids can go back to postgresql 
setwd("~/git/diversity/data/text_results/h2_results/")
write_rds(pubmed_abstract_data %>% 
            filter(word %in% diversity_only) %>% distinct(id), 
          str_c("h2_diversity_ids_",analysis_timeframe,".rds"))

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
  dt_mutate(ancestry = ifelse(test = str_detect(string = word, pattern = ancestry), 1, 0))

h2_counts <- pop_terms_bycats %>% 
  filter(all_pop_terms == 1) %>%
  group_by(year) %>% 
  count(all_pop_terms) %>% 
  arrange(year) %>% 
  mutate(term = "all population terms") %>% 
  select(term, n, year) 

h2_counts <- pop_terms_bycats %>% 
  filter(continental == 1) %>%
  group_by(year) %>% 
  count(continental) %>% 
  arrange(year) %>% 
  mutate(term = "continental") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(subcontinental == 1) %>%
  group_by(year) %>% 
  count(subcontinental) %>% 
  arrange(year) %>% 
  mutate(term = "subcontinental") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(national == 1) %>%
  group_by(year) %>% 
  count(national) %>% 
  arrange(year) %>% 
  mutate(term = "national") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(subnational == 1) %>%
  group_by(year) %>% 
  count(subnational) %>% 
  arrange(year) %>% 
  mutate(term = "subnational") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(directional == 1) %>%
  group_by(year) %>% 
  count(directional) %>% 
  arrange(year) %>% 
  mutate(term = "directional") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(race_ethnicity == 1) %>%
  group_by(year) %>% 
  count(race_ethnicity) %>% 
  arrange(year) %>% 
  mutate(term = "race/ethnicity") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(omb_uscensus == 1) %>%
  group_by(year) %>% 
  count(omb_uscensus) %>% 
  arrange(year) %>% 
  mutate(term = "omb/us-census") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- pop_terms_bycats %>% 
  filter(ancestry == 1) %>%
  group_by(year) %>% 
  count(ancestry) %>% 
  arrange(year) %>% 
  mutate(term = "ancestry") %>% 
  select(term, n, year) %>% 
  bind_rows(h2_counts)

h2_counts <- h2_counts %>% 
  arrange(year, -n)

h2_diversity_abstracts <- pop_terms_bycats %>% 
  distinct(id) %>% 
  select(id, word, term)

setwd("~/git/diversity/data/text_results/h2_results/")
write_rds(h2_counts, str_c("h2_set_counts_",analysis_timeframe,".rds"))

############################################################################################## percentages over time 

# articles with term mentioned each year 
all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(all_pop_terms == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "all population terms") %>% 
  select(term, n, year)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(continental == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "continental") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(subcontinental == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "subcontinental") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(national == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "national") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(subnational == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "subnational") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(directional == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "directional") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(race_ethnicity == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "race/ethnicity") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(omb_uscensus == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "omb/us-census") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- pop_terms_bycats %>% 
  filter(ancestry == 1) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(term = "ancestry") %>% 
  select(term, n, year) %>% 
  bind_rows(all_pop_prc_counts)

all_pop_prc_counts <- all_pop_prc_counts %>% 
  right_join(bycats_prc_counts, by = "year") %>% 
  mutate(percentage = round(n / total * 100, digits = 2))

all_pop_prc_counts <- all_pop_prc_counts %>% 
  arrange(year, -percentage) %>% 
  select(-total)

setwd("~/git/diversity/data/text_results/h2_results/")
write_rds(all_pop_prc_counts, str_c("h2_prc_trends_",analysis_timeframe,".rds"))

str_c("Finished all processes for ",analysis_timeframe, " at: ", Sys.time())

} # closing the function 

##################################################################################### for loop of all years 

for (year in 1990:2020) {
  test_h2(year)
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years 

setwd("~/git/diversity/data/text_results/h2_results/")

# percentages for all sets 
h2_all_counts <- list.files(pattern="h2_set_counts_*") %>% 
  map_df(~read_rds(.)) 

# overall set counts 
h2_all_prcs <- list.files(pattern="h2_prc_trends_*") %>% 
  map_df(~read_rds(.)) 

# need to do the matrix aggregation next 

setwd("~/git/diversity/data/text_results/h2_results/")
write_rds(h2_all_counts, "h2_all_counts.rds")
write_rds(h2_all_prcs, "h2_all_prcs.rds")

str_c("Aggregated data for all years at: ", Sys.time())
