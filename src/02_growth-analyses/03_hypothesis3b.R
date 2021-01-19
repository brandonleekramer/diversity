

##################################################################################################### load packages 

# this file produces 2 outputs 
# 1. analyzes how all population terms are used by researchers within each country (counts & percentages)
# 2. analyzes the use of OMB/Census categories in this data 

library("readr")
library("dplyr") 
library("stringr")  
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("purrr")
library("tm")
data(stop_words)

##################################################################################################### setting function 

test_h3b <- function(analysis_timeframe){
  
  ################################################################################################# ingestion/cleaning 
  
  #rm(list = ls())
  #analysis_timeframe <- "2018"
  
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
    WHERE year = ", analysis_timeframe, ";"))
  
  # disconnect from postgresql database 
  dbDisconnect(conn)
  
  str_c("Finished data pull at: ", Sys.time())
  
  # lets pull in the dictionaries 
  source("~/git/diversity/data/dictionaries/omb_terms.R")
  setwd("~/git/diversity/data/dictionaries/")
  h1_dictionary <- read_csv("diversity_project - h1_dictionary.csv")
  h2_dictionary <- read_csv("diversity_project - h2_dictionary.csv")
  h3_dictionary <- read_csv("diversity_project - h3_dictionary.csv")
  omb_black <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$black), "z3x)\\b"), collapse = "|")
  omb_native_american <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$native_american), "z3x)\\b"), collapse = "|")
  omb_pacific_islander <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$pacific_islander), "z3x)\\b"), collapse = "|")
  omb_hispanic_latinx <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$hispanic_latinx), "z3x)\\b"), collapse = "|")
  omb_asian <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$asian), "z3x)\\b"), collapse = "|")
  omb_white <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$white), "z3x)\\b"), collapse = "|")
  omb_racial <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$race), "z3x)\\b"), collapse = "|")
  omb_ethnicity <- paste(c("\\b(?i)(z3x",na.omit(h3_dictionary$ethnicity), "z3x)\\b"), collapse = "|")
  omb_diversity <- paste(c("\\b(?i)(z3x",na.omit(h1_dictionary$diversity), "z3x)\\b"), collapse = "|")
  
  # total articles each year 
  # banking this df now to optimize our memory for later 
  total_pubs <- pubmed_data %>% 
    distinct(fk_pmid, year, abstract) %>% 
    group_by(year) %>% 
    count(year) %>% 
    ungroup() %>% 
    rename(total = n)
  
  str_c("Started bigrams at: ", Sys.time())
  
  # recoding frequently occuring population-related bigrams 
  us_pop_bigrams <- pubmed_data %>% 
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>% 
    mutate(rc_bigram = ifelse(test = str_detect(string = bigram,
                                                pattern = black_bigrams), yes = "african-american", no = bigram)) %>%
    mutate(rc_bigram = ifelse(test = str_detect(string = bigram,
                                                pattern = native_american_bigrams), yes = "native-american", no = rc_bigram)) %>%
    mutate(rc_bigram = ifelse(test = str_detect(string = bigram,
                                                pattern = pacific_islander_bigrams), yes = "pacific-islander", no = rc_bigram)) %>%
    mutate(rc_bigram = ifelse(test = str_detect(string = bigram,
                                                pattern = hispanic_american_bigrams), yes = "hispanic-american", no = rc_bigram)) %>%
    mutate(rc_bigram = ifelse(test = str_detect(string = bigram,
                                                pattern = asian_american_bigrams), yes = "asian-american", no = rc_bigram)) %>%
    select(-bigram) %>% 
    filter(rc_bigram == "african-american" | rc_bigram == "native-american" | rc_bigram == "hispanic-american" |
             rc_bigram == "asian-american"   | rc_bigram == "pacific-islander") %>% 
    count(rc_bigram, year, sort = TRUE) %>% 
    rename(word = rc_bigram)
  
  # tokenizing the abstract data into words 
  pubmed_data <- pubmed_data %>% 
    unnest_tokens(word, abstract) %>% 
    anti_join(stop_words)
  
  str_c("Finished unnesting at: ", Sys.time())
  
  # combining the hyphenated words to our full dataset 
  us_pop_counts <- pubmed_data %>% 
    filter(word %in% h2_dictionary$term) %>% 
    group_by(year) %>% 
    count(word, sort = TRUE) %>% 
    ungroup() %>% 
    select(word, year, n) %>% 
    bind_rows(us_pop_bigrams) %>% 
    arrange(-n)
  
  setwd("~/git/diversity/data/text_results/h3_results/")
  write_rds(us_pop_counts, str_c("h3_omb_counts_",analysis_timeframe,".rds"))
  
  # get counts of all the omb terms 
  us_pop_counts_3d <- pubmed_data %>% 
    mutate(term = ifelse(test = str_detect(string = word,
                                           pattern = omb_black), yes = "black", no = word)) %>% 
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_white), yes = "white", no = term)) %>% 
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_asian), yes = "asian", no = term)) %>%
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_hispanic_latinx), yes = "hispanic/latinx", no = term)) %>% 
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_racial), yes = "race", no = term)) %>% 
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_ethnicity), yes = "ethnicity", no = term)) %>% 
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_native_american), yes = "native-american", no = term)) %>%
    mutate(term = ifelse(test = str_detect(string = word, 
                                           pattern = omb_pacific_islander), yes = "pacific-islander", no = term)) %>% 
    mutate(black = ifelse(test = str_detect(string = term, 
                                            pattern = "\\b(black)\\b"), yes = 1, no = 0)) %>% 
    mutate(white = ifelse(test = str_detect(string = term, 
                                            pattern = "\\b(white)\\b"), yes = 1, no = 0)) %>%
    mutate(asian = ifelse(test = str_detect(string = term, 
                                            pattern = "\\b(asian)\\b"), yes = 1, no = 0)) %>%
    mutate(hispanic = ifelse(test = str_detect(string = term, 
                                               pattern = "\\b(hispanic/latinx)\\b"), yes = 1, no = 0)) %>% 
    mutate(native_american = ifelse(test = str_detect(string = term, 
                                                      pattern = "\\b(native-american)\\b"), yes = 1, no = 0)) %>% 
    mutate(pacific_islander = ifelse(test = str_detect(string = term, 
                                                       pattern = "\\b(pacific-islander)\\b"), yes = 1, no = 0)) %>% 
    mutate(racial = ifelse(test = str_detect(string = term, 
                                             pattern = "\\b(race)\\b"), yes = 1, no = 0)) %>% 
    mutate(ethnic = ifelse(test = str_detect(string = term, 
                                             pattern = "\\b(ethnicity)\\b"), yes = 1, no = 0)) %>% 
    mutate(diversity = ifelse(test = str_detect(string = term, 
                                                pattern = "\\b(diversity)\\b"), yes = 1, no = 0))
  
  # convert those to percentages 
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(black == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_black = n_distinct(fk_pmid)) %>% 
    left_join(total_pubs, by = "year") %>% 
    mutate(prc_black = round(cnt_black / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(white == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_white = n_distinct(fk_pmid)) %>% 
    left_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_white = round(cnt_white / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(asian == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_asian = n_distinct(fk_pmid)) %>% 
    left_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_asian = round(cnt_asian / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(hispanic == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_hispanic = n_distinct(fk_pmid)) %>% 
    right_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_hispanic = round(cnt_hispanic / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(native_american == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_native_american = n_distinct(fk_pmid)) %>% 
    right_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_native_american = round(cnt_native_american / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(pacific_islander == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_pacific_islander = n_distinct(fk_pmid)) %>% 
    right_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_pacific_islander = round(cnt_pacific_islander / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(racial == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_racial = n_distinct(fk_pmid)) %>% 
    right_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_racial = round(cnt_racial / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(ethnic == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_ethnic = n_distinct(fk_pmid)) %>% 
    right_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_ethnic = round(cnt_ethnic / total * 100, digits = 2))
  us_pop_prc_counts <- us_pop_counts_3d %>% 
    filter(diversity == 1) %>% 
    group_by(year) %>% 
    summarise(cnt_diversity = n_distinct(fk_pmid)) %>% 
    right_join(us_pop_prc_counts, by = "year") %>% 
    mutate(prc_diversity = round(cnt_diversity / total * 100, digits = 2))
  
  setwd("~/git/diversity/data/text_results/h3_results/")
  write_rds(us_pop_prc_counts, str_c("h3_omb_prcs_",analysis_timeframe,".rds"))
  
  str_c("Finished all processes for ",analysis_timeframe, " at: ", Sys.time())
  
}

##################################################################################### for loop of all years 

for (year in 1990:2020) {
  test_h3b(year)
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years

setwd("~/git/diversity/data/text_results/h3_results/")

# percentages for all sets 
h3_omb_counts <- list.files(pattern="h3_omb_counts_*") %>% 
  map_df(~read_rds(.)) 

# overall set counts 
h3_omb_prcs <- list.files(pattern="h3_omb_prcs_*") %>% 
  map_df(~read_rds(.)) 

setwd("~/git/diversity/data/text_results/h3_results/")
write_rds(h3_omb_counts, "h3_all_omb_counts.rds")
write_rds(h3_omb_prcs, "h3_all_omb_prcs.rds")
