

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

##################################################################################################### setting function 

test_h2 <- function(analysis_timeframe){
  
  ################################################################################################# ingestion/cleaning 
  
  #rm(list = ls())
  #analysis_timeframe <- "2000"
  
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
  source("~/git/diversity/data/dictionaries/diversity_project - omb_terms.R")
  setwd("~/git/diversity/data/dictionaries/")
  h1_dictionary <- read_csv("diversity_project - h1_dictionary.csv")
  h2_dictionary <- read_csv("diversity_project - h2_dictionary.csv")
  h3_dictionary <- read_csv("diversity_project - h3_dictionary.csv")
  omb_black <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$black), "z3x)\\b"), collapse = "|")
  omb_native_american <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$native_american), "z3x)\\b"), collapse = "|")
  omb_pacific_islander <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$pacific_islander), "z3x)\\b"), collapse = "|")
  omb_hispanic_latinx <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$hispanic_latinx), "z3x)\\b"), collapse = "|")
  omb_asian <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$asian), "z3x)\\b"), collapse = "|")
  omb_white <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$white), "z3x)\\b"), collapse = "|")
  omb_racial <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$race), "z3x)\\b"), collapse = "|")
  omb_ethnicity <- paste(c("\\b(?i)(z3x",na.omit(h2_dictionary$ethnicity), "z3x)\\b"), collapse = "|")
  
  h2_full_string <- c(na.omit(h2_dictionary$black), na.omit(h2_dictionary$native_american), 
                           na.omit(h2_dictionary$pacific_islander), na.omit(h2_dictionary$asian), 
                           na.omit(h2_dictionary$hispanic_latinx), na.omit(h2_dictionary$white), 
                           na.omit(h2_dictionary$race), na.omit(h2_dictionary$ethnicity))
  
  # total articles each year 
  # banking this df now to optimize our memory for later 
  total_pubs <- pubmed_data %>% 
    distinct(fk_pmid, year, abstract) %>% 
    group_by(year) %>% 
    count(year) %>% 
    ungroup() %>% 
    rename(total = n)
  
  str_c("Started bigrams at: ", Sys.time())
  
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
  
  # tokenizing the abstract data into words 
  pubmed_tokens <- pubmed_data %>% 
    unnest_tokens(word, abstract) %>% 
    # filter down to only include diversity terms 
    filter(word %in% h2_full_string)
  
  str_c("Finished unnesting at: ", Sys.time())
  
  # get counts of all the omb terms 
  us_pop_counts_3d <- pubmed_tokens %>% 
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
           pattern = "\\b(ethnicity)\\b"), yes = 1, no = 0)) 
  
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
  
  # this gives you the more specific term counts 
  annual_summary <- us_pop_counts_3d %>% 
    select(word, term, year) %>% 
    group_by(year, word, term) %>% 
    count() %>% 
    rename(count = n) %>% 
    arrange(-count)
  
  # this gives you the social diversity counts from h1 
  # (but you must run h1 before you can get this) 
  social_diversity <- read_rds("~/git/diversity/data/text_results/h1_results/h1_all_subset_counts_trends.rds") %>% 
    filter(term == "diversity (social)" & year == analysis_timeframe) 
  
  # bind social_diversity to the annual_summary df 
  annual_summary <- social_diversity %>% 
    bind_rows(annual_summary) %>% 
    arrange(-count)
  
  # then add social_diversity to counts and clean the cols 
  us_pop_prc_counts$cnt_diversity <- sum(social_diversity$count)[1]
  us_pop_prc_counts <- us_pop_prc_counts %>% 
    select(year, total, starts_with("cnt_"), starts_with("prc_"))
  
  # turn that count into a percentage 
  us_pop_prc_counts <- us_pop_prc_counts %>% 
    mutate(prc_diversity = round(cnt_diversity / total * 100, digits = 2))

  setwd("~/git/diversity/data/text_results/h2_results/")
  write_rds(annual_summary,  str_c("h2_omb_counts_",analysis_timeframe,".rds"))
  write_rds(us_pop_counts_3d,  str_c("h2_omb_ids_",analysis_timeframe,".rds"))
  write_rds(us_pop_prc_counts, str_c("h2_omb_prcs_",analysis_timeframe,".rds"))
  
  #sensitivity checks 
  #chk_abstracts <- pubmed_data %>% 
  #  inner_join(pubmed_tokens %>% 
  #               select(fk_pmid, word), 
  #             by = "fk_pmid") %>% 
  #  distinct(fk_pmid, year, word, abstract)
  #setwd("~/git/diversity/data/sensitivity_checks/")
  #write_csv(chk_abstracts, str_c("h2_sensitivity_chks_",analysis_timeframe,".csv"))
  
  str_c("Finished all processes for ",analysis_timeframe, " at: ", Sys.time())
  
}

##################################################################################### for loop of all years 

for (year in 1990:2020) {
  test_h2(year)
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years

setwd("~/git/diversity/data/text_results/h2_results/")

# get all the ids from the abstracts that mention omb terms 
h2_omb_ids <- list.files(pattern="h2_omb_ids_*") %>% 
  map_df(~read_rds(.)) %>% 
  select(fk_pmid, year, black:ethnic) %>% 
  group_by(fk_pmid, year) %>% 
  summarise(across(black:ethnic, sum)) %>% 
  arrange(fk_pmid, year)

# percentages for all sets 
h2_omb_prcs <- list.files(pattern="h2_omb_prcs_*") %>% 
  map_df(~read_rds(.)) 

# percentages for all sets 
h2_omb_counts <- list.files(pattern="h2_omb_counts_*") %>% 
  map_df(~read_rds(.)) 

# overall set counts 

setwd("~/git/diversity/data/text_results/h2_results/")
write_rds(h2_omb_ids, "h2_all_omb_ids.rds")
write_rds(h2_omb_prcs, "h2_all_omb_prcs.rds")
write_rds(h2_omb_counts, "h2_all_omb_counts.rds")
