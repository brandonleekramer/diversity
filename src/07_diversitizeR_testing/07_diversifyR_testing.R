

rm(list = ls())

library("tidyverse")
library("tidytable")
library("RPostgreSQL")

# connect to postgresql to get our data
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
pubmed_data <- dbGetQuery(conn, str_c(
                          "SELECT DISTINCT(fk_pmid), year, abstract  
                          FROM pubmed_2021.biomedical_abstracts 
                          WHERE year = 1990 LIMIT 1000;"))
dbDisconnect(conn)


str_c("Old version start: ", Sys.time())
# animal exclusion + human inclusion clauses
library("maditr")
setwd("~/git/diversity/data/dictionaries/")
dictionary <- readr::read_csv("diversity_project - humanizeR.csv")
human <- dictionary %>% filter(category == "human")
nonhuman <- dictionary %>% filter(category == "nonhuman")
human <- paste(c("\\b(?i)(zqx", na.omit(human$terms), "zqx)\\b"), collapse = "|")
nonhuman <- paste(c("\\b(?i)(zqx", na.omit(nonhuman$terms), "zqx)\\b"), collapse = "|")
# remove abstracts with animals to reduce false positive animal studies 
version1 <- pubmed_data %>% 
  dt_mutate(human = ifelse(test = str_detect(
    string = abstract, pattern = human), yes = 1, no = 0)) %>% 
  dt_mutate(nonhuman = ifelse(test = str_detect(
    string = abstract, pattern = nonhuman), yes = 1, no = 0))
str_c("Old version end: ", Sys.time())

# initial benchmark with 16 cores and 200 GB RAM 
(145 * 2500) / 60 / 60


str_c("New version start: ", Sys.time())
source("~/git/diversity/scripts/diversitizeR.R")
version2 <- pubmed_data %>% 
  humanizeR(abstract) 
str_c("New version end: ", Sys.time())

# initial benchmark with 16 cores and 200 GB RAM 
(135 * 2500) / 60 / 60


str_c("Newest version start: ", Sys.time())
version3 <- pubmed_data %>% 
  tidytext::unnest_tokens(word, abstract) %>%
  as_tidytable() %>% 
  tidytable::mutate.(human = ifelse(stringr::str_detect(string = word, human), 1, 0)) %>% 
  #tidytable::mutate.(nonhuman = ifelse(stringr::str_detect(string = word, nonhuman), 1, 0)) %>%
  tidytable::select.(-word) %>% 
  tidytable::summarize.(human = sum(human), #nonhuman = sum(nonhuman), 
                        .by = fk_pmid)
str_c("Newest version end: ", Sys.time())


setwd("~/git/diversity/data/dictionaries/")
dictionary <- readr::read_csv("diversity_project - humanizeR.csv")
human <- dictionary %>% filter(category == "human")
nonhuman <- dictionary %>% filter(category == "nonhuman")


str_c("Newest version start: ", Sys.time())
tmp_df <- pubmed_data %>% 
  tidytext::unnest_tokens(word, abstract) %>%
  as_tidytable() %>% 
  tidytable::filter.(word %in% human$terms) %>% 
  tidytable::mutate.(human = 1, nonhuman = 0) %>% 
  tidytable::select.(-word) %>% 
  as.data.frame()

tmp_df <- pubmed_data %>% 
  tidytext::unnest_tokens(word, abstract) %>%
  as_tidytable() %>%
  tidytable::filter.(word %in% nonhuman$terms) %>% 
  tidytable::mutate.(human = 0, nonhuman = 1) %>% 
  tidytable::select.(-word) %>% 
  tidytable::bind_rows.(tmp_df) %>% 
  tidytable::summarize.(human = sum(human),
                        nonhuman = sum(nonhuman), 
                        .by = fk_pmid) %>% 
  tidytable::arrange.(fk_pmid)
str_c("Newest version end: ", Sys.time())


humanizeR <- function(df, key, input){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  setwd("~/git/diversity/data/dictionaries/")
  dictionary <- readr::read_csv("diversity_project - humanizeR.csv")
  human <- dictionary %>% filter(category == "human")
  nonhuman <- dictionary %>% filter(category == "nonhuman")
  
  key <- enquo(key)
  input <- enquo(input)
  
  tmp_df <- df %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    as_tidytable() %>% 
    tidytable::filter.(word %in% human$terms) %>% 
    tidytable::mutate.(human = 1, nonhuman = 0) %>% 
    tidytable::select.(-word) %>% 
    as.data.frame()
  
  tmp_df <- pubmed_data %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    as_tidytable() %>%
    tidytable::filter.(word %in% nonhuman$terms) %>% 
    tidytable::mutate.(human = 0, nonhuman = 1) %>% 
    tidytable::select.(-word) %>% 
    tidytable::bind_rows.(tmp_df) %>% 
    tidytable::summarize.(human = sum(human),
                          nonhuman = sum(nonhuman), 
                          .by = !!key) %>% 
    tidytable::arrange.(!!key)
  
  df <- df %>% 
    tidytable::left_join.(tmp_df) %>% 
    tidytable::mutate.(human = replace_na.(human, 0)) %>% 
    tidytable::mutate.(nonhuman = replace_na.(nonhuman, 0))
  
  df
  
}



source("~/git/diversity/scripts/diversitizeR.R")
str_c("Newest version end: ", Sys.time())
chk <- pubmed_data %>% 
  humanizeR(fk_pmid, abstract)
str_c("Newest version end: ", Sys.time())
















