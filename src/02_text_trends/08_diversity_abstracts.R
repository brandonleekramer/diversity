
rm(list = ls())

# load pkgs 
library("tidyverse")
library("tidytext")
library("RPostgreSQL")

# ingest data 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
diversity_abstracts <- dbGetQuery(conn, "SELECT * FROM pubmed_2021.soc_diversity_abstracts")
dbDisconnect(conn)


setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 
diverstionary <- paste(na.omit(divictionary$diversity), collapse = "|")

diversity_within <- diversity_abstracts %>% 
  select(fk_pmid, year, abstract, soc_div_terms, soc_diversity, ends_with("totals")) %>% 
  filter(grepl(diverstionary, abstract))

setwd("~/git/diversity/data/sensitivity_checks")
write_csv(diversity_within, "diversity_training_raw.csv")

data(stop_words)
word_counts <- diversity_within %>% 
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) 

diversity_abstracts %>% 
  filter(soc_diversity == 1) %>% 
  count()

social_diversity_only <- diversity_abstracts %>% 
  select(soc_diversity, asian:white, -diversity) 
  
sapply(social_diversity_only,function(x) sum(is.na(x)))


### modeling 

library(tidymodels)

soc_div_split <- social_diversity_only %>%
  initial_split(strata = soc_diversity)

soc_div_train <- training(soc_div_split)
soc_div_test <- testing(soc_div_split)

lm_fit <- linear_reg() %>%
  set_engine(engine = "lm") %>%
  fit(soc_diversity ~ .,
      data = soc_div_train)
lm_fit

rf_fit <- rand_forest(mode = "regression") %>%
  set_engine("ranger") %>%
  fit(soc_diversity ~ .,
      data = soc_div_train)
rf_fit



  


















