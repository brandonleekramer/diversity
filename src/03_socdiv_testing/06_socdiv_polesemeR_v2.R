
rm(list = ls())

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")
source("~/git/diversity/scripts/diversitizeR.R")

conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
diversity_abstracts <- dbGetQuery(conn, "SELECT * FROM pubmed_2021.all_diversity_abstracts_0321 WHERE diversity = 1 ;")
dbDisconnect(conn)

setwd("~/git/diversity/data/sensitivity_checks/")
labeled_data <- read_csv("diversity_labeled_061221.csv") 
labeled_data <- labeled_data %>% 
  rename(labels = blk_code) %>% 
  select(fk_pmid, labels)
diversity_abstracts <- diversity_abstracts %>% 
  left_join(labeled_data, by = "fk_pmid")

# this is the older data, so lets see if the new humanizeR function helps 
# note: humanizeR jumped from 237 animal terms to ~1100 animal terms 
diversity_humanized <- diversity_abstracts %>% 
  humanizeR(fk_pmid, abstract) %>% 
  filter(human > 0 | nonhuman == 0) %>% 
  mutate(ex_ratio = round(nonhuman / human, 2),
         ex_ratio = replace_na(ex_ratio, 0)) %>% 
  filter(ex_ratio < 5)

# this shows that the soc_diversity drops from 11789 to 10448
# minimizes 10% of false positives assuming we didn't remove true positives 
diversity_abstracts %>% group_by(soc_diversity) %>% count()
diversity_humanized %>% group_by(soc_diversity) %>% count()

true_positives <- diversity_humanized %>% 
  select(fk_pmid, abstract, soc_diversity, labels, soc_div_terms) %>% 
  filter(labels == 1 & soc_diversity == 1)

true_negatives <- diversity_humanized %>% 
  select(fk_pmid, abstract, soc_diversity, labels, soc_div_terms) %>% 
  filter(labels == 0 & soc_diversity == 0)

false_negatives <- diversity_humanized %>% 
  select(fk_pmid, abstract, soc_diversity, labels, soc_div_terms) %>% 
  filter(labels == 1 & soc_diversity == 0)

false_positives_original <- diversity_humanized %>% 
  select(fk_pmid, abstract, soc_diversity, labels, soc_div_terms) %>% 
  filter(labels == 0 & soc_diversity == 1)

# check impact of polysemeR
# false positives dropped from 209 to 166 
str_c("Started at: ", Sys.time())
false_positives_tmp <- diversity_humanized %>% 
  filter(soc_diversity > 0) %>% 
  polysemeR(fk_pmid, abstract) 
false_positives <- false_positives_tmp %>% 
  filter(labels == 0 & soc_diversity == 1 & heterogeneity == 0)
str_c("Finished at: ", Sys.time())

false_positives <- false_positives %>% 
  select(fk_pmid, abstract, soc_diversity, labels, soc_div_terms, heterogeneity, human, nonhuman) %>% 
  filter(labels == 0 & soc_diversity == 1 & heterogeneity == 0)

precision = nrow(true_positives) / (nrow(true_positives) + nrow(false_positives))
recall = nrow(true_positives) / (nrow(true_positives) + nrow(false_negatives))
f1_score = (2*nrow(true_positives)) / ((2*nrow(true_positives)) + nrow(false_positives) + nrow(false_negatives))



false_positives %>% 
  unnest_tokens(words, abstract, token = "ngrams", n = 2) %>% 
  count(words) %>% 
  arrange(-n) %>% 
  filter(grepl("^diverse", words))






setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(false_positives, "false_positives_070321_1243.csv")


tp_count <- true_positives %>% 
  filter(grepl(nonhuman_terms, abstract)) 


work_on_this %>% 
  unnest_tokens(trigram, abstract, token = "ngrams", n = ) %>% 
  filter(grepl("diversity|diverse|diversification|diversify", trigram)) %>% 
  count(trigram) %>% 
  arrange(-n)





df <- diversity_humanized

diversity_humanized %>% 
  top_n(5, abstract) %>%
  polysemeR(fk_pmid, abstract) 

df <- df %>% 
  tidytable::left_join.(ngrams) %>% 
  tidytable::mutate.(heterogeneity = replace_na.(heterogeneity, 0)) %>% 
  as.data.frame()

df




