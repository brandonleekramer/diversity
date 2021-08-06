

rm(list = ls())

# load pkgs 
library("tidyverse")
library("tidytext")
library("RPostgreSQL")
data(stop_words)


## results section of the 05/21 paper 

## first paragraph
# age and lifecourse were top terms 
# this sub-analysis helps to support the claim that these terms are used in clinical research 

# ingest data 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
diversity_abstracts <- dbGetQuery(conn, "SELECT * FROM pubmed_2021.soc_diversity_abstracts_0721")
dbDisconnect(conn = conn)

age_gender_abstracts <- diversity_abstracts %>% 
  select(fk_pmid, year, abstract, publication, lifecourse, sex_gender) %>% 
  filter(lifecourse > 1 | sex_gender > 1 )

`%notin%` <- Negate(`%in%`)

custom_stop_words = c("95", "1", "2", "3", "4", "5", "6", "10", "12", "ci")

word_freqs <- age_gender_abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>% 
  filter(word %notin% custom_stop_words)

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(word_freqs, "top_words_in_socdiv_abstracts_0721.csv")

### second paragraph 
# what percentage of all articles that mention "diversity" are social uses of that term? 

general_pop_terms <- read_rds("~/git/diversity/data/text_results/h1_results/h1_all_set_prc_trends.rds")

soc_diversity_df  <- general_pop_terms %>% 
  select(year, total, cnt_diversity, cnt_soc_diversity) %>% 
  mutate(prc_soc_diversity = round(cnt_soc_diversity / cnt_diversity * 100, 2))
mean(soc_diversity_df$prc_soc_diversity)













