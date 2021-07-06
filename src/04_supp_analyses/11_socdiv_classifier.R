

rm(list = ls())

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")
library("tidytable")
source("~/git/diversity/scripts/diversitizeR.R")

conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
pubmed_data <- dbGetQuery(conn, str_c("SELECT DISTINCT(fk_pmid), abstract  
                                      FROM pubmed_2021.biomedical_abstracts LIMIT 10000;"))
dbDisconnect(conn)

pubmed_data <- pubmed_data %>% 
  mutate(abstract = tolower(abstract)) %>% 
  humanizeR(fk_pmid, abstract) %>% 
  # different for diversity 
  filter(nonhuman == 0) # %>% compoundR(abstract)

setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 
all_diversity_cats  <- c(na.omit(divictionary$ancestry), na.omit(divictionary$cultural), 
                         na.omit(divictionary$disability), na.omit(divictionary$diversity), 
                         na.omit(divictionary$equity), na.omit(divictionary$lifecourse), 
                         na.omit(divictionary$migration), na.omit(divictionary$minority), 
                         na.omit(divictionary$population),
                         na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                         na.omit(divictionary$sexuality), na.omit(divictionary$social_class))

# three versions of splitting sentences 
# my rough version 
version_1 <- pubmed_data %>%
  mutate(abstract = gsub("-", " ", abstract),
         abstract = gsub("vs.", "", abstract),
         abstract = gsub("[0-9]", "", abstract), 
         abstract = gsub("([\\\\/%<,=+:?\\]\\[*)(])", "", abstract, perl=T)) %>% 
  unnest_tokens(input = abstract, output = sentences, 
                token = "regex", pattern = "\\. ") 
n_char = as.data.frame(nchar(version_1$sentences))
colnames(n_char) = "n_char"
version_1 <- cbind(version_1, n_char) %>% 
  filter(n_char > 25) %>% 
  select(-human, -nonhuman, -n_char); rm(n_char)

# the unnest_sentences version 
version_2 <- pubmed_data %>%
  unnest_tokens(input = "abstract", output = "sentence", token = "sentences")

version_3 <- pubmed_data %>% 
  mutate(abstract = gsub("[0-9]", "", abstract), 
         abstract = gsub("  |   ", " ", abstract), 
         abstract = gsub("([\\\\/%<,-=+:?.\\]\\[*)(])", "", abstract, perl=T),
         abstract = strsplit(abstract, "(?<=.{300})", perl = TRUE)
         ) %>% unnest(abstract) %>% 
  rename(sentences = abstract)


classify_diversities <- version_3 %>% 
  mutate(rowid = row_number(),
         sentence_id = paste0(fk_pmid, "_", rowid)) %>% 
  select(sentence_id, sentences) %>% 
  unnest_tokens(word, sentences) %>% 
  filter(word %in% all_diversity_cats) %>% 
  as_tidytable() %>%
  mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$ancestry), "zqx)\\b"), collapse = "|")), "ancestry", word)) %>% 
  mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$cultural), "zqx)\\b"), collapse = "|")), "cultural", term)) %>% 
  mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$disability), "zqx)\\b"), collapse = "|")), "disability", term)) %>%
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$diversity), "zqx)\\b"), collapse = "|")),  "diversity", term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$equity), "zqx)\\b"), collapse = "|")), "equity", term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$social_class), "zqx)\\b"), collapse = "|")),  "socioeconomic",  term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$lifecourse), "zqx)\\b"), collapse = "|")),  "lifecourse",  term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$migration), "zqx)\\b"), collapse = "|")),  "migration",  term)) %>%
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$minority), "zqx)\\b"), collapse = "|")),  "minority",  term)) %>%
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$population), "zqx)\\b"), collapse = "|")),  "population",  term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$race_ethnicity), "zqx)\\b"), collapse = "|")),  "race/ethnicity",  term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$sex_gender), "zqx)\\b"), collapse = "|")),  "sex/gender",  term)) %>% 
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(divictionary$sexuality), "zqx)\\b"), collapse = "|")),  "sexuality",  term)) %>% 
  mutate.(ancestry_cnt = ifelse( str_detect( term,  "\\b(ancestry)\\b"),  1,  0)) %>% 
  mutate.(cultural_cnt = ifelse( str_detect( term,  "\\b(cultural)\\b"),  1,  0)) %>%
  mutate.(class_cnt = ifelse( str_detect( term,  "\\b(socioeconomic)\\b"),  1,  0)) %>%
  mutate.(diversity_cnt = ifelse( str_detect( term,  "\\b(diversity)\\b"),  1,  0)) %>%
  mutate.(disability_cnt = ifelse( str_detect( term,  "\\b(disability)\\b"),  1,  0)) %>%
  mutate.(equity_cnt = ifelse( str_detect( term,  "\\b(equity)\\b"),  1,  0)) %>%
  mutate.(lifecourse_cnt = ifelse( str_detect( term,  "\\b(lifecourse)\\b"),  1,  0)) %>%
  mutate.(minority_cnt = ifelse( str_detect( term,  "\\b(minority)\\b"),  1,  0)) %>%
  mutate.(migration_cnt = ifelse( str_detect( term,  "\\b(migration)\\b"),  1,  0)) %>%
  mutate.(population_cnt = ifelse( str_detect( term,  "\\b(population)\\b"),  1,  0)) %>%
  mutate.(racial_cnt = ifelse( str_detect( term,  "\\b(race/ethnicity)\\b"),  1,  0)) %>%
  mutate.(sexgender_cnt = ifelse( str_detect( term,  "\\b(sex/gender)\\b"),  1,  0)) %>%
  mutate.(sexuality_cnt = ifelse( str_detect( term,  "\\b(sexuality)\\b"),  1,  0)) %>%  
  group_by(sentence_id) %>% 
  summarise(across(ancestry_cnt:sexuality_cnt, sum)) %>% 
  # in this version i will count both ancestry and population b/c divers* will be used in those contexts 
  mutate(total_cnt = ancestry_cnt + cultural_cnt + class_cnt + disability_cnt + diversity_cnt + equity_cnt + 
           lifecourse_cnt + minority_cnt + migration_cnt + population_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  select(sentence_id, total_cnt, everything()) %>%
  arrange(-total_cnt) %>% ungroup() %>%
  # get the intial social_diversity count (before polysemeR correction)
  mutate(soc_div_raw = if_else(diversity_cnt > 0 & total_cnt > diversity_cnt, diversity_cnt, 0)) %>% 
  mutate(fk_pmid = gsub("_.*", "", sentence_id)) %>% 
  group_by(fk_pmid, soc_div_raw) %>% 
  summarise(soc_diversity = sum(soc_div_raw)) %>% 
  select(fk_pmid, soc_diversity) %>% 
  filter(soc_diversity > 0) %>% 
  mutate(fk_pmid = as.integer(fk_pmid))

pubmed_classified <- pubmed_data %>% 
  left_join(classify_diversities, by = "fk_pmid") %>% 
  mutate(soc_diversity = replace_na(soc_diversity, 0)) %>% 
  filter(soc_diversity == 1) %>% 
  polysemeR(fk_pmid, abstract) %>% 
  mutate(soc_diversity = ifelse(heterogeneity == 1, 0, 1)) %>% 
  filter(soc_diversity == 1) %>% 
  select(fk_pmid, abstract, soc_diversity, human, nonhuman)

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(pubmed_classified, "diversity_chk_070521_0349.csv")


#sentences_with_diversity <- classify_diversities %>% 
#  filter(diversity_cnt > 0 & soc_div_raw < 1) %>% 
#  group_by(fk_pmid, diversity_cnt) %>% 
#  summarise(diversity_cnt = sum(diversity_cnt)) %>% 
#  mutate(fk_pmid = as.integer(fk_pmid)) %>% 
#  left_join(raw_sentences, by = "fk_pmid") %>% 
#  group_by(fk_pmid) %>%
#  summarize(abstract = str_c(sentences, collapse = " ")) %>%
#  ungroup() %>% 
#  arrange(fk_pmid)

#positive_cases <- c("diverse populations")

# classify then 
#sentences_with_diversity %>% 
#  unnest_tokens(words, abstract, token = "ngrams", n = 2) %>% 
#  filter(words %in% positive_cases) %>% 
#  mutate(soc_diversity = 1) %>% 
#  group_by(fk_pmid, words) %>% 
#  summarize(soc_diversity = sum(soc_diversity)) %>%  
#  select(fk_pmid, soc_diversity) %>% 
#  bind_rows(diversity_correlated)

# LAST STEP: left_join and replace_na(., 0) on the last column 


chk <- pubmed_classified %>% 
  unnest_tokens(sentences, abstract, token = "sentences") %>% 
  filter(grepl("diverse", sentences))



















