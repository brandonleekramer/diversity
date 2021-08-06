

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
all_diversity_cats <- all_diversity_cats[!all_diversity_cats %in% c("population", "populations")]
dictionary <- readr::read_csv("diversity_project - humanizeR.csv")
nonhuman <- dictionary %>% filter(category == "nonhuman")
nonhuman <- na.omit(nonhuman$terms)


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
         abstract = strsplit(abstract, "(?<=.{300})", perl = TRUE)) %>% 
  unnest(abstract) %>% rename(sentences = abstract)

# https://stackoverflow.com/questions/46260274/split-string-by-n-words-in-r
version_4 <- pubmed_data %>% 
  mutate(abstract = gsub("[0-9]", "", abstract), 
         abstract = gsub("  |   ", " ", abstract), 
         abstract = gsub("([\\\\/%<,-=+:?.\\]\\[*)(])", "", abstract, perl=T))

split_every <- function(x, n, pattern, collapse = pattern, ...) {
  x_split <- strsplit(x, pattern, perl = TRUE, ...)[[1]]
  out <- character(ceiling(length(x_split) / n))
  for (i in seq_along(out)) {
    entry <- x_split[seq((i - 1) * n + 1, i * n, by = 1)]
    out[i] <- paste0(entry[!is.na(entry)], collapse = collapse)
  }
  out
}

test <- split_every(version_4$abstract, 50, pattern = " ")


corpus <- "I need to break this corpus into chunks of ~3 words each"

library(tokenizers)
library(tidytext)
library(tibble)



chk_this <- pubmed_data$abstract %>% 
  chunk_text(300) %>%
  enframe(name = "group", value = "text") %>%
  unnest_tokens(word, text)
  
#str_c(sub(" diversity.*", "", x), " diversity ", sub(".*diversity ", "", x))


x <- "something something something. i want to extract everything before diversity and then after diversity."

chk <- pubmed_data %>% 
  #top_n(5, fk_pmid) %>% 
  filter(diversity == 1) %>% 
  mutate(abstract = gsub("[0-9]", "", abstract), 
         abstract = gsub("  |   ", " ", abstract), 
         abstract = gsub("([\\\\/%<,-=+:?.\\]\\[*)(])", "", abstract, perl=T)) %>% 
  #unnest_tokens(input = abstract, output = sentences, 
  #              token = "regex", pattern = "diversity ") %>% 
  mutate()

#word(x, 1, 5, sep=" ")

chk <- all_diversity_cats[!all_diversity_cats %in% c("population", "populations")]

x <- "Managing and adapting to climate change in urban areas will become increasingly important as urban populations grow, especially because unique features of cities amplify climate change impacts. High impervious cover exacerbates impacts of climate warming through urban heat island effects and of heavy rainfall by magnifying runoff and flooding. Concentration of human settlements along rivers and coastal zones increases exposure of people and infrastructure to climate change hazards, often disproportionately affecting those who are least prepared. Nature-based strategies (NBS), which use living organisms, soils and sediments, and/or landscape features to reduce climate change hazards, hold promise as being more flexible, multi-functional and adaptable to an uncertain and non-stationary climate future than traditional approaches. Nevertheless, future research should address the effectiveness of NBS for reducing climate change impacts and whether they can be implemented at scales appropriate to climate change hazards and impacts. Further, there is a need for accurate and comprehensive cost-benefit analyses that consider disservices and co-benefits, relative to grey alternatives, and how costs and benefits are distributed across different communities. NBS are most likely to be effective and fair when they match the scale of the challenge, are implemented with input from diverse voices and are appropriate to specific social, cultural, ecological and technological contexts. This article is part of the theme issue 'Climate change and ecosystems: threats, opportunities and solutions'."
word(x, 1, sep="diverse")
X2 = "here is another test string, with following text"
Y <- sub('.*(,.*)','', X2)
sub('.*diverse\\s*','\\1', x)





version_4 <- pubmed_data %>% 
  top_n(25, fk_pmid) %>% 
  tidytable::mutate.(sentences = strsplit(abstract, "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE)) %>% 
  tidytable::unnest.(sentences) %>% 
  tidytable::mutate.(rowid = row_number(),
         sentence_id = paste0(fk_pmid, "_", rowid)) %>% 
  tidytable::select.(sentence_id, sentences) %>% 
  unnest_tokens(word, sentences)



classify_diversities <- version_4 %>% 
  mutate(rowid = row_number(),
         sentence_id = paste0(fk_pmid, "_", rowid)) %>% 
  select(sentence_id, sentences) %>% 
  unnest_tokens(word, sentences) %>% 
  filter(word %in% c(all_diversity_cats, nonhuman)) %>% 
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
  mutate.(term = ifelse( str_detect(word,  paste(c("\\b(?i)(zqx", na.omit(nonhuman), "zqx)\\b"), collapse = "|")),  "nonhuman",  term)) %>%
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
  mutate.(nonhuman = ifelse( str_detect( term,  "\\b(nonhuman)\\b"),  1,  0)) %>% 
  group_by(sentence_id) %>% 
  summarise(across(ancestry_cnt:nonhuman, sum)) %>% 
  # in this version i will count both ancestry and population b/c divers* will be used in those contexts 
  mutate(total_cnt = ancestry_cnt + cultural_cnt + class_cnt + disability_cnt + diversity_cnt + equity_cnt + 
         lifecourse_cnt + minority_cnt + migration_cnt + population_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  select(sentence_id, total_cnt, everything()) %>%
  arrange(-total_cnt) %>% ungroup() %>%
  # get the intial social_diversity count (before polysemeR correction)
  mutate(soc_div_raw = if_else(diversity_cnt > 0 & total_cnt > diversity_cnt, diversity_cnt, 0)) %>% 
  # but if diversity is mentioned in the same sentence as any animal term then don't count it as social diversity 
  mutate(soc_div_raw = if_else(soc_div_raw > 0 & nonhuman > 0, 0, soc_div_raw)) %>% 
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

# plan::::
# update all of the dictionaries 
# run a model with all the versions of social_diversity 
# original, polysemeR, unnest_sentences, character variations, word variations 
# (would have to figure out tokenizers package)
# char: 200, 300, 400, 500 ??
# words: 10, 25, 50, 100, 200, 300 ?? 



# https://docs.google.com/document/d/1k8skQekkT9T3Xs-Ge4U6ViUkDTnagkTco3H3rkPDuo0/edit 
# https://www.thomasvanhoey.com/rbootcamp/3-tidytext#10
# https://stackoverflow.com/questions/11619616/how-to-split-a-string-into-substrings-of-a-given-length
# https://stackoverflow.com/questions/62525005/break-corpus-into-chunks-of-n-words-each-in-r
# https://github.com/ropensci/tokenizers/blob/master/R/chunk-text.R
# https://stackoverflow.com/questions/35304900/split-paragraph-into-sentences-in-r <<<<<<<<<<<<<<<<<<<











