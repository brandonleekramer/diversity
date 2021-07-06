
############################# step 1: random samples of all diversity and social diversity to label 

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")

# connect to postgresql to get our data
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))

# query the users_gh data (table of all github users) 
test <- dbGetQuery(conn, "SELECT * FROM pubmed_2021.all_diversity_abstracts ;")

# disconnect from postgresql database 
dbDisconnect(conn)

diversity_only = all_diversity_abstracts %>% 
  select(fk_pmid, year, abstract, diversity, soc_diversity, ends_with("totals")) %>% 
  filter(diversity == 1)

reg_div = diversity_only %>% 
  filter(diversity == 1 & soc_diversity != 1) 
  #%>% count()
# 39565

reg_div_rand = sample_n(reg_div, 500)

soc_div = diversity_only %>% 
  filter(soc_diversity == 1) 
  #%>% count()
# 11789

soc_div_rand = sample_n(soc_div, 500)

both_div_rand = bind_rows(reg_div_rand, soc_div_rand)

both_div_rand <- both_div_rand[sample(nrow(both_div_rand)),]

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(both_div_rand, "diversity_datatolabel_061221.csv")

############################# step 2: confusion matrix after manually labeling 1000 abstracts (500 each) 

setwd("~/git/diversity/data/sensitivity_checks/")
labeled_data <- read_csv("diversity_labeled_061221.csv") %>% select(-X4)

# double-check split is still correct 
labeled_data %>% 
  group_by(soc_diversity) %>% 
  count()

# checks how many are properly labeled 
labeled_eda <- labeled_data %>% 
  mutate(labels_match = if_else(((blk_code == 1 & soc_diversity == 1) | 
                                 (blk_code == 0 & soc_diversity == 0)), TRUE, FALSE)) 

labeled_eda %>% 
  group_by(labels_match) %>% 
  count()
#703 match and 299 do not 

labeled_eda %>% 
  filter(labels_match == TRUE) %>% 
  group_by(blk_code) %>% 
  count()
# 243 predicted yes/actual yes (true positives)
# 460 predicted no/actual no (true negatives)

labeled_eda %>% 
  filter(labels_match == FALSE) %>% 
  group_by(blk_code) %>% 
  count()
# 257 predicted yes/actual no (false positives)
# 40 predicted no/actual yes (false negatives)

# sensitivity 

false_positives <- labeled_eda %>% 
  filter(labels_match == FALSE & blk_code == 0 ) #%>% 
  #mutate(species = ifelse(test = str_detect(abstract, "\\b(?i)(species)\\b"), yes = 1, no = 0)) 

false_positive_sentences <- labeled_eda %>% 
  filter(labels_match == FALSE & blk_code == 0 ) %>% 
  unnest_tokens(output = sentence, 
                input = abstract, 
                token = "sentences", 
                drop = FALSE) %>% 
  filter(grepl("diversity|diverse|diversified", sentence)) 

false_positive_grams <- false_positive_sentences %>% 
  unnest_tokens(five_grams, abstract, token = "ngrams", n = 5)  %>% 
  filter(grepl("^diversity|^diverse|^diversified|diversity$|diverse$|diversified$", five_grams)) 

data("stop_words")

fp_counts <- false_positives %>% 
  unnest_tokens(word, abstract) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

fp_bigrams <- false_positives %>% 
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  filter(grepl("diversity", bigram))

# looks like adult and women create false positives 
# could keep working on animal exclusion model to improve this or build a BERT model 

true_positives <- labeled_eda %>% 
  filter(labels_match == TRUE & blk_code == 1 ) 

true_positives_detailed <- true_positives %>% 
  select(fk_pmid, blk_code, labels_match) %>% 
  left_join(all_diversity_abstracts, by = "fk_pmid") 

tp_correlations_prep <- true_positives_detailed %>% 
  select(soc_div_terms:white)
vis_miss(tp_correlations_prep)
tp_correlations <- cor(tp_correlations_prep)

all_diversity_prep <- all_diversity_abstracts %>% 
  select(soc_div_terms:white)
all_div_correlations <- data.frame(cor(all_diversity_prep))
select_cut <- all_div_correlations %>% select(soc_div_terms, diversity, soc_diversity) %>% arrange(-soc_diversity)


setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(true_positives_detailed, "true_positives.csv")
