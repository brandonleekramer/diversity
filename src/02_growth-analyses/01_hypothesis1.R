
######################################################################################################## testing h1 

# this file produces 6 outputs 
# 1. counts for all term sets for all years (after recoding) 
# 2. counts for all term subsets for all years (before and after recoding)
# 3. counts for all term sets by year (after recoding) 
# 4. counts for all term subsets by year (before and after recoding)
# 5. percentages for all terms by year (after coding)
# 6. a matrix that provides all the counts of terms within each publication 
# note: dataframe #6 can be written to PostgreSQL later to do additional analyses 

############################################################################################## install.packages (for slurm) 

#rm(list = ls())

install.packages("dplyr")
install.packages("tidytext")
install.packages("RPostgreSQL")
install.packages("data.table")
install.packages("maditr")

library("dplyr")
library("tidytext")
library("RPostgreSQL")
library("data.table")
library("maditr")

######################################################################################################## ingestion/cleaning 

data(stop_words)

# connect to postgresql to get our data
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))

# query the users_gh data (table of all github users) 
pubmed_data <- dbGetQuery(conn, 
                         "SELECT DISTINCT(fk_pmid), year, abstract  
                          FROM pubmed_2021.abstract_data 
                          WHERE year > 1990 AND year < 2021;")

# disconnect from postgresql database 
dbDisconnect(conn)

# total articles each year 
# banking this df now to optimize our memory for later 
gen_pop_prc_counts <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ungroup() %>% 
  rename(total = n) 

# tokenizing the abstract data into words 
pubmed_abstract_data <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  rename(id = fk_pmid) %>% 
  unnest_tokens(word, abstract) %>% 
  # run this since the dataset is so huge now 
  anti_join(stop_words) 

# clean up memory 
# rm(pubmed_data, stop_words, my_stopwords)

############################################################################################# filter to diversity terms

# lets draw all of our strings from the diversity_dictionary (divictionary)
# setwd("~/Documents/Diversity/Data")
setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 

divictionary_string <- c(na.omit(divictionary$aging), na.omit(divictionary$ancestry),
                         na.omit(divictionary$cultural), na.omit(divictionary$diversity),
                         na.omit(divictionary$genetic), na.omit(divictionary$social_class),
                         na.omit(divictionary$minority), na.omit(divictionary$population),
                         na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                         na.omit(divictionary$sexuality))

pubmed_abstract_data <- pubmed_abstract_data %>% 
  filter(word %in% divictionary_string)

######################################################################################################## convert to sets 

general_pop_terms <- pubmed_abstract_data %>% 
  as.data.table() %>%
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$aging), "zqx)\\b"), collapse = "|")), 
                          yes = "aging", no = word)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$ancestry), "zqx)\\b"), collapse = "|")), 
                          yes = "ancestry", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$cultural), "zqx)\\b"), collapse = "|")), 
                          yes = "cultural", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$diversity), "zqx)\\b"), collapse = "|")), 
                          yes = "diversity", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$genetic), "zqx)\\b"), collapse = "|")), 
                          yes = "genetic", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$social_class), "zqx)\\b"), collapse = "|")), 
                          yes = "socio-economic", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$minority), "zqx)\\b"), collapse = "|")), 
                          yes = "minority", no = term)) %>%
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$population), "zqx)\\b"), collapse = "|")), 
                          yes = "population", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$race_ethnicity), "zqx)\\b"), collapse = "|")), 
                          yes = "race/ethnicity", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$sex_gender), "zqx)\\b"), collapse = "|")), 
                          yes = "sex/gender", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$sexuality), "zqx)\\b"), collapse = "|")), 
                          yes = "sexuality", no = term)) %>% 
  dt_mutate(aging_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(aging)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(ancestry_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(ancestry)\\b"), yes = 1, no = 0)) %>% 
  dt_mutate(cultural_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(cultural)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(class_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(socio-economic)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(diversity_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(diversity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(genetic_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(genetic)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(minority_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(minority)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(population_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(population)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(racial_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(race/ethnicity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(sexgender_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(sex/gender)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(sexuality_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(sexuality)\\b"), yes = 1, no = 0)) 

################################################################################################### adding "social" diversity 

# this creates a variable where the term diversity is only used alongside another diversity set 
# basically, i mirror the diversity_cnt into soc_diversity so that we can group_by and count next 
diversity_terms_matrix <- general_pop_terms %>% 
  group_by(id, year) %>% 
  summarise(across(aging_cnt:sexuality_cnt, sum)) %>% 
  mutate(total_cnt = aging_cnt + ancestry_cnt + cultural_cnt + class_cnt + 
           diversity_cnt + minority_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  select(id, year, total_cnt, everything()) %>%
  arrange(-total_cnt) %>% ungroup() %>%
  mutate(soc_diversity = if_else(diversity_cnt > 0 & total_cnt != diversity_cnt, diversity_cnt, 0)) 

general_pop_terms <- diversity_terms_matrix %>% 
  select(id, soc_diversity) %>% 
  left_join(general_pop_terms, by = "id") %>% 
  select(-soc_diversity, soc_diversity)

######################################################################################################## get full counts 

# NEED TO CHECK THIS CREATE GROUP BY ARTICLE THAT CALCULATES TRUE DIVERSITY OUTCOMES 

# counts of term sets for all years 
h1_set_counts_full <- general_pop_terms %>% 
  group_by(term) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 
h1_set_counts_full <- general_pop_terms %>% 
  filter(soc_diversity == 1 & term == "diversity") %>% 
  group_by(term) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (social)")) %>% 
  bind_rows(h1_set_counts_full) %>% 
  arrange(-n)

# counts of term subsets for all years 
h1_subset_counts_full <- general_pop_terms %>% 
  group_by(word, term) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 
h1_subset_counts_full <- general_pop_terms %>% 
  filter(soc_diversity == 1 & term == "diversity") %>% 
  group_by(word, term) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (social)")) %>% 
  bind_rows(h1_subset_counts_full) %>% 
  arrange(-n)

# counts of all terms by year 
h1_set_counts_trends <- general_pop_terms %>% 
  group_by(term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 
h1_set_counts_trends <- general_pop_terms %>% 
  filter(soc_diversity == 1 & term == "diversity") %>% 
  group_by(term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (social)")) %>% 
  bind_rows(h1_set_counts_trends) %>% 
  arrange(-n)

# counts of term subsets by year
h1_subset_counts_trends <- general_pop_terms %>% 
  group_by(word, term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 
h1_subset_counts_trends <- general_pop_terms %>% 
  filter(soc_diversity == 1 & term == "diversity") %>% 
  group_by(word, term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (social)")) %>% 
  bind_rows(h1_subset_counts_trends) %>% 
  arrange(-n)

setwd("~/git/diversity/data/text_results/")
write_rds(h1_set_counts_full, "h1_set_counts_full.rds")
write_rds(h1_subset_counts_full, "h1_subset_counts_full.rds")
write_rds(h1_set_counts_trends, "h1_set_counts_trends.rds")
write_rds(h1_subset_counts_trends, "h1_subset_counts_trends.rds")
write_rds(diversity_terms_matrix %>% rename(fk_pmid = id), "h1_diversity_terms_matrix.rds")

######################################################################################################## convert to percentages

# articles with term mentioned each year 
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(aging_cnt == 1) %>% # aging
  group_by(year) %>% 
  summarise(cnt_aging = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_aging = round(cnt_aging / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(ancestry_cnt == 1) %>% # ancestry
  group_by(year) %>% 
  summarise(cnt_ancestry = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_ancestry = round(cnt_ancestry / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(class_cnt == 1) %>% # class
  group_by(year) %>% 
  summarise(cnt_class = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_class = round(cnt_class / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(cultural_cnt == 1) %>% #cultural
  group_by(year) %>% 
  summarise(cnt_cultural = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_cultural = round(cnt_cultural / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(diversity_cnt == 1) %>% #diversity
  group_by(year) %>% 
  summarise(cnt_diversity = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_diversity = round(cnt_diversity / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(soc_diversity == 1) %>% #diversity (social)
  group_by(year) %>% 
  summarise(cnt_soc_diversity = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_diversity = round(cnt_soc_diversity / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(genetic_cnt == 1) %>% #genetic
  group_by(year) %>% 
  summarise(cnt_genetic = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_genetic = round(cnt_genetic / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(minority_cnt == 1) %>% # minority
  group_by(year) %>% 
  summarise(cnt_minority = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_minority = round(cnt_minority / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(population_cnt == 1) %>% #population
  group_by(year) %>% 
  summarise(cnt_population = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_population = round(cnt_population / total * 100, digits = 2)) 
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(racial_cnt == 1) %>% #racial 
  group_by(year) %>% 
  summarise(cnt_racial = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>%  
  mutate(prc_racial = round(cnt_racial / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(sexgender_cnt == 1) %>% #sex/gender
  group_by(year) %>% 
  summarise(cnt_sexgender = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_sexgender = round(cnt_sexgender / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(sexuality_cnt == 1) %>% # sexuality
  group_by(year) %>% 
  summarise(cnt_sexuality = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_sexuality = round(cnt_sexuality / total * 100, digits = 2))

setwd("~/git/diversity/data/text_results/")
write_rds(gen_pop_prc_counts, "h1_set_prc_trends.rds")








