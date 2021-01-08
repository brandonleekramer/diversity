
######################################################################################################## ingestion/cleaning 

#rm(list = ls())

for (pkg in c("tidyverse", "tidytext", "RPostgreSQL", "data.table", "maditr")) {library(pkg, character.only = TRUE)}

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
                         "SELECT fk_pmid, year, abstract  
                          FROM pubmed_2021.abstract_data 
                          --WHERE year > 1989
                          LIMIT 10000;")

# disconnect from postgresql database 
dbDisconnect(conn)

# adding custom set of stopwords 
my_stopwords <- tibble(word = c(as.character(1:14), "[0-9]", "women.recommendations",
                                "rights", "reserved", "copyright", "elsevier",  
                                "cs.man.ac.uk", "sexes.please", "movements.baby"))

# tokenizing the abstract data into words 
pubmed_abstract_data <- pubmed_data %>% 
  rename(id = fk_pmid) %>% 
  unnest_tokens(word, abstract) %>% 
  # run this since the dataset is so huge now 
  anti_join(stop_words) %>% 
  anti_join(my_stopwords) 

######################################################################################################## filter to diversity terms

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
  dt_mutate(di_aging = ifelse(test = str_detect(string = term, pattern = "\\b(aging)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_ancestry = ifelse(test = str_detect(string = term, pattern = "\\b(ancestry)\\b"), yes = 1, no = 0)) %>% 
  dt_mutate(di_cultural = ifelse(test = str_detect(string = term, pattern = "\\b(cultural)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_class = ifelse(test = str_detect(string = term, pattern = "\\b(socio-economic)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_diversity = ifelse(test = str_detect(string = term, pattern = "\\b(diversity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_genetic = ifelse(test = str_detect(string = term, pattern = "\\b(genetic)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_minority = ifelse(test = str_detect(string = term, pattern = "\\b(minority)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_population = ifelse(test = str_detect(string = term, pattern = "\\b(population)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_racial = ifelse(test = str_detect(string = term, pattern = "\\b(race/ethnicity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_sexgender = ifelse(test = str_detect(string = term, pattern = "\\b(sex/gender)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(di_sexuality = ifelse(test = str_detect(string = term, pattern = "\\b(sexuality)\\b"), yes = 1, no = 0)) 

######################################################################################################## get full counts 

test <- general_pop_terms %>% 
  group_by(id, year, word, term) %>% 
  summarise(across(di_aging:di_sexuality, sum)) %>% 
  mutate(di_total = di_aging + di_ancestry + di_cultural + di_class + di_diversity + di_genetic +  
         di_minority + di_population + di_racial + di_sexgender + di_sexuality) %>% 
  select(id, year, word, term, di_total, everything()) %>%
  ungroup() %>% 
  mutate(soc_diversity = if_else(di_diversity > 0 & (di_aging > 0 | di_cultural > 0 | di_class > 0 | 
                         di_minority > 0 | di_racial > 0 | di_sexgender > 0 | di_sexuality > 0 ), 1, 0)) %>%
  select(check, id, year, word, term, di_total, everything()) %>% 
  arrange(-di_total) %>% 
  filter(check == 1)

# NEED TO CHECK THIS CREATE GROUP BY ARTICLE THAT CALCULATES TRUE DIVERSITY OUTCOMES 


h1_set_counts_full <- general_pop_terms %>% 
  group_by(term) %>% count(sort = TRUE)  

h1_subset_counts_full <- general_pop_terms %>% 
  group_by(word, term) %>% count(sort = TRUE)  

h1_set_counts_trends <- general_pop_terms %>% 
  group_by(term, year) %>% count(sort = TRUE) 

h1_setset_counts_trends <- general_pop_terms %>% 
  group_by(word, term, year) %>% count(sort = TRUE)  

setwd("~/git/diversity/data/text_results/")
write_rds(h1_set_counts_full, "h1_set_counts_full.rds")
write_rds(h1_subset_counts_full, "h1_subset_counts_full.rds")
write_rds(h1_set_counts_trends, "h1_set_counts_trends.rds")
write_rds(h1_setset_counts_trends, "h1_setset_counts_trends.rds")

######################################################################################################## convert to percentages

test <- general_pop_terms %>%
  replace(is.na(.), 0) %>% 
  group_by(id) %>% 
  summarise(across(di_aging:di_sexuality, sum)) %>% 
  ungroup()

  ungroup() %>%
  dt_mutate(soc_di_diversity = if_else(di_diversity > 0 & (di_aging > 0 | di_cultural > 0 | di_class > 0 | 
            di_minority > 0 | di_racial > 0 | di_sexgender > 0 | di_sexuality > 0 ), di_diversity, 0)) %>% 
  dt_mutate(soc_prc_diversity = ifelse(soc_di_diversity > 0, prc_ancestry, 0)) %>% 
  select(year, cnt_diversity, prc_diversity, soc_di_diversity, soc_prc_diversity, everything())


#gen_pop_prc_counts <- gen_pop_prc_counts %>%
replace(is.na(.), 0) %>% 
  dt_mutate(tr_di_diversity = ifelse(cnt_diversity > 0 & (cnt_aging > 0 | cnt_cultural > 0 | cnt_class > 0 | 
                                                            cnt_minority > 0 | cnt_racial > 0 | cnt_sexgender > 0 | cnt_sexuality > 0 ), cnt_diversity, 0)) %>% 
  dt_mutate(tr_prc_diversity = ifelse(tr_di_diversity > 0, prc_ancestry, 0)) %>% 
  select(year, cnt_diversity, prc_diversity, tr_di_diversity, tr_prc_diversity, everything())

test %>% 
  filter(di_diversity > 1)


# total articles each year 
gen_pop_prc_counts <- pubmed_data %>% 
  group_by(year) %>% 
  count(year) %>% 
  ungroup() %>% 
  rename(total = n) 

# articles with term mentioned each year 
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_racial == 1) %>% #racial 
  group_by(year) %>% 
  summarise(cnt_racial = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>%  
  mutate(prc_racial = round(cnt_racial / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_cultural == 1) %>% #cultural
  group_by(year) %>% 
  summarise(cnt_cultural = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_cultural = round(cnt_cultural / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_population == 1) %>% #population
  group_by(year) %>% 
  summarise(cnt_population = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_population = round(cnt_population / total * 100, digits = 2)) 
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_ancestry == 1) %>% # ancestry
  group_by(year) %>% 
  summarise(cnt_ancestry = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_ancestry = round(cnt_ancestry / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_diversity == 1) %>% #diversity
  group_by(year) %>% 
  summarise(cnt_diversity = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_diversity = round(cnt_diversity / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_genetic == 1) %>% #genetic
  group_by(year) %>% 
  summarise(cnt_genetic = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_genetic = round(cnt_genetic / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_sexgender == 1) %>% #sex/gender
  group_by(year) %>% 
  summarise(cnt_sexgender = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_sexgender = round(cnt_sexgender / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_sexuality == 1) %>% # sexuality
  group_by(year) %>% 
  summarise(cnt_sexuality = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_sexuality = round(cnt_sexuality / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_aging == 1) %>% # aging
  group_by(year) %>% 
  summarise(cnt_aging = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_aging = round(cnt_aging / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_class == 1) %>% # class
  group_by(year) %>% 
  summarise(cnt_class = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_class = round(cnt_class / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(di_minority == 1) %>% # minority
  group_by(year) %>% 
  summarise(cnt_minority = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_minority = round(cnt_minority / total * 100, digits = 2))

setwd("~/git/diversity/data/text_results/")
write_rds(general_pop_terms, "h1_counts.rds")
write_rds(gen_pop_prc_counts, "h1_percentages.rds")








