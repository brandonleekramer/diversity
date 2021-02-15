
#### this has been converted to a function that is run iteratively through the _by_year.R file 
#### can just run this for one year by changing the analysis_timeframe parameter

####################################################################################### install.packages (for slurm) 

#rm(list = ls())
#analysis_timeframe <- "1990"

#install.packages("readr", repos = "http://cran.us.r-project.org")
#install.packages("stringi", repos = "http://cran.us.r-project.org", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#install.packages("stringr", repos = "http://cran.us.r-project.org", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#install.packages("dplyr", repos = "http://cran.us.r-project.org")
#install.packages("tidytext", repos = "http://cran.us.r-project.org")
#install.packages("widyr", repos = "http://cran.us.r-project.org")
#install.packages("RPostgreSQL", repos = "http://cran.us.r-project.org")
#install.packages("data.table", repos = "http://cran.us.r-project.org")
#install.packages("maditr", repos = "http://cran.us.r-project.org")
#install.packages("purrr", repos = "http://cran.us.r-project.org")
#install.packages("tm", repos = "http://cran.us.r-project.org")

######################################################################################################## testing h1 

# this file produces 6 outputs 
# 1. counts for all term sets for all years (after recoding) 
# 2. counts for all term subsets for all years (before and after recoding)
# 3. counts for all term sets by year (after recoding) 
# 4. counts for all term subsets by year (before and after recoding)
# 5. percentages for all terms by year (after coding)
# 6. a matrix that provides all the counts of terms within each publication 
# note: dataframe #6 can be written to PostgreSQL later to do additional analyses 

test_h1 <- function(analysis_timeframe){

library("readr")
library("dplyr") 
library("stringr")  
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("purrr")
data(stop_words)

######################################################################################## load diversity dictionaries

# lets draw all of our strings from the diversity_dictionary (divictionary)
# setwd("~/Documents/Diversity/Data")
setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 

divictionary_string <- c(na.omit(divictionary$aging), na.omit(divictionary$ancestry),
                         na.omit(divictionary$social_class), na.omit(divictionary$cultural), 
                         na.omit(divictionary$disability), na.omit(divictionary$diversity), 
                         na.omit(divictionary$equity), na.omit(divictionary$migration),
                         na.omit(divictionary$minority), na.omit(divictionary$population),
                         na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                         na.omit(divictionary$sexuality))

diversity_only <- c(na.omit(divictionary$aging), na.omit(divictionary$ancestry),
                    na.omit(divictionary$social_class), na.omit(divictionary$cultural), 
                    na.omit(divictionary$disability), na.omit(divictionary$diversity), # this right? 
                    na.omit(divictionary$equity), na.omit(divictionary$migration),
                    na.omit(divictionary$minority), na.omit(divictionary$population),
                    na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                    na.omit(divictionary$sexuality))

################################################################################################# data ingestion/cleaning 

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
                          WHERE year =", analysis_timeframe, ";"))

# disconnect from postgresql database 
dbDisconnect(conn)

str_c("Finishing data pull at: ", Sys.time())


######################################################################################## pre-processing steps 

# total articles each year 
# we are banking this df of the total abstract counts for each year now 
# so that we can optimize our memory for later (removing raw pubmed_data soon)
gen_pop_prc_counts <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ungroup() %>% 
  rename(total = n) 

# pulls in the preprocessing dictionary 
preprocessing_terms <- read_csv("diversity_project - preprocessing.csv") %>%
  select(original_string, new_string) %>% tibble::deframe()

# animal exclusion clause
animal_exclusion_clause <- read_csv("diversity_project - animals.csv")
animal_exclusion_clause <- paste(c("\\b(?i)(zqx", na.omit(animal_exclusion_clause$animals), "zqx)\\b"), collapse = "|")

# convert text to lowercase 
pubmed_data <- pubmed_data %>% mutate(abstract = tolower(abstract))

# replaces all the hyphenated and compound words 
# helps to reduce false positives (e.g. double_blind)
pubmed_data$abstract <- pubmed_data$abstract %>% 
  str_replace_all(preprocessing_terms)

# remove abstracts with animals to reduce false positive animal studies 
pubmed_data <- pubmed_data %>% 
  dt_mutate(human_study = ifelse(test = str_detect(string = abstract, 
            pattern = "\\b(human|humans|person|persons|people)\\b"), yes = 1, no = 0)) %>% 
  dt_mutate(animal_study = ifelse(test = str_detect(string = abstract, 
            pattern = animal_exclusion_clause), yes = 1, no = 0)) %>% 
  filter(human_study == 1 | animal_study == 0)

######################################################################################## unnesting phase 

str_c("Starting to unnest tokens at: ", Sys.time())

# tokenizing the abstract data into words 
pubmed_abstract_data <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  rename(id = fk_pmid) %>% 
  unnest_tokens(word, abstract) %>% 
  # run this since the dataset is so huge now 
  anti_join(stop_words) %>% 
  # filter down to only include diversity terms 
  filter(word %in% divictionary_string)

# clean up memory 
rm(pubmed_data)

str_c("Finished unnesting tokens at: ", Sys.time())

############################################################################### save all the diversity abstract ids 

# this saves all of the unique ids that mention h1 diversity sets 
# basically, this table of uniques ids can go back to postgresql 
setwd("~/git/diversity/data/text_results/h1_results/")
write_rds(pubmed_abstract_data %>% 
          filter(word %in% diversity_only) %>% distinct(id), 
          str_c("h1_diversity_ids_",analysis_timeframe,".rds"))

################################################################################################## convert to sets 

str_c("Started recoding tokens at: ", Sys.time())

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
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$disability), "zqx)\\b"), collapse = "|")), 
                          yes = "disability", no = term)) %>%
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$diversity), "zqx)\\b"), collapse = "|")), 
                          yes = "diversity", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$equity), "zqx)\\b"), collapse = "|")), 
                          yes = "equity", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$social_class), "zqx)\\b"), collapse = "|")), 
                          yes = "socio-economic", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$migration), "zqx)\\b"), collapse = "|")), 
                          yes = "migration", no = term)) %>%
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
  dt_mutate(disability_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(disability)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(equity_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(equity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(minority_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(minority)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(migration_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(migration)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(population_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(population)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(racial_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(race/ethnicity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(sexgender_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(sex/gender)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(sexuality_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(sexuality)\\b"), yes = 1, no = 0)) 

str_c("Finished recoding tokens at: ", Sys.time())

####################################################################################### adding "social" diversity 

# this creates a variable where the term diversity is only used alongside another diversity set 
# basically, i mirror the diversity_cnt into soc_diversity so that we can group_by and count next 
diversity_terms_matrix <- general_pop_terms %>% 
  group_by(id, year) %>% 
  summarise(across(aging_cnt:sexuality_cnt, sum)) %>% 
  # NOT including population 
  mutate(total_cnt_v1 = aging_cnt + ancestry_cnt + cultural_cnt + class_cnt + disability_cnt + diversity_cnt + 
           equity_cnt + minority_cnt + migration_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  # NOT including ancestry OR population
  mutate(total_cnt_v2 = aging_cnt + cultural_cnt + class_cnt + disability_cnt + diversity_cnt + 
           equity_cnt + minority_cnt + migration_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  # NOT including ancestry OR equity OR population
  mutate(total_cnt_v3 = aging_cnt + cultural_cnt + class_cnt + disability_cnt + diversity_cnt + 
           minority_cnt + migration_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  select(id, year, total_cnt_v1, total_cnt_v2, total_cnt_v3, everything()) %>%
  arrange(-total_cnt_v1) %>% ungroup() %>%
  mutate(soc_diversity_v1 = if_else(diversity_cnt > 0 & total_cnt_v1 != diversity_cnt, diversity_cnt, 0),
         soc_diversity_v2 = if_else(diversity_cnt > 0 & total_cnt_v2 != diversity_cnt, diversity_cnt, 0),
         soc_diversity_v3 = if_else(diversity_cnt > 0 & total_cnt_v3 != diversity_cnt, diversity_cnt, 0)) 

general_pop_terms <- diversity_terms_matrix %>% 
  select(id, soc_diversity_v1, soc_diversity_v2, soc_diversity_v3) %>% 
  left_join(general_pop_terms, by = "id") %>% 
  select(-soc_diversity_v1, soc_diversity_v1, -soc_diversity_v2, soc_diversity_v2, -soc_diversity_v3, soc_diversity_v3)

## tmp - this is to output all the TEST CSVs 

#diversity_terms_matrix %>% filter(soc_diversity_v1 != soc_diversity_v2)
chk <- general_pop_terms %>% filter(disability_cnt == 1)
chk_abstracts <- pubmed_data %>% 
  rename(id = fk_pmid) %>% 
  inner_join(chk %>% select(id, word), by = "id") %>% 
  distinct(id, year, word, abstract)
write_csv(chk_abstracts, "~/git/diversity/data/sensitivity_checks/disability_checks.csv")

############################################################################################## get full counts 

str_c("Merging data at: ", Sys.time())

# counts of term sets for all years 
h1_set_counts_full <- general_pop_terms %>% 
  group_by(term) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 

h1_set_counts_full <- general_pop_terms %>% 
  filter(soc_diversity_v1 == 1 & term == "diversity") %>% 
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
  filter(soc_diversity_v1 == 1 & term == "diversity") %>% 
  group_by(word, term) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (social)")) %>% 
  bind_rows(h1_subset_counts_full) %>% 
  arrange(-n)

# counts of all terms by year                              I updated this subsection for the tests across three soc_div types
h1_set_counts_trends <- general_pop_terms %>% 
  group_by(term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 

h1_set_counts_trends <- general_pop_terms %>% 
  filter(soc_diversity_v1 == 1 & term == "diversity") %>% 
  group_by(term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (v1)")) %>% 
  bind_rows(h1_set_counts_trends) %>% 
  arrange(-n)

h1_set_counts_trends <- general_pop_terms %>% 
  filter(soc_diversity_v2 == 1 & term == "diversity") %>% 
  group_by(term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (v2)")) %>% 
  bind_rows(h1_set_counts_trends) %>% 
  arrange(-n)

h1_set_counts_trends <- general_pop_terms %>% 
  filter(soc_diversity_v3 == 1 & term == "diversity") %>% 
  group_by(term, year) %>% 
  count(sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (v3)")) %>% 
  bind_rows(h1_set_counts_trends) %>% 
  arrange(-n)

chk_disability <- h1_subset_counts_trends %>% filter(term == "disability")

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

setwd("~/git/diversity/data/text_results/h1_results/")
write_rds(h1_set_counts_full, str_c("h1_set_counts_full_",analysis_timeframe,".rds"))
write_rds(h1_subset_counts_full, str_c("h1_subset_counts_full_",analysis_timeframe,".rds"))
write_rds(h1_set_counts_trends, str_c("h1_set_counts_trends_",analysis_timeframe,".rds"))
write_rds(h1_subset_counts_trends, str_c("h1_subset_counts_trends_",analysis_timeframe,".rds"))
write_rds(diversity_terms_matrix %>% rename(fk_pmid = id), str_c("h1_diversity_terms_matrix_",analysis_timeframe,".rds"))

############################################################################################# convert to percentages

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
  mutate(prc_soc_diversity = round(cnt_soc_diversity / total * 100, digits = 2))
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

setwd("~/git/diversity/data/text_results/h1_results/")
write_rds(gen_pop_prc_counts, str_c("h1_set_prc_trends_",analysis_timeframe,".rds"))

##################################################################################### correlation matrices 

h1_set_cor_matrix <- general_pop_terms %>% 
  select(id, term) %>% 
  pairwise_cor(term, id, sort = TRUE) %>% 
  arrange(item1, item2) %>% 
  mutate(year = analysis_timeframe)

h1_subset_cor_matrix <- general_pop_terms %>% 
  select(id, word) %>% 
  pairwise_cor(word, id, sort = TRUE) %>% 
  arrange(item1, item2) %>% 
  mutate(year = analysis_timeframe)

setwd("~/git/diversity/data/text_results/h1_results/")
write_rds(h1_set_cor_matrix, str_c("h1_set_cor_matrix_",analysis_timeframe,".rds"))
write_rds(h1_subset_cor_matrix, str_c("h1_subset_cor_matrix_",analysis_timeframe,".rds"))

str_c("Finished all processes for ",analysis_timeframe, " at: ", Sys.time())

}

##################################################################################### for loop of all years 

for (year in 1990:2020) {
  test_h1(year)
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years 

library("tidyverse")
setwd("~/git/diversity/data/text_results/h1_results/")

# percentages for all sets 
h1_set_prc_trends <- list.files(pattern="h1_set_prc_trends_*") %>% 
  map_df(~read_rds(.)) %>% 
  select(year, total, everything())

# overall set counts 
h1_set_counts_full <- list.files(pattern="h1_set_counts_full*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(term) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall set counts by year 
h1_set_counts_trends <- list.files(pattern="h1_set_counts_trends*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(term, year) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall subset counts 
h1_subset_counts_full <- list.files(pattern="h1_subset_counts_full*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(word, term) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall subset counts by year 
h1_subset_counts_trends <- list.files(pattern="h1_subset_counts_trends*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(word, term, year) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall subset counts by year 
h1_all_diversity_ids <- list.files(pattern="h1_diversity_ids_*") %>% 
  map_df(~read_rds(.)) 

# need to do the matrix aggregation next 

setwd("~/git/diversity/data/text_results/h1_results")
write_rds(h1_set_prc_trends, "h1_all_set_prc_trends.rds")
write_rds(h1_set_counts_full, "h1_all_set_counts_full.rds")
write_rds(h1_set_counts_trends, "h1_all_set_counts_trends.rds")
write_rds(h1_subset_counts_full, "h1_all_subset_counts_full.rds")
write_rds(h1_subset_counts_trends, "h1_all_subset_counts_trends.rds")
write_rds(h1_all_diversity_ids, "h1_all_diversity_ids.rds")

str_c("Aggregated data for all years at: ", Sys.time())