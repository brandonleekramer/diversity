
#### this has been converted to a function that is run iteratively through the _by_year.R file 
#### can just run this for one year by changing the analysis_timeframe parameter

####################################################################################### install.packages (for slurm) 

#rm(list = ls())
#analysis_timeframe <- 1990

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
source("~/git/diversity/scripts/diversitizeR.R")

######################################################################################## load diversity dictionaries

# lets draw all of our strings from the diversity_dictionary (divictionary)
# setwd("~/Documents/Diversity/Data")
setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 

divictionary_string <- c(na.omit(divictionary$ancestry), na.omit(divictionary$cultural), 
                         na.omit(divictionary$disability), na.omit(divictionary$diversity), 
                         na.omit(divictionary$equity), na.omit(divictionary$lifecourse), 
                         na.omit(divictionary$migration),
                         na.omit(divictionary$minority), na.omit(divictionary$population),
                         na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                         na.omit(divictionary$sexuality), na.omit(divictionary$social_class))

# this removes the ancestry and population terms for when we output abstract ids 
diversity_only <- c(na.omit(divictionary$cultural), na.omit(divictionary$disability), 
                    na.omit(divictionary$diversity), na.omit(divictionary$equity), 
                    na.omit(divictionary$lifecourse),
                    na.omit(divictionary$migration), na.omit(divictionary$minority), 
                    na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                    na.omit(divictionary$sexuality), na.omit(divictionary$social_class))

################################################################################################# data ingestion/cleaning 

str_c("Starting data pull at: ", Sys.time())

#analysis_timeframe <- 1990

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

str_c("Started humanizeR & compoundR at: ", Sys.time())

pubmed_data <- pubmed_data %>% 
  mutate(abstract_raw = abstract, 
         abstract = tolower(abstract)) %>% 
  humanizeR(fk_pmid, abstract) %>% 
  # exclusion criteria predicated on: 
  # any human words being included OR 
  # no human or no nonhuman words OR 
  # having less than 5x the nonhuman:human words 
  #filter(human > 0 | nonhuman == 0) %>% 
  #mutate(ex_ratio = round(nonhuman / human, 2),
  #       ex_ratio = replace_na(ex_ratio, 0)) %>% 
  #filter(ex_ratio < 5) %>% 
  # different for diversity 
  filter(nonhuman == 0) %>% 
  compoundR(abstract)

str_c("Finished humanizeR & compoundR at: ", Sys.time())

# save the human only entries 
human_research_abstracts <- pubmed_data %>% 
  distinct(fk_pmid, year, human, nonhuman) 
setwd("~/git/diversity/data/text_results/h1_results/")
write_rds(human_research_abstracts, str_c("h1_human_research_ids_",analysis_timeframe,".rds"))
rm(human_research_abstracts)

# total articles each year 
# we are banking this df of the total abstract counts for each year now 
# so that we can optimize our memory for later (removing raw pubmed_data soon)
gen_pop_prc_counts <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ungroup() %>% 
  rename(total = n)

######################################################################################## unnesting phase 

str_c("Starting to unnest tokens at: ", Sys.time())

# tokenizing the abstract data into words 
pubmed_abstract_data <- pubmed_data %>% 
  distinct(fk_pmid, year, abstract) %>% 
  rename(id = fk_pmid) %>% 
  unnest_tokens(word, abstract) %>% 
  # filter down to only include diversity terms 
  filter(word %in% divictionary_string)

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
  dt_mutate(term = ifelse(test = str_detect(string = word, pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$ancestry), "zqx)\\b"), collapse = "|")), 
                          yes = "ancestry", no = word)) %>% 
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
                          yes = "socioeconomic", no = term)) %>% 
  dt_mutate(term = ifelse(test = str_detect(string = word, 
                          pattern = paste(c("\\b(?i)(zqx", na.omit(divictionary$lifecourse), "zqx)\\b"), collapse = "|")), 
                          yes = "lifecourse", no = term)) %>% 
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
  dt_mutate(ancestry_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(ancestry)\\b"), yes = 1, no = 0)) %>% 
  dt_mutate(cultural_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(cultural)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(class_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(socioeconomic)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(diversity_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(diversity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(disability_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(disability)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(equity_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(equity)\\b"), yes = 1, no = 0)) %>%
  dt_mutate(lifecourse_cnt = ifelse(test = str_detect(string = term, pattern = "\\b(lifecourse)\\b"), yes = 1, no = 0)) %>%
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
  summarise(across(ancestry_cnt:sexuality_cnt, sum)) %>% 
  # NOT including ancestry OR population
  mutate(total_cnt = cultural_cnt + class_cnt + 
         disability_cnt + diversity_cnt + equity_cnt + 
         lifecourse_cnt + minority_cnt + migration_cnt + 
         racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
  select(id, year, total_cnt, everything()) %>%
  arrange(-total_cnt) %>% ungroup() %>%
  # get the intial social_diversity count (before polysemeR correction)
  mutate(soc_div_raw = if_else(diversity_cnt > 0 & total_cnt != diversity_cnt, diversity_cnt, 0)) 

############################################################## correcting false positives for social diversity

# then get the correction count for social_diversity (run polysemeR)
correcting_for_heterogeneity <- pubmed_data %>% 
  rename(id = fk_pmid) %>% 
  left_join(diversity_terms_matrix, by = "id") %>% 
  select(id, abstract, soc_div_raw) %>%
  filter(soc_div_raw > 0) %>% 
  polysemeR(id, abstract) %>% 
  mutate(socdiv_cnt = ifelse(heterogeneity == 1, 0, 1)) %>% 
  select(-abstract, -soc_div_raw, -heterogeneity)
diversity_terms_matrix <- diversity_terms_matrix %>% 
  left_join(correcting_for_heterogeneity, by = "id") %>% 
  mutate(socdiv_cnt = replace_na(socdiv_cnt, 0)) %>% 
  select(-soc_div_raw)

# add that back to the general_pop_terms
general_pop_terms <- diversity_terms_matrix %>% 
  select(id, socdiv_cnt) %>% 
  left_join(general_pop_terms, by = "id") %>% 
  select(everything(), socdiv_cnt)

############################################################################################## get full counts 

str_c("Merging data at: ", Sys.time())

h1_set_counts_trends <- diversity_terms_matrix %>% 
  pivot_longer(cols = ends_with("cnt"), names_to = "term", values_to = "counts") %>% 
  group_by(term, year) %>% 
  summarize(n = sum(counts)) %>% 
  arrange(-n) %>% 
  mutate(term = str_replace(term, "_cnt", ""),
         term = str_replace(term, "sexgender", "sex/gender"),
         term = str_replace(term, "racial", "race/ethnicity"),
         term = str_replace(term, "class", "socioeconomic"),
         term = str_replace(term, "diversity", "diversity (all)"),
         term = str_replace(term, "socdiv", "diversity (social)"))

# counts of term subsets by year
h1_subset_counts_trends <- general_pop_terms %>% 
  #group_by(word, term, year) %>% # this should be updated to count(word, term, year, sort = TRUE)
  count(word, term, year, sort = TRUE) %>% 
  summarize(n = sum())
  mutate(term = str_replace(term, "diversity", "diversity (all)")) 

h1_subset_counts_trends <- general_pop_terms %>% 
  filter(socdiv_cnt == 1 & term == "diversity") %>% 
  #group_by(word, term, year) %>% 
  count(word, term, year, sort = TRUE) %>% 
  mutate(term = str_replace(term, "diversity", "diversity (social)")) %>% 
  bind_rows(h1_subset_counts_trends) %>% 
  arrange(-n)

## tmp - this is to output all the TEST CSVs 
#chk <- general_pop_terms %>% filter(lifecourse_cnt == 1)
#chk_abstracts <- pubmed_data %>% 
#  rename(id = fk_pmid) %>% 
#  inner_join(chk %>% select(id, word), by = "id") %>% 
#  distinct(id, year, word, abstract)
#write_csv(chk_abstracts, "~/git/diversity/data/sensitivity_checks/lifecourse_checks.csv")
#write_csv(h1_subset_counts_trends, "~/git/diversity/data/sensitivity_checks/h1_term_counts.csv")

setwd("~/git/diversity/data/text_results/h1_results/")
write_rds(h1_set_counts_trends, str_c("h1_set_counts_trends_",analysis_timeframe,".rds"))
write_rds(h1_subset_counts_trends, str_c("h1_subset_counts_trends_",analysis_timeframe,".rds"))
write_rds(diversity_terms_matrix %>% rename(fk_pmid = id), str_c("h1_diversity_terms_matrix_",analysis_timeframe,".rds"))

############################################################################################# convert to percentages

# articles with term mentioned each year 
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
  filter(socdiv_cnt == 1) %>% #diversity (social)
  group_by(year) %>% 
  summarise(cnt_soc_diversity = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_soc_diversity = round(cnt_soc_diversity / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(disability_cnt == 1) %>% #disability 
  group_by(year) %>% 
  summarise(cnt_disability = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_disability = round(cnt_disability / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(equity_cnt == 1) %>% #equity 
  group_by(year) %>% 
  summarise(cnt_equity = n_distinct(id)) %>% 
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_equity = round(cnt_equity / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(lifecourse_cnt == 1) %>% # lifecourse
  group_by(year) %>% 
  summarise(cnt_lifecourse = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_lifecourse = round(cnt_lifecourse / total * 100, digits = 2))
gen_pop_prc_counts <- general_pop_terms %>% 
  filter(migration_cnt == 1) %>% # migration
  group_by(year) %>% 
  summarise(cnt_migration = n_distinct(id)) %>%  
  right_join(gen_pop_prc_counts, by = "year") %>% 
  mutate(prc_migration = round(cnt_migration / total * 100, digits = 2))
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
  str_c("Just finished:", print(year))
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years 

library("tidyverse")
setwd("~/git/diversity/data/text_results/h1_results/")

# percentages for all sets 
h1_set_prc_trends <- list.files(pattern="h1_set_prc_trends_*") %>% 
  map_df(~read_rds(.)) %>% 
  select(year, total, everything())

# overall set counts by year 
h1_set_counts_trends <- list.files(pattern="h1_set_counts_trends*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(term, year) %>% 
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
  map_df(~read_rds(.)) %>% 
  distinct(id) %>% arrange(id)

# all human research ids 
all_human_research_ids <- list.files(pattern="h1_human_research_ids_*") %>% 
  map_df(~read_rds(.)) 
write_rds(all_human_research_ids, "h1_all_human_research_ids.rds")

# need to do the matrix aggregation next 

setwd("~/git/diversity/data/text_results/h1_results")
write_rds(h1_set_prc_trends, "h1_all_set_prc_trends.rds")
write_rds(h1_set_counts_trends, "h1_all_set_counts_trends.rds")
write_rds(h1_subset_counts_trends, "h1_all_subset_counts_trends.rds")
write_rds(h1_all_diversity_ids, "h1_all_diversity_ids.rds")

str_c("Aggregated data for all years at: ", Sys.time())