

compare_diversity_measures <- function(analysis_timeframe){
  
  #rm(list = ls())
  #analysis_timeframe <- 1990
  
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
  
  pubmed_data <- pubmed_data %>% 
    mutate(abstract_raw = abstract, 
           abstract = tolower(abstract)) %>% 
    humanizeR(fk_pmid, abstract) %>% 
    compoundR(abstract)
  
  # version_1 is based on co-occurences within the abstract 
  setwd("/project/biocomplexity/sdad/projects_data/ncses/oss/brandon_diversity_data/h1_results_0521/")
  terms_matrix <- read_rds(str_c("h1_diversity_terms_matrix_", analysis_timeframe,".rds")) %>% 
    select(fk_pmid, diversity_cnt, soc_diversity) %>% 
    rename(alldiv = diversity_cnt) 
  pubmed_data <- pubmed_data %>% 
    left_join(terms_matrix, by = "fk_pmid") %>% 
    mutate(socdiv_v1 = replace_na(soc_diversity, 0)) %>% 
    mutate(alldiv = replace_na(alldiv, 0)) %>% 
    select(-soc_diversity) 
  rm(terms_matrix)
  
  # version_2 removed the polysemes 
  setwd("~/git/diversity/data/text_results/h1_results/")
  terms_matrix <- read_rds(str_c("h1_diversity_terms_matrix_", analysis_timeframe,".rds")) %>% 
    select(fk_pmid, socdiv_cnt)
  pubmed_data <- pubmed_data %>% 
    left_join(terms_matrix, by = "fk_pmid") %>% 
    mutate(socdiv_v2 = replace_na(socdiv_cnt, 0)) %>% 
    select(-socdiv_cnt)
  rm(terms_matrix)
  
  # version_3 and version_4 based on more stringent exclusion clause 
  pubmed_v3_4 <- pubmed_data %>% 
    filter(human > 0 | nonhuman == 0) %>% 
    detect_social_diversity(fk_pmid, abstract_raw, 
                            remove_polysemes = FALSE) %>% 
    rename(socdiv_v3 = soc_diversity) %>% 
    detect_social_diversity(fk_pmid, abstract_raw, 
                            remove_polysemes = TRUE) %>% 
    rename(socdiv_v4 = soc_diversity) %>% 
    select(fk_pmid, socdiv_v3, socdiv_v4)
  pubmed_data <- pubmed_data %>%
    left_join(pubmed_v3_4, by = "fk_pmid") %>% 
    mutate(socdiv_v3 = replace_na(socdiv_v3, 0)) %>% 
    mutate(socdiv_v4 = replace_na(socdiv_v4, 0))
  rm(pubmed_v3_4)
  
  pubmed_v5_6 <- pubmed_data %>%
    filter(nonhuman == 0) %>% 
    detect_social_diversity(fk_pmid, abstract_raw, 
                            remove_polysemes = FALSE) %>% 
    rename(socdiv_v5 = soc_diversity) %>% 
    detect_social_diversity(fk_pmid, abstract_raw, 
                            remove_polysemes = TRUE) %>% 
    rename(socdiv_v6 = soc_diversity) %>% 
    select(fk_pmid, socdiv_v5, socdiv_v6)
  pubmed_data <- pubmed_data %>%
    left_join(pubmed_v5_6, by = "fk_pmid") %>% 
    mutate(socdiv_v5 = replace_na(socdiv_v5, 0)) %>% 
    mutate(socdiv_v6 = replace_na(socdiv_v6, 0))
  rm(pubmed_v5_6)
  
  # add in the more_stringent = TRUE clause and then you can run this on a loop through years 
  
  pubmed_v7 <- pubmed_data %>%
    filter(nonhuman == 0) %>% 
    detect_social_diversity_new(fk_pmid, abstract_raw) %>% 
    rename(socdiv_v7 = soc_diversity) %>% 
    select(fk_pmid, socdiv_v7)
  pubmed_data <- pubmed_data %>%
    left_join(pubmed_v7, by = "fk_pmid") %>% 
    mutate(socdiv_v7 = replace_na(socdiv_v7, 0))
  rm(pubmed_v7)
  
  setwd("~/git/diversity/data/text_results/h1_results/")
  write_csv(pubmed_data %>% select(-abstract_raw), 
            str_c("h1_soc_div_comps_",analysis_timeframe,".csv"))
  
}

for (year in 1990:2020) {
  compare_diversity_measures(year)
  print(str_c("Just finished: ", year))
}


#colSums(pubmed_data %>% select(-abstract, -abstract_raw))







