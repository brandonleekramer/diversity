
rm(list = ls())

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")

setwd("~/git/diversity/data/text_results/h1_results/")
h1_matrix <- list.files(pattern="h1_diversity_terms_matrix_*") %>% 
  map_df(~read_rds(.)) 


conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
pubmed_data <- dbGetQuery(conn, str_c(
                          "SELECT DISTINCT(fk_pmid), abstract  
                          FROM pubmed_2021.biomedical_abstracts;"))
dbDisconnect(conn)


diversity_only <- h1_matrix %>% 
  left_join(pubmed_data, by = "fk_pmid") %>% 
  select(fk_pmid, year, abstract, socdiv_cnt, everything()) %>% 
  filter(diversity_cnt == 1)

diversity_only

reg_div = diversity_only %>% 
  filter(diversity_cnt == 1 & socdiv_cnt != 1) #%>% count()
# 33710

reg_div_rand = sample_n(reg_div, 500)

soc_div = diversity_only %>% 
  filter(socdiv_cnt == 1) #%>% count()
# 9354

soc_div_rand = sample_n(soc_div, 500)

both_div_rand = bind_rows(reg_div_rand, soc_div_rand)

both_div_rand <- both_div_rand[sample(nrow(both_div_rand)),]


setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(both_div_rand, "diversity_datatolabel_070521.csv")
