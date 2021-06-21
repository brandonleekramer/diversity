
rm(list = ls())

# load pkgs 
library("tidyverse")
library("RPostgreSQL")

# h1 ingest term matrices  
#setwd("~/git/diversity/data/text_results/h1_results")
setwd("/project/class/bii_sdad_dspg/uva/ncses_oss/brandon_diversity_data/h1_results_0521/")
h1_ids <- list.files(pattern="h1_diversity_terms_matrix_*") %>% 
  map_df(~read_rds(.)) %>% 
  arrange(year, fk_pmid) %>% 
  rename_all(~ sub("*_cnt", "", .x)) %>% 
  rename(race_ethnicity = racial,
         sex_gender = sexgender,
         social_class = class,
         h1_totals = total) %>% 
  select(-ancestry, -population)

# h2 ingest and summarize 
#setwd("~/git/diversity/data/text_results/h2_results")
setwd("/project/class/bii_sdad_dspg/uva/ncses_oss/brandon_diversity_data/h2_results_0521/")
h2_ids <- list.files(pattern="h2_omb_ids_*") %>% 
  map_df(~read_rds(.)) %>% 
  select(-human_study:-term) %>% 
  group_by(fk_pmid, year) %>% 
  summarise(across(black:ethnic, sum)) %>% 
  arrange(year, fk_pmid) %>% 
  mutate(h2_totals = rowSums(across(black:ethnic))) %>% 
  select(fk_pmid, year, h2_totals, everything())

# h3 ingest and summarize 
#setwd("~/git/diversity/data/text_results/h3_results")
setwd("/project/class/bii_sdad_dspg/uva/ncses_oss/brandon_diversity_data/h3_results_0521/")
h3_ids <- list.files(pattern="h3_population_ids*") %>% 
  map_df(~read_rds(.)) %>% 
  select(-race_ethnicity) %>% 
  rename(h3_totals = all_pop_terms) %>% 
  #mutate(h3_totals = rowSums(across(continental:omb_uscensus))) %>% 
  select(fk_pmid, year, h3_totals, everything()) 

#chk = h3_ids %>% 
#  mutate(diff = (all_pop_terms - h3_totals)) %>% 
#  filter(diff != 0)

# note for later 
# there is a discrepency between the across column counts and all_pop_terms
# the undercounts are good bc they mean terms are not being counted twice 
# the overcounts are concerning bc i do not know what would cause this 
# just ignoring for now since h3 is likely to go in the next paper 

combined_ids <- h1_ids %>% 
  left_join(h2_ids, by = c("fk_pmid", "year")) %>% 
  left_join(h3_ids, by = c("fk_pmid", "year")) %>% 
  replace(is.na(.), 0) %>% 
  select(fk_pmid, year, h1_totals, h3_totals, racial, ethnic, diversity, everything()) %>% 
  mutate(soc_div_terms = rowSums(across(cultural:omb_uscensus))) %>% 
  select(fk_pmid, year, soc_div_terms, h1_totals, h2_totals, h3_totals, sort(names(.)))
  
# double-check they are all distinct - they are! 
combined_ids %>% 
  distinct(fk_pmid) %>% 
  count()
  
# only social diversity ids = 799445
soc_diversity_ids <- combined_ids %>% filter(soc_div_terms != 0)

# no relevant h1_h3 terms at all = 40,375
bio_diversity_ids <- combined_ids %>% filter(soc_div_terms == 0)

# writing our tables to the database 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("pubmed_2021", "all_diversity_ids"), 
             combined_ids, row.names = FALSE)
dbWriteTable(conn, c("pubmed_2021", "soc_diversity_ids"), 
             soc_diversity_ids, row.names = FALSE)
dbWriteTable(conn, c("pubmed_2021", "bio_diversity_ids"), 
             bio_diversity_ids, row.names = FALSE)
dbDisconnect(conn)



  
  
  
  







