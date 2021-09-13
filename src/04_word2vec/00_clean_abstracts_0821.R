
rm(list = ls())

library("RPostgreSQL")
library("tidyverse")
library("tidytext")
source("~/git/diversity/scripts/diversitizeR_0721.R")

# 1990-2000 model 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
pubmed_data <- dbGetQuery(conn,"SELECT DISTINCT(fk_pmid), year, abstract  
                                FROM pubmed_2021.biomedical_abstracts 
                                WHERE year BETWEEN 1990 AND 2000;")
dbDisconnect(conn)

Sys.time()
early_period <- pubmed_data %>% 
  mutate(abstract = tolower(abstract)) %>% 
  humanizeR_0721(fk_pmid, abstract) %>% 
  filter(human > 0 | nonhuman == 0) %>% 
  compoundR_0721(abstract)
Sys.time()

# write back to the database 

# reconnecting to the database 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
# writing the new users_gh_cc table to postgis_2
RPostgreSQL::dbWriteTable(conn, c("pubmed_2021", "abstracts_1990_2000_0821"), 
                          early_period, row.names = FALSE)
# disconnect from postgresql database  
RPostgreSQL::dbDisconnect(conn)
write_csv(early_period, "~/git/diversity/data/word_embeddings/cleaned_1990_2000_0821.csv")

# 2010-2020 model 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
pubmed_data <- dbGetQuery(conn,"SELECT DISTINCT(fk_pmid), year, abstract  
                                FROM pubmed_2021.biomedical_abstracts 
                                WHERE year BETWEEN 2010 AND 2020;")
dbDisconnect(conn)

Sys.time()
later_period <- pubmed_data %>% 
  mutate(abstract = tolower(abstract)) %>% 
  humanizeR_0721(fk_pmid, abstract) %>% 
  filter(human > 0 | nonhuman == 0) %>% 
  compoundR_0721(abstract)
Sys.time()
write_csv(later_period, "~/git/diversity/data/word_embeddings/cleaned_2010_2020_0821.csv")

# write back to the database 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, name = c(schema = "pubmed_2021" , name = "cleaned_2010_2020_0821"), 
             value = later_period, row.names = FALSE, temporary = TRUE)
dbDisconnect(conn)

#####

conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
soc_diversity_ids <- dbGetQuery(conn,"SELECT * FROM pubmed_2021.soc_diversity_ids_0721;")
soc_diversity_ids <- na.omit(soc_diversity_ids$fk_pmid)
dbDisconnect(conn)

earlier_period <- read_csv("~/git/diversity/data/word_embeddings/cleaned_1990_2000_0821.csv")
later_period <- read_csv("~/git/diversity/data/word_embeddings/cleaned_2010_2020_0821.csv")

earlier_period <- earlier_period %>% 
  distinct(fk_pmid, year, abstract)
later_period <- later_period %>% 
  distinct(fk_pmid, year, abstract) 
both_periods <- bind_rows(earlier_period, later_period)

transformed <- both_periods %>% 
  filter(fk_pmid %in% soc_diversity_ids) %>% 
  mutate(abstract = str_replace(abstract, "\\b(?i)(diverse)\\b", "socialdiverse"),
         abstract = str_replace(abstract, "\\b(?i)(diversely)\\b", "socialdiversely"),
         abstract = str_replace(abstract, "\\b(?i)(diversified)\\b", "socialdiversified"),
         abstract = str_replace(abstract, "\\b(?i)(diversification)\\b", "socialdiversification"),
         abstract = str_replace(abstract, "\\b(?i)(diversifying)\\b", "socialdiversifying"),
         abstract = str_replace(abstract, "\\b(?i)(diversity)\\b", "socialdiversity"))

original_with_transformed <- both_periods %>% 
  filter(!fk_pmid %in% soc_diversity_ids) %>% 
  bind_rows(transformed) %>% 
  arrange(year, fk_pmid)

early_period <- original_with_transformed %>% filter(year < 2001)
late_period <- original_with_transformed %>% filter(year > 2009)

write_csv(early_period, "~/git/diversity/data/word_embeddings/transformed_1990_2000_0821.csv")
write_csv(late_period, "~/git/diversity/data/word_embeddings/transformed_2010_2020_0821.csv")

late_period %>% 
  filter(grepl("socialdiversity", abstract))





