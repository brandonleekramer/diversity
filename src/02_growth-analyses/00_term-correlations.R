

for (pkg in c("tidyverse", "tidytext", "RPostgreSQL", "data.table", 
              "widyr", "maditr")) {library(pkg, character.only = TRUE)}

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

setwd("~/git/diversity/data/dictionaries/")
divictionary <- read_csv("diversity_project - h1_dictionary.csv") 
combined_dictionaries <- paste(c("\\b(?i)(zqx", 
                                 na.omit(divictionary$aging), 
                                 na.omit(divictionary$ancestry),
                                 na.omit(divictionary$cultural),
                                 na.omit(divictionary$diversity),
                                 na.omit(divictionary$genetic),
                                 na.omit(divictionary$minority),
                                 na.omit(divictionary$population),
                                 na.omit(divictionary$race_ethnicity),
                                 na.omit(divictionary$sex_gender),
                                 na.omit(divictionary$sexuality),
                                 "test this|distribution|medium",
                                 "zqx)\\b"), collapse = "|") # this has to be NOT collapsed
combined_dictionaries

# pairwise counts  
pubmed_pw_counts <- pubmed_data %>%
  select(year, fk_pmid, abstract) %>% 
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>% 
  anti_join(my_stopwords) %>% 
  filter(word %in% c("distribution", "medium")) %>% 
  pairwise_count(word, fk_pmid, sort = TRUE)

# pairwise correlations  
pubmed_correlations <- pubmed_data %>%
  select(year, fk_pmid, abstract) %>% 
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>% 
  anti_join(my_stopwords) %>% 
  filter(word %in% combined_dictionaries) %>% 
  pairwise_cor(word, fk_pmid, sort = TRUE)


  








