

get_human_research_ids <- function(analysis_timeframe){
  
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
  
  # convert text to lowercase 
  pubmed_data <- pubmed_data %>% mutate(abstract = tolower(abstract))
  
  # replaces all the hyphenated and compound words 
  # helps to reduce false positives (e.g. double_blind)
  pubmed_data$abstract <- pubmed_data$abstract %>% 
    str_replace_all(preprocessing_terms)
  
  # animal exclusion + human inclusion clauses
  in_exclusions <- read_csv("diversity_project - in_exclusions.csv")
  human_inclusion_clause <- paste(c("\\b(?i)(zqx", na.omit(in_exclusions$humans), "zqx)\\b"), collapse = "|")
  animal_exclusion_clause <- paste(c("\\b(?i)(zqx", na.omit(in_exclusions$animals), "zqx)\\b"), collapse = "|")
  
  # remove abstracts with animals to reduce false positive animal studies 
  human_research_abstracts <- pubmed_data %>% 
    dt_mutate(human_study = ifelse(test = str_detect(string = abstract, 
                                                     pattern = human_inclusion_clause), yes = 1, no = 0)) %>% 
    dt_mutate(animal_study = ifelse(test = str_detect(string = abstract, 
                                                      pattern = animal_exclusion_clause), yes = 1, no = 0)) %>% 
    filter(human_study == 1 | animal_study == 0) %>% 
    distinct(fk_pmid, year, human_study, animal_study) 
  
  setwd("~/git/diversity/data/text_results/h1_results/")
  write_rds(human_research_abstracts, str_c("h1_human_research_ids_",analysis_timeframe,".rds"))
  
}

##################################################################################### for loop of all years 

for (year in 1990:2020) {
  get_human_research_ids(year)
  str_c("Just finished:", print(year)) # ran this at 3:11pm 
}

setwd("~/git/diversity/data/text_results/h1_results/")
all_human_research_ids <- list.files(pattern="h1_human_research_ids_*") %>% 
  map_df(~read_rds(.)) 
write_rds(all_human_research_ids, "h1_all_human_research_ids.rds")

#setwd("~/git/diversity/data/text_results/h1_results/")
#chk = read_rds(str_c("h1_human_research_ids_",analysis_timeframe,".rds"))

##################################################################################### write to the database

# reconnecting to the database 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
# writing the new users_gh_cc table to postgis_2
dbWriteTable(conn, c("pubmed_2021", "human_research_ids"), all_human_research_ids, row.names = FALSE)
# disconnect from postgresql database  
dbDisconnect(conn)


  
  