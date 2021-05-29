
# silge's new embedding method 

################################################################################################# libs/paths 

library("readr")
library("dplyr") 
library("stringr")  
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("purrr")
library("furrr")
library("future")
library("parallel")
library("tidyr")

embeddings_path = "~/git/diversity/data/word_embeddings/"
first_year = "2015"
final_year  = "_2020"

################################################################################################# setting functions 

# create a function for slide_windows (move this to earlier in file at a later point)
# remember that 3-4 windows focus on how word is used and ~10 word windows capture topics/domains
slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, ~.x, .after = window_size - 1, .step = 1, .complete = TRUE
  )
  safe_mutate <- safely(mutate)
  out <- map2(skipgrams, 1:length(skipgrams), ~ safe_mutate(.x, window_id = .y))
  out %>%
    transpose() %>% 
    pluck("result") %>% 
    compact() %>%
    bind_rows()
}

# set up nearest_neighbors function to look at similar words 
nearest_neighbors <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), 
           sort = TRUE, 
           maximum_size = NULL)(item1, dimension, value) %>%
    select(-item2)
}

search_synonyms <- function(word_vectors, selected_vector) {
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  similarities %>%
    arrange(-similarity)    
}
  
  ################################################################################################# ingestion/cleaning
  
  #rm(list = ls())
  #analysis_timeframe <- "1990"
  
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
                          WHERE year >=", first_year, ";"))
  
  # disconnect from postgresql database 
  dbDisconnect(conn)
  
  str_c("Finished data pull at: ", Sys.time())
  
  ################################################################################################# counting words
  
  # unnest the words and filter 
  unnested_words <- pubmed_data %>%
    select(fk_pmid, abstract) %>%
    unnest_tokens(word, abstract) %>%
    group_by(word) %>%
    filter(n() >= 20) %>% #  
    ungroup()
  unnested_words
  
  # nest them back together after filter 
  nested_words <- unnested_words %>%
    tidyr::nest(words = c(word))
  nested_words
  
  # set up cores for multiprocessing 
  number_of_cores <- availableCores() - 1
  plan(multicore, workers = number_of_cores)
  
  # lets create skipgrams with a window of 4 
  # remember that 3-4 windows focus on how word is used
  skipgrams <- nested_words %>%  
    mutate(words = future_map(words, slide_windows, 4)) 
  setwd(embeddings_path)
  write_rds(skipgrams, str_c("pubmed_skipgrams_",first_year,final_year,".rds"))
  
  # then we want to calculate pmi 
  # pmi is logarithm of the probability of finding two words together, 
  # normalized for the probability of finding each of the words alone
  tidy_pmi <- skipgrams %>% 
    unnest(words) %>%
    unite(window_id, fk_pmid, window_id) %>%
    pairwise_pmi(word, window_id)
  setwd(embeddings_path)
  write_rds(tidy_pmi, str_c("pubmed_pmi_",first_year,final_year,".rds"))
  
  # determine word vectors using singular value decomposition
  tidy_word_vectors <- tidy_pmi %>%
    widely_svd(item1, item2, pmi, nv = 100, maxit = 1000)
  setwd(embeddings_path)
  write_rds(h3_counts, str_c("pubmed_word_vectors_",first_year,final_year,".rds"))
  







