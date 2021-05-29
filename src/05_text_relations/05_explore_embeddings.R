
# silge's new embedding method 

################################################################################################# libs/paths/ingest 

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
analysis_timeframe = "1990"

setwd(embeddings_path)
tidy_word_vectors = read_rds(str_c("pubmed_word_vectors_",analysis_timeframe,".rds"))
  
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

################################################################################################# exploring vectors 

racial_vector <- tidy_word_vectors %>%
  nearest_neighbors("racial")

black_vector <- tidy_word_vectors %>%
  nearest_neighbors("black")

risk_vector <- tidy_word_vectors %>%
  nearest_neighbors("risk")  
  
# adding and subtracting isn't the same binding rows, go back to silge's original code to see how that df is structured 

pcos_vector <- tidy_word_vectors %>%
  nearest_neighbors("polycystic") %>% 
  mutate(source = "polycystic")

ovary_vector <- tidy_word_vectors %>%
  nearest_neighbors("ovary") %>% 
  mutate(source = "ovary")  
  
pcos_vector <- pcos_vector %>% 
  bind_rows(ovary_vector) %>% 
  arrange(-value)

chk <- pcos_vector %>% 
  filter(item1 %in% c("white", "black", "woman"))


mystery_product <- tidy_word_vectors["racial",] - tidy_word_vectors["black",] + tidy_word_vectors["men",]

           

  
# leftovers from before - don't delete yet   
  
  # convert to a sparse matrix format 
  #word_matrix <- unnested_words %>% 
  #  count(fk_pmid, word) %>%  
  #  cast_sparse(fk_pmid, word, n)
  
  #embedding_matrix <- tidy_word_vectors %>%
  #  cast_sparse(item1, dimension, value)
  
  #doc_matrix <- word_matrix %*% embedding_matrix
  
  #dim(doc_matrix)
  
  # not sure if this will work but cf. https://juliasilge.com/blog/tidy-word-vectors/
  #racial_vector <- search_synonyms(tidy_word_vectors, tidy_word_vectors["racial",])
  #racial_vector
  
  # need to think conceptually about 
  #mystery_product <- word_vectors["racial",] - word_vectors["black",] + word_vectors["men",]
  #search_synonyms(tidy_word_vectors, mystery_product)
  
  # could just add a bunch of vectors together with a function 
  # nelson does this on page 10: https://inequality.hks.harvard.edu/files/inequality/files/nelson2021seminarpaper.pdf
  #all_relevant_terms = c("race", "racial", "ethic", "ethinicity")
  #aggregated_racial_vectors <- word_vectors[all_relevant_terms,] 
  # then you can calculate how far different terms are from those terms 
  
#}

# create the for loop 
#test_embeddings("1990")

# add in more about saving each set of word embeddings 
# do i save them as sparse matrices? and then decast them after? 


#tidy_word_vectors 





