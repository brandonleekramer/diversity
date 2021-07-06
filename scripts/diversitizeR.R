
# compoundR combines compound word sequences 

compoundR <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # load dictionary 
  setwd("~/git/diversity/data/dictionaries/")
  compound_dictionary <- readr::read_csv("diversity_project - compoundR.csv") %>%
    dplyr::select(original_string, new_string) %>% tibble::deframe()
  
  # load dictionary 
  df <- df %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.("{{input}}" := stringr::str_replace_all({{ input }}, compound_dictionary))
  
  df
  
}

# humanizeR creates two binary indicator variables to detect whether the study is a non/human research study 

humanizeR <- function(df, id, input){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  setwd("~/git/diversity/data/dictionaries/")
  dictionary <- readr::read_csv("diversity_project - humanizeR.csv")
  human <- dictionary %>% filter(category == "human")
  nonhuman <- dictionary %>% filter(category == "nonhuman")
  
  id <- enquo(id)
  input <- enquo(input)
  
  tmp_df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    as.data.frame() %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    as_tidytable() %>% 
    tidytable::filter.(word %in% human$terms) %>% 
    tidytable::mutate.(human = 1, nonhuman = 0) %>% 
    tidytable::select.(-word) %>% 
    as.data.frame()
  
  tmp_df <- df %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    as_tidytable() %>%
    tidytable::filter.(word %in% nonhuman$terms) %>% 
    tidytable::mutate.(human = 0, nonhuman = 1) %>% 
    tidytable::select.(-word) %>% 
    tidytable::bind_rows.(tmp_df) %>% 
    tidytable::summarize.(human = sum(human),
                          nonhuman = sum(nonhuman), 
                          .by = !!id) %>% 
    tidytable::arrange.(!!id)
  
  df <- df %>% 
    tidytable::left_join.(tmp_df) %>% 
    tidytable::mutate.(human = replace_na.(human, 0)) %>% 
    tidytable::mutate.(nonhuman = replace_na.(nonhuman, 0))
  
  df
  
}

######



polysemeR <- function(df, id, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  library("tidytext")
  
  # dictionary 
  setwd("~/git/diversity/data/dictionaries/")
  false_positives <- read_csv("diversity_project - polysemeR.csv") %>% arrange(heterogeneity)
  false_positives <- na.omit(false_positives$heterogeneity)
    
  id <- enquo(id)
  input <- enquo(input)
  
  bigrams <- df %>% 
    as.data.frame() %>% 
    tidytext::unnest_tokens(words, !!input, token = "ngrams", n = 2) %>% 
    as_tidytable() %>% 
    tidytable::filter.(words %in% false_positives) %>% 
    tidytable::mutate.(heterogeneity = 1) %>% 
    tidytable::select.(!!id, heterogeneity) %>% 
    tidytable::summarize.(heterogeneity = sum(heterogeneity), .by = !!id)
  
  trigrams <- df %>% 
    as.data.frame() %>% 
    tidytext::unnest_tokens(words, !!input, token = "ngrams", n = 3) %>% 
    as_tidytable() %>% 
    tidytable::filter.(words %in% false_positives) %>% 
    tidytable::mutate.(heterogeneity = 1) %>% 
    tidytable::select.(!!id, heterogeneity) %>% 
    tidytable::summarize.(heterogeneity = sum(heterogeneity), .by = !!id)
  
  ngrams <- bind_rows(bigrams, trigrams) %>% 
    tidytable::summarize.(heterogeneity = sum(heterogeneity), .by = !!id)
  
  df <- df %>% 
    tidytable::left_join.(ngrams) %>% 
    tidytable::mutate.(heterogeneity = replace_na.(heterogeneity, 0)) %>% 
    as.data.frame()
  
  df
  
}





















