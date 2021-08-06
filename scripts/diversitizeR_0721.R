

######### version 3 functions (minor, minor updates after testing that make them 07/21)

# humanizeR creates two binary indicator variables to detect whether the study 
# is a non/human research study 

humanizeR_0721 <- function(df, id, input){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  setwd("~/git/diversity/data/dictionaries/")
  dictionary <- readr::read_csv("diversity_project - humanizeR_0721.csv")
  human <- dictionary %>% filter(category == "human")
  human_terms <- na.omit(as.vector(human$terms))
  nonhuman <- dictionary %>% filter(category == "nonhuman")
  nonhuman_terms <- na.omit(as.vector(nonhuman$terms))
  
  id <- enquo(id)
  input <- enquo(input)
  
  tmp_df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    tidytable::filter.(word %in% human_terms) %>% 
    tidytable::mutate.(human = 1, nonhuman = 0) %>% 
    tidytable::select.(-word) 
  
  tmp_df <- df %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    tidytable::filter.(word %in% nonhuman_terms) %>% 
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

compoundR_0721 <- function(df, input){
  
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
  compound_dictionary <- readr::read_csv("diversity_project - compoundR_0721.csv") %>%
    dplyr::select(original_string, new_string) %>% tibble::deframe()
  
  # load dictionary 
  df <- df %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.("{{input}}" := stringr::str_replace_all({{ input }}, compound_dictionary)) 
  df
  
}

polysemeR_0721 <- function(df, id, input){
  
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
  false_positives <- read_csv("diversity_project - polysemeR_0721.csv") 
  false_positives <- na.omit(false_positives$heterogeneity)
  
  id <- enquo(id)
  input <- enquo(input)
  
  bigrams <- df %>% 
    tidytext::unnest_tokens(words, !!input, token = "ngrams", n = 2) %>% 
    tidytable::filter.(words %in% false_positives) %>% 
    tidytable::mutate.(heterogeneity = 1) %>% 
    tidytable::select.(!!id, heterogeneity) %>% 
    tidytable::summarize.(heterogeneity = sum(heterogeneity), .by = !!id)
  
  trigrams <- df %>% 
    tidytext::unnest_tokens(words, !!input, token = "ngrams", n = 3) %>% 
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


detect_socdiv_0721 <- function(df, id, input, remove_polysemes = TRUE){
  
  # load packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries 
  setwd("~/git/diversity/data/dictionaries/")
  divictionary <- read_csv("diversity_project - h1_dictionary_0721.csv") 
  all_diversity_cats  <- c(na.omit(divictionary$ancestry), na.omit(divictionary$cultural), 
                           na.omit(divictionary$disability), na.omit(divictionary$diversity), 
                           na.omit(divictionary$equity), na.omit(divictionary$lifecourse), 
                           na.omit(divictionary$migration), na.omit(divictionary$minority), 
                           na.omit(divictionary$group),
                           na.omit(divictionary$race_ethnicity), na.omit(divictionary$sex_gender),
                           na.omit(divictionary$sexuality), na.omit(divictionary$social_class))
  all_diversity_cats <- all_diversity_cats[!all_diversity_cats %in% c("population", "populations", 
                                                                      "subpopulation", "subpopulations")]
  nonhuman <- readr::read_csv("diversity_project - humanizeR_0721.csv")
  nonhuman <- nonhuman %>% filter(category == "nonhuman")
  nonhuman <- na.omit(nonhuman$terms)
  exclusion_terms <- readr::read_csv("diversity_project - polysemeR_0721.csv")
  exclusion_terms <- na.omit(exclusion_terms$exclusion_terms)
  
  id <- enquo(id)
  input <- enquo(input)
  
  tmp_df <- df %>% 
    as_tidytable() %>%
    
    # split sentences
    tidytable::mutate.(sentences = strsplit(!!input, "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE)) %>% 
    tidytable::unnest.(sentences) %>% 
    
    # unnest and filter to terms 
    tidytable::mutate.(rowid = row_number(),
                       sentence_id = paste0(!!id, "_", rowid)) %>% 
    tidytable::select.(sentence_id, sentences) %>% 
    tidytext::unnest_tokens(word, sentences) %>% 
    tidytable::filter.(word %in% c(all_diversity_cats, nonhuman, exclusion_terms)) %>%
    
    # classify population terms 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$ancestry), "zqx)\\b"), collapse = "|")), "ancestry", word)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$cultural), "zqx)\\b"), collapse = "|")), "cultural", term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$disability), "zqx)\\b"), collapse = "|")), "disability", term)) %>%
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$diversity), "zqx)\\b"), collapse = "|")),  "diversity", term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$equity), "zqx)\\b"), collapse = "|")), "equity", term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$social_class), "zqx)\\b"), collapse = "|")),  "socioeconomic",  term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$lifecourse), "zqx)\\b"), collapse = "|")),  "lifecourse",  term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$migration), "zqx)\\b"), collapse = "|")),  "migration",  term)) %>%
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$minority), "zqx)\\b"), collapse = "|")),  "minority",  term)) %>%
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$group), "zqx)\\b"), collapse = "|")),  "population",  term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$race_ethnicity), "zqx)\\b"), collapse = "|")),  "race/ethnicity",  term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$sex_gender), "zqx)\\b"), collapse = "|")),  "sex/gender",  term)) %>% 
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(divictionary$sexuality), "zqx)\\b"), collapse = "|")),  "sexuality",  term)) %>%
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(nonhuman), "zqx)\\b"), collapse = "|")),  "nonhuman",  term)) %>%
    tidytable::mutate.(term = ifelse( str_detect(word, paste(c("\\b(?i)(zqx", na.omit(exclusion_terms), "zqx)\\b"), collapse = "|")),  "nonhuman",  term)) %>%
    tidytable::mutate.(ancestry_cnt = ifelse( str_detect( term,  "\\b(ancestry)\\b"),  1,  0)) %>% 
    tidytable::mutate.(cultural_cnt = ifelse( str_detect( term,  "\\b(cultural)\\b"),  1,  0)) %>%
    tidytable::mutate.(class_cnt = ifelse( str_detect( term,  "\\b(socioeconomic)\\b"),  1,  0)) %>%
    tidytable::mutate.(diversity_cnt = ifelse( str_detect( term,  "\\b(diversity)\\b"),  1,  0)) %>%
    tidytable::mutate.(disability_cnt = ifelse( str_detect( term,  "\\b(disability)\\b"),  1,  0)) %>%
    tidytable::mutate.(equity_cnt = ifelse( str_detect( term,  "\\b(equity)\\b"),  1,  0)) %>%
    tidytable::mutate.(lifecourse_cnt = ifelse( str_detect( term,  "\\b(lifecourse)\\b"),  1,  0)) %>%
    tidytable::mutate.(minority_cnt = ifelse( str_detect( term,  "\\b(minority)\\b"),  1,  0)) %>%
    tidytable::mutate.(migration_cnt = ifelse( str_detect( term,  "\\b(migration)\\b"),  1,  0)) %>%
    tidytable::mutate.(group_cnt = ifelse( str_detect( term,  "\\b(group)\\b"),  1,  0)) %>%
    tidytable::mutate.(racial_cnt = ifelse( str_detect( term,  "\\b(race/ethnicity)\\b"),  1,  0)) %>%
    tidytable::mutate.(sexgender_cnt = ifelse( str_detect( term,  "\\b(sex/gender)\\b"),  1,  0)) %>%
    tidytable::mutate.(sexuality_cnt = ifelse( str_detect( term,  "\\b(sexuality)\\b"),  1,  0)) %>%  
    tidytable::mutate.(nonhuman = ifelse( str_detect( term,  "\\b(nonhuman)\\b"),  1,  0)) %>%
    tidytable::mutate.(exclusion_terms = ifelse( str_detect( term,  "\\b(exclusion_terms)\\b"),  1,  0)) %>%
    group_by(sentence_id) %>% 
    summarise(across(ancestry_cnt:exclusion_terms, sum))
  
  # classify social diversity 
  tmp_df <- tmp_df %>% 
    # to classify soc_div_raw we basically count all mentions of diversity with one of the other terms in our cats 
    # in this version i will count both ancestry and population b/c divers* will be used in those contexts 
    tidytable::mutate.(total_cnt = ancestry_cnt + cultural_cnt + class_cnt + disability_cnt + diversity_cnt + equity_cnt + 
                         lifecourse_cnt + minority_cnt + migration_cnt + group_cnt + racial_cnt + sexgender_cnt + sexuality_cnt) %>% 
    tidytable::select.(sentence_id, total_cnt, everything()) %>%
    tidytable::arrange.(-total_cnt) %>% 
    tidytable::mutate.(soc_diversity = ifelse(diversity_cnt > 0 & total_cnt > diversity_cnt, diversity_cnt, 0)) %>% 
    # but if diversity is mentioned in the same sentence as any nonhuman term then don't count it as social diversity 
    tidytable::mutate.(soc_diversity = ifelse(soc_diversity > 0 & nonhuman > 0, 0, soc_diversity)) %>%
    # and if diversity is mentioned in the same sentence as the exclusion terms then don't count it as social diversity
    tidytable::mutate.(soc_diversity = ifelse(exclusion_terms > 0, 0, soc_diversity)) %>%
    tidytable::mutate.(entry_id = gsub("_.*", "", sentence_id)) %>% 
    tidytable::select.(entry_id, soc_diversity) %>%
    tidytable::summarise.(soc_diversity = sum(soc_diversity), .by = entry_id) %>% 
    tidytable::filter.(soc_diversity > 0) %>% 
    tidytable::mutate.("{{id}}" := as.integer(entry_id)) 
  
  if (remove_polysemes == TRUE) {
    
    tmp_df <- df %>% 
      tidytable::left_join.(tmp_df) %>% 
      tidytable::mutate.(soc_diversity = replace_na(soc_diversity, 0)) %>% 
      tidytable::filter.(soc_diversity == 1) %>% 
      polysemeR_0721(!!id, !!input) %>% 
      tidytable::mutate.(soc_diversity = ifelse(heterogeneity == 1, 0, 1)) %>% 
      tidytable::filter.(soc_diversity == 1) %>% 
      tidytable::select.(!!id, !!input, soc_diversity)
  } 
  
  if (remove_polysemes == FALSE) {
    
    tmp_df <- df %>% 
      tidytable::left_join.(tmp_df) %>% 
      tidytable::mutate.(soc_diversity = replace_na(soc_diversity, 0)) %>% 
      tidytable::filter.(soc_diversity == 1) %>% 
      tidytable::select.(!!id, !!input, soc_diversity)
  }
  
  df <- df %>% 
    tidytable::left_join.(tmp_df) %>% 
    tidytable::mutate.(soc_diversity = replace_na(soc_diversity, 0))
  
  df
  
}
















