
rm(list = ls())

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")

# connect to postgresql to get our data
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))

# query the users_gh data (table of all github users) 
diversity_abstracts <- dbGetQuery(conn, "SELECT * 
                                  FROM pubmed_2021.all_diversity_abstracts_0321 
                                  WHERE diversity = 1 ;")

# disconnect from postgresql database 
dbDisconnect(conn)

# look at counts by publication, by terms
diversity_abstracts %>% 
  summarise(across(where(is.numeric), sum))
  
diversity_abstracts %>% 
  group_by(publication) %>% 
  count() %>%
  arrange(-n)


diversity_sentences <- diversity_abstracts %>% 
  filter(soc_div_terms < 1) %>% 
  unnest_tokens(output = sentence, 
                input = abstract, 
                token = "sentences", 
                drop = FALSE) %>% 
  filter(grepl("diversity|diverse|diversified", sentence)) %>% 
  select(fk_pmid, year, sentence, abstract, everything())

diversity_bigrams <- diversity_sentences %>% 
  unnest_tokens(n_grams, abstract, token = "ngrams", n = 2)  %>% 
  filter(grepl("^diversity|^diverse|^diversified|diversity$|diverse$|diversified$", n_grams)) %>% 
  group_by(soc_diversity, n_grams) %>% 
  count() %>% arrange(-n)

diversity_trigrams <- diversity_sentences %>% 
  unnest_tokens(n_grams, abstract, token = "ngrams", n = 3)  %>% 
  filter(grepl("^diversity|^diverse|^diversified|diversity$|diverse$|diversified$", n_grams)) %>% 
  group_by(soc_diversity, n_grams) %>% 
  count() %>% arrange(-n)

diversity_fourgrams <- diversity_sentences %>% 
  unnest_tokens(n_grams, abstract, token = "ngrams", n = 4)  %>% 
  filter(grepl("^diversity|^diverse|^diversified|diversity$|diverse$|diversified$", n_grams)) %>% 
  group_by(soc_diversity, n_grams) %>% 
  count() %>% arrange(-n)

diversity_fivegrams <- diversity_sentences %>% 
  unnest_tokens(n_grams, abstract, token = "ngrams", n = 5)  %>% 
  filter(grepl("^diversity|^diverse|^diversified|diversity$|diverse$|diversified$", n_grams)) %>% 
  group_by(soc_diversity, n_grams) %>% 
  count() %>% arrange(-n)

diversity_ngrams <- bind_rows(diversity_bigrams, diversity_trigrams, 
                              diversity_fourgrams, diversity_fivegrams) %>% 
  filter(grepl("\\b(?i)(diversity$)\\b", n_grams)) %>%
  arrange(-n)

diversity_ngrams_filtered <- diversity_ngrams %>% 
  filter(!grepl("diverse applications|diverse signaling|diverse functional|diverse bacterial|diverse biological processes|diverse biological functions|diverse tumor|diverse cancer|diverse neuronal|diverse eukaryotic|diverse extracellular|diverse stimuli|diverse fields|diverse tissues|diverse immune|diverse proteins|diverse regulatory|diverse ligands|diverse pathogens|diverse transcriptional|diverse plants|sequence diversity|nucleotide diversity|microbial diversity|antigenic diversity|clonal diversity|fungal diversity|antibody diversity|microbiome diversity|transcriptome diversity|transcript diversity|diversity of bacteria|diversity of microbial|diversity of proteins|diversity of microorganisms|expression diversity", n_grams))

# order the statement above by types 
# order these specualitive ones in tiers based on probability groupings 
not_sure_about <- something %>% 
  filter(!grepl("diverse range|diverse set|diverse array|diverse aspects|diverse effects|diverse repertoire|diverse areas|diverse environmental|diverse collection|diverse environments|diverse processes|diverse sources|diverse datasets", n_grams))

starts_with_diverse <- diversity_ngrams_filtered %>% 
  filter(grepl("^diverse", n_grams))


sum(diversity_ngrams$n)
sum(diversity_ngrams_filtered$n)

false_positives <- data.frame(biological = c("agonist(s)", "antibod(y|ies)", "antigen(s|ic)"))
false_positives$cellular <- c("agonist(s)", "antibod(y|ies)", "antigen(s|ic)")

false_positives <- read_csv("diversity_project - false_positives.csv")
fp_diverse_x <- paste(c("\\b(?i)(diverse (biological function(s)|biological process(es)", na.omit(false_positives$cellular), "zqx))\\b"), collapse = "|")
fp_x_diversity <- paste(c("\\b(?i)(zqx", na.omit(false_positives$cellular), "extracellular|zqx) diversity)\\b"), collapse = "|")
fp_diversity_of <- paste(c("\\b(?i)(diversity of (zqx", na.omit(false_positives$cellular), "zqx)\\b"), collapse = "|")
fp_div_hetero <- paste(c("\\b(?i)(zqx", na.omit(false_positives$heterogeneity), "zqx)\\b"), collapse = "|")

pubmed_data <- pubmed_data %>% 
  dt_mutate(fp_diverse_x = ifelse(test = str_detect(
    string = abstract, pattern = fp_diverse_x), yes = 1, no = 0)) %>% 
  dt_mutate(fp_x_diversity = ifelse(test = str_detect(
    string = abstract, pattern = fp_x_diversity), yes = 1, no = 0)) %>% 
  dt_mutate(fp_diversity_of = ifelse(test = str_detect(
    string = abstract, pattern = fp_diversity_of), yes = 1, no = 0)) %>%
  dt_mutate(fp_div_hetero = ifelse(test = str_detect(
    string = abstract, pattern = fp_div_hetero), yes = 1, no = 0)) 
















