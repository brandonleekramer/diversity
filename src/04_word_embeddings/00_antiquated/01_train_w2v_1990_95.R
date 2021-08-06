
######################################################################################################## pkg mgmt 

library("readr")
library("dplyr") 
library("stringr")  
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("parallel")
library("tidyr")
library("word2vec")
library("tm")

#rm(list = ls())
embeddings_path = "~/git/diversity/data/word_embeddings/"

################################################################################################# ingestion/cleaning

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
                          WHERE year <= 1995;"))

# disconnect from postgresql database 
dbDisconnect(conn)

#################################################################################################### preprocessing

# lower case, remove punctuation, change the newline issue 
pubmed_data = pubmed_data %>% 
  mutate(abstract = tolower(abstract),
         abstract = removePunctuation(abstract),
         abstract = str_replace(abstract, "\n", " ")) #%>% sample_n(303370, weight = fk_pmid)

# training the word2vec model using 512 dims (neil's suggestion) and 5 iters 
model <- word2vec(x = pubmed_data$abstract, type = "cbow", dim = 512, iter = 5)

# save that stuff! 
setwd(embeddings_path)
write.word2vec(model,  str_c("pubmed_word2vec_1990_95.bin"))

#################################################################################################### references

# https://github.com/bnosac/word2vec
# https://groups.google.com/g/gensim/c/17Knu4Xoe9U?pli=1
# https://www.nature.com/articles/s41597-019-0055-0#Sec2
# https://github.com/RaRe-Technologies/gensim/issues/467
# https://stackoverflow.com/questions/26569299/word2vec-number-of-dimensions
# https://juliasilge.github.io/why-r-webinar/#29
# https://ep.liu.se/ecp/131/039/ecp17131039.pdf
# https://github.com/ncbi-nlp/BioWordVec


























