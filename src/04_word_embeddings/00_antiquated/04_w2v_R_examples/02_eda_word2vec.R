
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

embeddings_path = "~/git/diversity/data/word_embeddings/"
setwd(embeddings_path)
model_1990_95 <- read.word2vec("pubmed_word2vec_1990_95.bin")
model_2015_20 <- read.word2vec("pubmed_word2vec_2015_20.bin")

# set vectors of interest 
vectors_of_interest = c("race", "racial", "ethnic", "ethnicity", "diversity")
embedding_1990_95 <- predict(model_1990_95, vectors_of_interest, type = "embedding")
embedding_2015_20 <- predict(model_2015_20, vectors_of_interest, type = "embedding")

# look at the neighbors 
nn_1990_95 <- predict(model_1990_95, vectors_of_interest, type = "nearest", top_n = 10000)
nn_2015_20 <- predict(model_2015_20, vectors_of_interest, type = "nearest", top_n = 10000)

nn_1990_95
nn_2015_20$race %>% 
  filter(grepl("div", term2))

nn_1990_95$diversity %>% 
  filter(grepl("gender", term2))

nn_2015_20$diversity %>% 
  filter(grepl("gender", term2))












#################################################################################################### references

# https://github.com/bnosac/word2vec
# https://bnosac.github.io/udpipe/docs/doc1.html
# https://groups.google.com/g/gensim/c/17Knu4Xoe9U?pli=1
# https://www.nature.com/articles/s41597-019-0055-0#Sec2
# https://github.com/RaRe-Technologies/gensim/issues/467
# https://stackoverflow.com/questions/26569299/word2vec-number-of-dimensions
# https://juliasilge.github.io/why-r-webinar/#29
# https://ep.liu.se/ecp/131/039/ecp17131039.pdf
# https://github.com/ncbi-nlp/BioWordVec
