

####################################################################################### install.packages (for slurm)

# this file produces 2 outputs 
# 1. analyzes how all population terms are used by researchers within each country (counts & percentages)
# 2. analyzes the use of OMB/Census categories in this data 

library("tidyverse")
library("tidytext")
library("widyr")
library("RPostgreSQL")
library("data.table")
library("maditr")
library("purrr")
library("tm")
library("countrycode")
data(stop_words)

##################################################################################################### setting function 

test_h3a <- function(analysis_timeframe){
  
  ################################################################################################# ingestion/cleaning 
  
  #rm(list = ls())
  #analysis_timeframe <- "2018"
  
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
                          "SELECT DISTINCT(fk_pmid), year, abstract, pub_affiliation AS department  
                          FROM pubmed_2021.biomedical_abstracts 
                          WHERE year = ", analysis_timeframe, ";"))
  
  # disconnect from postgresql database 
  dbDisconnect(conn)
  
  str_c("Finished data pull at: ", Sys.time())
  
  # pulling our list of population terms 
  setwd("~/git/diversity/data/dictionaries/")
  h2_dictionary <- read_csv("diversity_project - h2_dictionary.csv") 
  all_pop_terms <- paste(c("\\b(?i)(zcx",h2_dictionary$term, "zxc)\\b"), collapse = "|")
  source("~/git/diversity/data/dictionaries/country_recoding.R")
  
  ########################################################## converts affiliation data into countries of authors
  
  pubmed_data <- pubmed_data %>%
    mutate(department = na_if(department, "{}")) %>% 
    drop_na(department) %>% 
    mutate(department = tm::removePunctuation(department)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, 
                                                       pattern = af), yes = "af", no = "")) %>%  
    #paste("af", detected_country, sep=","), no = "")) 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ax), 
                                     paste("ax", detected_country, sep=","), no = detected_country)) %>% 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = al), 
                                     paste("al", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = dz), 
                                     paste("dz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = as), 
                                     paste("as", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ad), 
                                     paste("ad", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ao), 
                                     paste("ao", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ai), 
                                     paste("ai", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = aq), 
                                     paste("aq", detected_country, sep=","), no = detected_country)) %>% 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ag), 
                                     paste("ag", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ar), 
                                     paste("ar", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = am), 
                                     paste("am", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = aw), 
                                     paste("aw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = au), 
                                     paste("au", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = at), 
                                     paste("at", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = be), 
                                     paste("be", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bg), 
                                     paste("bg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = br), 
                                     paste("br", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = az), 
                                     paste("az", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bh), 
                                     paste("bh", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bs), 
                                     paste("bs", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bd), 
                                     paste("bd", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bb), 
                                     paste("bb", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = by), 
                                     paste("by", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bz), 
                                     paste("bz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bj), 
                                     paste("bj", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bm), 
                                     paste("bm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bt), 
                                     paste("bt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bo), 
                                     paste("bo", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bq), 
                                     paste("bq", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ba), 
                                     paste("ba", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bw), 
                                     paste("bw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bv), 
                                     paste("bv", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bn), 
                                     paste("bn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bf), 
                                     paste("bf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bi), 
                                     paste("bi", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cy), 
                                     paste("cy", detected_country, sep=","), no = detected_country)) %>% 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cz), 
                                     paste("cz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cn), 
                                     paste("cn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kh), 
                                     paste("kh", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cm), 
                                     paste("cm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ca), 
                                     paste("ca", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cv), 
                                     paste("cv", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ky), 
                                     paste("ky", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cf), 
                                     paste("cf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = td), 
                                     paste("td", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cl), 
                                     paste("cl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cx), 
                                     paste("cx", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cc), 
                                     paste("cc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = co), 
                                     paste("co", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = km), 
                                     paste("km", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ck), 
                                     paste("ck", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cr), 
                                     paste("cr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ci), 
                                     paste("ci", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = hr), 
                                     paste("hr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cu), 
                                     paste("cu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = cw), 
                                     paste("cw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = dk), 
                                     paste("dk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ee), 
                                     paste("ee", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = fi), 
                                     paste("fi", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = fr), 
                                     paste("fr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = dj), 
                                     paste("dj", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = dm), 
                                     paste("dm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = do), 
                                     paste("do", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ec), 
                                     paste("ec", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = eg), 
                                     paste("eg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sv), 
                                     paste("sv", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gq), 
                                     paste("gq", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = er), 
                                     paste("er", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = et), 
                                     paste("et", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = fk), 
                                     paste("fk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = fo), 
                                     paste("fo", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = fj), 
                                     paste("fj", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = de), 
                                     paste("de", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gr), 
                                     paste("gr", detected_country, sep=","), no = detected_country)) %>% 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gf), 
                                     paste("gf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pf), 
                                     paste("pf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tf), 
                                     paste("tf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ga), 
                                     paste("ga", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gm), 
                                     paste("gm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ge), 
                                     paste("ge", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gh), 
                                     paste("gh", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gi), 
                                     paste("gi", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gl), 
                                     paste("gl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gd), 
                                     paste("gd", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gp), 
                                     paste("gp", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gu), 
                                     paste("gu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gt), 
                                     paste("gt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gg), 
                                     paste("gg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gn), 
                                     paste("gn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gw), 
                                     paste("gw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gy), 
                                     paste("gy", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = hu), 
                                     paste("hu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ie), 
                                     paste("ie", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = it), 
                                     paste("it", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ht), 
                                     paste("ht", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = hm), 
                                     paste("hm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = va), 
                                     paste("va", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = hn), 
                                     paste("hn", detected_country, sep=","), no = detected_country)) %>% 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = hk), 
                                     paste("hk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = is), 
                                     paste("is", detected_country, sep=","), no = detected_country)) %>% 
    #mutate(detected_country = ifelse(test = str_detect(string = department, pattern = id), 
    #                                 paste("id", detected_country, sep=","), no = detected_country)) %>%  
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ir), 
                                     paste("ir", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = iq), 
                                     paste("iq", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = im), 
                                     paste("im", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = il), 
                                     paste("il", detected_country, sep=","), no = detected_country)) %>%
    # i did india down here so that i didn't have to name a variable "in", which created problems 
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = india_string), 
                                     paste("in", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = jp), 
                                     paste("jp", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lv), 
                                     paste("lv", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lu), 
                                     paste("lu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lt), 
                                     paste("lt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = jm), 
                                     paste("jm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = jo), 
                                     paste("jo", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kz), 
                                     paste("kz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ke), 
                                     paste("ke", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ki), 
                                     paste("ki", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kp), 
                                     paste("kp", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kr), 
                                     paste("kr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kw), 
                                     paste("kw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kg), 
                                     paste("kg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = xk), 
                                     paste("xk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = la), 
                                     paste("la", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lb), 
                                     paste("lb", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ls), 
                                     paste("ls", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lr), 
                                     paste("lr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ly), 
                                     paste("ly", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = li), 
                                     paste("li", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mt), 
                                     paste("mt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mo), 
                                     paste("mo", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mk), 
                                     paste("mk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mg), 
                                     paste("mg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mw), 
                                     paste("mw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = my), 
                                     paste("my", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mv), 
                                     paste("mv", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ml), 
                                     paste("ml", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mh), 
                                     paste("mh", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mq), 
                                     paste("mq", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mr), 
                                     paste("mr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mu), 
                                     paste("mu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mx), 
                                     paste("mx", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = yt), 
                                     paste("yt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = fm), 
                                     paste("fm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = md), 
                                     paste("md", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mc), 
                                     paste("mc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mn), 
                                     paste("mn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = me), 
                                     paste("me", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ms), 
                                     paste("ms", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ma), 
                                     paste("ma", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mz), 
                                     paste("mz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mm), 
                                     paste("mm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = na), 
                                     paste("na", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = nr), 
                                     paste("nr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = np), 
                                     paste("np", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = nl), 
                                     paste("nl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = nc), 
                                     paste("nc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = nz), 
                                     paste("nz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ni), 
                                     paste("ni", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ne), 
                                     paste("ne", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ng), 
                                     paste("ng", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = nu), 
                                     paste("nu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = nf), 
                                     paste("nf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mp), 
                                     paste("mp", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = no), 
                                     paste("no", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = om), 
                                     paste("om", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pk), 
                                     paste("pk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pw), 
                                     paste("pw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ps), 
                                     paste("ps", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pa), 
                                     paste("pa", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pg), 
                                     paste("pg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = py), 
                                     paste("py", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pe), 
                                     paste("pe", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ph), 
                                     paste("ph", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pn), 
                                     paste("pn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pl), 
                                     paste("pl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pt), 
                                     paste("pt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pr), 
                                     paste("pr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = qa), 
                                     paste("qa", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = re), 
                                     paste("re", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ro), 
                                     paste("ro", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ru), 
                                     paste("ru", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = rw), 
                                     paste("rw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = bl), 
                                     paste("bl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sh), 
                                     paste("sh", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = kn), 
                                     paste("kn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lc), 
                                     paste("lc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = mf), 
                                     paste("mf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = pm), 
                                     paste("pm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = vc), 
                                     paste("vc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ws), 
                                     paste("ws", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sm), 
                                     paste("sm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = st), 
                                     paste("st", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sa), 
                                     paste("sa", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sn), 
                                     paste("sn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = rs), 
                                     paste("rs", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sc), 
                                     paste("sc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sl), 
                                     paste("sl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sg), 
                                     paste("sg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sx), 
                                     paste("sx", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sk), 
                                     paste("sk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = si), 
                                     paste("si", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sb), 
                                     paste("sb", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = so), 
                                     paste("so", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = za), 
                                     paste("za", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gs), 
                                     paste("gs", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ss), 
                                     paste("ss", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = es), 
                                     paste("es", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = lk), 
                                     paste("lk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sd), 
                                     paste("sd", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sr), 
                                     paste("sr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sj), 
                                     paste("sj", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sz), 
                                     paste("sz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = se), 
                                     paste("se", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ch), 
                                     paste("ch", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = sy), 
                                     paste("sy", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tw), 
                                     paste("tw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tj), 
                                     paste("tj", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tz), 
                                     paste("tz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = th), 
                                     paste("th", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tl), 
                                     paste("tl", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tg), 
                                     paste("tg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tk), 
                                     paste("tk", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = to), 
                                     paste("to", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tt), 
                                     paste("tt", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tn), 
                                     paste("tn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tr), 
                                     paste("tr", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tm), 
                                     paste("tm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tc), 
                                     paste("tc", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = tv), 
                                     paste("tv", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ug), 
                                     paste("ug", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ua), 
                                     paste("ua", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ae), 
                                     paste("ae", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = gb), 
                                     paste("gb", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = uy), 
                                     paste("uy", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = uz), 
                                     paste("uz", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = vu), 
                                     paste("vu", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ve), 
                                     paste("ve", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = vn), 
                                     paste("vn", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = vg), 
                                     paste("vg", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = vi), 
                                     paste("vi", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = wf), 
                                     paste("wf", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = eh), 
                                     paste("eh", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = ye), 
                                     paste("ye", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = zm), 
                                     paste("zm", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = zw), 
                                     paste("zw", detected_country, sep=","), no = detected_country)) %>%
    mutate(detected_country = ifelse(test = str_detect(string = department, pattern = us), 
                                     paste("us", detected_country, sep=","), no = detected_country)) %>% 
    dplyr::rename(country_code = detected_country) 
  
  # removing the commas at the end of the strings from the column just created
  pubmed_data$country_code <- gsub("\\,$", "", pubmed_data$country_code)
  pubmed_data$country_code <- trimws(pubmed_data$country_code)
  
  pubmed_data <- pubmed_data %>% 
    separate_rows(country_code, sep = "," , convert = FALSE) %>% 
    mutate(country_code = trimws(country_code)) %>% 
    mutate(country_code = na_if(country_code, ""))
  
  pubmed_data$country <- countrycode::countrycode(pubmed_data$country_code, 
                                                origin = "iso2c", destination = "country.name")
  
  ######################################################################### bank total articles each year by country 
  
  total_articles_gb_year_country <- pubmed_data %>% 
    group_by(country, year) %>% 
    count(sort = TRUE) %>% 
    rename(total_pubs = n)
  
  ########################################################################## tokenizing the abstract data into words
  
  pubmed_data <- pubmed_data %>% 
    unnest_tokens(word, abstract) %>% 
    anti_join(stop_words)
  
  ################################################################################################# filter data down
  
  filtered_diversity_terms <- h2_dictionary$term
  
  pubmed_data <- pubmed_data %>% 
    filter(word %in% filtered_diversity_terms)
  
  ########################################################################## summarize the data for graphing 
  
  pop_terms_bycountry <- pubmed_data %>% 
    mutate(diversity = ifelse(test = str_detect(string = word, pattern = "diversity"), 1, 0)) %>% 
    mutate(term = ifelse(test = str_detect(string = word, pattern = all_pop_terms), yes = "all population terms", no = word)) %>%  
    mutate(all_pop_terms = ifelse(test = str_detect(string = term, pattern = "\\b(all population terms)\\b"), 1, 0)) %>% 
    select(fk_pmid, year, country, word, term, diversity, all_pop_terms) 
  
  # diversity check (group_by id)
  pop_terms_byid <- pop_terms_bycountry %>% 
    group_by(fk_pmid, year, country) %>% 
    summarise(across(diversity:all_pop_terms, sum)) %>%
    mutate(soc_diversity = if_else(diversity > 1 & all_pop_terms > 1, 1, 0)) 
  
  # counts by year (group_by year)
  pop_terms_bycountry_output <- pop_terms_bycountry %>% 
    drop_na() %>% 
    group_by(year) %>% 
    count(term, country, sort = TRUE)  
  
  ##########################################################################  analyzing by percentages 
  
  bycountry_prc_counts <- pop_terms_bycountry %>% 
    filter(all_pop_terms == 1) %>% 
    group_by(country, year) %>% 
    summarise(includes_terms = n_distinct(fk_pmid)) %>% 
    right_join(total_articles_gb_year_country, by = c("country", "year")) %>% 
    mutate(prc_pop_terms = round(includes_terms / total_pubs * 100, digits = 2))
  
  ########################################################################## save the data as rds files 
  
  setwd("~/git/diversity/data/text_results/h3_results/")
  write_rds(pop_terms_byid, str_c("h3_pop_terms_by_id_",analysis_timeframe,".rds"))
  write_rds(pop_terms_bycountry_output, str_c("h3_pop_terms_by_country_",analysis_timeframe,".rds"))
  write_rds(bycountry_prc_counts, str_c("h3_pop_prcs_by_country_",analysis_timeframe,".rds"))
  
  str_c("Finished all processes for ",analysis_timeframe, " at: ", Sys.time())
  
}


##################################################################################### for loop of all years 

for (year in 1990:1993) {
  test_h3a(year)
}

str_c("Finished all processes for all years at: ", Sys.time())

####################################################################################### aggregate all years 

setwd("~/git/diversity/data/text_results/h3_results/")

# percentages for all sets 
h3_pop_terms_by_id <- list.files(pattern="h3_pop_terms_by_id_*") %>% 
  map_df(~read_rds(.)) 

# percentages for all sets 
h3_pop_terms_by_country <- list.files(pattern="h3_pop_terms_by_country_*") %>% 
  map_df(~read_rds(.)) 

# overall set counts 
h3_pop_prcs_by_country <- list.files(pattern="h3_pop_prcs_by_country_*") %>% 
  map_df(~read_rds(.)) 

setwd("~/git/diversity/data/text_results/h3_results/")
write_rds(h3_pop_terms_by_id, "h3_all_pop_terms_by_id.rds")
write_rds(h3_pop_terms_by_country, "h3_all_pop_terms_by_country.rds")
write_rds(h3_pop_prcs_by_country, "h3_all_pop_prcs_by_country.rds")




