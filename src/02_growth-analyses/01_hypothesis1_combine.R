
rm(list = ls())

library("tidyverse")
setwd("~/git/diversity/data/text_results/")

# percentages for all sets 
h1_set_prc_trends <- list.files(pattern="h1_set_prc*") %>% 
  map_df(~read_rds(.)) %>% 
  select(year, total, everything())

# overall set counts 
h1_set_counts_full <- list.files(pattern="h1_set_counts_full*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(term) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall set counts by year 
h1_set_counts_trends <- list.files(pattern="h1_set_counts_trends*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(term, year) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall subset counts 
h1_subset_counts_full <- list.files(pattern="h1_subset_counts_full*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(word, term) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# overall subset counts by year 
h1_subset_counts_trends <- list.files(pattern="h1_subset_counts_trends*") %>% 
  map_df(~read_rds(.)) %>% 
  group_by(word, term, year) %>% 
  summarize(count = sum(n)) %>% 
  arrange(-count)

# need to do the matrix aggregation next 

setwd("~/git/diversity/data/text_results/")
write_rds(h1_set_prc_trends, "h1_set_prc_trends_all.rds")
write_rds(h1_set_counts_full, "h1_set_counts_full_all.rds")
write_rds(h1_set_counts_trends, "h1_set_counts_trends_all.rds")
write_rds(h1_subset_counts_full, "h1_subset_counts_full_all.rds")
write_rds(h1_subset_counts_trends, "h1_subset_counts_trends_all.rds")









