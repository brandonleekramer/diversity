
#rm(list = ls())

library("purrr")
setwd("~/git/diversity/data/text_results/h2_results/")

# percentages for all sets 
h2_all_counts <- list.files(pattern="h2_set_counts_*") %>% 
  map_df(~read_rds(.)) 

# overall set counts 
h2_all_prcs <- list.files(pattern="h2_prc_trends_*") %>% 
  map_df(~read_rds(.)) 

# need to do the matrix aggregation next 

setwd("~/git/diversity/data/text_results/h2_results/")
write_rds(h2_all_counts, "h2_all_counts.rds")
write_rds(h2_all_prcs, "h2_all_prcs.rds")








