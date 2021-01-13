

library(tidyverse)
library(RPostgreSQL)

filtered_publications <- read_csv("~/git/diversity/data/dictionaries/pubmed_2021 - top_journals.csv")

# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))

# write the edgelist to the database
dbWriteTable(conn, name = c(schema = "pubmed_2021" , name = "filtered_publications"), 
             value = filtered_publications, row.names = FALSE, temporary = TRUE)
# disconnect from postgresql
dbDisconnect(conn)