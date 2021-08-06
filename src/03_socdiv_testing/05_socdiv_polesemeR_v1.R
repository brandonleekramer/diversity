
polysemeR_v1 <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # 
  setwd("~/git/diversity/data/dictionaries/antiquated/")
  false_positives <- read_csv("diversity_project - polysemeR.csv")
  fp_cell_dx <- paste(c("\\b(?i)(diverse (biological function(s)|biological process(es)", na.omit(false_positives$het_cellular), "zqx))\\b"), collapse = "|")
  fp_cell_xd <- paste(c("\\b(?i)((zqx", na.omit(false_positives$het_cellular), "extracellular|zqx) diversity)\\b"), collapse = "|")
  fp_cell_dof <- paste(c("\\b(?i)(diversity of (zqx", na.omit(false_positives$het_cellular), "zqx))\\b"), collapse = "|")
  fp_lcell_dx <- paste(c("\\b(?i)(diverse (zqx", na.omit(false_positives$het_likely_cellular), "zqx))\\b"), collapse = "|")
  fp_lcell_xd <- paste(c("\\b(?i)((zqx", na.omit(false_positives$het_likely_cellular), "zqx) diversity)\\b"), collapse = "|")
  fp_lcell_dof <- paste(c("\\b(?i)(diversity of (zqx", na.omit(false_positives$het_likely_cellular), "zqx))\\b"), collapse = "|")
  fp_meth_dx <-  paste(c("\\b(?i)(diverse (zqx", na.omit(false_positives$het_methods), "zqx))\\b"), collapse = "|")
  fp_meth_xd <- paste(c("\\b(?i)((zqx", na.omit(false_positives$het_methods), "zqx) diversity)\\b"), collapse = "|")
  fp_meth_dof <-  paste(c("\\b(?i)(diversity of (zqx", na.omit(false_positives$het_methods), "zqx))\\b"), collapse = "|")
  fp_proc_dx <-  paste(c("\\b(?i)(diverse (zqx", na.omit(false_positives$het_processes), "zqx))\\b"), collapse = "|")
  fp_proc_xd <- paste(c("\\b(?i)((zqx", na.omit(false_positives$het_processes), "zqx) diversity)\\b"), collapse = "|")
  fp_proc_dof <-  paste(c("\\b(?i)(diversity of (zqx", na.omit(false_positives$het_processes), "zqx))\\b"), collapse = "|")
  fp_gen_dx <-   paste(c("\\b(?i)(diverse (zqx", na.omit(false_positives$het_general), "zqx))\\b"), collapse = "|")
  fp_gen_xd <- paste(c("\\b(?i)((zqx", na.omit(false_positives$het_general), "zqx) diversity)\\b"), collapse = "|")
  fp_gen_dof <-   paste(c("\\b(?i)(diversity of (zqx", na.omit(false_positives$het_general), "zqx))\\b"), collapse = "|")
  
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.(fp_cell_dx = ifelse(
      stringr::str_detect(string = {{ input }}, fp_cell_dx), 1, 0)) %>% 
    tidytable::mutate.(fp_cell_xd = ifelse(
      stringr::str_detect(string = {{ input }}, fp_cell_xd), 1, 0)) %>% 
    tidytable::mutate.(fp_cell_dof = ifelse(
      stringr::str_detect(string = {{ input }}, fp_cell_dof), 1, 0)) %>% 
    tidytable::mutate.(fp_lcell_dx = ifelse(
      stringr::str_detect(string = {{ input }}, fp_lcell_dx), 1, 0)) %>% 
    tidytable::mutate.(fp_lcell_xd = ifelse(
      stringr::str_detect(string = {{ input }}, fp_lcell_xd), 1, 0)) %>% 
    tidytable::mutate.(fp_lcell_dof = ifelse(
      stringr::str_detect(string = {{ input }}, fp_lcell_dof), 1, 0)) %>% 
    tidytable::mutate.(fp_meth_dx = ifelse(
      stringr::str_detect(string = {{ input }}, fp_meth_dx), 1, 0)) %>% 
    tidytable::mutate.(fp_meth_xd = ifelse(
      stringr::str_detect(string = {{ input }}, fp_meth_xd), 1, 0)) %>% 
    tidytable::mutate.(fp_meth_dof = ifelse(
      stringr::str_detect(string = {{ input }}, fp_meth_dof), 1, 0)) %>% 
    tidytable::mutate.(fp_proc_dx = ifelse(
      stringr::str_detect(string = {{ input }}, fp_proc_dx), 1, 0)) %>% 
    tidytable::mutate.(fp_proc_xd = ifelse(
      stringr::str_detect(string = {{ input }}, fp_proc_xd), 1, 0)) %>% 
    tidytable::mutate.(fp_proc_dof = ifelse(
      stringr::str_detect(string = {{ input }}, fp_proc_dof), 1, 0)) %>% 
    tidytable::mutate.(fp_gen_dx = ifelse(
      stringr::str_detect(string = {{ input }}, fp_gen_dx), 1, 0)) %>% 
    tidytable::mutate.(fp_gen_xd = ifelse(
      stringr::str_detect(string = {{ input }}, fp_gen_xd), 1, 0)) %>% 
    tidytable::mutate.(fp_gen_dof = ifelse(
      stringr::str_detect(string = {{ input }}, fp_gen_dof), 1, 0)) %>% 
    as.data.frame()
  
  df <- df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(fp_cell_all = sum(across(contains("fp_cell")), na.rm = TRUE)) %>% 
    dplyr::mutate(fp_lcell_all = sum(across(contains("fp_lcell")), na.rm = TRUE)) %>% 
    dplyr::mutate(fp_meth_all = sum(across(contains("fp_meth")), na.rm = TRUE)) %>% 
    dplyr::mutate(fp_proc_all = sum(across(contains("fp_proc")), na.rm = TRUE))  
    dplyr::mutate(fp_gen_all = sum(across(contains("fp_gen")), na.rm = TRUE)) %>% 
    dplyr::mutate(fp_dx_all = sum(across(contains("_dx")), na.rm = TRUE)) %>% 
    dplyr::mutate(fp_xd_all = sum(across(contains("_xd")), na.rm = TRUE)) %>% 
    dplyr::mutate(fp_dof_all = sum(across(contains("_dof")), na.rm = TRUE)) %>%  
    dplyr::mutate(fp_div_all = sum(across(contains("_all")), na.rm = TRUE))
  
  df
  
}




rm(list = ls())

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")
source("~/git/diversity/scripts/diversitizeR.R")

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

setwd("~/git/diversity/data/sensitivity_checks/")
labeled_data <- read_csv("diversity_labeled_061221.csv") 
labeled_data <- labeled_data %>% 
  rename(labels = blk_code) %>% 
  select(fk_pmid, labels)
diversity_abstracts <- diversity_abstracts %>% 
  left_join(labeled_data, by = "fk_pmid")

# polysemeR
str_c("Started at: ", Sys.time())
diversity_abstracts <- diversity_abstracts %>% 
  polysemeR(abstract)
str_c("Finished at: ", Sys.time())
# 3:25 - 3:35

false_positives <- diversity_abstracts %>% 
  filter(soc_diversity == 1) %>% 
  select(fk_pmid, abstract, soc_diversity, labels, starts_with("fp_")) 

str(false_positives)

cell <- false_positives %>% 
  select(fk_pmid, abstract, fp_cell_dof, fp_cell_dx, fp_cell_xd, soc_diversity, labels, everything()) %>% 
  filter(fp_cell_dof == 1 | fp_cell_dx == 1 | fp_cell_xd == 1)

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(cell, "false_positives_cell.csv")

lcell <- false_positives %>% 
  select(fk_pmid, abstract, fp_lcell_dof, fp_lcell_dx, fp_lcell_xd, soc_diversity, labels, everything()) %>% 
  filter(fp_lcell_dof == 1 | fp_lcell_dx == 1 | fp_lcell_xd == 1)

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(lcell, "false_positives_lcell.csv")

methods <- false_positives %>% 
  select(fk_pmid, abstract, fp_meth_dof, fp_meth_dx, fp_meth_xd, soc_diversity, labels) %>% 
  filter(fp_meth_dof == 1 | fp_meth_dx == 1 | fp_meth_xd == 1) %>% 
  mutate(application = ifelse(str_detect(string = abstract, "\\b(?i)(applicatio(n|ns))\\b"), 1, 0)) %>%
  mutate(datasets = ifelse(str_detect(string = abstract, "\\b(?i)(dat(ase(t|ts)| se(t|ts)|sourc(e|es)| sourc(e|es)))\\b"), 1, 0)) %>%  
  mutate(discipline = ifelse(str_detect(string = abstract, "\\b(?i)(disciplin(e|es|ary))\\b"), 1, 0)) %>% 
  mutate(sample = ifelse(str_detect(string = abstract, "\\b(?i)(sampl(e|es))\\b"), 1, 0)) %>% 
  mutate(collection = ifelse(str_detect(string = abstract, "\\b(?i)(collectio(n|ns))\\b"), 1, 0)) %>% 
  mutate(models = ifelse(str_detect(string = abstract, "\\b(?i)(mode(l|ls))\\b"), 1, 0))

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(methods, "false_positives_meth.csv")

general <- false_positives %>% 
  select(fk_pmid, abstract, fp_gen_dof, fp_gen_dx, fp_gen_xd, soc_diversity, labels) %>% 
  filter(fp_gen_dof == 1 | fp_gen_dx == 1 | fp_gen_xd == 1) %>% 
  mutate(approach = ifelse(str_detect(string = abstract, "\\b(?i)(approac(h|hes))\\b"), 1, 0)) %>%
  mutate(area = ifelse(str_detect(string = abstract, "\\b(?i)(are(a|as))\\b"), 1, 0)) %>%  
  mutate(array = ifelse(str_detect(string = abstract, "\\b(?i)(arra(y|ys))\\b"), 1, 0)) %>% 
  mutate(aspect = ifelse(str_detect(string = abstract, "\\b(?i)(aspec(t|ts))\\b"), 1, 0)) %>% 
  mutate(field = ifelse(str_detect(string = abstract, "\\b(?i)(fiel(d|ds))\\b"), 1, 0)) %>% 
  mutate(range = ifelse(str_detect(string = abstract, "\\b(?i)(rang(e|es))\\b"), 1, 0)) %>% 
  mutate(set = ifelse(str_detect(string = abstract, "\\b(?i)(se(t|ts))\\b"), 1, 0)) %>% 
  mutate(environment = ifelse(str_detect(string = abstract, "\\b(?i)(environmen(t|ts|tal))\\b"), 1, 0))  

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(general, "false_positives_gen.csv")

processes <- false_positives %>% 
  select(fk_pmid, abstract, fp_proc_dof, fp_proc_dx, fp_proc_xd, soc_diversity, labels) %>% 
  filter(fp_proc_dof == 1 | fp_proc_dx == 1 | fp_proc_xd == 1) %>% 
  mutate(effect = ifelse(str_detect(string = abstract, "\\b(?i)(effec(t|ts))\\b"), 1, 0)) %>%
  mutate(mode = ifelse(str_detect(string = abstract, "\\b(?i)(mod(e|es))\\b"), 1, 0)) %>%  
  mutate(process = ifelse(str_detect(string = abstract, "\\b(?i)(proces(s|ses))\\b"), 1, 0)) %>% 
  mutate(response = ifelse(str_detect(string = abstract, "\\b(?i)(respons(e|es))\\b"), 1, 0)) %>% 
  mutate(stimuli = ifelse(str_detect(string = abstract, "\\b(?i)(stimul(us|i))\\b"), 1, 0)) %>% 
  mutate(repertoire = ifelse(str_detect(string = abstract, "\\b(?i)(repertoir(e|es))\\b"), 1, 0))

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(processes, "false_positives_proc.csv")


# fp_cell_all, fp_lcell_all, fp_meth_all, fp_proc_all, fp_gen_all, fp_dx_all, fp_xd_all, fp_dof_all, fp_div_all

polysemeR_v2 <- function(df, input){
  
  # load required packages 
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytable")
  
  # dictionary 
  setwd("~/git/diversity/data/dictionaries/")
  false_positives <- read_csv("diversity_project - polysemeR.csv")
  false_positives <- paste(c("\\b(?i)((diverse (biological function(s)|biological process(es)|biological effect(s)", 
                             na.omit(false_positives$false_positives), 
                             "zqx|functionally diverse))|((zqx", na.omit(false_positives$false_positives), "extracellular|zqx) diversity)", 
                             "(diversity of (zqx", na.omit(false_positives$false_positives), "zqx)))\\b"), collapse = "|")
  # classifier 
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    tidytable::mutate.(heterogeneity = ifelse(
      stringr::str_detect(string = {{ input }}, false_positives), 1, 0)) %>% 
    as.data.frame()
  
  df
  
}










