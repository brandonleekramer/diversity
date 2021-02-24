

rm(list = ls())

# load packages 
for (pkg in c("tidyverse", "RPostgreSQL", "R.utils", "XML")) {library(pkg, character.only = TRUE)}

#setwd("/scratch/kb7hp/pubmed")
unzipped <- gunzip("/scratch/kb7hp/pubmed/pubmed20n0002.xml.gz")
pubmed_xml <- xmlParse("/scratch/kb7hp/pubmed/pubmed20n0002.xml")
pmid_list <- as.numeric(xpathSApply(pubmed_xml, "//PubmedArticle/MedlineCitation/PMID", xmlValue))

#function to extract PMID and Authors
pubmed_df <- function(pmid_value){
  PMID <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/PMID'), xmlValue)
  # Year
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Journal/JournalIssue/PubDate/Year'), xmlValue)) > 0){
    year <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Journal/JournalIssue/PubDate/Year'), xmlValue)}
  else{year <- 'NA'}
  # Abstract Text 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Abstract/AbstractText'), xmlValue)) > 0){
    abstract <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Abstract/AbstractText'), xmlValue)}
  else{abstract <- 'NA'}
  # ArticleTitle 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/ArticleTitle'), xmlValue)) > 0){
    title <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/ArticleTitle'), xmlValue)}
  else{title <- 'NA'}
  # Title 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Journal/Title'), xmlValue)) > 0){
    journal <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Journal/Title'), xmlValue)}
  else{journal <- 'NA'}
  # Affiliation 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/AuthorList/Author/AffiliationInfo'), xmlValue)) > 0){
    affiliations <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/AuthorList/Author/AffiliationInfo'), xmlValue)}
  else{affiliations <- 'NA'}
  # Authors 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/AuthorList/Author'), xmlValue)) > 0){
    authors <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/AuthorList/Author'), xmlValue)}
  else{authors <- 'NA'}
  # Language 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Language'), xmlValue)) > 0){
    language <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Language'), xmlValue)}
  else{language <- 'NA'}
  # PublicationType 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/PublicationTypeList/PublicationType'), xmlValue)) > 0){
    pub_type <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/PublicationTypeList/PublicationType'), xmlValue)}
  else{pub_type <- 'NA'}
  
  # Funding GrantID 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/GrantID'), xmlValue)) > 0){
    funding_id <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/GrantID'), xmlValue)}
  else{funding_id <- 'NA'}
  # Funding Agency 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/Agency'), xmlValue)) > 0){
    funding_agency <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/Agency'), xmlValue)}
  else{funding_agency <- 'NA'}
  # Funding Country 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/Country'), xmlValue)) > 0){
    funding_country <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/Country'), xmlValue)}
  else{funding_country <- 'NA'}
  # Funding Acronym 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/Acronym'), xmlValue)) > 0){
    funding_acronym <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/GrantList/Acronym'), xmlValue)}
  else{funding_acronym <- 'NA'}
  # Data Frame
  as.data.frame(cbind(PMID=PMID, abstract=abstract, year=year, journal=journal, title=title, affiliations=affiliations, authors=authors, language=language,
                      funding_id=funding_id, funding_agency=funding_agency, funding_country=funding_country, funding_acronym=funding_acronym, pub_type=pub_type, doi=doi))
} 

#loop through this function with a list of PMIDs
data_list <- lapply(pmid_list, pubmed_df)
abstracts_df <- as.data.frame(do.call("rbind", data_list), stringsAsFactors = FALSE)
abstracts_df <- abstracts_df %>% 
  group_by(PMID, year, title, journal, language, doi) %>% 
  summarize(abstract = str_c(abstract, collapse = " "),
            affiliations = str_c(affiliations, collapse = ";"),
            authors = str_c(authors, collapse = ";"),
            pub_type = str_c(pub_type, collapse = ";"),
            #language = str_c(language, collapse = ";"),
            funding_id = str_c(funding_id, collapse = ";"),
            funding_agency = str_c(funding_agency, collapse = ";"),
            funding_country = str_c(funding_country, collapse = ";"),
            funding_acronym = str_c(funding_acronym, collapse = ";")) %>% 
  ungroup() %>% 
  select(PMID, year, abstract, title, journal, affiliations, authors, funding_id, funding_agency, funding_country, language, funding_acronym, pub_type) 


# https://github.com/brandonleekramer/diversity/blob/master/src/00_pubmed-queries/01_pubmed-queries.Rmd


# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data  
intl_stats <- dbGetQuery(conn, "SELECT country, users, repos, commits, additions, deletions
                         FROM gh.sna_intl_ctry_nodelist_temp")
# disconnect from postgresql
dbDisconnect(conn)