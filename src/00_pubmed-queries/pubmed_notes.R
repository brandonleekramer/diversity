




# https://rstudio-pubs-static.s3.amazonaws.com/499292_d6edbb19b08f456097333fbf9443f9b7.html
library(XML)
library(easyPubMed)
library(methods)
setwd("/sfs/qumulo/qhome/kb7hp/data/diversity-data/")
data <- xmlParse('tilapia.xml')
class(data)
xmlRoot(data)
xmltop = xmlRoot(data) #gives content of root
xmlName(xmltop) #give name of node, PubmedArticleSet
xmlSize(xmltop) 
xmlName(xmltop[[1]])
xmlName(xmltop[[1]][[1]][[1]])

#create a list of main article PMIDs
pmid_list <- as.numeric(xpathSApply(data, "//PubmedArticle/MedlineCitation/PMID", xmlValue))

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
  # Title 
  if(length(xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Journal/Title'), xmlValue)) > 0){
    journal <- xpathSApply(data, paste('//PubmedArticle/MedlineCitation[PMID=',pmid_value,']/Article/Journal/Title'), xmlValue)}
  else{journal <- 'NA'}
  
  as.data.frame(cbind(PMID=PMID, journal=journal, abstract=abstract, year=year))
} 

#loop through this function with a list of PMIDs
data_list <- lapply(pmid_list, pubmed_df)
abstracts_df <- as.data.frame(do.call("rbind", data_list), stringsAsFactors = FALSE)

MedlineCitation/PMID Year AbstractText Title Affiliation LastName Initials PublicationType Language GrantID Agency Acronym Country






abstracts_df <- abstracts_df %>% 
  group_by(PMID) %>% 
  summarize(new_abstract = str_c(abstract, collapse = " "),
            new_affiliations = str_c(affiliations, collapse = ";")) %>%
  ungroup()

setwd("/sfs/qumulo/qhome/kb7hp/data/diversity-data/")
write_csv(test_table, "test_table.csv")


library(tidyverse)


library(data.table)
rm(list = ls())
setwd("/sfs/qumulo/qhome/kb7hp/data/diversity-data/")
data <- fread("tilapia_data.txt", sep = "|", header=FALSE, fill=TRUE, col.names=paste0("C", 1:13)) #this works!




my_PM_list <- articles_to_list(pubmed_data = my_abstracts_xml)

article_to_df()

setwd("/sfs/qumulo/qhome/kb7hp/data/diversity-data/")
theData <- extract_xml("practice.xml")


doc = xmlTreeParse("pubmed.xml", useInternal = TRUE)



data <- read.table("tilapia_data.txt", header=FALSE, sep="@@@", nrows=55, col.names=paste0("C", 1:13), fill=TRUE)


?read.table

#####
rm(list = ls())
setwd("/sfs/qumulo/qhome/kb7hp")
data <- fread("fish_data.txt", sep = "|", header=TRUE, fill=TRUE)
data <- fread("diversity_pubmed_data.txt", sep = "|", header=FALSE, fill=TRUE)

PMID|Year|AbstractText|Journal|Affiliation|LastName|Initials|PublicationType|Language|GrantID|Agency|Acronym|Country

MedlineCitation/PMID PubDate/Year AbstractText Title Affiliation LastName Initials PublicationType Language GrantID Agency Acronym Country


esearch -db pubmed -query "black tilapia" | efetch -format xml | xtract -pattern PubmedArticle -tab "|" -sep ";" -element MedlineCitation/PMID PubDate/Year AbstractText Title Affiliation LastName Initials PublicationType Language GrantID Agency Acronym Country > fish_data.txt

esearch -db pubmed -query "biologic* OR biomedic* OR medic* OR gene* OR pharma* AND (humans[Filter]) AND 1990[PDAT] : 2019[PDAT]" | efetch -format xml | xtract -pattern PubmedArticle -tab "|" -sep ";" -element MedlineCitation/PMID Year AbstractText Title Affiliation LastName Initials PublicationType Language GrantID Agency Acronym Country > diversity_pubmed_data.txt


echo -e "\n" > diversity_pubmed_data.txt
sed -i -e '$a\' diversity_pubmed_data.txt

esearch -db pubmed -query "black tilapia" | efetch -format xml | xtract -pattern PubmedArticle -tab "|" -sep ";" -def "NA" -element MedlineCitation/PMID Year AbstractText Affiliation Title LastName Initials PublicationType Language GrantID Agency Acronym Country > tilapia_data.txt



esearch -db pubmed -query "black tilapia south" | efetch -format xml > tilapia_data.xml

esearch –db pubmed –query “black tilapia south” | efetch -format xml > tilapia_data.xml


eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi
eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi


eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi$db=pubmed;&term=pharma+AND+(humans[Filter])+AND+1990[pdat]

#257518 (URL,UNIX)
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=pharma*+AND+(humans[Filter])+AND+1990[pdat]:2019[PDAT]


&api_key=afdc068e710575a51176be74708037989207


esearch -db pubmed -query "pharma* AND (humans[Filter]) AND 1990[PDAT] : 2019[PDAT]"

https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=black+tilapia+AND+1990[pdat]:2019[PDAT]&usehistory=y
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&term=black+tilapia+AND+1990[pdat]:2019[PDAT]&WebEnv=NCID_1_20838658_130.14.22.33_9001_1593204511_1771700350_0MetA0_S_MegaStore&query_key=1&retmode=xml


&api_key=afdc068e710575a51176be74708037989207

