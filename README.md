
#### The Rise of Diversity and Population Terminology in Biomedical Research

As of: 02-23-2021

This repository provides the source code for the Kramer and Lee's "The Rise of Diversity and Population Terminology in Biomedical Research." After uploading the PubMed/MEDLINE database with `PubMedPortable.py`, we use `R`'s `tidytext` package to examine trends in the use of diversity in more than 19 million scientific abstracts from 1990-2020. Overall, our analyses demonstrate that various types of "diversity" and other population terminiology, including race and ethnicity, are rising over time. While we provide some prelimiary results and a full appendix on our [project website](https://growthofdiversity.netlify.app/), the source code, database, and outputs are detailed below. This project is still in progress, but is updated often. 

#### TODO  

- Update data sources 
  - Post data dictionaries as `.rds` 
- Update H1 to reflect `.rds` output files to `data` folder 
- Update H1 diversity in context totals 
  - Should just be a minor change in the percentages df 
- Streamline H1-H3 to only include one phase (for full db)
- Convert analysis process to `.R` files and run `slurms`
- Update website files to input `.rds` and plot to make website construction faster 
- Update URL 
- Update project description to reflect MEDLINE data   

#### Code structure 

    ├── content
        ├── website_description
        ├── analyses
            ├── hypothesis1.Rmd
            ├── hypothesis2.Rmd
            ├── hypothesis3.Rmd
            ├── dictionaries.Rmd
            ├── supplementary.Rmd
    ├── data
        ├── dictionaries
            ├── pubmed_2021_top_journals.csv
            ├── preprocessing.csv
            ├── in_exclusions.csv
            ├── h1_dictionary.csv
            ├── h2_dictionary.csv
            ├── h3_dictionary.csv
            ├── tree_data.csv
            ├── country_recoding.R
            ├── omb_terms.R
            ├── subject_categories.csv
        ├── sensitivity_checks
        ├── text_results 
            ├── h1_results
            ├── h2_results
            ├── h3_results 
    ├── src
        ├── 01_pubmed_db
            ├── 01_download_medline.sh
            ├── 02_pubmed_parser.ipynb
            ├── 03_clean_db.sql
            ├── 04_pubmed_abstract_db.sql
            ├── 05_filtered_publications.R
            ├── 06_articles_per_journal.sql
            ├── 07_articles_per_year.sql
            ├── 08_biomedical_abstracts.sql
            ├── 09_check_abstracts_tbl.sql
        ├── 02_text_trends
            ├── 01_hypothesis1.R
            ├── 02_hypothesis2.R
            ├── 03_hypothesis3.R
            ├── 04_all_hypotheses.slurm
        ├── 03_text_relations
        ├── 04_collaborations

#### Database structure 

    ├── pubmed_2021
        ├── abstract_data
        ├── articles_per_journal
        ├── articles_per_year
        ├── biomedical_abstracts 
        ├── filtered_publications 





