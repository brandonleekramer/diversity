
#### The Rise of Diversity and Population Terminology in Biomedical Research

As of: 05-17-2021

This repository provides the source code for the Brandon Kramer and Catherine Lee's "The Rise of Diversity and Population Terminology in Biomedical Research." After uploading the PubMed/MEDLINE database with `PubMedPortable` in `Python`, we used `R`'s `tidytext` package to examine trends in the use of diversity in more than 2.5 million scientific abstracts from 1990-2020. Overall, our analyses demonstrate that various types of "diversity" and other population terminiology, including race and ethnicity, are rising over time. While we provide some prelimiary results and a full appendix on our [project website](https://growthofdiversity.netlify.app/), the source code, database, and outputs are detailed below. This project is still in progress, but is updated often. 

#### Code structure 

    ├── content
        ├── overview.Rmd
        ├── methods.Rmd
        ├── analyses
            ├── hypothesis1.Rmd
            ├── hypothesis2.Rmd
            ├── hypothesis3.Rmd
    ├── data
        ├── dictionaries
            ├── preprocessing.csv
            ├── in_exclusions.csv
            ├── h1_dictionary.csv
            ├── h2_dictionary.csv
            ├── h3_dictionary.csv
            ├── tree_data.csv
            ├── country_recoding.R
            ├── omb_terms.R
            ├── subject_categories.csv
        ├── journal_rankings
        ├── sensitivity_checks
        ├── text_results 
            ├── h1_results
            ├── h2_results
            ├── h3_results 
        ├── word_embeddings
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
            ├── 05_pub_figures.Rmd
            ├── supplementary_analyses
                ├── 06_aggregate_ids.R
                ├── 07_diversity_abstracts.sql
                ├── 08_diversity_abstracts.R
                ├── 09_soc_diversity_eda.R
                ├── 10_human_abstracts.R
        ├── 03_word_embeddings
            ├── 01_w2v_train.ipynb
            ├── 02_w2v_results.ipynb
        ├── 04_text_relations
            ├── unfinished_analyses
        ├── 05_collaborations
            ├── unfinished_analyses

#### Database structure 

    ├── pubmed_2021
        ├── abstract_data
        ├── articles_per_journal
        ├── articles_per_year
        ├── biomedical_abstracts 
        ├── filtered_publications 





