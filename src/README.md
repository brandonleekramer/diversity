
#### The Rise of Diversity and Population Terminology in Biomedical Research

As of: 05-17-2021

This repository provides the source code for the Brandon Kramer and Catherine Lee's "The Rise of Diversity and Population Terminology in Biomedical Research." After uploading the PubMed/MEDLINE database with `PubMedPortable` in `Python`, we used `R`'s `tidytext` package to examine trends in the use of diversity in more than 2.5 million scientific abstracts from 1990-2020. Overall, our analyses demonstrate that various types of "diversity" and other population terminiology, including race and ethnicity, are rising over time. While we provide some prelimiary results and a full appendix on our [project website](https://growthofdiversity.netlify.app/), the source code, database, and outputs are detailed below. This project is still in progress, but is updated often. 

#### Analysis Pipeline 

_Data Extraction._ Our dataset derives from the PubMed/MEDLINE database - a free and publically-available collection of scientific abstracts provided by the U.S. National Library of Medicine (2021). In full, the database contains metadata of more than 27 million publications stored in XML format, including author, abstract, and publication source information. To download these data, we used an open source Python package named `PubMedPortable` to automate the extraction of metadata from PubMed’s source files into a PostgreSQL database (Döring et al., 2016). Second, we filtered the dataset to include only 250 highly-ranked biomedical journals based on Elseveir’s 2019 CiteScore rankings (Nasir et al., 2019). Drawing inspiration from a filtering feature available in the Web of Science database that omits animal-related research articles, we created two binary indicator variables to classify “animal” and “human” abstracts, removing entries that focused exclusively on animal research without any mention of human-related terminology. In full, we analyzed 2,510,266 distinct abstracts during our hypothesis testing. 

- H1: The use of the term diversity and related terminology has increased in biomedical abstracts since 1990.

- H2: The use of U.S.-specific racial and ethnic categories outlined in the OMB Directive 15 has grown in use in biomedical abstracts.

_Trend Analysis._ Next, we used computational text analysis to measure the frequency that diversity-related terms were used in biomedical abstracts. To do this, we implemented a “nested dictionary” approach to supervised text mining. This strategy entails the curation of two separate “dictionaries” with a collection of “terms” that are allocated into distinct “categories”. Our primary objective in these analyses is to count all relevant terms in our sample based on whether they match terms in our dictionaries. These terms can be counted independently or can be grouped into the categories like those we discuss below. To test H1, our diversity dictionary contains 396 total terms that fall into the 12 following diversity-related categories: cultural, disability, diversity, equity/justice, lifecourse, migration, minority, race/ethnicity, sex/gender, sexuality, social class, and social diversity. To test H2, we constructed an “OMB/Census dictionary” that contains 59 terms allotted into 8 categories: Asian, Black/African American, ethnicity, Hispanic/Latinx, Native American, Pacific Islander, race, and White/Caucasian. After the construction of our dictionaries was completed, we used R’s `tidyverse` and `tidytext` packages to conducted several pre-processing steps to standardize our text data (Silge and Robinson, 2016; Wickham et al., 2019), including converting text to lower case, removing special characters and numbers, recoding select compound and hyphenated terms, unnesting terms to obtain counts within each abstract, and then classifying each term into the categories we constructed for the first two hypotheses. Using this procedure, we determined: (a) the raw counts of each term, including when terms are mentioned multiple times in each article, and (b) the total proportion of articles that these terms are used in, which was calculated by using a binary indicator variable based on whether a term is mentioned in the article and then taking the sum of articles that include those terms divided by the total number of articles in each year across our sample. NOTE: We also have code for a third analysis evaluating trends in a more comprehensive analysis of population terms (currently labeled H3 in correspondence with the website), but this work is likely to be included in a future paper. 

- H3: The concept of diversity has replaced the racial/ethnic labels in biomedical research.  

_Word Embeddings._ Finally, we used Python’s `gensim` module (Řehůřek and Sojka 2010) to examine how semantically similar the terms related to race, ethnicity, and diversity are in PubMed abstracts and compare how their semantic similarity changes over time using two word embedding models. To do this, we ingested the human-only abstracts from the 250 biomedical journals using all of the 1990-2000 abstracts and a random sample of the 2010-2020 abstracts to ensure both models were trained on datasets of the same size (n = 475,077). Next, we converted the text to lowercase, removed punctuation, numbers, digits, white space and stopwords, and then lemmatized to remove inflectional word endings. We then trained a `Word2Vec` model for each time period with words mentioned less than 5 times being removed from the model, the skip gram window set at 5, a vector dimensionality of 256, and the iterations set at 5. Using the results of each model, we performed three analyses. First, we extracted the most similar word vectors for the race, ethnicity, and diversity vectors based on their cosine similarity. Second, we compared the word vectors (i.e., race, racial, ethnic, ethnicity, diverse, and diversity) to one another within the 1990-1995 and 2015-2020 models, and then subsequently subtracted the later model’s score from the baseline to obtain a change score between the two time points. Third, we used the `TSNE` function from Python’s `scikit-learn` package (Pedregosa et al. 2011) to decompose the model into 2 dimensions to graph the race, ethnicity, and diversity vectors relative to one another in each model’s vector space. Lastly, we then developed a procedure to compare the cosine similarity of terms from our “diversity dictionary” between the 1990-1995 and 2015-2020 models, which were then plotted as heat maps using Python’s `seaborn` package (Waskom 2021).

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
            
