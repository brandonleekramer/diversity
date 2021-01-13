
-- this filters down the table of all_abstracts to the top-250 biomedical abstracts 

CREATE MATERIALIZED VIEW pubmed_2021.biomedical_abstracts AS (
SELECT A.fk_pmid, A.year, A.abstract, A.publication, A.article_title, 
	   A.inv_affiliation, A.pub_affiliation, A.grant_agency, A.grant_country, A.language
FROM pubmed_2021.abstract_data A 
INNER JOIN pubmed_2021.filtered_journals
ON A.publication = filtered_journals.publication 
WHERE year > 1989 AND year < 2021 
); 