
-- make sure the journals joined correctly 

WITH count_table AS (SELECT publication, COUNT(*)
FROM pubmed_2021.biomedical_abstracts 
GROUP BY publication
ORDER BY count DESC)

SELECT * 
FROM count_table
FULL JOIN pubmed_2021.filtered_publications
ON count_table.publication = filtered_publications.publication
ORDER BY overall_rank ASC; 

-- get the counts by year 

SELECT year, COUNT(fk_pmid)
FROM pubmed_2021.biomedical_abstracts
GROUP BY biomedical_abstracts.year
ORDER BY year ASC; 