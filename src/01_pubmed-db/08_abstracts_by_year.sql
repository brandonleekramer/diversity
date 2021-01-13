

SELECT year, COUNT(fk_pmid)
FROM pubmed_2021.biomedical_abstracts
GROUP BY biomedical_abstracts.year
ORDER BY year ASC; 