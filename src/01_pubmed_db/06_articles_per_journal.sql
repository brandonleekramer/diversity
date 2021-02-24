
CREATE MATERIALIZED VIEW pubmed_2021.articles_per_journal AS (
SELECT publication, COUNT(*)
FROM pubmed_2021.abstract_data 
GROUP BY publication 
ORDER BY count DESC); 