
CREATE MATERIALIZED VIEW pubmed_2021.publication_counts AS (
SELECT publication, COUNT(*)
FROM pubmed_2021.abstract_data 
GROUP BY publication 
ORDER BY count DESC); 