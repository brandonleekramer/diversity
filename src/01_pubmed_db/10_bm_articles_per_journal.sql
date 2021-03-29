
CREATE MATERIALIZED VIEW pubmed_2021.biomedical_articles_per_journal AS (
SELECT publication, COUNT(*)
FROM pubmed_2021.biomedical_abstracts 
GROUP BY publication 
ORDER BY count DESC);