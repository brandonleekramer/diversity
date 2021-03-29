
CREATE MATERIALIZED VIEW pubmed_2021.biomedical_articles_per_year AS (
SELECT year, COUNT(year)
FROM pubmed_2021.biomedical_abstracts
WHERE year > 1989 AND year < 2021
GROUP BY year); 