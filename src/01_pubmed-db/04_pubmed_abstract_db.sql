CREATE MATERIALIZED VIEW pubmed_2021.abstract_data AS (

WITH abs_tbl AS (
	SELECT fk_pmid, abstract_text AS abstract
	FROM pubmed_2021.tbl_abstract 
), lang_tbl AS (
	SELECT fk_pmid, language 
	FROM pubmed_2021.tbl_language 
), meta_tbl AS (
	SELECT pmid AS fk_pmid, article_title AS article_title, article_affiliation AS pub_affiliation 
	FROM pubmed_2021.tbl_medline_citation
	WHERE article_affiliation IS NOT NULL 
), inv_tbl AS (
	SELECT fk_pmid, investigator_affiliation AS inv_affiliation  
	FROM pubmed_2021.tbl_investigator
), grant_tbl AS (
	SELECT fk_pmid, agency AS grant_agency, country AS grant_country
	FROM pubmed_2021.tbl_grant
), jrnl_tbl AS (
	SELECT fk_pmid, pub_date_year AS year, title AS publication
	FROM pubmed_2021.tbl_journal
)

SELECT DISTINCT(abs_tbl.fk_pmid), year, abstract, publication, article_title,
	   inv_affiliation, pub_affiliation, grant_agency, grant_country, language
FROM abs_tbl
LEFT JOIN jrnl_tbl ON abs_tbl.fk_pmid = jrnl_tbl.fk_pmid
LEFT JOIN lang_tbl ON abs_tbl.fk_pmid = lang_tbl.fk_pmid
LEFT JOIN meta_tbl ON abs_tbl.fk_pmid = meta_tbl.fk_pmid
LEFT JOIN inv_tbl ON abs_tbl.fk_pmid = inv_tbl.fk_pmid
LEFT JOIN grant_tbl ON abs_tbl.fk_pmid = grant_tbl.fk_pmid
WHERE abstract IS NOT NULL AND year IS NOT NULL AND language = 'eng' 
ORDER BY abs_tbl.fk_pmid ASC 

); 