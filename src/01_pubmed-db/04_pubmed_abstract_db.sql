CREATE MATERIALIZED VIEW pubmed_2021.abstract_data AS (

WITH abs_tbl AS (
	SELECT fk_pmid, abstract_text AS abstract
	FROM pubmed_2021.tbl_abstract 
	LIMIT 10000
), lang_tbl AS (
	SELECT fk_pmid, language 
	FROM pubmed_2021.tbl_language 
	LIMIT 10000
), meta_tbl AS (
	SELECT pmid AS fk_pmid, article_title AS article_title, article_affiliation AS pub_affiliation 
	FROM pubmed_2021.tbl_medline_citation
	WHERE article_affiliation IS NOT NULL 
	LIMIT 10000
), inv_tbl AS (
	SELECT fk_pmid, investigator_affiliation AS inv_affiliation  
	FROM pubmed_2021.tbl_investigator
	LIMIT 10000
), grant_tbl AS (
	SELECT fk_pmid, agency AS grant_agency, country AS grant_country
	FROM pubmed_2021.tbl_grant
	LIMIT 10000
), jrnl_tbl AS (
	SELECT fk_pmid, pub_date_year AS year, title AS publication
	FROM pubmed_2021.tbl_journal
	LIMIT 10000
)

SELECT jrnl_tbl.fk_pmid, year, abstract, publication, article_title,
	   inv_affiliation, pub_affiliation, grant_agency, grant_country, language
FROM jrnl_tbl
FULL JOIN abs_tbl ON jrnl_tbl.fk_pmid = abs_tbl.fk_pmid
FULL JOIN lang_tbl ON jrnl_tbl.fk_pmid = lang_tbl.fk_pmid
FULL JOIN meta_tbl ON jrnl_tbl.fk_pmid = meta_tbl.fk_pmid
FULL JOIN inv_tbl ON jrnl_tbl.fk_pmid = inv_tbl.fk_pmid
FULL JOIN grant_tbl ON jrnl_tbl.fk_pmid = grant_tbl.fk_pmid
WHERE abstract IS NOT NULL AND language = 'eng' 
ORDER BY jrnl_tbl.fk_pmid ASC 
LIMIT 10000
); 
