--- after the medline data was parsed i joined all of the tables i wanted 

WITH abs_tbl AS (
	SELECT fk_pmid, abstract_text AS abstract
	FROM pubmed_2021.tbl_abstract 
	LIMIT 1000
), lang_tbl AS (
	SELECT fk_pmid, language 
	FROM pubmed_2021.tbl_language 
	LIMIT 1000
), meta_tbl AS (
	SELECT pmid AS fk_pmid, article_title AS title, article_affiliation AS pub_affiliation 
	FROM pubmed_2021.tbl_medline_citation
	WHERE article_affiliation IS NOT NULL 
	LIMIT 1000
), inv_tbl AS (
	SELECT fk_pmid, investigator_affiliation AS inv_affiliation  
	FROM pubmed_2021.tbl_investigator
	LIMIT 1000
), grant_tbl AS (
	SELECT fk_pmid, agency AS grant_agency, country AS grant_country
	FROM pubmed_2021.tbl_grant
	LIMIT 1000
), jrnl_tbl AS (
	SELECT fk_pmid, pub_date_year AS year, title AS publication
	FROM pubmed_2021.tbl_journal
	LIMIT 1000 
)

SELECT jrnl_tbl.fk_pmid, year, abstract, title, publication, language, 
	   inv_affiliation, pub_affiliation, grant_agency, grant_country
FROM jrnl_tbl
FULL JOIN abs_tbl ON jrnl_tbl.fk_pmid = abs_tbl.fk_pmid
FULL JOIN lang_tbl ON jrnl_tbl.fk_pmid = lang_tbl.fk_pmid
FULL JOIN meta_tbl ON jrnl_tbl.fk_pmid = meta_tbl.fk_pmid
FULL JOIN inv_tbl ON jrnl_tbl.fk_pmid = inv_tbl.fk_pmid
FULL JOIN grant_tbl ON jrnl_tbl.fk_pmid = grant_tbl.fk_pmid
WHERE abstract IS NOT NULL AND language = 'eng' -- AND pub_date_year > 1989
ORDER BY jrnl_tbl.fk_pmid ASC 
LIMIT 1000; 

