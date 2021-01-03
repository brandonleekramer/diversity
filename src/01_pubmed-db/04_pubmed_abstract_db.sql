CREATE MATERIALIZED VIEW pubmed_2021.abstract_data AS (

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
LIMIT 1000 
); 






--- check for tables with less than 29k values 
WITH tbl AS (
	SELECT xml_file_name, COUNT(fk_pmid) 
	FROM pubmed_2021.tbl_pmids_in_file
	GROUP BY xml_file_name 
), missing_tables AS (
	SELECT * 
	FROM tbl 
	WHERE count < 29000
)
SELECT * FROM missing_tables LIMIT 1000; 

--- check for duplicates 
SELECT fk_pmid, COUNT( fk_pmid )
FROM pubmed_2021.tbl_abstract
GROUP BY fk_pmid
HAVING COUNT( fk_pmid )> 1
ORDER BY fk_pmid;








-- don't use - too much work -----
-- this one deletes all of the pmids from a given table with less than n files
-- the problem is that it only deletes entries from one table so i would 
-- have to run this on all the relevant tables for each list of tables 
-- its ultimately easier just to delete all the duplicates from each row later 

--CREATE TABLE pubmed_2021.pmids_to_delete AS (

WITH tbl AS (
	SELECT xml_file_name, COUNT(fk_pmid) 
	FROM pubmed_2021.tbl_pmids_in_file
	GROUP BY xml_file_name 
), missing_tables AS (
	SELECT * 
	FROM tbl 
	--WHERE count < 29000
	WHERE xml_file_name = 'pubmed21n0865.xml.gz'
), pmids_to_delete AS (
	SELECT fk_pmid, missing_tables.xml_file_name
	FROM pubmed_2021.tbl_pmids_in_file 
	INNER JOIN missing_tables
	ON tbl_pmids_in_file.xml_file_name = missing_tables.xml_file_name
	--LIMIT 1000
)

DELETE FROM pubmed_2021.tbl_abstract 
WHERE EXISTS (
SELECT FROM pmids_to_delete 
WHERE  pmids_to_delete.fk_pmid = tbl_abstract.fk_pmid
);

--);




