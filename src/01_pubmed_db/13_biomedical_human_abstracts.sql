---- join to abstracts on the database

create materialized view pubmed_2021.biomedical_human_abstracts AS (
  select A.fk_pmid, A.year, abstract, publication, human_study, animal_study
  from pubmed_2021.biomedical_abstracts A
  inner join pubmed_2021.human_research_ids B
  on A.fk_pmid = B.fk_pmid 
); 
create materialized view pubmed_2021.biomedical_human_per_year as (
  select year, count(*) 
  from pubmed_2021.biomedical_human_abstracts
  group by year 
  order by year asc 
); 
create materialized view pubmed_2021.biomedical_human_per_journal as (
  select publication, count(*) as pub_count
  from pubmed_2021.biomedical_human_abstracts
  group by publication 
  order by pub_count desc
); 