
create materialized view pubmed_2021.all_div_abstracts_0721 as (
	select distinct a.fk_pmid, a.year, abstract, b.publication,  
		soc_div_terms, h1_totals, h2_totals, h3_totals,
		asian, black, continental, cultural, directional, 
		disability, diversity, equity, ethnic, hispanic, 
		lifecourse, migration, minority, a.national, native_american, 
		omb_uscensus, pacific_islander, race_ethnicity, racial, sex_gender, 
		sexuality, socdiv as soc_diversity, social_class, subcontinental, subnational, white 
	from pubmed_2021.all_diversity_ids_0721 a
	inner join pubmed_2021.biomedical_abstracts b
	on a.fk_pmid = b.fk_pmid 
	order by soc_div_terms desc 
	--limit 100 
);

create materialized view pubmed_2021.soc_diversity_abstracts_0721 as (
	select distinct a.fk_pmid, a.year, abstract, b.publication,  
		soc_div_terms, h1_totals, h2_totals, h3_totals,
		asian, black, continental, cultural, directional, 
		disability, diversity, equity, ethnic, hispanic, 
		lifecourse, migration, minority, a.national, native_american, 
		omb_uscensus, pacific_islander, race_ethnicity, racial, sex_gender, 
		sexuality, socdiv as soc_diversity, social_class, subcontinental, subnational, white 
	from pubmed_2021.soc_diversity_ids_0721 a
	inner join pubmed_2021.biomedical_abstracts b
	on a.fk_pmid = b.fk_pmid  
	order by soc_div_terms desc 
	--limit 100 
);