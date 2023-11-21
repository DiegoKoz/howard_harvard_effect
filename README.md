# The Howard-Harvard effect: Institutional reproduction of intersectional inequalities


This repository contains the necessary code to reproduce the results from the paper *The Howard-Harvard effect: Institutional reproduction of intersectional inequalities*.

-The folder **code** contains the following:

- 0_SQL: scripts to query WOS database to build the dataset used in this research:
 - us_papers_JIF.sql
 - impact.sql
 - us_papers_citations_top.sql
 - WC_query.sql
 - HBCU_query.sql
 - HSI_query.sql
- 1_inst_cleaning: scripts for cleaning the instutional affiliation of authors in WOS and match it with Carnegie information:
  - 0_organise_institutional_type.R
  - 00_diversity_institutions.ipynb
  - 000_carnegie_query_builder.R
  - 1_institutional_impact.ipynb
  - 2_create_handcoding_file.ipynb
  - 3_parse_handcoding.ipynb
  - 4_institutions_impact.ipynb
  - 5_NewsReport.ipynb
  - 6_dataset_deciles.ipynb
- 2_inst_diversity: scripts to compute the distribution of authors by race and gender
  - 1_diversity_carnegie.ipynb
  - 2_stats_for_paper.ipynb
- 3_topics_and_inst: scripts to compute the distribution by race, gender, topic, and institutions
  - 1_carnegie_and_topics.ipynb
  - 2_carnegie_topics_and_rg.ipynb
  - 3_expected_distribution_of_topics.ipynb
  - 4_all_institutions.ipynb
  - 5_topic_labels.R
- 4_citations: Scripts to run the linear models on citations
  - 0_data_prep.ipynb
  - 1_agg_OLS.ipynb
  - 2_subset_OLS.ipynb
  - 3_avg_citations_rg_inst.ipynb
- 5_misc:analysis of the correlation w.r.t the expected values
  - correlations_control_by_rg_composition.R
- 6_figures: figures and tables for the paper:
  - 1_main_figures.R
  - 2_SI_figures.R
  - 3_SI_tables.R
  
Data availability: We used proprietary bibliometric data from a licensed version of Web of Science. To obtain the bibliometric data in the same manner as authors, readers can contact Clarivate Analytics at the following URL: https://clarivate.com/webofsciencegroup/solutions/web-of-science/contact-us/. Aggregate data can be obtained at: https://sciencebias.ebsi.umontreal.ca/. The code developed for this project will be available at https://github.com/DiegoKoz/howard_harvard_effect upon acceptance