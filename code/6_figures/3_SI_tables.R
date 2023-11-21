library(tidyverse)
library(fs)
library(ggthemes)
library(scales)
library(wesanderson)
library(ggpubr)
library(ggtext)
library(stringr)
library(glue)
library(ggalt)
library(gt)
library(ggrepel)
library(readxl) 
library(GGally)

load('data/shiny_data.Rdata')


sorted_cat <- c('top_10','top_100','not_top','more_selective','selective',
                'inclusive','low','mid','high')
cat_labels <- c('Top 10','Top 100','Not Top','More Selective','Selective',
                'Inclusive','Low (0.1, 1.47)','Medium (1.48, 1.74)','High (1.77, 4.07)' )

inst_group_cat <- c('usnr_rank_cat','selindex','avg_citations_Q')
inst_group_labels <- c('US News & World Report','Carnegie Selectivity Index','Average citations of institutions')



# Table S1 ----------------------------------------------------------------

n_papers_by_institution <- read_excel("data/n_papers_by_institution.xlsx")

n_papers_by_institution %>% 
  select(n_papers,carnegie_id, hbcu, hsi, womens, selindex,avg_citations_Q,usnr_rank_cat) %>% 
  pivot_longer(-c(n_papers,carnegie_id), names_to = 'group', values_to = 'cat') %>% 
  group_by(group, cat) %>% 
  summarise(min = min(n_papers),
            mean = mean(n_papers),
            max = max(n_papers)) %>% 
  filter(!is.na(cat),cat!=2)

table_S1 <- read_excel("data/table_S1.xlsx", range = "A1:E14")

many_colors = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[1:100]
table_S1 %>% 
  group_by(group_cat) %>% 
  gt() %>% 
  tab_options(table.font.names='arial', table.font.size = 10) %>% 
  data_color(
    columns = `# papers`,
    colors = scales::col_numeric(
      palette = many_colors,
      domain = summary(table_S1$`# papers`)[c(1,6)]
    )
  ) %>% 
  data_color(
    columns = `# authors`,
    colors = scales::col_numeric(
      palette = many_colors,
      domain = summary(table_S1$`# authors`)[c(1,6)]
    )
  ) %>% 
  data_color(
    columns = `# institutions`,
    colors = scales::col_numeric(
      palette = many_colors,
      domain = summary(table_S1$`# institutions`)[c(1,6)]
    )
  ) %>% gtsave('figures/tables/table_S1.docx')



# models parameters -------------------------------------------------------


inst_group_cat_ols <- c('usnr_rank_cat','selindex','avg_citations_Q','avg_citations')
inst_group_labels_ols <- c('US News & World Report','Carnegie Selectivity Index',
                       'Average citations of institutions(discrete)',
                       'Average citations of institutions(continuous)')

agg_ols %>% 
  filter(str_detect(dep,'norm')) %>% 
  mutate(dependent_var = case_when(str_detect(dep,'cit')~'topic normalized citations',
                                   str_detect(dep,'FI')~'topic normalized JIF')) %>% 
  select(-...1,-var_group,-dep,-n) %>%
  mutate(institution_cov = factor(institution_cov,levels=inst_group_cat_ols,labels=inst_group_labels_ols),
         coeff = number(coeff, accuracy = 0.01),
         conf_lower = number(conf_lower,accuracy = 0.01),
         conf_higher = number(conf_higher,accuracy = 0.01),
         coeff = case_when(pvals<0.001 ~ glue("{coeff}***\n({conf_lower}, {conf_higher})"),
                           pvals<0.01 ~ glue("{coeff}**\n({conf_lower}, {conf_higher})"),
                           pvals<0.05 ~ glue("{coeff}*\n({conf_lower}, {conf_higher})"),
                           TRUE ~glue("{coeff}\n({conf_lower}, {conf_higher})"))) %>% 
  select(-pvals,-conf_lower,-conf_higher) %>% 
  ungroup() %>% 
  pivot_wider(names_from = institution_cov,values_from = coeff,values_fill = '') %>% 
  group_by(dependent_var) %>% 
  gt() %>% 
  gtsave('figures/tables/table_S2.docx')


ols_subsets %>% 
  filter(str_detect(dep,'norm'), university_group!='avg_citations_Q10') %>% 
  mutate(dependent_var = case_when(str_detect(dep,'cit')~'topic normalized citations',
                                   str_detect(dep,'FI')~'topic normalized JIF')) %>% 
  select(-dep,-n) %>%
  mutate(university_group = factor(university_group,levels=inst_group_cat,labels=inst_group_labels),
         coeff = number(coeff, accuracy = 0.01),
         conf_lower = number(conf_lower,accuracy = 0.01),
         conf_higher = number(conf_higher,accuracy = 0.01),
         coeff = case_when(pvals<0.001 ~ glue("{coeff}***\n({conf_lower}, {conf_higher})"),
                           pvals<0.01 ~ glue("{coeff}**\n({conf_lower}, {conf_higher})"),
                           pvals<0.05 ~ glue("{coeff}*\n({conf_lower}, {conf_higher})"),
                           TRUE ~glue("{coeff}\n({conf_lower}, {conf_higher})")),
         subset2=case_match(subset,
                            'top_10'~'high prestige',
                            'top_100'~'medium prestige',
                            'not_top'~'low prestige',
                            'more_selective'~'high prestige',
                            'selective'~'medium prestige',
                            'inclusive'~'low prestige',
                            'high'~'high prestige',
                            'mid'~'medium prestige',
                            'low'~'low prestige',
                            ),
         subset2 = factor(subset2,levels=c('high prestige','medium prestige',
                                           'low prestige'))) %>% 
  select(-pvals,-conf_lower,-conf_higher, -subset) %>% 
  ungroup() %>% 
  pivot_wider(names_from = subset2,values_from = coeff,values_fill = '') %>% 
  group_by(dependent_var) %>% 
  gt(rowname_col = 'university_group') %>% 
  tab_options(row_group.as_column = TRUE) %>% 
  gtsave('figures/tables/table_S3.docx')


# citation gaps -----------------------------------------------------------
rg_inst_gap2 <- rg_inst_gap %>% 
  rename(Group=categories) %>% 
  mutate(Group= str_replace_all(Group,'_',' '),
         inst_group = case_when(inst_group=='usnr_rank_cat' ~'US news report ranking',
                                inst_group=='selindex' ~'Carnegie Selectivity Index',
                                inst_group=='avg_citations_Q' ~'Average citations of institutions',
                                inst_group=='hhw' ~'Women and Minority Serving Institutions'),
         Group = case_when(Group=='hbcu' ~'HBCU',
                           Group=='hsi' ~'HSI',
                           Group=='womens' ~'WC',
                           TRUE ~ Group)) %>% 
  filter(inst_group != 'Women and Minority Serving Institutions')

rg_inst_gap %>% 
  mutate(categories = factor(categories, levels=sorted_cat,labels=cat_labels),
         inst_group = factor(inst_group, levels=inst_group_cat, labels= inst_group_labels)) %>% 
  filter(metric=='norm_cit_all_IAC', !is.na(categories)) %>% 
  select(-metric) %>% 
  arrange(categories) %>% 
  group_by(inst_group) %>%
  rename(Group=categories) %>% 
  gt() %>%
  tab_options(table.font.names="Times New Roman", table.font.size = 10, row_group.font.weight =  'bold', column_labels.font.weight = 'bold') %>% 
  fmt_percent(columns= `Black\nMen`:`White\nWomen`) %>% 
  data_color(
    columns = `Black\nMen`:`White\nWomen`,
    colors = scales::col_numeric(
      palette = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[1:100],
      domain = range(rg_inst_gap %>% filter(metric=='norm_cit_all_IAC') %>% select(`Black\nMen`:`White\nWomen`))
    )
  ) %>% 
  gtsave('figures/tables/table_S4.docx')

rg_inst_gap %>% 
  mutate(categories = factor(categories, levels=sorted_cat,labels=cat_labels),
         inst_group = factor(inst_group, levels=inst_group_cat, labels= inst_group_labels)) %>% 
  filter(metric=='norm_FI_2', !is.na(categories)) %>% 
  select(-metric) %>% 
  arrange(categories) %>% 
  group_by(inst_group) %>%
  rename(Group=categories) %>% 
  gt() %>% 
  tab_options(table.font.names='arial', table.font.size = 10) %>% 
  fmt_percent(columns= `Black\nMen`:`White\nWomen`) %>% 
  data_color(
    columns = `Black\nMen`:`White\nWomen`,
    colors = scales::col_numeric(
      palette = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[1:100],
      domain = range(rg_inst_gap %>% filter(metric=='norm_FI_2') %>% select(`Black\nMen`:`White\nWomen`))
    )
  ) %>% 
  gtsave('figures/tables/table_S5.docx')

rg_inst_gap %>% 
  mutate(categories = factor(categories, levels=sorted_cat,labels=cat_labels),
         inst_group = factor(inst_group, levels=inst_group_cat, labels= inst_group_labels)) %>% 
  filter(metric=='cit_rel_all_IAC', !is.na(categories)) %>% 
  select(-metric) %>% 
  arrange(categories) %>% 
  group_by(inst_group) %>%
  rename(Group=categories) %>% 
  gt() %>% 
  tab_options(table.font.names='arial', table.font.size = 10) %>% 
  fmt_percent(columns= `Black\nMen`:`White\nWomen`) %>% 
  data_color(
    columns = `Black\nMen`:`White\nWomen`,
    colors = scales::col_numeric(
      palette = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[1:100],
      domain = range(rg_inst_gap %>% filter(metric=='cit_rel_all_IAC') %>% select(`Black\nMen`:`White\nWomen`))
    )
  ) %>% 
  gtsave('figures/tables/table_S6.docx')

rg_inst_gap %>% 
  mutate(categories = factor(categories, levels=sorted_cat,labels=cat_labels),
         inst_group = factor(inst_group, levels=inst_group_cat, labels= inst_group_labels)) %>% 
  filter(metric=='FIR_2', !is.na(categories)) %>% 
  select(-metric) %>% 
  arrange(categories) %>% 
  group_by(inst_group) %>%
  rename(Group=categories) %>% 
  gt() %>% 
  tab_options(table.font.names='arial', table.font.size = 10) %>% 
  fmt_percent(columns= `Black\nMen`:`White\nWomen`) %>% 
  data_color(
    columns = `Black\nMen`:`White\nWomen`,
    colors = scales::col_numeric(
      palette = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[1:100],
      domain = range(rg_inst_gap %>% filter(metric=='FIR_2') %>% select(`Black\nMen`:`White\nWomen`))
    )
  ) %>% 
  gtsave('figures/tables/table_S7.docx')


# interaction models ------------------------------------------------------
model_table_interaction <- function(pred='norm_FI_2',rg='rg',sec="no se_cluster" ){
  #se_clusters "no se_cluster" "carnegie_id"   "obereg"        "ESpecialite"  
  df <- rg_inst_interaction %>% 
    filter(
      dep == pred,
      variable!='Intercept', rg_cov==rg,se_clusters==sec) %>% 
    arrange(institution_cat) %>% 
    mutate(rg = str_extract(variable, '.*_[M|F]'),
           rg = factor(rg,levels= groups,labels = str_replace(groups_label,'\n',' ')),
           variable = str_remove(variable, '.*_[M|F]'),
           variable = case_when(!is.na(rg) ~ paste0(rg,variable),
                                TRUE ~ variable))
  
  df %>% 
    mutate(coeff = number(coeff, accuracy = 0.01),
           coeff = case_when(pvals<0.001 ~paste0(coeff,'***'),
                             pvals<0.01 ~paste0(coeff,'**'),
                             pvals<0.05 ~paste0(coeff,'*'),
                             TRUE ~coeff),
           institution_cat = glue("{institution_cat}({n})")) %>% 
    pivot_wider(id_cols = variable,names_from = institution_cat, values_from = coeff)
}

table_S6 <- model_table_interaction(pred='norm_cit_all_IAC',rg='rg',sec="no se_cluster" )

table_S6 %>% 
  as.data.frame(.) %>%  
  xlsx::write.xlsx('figures/tables/table_S8.xlsx',sheetName = 'citations',showNA = FALSE,row.names = FALSE, append = FALSE)

table_S6_2 <- model_table_interaction(pred='norm_FI_2',rg='rg',sec="no se_cluster" )

table_S6_2 %>% 
  as.data.frame(.) %>%  
  xlsx::write.xlsx('figures/tables/table_S8.xlsx',sheetName = 'JIF',showNA = FALSE,row.names = FALSE, append = TRUE)


