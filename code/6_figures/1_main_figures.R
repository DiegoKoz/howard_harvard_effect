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


##this is not used in the app, so I only load it here
institutions_marginal_by_topic_socsci <- read_csv('../rg_institutions/results/institutions_topics/marginal_by_topic_gender_socsci.csv')
institutions_marginal_by_topic_health <- read_csv('../rg_institutions/results/institutions_topics/marginal_by_topic_gender_health.csv')

topic_labels <- read_csv('handcoding/topic_labels_socsci.csv')

clean_marginal <- function(df, type='socsci'){
  df %>%
    pivot_df_rg(.,name='prob') %>%
    left_join(top_words_socsci, by='topic') %>%
    left_join(retrieve_tp(type), by='topic') %>%
    rename(topic_proportion = proportion) %>%
    refactor_citation_quartiles(.)
}


institutions_marginal_by_topic_socsci <- institutions_marginal_by_topic_socsci %>%
  clean_marginal('socsci')
institutions_marginal_by_topic_health <- institutions_marginal_by_topic_health %>%
  clean_marginal('health')


#for reference
read_csv('results/institutions/rg_carnegie.csv')$avg_citations_Q %>% unique()

colors_pair <- RColorBrewer::brewer.pal(8, 'Paired')#[c(1,3,5,7,8,6,4,2)]
race_gender_order <- c("White_Men","Black_Men","Latinx_Men","Asian_Men","Asian_Women","Latinx_Women","Black_Women","White_Women")


scatterplot_labels <- function(grp=c('Black_Women', 'White_Men'),
                        carnegie_grp= c("top_10","top_100","not_top"), data = 'socsci'){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
 
  
  mean_citations <- retrieve_table(result = 'citations', set = data) %>%
    left_join(retrieve_table('marginal_by_topic',set=data) %>%  select(topic, group, gender, marginal_by_topic=prob)) %>% 
    group_by(topic) %>% 
    summarise(mean_citations = sum(mean*marginal_by_topic))
  
  carnegie_labels <- str_to_title(str_replace_all(
    str_remove(carnegie_grp,'avg_citations_'),'_', ' '))
  
  carnegie_labels <- case_when(carnegie_labels=='Hbcu'~ 'HBCU',
                               carnegie_labels=='Hsi'~ 'HSI',
                               carnegie_labels=='Womens'~ 'WC',
                               carnegie_labels=='Mid'~ 'Medium',
                               TRUE~carnegie_labels)
  
  prob_carnegie <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
    filter(carnegie_tag%in% carnegie_grp) %>%
    rename(prob_carnegie = prob) %>% 
    mutate(carnegie_tag = factor(carnegie_tag, levels =carnegie_grp,
                                 labels = carnegie_labels))
  
  mean_prob_carnegie <- left_join(prob_carnegie,df2 %>% 
                                    select(topic, topic_proportion) %>% 
                                    distinct(), by = "topic") %>% 
    group_by(carnegie_tag) %>% 
    summarise(prob = weighted.mean(prob_carnegie,topic_proportion))
  
  group_average <- df2 %>%
    mutate(rg = paste(group,gender, sep = '_' )) %>% 
    filter(rg%in%grp) %>% 
    # mutate(rg = str_replace_all(rg,'_','\n')) %>% 
    group_by(rg) %>% 
    summarise(prob = sum(prob))
  
  df_labels <- topic_labels %>% 
    mutate(topic= factor(topic)) %>% 
    select(-top_words)
  
  df <- df %>%
    mutate(rg = paste(group,gender, sep = '_' )) %>% 
    filter(rg%in%grp) %>%
    group_by(rg,topic,topic_proportion) %>%
    # group_by(group,gender,topic,topic_proportion) %>% 
    summarise(prob = sum(prob),
              top_words = unique(top_words)) %>% 
    left_join(mean_citations, by='topic') %>% 
    left_join(prob_carnegie, by='topic') %>% 
    left_join(df_labels, by = "topic") #only works for social sciences!
    # mutate(rg = factor(rg, levels=race_gender_order, labels= str_replace_all(race_gender_order,'_','\n'))) %>%
    
  plt <- df %>% 
  ggplot(., aes(prob_carnegie,prob,size= topic_proportion,#atan(prob/feminisation),
                  group=NA))+
    geom_point(aes(color=mean_citations))+
    # geom_smooth(method = 'lm', se = FALSE, show.legend = FALSE)+
    geom_smooth(method = 'lm', se = TRUE, show.legend = FALSE)+
    # geom_smooth(method = 'lm', se = FALSE, show.legend = FALSE)+
    geom_vline(data = mean_prob_carnegie, aes(xintercept = prob ), color='grey70')+
    geom_hline(data = group_average, aes(yintercept = prob ), color='grey70')+
    geom_text_repel(data = df %>% filter(!is.na(label)), aes(label=label))+
    labs(color= 'Citations',x='', y = '', size = 'Topic size')+
    scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 10, type = "continuous"),breaks=breaks_pretty(3)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
    scale_x_continuous(labels = percent, n.breaks = 4.1)+
    # stat_cor(aes(label = ..r.label..),output.type	 = 'text',show.legend  = FALSE, method='spearman')+
    stat_cor(aes(label = ..r.label..),cor.coef.name="ρ",digits=2,output.type = 'text',show.legend  = FALSE, method='spearman')+
    # facet_wrap(gender+group~carnegie_tag, scales = 'free', switch = 'both')+
    facet_grid(factor(rg, levels=race_gender_order, labels= str_replace_all(race_gender_order,'_','\n'))~carnegie_tag, scales = 'free', switch = 'both')+
    # facet_grid(rg~carnegie_tag, scales = 'free', switch = 'both')+
    # facet_grid(factor(rg, levels = race_gender_order)~carnegie_tag, scales = 'free', switch = 'both')+
    theme_minimal()+
    theme(text = element_text(size=18),
          legend.position = 'bottom',
          strip.placement = 'outside',
          legend.text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          panel.border = element_rect(color='grey90', fill = NA ),
          plot.title = element_text(hjust = 0.5),
          strip.text.y = element_text(angle = 0),
          legend.spacing.y = unit(0, "mm"),
          panel.spacing.x = unit(4, "mm"))
  
  plt
}

scatterplot_labels
fig1 <- scatterplot_labels()
ggsave(filename =  'figures/Fig1.svg',plot = fig1 ,width = 10,height = 7, dpi = 400,device = 'svg')
# ggsave(filename =  'figures/Fig1.png',plot = ,width = 10,height = 7, dpi = 400)

# Figure 2 ----------------------------------------------------------------
#heatmap of correlations

heatmap_plot <- function(data = 'socsci',
                         institutions= c("top_10","top_100","not_top"),
                         mt=-2){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  
  mean_citations <- retrieve_table(result = 'citations', set = data) %>%
    left_join(retrieve_table('marginal_by_topic',set=data) %>%  select(topic, group, gender, marginal_by_topic=prob)) %>% 
    group_by(topic) %>% 
    summarise(mean_citations = sum(mean*marginal_by_topic))
  
  prob_carnegie <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
    filter(carnegie_tag%in% institutions) %>%
    select(-carnegie_group, group=carnegie_tag)
  
  correlations <- df %>%
    mutate(group = paste(group,gender, sep = '_' )) %>% 
    select(topic, group, prob) %>% 
    bind_rows(prob_carnegie) %>% 
    pivot_wider(id_cols = topic,names_from = group, values_from = prob) %>% 
    arrange(as.numeric(topic)) %>% 
    select(-topic) %>% 
    cor(method = 'spearman')
  # cor(method = 'pearson')
  
  institutions_labels <- str_to_title(str_replace_all(str_remove(institutions,'avg_citations_'),'_', ' '))
  
  institutions_labels <- case_when(institutions_labels=='Hbcu'~ 'HBCU',
                                   institutions_labels=='Hsi'~ 'HSI',
                                   institutions_labels=='Womens'~ 'WC',
                                   institutions_labels=='Mid'~ 'Medium',
                                   institutions_labels=='More Selective'~ 'More\nSelective',
                                   TRUE~institutions_labels)
  
  correlations_df <- 
    correlations[race_gender_order,institutions] %>% 
    as_tibble(rownames = 'rg') %>% 
    pivot_longer(-rg, names_to = 'carnegie_tag', values_to = 'correlation') %>% 
    mutate(group = factor(rg, levels = race_gender_order,label=groups_label),
           carnegie_tag = factor(carnegie_tag, levels = institutions, 
                                 labels =  institutions_labels),
           lab = round(correlation, digits = 2)) %>% 
    select(-rg)
  
  plt <- ggplot(correlations_df, aes(group, carnegie_tag, fill=correlation, label=lab))+
    geom_tile()+
    geom_text(size=6)+
    scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 9, type = "continuous"),
                         breaks = c(min(correlations_df$correlation),max(correlations_df$correlation)),
                         trans = modulus_trans(mt),
                         labels = c('very different', 'very similar'))+
    theme_minimal()+
    scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 10, type = "continuous"),
                          breaks=breaks_pretty(3)) +
    theme(text = element_text(size=18),
          legend.position = 'bottom',
          legend.text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          plot.title = element_text(hjust = 0.5),
          strip.text.y = element_text(angle = 0),
          panel.spacing.x = unit(4, "mm"))+
    labs(x='',y='', fill='')
  
  plt
}


fig2 <- heatmap_plot(institutions= c("not_top","top_100","top_10"),data = 'socsci')
ggsave(filename =  'figures/Fig2.png',plot = fig2,width = 12,height = 6, dpi = 400)


scatterplot_2 <- function(data = 'socsci'){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  
  mean_citations <- retrieve_table(result = 'citations', set = data) %>%
    left_join(retrieve_table('marginal_by_topic',set=data) %>%  select(topic, group, gender, marginal_by_topic=prob)) %>% 
    group_by(topic) %>% 
    summarise(mean_citations = sum(mean*marginal_by_topic))
  
  hh_topic_prop_df <- hh_topic_prop %>% 
    pivot_longer(cols = -c('carnegie_name'), names_to = 'topic', names_prefix = 'topic_', values_to='prob') %>% 
    mutate(prob = replace_na(prob, 0)) %>% 
    rename(group=carnegie_name)
  
  prob_carnegie <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
    filter(carnegie_group%in% c( "hbcu","hsi","womens","selindex","usnr_rank_cat","avg_citations_Q")) %>%
    select(-carnegie_group, group=carnegie_tag) %>% 
    bind_rows(hh_topic_prop_df)
  
  correlations <- df %>%
    mutate(group = paste(group,gender, sep = '_' )) %>% 
    select(topic, group, prob) %>% 
    bind_rows(prob_carnegie) %>% 
    pivot_wider(id_cols = topic,names_from = group, values_from = prob) %>% 
    arrange(as.numeric(topic)) %>% 
    select(-topic) %>% 
    cor(method = 'spearman')
  
  group_labs <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
    select(-topic, -prob) %>% unique()
  
  correlations_df <- as_tibble(correlations) %>% 
    mutate(group=rownames(correlations)) %>% 
    filter(group %in% c("White_Men", 'Black_Women')) %>% 
    pivot_longer(cols = -group, names_to = 'carnegie_tag') %>% 
    pivot_wider(names_from = group) %>% 
    left_join(group_labs, by = "carnegie_tag") %>% 
    filter(!carnegie_tag %in% c("2","not_indexed")) %>% 
    mutate(carnegie_tag = case_when(carnegie_tag=="Womens" ~"WC",
                                    carnegie_tag=="not womens" ~"not WC",
                                    carnegie_tag=="more_selective" ~"more selective",
                                    carnegie_tag=="top_10" ~"USNWR: 10",
                                    carnegie_tag=="top_100" ~"USNWR: 100",
                                    carnegie_tag=="not_top" ~"USNWR: not top",
                                    carnegie_tag=="avg_citations_low" ~"citations: low",
                                    carnegie_tag=="avg_citations_mid" ~"citations: mid",
                                    carnegie_tag=="avg_citations_high" ~"citations: high",
                                    TRUE ~ carnegie_tag),
           carnegie_tag = str_replace_all(carnegie_tag,'_',' '),
           carnegie_group = case_when(str_detect(carnegie_tag, 'University') ~'HH',
                                      TRUE ~carnegie_group))
  
  cp <- wesanderson::wes_palette("Zissou1", 6, type = "continuous")[1:6]
  ggplot(correlations_df, aes(Black_Women, White_Men, label=carnegie_tag, color = carnegie_group))+
    geom_point()+
    geom_text_repel(size =6) +
    theme_minimal()+
    scale_color_manual(values = 
                         c(cp[1:2],'Black',cp[3:6]))+
    theme(text = element_text(size=18),
          legend.position = 'none',
          legend.text = element_text(size=14))+
    lims(x= c(-1,1),y=c(-1,1))+
    geom_hline(yintercept = 0, color='grey70')+
    geom_vline(xintercept = 0, color='grey70')+
    labs(x='Black Women',y='White Men', fill='')
  
}

fig3 <- scatterplot_2()

ggsave(filename =  'figures/Fig3.png',plot = fig3,width = 10,height = 6, dpi = 400)

#### Figure 4 ----------------------------------------------------------------

model_plot_field_topics_norm <- function(inst_group='usnr_rank_cat'){
  
  vars <- c("usnr [top 10]","usnr [top 100]","#authors", "career age", #"year",
            "Black men","Latinx men", "Asian men", "Asian women","Latinx women","Black women","White women")
  #"usnr_rank_cat"   "selindex"        "avg_citations_Q" "avg_citations" 
  
  df <- agg_ols %>%
    filter(institution_cov %in% inst_group) %>% 
    mutate(type = case_when(str_detect(dep,'norm')~'Topic normalized',
                            str_detect(dep,'diff')~'difference',
                            TRUE ~'Field normalized'),
           dep_type = case_when(str_detect(dep,'cit')~'Citations',
                           str_detect(dep,'FI')~'JIF'),
           type = factor(type, levels = c('Field normalized', 'Topic normalized','difference')),
           variable = factor(variable,levels = vars)
           # institution_cov= glue("{institution_cov}\n({n})" )
    ) %>% 
    filter(!is.na(variable), type!='difference')
  
  # plt <- 
    df %>% 
    ggplot(aes(coeff,variable, color=type))+
    facet_grid(.~dep_type) +
    geom_vline(xintercept = 0)+
    geom_pointrange(shape=21, size=.5, aes(xmin= conf_lower, xmax=conf_higher))+
    geom_point(data = df %>% filter(type=='difference'))+
    geom_vline(xintercept = 0)+
    scale_x_continuous(n.breaks = 6)+
    labs(x='coeff',y= 'covariables', color= "")+ #glue('n={n}')
    scale_color_manual(values = wesanderson::wes_palette("Zissou1", 5, type = "discrete")[c(1,3,2,5)])+
    theme_minimal()+
    theme(legend.position = 'bottom',
          text = element_text(size=16),
          axis.text = element_text(size=14),
          panel.background = element_rect(color = 'grey80'),
          strip.text.y.left = element_text(angle=0, size = 10),
          # plot.margin = margin(-10,0,0,0),
          axis.ticks.y = element_blank())
  
}


fig4 <- model_plot_field_topics_norm() 

# fig4 <- ggarrange(plt1,plt2, ncol = 1,nrow = 2, heights = c(1,1.015),legend = 'bottom',common.legend=TRUE)

ggsave('figures/Fig4.png',plot = fig4,width = 12,height = 6, dpi = 400)

split_model_plot <- function(inst_group='usnr_rank_cat'){
  
  vars <- c("nb_auteur","career_age","black_M","hispanic_M","asian_M","asian_F","hispanic_F","black_F",'white_F')
  labels <- c("#authors", "career age","Black\nmen","Latinx\nmen","Asian\nmen","Asian\nwomen","Latinx\nwomen",
              "Black\nwomen","White\nwomen")
  
  df <- ols_subsets %>% 
    filter(university_group%in% inst_group) %>% 
    mutate( dep_type = case_when(str_detect(dep,'cit')~'Citations',
                                 str_detect(dep,'FI')~'JIF'),
           institutions_labels =  str_to_title(str_replace_all(subset,'_', ' ')),
           institutions_labels = case_when(institutions_labels=='Mid'~ 'Medium',
                                           institutions_labels=='Hbcu'~ 'HBCU',
                                           institutions_labels=='Hsi'~ 'HSI',
                                           institutions_labels=='Womens'~ 'WC',
                                           TRUE~institutions_labels),
           # institutions_labels = glue("{institutions_labels}\n({n})"),
           variable = factor(variable,levels = vars,labels = labels)) %>% 
    filter(!is.na(variable))
  
  if (inst_group=='usnr_rank_cat') {
    df <- df %>% 
      mutate(institutions_labels = factor(institutions_labels, 
                                          levels=c('Top 10','Top 100','Not Top')))
  }
  
  plt <- df %>% 
    ggplot(aes(coeff,variable, color=institutions_labels,xmin= conf_lower, xmax=conf_higher ))+
    facet_grid(.~dep_type,scales = 'free') +
    geom_vline(xintercept = 0)+
    geom_pointrange(shape=21, size=.5)+
    scale_color_manual(values = wesanderson::wes_palette("Zissou1", 5, type = "discrete")[c(1,3,5)])+
    labs(x='coeff',y= 'covariables', color= '')+
    # guides(color=guide_legend(nrow=2, byrow=TRUE))+
    theme_minimal()+
    # lims(x=lims)+
    theme(text = element_text(size=16),
          # axis.text = element_text(size=16),
          # strip.text.y.left = element_text(angle=0, size = 16),
          legend.position = 'bottom',strip.background = element_blank(),
          strip.text.y.left = element_text(angle=0),
          # axis.text.y = element_blank(),
          # axis.ticks.y = element_blank(),
          axis.title.y = element_text(size=16),
          plot.title = element_text(size=16,hjust = 0.5))
  plt
}

split_model_plot()

ggsave(filename =  'figures/Fig5.png',width = 12,height = 6, dpi = 400, device = 'png')
