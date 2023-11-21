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

fs::file_copy('../rg_app/data.Rdata','data/shiny_data.Rdata',overwrite = T)

load('data/shiny_data.Rdata')


##this is not used in the app, so I only load it here
institutions_marginal_by_topic_socsci <- read_csv('../rg_institutions/results/institutions_topics/marginal_by_topic_gender_socsci.csv')
institutions_marginal_by_topic_health <- read_csv('../rg_institutions/results/institutions_topics/marginal_by_topic_gender_health.csv')

clean_marginal <- function(df, type='socsci'){
  df %>%
    pivot_df_rg(.,name='prob') %>%
    left_join(top_words_socsci, by='topic') %>%
    left_join(retrieve_tp(type), by='topic') %>%
    rename(topic_proportion = proportion) %>%
    refactor_citation_quartiles(.)
}

career_age <- read_csv('data/age_dist.csv') %>% #this file comes from the original github_repo
  pivot_longer(-age, names_to = 'group', values_to = 'p') %>%
  mutate(group = factor(group, levels = groups, labels = groups_label))


institutions_marginal_by_topic_socsci <- institutions_marginal_by_topic_socsci %>%
  clean_marginal('socsci')
institutions_marginal_by_topic_health <- institutions_marginal_by_topic_health %>%
  clean_marginal('health')

race_groups <- c('Black','Latinx','White','Asian')


#for reference
read_csv('results/institutions/rg_carnegie.csv')$avg_citations_Q %>% unique()


#colors convention

colors_pair <- RColorBrewer::brewer.pal(8, 'Paired')#[c(1,3,5,7,8,6,4,2)]
two_colors <- wesanderson::wes_palette("Zissou1", 5, type = "discrete")[c(5,1)]
four_colors_d <- wesanderson::wes_palette("Darjeeling1", 5, type = "discrete")[c(1,2,4,5)]


three_colors_c <- wesanderson::wes_palette("Zissou1", 3, type = "continuous")
three_colors_d <- wesanderson::wes_palette("Zissou1", 5, type = "discrete")[c(1,3,5)]
full_colors_c <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")

race_gender_order <- c("White_Men","Black_Men","Latinx_Men","Asian_Men","Asian_Women","Latinx_Women","Black_Women","White_Women")

topic_labels <- read_csv('handcoding/topic_labels_socsci.csv')
topic_labels_hh <- read_csv('handcoding/top_words_socsci_hh.csv')

# Figure S1 career age ----------------------------------------------------

heatmap_age <- function(){
  
  df <- career_age %>% 
    filter(age<=40)
  
  plt <- ggplot(df,aes(group,age,fill=p))+
    # geom_col(position = 'dodge')
    geom_tile(color='grey30')+
    # scale_fill_binned(palette = ggplot2:::binned_pal(scales::manual_pal(values = pal)))+
    scale_fill_gradientn(colours = three_colors_c,
                         breaks = c(min(df$p),max(df$p)),
                         # trans = modulus_trans(2),
                         labels = c('Under\nrepresentation', 'Over\nrepresentation'))+
    guides( fill = guide_colorbar(title = '',barwidth = unit(150, units = 'pt')))+
    # geom_text(size =6, aes(label=percent(value,accuracy = 0.1))) +
    theme_minimal()+
    # scale_y_reverse()+
    # scale_x_discrete(position = "top") +
    labs(x ='', y='Career Age')+
    theme(text = element_text(size=18,color='grey15', face='bold'),
          legend.position = 'bottom',
          # axis.text = element_text(size=16,color = c('grey15', 'grey45'), face = c('plain','bold')),
          panel.grid.major.y = element_blank()
          # panel.border = element_rect(color = "grey30", fill = NA, size = 1)
    )
  
  plt
}

heatmap_age()

# ggsave('figures/Fig_S1.svg', width = 12,height = 6, dpi = 400, device = 'svg')
ggsave('figures/Fig_S1.png', width = 12,height = 6, dpi = 400, device = 'png')


# collaborations ----------------------------------------------------------

heatmap_collaborations <- function(data_type = 'first_last',value_type = 'diff', control_by_speciality = TRUE){
  
  df <- collaborations %>% 
    filter(d_type==data_type, 
           v_type==value_type, 
           controlled == control_by_speciality)
  
  if (data_type == 'all' & value_type == 'diff') {
    norm_exp = -2
  }else{norm_exp = 1}
  
  
  g <-
    ggplot(df, aes(to, from, fill=value)) +
    geom_tile(color='grey30')+
    scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 9, type = "continuous"),
                         breaks = c(min(df$value),max(df$value)),
                         trans = modulus_trans(norm_exp),
                         labels = c('Under\nrepresentation', 'Over\nrepresentation'))+
    guides( fill = guide_colorbar(title = '',barwidth = unit(150, units = 'pt')))+
    # geom_text(size =6, aes(label=percent(value,accuracy = 0.1))) +
    geom_text( aes(label=percent(value,accuracy = 0.1))) +
    theme_minimal()+
    scale_x_discrete(position = "top") +
    theme(text = element_text(size=18,color='grey15', face='bold'),
          legend.position = 'bottom',
          axis.text = element_text(color = c('grey15', 'grey45'), face = c('plain','bold')),
          # axis.text = element_text(size=16,color = c('grey15', 'grey45'), face = c('plain','bold')),
          panel.grid.major.y = element_blank()
          # panel.border = element_rect(color = "grey30", fill = NA, size = 1)
    )
  
  ngroups <- 8
  if (data_type == 'first_last') {
    boxdata <- tibble(xmin = c(ngroups-2.5, 3.5, 0.5),
                      xmax = c(ngroups + 0.5, 5.5, 3.5),
                      ymin = c(0.5, 3.5, ngroups-2.5),
                      ymax = c(3.5, 5.5, ngroups +0.5))
  }
  if (data_type == 'all') {
    boxdata <- tibble(xmin = c(ngroups-3.5, 0.5),
                      xmax = c(ngroups + 0.5, 3.5),
                      ymin = c(0.5, ngroups-2.5),
                      ymax = c(4.5, ngroups +0.5))
  }
  
  if (value_type =='diff') {
    g <- g +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), data = boxdata, inherit.aes = FALSE,
                fill = "transparent", color = "black", size = 1.5)
  }
  
  if (data_type == 'first_last') {
    
    g <- g +
      labs(fill = '', x= 'Last author', y = 'First author')
  }
  if (data_type == 'all') {
    g <- g +
      labs(fill = '', x= 'All co-authors', y ='All co-authors')
  }
  
  g  
  
}


colab_first_last <- heatmap_collaborations()
colab_all <- heatmap_collaborations(data_type = 'all')

figS2 <- ggarrange(colab_first_last,colab_all, common.legend = TRUE,
                   legend = 'bottom')

ggsave('figures/Fig_S2.png',plot = figS2, width = 14,height = 6, dpi = 400, device = 'png')


# figure S3, correlation between prestige metrics -------------------------

institutions_prestige_metrics <- read_csv("data/institutions_prestige_metrics.csv")


institutions_prestige_metrics <- institutions_prestige_metrics %>% 
  select(-c(carnegie_id,carnegie_name)) %>%
  mutate(avg_citations_Q = factor(avg_citations_Q, level=c( "(0.1, 1.47)", "(1.48, 1.74)","(1.77, 4.07)") ),
         usnr_rank = case_when(usnr_rank==999~NaN,
                               TRUE ~usnr_rank),
         selindex = factor(selindex, levels=c("more_selective","selective","inclusive")),
         usnr_rank_cat = factor(usnr_rank_cat,levels=c('top_10','top_100','not_top'))) %>% 
  filter(!is.na(selindex))
#usnr_rank = factor(usnr_rank, ordered = TRUE))

ggally_density <- function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(mapping = aes_string(fill="selindex"), alpha = 0.5, color=NA)
}

institutions_prestige_metrics %>% 
  # filter(!is.na(usnr_rank), selindex!='not_indexed') %>% 
  # filter(selindex!='not_indexed') %>% 
  select(-usnr_rank,-avg_citations_Q) %>%
  ggpairs(mapping=aes(fill=selindex),
          diag = list(continuous =ggally_density),
          upper = list(continuous = wrap("density", alpha = 0.5)),
          # lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4))
          lower = list(discrete = wrap('facetbar'),combo='facethist'),
          legend = 4
  )+
  theme_minimal()+
  scale_fill_manual(values = three_colors_d)+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size=14),
        strip.text =  element_text(size=14))

# ggsave('figures/Fig_S3.svg', width = 10,height = 10, dpi = 400, device = 'svg')
ggsave('figures/Fig_S3.png', width = 10,height = 10, dpi = 400, device = 'png')

#S4-S6
# relative over/underrepresentation and proportion by race and gender  and absolute proportion

plot_1_base <- function(df,metric){
  
  plt <- df %>% 
    ggplot(aes(Annee_Bibliographique,value, color = race, linetype=gender))+
    geom_line()+
    geom_point()+
    scale_color_manual(values = four_colors_d)+
    # scale_color_manual(values = colors_pair)+
    theme_minimal()+
    theme(legend.position = 'bottom',
          text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          plot.title = element_text(hjust = 0.5))+
    guides(color = guide_legend(title = '',reverse=TRUE,keywidth = unit(1,'cm'), override.aes = list(size=4)),
           linetype=guide_legend(title = ''))+
    scale_x_continuous(breaks = seq(2008,2019,5))+
    labs(x='',y= '')
  
  if (metric=='rel_diff') {
    plt <- plt + 
      geom_hline(yintercept = 0)
  }
  plt
}

representation_plot <- function(metric='rel_diff'){
  
  #metric:proportion,rel_diff,disp_control, rel_census, rel_residents
  #category:r1,control,obereg,hbcu, hsi, msi, womens, selindex,usnr_rank_cat, flags
  
  if (metric=='rel_diff') {
    df <- carnegie_institutions_rel_diff %>% 
      select(Annee_Bibliographique,hbcu,hsi,womens,selindex,usnr_rank_cat,avg_citations_Q,
             group,value,n)
    ylims=c(-0.17,.25)
    ylab='Relative representation'
  }
  if (metric =='proportion') {
    df <- carnegie_institutions_prop %>% 
      select(Annee_Bibliographique,hbcu,hsi,womens,selindex,usnr_rank_cat,avg_citations_Q,
             group,value, n=inst_types_n)
    ylims=c(0,0.46)
    ylab='Proportion'
  }
  
  df <- df %>% 
    mutate(id = row_number(),
           gender = str_extract(group,'(Men|Women)'),
           race = str_extract(group,paste0(race_groups,collapse = '|'))) %>% 
    pivot_longer(hbcu:avg_citations_Q, names_to = 'categorization',values_to = 'category') %>% 
    group_by(Annee_Bibliographique,group,race,gender,categorization,category) %>% 
    summarise(value = weighted.mean(value,n),
              n = sum(n)) %>% 
    filter(!category %in% c('not HBCU','not HSI','not womens','not_indexed' ))
  
  diversity_n <- df %>% 
    filter(category %in% c('HBCU','HSI','Womens')) %>% 
    mutate(category = replace(category, category=='Womens', 'WC')) %>% 
    ungroup() %>% 
    select(-group, -value) %>% 
    distinct() %>% 
    group_by(category) %>% 
    summarise(total_n = sum(n),
              label= glue::glue('{category} ({number(total_n)} authors)'))
  
  rank_n <- df %>% 
    filter(categorization%in%c('avg_citations_Q','selindex','usnr_rank_cat')) %>% 
    ungroup() %>% 
    select(Annee_Bibliographique,category, n) %>% 
    distinct() %>% 
    group_by(category) %>% 
    summarise(total_n = sum(n)) 
  
  plt1 <- df %>% 
    filter(category %in% c('HBCU','HSI','Womens')) %>% 
    mutate(category = replace(category, category=='Womens', 'WC')) %>% 
    left_join(diversity_n, by = "category") %>% 
    plot_1_base(.,metric)+
    facet_wrap(label~., scales = 'free')
  # theme(legend.position = 'none')
  
  plt2_a <- df %>% 
    filter(categorization=='avg_citations_Q') %>% 
    left_join(rank_n, by = "category") %>% 
    mutate(category = str_to_title(str_remove(category,'avg_citations_')),
           category = replace(category, category=='Mid', 'Medium'),
           order =  case_when(str_detect(category,'High') ~1,
                              str_detect(category,'Medium') ~2,
                              str_detect(category,'Low') ~3),
           category = glue::glue('{category} ({number(total_n)} authors)'),
           category = factor(category),
           category = fct_reorder(category,order)) %>% 
    plot_1_base(.,metric)+
    labs(y=ylab,title ='Average citations rank')+
    scale_y_continuous(limits = ylims)+
    facet_wrap(category~., ncol = 1)
  
  plt2_b <- df %>% 
    filter(categorization=='selindex') %>% 
    left_join(rank_n, by = "category") %>% 
    mutate(category = str_to_title(str_replace_all(category,'_',' ')),
           order =  case_when(str_detect(category,'More Selective') ~1,
                              str_detect(category,'Selective') ~2,
                              str_detect(category,'Inclusive') ~3),
           category = glue::glue('{category} ({number(total_n)} authors)'),
           category = factor(category),
           category = fct_reorder(category,order)) %>% 
    plot_1_base(.,metric)+
    labs(title = 'Carnegie selectivity index')+
    scale_y_continuous(limits = ylims)+
    facet_wrap(category~., ncol = 1)
  # theme(axis.text.y = element_blank())
  
  plt2_c <- df %>% 
    filter(categorization=='usnr_rank_cat') %>% 
    left_join(rank_n, by = "category") %>% 
    mutate(category = str_to_title(str_replace_all(category,'_',' ')),
           order =  case_when(str_detect(category,'Top 10') ~1,
                              str_detect(category,'Top 100') ~2,
                              str_detect(category,'Not Top') ~3),
           category = glue::glue('{category} ({number(total_n)} authors)'),
           category = factor(category),
           category = fct_reorder(category,order))%>% 
    plot_1_base(.,metric)+
    labs(title = 'US newsreport rank')+
    scale_y_continuous(limits = ylims)+
    facet_wrap(category~., ncol = 1)
  
  prestige_fig <- ggarrange(plt2_a,plt2_b,plt2_c, nrow = 1,legend = 'none')
  
  # complete_fig <- 
  ggarrange(plt1,prestige_fig,common.legend = TRUE,
            legend.grob =  get_legend(plt1),
            nrow = 2, heights = c(1.25,3),
            legend = "bottom")
}

figS4 <- representation_plot('proportion')
# ggsave(filename =  'figures/Fig_S6.svg',plot = figS6,width = 10,height = 8, dpi = 400, device = 'svg')
ggsave(filename =  'figures/Fig_S4.png',plot = figS4,width = 10,height = 8, dpi = 400, device = 'png')


figS5 <- representation_plot(metric = 'rel_diff')

# ggsave(filename =  'figures/Fig_S5.svg',plot = figS5,width = 10,height = 6, dpi = 400, device = 'svg')
ggsave(filename =  'figures/Fig_S5.png',plot = figS5,width = 10,height = 8, dpi = 400)


#S6 Migration

figS6 <- sed %>% 
  mutate(rg_group = paste(group, gender, sep='\n')) %>% 
  ggplot(aes(rg_group,p, fill = citizenship,label=percent(p)))+
  geom_col(position = position_stack())+
  geom_text( position = position_stack(vjust = 0.5))+
  geom_hline(yintercept = .5,linetype= 'dashed',color='gray50')+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = alpha(two_colors,alpha = 0.9))+
  labs(x='',y='proportion of doctoral graduates')+
  theme_minimal()+
  theme(legend.position = 'bottom')

ggsave(filename =  'figures/Fig_S6.png',plot = figS5,width = 10,height = 6, dpi = 400, device = 'png')


#topics scatterplot Howard Harvard

scatterplot_labels_hh <- function(grp=c('Black_Women', 'White_Men'), data = 'socsci'){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  hh_topic_prop_df <- hh_topic_prop %>% 
    pivot_longer(cols = -c('carnegie_name'), names_to = 'topic', names_prefix = 'topic_', values_to='prob') %>% 
    mutate(prob = replace_na(prob, 0)) %>% 
    rename(group=carnegie_name,prob_hh=prob)
  
  mean_citations <- retrieve_table(result = 'citations', set = data) %>%
    left_join(retrieve_table('marginal_by_topic',set=data) %>%  select(topic, group, gender, marginal_by_topic=prob)) %>% 
    group_by(topic) %>% 
    summarise(mean_citations = sum(mean*marginal_by_topic))
  
  mean_prob_hh <- left_join(hh_topic_prop_df,df2 %>% 
                              select(topic, topic_proportion) %>% 
                              distinct(), by = "topic") %>% 
    group_by(group) %>% 
    summarise(prob = weighted.mean(prob_hh,topic_proportion))
  
  group_average <- df2 %>%
    mutate(rg = paste(group,gender, sep = '_' )) %>% 
    filter(rg%in%grp) %>% 
    # mutate(rg = str_replace_all(rg,'_','\n')) %>% 
    group_by(rg) %>% 
    summarise(prob = sum(prob))
  
  df_labels <- topic_labels_hh %>% 
    mutate(topic= factor(topic)) %>% 
    select(-top_words)
  
  df <- df %>%
    mutate(rg = paste(group,gender, sep = '_' )) %>% 
    filter(rg%in%grp) %>%
    group_by(rg,topic,topic_proportion) %>%
    summarise(prob = sum(prob),
              top_words = unique(top_words)) %>% 
    left_join(mean_citations, by='topic') %>% 
    left_join(hh_topic_prop_df, by='topic') %>%
    left_join(df_labels, by = "topic") #only works for social sciences!
  
  plt <- df %>% 
    ggplot(., aes(prob_hh,prob,size= topic_proportion,#atan(prob/feminisation),
                  group=NA))+
    geom_point(aes(color=mean_citations))+
    geom_smooth(method = 'lm', se = TRUE, show.legend = FALSE)+
    # geom_smooth(method = 'lm', se = FALSE, show.legend = FALSE)+
    geom_vline(data = mean_prob_hh, aes(xintercept = prob ), color='grey70')+
    geom_hline(data = group_average, aes(yintercept = prob ), color='grey70')+
    geom_text_repel(data = df %>% filter(!is.na(label)), aes(label=label))+
    labs(color= 'Citations',x='', y = '', size = 'Topic size')+
    scale_color_gradientn(colours = full_colors_c,breaks=breaks_pretty(3)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
    scale_x_continuous(labels = percent, n.breaks = 4.1)+
    stat_cor(aes(label = ..r.label..),cor.coef.name="ρ",digits=2,output.type = 'text',show.legend  = FALSE, method='spearman')+
    facet_grid(factor(rg, levels=race_gender_order, labels= str_replace_all(race_gender_order,'_','\n'))~group, scales = 'free', switch = 'both')+
    theme_minimal()+
    theme(text = element_text(size=18),
          legend.position = 'bottom',
          strip.placement = 'outside',
          legend.text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          panel.border = element_rect(color='grey90', fill = NA ),
          plot.title = element_text(hjust = 0.5),
          strip.text.y = element_text(angle = 0)
          # legend.margin = margin(0, 0, 0, 0),
          # plot.margin  = margin(0, 0, 0, 0),
          # legend.spacing.y = unit(0, "mm"),
          # panel.spacing.x = unit(4, "mm"))
    )
  plt
}


scatterplot_labels_hh() 
ggsave(filename =  'figures/Fig_S7.png',width = 10,height = 7, dpi = 400)

scatterplot <- function(grp=c('Black_Women', 'White_Men'),
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
  
  df %>%
    mutate(rg = paste(group,gender, sep = '_' )) %>% 
    filter(rg%in%grp) %>%
    group_by(rg,topic,topic_proportion) %>%
    # group_by(group,gender,topic,topic_proportion) %>% 
    summarise(prob = sum(prob),
              top_words = unique(top_words)) %>% 
    left_join(mean_citations, by='topic') %>% 
    left_join(prob_carnegie, by='topic') %>% 
    # mutate(rg = factor(rg, levels=race_gender_order, labels= str_replace_all(race_gender_order,'_','\n'))) %>%
    ggplot(., aes(prob_carnegie,prob,size= topic_proportion,#atan(prob/feminisation),
                  group=NA))+
    geom_point(aes(color=mean_citations))+
    # geom_smooth(method = 'lm', se = FALSE, show.legend = FALSE)+
    geom_smooth(method = 'lm', se = TRUE, show.legend = FALSE)+
    # geom_smooth(method = 'lm', se = FALSE, show.legend = FALSE)+
    geom_vline(data = mean_prob_carnegie, aes(xintercept = prob ), color='grey70')+
    geom_hline(data = group_average, aes(yintercept = prob ), color='grey70')+
    labs(color= 'Citations',x='', y = '', size = 'Topic size')+
    scale_color_gradientn(colours =full_colors_c,breaks=breaks_pretty(3)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
    scale_x_continuous(labels = percent, n.breaks = 4.1)+
    # stat_cor(aes(label = ..r.label..),output.type	 = 'text',show.legend  = FALSE, method='spearman')+
    stat_cor(cor.coef.name="ρ",digits=2,output.type = 'text',show.legend  = FALSE, method='spearman', label.y.npc='top')+
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
          strip.text.y = element_text(angle = 0))
  # legend.margin = margin(0, 0, 0, 0),
  # plot.margin  = margin(0, 0, 0, 0),
  # legend.spacing.y = unit(0, "mm"),
  # panel.spacing.x = unit(4, "mm"))
}
# Figure S8-9 ----------------------------------------------------------------
# Scatterplot, relation between institutions and race & gender, by the proportion each represents on topics composition

scatterplot(grp=race_gender_order, data = 'socsci') +
  theme(text = element_text(size=12))

ggsave('figures/Fig_S8.png',width = 10,height = 10, dpi = 400, device = 'png')

scatterplot(grp=race_gender_order, data = 'health') +
  theme(text = element_text(size=12))

ggsave('figures/Fig_S9_health.png',width = 10,height = 10, dpi = 400, device = 'png')


# Figure 2 ----------------------------------------------------------------
#heatmap of correlations

heatmap_plot <- function(data = 'socsci',
                         institutions= c("top_10","top_100","not_top"),
                         mt=-2){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  
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
    geom_text()+
    scale_fill_gradientn(colours = full_colors_c,
                         breaks = c(min(correlations_df$correlation),max(correlations_df$correlation)),
                         trans = modulus_trans(mt),
                         labels = c('very different', 'very similar'))+
    theme_minimal()+
    scale_color_gradientn(colours = full_colors_c,
                          breaks=breaks_pretty(3)) +
    theme(text = element_text(size=14),
          legend.position = 'bottom',
          legend.text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          plot.title = element_text(hjust = 0.5),
          strip.text.y = element_text(angle = 0),
          panel.spacing.x = unit(4, "mm"))+
    labs(x='',y='', fill='')
  
  plt
}


figure_S10 <- function(d='socsci'){
  
  plt1 <- heatmap_plot(institutions= c("HBCU","Womens","HSI"),data = d)
  
  plt2 <- heatmap_plot(institutions= c("avg_citations_low","avg_citations_mid","avg_citations_high"),
                       data = d)
  plt3 <- heatmap_plot(institutions= c("inclusive","selective","more_selective"),data = d)
  
  
  ggarrange(plt1,plt2,plt3, ncol = 1,nrow = 3,common.legend = TRUE, legend = "bottom", labels = c('A.','B.','C.','D.'))
  
}

figS10 <- figure_S10('socsci')
ggsave(filename =  'figures/Fig_S10.png',plot = figS10,width = 12,height = 10, dpi = 400, device = 'png')
# ggsave(filename =  'figures/Fig2.svg',plot = fig2,width = 12,height = 7, dpi = 400, device = 'svg')

figure_S12 <- function(d='health'){
  
  plt1 <- heatmap_plot(institutions= c("HBCU","Womens","HSI"),data = d)
  
  plt2 <- heatmap_plot(institutions= c("not_top","top_100","top_10"),data = d)
  
  plt3 <- heatmap_plot(institutions= c("avg_citations_low","avg_citations_mid","avg_citations_high"),
                       data = d)
  
  plt4 <- heatmap_plot(institutions= c("inclusive","selective","more_selective"),data = d)
  
  
  ggarrange(plt1,plt2,plt3,plt4, ncol = 2,nrow = 2,common.legend = TRUE, legend = "bottom", labels = c('A.','B.','C.','D.'))
  
}

figS12 <- figure_S12('health')
ggsave(filename =  'figures/fig_S12_health.png',plot = figS12,width = 12,height = 7, dpi = 400, device = 'png')



# heatmap, control size effect S11--------------------------------------------

#only for USNR
heatmap_plot_size_control <- function(data = 'socsci',mt=-2){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  
  institutions <- rev(c("top_10","top_100","not_top"))
  
  prob_carnegie <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
    filter(carnegie_tag%in% institutions) %>%
    select(-carnegie_group, group=carnegie_tag)
  
  prop_rg_institutions <- carnegie_institutions_prop %>% 
    select(Annee_Bibliographique,usnr_rank_cat,
           group,value,inst_types_n, group_n) %>% 
    group_by(usnr_rank_cat,group) %>% 
    summarise(n=sum(group_n)) %>% 
    group_by(usnr_rank_cat) %>% 
    mutate(p=n/sum(n),
           gender = str_extract(group,'Men|Women'),
           group= str_extract(group,paste0(race_groups,collapse = '|')))
  
  expected_prob_carnegie <- 
    prop_rg_institutions %>% 
    left_join(df) %>% 
    group_by(usnr_rank_cat,topic) %>% 
    summarise(n_expected = sum(n*prob)) %>% 
    group_by(topic) %>% 
    mutate(p_expected = n_expected/sum(n_expected))
  
  expected_prob_carnegie %>% 
    group_by(topic) %>% 
    summarise(sum(p_expected))
  
  corrected_prob_carnegie <- expected_prob_carnegie %>% 
    left_join(prob_carnegie, join_by(topic==topic, usnr_rank_cat==group)) %>% 
    mutate(prob=prob/p_expected) %>%  #It's named prob to merge it later,
    # but it's really a relative over/under representation, given the 
    #race & gender dist in that institution group
    select(topic, group=usnr_rank_cat,prob)
  
  correlations <- df %>%
    mutate(group = paste(group,gender, sep = '_' )) %>% 
    select(topic, group, prob) %>% 
    bind_rows(corrected_prob_carnegie) %>% 
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
    geom_text()+
    scale_fill_gradientn(colours = full_colors_c,
                         breaks = c(min(correlations_df$correlation),max(correlations_df$correlation)),
                         trans = modulus_trans(mt),
                         labels = c('very different', 'very similar'))+
    theme_minimal()+
    scale_color_gradientn(colours = full_colors_c,
                          breaks=breaks_pretty(3)) +
    theme(text = element_text(size=14),
          legend.position = 'bottom',
          legend.text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          plot.title = element_text(hjust = 0.5),
          strip.text.y = element_text(angle = 0),
          panel.spacing.x = unit(4, "mm"))+
    labs(x='',y='', fill='')
  
  plt
}
figS11 <- heatmap_plot_size_control()
ggsave(filename =  'figures/fig_S11.png',plot = figS11,width = 10,height = 4, dpi = 400, device = 'png')


#deciles S13-S14
fig_S13 <- heatmap_plot(institutions = rev(paste0('D',rev(1:10))),data = 'socsci')

ggsave(filename =  'figures/fig_S13.png',plot = fig_S13,width = 12,height = 7, dpi = 400, device = 'png')

fig_S14_health <- heatmap_plot(institutions = rev(paste0('D',rev(1:10))),data = 'health')

ggsave(filename =  'figures/fig_S14_health.png',plot = fig_S14_health,width = 12,height = 7, dpi = 400, device = 'png')


# correlation wrt to same identity S15-S18----------------------------------------

heatmap_self <- function(institutions= c("HBCU","Womens","HSI"), data = 'socsci', against = 'inst'){ 
  
  institutions_labels <- str_to_title(str_replace_all(str_remove(institutions,'avg_citations_'),'_', ' '))
  
  institutions_labels <- case_when(institutions_labels=='Hbcu'~ 'HBCU',
                                   institutions_labels=='Hsi'~ 'HSI',
                                   institutions_labels=='Womens'~ 'WC',
                                   institutions_labels=='Mid'~ 'Medium',
                                   institutions_labels=='More Selective'~ 'More\nSelective',
                                   TRUE~institutions_labels)
  # how many papers from each topic are published by race&gender&institution group
  
  df_weights <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% # % of papers each inst group produces for each topic
    filter(carnegie_tag %in% institutions) %>%
    select(-carnegie_group, institution_group=carnegie_tag, w=prob)
  
  df <- get(paste0('institutions_marginal_by_topic','_',data)) %>% 
    filter(carnegie_tag %in% institutions) %>% 
    select(-carnegie_group, institution_group=carnegie_tag) %>% 
    mutate(rg = paste(group,gender, sep = '_' ))%>%
    left_join(df_weights, by = c("topic", "institution_group")) %>%
    mutate(prob = prob*w)
  
  # compared to how many papers from each topic are published by {race&gender} OR {institution} group
  
  if (against == 'rg') {
    df_all <- retrieve_table(result = 'marginal_by_topic',set = data) %>% 
      mutate(rg = paste(group,gender, sep = '_' )) %>% 
      # filter(rg%in%grp) %>% 
      select(topic, prob_all = prob, rg)
    df_corr <- df %>% 
      left_join(df_all, by = c("topic", "rg")) %>% 
      select(topic,rg,institution_group,prob,prob_all)
  }
  if (against == 'inst') {
    df_all <-  get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
      filter(carnegie_tag%in% institutions) %>%
      select(-carnegie_group, prob_all = prob, institution_group=carnegie_tag) %>% 
      arrange(topic)
    
    df_corr <- df %>% 
      left_join(df_all,by = c("topic", "institution_group")) %>% 
      select(topic,rg,institution_group,prob,prob_all)
  }
  
  correlations <- df_corr %>%
    group_by(rg,institution_group) %>% 
    nest() %>% 
    mutate(correlation = map_dbl(data,~cor(.x$prob,.x$prob_all,method='spearman'))) %>% 
    select(-data)
  
  # plotly::ggplotly(df_corr %>%
  #   filter(rg%in%c('White_Men', 'Black_Women')) %>%
  #   ggplot(aes(prob_all,prob, color = topic)) +
  #   geom_point() +
  #   facet_grid(rg~institution_group, scale='free'))
  
  correlations_df <- correlations %>% 
    mutate(group = factor(rg, levels = race_gender_order,label=groups_label),
           institution_group = factor(institution_group, levels = institutions, 
                                      labels =  institutions_labels),
           lab = round(correlation, digits = 2)) %>% 
    select(-rg)
  
  plt <- ggplot(correlations_df, aes(group, institution_group, fill=correlation, label=lab))+
    geom_tile()+
    geom_text()+
    scale_fill_gradientn(colours = full_colors_c,
                         breaks = c(min(correlations_df$correlation),max(correlations_df$correlation)),
                         labels = c('very different', 'very similar'))+
    theme_minimal()+
    scale_color_gradientn(colours = full_colors_c,
                          breaks=breaks_pretty(3)) +
    theme(text = element_text(size=14),
          legend.position = 'bottom',
          legend.text = element_text(size=14),
          plot.background = element_rect(color = "black"),
          plot.title = element_text(hjust = 0.5),
          strip.text.y = element_text(angle = 0),
          panel.spacing.x = unit(4, "mm"))+
    labs(x='',y='', fill='')
  
  plt
  
}

heatmap_self_plot <- function(d='socsci', vs = 'inst'){
  #vs: inst, rg. 
  #inst means the correlation between (race, gender, institution) wrt (institution)
  #rg means the correlation between (race, gender, institution) wrt (rg)
  
  plt1 <- heatmap_self(institutions= c("HBCU","Womens","HSI"), data = d, against = vs)
  plt2 <- heatmap_self(institutions= c("not_top","top_100","top_10"), data = d, against = vs)
  plt3 <- heatmap_self(institutions= c("avg_citations_low","avg_citations_mid","avg_citations_high"), against = vs, data = d)
  plt4 <- heatmap_self(institutions= c("inclusive","selective","more_selective"), against = vs, data = d)
  
  ggarrange(plt1,plt2,plt3,plt4, ncol = 2,nrow = 2,common.legend = TRUE, legend = "bottom",labels = c('A.','B.','C.','D.'))
  
}

figS15 <- heatmap_self_plot('socsci', vs= 'rg')
ggsave(filename =  'figures/Fig_S15.png',plot = figS15,width = 12,height = 7)
figS16 <- heatmap_self_plot('socsci', vs= 'inst')
ggsave(filename =  'figures/Fig_S16.png',plot = figS16,width = 12,height = 7)

figS17 <- heatmap_self_plot('health', vs= 'rg')
ggsave(filename =  'figures/Fig_S17_health.png',plot = figS17,width = 12,height = 7)
figS18 <- heatmap_self_plot('health', vs= 'inst')
ggsave(filename =  'figures/Fig_S18_health.png',plot = figS18,width = 12,height = 7)

# Figure S13 --------------------------------------------------------------
#parameters of linear models
model_plot_all_diff <- function(d, y_title=TRUE, difference = TRUE){
  
  vars <- c("selindex [more selective]", "selindex [selective]", "avg citations [high]","avg citations [medium]",
            "avg citations","usnr [top 10]","usnr [top 100]","#authors", "career age", #"year",
            "Black men","Latinx men", "Asian men", "Asian women","Latinx women","Black women","White women")
  
  n = agg_ols %>%
    filter(str_detect(dep,d)) %>% pull(n) %>% unique()
  if (y_title) {
    if (str_detect(d,'cit')) {
      title=glue('Citations')
      # title=glue('Citations n={n}')
    }
    if (str_detect(d,'FI')) {
      title=glue('JIF')
      # title=glue('JIF n={n}')
    }
  }
  else{
    title = ''
  }
  
  
  df <- agg_ols %>%
    filter(str_detect(dep,d)) %>% 
    mutate(type = case_when(str_detect(dep,'norm')~'Topic normalized',
                            str_detect(dep,'diff')~'difference',
                            TRUE ~'Field normalized'),
           type = factor(type, levels = c('Field normalized', 'Topic normalized','difference')),
           variable = factor(variable,levels = vars)
           # institution_cov= glue("{institution_cov}\n({n})" )
    ) %>% 
    filter(!is.na(variable))
  
  if (difference) {
    df <- df %>% 
      filter(type == 'difference')
  }
  else{
    df <- df %>% 
      filter(type != 'difference')
  }
  
  plt <- df %>% 
    ggplot(aes(coeff,variable, color=institution_cov))+
    # facet_wrap(variable~.,scales = 'free_y',ncol = 1,strip.position = "left") + 
    facet_grid(.~type,scales = 'free',switch = 'y', space='free_y') +
    geom_vline(xintercept = 0)+
    geom_pointrange(shape=21, size=.5, aes(xmin= conf_lower, xmax=conf_higher))+
    geom_point(data = df %>% filter(type=='difference'), alpha=.7)+
    geom_vline(xintercept = 0)+
    scale_color_manual(values = four_colors_d)+
    scale_x_continuous(n.breaks = 6)+
    labs(x='',y= title, color= "", title = "")+ #glue('n={n}')
    theme_minimal()+
    theme(legend.position = 'bottom',
          text = element_text(size=16),
          axis.text = element_text(size=14),
          strip.text.y.left = element_text(angle=0, size = 10),
          # strip.background = element_blank(),
          # axis.text.y = element_blank(),
          plot.margin = margin(-10,0,0,0),
          axis.ticks.y = element_blank())
  
  
  if (difference) {
    plt <- plt +   
      labs(x='',y= "", color=  "", title = title) + #glue('n={n}')
      theme(plot.title = element_text(hjust = 0.5),
            plot.margin = margin(0,0,0,0),
            strip.text = element_blank())
  }
  plt
}


plt1 <- model_plot_all_diff(d='cit', difference = FALSE) 
plt2 <- model_plot_all_diff(d='FI', difference = FALSE) 

FigS19 <- ggarrange(plt1,plt2, ncol = 1,nrow = 2, heights = c(1,1.015),legend = 'bottom',common.legend=TRUE)

ggsave('figures/Fig_S19.png',plot = FigS19,width = 10,height = 10, dpi = 400)

plt3a <- model_plot_all_diff(d='cit', difference = TRUE)
plt3b <- model_plot_all_diff(d='FI', difference = TRUE) + 
  theme(strip.text.y.left = element_blank(),
        axis.text.y = element_blank())

FigS20 <- ggarrange(plt3a,plt3b, ncol =2,nrow = 1, widths = c(1.35,1),legend = 'bottom',common.legend=TRUE)

ggsave('figures/Fig_S20.png',plot = FigS20,width = 10,height = 6, dpi = 400)

model_plot <- function(d='', institutions=c("top_10","top_100","not_top"), lims = c(-.5,.2)){
  
  vars <- c("black_M","hispanic_M","asian_M","asian_F","hispanic_F","black_F",'white_F')
  labels <- c("Black\nmen","Latinx\nmen","Asian\nmen","Asian\nwomen","Latinx\nwomen",
              "Black\nwomen","White\nwomen")
  
  df <- ols_subsets %>% 
    filter(variable%in%vars, subset%in%institutions, str_detect(dep,d))
  
  inst_lev <- df %>% 
    select(subset,n) %>% 
    distinct() %>%
    mutate(order = factor(subset,levels = institutions)) %>% 
    arrange(order) %>% 
    mutate(institutions_labels =  str_to_title(str_replace_all(subset,'_', ' ')),
           institutions_labels = case_when(institutions_labels=='Mid'~ 'Medium',
                                           institutions_labels=='Hbcu'~ 'HBCU',
                                           institutions_labels=='Hsi'~ 'HSI',
                                           institutions_labels=='Womens'~ 'WC',
                                           TRUE~institutions_labels),
           # institutions_labels = glue("{institutions_labels}\n({n})"),
           subset = factor(subset,levels =institutions, labels = institutions_labels)) %>% 
    arrange(subset) %>% pull(subset)
  
  if ('top_10'%in% institutions) {
    title = 'US newsreport rank'
    if (str_detect(d,'cit')) {
      y_title =  'Citations'
    }  
    if (str_detect(d,'FI')) {
      y_title =  'JIF'
      title=element_blank()
    }
  }
  if ('selective'%in%institutions) {
    title='Carnegie selectivity index'
    y_title =element_blank()
  }
  if ('low'%in%institutions) {
    title='Average citations rank'
    y_title =element_blank()
  }
  if ('HBCU'%in%institutions) {
    title='HBCU, HSI, WC' 
    y_title =element_blank()
  }
  if (str_detect(d,'FI')) {
    title=element_blank()
  }
  
  df <- df %>% 
    mutate(institutions_labels =  str_to_title(str_replace_all(subset,'_', ' ')),
           institutions_labels = case_when(institutions_labels=='Mid'~ 'Medium',
                                           institutions_labels=='Hbcu'~ 'HBCU',
                                           institutions_labels=='Hsi'~ 'HSI',
                                           institutions_labels=='Womens'~ 'WC',
                                           TRUE~institutions_labels),
           # institutions_labels = glue("{institutions_labels}\n({n})"),
           institutions_labels = factor(institutions_labels,levels =levels(inst_lev)),
           variable = factor(variable,levels = vars,labels = labels))
  
  plt <- df %>% 
    ggplot(aes(coeff,institutions_labels, color=institutions_labels,xmin= conf_lower, xmax=conf_higher ))+
    facet_wrap(variable~.,scales = 'free_y',ncol = 1,strip.position = "left") +
    geom_vline(xintercept = 0)+
    geom_pointrange(shape=21, size=.5)+
    scale_color_manual(values = three_colors_d)+
    labs(x='',y= y_title, color= '', title = title)+
    # guides(color=guide_legend(nrow=2, byrow=TRUE))+
    theme_minimal()+
    lims(x=lims)+
    theme(text = element_text(size=16),
          # axis.text = element_text(size=16),
          # strip.text.y.left = element_text(angle=0, size = 16),
          legend.position = 'bottom',strip.background = element_blank(),
          legend.box.margin = margin(-20, 0, 0, 0),
          strip.text.y.left = element_text(angle=0),
          axis.text.y = element_blank(),
          panel.border = element_rect(fill = NA, color = "gray90"),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(size=16),
          plot.title = element_text(size=16,hjust = 0.5))
  # plot.background =  ct(color = "black"))
  if (str_detect(d,'cit')) {
    plt <- plt+
      theme(legend.position = 'none')
  }
  if (str_detect(d,'FI')) {
    plt <- plt+
      theme(plot.margin = margin(-10,0,0,0))
  }
  plt
}

plt1 <- model_plot(d='cit',c("not_top","top_100","top_10"),lims = c(-.55,.3)) 
plt2 <- model_plot(d='cit',c("low","mid","high"),lims = c(-.55,.3)) +  theme(strip.text.y.left = element_blank())
plt3 <- model_plot(d='cit',c("inclusive","selective","more_selective"),lims = c(-.55,.3)) + theme(strip.text.y.left = element_blank())

plt1_jif <- model_plot(d='FI',c("not_top","top_100","top_10"),lims = c(-.3,.11)) 
plt2_jif <- model_plot(d='FI',c("low","mid","high"),lims = c(-.3,.11)) + theme(strip.text.y.left = element_blank())
plt3_jif <- model_plot(d='FI',c("inclusive","selective","more_selective"),lims = c(-.3,.11)) + theme(strip.text.y.left = element_blank())

FigS21 <- ggarrange(plt1,plt2,plt3,plt1_jif,plt2_jif,plt3_jif,
                    ncol = 3,nrow = 2, widths = c(1.2,1,1,1.2,1,1))

ggsave(filename =  'figures/Fig_S21.png',plot = FigS21,width = 12,height = 8, dpi = 400, device = 'png')

# institutions_effect ------------------------------------------------------------
## Fig S22
institutions_effect <- function(ig=c("usnr_rank_cat","selindex","avg_citations_Q"), m = c("norm_cit_all_IAC","norm_FI_2")){
  
  plt <-
    rg_inst_citations %>% 
    filter(str_detect(categories,'effect'),
           inst_group %in% ig,
           metric %in% m) %>% 
    mutate(inst_group = factor(inst_group, 
                               levels = c("usnr_rank_cat","avg_citations_Q","selindex"),
                               labels = c("US news report","Average citations","Selectivity")),
           metric = factor(metric,levels = c('norm_cit_all_IAC','norm_FI_2'),
                           labels = c('Citations','JIF')),
           rg = fct_relabel(rg,~str_replace(.x,'\n',' ')),
           rg = fct_rev(rg),
           categories = str_replace(categories,'_',' ')) %>% #
    ggplot(aes(x= rg,y=value, color =categories))+
    geom_lollipop(point.size = 2)+
    scale_color_manual(values = two_colors)+
    geom_hline(yintercept = 0)+
    labs(x='',y= '', color= '')+
    facet_grid(inst_group~metric, scales='free_x')+
    guides(color = guide_legend(keywidth = unit(1,'cm'), override.aes = list(size=4)))+
    coord_flip()+
    theme_minimal()+
    theme(text = element_text(size=14),
          legend.position = 'bottom',strip.background = element_blank(),
          strip.text.y.left = element_text(angle=0),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(color = "black"))
  
  plt
}

FigS22 <- institutions_effect()

ggsave(filename =  'figures/Fig_S22.png',plot = FigS21,width = 10,height = 7, dpi = 400)

