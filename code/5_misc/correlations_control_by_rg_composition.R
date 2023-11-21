#only for USNR
## this script repeats the function from 5_naturecomms_SI.R

heatmap_plot_size_control <- function(data = 'socsci',mt=-2){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  
  institutions <- c("top_10","top_100","not_top")
  
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
  
  
  #the expected value is the expected proportion of papers in a topic 
  #coming from each institution type, given the composition of authors by reace and gender
  
  expected_prob_carnegie <- 
    prop_rg_institutions %>% 
    left_join(df) %>% 
    group_by(usnr_rank_cat,topic) %>%
    #we compute the expected number of papers from each institution&topic with
    #the weighted sum of papers from a rg group in an institution
    # by the proportion of authors in each topic (n)
    summarise(n_expected = sum(n*prob)) %>% 
    group_by(topic) %>% 
    # then we renormalize those expected values to get the expected probability of
    # an institution grouop in a topic
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

