library(tidyverse)

load('data/shiny_data.Rdata')


get_topics_to_label <- function(grp=c('Black_Women', 'White_Men'),
                               carnegie_grp= c("top_10","top_100","not_top"), data = 'socsci'){
  
  df <- retrieve_table(result = 'marginal_by_topic',set = data)
  df2 <- retrieve_table(result = 'joint_prob',set = data)
  
  prob_carnegie <- get(paste0('topic_prop_by_carnegie_groups_',data)) %>% 
    filter(carnegie_tag%in% carnegie_grp) %>%
    rename(prob_carnegie = prob)
  
   prob_rg <- df %>%
    mutate(rg = paste(group,gender, sep = '_' )) %>% 
    filter(rg%in%grp) %>%
    group_by(rg,topic,topic_proportion) %>%
    # group_by(group,gender,topic,topic_proportion) %>% 
    summarise(prob = sum(prob),
              top_words = unique(top_words)) 
   
   rg_top_topics <- prob_rg %>% 
     group_by(rg) %>% 
     slice_max(prob,n = 5) %>% 
     ungroup() %>% 
     select(topic, top_words)

   carnegie_top_topics <- prob_carnegie %>% 
     group_by(carnegie_tag) %>% 
     slice_max(prob_carnegie,n = 5) %>% 
     ungroup() %>% 
     left_join(df %>% select(topic,top_words) %>% distinct()) %>% 
     select(topic, top_words)
   
   bind_rows(rg_top_topics,carnegie_top_topics) %>% 
     distinct() %>% 
     arrange(as.numeric(topic))
    
}

# get_topics_to_label() %>% 
#   write_csv('handcoding/topic_labels_raw.csv')
  
howard_harvard_topics_to_label <- function(grp=c('Black_Women', 'White_Men'), data = 'socsci'){
    
    df <- retrieve_table(result = 'marginal_by_topic',set = data)
    df2 <- retrieve_table(result = 'joint_prob',set = data)
    
    hh_topic_prop_df <- hh_topic_prop %>% 
      pivot_longer(cols = -c('carnegie_name'), names_to = 'topic', names_prefix = 'topic_', values_to='prob') %>% 
      mutate(prob = replace_na(prob, 0)) %>% 
      rename(group=carnegie_name,prob_hh=prob)
    
    prob_rg <- df %>%
      mutate(rg = paste(group,gender, sep = '_' )) %>% 
      filter(rg%in%grp) %>%
      group_by(rg,topic,topic_proportion) %>%
      # group_by(group,gender,topic,topic_proportion) %>% 
      summarise(prob = sum(prob),
                top_words = unique(top_words)) 
    
    rg_top_topics <- prob_rg %>% 
      group_by(rg) %>% 
      slice_max(prob,n = 5) %>% 
      ungroup() %>% 
      select(topic, top_words)
    
    hh_top_topics <- hh_topic_prop_df %>% 
      group_by(group) %>% 
      slice_max(prob_hh,n = 5) %>% 
      ungroup() %>% 
      left_join(df %>% select(topic,top_words) %>% distinct()) %>% 
      select(topic, top_words)
    
    bind_rows(rg_top_topics,hh_top_topics) %>% 
      distinct() %>% 
      arrange(as.numeric(topic))
    
  }

 howard_harvard_topics_to_label() %>% 
  write_csv('handcoding/topic_labels_raw_hh.csv')
