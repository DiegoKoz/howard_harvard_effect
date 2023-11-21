library(readxl)
library(tidyverse)
library(glue)
library(fuzzyjoin)
library(stringr)

carnegie <- read_excel('data/carnegie/CCIHE2021-PublicData.xlsx', sheet = 'Data') %>% 
  mutate(unitid = as.character(unitid)) %>% 
  select(carnegie_name=name,unitid, stabbr)

HBCU <- read_excel("handcoding/carnegie_flags.xlsx",sheet = "hbcu")
HSI <- read_excel("handcoding/carnegie_flags.xlsx",sheet = "hsi")
WC <- read_excel("handcoding/carnegie_flags.xlsx",sheet = "womens")

make_query <- function(df){
  df %>% 
    mutate(name = toupper(name),
           name = str_replace_all(name, ' & ', '&'),
           name = str_remove_all(name, 'UNIVERSITY'),
           name = str_remove_all(name, 'COLLEGE'),
           name = str_remove_all(name, "'"),
           name = trimws(name),
           name = toupper(name),
           name = str_replace_all(name, ' ', '-'),
           query =   glue("(Institution LIKE '%{name}%' AND [Province]='{stabbr}') OR"))
}

# HBCU --------------------------------------------------------------------

hbcu_query <- HBCU %>% 
  filter(retrieved==FALSE) %>% 
  left_join(carnegie, by = "unitid") %>% 
  make_query()

#write query to clipboard
hbcu_query$query %>% clipr::write_clip()

# HSI --------------------------------------------------------------------

hsi_query <- HSI %>% 
  filter(retrieved==FALSE) %>% 
  left_join(carnegie, by = "unitid") %>% 
  make_query()

#write query to clipboard
hsi_query$query %>% clipr::write_clip()


# WC ----------------------------------------------------------------------
wc_query <- WC %>% 
  filter(retrieved==FALSE) %>% 
  left_join(carnegie, by = "unitid") %>% 
  make_query()

#write query to clipboard
wc_query$query %>% clipr::write_clip()

######################################################################################################################
###################run the queries in sql and save results to handcoding/carnegie_hbcu_hsi_wc_names###################
######################################################################################################################
hbcu_sql <- read_tsv('handcoding/carnegie_hbcu_hsi_wc_names/HBCU_institutions.tsv',col_names = c('name', 'state','n'), skip = 1)
hsi_sql <- read_tsv('handcoding/carnegie_hbcu_hsi_wc_names/HSI_institutions.tsv',col_names = c('name', 'state','n'), skip = 1)
wc_sql <- read_tsv('handcoding/carnegie_hbcu_hsi_wc_names/WC_institutions.tsv',col_names = c('name', 'state','n'), skip = 1)

df <- hbcu_sql %>% 
  mutate(group='hbcu') %>% 
  bind_rows(hsi_sql %>% 
              mutate(group='hsi')) %>% 
  bind_rows(wc_sql %>% 
              mutate(group='wc'))


check_threshold <- function(t){
 x <-  df %>% 
    filter(n>t)
  nrow(x)
}

tibble(x=1:100) %>% 
  mutate(n=unlist(map(x,check_threshold))) %>% 
  ggplot(aes(x,n))+
  geom_line()

#with a treshold of 25 we keep only 1000 rows

carnegie_data <- hbcu_query %>% 
  select(unitid, name, city,group='hbcu') %>% 
  bind_rows(hsi_query %>% 
              select(unitid, name, city,group='hsi')) %>% 
  bind_rows(wc_query %>% 
              select(unitid, name, city,group='womens'))

df2 <- df %>% 
  filter(n>=25) %>% 
fuzzy_left_join(carnegie_data,by = 'name',match_fun =str_detect )


df2 %>% 
  left_join(carnegie %>% select(unitid,carnegie_name), by = "unitid") %>% 
  select(name_wos =name.x, state, city, n, group=group.x, unitid,carnegie_name, text_query=name.y) %>% 
  xlsx::write.xlsx(x = .,file = 'handcoding/carnegie_hbcu_hsi_wc_names/hbcu_hsi_wc_to_clean.xlsx')

