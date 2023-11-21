library(tidyverse)
library(janitor)
library(readxl)
library(glue)

address <-  read_tsv('data/Adresses_race.txt')

df_institutions <- address %>% 
  select(Institution) %>% 
  unique()

rm(address)
gc()

institutions_list_1 <- read_excel('data/institution sector.xlsx') %>% 
  select(Institution, sector = Secteur) %>% 
  mutate(sector = case_when(sector=='A' ~'goverment',
                            sector=='E' ~'company',
                            sector=='G' ~'goverment',
                            sector=='U' ~'university')) %>% 
  distinct(Institution,.keep_all= TRUE)

institutions_list_2 <- read_excel('data/ClassificationSecteurMonde.xlsx', sheet = 'ListeEntreprises') %>% 
  bind_rows(read_excel('data/ClassificationSecteurMonde.xlsx', sheet = 'ListeUnivUSA')) %>% 
  bind_rows(read_excel('data/ClassificationSecteurMonde.xlsx', sheet = 'ListeAgencesUSA')) %>% 
  rename(company='Entreprise', university = 'U.S. University/College', goverment='DepartmentAgencies') %>% 
  select(-acronymecommentaire) %>% 
  pivot_longer(everything(), names_to = 'sector', values_to = 'Institution',values_drop_na=TRUE) %>% 
  distinct(Institution,.keep_all= TRUE)

hospitals_w <- c("NHS", "HOSP", "CLIN", "OSPED", "MED-CTR",
                 'HLTH','HEALTHCARE','EPIDEMIOL', 'MED','ONCOL','BLOOD-CTR','UNIT')
regex_hospitals <- glue_collapse(hospitals_w, sep = '|')
university_w <- c('UNIV','SCH','UCLA','CARNEGIE','COLL')
regex_university <- glue_collapse(university_w, sep = '|')
gov_w <- c('FED-RESERVE','AIR-FORCE', 'SEATTLE-CHILDRENS','US-DEPT','VIRGINIA-INST','NEW-YORK-','NATL-LAB','NATL-PRESERVE','SANITAT','DEPT-ESTATAL','POLICE-DEPT')
regex_gov <- glue_collapse(gov_w, sep = '|')
company_w <- c('PHARMA','CHAN-ZUCKERBERG','GEN-MOTORS','KAISER-PERMANENTE',
               'LA-JOLLA-INST-ALLERGY-&-IMMUNOL','FRED-HUTCHINSON','FOX-CHASE','INC$','LLC$', 'LTD','CORP','R&D','PHARM','COPORAT','BUSINESS','CONSULTING','ASSOCIATES',
               'VANDERBILT', 'FLATIRON-INST', 'LABS')
regex_company <- glue_collapse(company_w, sep = '|')


df_institutions2 <- df_institutions %>%
  left_join(institutions_list_1, by = "Institution") %>% 
  left_join(institutions_list_2, by = "Institution") %>% 
  mutate(sector = unique(sector.x, sector.y),
         sector = case_when(str_detect(Institution,regex_hospitals) ~ 'hospital',
                            str_detect(Institution,regex_university) ~ 'university',
                            str_detect(Institution,regex_gov) ~ 'goverment',
                            str_detect(Institution,regex_company) ~ 'company',
                            is.na(sector) ~ 'other',
                            TRUE ~ sector)) %>% 
  select(-sector.x, -sector.y) 


df_institutions2 %>% 
  filter(sector!='other') %>% 
  write_csv('data/institutions_us.csv')