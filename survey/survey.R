library(tidyverse)
library(openxlsx)
library(stringr)
library(plotly)
library(ggplot2)

survey <- read.xlsx('/Users/sara/Desktop/R/STARS/survey/Sustainability Campus Survey_January 4, 2024_03.06.xlsx') %>% 
  janitor::clean_names()

df <- survey %>% 
  filter(status == 'IP Address'|status == 'Response Type') %>% 
  mutate(name = paste(recipient_first_name, recipient_last_name))

df <- df[, -c(1:4, 7:17, 84)] 

df <- df %>% 
  mutate(across(c(q11, q42), ~ sub('.*>', '', .x))) %>% 
  rename(score = sc0, comments = q42, type2 = q2, transportation_mode = q11, gender = q116, staff_years = q4, class_year = q5, major = q6) %>% 
  select(name, type, type2, score, comments, race, gender, class_year, major, staff_years, progress, duration_in_seconds, everything()) %>% 
  mutate(staff_years = sub('&gt;', '>', .x))

questions <- df[1,]

df2 <- df2 %>% 
  nest(transportation = c("q7", "q81", "q82", "q84", "q84_5_text", "q9_1", "transportation_mode", "q11_7_text", "q12_1", 
                          "q12_2", "q12_3", "q12_4", "q12_5", "q12_6", "q12_7", "q12_7_text", "q85", "q13_1", "q14", "q14_4_text", "q15")) %>% 
  nest(culture = c("q86", "q91", "q91_5_text", "q88", "q90", "q89", "q92", "q93", "q94", "q95", "q96", "q119_1", "q119_2", "q119_3",
                   "q119_4", "q119_5", "q97_1", "q97_2", "q97_3", "q99_1", "q99_2", "q99_3", "q99_4")) %>% 
  nest(literacy = c("q25", "q100", "q101", "q102", "q103", "q104", "q106", "q107", "q108", "q109", "q110", "q111", "q112", "q113", "q114")) 

transportation <- df2 %>% 
  select(c(name:staff_years, transportation)) %>% 
  unnest(transportation)

culture <- df2 %>% 
  select(c(name:staff_years, culture)) %>% 
  unnest(culture)

literacy <- df2 %>% 
  select(c(name:staff_years, literacy)) %>% 
  unnest(literacy) 

write_csv(df, '/Users/sara/Desktop/R/STARS/survey/data_clean.csv')






