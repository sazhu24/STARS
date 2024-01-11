library(tidyverse)
library(dplyr)
library(bslib)
library(stringr)
library(openxlsx)
library(rvest) 
library(purrr)

url <- 'https://www.amherst.edu/academiclife/provost_dean_faculty/studying-student-learning'

ac_depts <- url %>%
  read_html() %>% 
  html_elements("#node-643448 > article > div > div > ul:nth-child(2) > li > a") %>% 
  html_text() %>% 
  as.data.frame() %>% 
  rename(dept = 1) 

ac_depts2 <- url %>%
  read_html() %>% 
  html_elements("#node-643448 > article > div > div > ul:nth-child(4) > li > a") %>% 
  html_text() %>% 
  as.data.frame() %>% 
  rename(dept = 1) 

ac_depts <- ac_depts %>% 
  rbind(ac_depts2) 

ac_urls <- url %>%
  read_html() %>% 
  html_nodes("#node-643448 > article > div > div > ul:nth-child(2) > li > a") %>% 
  html_attr("href") %>% 
  as.data.frame() %>% 
  rename(url = 1) 

ac_urls2 <- url %>%
  read_html() %>% 
  html_elements("#node-643448 > article > div > div > ul:nth-child(4) > li > a") %>%   
  html_attr("href") %>% 
  as.data.frame() %>% 
  rename(url = 1) 

ac_urls <- ac_urls %>% 
  rbind(ac_urls2)

###

goals <- data.frame(goals = '')

for(i in 1:nrow(ac_urls)){
  
  goal <- ac_urls[i, 1] %>%
    read_html() %>% 
    html_elements("article > div > div") %>% 
    html_text() %>% 
    as.data.frame() %>% 
    rename(goals = 1) 
  
  goals <- goals %>% 
    rbind(goal[1, 1])
}

goals <- goals[-1, 1]

ac_dept_goals <- ac_depts %>% 
  cbind(ac_urls) %>% 
  cbind(goals)

###

df <- ac_dept_goals %>% 
  mutate(goals = str_trim(gsub('Image', '', str_trim(goals))))

write.xlsx(df, '/Users/sara/Desktop/R/STARS/learning_goals/learning_goals.xlsx')

