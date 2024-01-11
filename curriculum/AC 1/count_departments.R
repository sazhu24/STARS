library(tidyverse)
library(stringr)
library(dplyr)
library(janitor)
library(googlesheets4)
library(fuzzyjoin)
library(cronR)

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "sazhu24@amherst.edu")

# Read in Check-in sheet
ss <- "https://docs.google.com/spreadsheets/d/19cyOR583Ynrvxb-up_2LqARJ70WX0tce6LkWA5Aq0yM/edit"
courses <- read_sheet(ss, sheet = 2, col_names = TRUE, skip = 1) %>% 
  tibble::as_tibble() %>% 
  clean_names() 

#####

deps <- c("AMST", "ANTH", "SOCI", "ARCH", "ARHA", "ARAB", "ASLC", "BCBP", "BIOL", "BLST", "CHEM", "CHIN", "CLAS", "COLQ", "COSC", "ECON", 
          "EDST", "ENGL", "ENST", "EUST", "FAMS", "FYSE", "FREN", "GEOL", "GERM", "GREE", "HIST", "JAPA", "LATI", "LLAS", "LJST", "MATH", 
          "STAT", "MUSI", "MUSL", "NEUR", "PHIL", "PHYS", "ASTR", "POSC", "PSYC", "RELI", "RUSS", "SWAG", "SPAN", "THDA")

rd <- c('ARAB', 'CHIN', 'JAPA', 'GREE',  'LATI', 'COLQ', 'FYSE', 'MUSL')
dp2 <- setdiff(deps, rd)

df <- courses %>% 
  filter(qualification == 'Sustainability-Focused')

names(df) <- c("course_title", "dept", "course_description", "qualification", "keywords", "sdgs", "num_sdgs",
               "semesters_offered",  "professors")

count_department_courses <- function(df, list, departmentsOnly = F, duplicates = F){
  
  df1 <- data.frame()
  df1 <- df1[0, ]
  
  for(i in seq(length(list))){
    for(j in seq(nrow(df))){
      if(grepl(list[i], df[j, 'dept'])){
        nr <- data.frame(department = list[i], df[j, ])
        df1 <- df1 %>% 
          rbind(nr)
      }
    }
  }
  
  if(departmentsOnly){
    if(!duplicates){
      df1 <- df1 %>% distinct(course_title, .keep_all = TRUE)
    }

    
    df_sum <- df1 %>% 
      group_by(department) %>% 
      summarise(count = n(),
                courses = paste(unique(course_title), collapse = "; "))
    
    df_all <- data.frame(department = dp2) %>% 
      left_join(df_sum) %>% 
      mutate_at(c('count'), ~replace(., is.na(.), 0))

  }
  
  return(list(df1, df_all))
  
}

### only count one department per course (no duplicates)

k1 <- count_department_courses(df = df, list = dp2, departmentsOnly = T)[[1]] %>% 
  arrange(department, desc(qualification), desc(num_sdgs))

k2 <- count_department_courses(df = df, list = dp2, departmentsOnly = T)[[2]]

sum(k2$count == 0)

names(k2) <- c('Department', 'Number of Qualifying Courses', 'Course Titles')

sheet_write(k2, ss = ss, sheet = "Departments (No Duplicates)")

### count all cross-listed department per course (duplicates)

d1 <- count_department_courses(df = df, list = dp2, departmentsOnly = T, duplicates = T)[[1]] %>% 
  arrange(department, desc(qualification), desc(num_sdgs))

d2 <- count_department_courses(df = df, list = dp2, departmentsOnly = T, duplicates = T)[[2]]

sum(d2$count == 0)

names(d2) <- c('Department', 'Number of Qualifying Courses', 'Course Titles')

sheet_write(d2, ss = ss, sheet = "Departments (Duplicates)")


