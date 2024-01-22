library(tidyverse)
library(stringr)
library(janitor)
library(googlesheets4)
library(fuzzyjoin)

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "sazhu24@amherst.edu")

# Read in Check-in sheet
ss <- "https://docs.google.com/spreadsheets/d/1Lb-ly2q2orf50A099wa1RliaNHBG2EIQNLbJKcZJCQE/edit#gid=1358183820"
research <- read_sheet(ss, sheet = 2) %>% 
  tibble::as_tibble() %>% 
  clean_names() 

df <- research %>% 
  filter(is.na(status),
         !grepl('Visiting|Lecturer', rank)) %>% 
  select(-status)

potential <- read_sheet(ss, sheet = 3) %>% 
  tibble::as_tibble() %>% 
  clean_names() %>% 
  select()

df2 <- df %>% 
  left_join(select(potential, c('first_name', 'last_name', 'affiliate', 'potential', 'interest_areas', 'relevant_courses')), 
            by = c('first_name', 'last_name'))
#####

deps <- c("AMST", "ANTH", "SOCI", "ARCH", "ARHA", "ARAB", "ASLC", "BCBP", "BIOL", "BLST", "CHEM", "CHIN", "CLAS", "COLQ", "COSC", "ECON", 
          "EDST", "ENGL", "ENST", "EUST", "FAMS", "FYSE", "FREN", "GEOL", "GERM", "GREE", "HIST", "JAPA", "LATI", "LLAS", "LJST", "MATH", 
          "STAT", "MUSI", "MUSL", "NEUR", "PHIL", "PHYS", "ASTR", "POSC", "PSYC", "RELI", "RUSS", "SWAG", "SPAN", "THDA")

rd <- c('ARAB', 'CHIN', 'JAPA', 'GREE',  'LATI', 'COLQ', 'FYSE', 'MUSL')
dp2 <- setdiff(deps, rd)