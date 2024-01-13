library(tidyverse)
library(openxlsx)
library(stringr)
library(dplyr)
library(text2sdg)
library(tidytext)
library(googlesheets4)

# prefer filter function from dplyr
conflicted::conflicts_prefer(dplyr::filter)

#### get functions #### 
source("mapping-functions.R")  

### read in workday data ###
coursesF22 <- read.xlsx('data/workday/courses_f22.xlsx', sheet = 'fall 22') 
coursesS23 <- read.xlsx('data/workday/courses_s23.xlsx', sheet = 'spring 23') 
coursesF23 <- read.xlsx('data/workday/courses_f23.xlsx', sheet = 'fall 23') 
coursesS24 <- read.xlsx("data/workday/courses_s24.xlsx", sheet = 'spring 24')

### clean data ###

# clean semester course catalogs scraped from workday
courses_f22 <- cleanSemester(coursesF22, 'F22', 1)
courses_s23 <- cleanSemester(coursesS23, 'S23', 2)
courses_f23 <- cleanSemester(coursesF23, 'F23', 3)
courses_s24 <- cleanSemester(coursesS24, 'S24', 4)

# combine all semesters and reformat
all_courses <- bindSemesters(list(courses_f22, courses_s23, courses_f23, courses_s24))

### apply text2sdg mapping function ###
ac_courses <- all_courses %>% 
  select(course, ids, semesters, professors, description, cleaned_description) %>% 
  tibble::rownames_to_column(var = "rowID") 

allHits <- getHits(ac_courses)

# reformat
courses_allsystems <- allHits %>% 
  ungroup() %>% 
  group_by(course, goal, ids, semesters, professors, cleaned_description, description) %>%
  summarise(systems = paste(unique(system), collapse = ", "),
            unique_keywords = paste(unique(sdg_keywords), collapse = ", "),
            keywords_system = paste(system, ":", sdg_keywords, collapse = ", ")) %>% 
  ungroup() %>% 
  # remove duplicate keywords
  getUniqueList('unique_keywords') %>% 
  # count unique keywords
  mutate(unique_keyword_count = str_count(unique_keywords, ",") + 1,
         total_keyword_count = str_count(keywords_system, ",") + 1) %>% 
  arrange(goal, -unique_keyword_count) %>% 
  relocate(systems, .before = goal) 

### filter courses for course tool ###
remove1 <- courses_allsystems %>% 
  filter((systems == 'SDGO' | systems == 'SDSN') | (systems == 'usc_pwg' & unique_keyword_count == 1))

remove2 <- filter(courses_allsystems, goal == 1 & (unique_keywords == 'spend, class' | unique_keywords == 'family, class')) %>% 
  bind_rows(filter(courses_allsystems, goal == 8 & (unique_keywords %in% c('slavery', 'enterprise') | (grepl('culture', unique_keywords) & str_count(unique_keywords, ",") <= 1)) ),
            filter(courses_allsystems, goal == 5 & (unique_keywords %in% c('human rights', 'sex', 'equality')) & !grepl('SWAG', ids)),
            filter(courses_allsystems, goal == 9 & grepl('research', unique_keywords) & str_count(unique_keywords, ",") <= 1 & !grepl('BIOL|CHEM', ids) & 
                     !course %in% c('Green New Deal', 'Education and Inequality', 'Creativity', 'Chemistry in Society', 'Art and the Nonhuman', 'Architectural Anthro', 'Museum in Digital Age', 'Space and Design')),
            filter(courses_allsystems, goal == 11 & ((str_count(unique_keywords, ",") == 0) | (str_count(unique_keywords, ",") == 1 & grepl('cultural', unique_keywords)))),
            filter(courses_allsystems, goal == 12 & ((grepl('consumerism|consumption|life cycle', unique_keywords) & str_count(unique_keywords, ",") == 0) | (unique_keywords == 'consumption, production'))),
            filter(courses_allsystems, goal == 16 & (unique_keywords %in% c('law', 'freedom', 'politics', 'policy', 'crime'))),
            filter(courses_allsystems, goal == 17)) %>% 
  arrange(goal, -unique_keyword_count) 

remove3 <- read.xlsx("data/remove/remove_courses.xlsx") %>% 
  select(ids, course, goal, remove) %>% 
  filter(remove == 'x') %>% 
  mutate(ids = gsub(',', ' /', ids))

courses_filtered <- courses_allsystems %>% 
  anti_join(remove1) %>% 
  anti_join(remove2) %>% 
  anti_join(remove3, by = c('ids', 'goal')) %>% 
  filter(!is.na(course)) %>% 
  arrange(goal, -unique_keyword_count)

### group by SDG to summarize data ###
df1 <- courses_filtered %>% 
  group_by(course) %>% 
  mutate(all_goals = paste(unique(goal), collapse = ", ")) %>% 
  ungroup()

# get keyword count for each SDG 
for(i in 1:16){
  df1 <- df1 %>% 
    mutate(score = ifelse(goal == i, as.integer(str_count(unique_keywords, ",") + 1), '')) %>%
    rename_with(.fn = ~ paste0(., i), .cols = c(score)) 
}

# get list of keywords for each SDG 
for(i in 1:16){
  df1 <- df1 %>% 
    mutate(keywords = ifelse(goal == i, paste0(unique_keywords), '')) %>%
    rename_with(.fn = ~ paste0(., i), .cols = c(keywords))
}

### tidy data for course tool ###

df1 <- df1 %>% 
  # remove duplicate courses
  group_by(ids, course, description, semesters, professors) %>% 
  summarize(across(c('score1':'keywords16'), ~ paste(unique(.), collapse = "")),
            all_keywords = paste(unique(unique_keywords), collapse = ", "),
            all_goals = paste(unique(goal), collapse = ", "),
            num_goals = n()) %>% 
  ungroup() %>% 
  arrange(ids) %>% 
  # remove duplicate keywords
  getUniqueList('all_keywords') %>% 
  # create course URL
  mutate(url = 'https://amherst.edu') %>% 
  select(depts = ids, course, description, semesters, professors, url, 
         all_keywords, all_goals, num_goals, score1:keywords16) 

# remove selected courses
remove4 <- read.xlsx("data/remove/remove_courses2.xlsx") %>% 
  filter(remove == 'x') 

df_shiny <- df1 %>% 
  anti_join(remove4, by = 'course')

# write data for shiny app tool
# write.xlsx(df_shiny, "data/output/shiny_course_data.xlsx", rowNames = FALSE)

### prepare data for STARS course assessment ###
courses_mapped <- df_shiny %>% 
  select(course, depts, description, semesters, professors, all_keywords, all_goals, num_goals) %>% 
  arrange(num_goals) %>% 
  # sustainability classification
  mutate(sustainability_classification = sapply(courses_mapped$all_goals, determine_classification))

# get Sustainability-Focused courses 
courses_focused <- courses_mapped %>% 
  filter(sustainability_classification == 'Sustainability-Focused')

# manually edit list and read in changes
courses_focused <- read.xlsx("data/remove/focused_remove.xlsx") %>% 
  # filter courses for course assessment
  filter(is.na(remove)) %>% 
  mutate(sustainability_classification = 'Sustainability-Focused',
         num_goals = ifelse(is.na(num_goals), 0, num_goals)) %>% 
  arrange(-num_goals) %>% 
  select(course, depts, description, sustainability_classification, all_keywords, all_goals, num_goals, semesters, professors)

# rename columns
names(courses_focused) <- c("Course Title", "Department", "Course Description", "Qualification",
                            "Keywords", "SDGs", "# of SDGs", "Semesters Offered", "Professors")

write.xlsx(courses_focused, "data/output/Amherst_College_Course_Inventory.xlsx", rowNames = FALSE)

### compare to google sheet course inventory ###

# authenticate using token
# gs4_auth(cache = ".secrets", email = "sazhu24@amherst.edu")
# 
# # read sheet
# ss <- "https://docs.google.com/spreadsheets/d/19cyOR583Ynrvxb-up_2LqARJ70WX0tce6LkWA5Aq0yM/edit"
# courses <- read_sheet(ss, sheet = 2, col_names = TRUE, skip = 1) %>% 
#   tibble::as_tibble() %>% 
#   janitor::clean_names() 
# 
# anti1 <- courses_focused %>% anti_join(courses, by = c('depts' = 'departments'))
# anti2 <- courses %>% anti_join(courses_focused, by = c('departments' = 'depts'))

# jan 10, 2024
