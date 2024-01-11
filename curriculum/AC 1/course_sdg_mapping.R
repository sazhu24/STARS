library(tidyverse)
library(openxlsx)
library(stringr)
library(dplyr)
library(text2sdg)
library(tidytext)
library(googlesheets4)

# prefer filter from dplyr
conflicted::conflicts_prefer(dplyr::filter)

#### GET PACKAGES AND FUNCTIONS #### 
source("mapping-functions.R")  

### read in data ###
coursesF22 <- read.xlsx('data/workday/courses_f22.xlsx', sheet = 'fall 22') 
coursesS23 <- read.xlsx('data/workday/courses_s23.xlsx', sheet = 'spring 23') 
coursesF23 <- read.xlsx('data/workday/courses_f23.xlsx', sheet = 'fall 23') 
coursesS24 <- read.xlsx("data/workday/courses_s24.xlsx", sheet = 'spring 24')

### clean data ###

# clean semester course catalogs scraped from workday
courses_f22 <- cleanWorkdayCourses(coursesF22, 'F22', 1)
courses_s23 <- cleanWorkdayCourses(coursesS23, 'S23', 2)
courses_f23 <- cleanWorkdayCourses(coursesF23, 'F23', 3)
courses_s24 <- cleanWorkdayCourses(coursesS24, 'S24', 4)

# combine all semesters
all_courses <- bind_rows(courses_f22, courses_s23, courses_f23, courses_s24) %>% 
  # account for courses that change ids between semesters
  mutate(ids = gsub('ENST 314 / SOCI 314', 'ENST 272 / SOCI 272', ids),
         course = str_replace(course, '(Dis|Discussion|Dis\\.|Lab|w/Lab|W/Lab)$', '')) %>% 
  arrange(semester_order) %>% 
  group_by(ids) %>% 
  summarise(course = dplyr::last(course),
            professors = paste(unique(professors), collapse = ", "),
            description = dplyr::last(description),
            cleaned_description = dplyr::last(cleaned_description),
            num = n(),
            semesters = paste(semester, collapse = ", "),
            semester_order = paste(semester_order, collapse = ", "),) %>% 
  ungroup() %>% 
  arrange(semester_order) %>% 
  group_by(course) %>% 
  summarise(name = dplyr::last(course),
            ids = dplyr::last(ids),
            semesters = paste(semesters, collapse = ", "),
            professors = paste(unique(professors), collapse = ", "),
            description = dplyr::last(description),
            cleaned_description = dplyr::last(cleaned_description)) %>% 
  mutate(cleaned_description = apply_context_dependency(cleaned_description),
         professors = trimws(gsub('^,', ' ', professors)))

# remove duplicate listings for professors
all_courses <- getUniqueList(all_courses, 'professors')

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
  mutate(unique_keyword_count = str_count(unique_keywords, ",") + 1,
         total_keyword_count = str_count(keywords_system, ",") + 1) %>% 
  arrange(goal, -unique_keyword_count) %>% 
  relocate(systems, .before = goal) 

# remove duplicate keywords
k <- strsplit(courses_allsystems$unique_keywords, ",")
k <- map(k, str_trim)
k <- map(k, unique)
courses_allsystems$unique_keywords <- list_flatten(k) 
courses_allsystems$unique_keyword_count <- as.integer(map(k, length))
courses_allsystems <- courses_allsystems %>% 
  mutate(unique_keywords = gsub("^c\\(|\\)$", "", unique_keywords),
         unique_keywords = gsub('"', "", unique_keywords))

# filter courses
not <- courses_allsystems %>% 
  filter((systems == 'SDGO' | systems == 'SDSN') | (systems == 'usc_pwg' & unique_keyword_count == 1))

not2 <- filter(courses_allsystems, goal == 1 & (unique_keywords == 'spend, class' | unique_keywords == 'family, class')) %>% 
  bind_rows(filter(courses_allsystems, goal == 8 & (unique_keywords %in% c('slavery', 'enterprise') | (grepl('culture', unique_keywords) & str_count(unique_keywords, ",") <= 1)) ),
            filter(courses_allsystems, goal == 5 & (unique_keywords %in% c('human rights', 'sex', 'equality')) & !grepl('SWAG', ids)),
            filter(courses_allsystems, goal == 9 & grepl('research', unique_keywords) & str_count(unique_keywords, ",") <= 1 & !grepl('BIOL|CHEM', ids) & 
                     !course %in% c('Green New Deal', 'Education and Inequality', 'Creativity', 'Chemistry in Society', 'Art and the Nonhuman', 'Architectural Anthro', 'Museum in Digital Age', 'Space and Design')),
            filter(courses_allsystems, goal == 11 & ((str_count(unique_keywords, ",") == 0) | (str_count(unique_keywords, ",") == 1 & grepl('cultural', unique_keywords)))),
            filter(courses_allsystems, goal == 12 & ((grepl('consumerism|consumption|life cycle', unique_keywords) & str_count(unique_keywords, ",") == 0) | (unique_keywords == 'consumption, production'))),
            filter(courses_allsystems, goal == 16 & (unique_keywords %in% c('law', 'freedom', 'politics', 'policy', 'crime'))),
            filter(courses_allsystems, goal == 17)) %>% 
  arrange(goal, -unique_keyword_count) 

courses_filtered <- courses_allsystems %>% 
  anti_join(not) %>% 
  anti_join(not2) %>% 
  filter(!is.na(course)) %>% 
  arrange(goal, -unique_keyword_count) 

remove <- read.xlsx("data/remove/remove_courses.xlsx") %>% 
  select(ids, course, goal, remove) %>% 
  filter(remove == 'x') %>% 
  mutate(ids = gsub(',', ' /', ids))

df1 <- courses_filtered %>% 
  anti_join(remove, by = c('ids', 'goal')) %>% 
  group_by(course) %>% 
  mutate(all_goals = paste(unique(goal), collapse = ", ")) %>% 
  ungroup()

# count words for each SDG
for(i in 1:16){
  df1 <- df1 %>% 
    mutate(score = ifelse(goal == i, as.integer(str_count(unique_keywords, ",") + 1), '')) %>%
    rename_with(.fn = ~ paste0(., i), .cols = c(score)) 
}

for(i in 1:16){
  df1 <- df1 %>% 
    mutate(keywords = ifelse(goal == i, paste0(unique_keywords), '')) %>%
    rename_with(.fn = ~ paste0(., i), .cols = c(keywords))
}

# reformat
df2 <- df1 %>% 
  select(-all_goals) %>% 
  group_by(ids, course, description, semesters, professors) %>% 
  summarize(across(c('score1':'keywords16'), ~ paste(unique(.), collapse = "")),
            all_keywords = paste(unique(unique_keywords), collapse = ", "),
            all_goals = paste(unique(goal), collapse = ", "),
            num_goals = n()) %>% 
  ungroup() %>% 
  arrange(ids) %>% 
  mutate(url = 'https://amherst.edu') %>% 
  select(depts = ids, course, description, semesters, professors, url, all_keywords, all_goals, num_goals, score1:keywords16) 

df2 <- getUniqueList(df2, 'all_keywords') 
#write.xlsx(courses_mapped, "data/remove/remove_courses2.xlsx", rowNames = FALSE)

# remove selected courses
remove2 <- read.xlsx("data/remove/remove_courses2.xlsx") %>% 
  filter(remove == 'x') 

df2 <- df2 %>% 
  anti_join(remove2, by = 'course')

# write data for shiny app tool
# write.xlsx(df2, "data/output/shiny_data_11-25.xlsx", rowNames = FALSE)

### sustainability classification ###
courses_mapped <- df2 %>% 
  select(course, depts, description, semesters, professors, all_keywords, all_goals, num_goals) %>% 
  arrange(num_goals)

courses_mapped$sustainability_classification <- sapply(courses_mapped$all_goals, determine_classification)

courses_focused <- courses_mapped %>% 
  filter(sustainability_classification == 'Sustainability-Focused')

#write.xlsx(courses_focused, "/Users/sara/Desktop/R/sustainability/SDG_courses/data/focused_remove.xlsx", rowNames = FALSE)
courses_focused <- read.xlsx("data/remove/focused_remove.xlsx") %>% 
  filter(is.na(remove)) %>% 
  select(-remove) %>% 
  mutate(sustainability_classification = 'Sustainability-Focused',
         num_goals = ifelse(is.na(num_goals), 0, num_goals)) %>% 
  arrange(-num_goals) %>% 
  select(course, depts, description, sustainability_classification, all_keywords, all_goals, num_goals, semesters, professors)

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
