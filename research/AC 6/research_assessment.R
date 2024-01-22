library(tidyverse)
library(stringr)
library(janitor)
library(googlesheets4)
library(fuzzyjoin)
library(tidytext)
library(openxlsx)

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
  clean_names()

df <- df %>%
  left_join(select(potential, c('first_name', 'last_name', 'affiliate', 'potential', 'interest_areas', 'relevant_courses')),
            by = c('first_name', 'last_name'))

#####

getUniqueList <- function(df, col){
  df_col <- df[col][[1]]

  k <- strsplit(df_col, ",") %>%
    map(str_trim) %>%
    map(unique) %>%
    map(str_remove, "NA")

  df[col][[1]] <- map(k, str_c, collapse = ', ')
  df[col][[1]] <- gsub(', $', '', df[col][[1]])

  return(df)
}

data(stop_words)
mystopwords <- tibble(word = c("students", "attention", "keywords", "de", "research", "pre", "study", "hours", "week", "readings",
                               "including", "include", "topics", "understanding", "basic", "assignments", "meetings", "examine",
                               "understand", "questions", "oral", "written", "short", "papers", "pay", "special", "attention",
                               "prof", "is", "on", "her", "his", "wolpaw", "it's", "i've", "i'm", "examined",
                               tolower(df$first_name), tolower(df$last_name)), lexicon = "SZ")

stop_words <- stop_words %>% rbind(mystopwords)

#####

research <- df %>%
  #filter(!is.na(research_summary)) %>%
  arrange(department_1) %>%
  unite("name", 'first_name':'last_name', sep = ' ', na.rm = TRUE) %>%
  mutate(across(c('department_1':'department_2'),
                ~ ifelse(grepl("Gender Studies", .x), "Women and Gender Studies", .x)),
         across(c('department_1':'department_2'),
                ~ case_match(.x,
                             "American Studies" ~ "AMST", "Anthropology" ~ "ANTH", "Sociology" ~ "SOCI",
                             "Architectural St" ~ "ARCH", "Art" ~ "ARHA", "Asian Languages" ~ "ASLC",
                             "Biochemistry and Biophysics" ~ "BCBP", "Biology" ~ "BIOL", "Black Studies" ~ "BLST", "Chemistry" ~ "CHEM",
                             "Classics" ~ "CLAS", "Computer Science" ~ "COSC", "Economics" ~ "ECON", "Educational Studies" ~ "EDST",
                             "English" ~ "ENGL", "Environmental Studies" ~ "ENST", "European Studies" ~ "EUST", "Film and Media Studies" ~ "FAMS",
                             "French" ~ "FREN", "Geology" ~ "GEOL", "German" ~ "GERM", "History" ~ "HIST", "Latinx and Latin American Studies" ~ "LLAS",
                             "Law Jurisprudence" ~ "LJST", "Math" ~ "MATH", "Statistics" ~ "STAT",
                             "Music" ~ "MUSI", "Neuroscience" ~ "NEUR", "Philosophy" ~ "PHIL",
                             "Physics" ~ "PHYS", "Astronomy" ~ "ASTR", "Political Science" ~ "POSC", "Psychology" ~ "PSYC",
                             "Religion" ~ "RELI", "Russian" ~ "RUSS", "Women and Gender Studies" ~ "SWAG",
                             "Spanish" ~ "SPAN", "Theater and Dance" ~ "THDA", NA ~ NA, .default = "MISSING"))) %>%
  unite("department", 'department_1':'department_2', sep = ' / ', na.rm = TRUE) %>%
  mutate(text = tolower(gsub('[[:punct:] ]+',' ', research_summary)))

research_words <- research %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

### apply text2sdg mapping function ###
df2 <- research %>%
  select(name, text) %>%
  filter(!is.na(text)) %>%
  distinct(name, .keep_all = TRUE) %>%
  tibble::rownames_to_column(var = "rowID") %>%
  mutate(rowID = as.integer(rowID))

#write.xlsx(df2, "/Users/sara/Desktop/GitHub/STARS/research/AC 6/research_description.xlsx", rowNames = FALSE)

# detect SDGs using all systems
all_hits <- detect_sdg_systems(df2$text)

# detect SDGs using all five systems
# usc_pwg_keywords <- read.csv("data/keywords/usc_keywords.csv") %>%
#   filter(remove != 'x')
#
# usc_pwg_keywords <- usc_pwg_keywords[-grep("#", usc_pwg_keywords$keyword),]
# usc_pwg_keywords <- usc_pwg_keywords[!duplicated(usc_pwg_keywords),]
#
# # create system for text2sdg
# usc_pwg_system <- usc_pwg_keywords %>%
#   mutate(system = "usc_pwg",
#          query = paste0('"', keyword, '"')) %>%
#   rename(sdg = goal) %>%
#   select(system, sdg, query)
#
# # detect SDGs
# hits_USC <- detect_any(text = df2$text,
#                        system = usc_pwg_system)
#
# hits_text2sdg <- detect_sdg_systems(df2$text,
#                                     system = c("Aurora", "Elsevier", "SIRIS", "SDSN", "SDGO"))
#
# all_hits <- hits_USC %>% rbind(hits_text2sdg)

top_hits <- all_hits %>%
  unnest_tokens(word, features) %>%
  count(word, sort = TRUE)

hits <- all_hits %>%
  getUniqueList('features')

df3 <- df2 %>%
  left_join(hits, by = c("rowID" = "document")) %>%
  filter(!features %in% c('work', 'research', 'education', 'who', 'university', 'teaching', 'learning',
                          'undergraduate', 'degree', 'graduate')) %>%
  select(name, text, goal = sdg, system, keyword = features) %>%
  mutate(goal = as.numeric(gsub("\\D", "", goal)))

df4 <- df3 %>%
  group_by(name, goal) %>%
  summarise(keywords = paste(unique(keyword), collapse = ", "),
            goals = paste(sort(unique(goal)), collapse = ", ")) %>%
  getUniqueList('keywords')

df5 <- df3 %>%
  group_by(name, goal) %>%
  summarise(keywords = paste(unique(keyword), collapse = ", ")) %>%
  ungroup() %>%
  getUniqueList('keywords') %>%
  #filter(grepl(',', keywords)) %>%
  group_by(name) %>%
  summarise(keywords = paste(unique(keywords), collapse = ", "),
            goals = paste(sort(unique(goal)), collapse = ", ")) %>%
  ungroup() %>%
  getUniqueList('keywords') %>%
  mutate(num_keywords = ifelse(keywords != '', as.integer(str_count(keywords, ",") + 1), 0),
         num_goals = ifelse(keywords != '', as.integer(str_count(goals, ",") + 1), 0)) %>%
  arrange(-num_keywords)

faculty <- research %>%
  select("name", "department", "research_summary", "potential", "interest_areas", "relevant_courses") %>%
  left_join(df5, by = "name") %>%
  select("name", "department", "keywords","goals", "num_keywords", "num_goals",
         "research_summary", "potential", "interest_areas", "relevant_courses")

write.xlsx(faculty, "/Users/sara/Desktop/GitHub/STARS/research/AC 6/research_assessment.xlsx", rowNames = FALSE)

###

research <- read.xlsx("/Users/sara/Desktop/GitHub/STARS/research/AC 6/research_assessment.xlsx", sheet = 2) %>%
  filter(focused == 'x')

deps <- c("AMST", "ANTH", "SOCI", "ARCH", "ARHA", "ARAB", "ASLC", "BCBP", "BIOL", "BLST", "CHEM", "CHIN", "CLAS", "COLQ", "COSC", "ECON",
          "EDST", "ENGL", "ENST", "EUST", "FAMS", "FYSE", "FREN", "GEOL", "GERM", "GREE", "HIST", "JAPA", "LATI", "LLAS", "LJST", "MATH",
          "STAT", "MUSI", "MUSL", "NEUR", "PHIL", "PHYS", "ASTR", "POSC", "PSYC", "RELI", "RUSS", "SWAG", "SPAN", "THDA")

rd <- c('ARAB', 'CHIN', 'JAPA', 'GREE', 'LATI', 'COLQ', 'FYSE', 'MUSL')
pm <- c('FAMS', 'BCBP', 'EDST', 'NEUR', 'LLAS', 'EUST')
depts <- setdiff(deps, rd)
depts <- setdiff(depts, pm)

df <- research
list <- depts
col <- 'department'
df1 <- data.frame()

for(i in seq(length(list))){
  for(j in seq(nrow(df))){
    if(grepl(list[i], df[j, col])){
      nr <- data.frame(department = list[i], df[j, ])
      df1 <- df1 %>% rbind(nr)
    }
  }
}

df_sum <- df1 %>%
  group_by(department) %>%
  summarise(count = n(),
            faculty = paste(unique(name), collapse = "; "))

df_all <- data.frame(department = depts) %>%
  left_join(df_sum) %>%
  mutate_at(c('count'), ~replace(., is.na(.), 0))

yes <- sum(df_all$count != 0) # 22
no <- sum(df_all$count == 0) # 10
num_depts <- length(depts) # 32
dept_pct <- round(yes/num_depts, 2) # 0.6 - 0.69

no_dept <- df_all %>% 
  filter(count == 0)

# blst, stat, thda
names(df_all) <- c('Department', 'Number of Faculty', 'Faculty Names')

ss <- 'https://docs.google.com/spreadsheets/d/1NJa1ShT4iUVXSZYGWvmclur1t1vdh5v51FrJo-HFD6c/edit#gid=0'
sheet_write(df_all, ss = ss, sheet = 1)
