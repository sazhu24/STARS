
### functions ###

cleanWorkdayCourses <- function(df, semester, semester_order){
  
  cf <- df %>% 
    mutate(course_name = ifelse(grepl('^[A-Z]{4} [0-9]{3}-', course), course, ''),
           professor = '',
           description = '',
           details = '',
           size = '')
  
  for(i in 1:nrow(cf)){
    if(i > 1) { 
      j <- (i - 1) 
      if(cf[j, 'course'] == 'Instructors'){
        if(cf[i + 3, 'course'] == 'Instructional Format'){
          cf[i, 'professor'] <- paste(as.character(cf[i, 'course']), as.character(cf[i + 1, 'course']), as.character(cf[i + 2, 'course']), sep = ', ')
        } else if(cf[i + 1, 'course'] != 'Instructional Format'){
          cf[i, 'professor'] <- paste(as.character(cf[i, 'course']), as.character(cf[i + 1, 'course']), sep = ', ')
        } else {
          cf[i, 'professor'] <- cf[i, 'course']
        }
      }
      
      if(cf[j, 'course'] == 'Course Section Description'){
        
        if(cf[i, 'course'] == '(empty)'){
          cf[i, 'description'] <- 'EMPTY'
        } else if(str_trim(gsub("^(\\s*\\([^\\)]+\\))", "", cf[i, 'course'])) != ''){
          cf[i, 'description'] <- cf[i, 'course']
        } else {
          cf[i, 'description'] <- cf[i + 1, 'course']
        }
        
      }
      if(i > 2) { 
        if(cf[j, 'course'] == 'Section Details' & cf[i - 2, 'details'] == ''){
          cf[i, 'details'] <- cf[i, 'course']
        }
      }
      if(grepl('(Closed \\|)|(Open \\|)', cf[i, 'course'])){
        cf[i, 'size'] <- cf[i, 'course']
      }
    }
  }
  
  cf2 <- cf %>% 
    filter_at(vars(course_name:size), any_vars(. != '')) 
  
  for(i in 1:(nrow(cf2) + 4)){
    
    cf2[i, 'professor'] <- cf2[i + 4, 'professor']
    cf2[i, 'description'] <- cf2[i + 3, 'description']
    cf2[i, 'details'] <- cf2[i + 2, 'details']
    cf2[i, 'size'] <- cf2[i + 1, 'size']
    
  }
  
  cf2 <- cf2 %>% na.omit()
  
  for(i in 1:(nrow(cf2) - 1)){
    if(cf2[i, 'professor'] == ''){
      cf2[i, 'professor'] <- cf2[i + 1, 'professor']
    }
    if(cf2[i, 'description'] == ''){
      cf2[i, 'description'] <- cf2[i + 1, 'description']
    }
  }
  
  cf3 <- cf2 %>% 
    filter_at(vars(course_name:size), any_vars(. != '')) %>% 
    filter(!grepl('Special Topics|Senior Honors|Capstone Project|Senior Seminar|Senior Tutorial|Senior Research Seminar|Advanced Research', course_name) & course_name != '') %>% 
    select(-course_name) %>% 
    mutate(course_type = ifelse(grepl('((01|02)L -)|-L01)', course), 'L',
                                ifelse(grepl('((01|02)F -)', course), 'F', 
                                       ifelse(grepl('((01|02)S -)', course), 'S', ''))),
           course = gsub('L - ', ' - ', course),
           course = gsub('F - ', ' - ', course), 
           course = gsub('S - ', ' - ', course), 
           section = str_match(course, "[0-9]{3}-\\s*(.*?)\\s*( -)")[,2],)
  
  cf3 <- cf3 %>% 
    filter(course_type == '') %>% 
    mutate(course = sub("-0[0-9](.*)- ", " - ", course),
           size = str_extract_all(size, '[0-9]{1,2}'),
           num_enrolled = '',
           professor = ifelse(professor == '(empty)'|professor == 'NA', '', professor))
  
  for(i in 1:nrow(cf3)){
    cf3[i, 'num_enrolled'] <- cf3[i, 'size'][[1]][1]
  }
  
  cf4 <- cf3 %>% 
    group_by(course) %>% 
    summarise(professors = paste(unique(professor), collapse = ", "),
              students_enrolled = sum(as.integer(num_enrolled)),
              sections = n())
  
  cf5 <- cf3 %>% 
    select(course, description) %>% 
    mutate(description = str_trim(gsub("^(\\s*\\([^\\)]+\\))", "", description)),
           description = str_trim(gsub("^(\\s*\\[[^\\)]+\\])", "", description)),
           description = str_trim(str_replace(description, '\\(Offered as SPAN|Offered as BIOL 291 and BCBP 291\\)|\\(Offered as LJST 349 and SWAGS 349\\)', '')))
  
  for(i in 1:nrow(cf5)){
    desc_string <- cf5[i, 'description']
    sentences <- str_split(desc_string, boundary("sentence"))[[1]]
    first_sentence <- sentences[1]
    
    if(grepl('^Offered as|^\\(Offered as', first_sentence)){
      sentences <- sentences[-1]
      cf5[i, 'description'] <- str_c(sentences, collapse = '')
    }
  }
  
  cf5 <- cf5 %>% 
    mutate(description = str_trim(gsub('^(\\.)', '', description)),
           cleaned_description = gsub("[^[:alnum:][:space:]]", " ", description),
           cleaned_description = gsub("'", "", cleaned_description),
           cleaned_description = tolower(str_trim(cleaned_description)),
           dept = str_extract(course, '^[A-Z]{4}'),
           id = str_extract(course, '^[A-Z]{4} [0-9]{3}'),
           name = gsub("^[A-Z]{4}", "", course),
           name = str_trim(gsub("^[-0-9]", "", name)),
           name = gsub("^[0-9]{3} -", "", name),
           name = gsub("^[0-9]{3}-0[0-9] -", "", name),
           name = gsub('^221-L01 - ', '', name),
           name = str_trim(name),
           semester = semester, 
           semester_order = semester_order,
           name = str_trim(str_replace(name, '(Dis|Discussion|Dis\\.|Lab|w/Lab|W/Lab)$', ''))) %>% 
    filter(name != 'Organic Chem I' & !is.na(description)) %>% 
    distinct(course, .keep_all = TRUE) %>% 
    left_join(cf4)
  
  cf6 <- cf5 %>% 
    group_by(id) %>% 
    summarise(name = last(name),
              dept = last(dept),
              semester = last(semester),
              semester_order = last(semester_order))
  
  cf6 <- cf6 %>% 
    group_by(name, semester, semester_order) %>% 
    summarise(ids = paste(unique(id), collapse = " / "),
              depts = paste(unique(dept), collapse = ", "))
  
  cf7 <- cf5 %>% 
    distinct(name, .keep_all = TRUE) 
  
  cf6 <- cf6 %>% 
    left_join(select(cf7, c(name, description, cleaned_description, professors, students_enrolled, sections)), by = 'name') %>% 
    rename(course = name)
  
  print(paste(semester, 'CLEAN'))
  return(cf6)
  
}

apply_context_dependency <- function(tt) {
  tt <- tolower(tt)
  corrections$before <- tolower(corrections$before)
  corrections$after <- tolower(corrections$after)
  tt <- stringi::stri_replace_all_regex(tt,
                                        pattern = corrections$before,
                                        replacement = corrections$after,
                                        vectorize = FALSE)
  tt
}

getUniqueList <- function(df, col){
  df_col <- df[col][[1]]
  k <- strsplit(df_col, ",")
  k <- map(k, str_trim)
  k <- map(k, unique)
  k <- map(k, str_remove, "NA")
  df[col][[1]] <- map(k, str_c, collapse = ', ') 
  df[col][[1]] <- gsub(', $', '', df[col][[1]])
  return(df)
}

### course mapping ###

cleanHits <- function(df){
  df$keyword <- map(df$features, ~ str_c(unique(str_split(., boundary("word"))[[1]]), collapse = ", "))
  
  master_course_sdg_data <- df %>% 
    mutate(keyword = gsub(",", "", keyword),
           goal = as.numeric(gsub("\\D", "", sdg))) %>% 
    left_join(ac_courses, by = c("document" = "rowID")) %>%
    select(system, goal, course, ids, semesters, professors, description, cleaned_description, system, keyword) %>%
    filter(!keyword %in% c('work', 'who', 'erotica'),
           !(keyword %in% c('laws') & goal == 16),
           goal!= 17) %>% 
    group_by(course, system) %>%
    mutate(all_keywords = paste(unique(keyword), collapse = ", "),
           all_goals = paste(sort(unique(goal)), collapse = ", ")) %>% 
    ungroup() %>% 
    group_by(course, system, goal, ids, semesters, professors, description, cleaned_description, all_keywords, all_goals) %>%
    summarise(sdg_keywords = paste(unique(keyword), collapse = ", "),
              sdg_keyword_count = n()) %>% 
    arrange(goal) %>% 
    relocate(sdg_keywords, .before = all_keywords)
  
  return(master_course_sdg_data)
}


### sustainability classification ###
# function returns sustainability classification based on list of goals (SDGs):
# sustainability-focused: at least 1 social economic goal and at least 1 environment goal
# sdg-related: at least 1 goal

determine_classification <- function(x) {
  
  social_economic_goals = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 16, 17)
  environment_goals = c(6, 7, 12, 13, 14, 15)
  
  if (is.na(x) | x == "NA" | x == "") {
    return("Not Related")
  }
  
  separate_sdgs <- as.numeric(trimws(strsplit(x, ",")[[1]]))
  if ((length(intersect(social_economic_goals, separate_sdgs)) >= 1 & 
       length(intersect(environment_goals, separate_sdgs)) >= 1)|length(separate_sdgs) > 6) {
    return("Sustainability-Focused")
  } else {
    return("SDG-Related")
  }
}

