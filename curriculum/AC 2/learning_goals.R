library(tidyverse)
library(bslib)
library(stringr)
library(openxlsx)
library(rvest) 
library(purrr)
library(googledrive)
library(googlesheets4)
library(tidyr)

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


### edit xlsx ###

# load workbook
wb <- loadWorkbook(file = "/Users/sara/Desktop/GitHub/STARS/curriculum/AC 2/learning_goals.xlsx")

names(wb) # list worksheets
wb # view object

# add/remove a worksheet
addWorksheet(wb, "A new worksheet")
removeWorksheet(wb, "Web Scraping Output")

# list worksheets
names(wb)

## save workbook
saveWorkbook(wb, "/Users/sara/Desktop/GitHub/STARS/curriculum/AC 2/output.xlsx", overwrite = TRUE)



### use google drive API ###

# authenticate using token
gs4_auth(cache = ".secrets", email = "sazhu24@amherst.edu")

# set folder url
folder_url <- 'https://drive.google.com/drive/folders/1O1UMPKuXuUKcA_GNlvvDleHMwS7x7Ffo'

# set path using id
file_path <- as_id(folder_url)

### upload excel workbook as google sheet

# create tempfile
(tempfile <- tempfile(pattern = "learning_goals-", fileext = ".xlsx"))

# load workbook
wb <- loadWorkbook(file = "/Users/sara/Desktop/GitHub/STARS/curriculum/AC 2/learning_goals.xlsx")

# list worksheets
names(wb) 
# remove worksheet
removeWorksheet(wb, "Web Scraping Output")
# list worksheets
names(wb)

# save workbook to tempfile
saveWorkbook(wb, tempfile)

# upload
(upload_ss <- drive_upload(tempfile, 
                           type = "spreadsheet", 
                           name = "AC 2-3: Assessment of Learning Outcomes", 
                           path = file_path, 
                           overwrite = T))

# view uploaded sheet in browser
drive_browse(upload_ss)

# move to trash
drive_trash(upload_ss)

# ## read in excel file as a table, save as xlsx
# final <- read.xlsx('/Users/sara/Desktop/GitHub/STARS/curriculum/AC 2/learning_goals.xlsx', sheet = 1) 
# (tempfile <- tempfile(pattern = "learning_goals-", fileext = ".xlsx"))
# write.xlsx(final, tempfile)
# 
# ## read in excel file as a table, save as csv
# (tempfile <- tempfile(pattern = "learning_goals-", fileext = ".csv"))
# write.csv(final, tempfile, row.names = FALSE)


### download google sheet as xlsx

# get sheet file path

# option 1: get id from uploaded sheet
ss <- upload_ss$id

# option 2: get id from url
sheet_url <- 'https://docs.google.com/spreadsheets/d/1zp--w52vh_2SSjQgQn23ZAFhZWTVGRDMG7lYnz1tBUw/edit?usp=drive_link'
ss <- as_id(sheet_url)

# download
download_path <- '/Users/sara/Desktop/GitHub/STARS/test.xlsx'
drive_download(ss, path = download_path, overwrite = T)


### update google sheet range (to preserve formatting)

## use excel file to update
final <- read.xlsx('/Users/sara/Desktop/GitHub/STARS/curriculum/AC 2/learning_goals.xlsx', sheet = 1)
final <- final[-c(1, 2, 40, 41),]

## use existing google sheet to update
sheet <- read_sheet(ss = ss)
sheet <- sheet[-c(1, 2, 40, 41), -7]

## read google sheet
range_write(
  ss = ss,
  data = sheet,
  sheet = 1,
  range = "A4:E40",
  col_names = FALSE,
  reformat = FALSE
)

# sheet_append(ss, sheet, sheet = 1)
# sheet_write(final, ss = ss, sheet = "Learning Outcomes")

