library(tidyverse)
library(dplyr)
library(bslib)
library(stringr)
library(openxlsx)

# Power BI data set with local tags
local <- read_csv("data/local.csv") 
# Power BI complete data set
df <- read_csv("data/vdata.csv") 
# Maetadata SKU
sku_df2 <- read_csv("data/master.csv") 

sku_df <- read.xlsx("data/all_distributors.xlsx") 
sku_df <- sku_df[, c(1, 5, 6, 12, 2, 11, 8)]
names(sku_df) <- c('category', 'MD_attributes', 'vendor', 'product', 'distributor', 'unit_price', 'state')
sku_dup <- sku_df %>% mutate(dup = ifelse(duplicated(product), 'duplicate', ""))
# Remove duplicate rows in Maetadata
sku_df <- distinct(sku_df) %>% 
  mutate(state = toupper(state))

names(sku_df) <- c('category', 'MD_attributes', 'vendor', 'product', 'distributor', 'unit_price', 'state')

sku2 <- read.xlsx("data/sku/local.xlsx") 
sku2 <- sku2[, c(1, 2, 3, 7, 6, 5)]
names(sku2) <- c('category', 'MD_attributes', 'vendor', 'product', 'unit_price', 'state')
sku2 <- sku2 %>% distinct()

sku3 <- read.xlsx("data/sku/business_certifications.xlsx") 
sku3 <- sku3[, c(1, 5, 6, 12, 11, 8)]
names(sku3) <- c('category', 'MD_attributes', 'vendor', 'product', 'unit_price', 'state')
sku3 <- sku3 %>% distinct()

sku4 <- read.xlsx("data/sku/veg_alternatives.xlsx") 
sku4 <- sku4[, c(1, 5, 6, 12, 11, 8)]
names(sku4) <- c('category', 'MD_attributes', 'vendor', 'product', 'unit_price', 'state')

sku5 <- read.xlsx("data/sku/seafood.xlsx") 
sku5 <- sku5[, c(1, 5, 6, 12, 11, 8)]
names(sku5) <- c('category', 'MD_attributes', 'vendor', 'product', 'unit_price', 'state')

sku6 <- read.xlsx("data/sku/organic.xlsx") 
sku6 <- sku6[, c(1, 5, 6, 12, 11, 8)]
names(sku6) <- c('category', 'MD_attributes', 'vendor', 'product', 'unit_price', 'state')

att <- sku2 %>% rbind(sku3) %>% rbind(sku4) %>% rbind(sku5) %>% rbind(sku6)

att <- att %>% distinct(vendor, product, unit_price, .keep_all = TRUE)
  
sku_df <- att %>% 
  mutate(distributor = '') %>% 
  rbind(sku_df) %>% 
  distinct(vendor, product, unit_price, .keep_all = TRUE)


#vendors_sku <- read.xlsx("food_systems/data/vendors-sku.xlsx") 

attributes <- c("USDA Certified Organic", "Rainforest Alliance Certified", "B-Corp. Certified", 
                "MBE- Business Certification", "WBE- Business Certification", "Non GMO Verified",
                "Veg Alternatives to Meat/Dairy", "Cage Free", "MSC Certified Fisheries (No Chain of Custody)")

umass_EFH_vendors <- c('JOE CZAJKOWSKI FARM LLC', 'LITTLE LEAF FARMS', 
                       'HIGH LAWN FARMS', 'MISTY KNOLL FARM', 
                       'WARM COLORS APIARY', "QUEEN'S GREENS", 'WILD PLANET FOODS')

names(umass_EFH_vendors) <- c('Some Organic and All GAP Certified', '100% Recaptured Rainwater, No Pesticides, Non-GMO, Grown Hydroponically, Whole Facility is Sustainably Engineered', 
                              'Free of Artificial Growth Hormones and Pesticides', 'Principles of Organic Agriculture (IFOAM)', 
                              'Principles of Organic Agriculture (IFOAM)', 'Certified Organic (any IFOAM-recognized standard)', 'Monterey Bay Aquarium Seafood Watch (Best Choices, Good Alternatives)')


names(df) <- c('category', 'vendor', 'product', 'BI_plant_based', 'BI_certifications', 'spend')
names(local) <- c('category', 'vendor', 'product', 'BI_plant_based', 'BI_certifications', 'spend', 'BI_local')

#df2 <- df %>% mutate(dup = ifelse(duplicated(product), 'duplicate', ""))

# Join local and SKU
val_data <- df %>% 
  left_join(select(local, product, BI_local, vendor), by=c('product', 'vendor')) %>% 
  left_join(select(sku_df, product, MD_attributes, distributor, unit_price, state), by=c('product')) 

# Filter out non-food items and select columns
master <- val_data %>% 
  select(category, vendor, product, MD_attributes, BI_plant_based, BI_local, state, spend, unit_price, distributor) %>% 
  filter(category != "Non-Food") %>% 
  # reduces from 1852 to 1828 when distinct() is applied ?
  distinct()

totalspend <- sum(master$spend)

###

local_att <- c('Local')
plant_based_att <- c('Processed Culinary Ingredients', 'Simple Processed Foods', 'Minimal (or No) Processing', 'Veg Alternatives to Meat/Dairy')
tp_att <- c("USDA Certified Organic", "Rainforest Alliance Certified", "Cage Free", "MSC Certified Fisheries (No Chain of Custody)", 
            "Monterey Bay Seafood Watch - Good Alternative (Other)", "Fair Trade USA Certified")
business_certification_att <- c('MBE- Business Certification', 'WBE- Business Certification', "B-Corp. Certified")

# Create new data frame
m <- master %>% 
  add_column(plant_based = master$BI_plant_based) %>% 
  add_column(local = master$BI_local) %>% 
  add_column(EFH_certification = '') %>% 
  add_column(business_certification = '')

# Iterate through each row to extract list items from attributes column
for (i in 1:nrow(m)){

  # grab the list of attributes
  att <- as.list(str_trim(strsplit(m$MD_attributes[i], ",")[[1]]))
  
  # Iterate through each item in the list
  for (j in 1:length(att)){
    
    # tag plant-based
    if(is.na(m$plant_based[i])){
      if (att[j] %in% plant_based_att){
        m$plant_based[i] <- att[j]
        # add plant-based items here
      } else if (grepl('VEGAN|PLANT BASED|IMIT|TOFU|TEMPEH|BURGER BLK BEAN', toupper(m$product[i]))) {
        m$plant_based[i] <- 'Veg Alternatives to Meat/Dairy'
      }
    }
    
    # tag local
    if(is.na(m$local[i])){
        if (att[j] %in% local_att){
          m$local[i] <- att[j]
        }
    }
  
    # tag certifications
    if (att[j] %in% tp_att){
      if(m$EFH_certification[i] == ''){
        m$EFH_certification[i] <- att[j]
      } else {
        m$EFH_certification[i] <- paste(m$EFH_certification[i], att[j], sep=', ')
      }
    }
    # check to see if item is UMASS certified if it lacks a EFH_certification
    if(m$EFH_certification[i] == '' & toupper(m$vendor[i]) %in% umass_EFH_vendors){
      m$EFH_certification[i] <- paste('UMASS - ', names(which(umass_EFH_vendors == toupper(m$vendor[i]))), sep = '')
    }
  
    # tag business_certification
    if (att[j] %in% business_certification_att){
      if(m$business_certification[i] == ''){
        m$business_certification[i] <- att[j]
      } else {
        m$business_certification[i] <- paste(m$business_certification[i], att[j], sep=', ')
      }
    }
    # check to see if item is UMASS certified if it lacks a business_certification
    # if(m$business_certification[i] == '' & toupper(m$vendor[i]) %in% umass_BC_vendors){
    #   m$business_certification[i] <- names(which(umass_BC_vendors == toupper(m$vendor[i])))
    # }
  }

  
}

# Select columns
master <- m %>% 
  select(category, vendor, product, plant_based, local, state, EFH_certification, business_certification, spend, unit_price, distributor) %>% 
  mutate(local = gsub('Local Vendors/', '', local))

master$plant_based <- vapply(master$plant_based, paste, collapse = ", ", character(1L))
master$EFH_certification <- vapply(master$EFH_certification, paste, collapse = ", ", character(1L))
master$business_certification <- vapply(master$business_certification, paste, collapse = ", ", character(1L))

veg_cat <- c("Apples", "Berries", "Broccoli", "Cabbage", "Onions/Garlic", "Melons", "Squash", "Root Vegetables", 
             "Leafy Greens",  "Cauliflower", "Fruit", "Vegetables" )

master <- master %>% 
  mutate_at(c(4,7), ~ifelse(. == 'NA', NA, .)) %>% 
  mutate(EFH_certification = ifelse(EFH_certification == 'NA', NA, EFH_certification)) %>% 
  mutate(category = ifelse(category %in% veg_cat, 'Produce', category))

#Manual Fixes
local_fix <- c('SALMON PORTN 4 OZ SKNLS', 'SALMON PORTN 4 OZ FRSH')
EFH_fix <- c('Teatulia', 'GRANDYOATS', 'HARNEY & SONS TEA CO LLC', 'NASOYA FOODS USA LLC')
names(EFH_fix) <- c('USDA Certified Organic, Rainforest Alliance Certified', 'USDA Certified Organic', 'USDA Certified Organic', 'USDA Certified Organic')
veg_fix <- c('CRAB IMIT FLAKE USA FZ', 'CRAB IMIT LEG FZ', 'CRABMEAT IMIT 2.5 LB FZ')

for(i in 1:nrow(master)){
  
  if(master$product[i] %in% local_fix){
    master$local[i] <- 'Non-Local Products'
  }
  if(master$vendor[i] %in% EFH_fix){
    master$EFH_certification[i] <- names(which(master$vendor[i] == EFH_fix))
  } 
  if(grepl('ORGANIC', toupper(master$product[i])) & master$EFH_certification[i] == ''){
    master$EFH_certification[i] <- 'USDA Certified Organic'
  } 
  if(master$product[i] %in% veg_fix){
    master$plant_based[i] <- ''
  } 
  if(grepl('VEGAN', toupper(master$product[i]))){
    master$category[i] <- 'Other Beverages (Non Dairy)'
  }
  if (grepl('VEGAN|PLANT BASED|TOFU|TEMPEH|BURGER BLK BEAN', toupper(master$product[i]))) {
    master$plant_based[i] <- 'Veg Alternatives to Meat/Dairy'
  }
  if(grepl('Veg Alternatives to Meat/Dairy', master$plant_based[i]) & master$category[i] != 'Other Beverages (Non Dairy)'){
    master$category[i] <- 'Meat Alternatives'
  } 
  
}


##

vendors <- master %>% 
  group_by(vendor) %>% 
  summarise(total_spend = sum(spend)) %>% 
  arrange(desc(total_spend))

# MINORITY & WOMEN OWNED
business_certification_only <- master %>%
  filter(business_certification != '') %>%
  arrange(desc(spend))

bc_vendors <- business_certification_only %>% 
  group_by(vendor) %>% 
  select(vendor, business_certification) %>% 
  #filter(business_certification != 'B-Corp. Certified') %>% 
  distinct() %>% 
  left_join(vendors)

for(i in 1:nrow(bc_vendors)){
  master <- master %>% 
    mutate(business_certification = ifelse(vendor == bc_vendors$vendor[i], bc_vendors$business_certification[i], business_certification))
}

master <- master %>% 
  mutate_at(c(4,5), ~ifelse(is.na(.), '', .))

# went from 45 to 85
business_certification_only <- master %>%
  filter(business_certification != '') %>%
  arrange(desc(spend))

total_business_certification <- sum(bc_vendors$total_spend)/totalspend # = 0.08 or 0.126 w/B-Corp

bc_vendors <- business_certification_only %>% 
  group_by(vendor) %>% 
  summarise(total = sum(spend),
            certification = business_certification) %>% 
  distinct() %>% 
  arrange(desc(total)) 

wbe <- sum(bc_vendors[grepl('WBE- Business Certification', bc_vendors$certification), 'total'])/totalspend
mbe <- sum(bc_vendors[grepl('MBE- Business Certification', bc_vendors$certification), 'total'])/totalspend
bcorp <- sum(bc_vendors[grepl('B-Corp. Certified', bc_vendors$certification), 'total'])/totalspend

## CERTIFIED
certified_only <- master %>% 
  filter(EFH_certification != '') %>% 
  arrange(vendor) 

cert_vendors <- certified_only %>% 
  group_by(vendor) %>% 
  select(vendor) %>% 
  distinct()

total_cert <- sum(certified_only$spend)/totalspend # = 0.07

certified_vendors <- certified_only %>% 
  group_by(vendor) %>% 
  summarise(total = sum(spend)) %>% 
  arrange(desc(total))

## LOCAL
local_only <- master %>% 
  filter(local != '') %>% 
  arrange(desc(spend)) %>% 
  select(-c(unit_price, distributor)) %>% 
  distinct() 

local_vendors <- local_only %>% 
  group_by(vendor) %>% 
  select(vendor, local) %>% 
  distinct() 

local_vendors <- local_only %>% 
  filter(local != 'Non-Local Products') %>% 
  group_by(vendor) %>% 
  summarise(total = sum(spend)) %>% 
  arrange(desc(total))

nonlocal_vendors <- local_only %>% 
  group_by(vendor) %>% 
  summarise(total = sum(spend)) %>% 
  arrange(desc(total))


nonlocal_only <- local_only %>% 
  filter(local == 'Non-Local Products')

local_only <- local_only %>% 
  filter(local != 'Non-Local Products') %>% 
  rbind(nonlocal_only) %>% 
  mutate(spend = round(spend, digits = 2))

total_nonlocal <- sum(nonlocal_only$spend)/totalspend # = 0.04
total_local <- sum(local_only$spend)/totalspend # = 0.11

## PLANT_BASED
plant_based_only <- master %>% 
  filter(plant_based != '') %>% 
  filter(plant_based != 'NA') %>% 
  arrange(desc(spend))

total_plant_based <- sum(plant_based_only$spend)/totalspend # = 0.35

ordered <- master %>% 
  arrange(desc(spend))
ordered <- ordered[1:50,]

meat <- master %>% 
  filter(category == 'Meat'|category == 'Poultry') %>% 
  arrange(desc(spend))

meattotal <- sum(meat$spend/totalspend)  # .19

seafood <- master %>% 
  filter(category == 'Fish/Seafood') %>% 
  arrange(desc(spend))

seafoodtotal <- sum(seafood$spend/totalspend) # 0.027

dairy <- master %>% 
  filter(category == 'Dairy') %>% 
  arrange(desc(spend))

dairy <- master %>% 
  filter(category == 'Dairy') %>% 
  arrange(desc(spend))

pb <- master %>% 
  filter(category == 'Meat Alternatives'|grepl('PAKORA|SAMOSA|SPRING ROLL VEG', toupper(product))&!grepl('CANDY CHOC', toupper(product))) %>% 
  arrange(desc(spend))

pbtotal <- sum(pb$spend/totalspend)  # .02

ordered <- ordered[1:50,]
write.csv(dairy, 'food_systems/dairy.csv', row.names = F)


###
food_data <- data.frame(credit = '', indicator = '', score = '', points = '', comment = '') %>% 
  rbind(c('OP 7: Dining Services Procurement', '% of food and beverage spend that meets sustainability criteria (EFH)', 
          total_cert, total_cert*6,
'- ecological:
- fair:
- human:
- UMASS:')) %>% 
  rbind(c('OP 7: Dining Services Procurement', '% of food and beverage spend that is plant-based', 
          total_plant_based, total_plant_based*6, '35.1% is plant-based')) %>% 
  rbind(c('OP 7: Dining Services Procurement', '% of dining service spend with social impact suppliers',
          total_business_certification, ifelse((total_business_certification - 0.1 > 0), 2, 1),
'- 6.8% with women-owned businesses
- 1.5% with minority-owned businesses
- 4.6% with B-Corp. Certified businesses')) %>%  
  filter(credit != '') %>% 
  mutate(score = round(as.numeric(score), 3),
         points = round(as.numeric(points), 3),)

names(food_data) <- c('Credit', 'Indicator', 'Score', 'Points', 'Notes')

file <- "/Users/sara/Desktop/GitHub/STARS/food and dining/food_data.xlsx"
write.xlsx(food_data, file, rowNames = FALSE)

# format excel workbook
wb <- openxlsx::loadWorkbook(file)

setColWidths(wb, 1, cols = c(1, 2, 3, 4, 5), widths = c(20, 40, 10, 10, 50))
freezePane(wb, 1, firstRow = TRUE)
header_style <- createStyle(
  textDecoration = c("bold"), fontSize = 12, fontName = 'Tw Cen MT', halign = "center"
)
addStyle(wb, 1, header_style, rows = 1, cols = 1:5)
#addStyle(wb, 1, createStyle(fontName = 'Tw Cen MT'), rows = 1:9, cols = 1:9)
openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

### use google drive API ###
library(googledrive)
library(googlesheets4)
# authenticate using token
gs4_auth(cache = ".secrets", email = "sazhu24@amherst.edu")

### upload excel workbook as google sheet
upload2GDrive <- function(data, folder_url, filename){
  
  # set path using id from folder url
  file_path <- as_id(folder_url)
  
  # create tempfile
  (tempfile <- tempfile(pattern = "file-", fileext = ".xlsx"))
  
  # save workbook to tempfile
  openxlsx::saveWorkbook(wb, tempfile)
  
  # upload
  (upload_ss <- drive_upload(tempfile, 
                             type = "spreadsheet", 
                             name = filename,
                             path = file_path, 
                             overwrite = T))
  
  # view uploaded sheet in browser
  drive_browse(upload_ss)
}

upload2GDrive(data = wb, 
              folder_url = 'https://drive.google.com/drive/folders/1_aDHUiQuqsmLdhDHHSdXXKEzcjtk2fcB', 
              filename = 'Amherst Food Analysis 21-22')


