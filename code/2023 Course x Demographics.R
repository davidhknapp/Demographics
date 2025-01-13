### THIS SCRIPT CREATES A BINARY LOGISTIC REGRESSION FOR SPECIFIC COURSES ###
### WITH SELECTED DEMOGRAPHIC VARIABLES AS THE PREDICTOR.                 ###

### LIBRARIES
library(dplyr)
library(naniar)

### DATA
#setwd("/Users/dknapp/Library/CloudStorage/OneDrive-FloridaStateUniversity/Data Sets")
setwd("data_raw")
CCMR_data <- read.csv("2023 CCMR.csv", header = TRUE)
Demo_data <- read.csv("2023 Demo.csv", header = TRUE)
Courses_data <- read.csv("2023 Courses.csv", header = TRUE)

### EXPLORE DATA

## CCMR_data
str(CCMR_data)
head(CCMR_data)
summary(CCMR_data$Race)
summary(CCMR_data$EcoDis)
summary(CCMR_data$ELL)
summary(CCMR_data$IEP)

count_ids_courses <- n_distinct(Courses_data$id)
count_ids_ccmr <- n_distinct(CCMR_data$id)

## Course Data
fa_data <- Courses_data %>%
  filter(crs_area == "Fine Arts") ### Filter Fine Arts courses

fa_courses <- fa_data %>%
  distinct(crsname, CRSNUM) ### Create a list of FA courses and numbers

### COURSES DATA

## Filter Music Courses
music_course_numbers <- c("8813", "8190", "8401", "8169", "8176", "8446", 
                          "8835", "8434", "8468", "8555", "8179", "8194", 
                          "8545", "8469", "8181", "8399", "8609", "8689", 
                          "8559", "8413", "8171", "8409", "8558", "8417", 
                          "8548", "8204", "8205", "8554", "8432", "8694", 
                          "8425", "8563", "8546", "8428", "8711", "8551", 
                          "8175", "8611", "8678", "8173", "8561", "4413", 
                          "8779", "8780", "8408", "8444", "8445", "8567", 
                          "8547", "8544", "8429", "8182", "8201", "8601", 
                          "8419", "8198", "8590", "8484", "8191", "8202", 
                          "8552", "8423", "8200", "8203", "8433", "8398", 
                          "8439", "8174", "8170", "8639", "8565", "8615", 
                          "4204", "8597", "6509", "6510", "8553", "8703", 
                          "8177", "8193", "8549", "8704", "6507", "6508", 
                          "8624", "8564", "8638", "8557", "8566", "8180", 
                          "8418", "8172", "8183", "8560", "8951", "8953", 
                          "8310", "8479", "8550", "8618", "8438")
  
  music_courses <- Courses_data %>% 
  filter(CRSNUM %in% music_course_numbers)

## Transform Courses


### DEMOGRAPHIC DATA

## Data Transformation

# Re-Categorize Data
CCMR_data$ELL[CCMR_data$ELL == "C"] <- "Y" ###Current to Yes
CCMR_data$ELL[CCMR_data$ELL == "F"] <- "N" ###Former to No
CCMR_data$ELL[CCMR_data$ELL == "M"] <- "N" ###Monitoring to No
CCMR_data$ELL[CCMR_data$ELL == ""] <- "N"  ###Empty value to No

CCMR_data$IEP[CCMR_data$IEP == "S"] <- "Y" ###Special to Yes
CCMR_data$IEP[CCMR_data$IEP == "F"] <- "N" ###Former to No
CCMR_data$IEP[CCMR_data$IEP == ""] <- "N"  ###Empty value to No


#Convert Strings to factors
CCMR_data$Race <- as.factor(CCMR_data$Race)
CCMR_data$EcoDis <- as.factor(CCMR_data$Eco_Dis)
CCMR_data$ELL <- as.factor(CCMR_data$ELL)
CCMR_data$IEP <- as.factor(CCMR_data$IEP)

Demo_data$sex <- as.factor(Demo_data$sex)


###DATA MERGE

## Merge Demographic Data

combined_demo <- merge(CCMR_data, Demo_data, by = "id", all.x = TRUE) ### Left join

## Merge Demographic Data into Course Data

music_course_demo_merge <- merge(music_courses, combined_demo, by = "id", all.x = TRUE) ### Left join
