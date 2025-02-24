### THIS SCRIPT CREATES A BINARY LOGISTIC REGRESSION FOR SPECIFIC COURSES ###
### WITH SELECTED DEMOGRAPHIC VARIABLES AS THE PREDICTOR.                 ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### LIBRARIES
library(dplyr)
library(naniar)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Working Directory
#setwd("R Source Data")
#setwd("data_raw")
setwd("/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw")# Read the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Data
# We will use the CCMR_demo19_cum as a master reference for all student demographics
# and match student ids for the 2023 course enrollment data
# To get CCMR_demo19_cum in your environment please run the code cleaning and 
#merging the data set in dallas_data_script_02

courses_23 <- read.csv("2023 Courses.csv", header = TRUE)

# Examining course_23 data for missings

str(courses_23)
length(unique(courses_23$id))

missing_id_courses_23 <- courses_23 %>%
  filter(if_all(-id, is.na)) %>%
  select(id) 

View(missing_id_courses_23)#None - yay!

#remove unnecessary columns

courses_23 <- courses_23 %>%
  select(id, GRADE, crs_area, CRSNUM, crsname)

str(courses_23)
table(courses_23$GRADE)#The grade data does not make sense, but we aren't really using this
table(courses_23$crs_area)# There are 34120 Uncategorized course entries

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Create Music Course List

unique_courses_23 <- courses_23 %>%
  distinct(crsname, CRSNUM) %>%
  arrange(crsname)

View(unique_courses_23)

write.csv(unique_courses_23, "music_courses_filtered_2023.csv", row.names = FALSE)

# Visual inspection and coding of courses into music categories was done in excel
# using the above exported file and brought back below

## Notes on Categorization
## indicator column : music_indicator
#   I will use 0 for not enrolled
#   1 For Band
#   2 For Choir
#   3 For Orchestra
#   4 For Modern Band
#   5 For Theory
#   6 Jazz
#   7 Music Appreciation
#   8 Guitar
#   9 Other
#   10 Piano

# Note Outliers when interpreting data
#     Chamber ensemble was once again present, in 2019 we guessed band, we will do same here
#     IB music studies were placed in other
#     Applied Music was placed in other
#     The same as in 2019, music production was combine with modern band
#     Handbells were placed in other, same as 2019
#     I decided to place jazz piano in Jazz as opposed to piano
#     Conducting was placed in other same as 2019
#     Composition courses were placed in theory
#     World Music Meister Singers was placed in choir, despite the world music component I prioritized the "singers"
#     Music and Media was placed in outher
#     Mariachi was placed in orchestra again, same as 2019

music_courses_list_23 <- read.csv("music_courses_filtered_2023.csv", header = TRUE)

# ensure columns are numeric
music_courses_list_23$CRSNUM <- as.integer(music_courses_list_23$CRSNUM)

# The 2019 course set had 139 courses, where as this list has 90
# My first theory is that the data no longer includes middle school data
#   Have we really been tracing the same students from 19 - 23? was it one cohort in 19? I don't understand

# Check work
View(music_courses_list_23)

# Add the music_indicator column to the original course list by matching CRSNUM
courses_23 <- merge(courses_23, 
                    music_courses_list_23[, c("CRSNUM", "music_indicator")], 
                    by = "CRSNUM", 
                    all.x = TRUE)

# View the updated dataframe
View(courses_23)

# Change NA music_indicator values to 0
courses_23 <- courses_23 %>%
  mutate(music_indicator = ifelse(is.na(music_indicator), 0, music_indicator))

# Verify changes
table(courses_23$music_indicator)

### ^This Counts the number of students in each Category
#     There are...
#           209 Band
#           243 Choir
#           27 Orchestra
#           245 Modern Band
#           105 Music Theory
#           18 Jazz
#           1427 Music Appreciation
#           9 Guitar
#           39 Other
#           18 Piano

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The numbers are way too small this may actually be cohort data too?
# Count unique $id where $CRSNUM is NA or empty
courses23_unique_ids <- courses_23 %>%
  filter(is.na(CRSNUM) | CRSNUM == "") %>%
  summarise(unique_count = n_distinct(id)) %>%
  pull(unique_count)

View(courses23_unique_ids)
# 33995 id's without course data
# Trying a second way to verify..
#   Drop all rows with CRSNUM = NA or " " then count unique id's left
courses23_unique_ids_2.0 <- courses_23 %>%
  filter(!(is.na(CRSNUM) | CRSNUM == ""))
  
length(unique(courses23_unique_ids_2.0$id))
#only 9007 students of data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# i should do the same for 19, 20 21 etc 
courses19_unique_ids_2.0 <- courses_19 %>%
  filter(!(is.na(CRSNUM) | CRSNUM == ""))

length(unique(courses19_unique_ids_2.0$id))
#39941
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
courses_20 <- read.csv("2020 Courses.csv", header = TRUE)

courses20_unique_ids_2.0 <- courses_20 %>%
  filter(!(is.na(CRSNUM) | CRSNUM == ""))

length(unique(courses20_unique_ids_2.0$id))
# 33137
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
courses_21 <- read.csv("2021 Courses.csv", header = TRUE)

courses21_unique_ids_2.0 <- courses_21 %>%
  filter(!(is.na(CRSNUM) | CRSNUM == ""))

length(unique(courses21_unique_ids_2.0$id))
# 24969
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
courses_22 <- read.csv("2022 Courses.csv", header = TRUE)

courses22_unique_ids_2.0 <- courses_22 %>%
  filter(!(is.na(CRSNUM) | CRSNUM == ""))

length(unique(courses22_unique_ids_2.0$id))
#17402
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Combine Courses_data and combined_demo before analysis

# Combine CCMR Demo and Courses 

courses_20 <- courses_20 %>%
  mutate(id = as.character(id))

cumulative_master_20 <- courses_20 %>%
  left_join(CCMR_demo19_comb, by = "id")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
