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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CCMR Data

CCMR19_data <- read.csv("2019 CCMR.csv", header = TRUE)
CCMR20_data <- read.csv("2020 CCMR.csv", header = TRUE)
CCMR21_data <- read.csv("2021 CCMR.csv", header = TRUE)
CCMR22_data <- read.csv("2022 CCMR.csv", header = TRUE)
CCMR23_data <- read.csv("2023 CCMR.csv", header = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjust id columns to match before merging
CCMR20_data <- CCMR20_data %>%
  rename(idnr = id)

CCMR23_data <- CCMR23_data %>%
  rename(idnr = id)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjust campus column names
## 2019
CCMR19_data <- CCMR19_data %>%
  rename(campus_id_19 = Campus..)

CCMR19_data <- CCMR19_data %>%
  rename(campus_name_19 = Campus.Name)

CCMR19_data <- CCMR19_data %>%
  rename(district_name_19 = District.Name)

## 2020
CCMR20_data <- CCMR20_data %>%
  rename(campus_id_20 = Campus..)

CCMR20_data <- CCMR20_data %>%
  rename(campus_name_20 = Campus.Name)

CCMR20_data <- CCMR20_data %>%
  rename(district_name_20 = District.Name)

## 2021
CCMR21_data <- CCMR21_data %>%
  rename(campus_id_21 = Campus..)

CCMR21_data <- CCMR21_data %>%
  rename(campus_name_21 = Campus.Name)

CCMR21_data <- CCMR21_data %>%
  rename(district_name_21 = District.Name)

## 2022
CCMR22_data <- CCMR22_data %>%
  rename(campus_id_22 = Campus..)

CCMR22_data <- CCMR22_data %>%
  rename(campus_name_22 = Campus.Name)

CCMR22_data <- CCMR22_data %>%
  rename(district_name_22 = District.Name)

# 2023
CCMR23_data <- CCMR23_data %>%
  rename(campus_id_23 = Campus..)

CCMR23_data <- CCMR23_data %>%
  rename(campus_name_23 = Campus.Name)

CCMR23_data <- CCMR23_data %>%
  rename(district_name_23 = District.Name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine all data frames using Reduce() and merge()
CCMR_list <- list(CCMR19_data, CCMR20_data, CCMR21_data, CCMR22_data, CCMR23_data)

CCMR_Master <- Reduce(function(x, y) merge(x, y, by = "idnr", all = TRUE), CCMR_list)

# Verify and adjust merged columns
str(CCMR_Master)

# CCMR demographic columns were not merged, rather adjoined. Data from each needs to be transferred to a single column
#         Ex. Exo.Dis is Eco.Disx Eco.Disy Etc...
#   the same goes for ELL, IEP, and Race

# R does not allow for data frame manipulation of any kind (renaming, mutating, merging) if it detects duplicate columns
#   to fix this we need to filter out all irrelevant columns and deal with the duplicates of the columns we do want
#       start by identifying a list of neccessary columns

des_columns <- c("idnr", "Eco.Dis.x", "Eco.Dis.y", "EcoDis", "Eco_Dis", "Economically.Disadvantaged", "EL.Status", 
                 "EB.EL.Status", "EB.EL.Status..Current.and.Monitored.", "ELL.x", "ELL.y", "IEP", "Special.Ed.x",
                 "Special.Ed.y", "SpecEd", "Special.Ed..S.Special.ED.F.Former.Special.ED.", "Race.x", "Race.y", 
                 "Ethnicity.x", "Ethnicity.y", "Ethnicity", "campus_id_19", "campus_name_19", "district_name_19",
                 "campus_id_20", "campus_name_20", "district_name_20", "campus_id_21", "campus_name_21", 
                 "district_name_21", "campus_id_22", "campus_name_22", "district_name_22", "campus_id_23", 
                 "campus_name_23", "district_name_23")

# remove all unneccessary columns
CCMR_Master <- CCMR_Master %>% 
  select(all_of(des_columns))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Coalesce duplicate columns for each demographic

## Eco Dis
# Define the list of columns to check
eco_dis_columns <- c("Eco.Dis.x", "Eco.Dis.y", "EcoDis", "Eco_Dis", "Economically.Disadvantaged")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(Eco_Dis_Cum = coalesce(!!!syms(eco_dis_columns)))

# Check the results
table(CCMR_Master$Eco_Dis_Cum)
length(unique(CCMR_Master$Eco_Dis_Cum))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ELL

# Define the list of columns to check
Ell_dis_col <- c("EL.Status", "EB.EL.Status", "EB.EL.Status..Current.and.Monitored.",
                     "ELL.x", "ELL.y")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(ELL_cum = coalesce(!!!syms(Ell_dis_col)))

# Check the results
table(CCMR_Master$ELL_cum)
length(unique(CCMR_Master$ELL_cum))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## IEP

# Define the list of columns to check
IEP_dis_col <- c("IEP", "Special.Ed.x", "Special.Ed.y", "SpecEd", 
                 "Special.Ed..S.Special.ED.F.Former.Special.ED.")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(IEP_cum = coalesce(!!!syms(IEP_dis_col)))

# Check the results
table(CCMR_Master$IEP_cum)
length(unique(CCMR_Master$IEP_cum))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Race

# Define the list of columns to check
Race_dis_col <- c("Race.x", "Race.y", "Ethnicity.x", "Ethnicity.y", "Ethnicity")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(Race_cum = coalesce(!!!syms(Race_dis_col)))

# Check the results
table(CCMR_Master$Race_cum)
length(unique(CCMR_Master$Race_cum))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Campus_Id

# Define the list of columns to check
campus_id_col <- c("campus_id_19", "campus_id_20", "campus_id_21", "campus_id_22", "campus_id_23")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(campus_id = coalesce(!!!syms(campus_id_col)))

# Check the results
table(CCMR_Master$campus_id)
length(unique(CCMR_Master$campus_id))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Campus_Name

# Define the list of columns to check
campus_name_col <- c("campus_name_19", "campus_name_20", "campus_name_21", "campus_name_22", "campus_name_23")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(campus_name = coalesce(!!!syms(campus_name_col)))

# Check the results
table(CCMR_Master$campus_name)
length(unique(CCMR_Master$campus_name))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## District_Name

# Define the list of columns to check
district_name_col <- c("district_name_19", "district_name_20", "district_name_21", "district_name_22", "district_name_23")

# Create the new column with the first non-NA value from the specified columns
CCMR_Master <- CCMR_Master %>%
  mutate(district_name = coalesce(!!!syms(district_name_col)))

# Check the results
table(CCMR_Master$district_name)
length(unique(CCMR_Master$district_name))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Rename variables/columnss to binary indicators or better colnames

## ELL recategorization
CCMR_Master$ELL_cum[CCMR_Master$ELL_cum == "C"] <- "Y" ###Current to Yes
CCMR_Master$ELL_cum[CCMR_Master$ELL_cum == "F"] <- "N" ###Former to No
CCMR_Master$ELL_cum[CCMR_Master$ELL_cum == "M"] <- "N" ###Monitoring to No
CCMR_Master$ELL_cum[CCMR_Master$ELL_cum == ""] <- "N"  ###Empty value to No

## IEP Recategorization
CCMR_Master$IEP_cum[CCMR_Master$IEP_cum == "S"] <- "Y" ###Special to Yes
CCMR_Master$IEP_cum[CCMR_Master$IEP_cum == "F"] <- "N" ###Former to No
CCMR_Master$IEP_cum[CCMR_Master$IEP_cum == ""] <- "N"  ###Empty value to No

#Verify CCMR Data
str(CCMR_Master)
table(CCMR_Master$ELL_cum)
table(CCMR_Master$IEP_cum)
table(CCMR_Master$Race_cum)
table(CCMR_Master$Eco_Dis_Cum)
table(CCMR_Master$district_name)
table(CCMR_Master$campus_id)
table(CCMR_Master$campus_name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Drop remaining duplicate columns
ccmr_mast_descol <- c("id", "ELL_cum", "IEP_cum", "Race_cum", "Eco_Dis_Cum", "district_name", "campus_id", "campus_name")

CCMR_Master <- CCMR_Master %>%
  select(all_of(ccmr_mast_descol))

#Check Clean CCMR data
str(CCMR_Master)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Demo Data

# The only thing needed from here is sex
#   But there are multiple years of demo data, but 2019 has demo data for every cohort
#     First we need to see if there are significant changes in the demo sex data
#     between years. If not we can just use the 2019 demo data as the master set

#load data
demo_19 <- read.csv("2019 Demo.csv", header = TRUE)
demo_20 <- read.csv("2020 Demo.csv", header = TRUE)
demo_21 <- read.csv("2021 Demo.csv", header = TRUE)
demo_22 <- read.csv("2022 Demo.csv", header = TRUE)
demo_23 <- read.csv("2023 Demo.csv", header = TRUE)

#select only the necessary data
# Ensure all data sets have the same structure, and make colnames match
demo_23 <- demo_23 %>%
  rename(idnr = id)

demo_19 <- demo_19 %>% select(idnr, sex) %>% mutate(year = 2019)
demo_20 <- demo_20 %>% select(idnr, sex) %>% mutate(year = 2020)
demo_21 <- demo_21 %>% select(idnr, sex) %>% mutate(year = 2021)
demo_22 <- demo_22 %>% select(idnr, sex) %>% mutate(year = 2022)
demo_23 <- demo_23 %>% select(idnr, sex) %>% mutate(year = 2023)

# Combine into one long dataset
demo_cum <- bind_rows(demo_19, demo_20, demo_21, demo_22, demo_23)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## looking for demographic changes across years

sex_changes <- demo_cum %>%
  group_by(idnr) %>%
  summarise(unique_sex = n_distinct(sex)) %>%
  filter(unique_sex > 1)

# Join back to original dataset to see details
sex_changes_details <- demo_cum %>%
  filter(idnr %in% sex_changes$idnr) %>%
  arrange(idnr, year)

print(sex_changes_details)

# Find students whose sex changed between 'F' and 'M'
sex_changes <- demo_cum %>%
  distinct(idnr, sex) %>%  # Ensure only unique id-sex combinations
  group_by(idnr) %>%
  summarise(has_F = any(sex == "F"),
            has_M = any(sex == "M"), .groups = "drop") %>%
  filter(has_F & has_M) %>%  # Keep students with both 'F' and 'M'
  select(idnr)

# Retrieve details of affected students
sex_changes_details <- demo_cum %>%
  filter(idnr %in% sex_changes$idnr) %>%
  arrange(idnr, year)

# View results
print(sex_changes_details)
length(unique(sex_changes_details$idnr)) # 55 students have sex changes
#     some may be actual transitioning students, some may be miss entry
# This data also reveals that sex data is not consistent across all demo years
# all students here have sex values, but not all students have sex values in 2019
# Therefore we cannot use the 2019 demo data as a master set.

# Examining characteristics of sex change observations
sex_change_details <- sex_changes_details %>%
  rename("id" = "idnr")

CCMR_Master$id <- as.character(CCMR_Master$id)
sex_changes_details$id <- as.character(sex_changes_details$id)

CCMR_sex_change_details <- CCMR_Master %>%
  filter(id %in% as.vector(sex_changes_details$id))

View(CCMR_sex_change_details)
#Most are from different schools, no trend realized.

### With no trend realized, we will assume miss entry for these 55 students.
#   Remove these 55 students from the data.
#   This will be easier if done after merging CCMR demo and course data sets.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Further Data Cleaning

#revisiting districtless students

# Filter students where district_name is missing (NA or empty)
districtless_students <- CCMR_Master %>%
  filter(is.na(district_name) | district_name == "")

View(districtless_students)
## All students are from the same school which closed in 2019.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examining missing student id's

# Count students where id is missing (NA or empty)
missing_id_count <- CCMR_Master %>%
  filter(is.na(id) | id == "") %>%
  summarise(count = n())

View(missing_id_count)

# Filter students where id is missing (NA or empty)
students_missing_id <- CCMR_Master %>%
  filter(is.na(id) | id == "")

View(students_missing_id)

# There are 97 students with spaces for student ID numbers. There were no patterns
#   in their enrollments either so I will remove them from the data as we cannot match 
#   them with their course data because of this. This also shouldn't skew the distributions 
#   much either as there was no group > 9 from any one school. Most were around 3 - 5.

# remove the 97
length(CCMR_Master$id)
CCMR_Master <- CCMR_Master %>%
  filter(!is.na(id) | id == "")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Convert Strings to factors

CCMR_Master$Race_cum <- as.factor(CCMR_Master$Race_cum)
CCMR_Master$Eco_Dis_Cum <- as.factor(CCMR_Master$Eco_Dis_Cum)
CCMR_Master$ELL_cum <- as.factor(CCMR_Master$ELL_cum)
CCMR_Master$IEP_cum <- as.factor(CCMR_Master$IEP_cum)

demo_cum$sex <- as.factor(demo_cum$sex)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Dealing with Gender in demo data

# The demo_cum data set I made is a long data set with all five years of data for
# each participant.Since it is long, it shouldn't be merged with the wide CCMR data
# set. To fix this we need to pick one year of demo data to use as the master reference.
# Based on the email we received, 2019 should have the data for all students. 
# Before we can do this though we need to remove the sex changes, but also find
# anyone who has na in 2019 but a consistent sex in later years.

# Ensure gender values are clean (trim spaces)

demo_cum <- demo_cum %>%
  mutate(idnr = as.character(idnr))  # Change it back to a vector at the end

student_sex <- demo_cum %>%
  distinct(idnr, sex) %>%  # Ensure unique idnr-sex combinations
  group_by(idnr) %>%
  summarise(unique_sex = list(unique(sex)), .groups = "drop") %>%
  filter(all(unique_sex %in% c(NA, "", "M")) & any(unique_sex == "M") & !any(unique_sex == "F") |
           all(unique_sex %in% c(NA, "", "F")) & any(unique_sex == "F") & !any(unique_sex == "M")) %>%
  select(idnr)

# Retrieve full details of filtered students
filtered_demo_cum <- demo_cum %>%
  filter(idnr %in% student_sex$idnr) %>%
  arrange(idnr, year)

View(filtered_demo_cum) # Return 0 observations, so in theory we should just be
                        # able to use the 2019 demo data.

demo_cum <- demo_cum %>%
  mutate(idnr = as.vector(idnr))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Merging Demo and CCMR data

demo_19 <- demo_19 %>%
  rename(id = idnr)

CCMR_demo19_comb <- merge(CCMR_Master, demo_19, by = "id", all.x = TRUE) ### Left join

# The sex vector is still a character because it was taken from the 2019 data set
CCMR_demo19_comb <- CCMR_demo19_comb %>%
  mutate(sex = as.factor(sex))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Courses Data

courses_19 <- read.csv("2019 Courses.csv", header = TRUE)

# Examining course_19 data for missings

str(courses_19)
length(unique(courses_19$idnr))

missing_id_courses_19 <- courses_19 %>%
  filter(if_all(-idnr, is.na)) %>%
  select(idnr) 

View(missing_id_courses_19)#None - yay!

#remove unnecessary columns

courses_19 <- courses_19 %>%
  select(idnr, GRADE, crs_area, CRSNUM, crsname)

str(courses_19)
table(courses_19$GRADE)#The grade data does not make sense, but we aren't really using this
table(courses_19$crs_area)#note that there seems to be both "Fine arts"and "Fine Arts courses" categories

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Create Music Course List

unique_courses_19 <- courses_19 %>%
  distinct(crsname, CRSNUM) %>%
  arrange(crsname)

print(unique_courses_19)

music_courses_list_19 <- read.csv("music_courses_filtered_2019.csv", header = TRUE)

# ensure columns are numeric
music_courses_list_19$crs_row_num <- as.integer(music_courses_list_19$crs_row_num)
music_courses_list_19$CRSNUM <- as.integer(music_courses_list_19$CRSNUM)

# Check work
View(music_courses_list_19)

# For Reference, music course categories
## indicator columns
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
#     4 classes labeled Chamber Ensemble, could be band/choir/orch I chose band
#     I put conducting in other
#     I put hand bells in Other
#     I put Mariachi in Orchestra
#     I put music business in other
#     I put music history in appreciation
#     I put music production in Modern Band, feel free to change it
#     There was a "Partner Band Coach" I left this in Band category but we may change it
#     I put world music ensembles in other

# Add the music_indicator column to the original course list by matching CRSNUM
courses_19 <- merge(courses_19, 
                      music_courses_list_19[, c("CRSNUM", "music_indicator")], 
                      by = "CRSNUM", 
                      all.x = TRUE)

# View the updated dataframe
View(courses_19)

# Change NA music_indicator values to 0
courses_19 <- courses_19 %>%
  mutate(music_indicator = ifelse(is.na(music_indicator), 0, music_indicator))

# Verify changes
table(courses_19$music_indicator)

### ^This Counts the number of students in each Category
#     There are...
#           3560 Band
#           1432 Choir
#           504 Orchestra
#           776 Modern Band
#           677 Music Theory
#           251 Jazz
#           3558 Music Appreciation
#           40 Guitar
#           108 Other
#           97 Piano
###COMEBACK TO THIS THE NUMBERS CHANGED? I THINK ITS BECAUSE I HAVENT REMOVED MISSING IDS YET
###line 421 it seems i overwrote CCMR-demo combined with just the sex vector..
### Combine Courses_data and combined_demo before analysis

# Combine CCMR Demo and Courses 
courses_19 <- courses_19 %>%
  rename(id = idnr)

courses_19 <- courses_19 %>%
  mutate(id = as.character(id))

cumulative_master_19 <- courses_19 %>%
  left_join(CCMR_demo19_comb, by = "id")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Double check merged data sets
head(cumulative_master_19)

str(cumulative_master_19)

length(unique(cumulative_master_19$id))
length(cumulative_master_19$id)

# look for any missentry in id
sum(is.na(cumulative_master_19$id) | cumulative_master_19$id == "" | trimws(cumulative_master_19$id) == "" | cumulative_master_19$id == ".", na.rm = TRUE)

# double check no id's are missing data
missing_id_cumulative_19 <- cumulative_master_19 %>%
  filter(if_all(-id, is.na)) %>%
  select(id) 

View(missing_id_cumulative_19)

#look for missing demographics to double check our merging
levels(cumulative_master_19$ELL_cum)
levels(cumulative_master_19$IEP_cum)
levels(cumulative_master_19$Race_cum)
levels(cumulative_master_19$sex)
levels(cumulative_master_19$Eco_Dis_Cum)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Earlier we identified two populations that need to be removed from all of the data

# 55 students with errors/changes in sex
cumulative_master_19 <- cumulative_master_19 %>%
  filter(!id %in% sex_change_details$id)

# Verify only 55 students were dropped
length(unique(cumulative_master_19$id))#43,002 - 42,947 = 55


# there are more students with missing sex
missing_master_sex <- cumulative_master_19 %>%
  filter(is.na(sex) | sex == " ")
length(unique(missing_master_sex$id))

#look for trends in the 4119 missing sex students
table(missing_master_sex$GRADE)
table(missing_master_sex$campus_name)
#compare to total campus numbers
table(cumulative_master_19$campus_name)# seems random

#pull demographic data for the students with missing sex from the remaining demo years
missing_master_sex <- missing_master_sex %>%
  rename(idnr = id)

missing_master_sex <- missing_master_sex %>%
  mutate(idnr = as.numeric(idnr))

#join 19x20
missing_sexXyear <- missing_master_sex %>%
  inner_join(demo_20, by = "idnr")

missing_sexXyear <- missing_sexXyear %>%
  rename(sex19 = sex.x, sex20 = sex.y)

missing_sexXyear <- missing_sexXyear %>%
  select(idnr, sex19, sex20)

#add 21
demo_21_sex <- demo_21 %>%
  select(idnr, sex) %>%
  rename(sex21 = sex)

missing_sexXyear <- missing_sexXyear %>%
  inner_join(demo_21_sex, by = "idnr")

#add 22
demo_22_sex <- demo_22 %>%
  select(idnr, sex) %>%
  rename(sex22 = sex)

missing_sexXyear <- missing_sexXyear %>%
  inner_join(demo_22_sex, by = "idnr")

#add 23
demo_23_sex <- demo_23 %>%
  select(idnr, sex) %>%
  rename(sex23 = sex)

missing_sexXyear <- missing_sexXyear %>%
  inner_join(demo_23_sex, by = "idnr")

View(missing_sexXyear)
#at a glance these seem to be a combination of only one sex and missings
# We will double check for sex changes, but if there are no sex changes (which
#   there shouldn't be since we removed them) then we can coalesce these years.

students_with_MF_dblcheck <- missing_sexXyear %>%
  rowwise() %>%
  filter(any(c(sex19, sex20, sex21, sex22, sex23) == "M") & 
           any(c(sex19, sex20, sex21, sex22, sex23) == "F")) %>%
  ungroup()

View(students_with_MF_dblcheck)  # nothing!

#coalesce columns
missing_sexXyear_coalesced <- missing_sexXyear %>%
  # Convert empty spaces to NA before coalescing
  mutate(across(starts_with("sex"), ~na_if(trimws(.), ""))) %>%
  mutate(sex = coalesce(sex19, sex20, sex21, sex22, sex23)) %>%
  select(idnr, sex)

# Check for remaining NA values
sum(is.na(missing_sexXyear_coalesced$sex))
View(missing_sexXyear_coalesced)#Of the 13,019 originally missing sex I was able
                                #to find sex for all but 1100 of them. THIS IS LONG

missing_sexXyear_coalesced <- missing_sexXyear_coalesced %>%
  rename(id = idnr)

#Condense the long data set
missing_sexXyear_coalesced <- missing_sexXyear_coalesced %>%
  group_by(id) %>%
  summarise(sex = first(sex), .groups = "drop")

# The table below shows that there were 4132 students with missing sex in 2019.
# I was able to find consistent sex values for 3866 of them leaving blanks for
# 266. These 266 will be removed.
table(missing_sexXyear_coalesced$sex)

#Move the 266 NA sex students to a seperate df
missing_sex_ids <- missing_sexXyear_coalesced %>%
  filter(is.na(sex)) %>%
  distinct(id)

#Drop the 266 from cumulative_master_19 and missing_sexXyear_coalesced
missing_sexXyear_coalesced <- missing_sexXyear_coalesced %>%
  filter(!id %in% missing_sex_ids$id)

cumulative_master_19 <- cumulative_master_19 %>%
  filter(!id %in% missing_sex_ids$id)

#impute the updated 3866 students sex into cumulative_master_19
#this feels tricky, I'm making a back up first...glad I did...
cumulative_master_19_bckup <- cumulative_master_19

missing_sexXyear_coalesced <- missing_sexXyear_coalesced %>%
  mutate(id = as.character(id))

table(cumulative_master_19$sex)

cumulative_master_19 <- cumulative_master_19 %>%
  mutate(sex = na_if(trimws(sex), "")) %>%
  left_join(missing_sexXyear_coalesced %>%
  select(id, sex), by = "id", suffix = c("", "_new")) %>%
  mutate(sex = coalesce(sex, sex_new)) %>%
  select(-sex_new)

#Double check it worked
#   subtract the sum of the table below from the sum of the table before the mutation
#     (211006 + 194316) - (11919 + 204931 + 188472) = 0
# 0 means it worked
table(cumulative_master_19$sex)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 97 students without id numbers
missing_master_id_count <- cumulative_master_19 %>%
  filter(is.na(id) | id == "") %>%
  summarise(count = n())
print(missing_master_id_count)#the 97 disapeared?
## The 97 students with missing id's were dropped when I used a left join with
#     the courses_19 and the CCMR_demo_comb, since left join preserves the rows
#     of the left arguement and the 97 students were not in the course data, they
#     were dropped
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Main Analysis : Data Prep

##Prep data with binary indicators for each category

# Create a binary column for overall music enrollment
cumulative_master_19$music_binary <- ifelse(cumulative_master_19$music_indicator >= 1, 1, 0)

# Create a binary column for Band
cumulative_master_19$band_binary <- ifelse(cumulative_master_19$music_indicator == 1, 1, 0)

# Create a binary column for Choir
cumulative_master_19$choir_binary <- ifelse(cumulative_master_19$music_indicator == 2, 1, 0)

# Create a binary column for Orchestra
cumulative_master_19$orch_binary <- ifelse(cumulative_master_19$music_indicator == 3, 1, 0)

# Create a binary column for Modern Band
cumulative_master_19$modband_binary <- ifelse(cumulative_master_19$music_indicator == 4, 1, 0)

# Create a binary column for Music Theory
cumulative_master_19$mut_binary <- ifelse(cumulative_master_19$music_indicator == 5, 1, 0)

# Create a binary column for Jazz
cumulative_master_19$jazz_binary <- ifelse(cumulative_master_19$music_indicator == 6, 1, 0)

# Create a binary column for Music Appreciation
cumulative_master_19$musicapp_binary <- ifelse(cumulative_master_19$music_indicator == 7, 1, 0)


## The data set was converted to a long data set when merged with courses. 
#     Convert back to student level data while preserving binary indicators

cum_data_for_analysis <- cumulative_master_19 %>%
  group_by(id) %>%
  summarise(
    ELL_cum = first(ELL_cum),
    IEP_cum = first(IEP_cum),
    Race_cum = first(Race_cum),
    sex = first(sex),
    Eco_Dis_Cum = first(Eco_Dis_Cum),
    
    music_binary = max(music_binary),
    band_binary = max(band_binary),
    choir_binary = max(choir_binary),
    orch_binary = max(orch_binary),
    modband_binary = max(modband_binary),
    mut_binary = max(mut_binary),
    jazz_binary = max(jazz_binary),
    musicapp_binary = max(musicapp_binary)
  ) %>%
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Music Enrollment Model
# many assumptions tests in R like to use the models after they have been run so we
#   will run the regressions first then return to check assumptions

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Music Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Set "White" as the reference level for Race
cum_data_for_analysis$Race_cum <- relevel(cum_data_for_analysis$Race_cum, ref = "White")

# Set "Male" as the reference level for Sex
cum_data_for_analysis$sex <- factor(cum_data_for_analysis$sex)
cum_data_for_analysis$sex <- relevel(cum_data_for_analysis$sex, ref = "M")

# Run logistic regression model
Music_Enrollment_Analysis <- glm(music_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                 data = cum_data_for_analysis, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Music_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

library(car)
vif(Music_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(Music_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(Music_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions
table(cum_data_for_analysis$music_binary)
prop.table(table(cum_data_for_analysis$music_binary))

##Model Fit, Goodness of Fit Test
library(ResourceSelection)

hoslem.test(Music_Enrollment_Analysis$y, fitted(Music_Enrollment_Analysis), g = 9)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Band Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Band Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Band_Enrollment_Analysis <- glm(band_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                 data = cum_data_for_analysis, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Band_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Band_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(Band_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(Band_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$band_binary)
prop.table(table(cum_data_for_analysis$band_binary))
#Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(Band_Enrollment_Analysis$y, fitted(Band_Enrollment_Analysis), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Choir Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Choir Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Choir_Enrollment_Analysis <- glm(choir_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(Choir_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Choir_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(Choir_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(Choir_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$band_binary)
prop.table(table(cum_data_for_analysis$band_binary))
#Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(Choir_Enrollment_Analysis$y, fitted(Choir_Enrollment_Analysis), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Orchestra Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Orchestra Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Orchestra_Enrollment_Analysis <- glm(orch_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(Orchestra_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Orchestra_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(Orchestra_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(Orchestra_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$orch_binary)
prop.table(table(cum_data_for_analysis$orch_binary))
# Very Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(Orchestra_Enrollment_Analysis$y, fitted(Orchestra_Enrollment_Analysis), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Very Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Modern Band Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Modern Band Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
ModBand_Enrollment_Analysis <- glm(modband_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(ModBand_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(ModBand_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(ModBand_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(ModBand_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$modband_binary)
prop.table(table(cum_data_for_analysis$modband_binary))
# Very Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(ModBand_Enrollment_Analysis$y, fitted(ModBand_Enrollment_Analysis), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Music Theory Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Music Theory Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
MusicTheory_Enrollment_Analysis <- glm(mut_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(MusicTheory_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(MusicTheory_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(MusicTheory_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(MusicTheory_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$mut_binary)
prop.table(table(cum_data_for_analysis$mut_binary))
#Very Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(MusicTheory_Enrollment_Analysis$y, fitted(MusicTheory_Enrollment_Analysis), g = 7)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Very Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Jazz Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Jazz Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Jazz_Enrollment_Analysis <- glm(jazz_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(Jazz_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Jazz_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(Jazz_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(Jazz_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$jazz_binary)
prop.table(table(cum_data_for_analysis$jazz_binary))
# Possibly not even viable

##Model Fit, Goodness of Fit Test

hoslem.test(Jazz_Enrollment_Analysis$y, fitted(Jazz_Enrollment_Analysis), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## surprisingly its not as bad as most of the others

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Music Appreciation Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Music Appreciation Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
MusicAppreciation_Enrollment_Analysis <- glm(musicapp_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(MusicAppreciation_Enrollment_Analysis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(MusicAppreciation_Enrollment_Analysis)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(MusicAppreciation_Enrollment_Analysis)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(MusicAppreciation_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis$musicapp_binary)
prop.table(table(cum_data_for_analysis$musicapp_binary))
#Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(MusicAppreciation_Enrollment_Analysis$y, fitted(MusicAppreciation_Enrollment_Analysis), g = 9)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Extracting Main Analysis Results to CSV

# List of saved regression models in your environment
model_list <- list(
  Music_Enrollment = Music_Enrollment_Analysis,
  Band_Enrollment = Band_Enrollment_Analysis,
  Choir_Enrollment = Choir_Enrollment_Analysis,
  Orchestra_Enrollment = Orchestra_Enrollment_Analysis,
  Jazz_Enrollment = Jazz_Enrollment_Analysis,
  MusicTheory_Enrollment = MusicTheory_Enrollment_Analysis,
  MusicAppreciation_Enrollment = MusicAppreciation_Enrollment_Analysis,
  ModBand_Enrollment = ModBand_Enrollment_Analysis
)

# Function to extract model summaries
extract_enrollment_model_results <- function(model_name, model) {
  summary_data <- summary(model)
  coef_data <- as.data.frame(coef(summary_data))
  coef_data$Predictor <- rownames(coef_data)
  coef_data$Model <- model_name
  
  # Reorder columns for better presentation
  coef_data <- coef_data[, c("Model", "Predictor", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
  
  return(coef_data)
}

# Apply function to all models and combine into one dataframe
all_enrollmentXdemographic_results <- do.call(rbind, lapply(names(model_list), function(name) {
  extract_enrollment_model_results(name, model_list[[name]])
}))

# View the combined results
head(all_enrollmentXdemographic_results)

# Adding a column which interprets the log-odds as Odd Ratios
all_enrollmentXdemographic_results$Odds_Ratio <- exp(all_enrollmentXdemographic_results$Estimate)

# Verify the new column
head(all_enrollmentXdemographic_results)

# Export to a CSV file
write.csv(all_enrollmentXdemographic_results, "EnrollmentXDemographic_Analysis_Results_V2.csv", row.names = FALSE)
