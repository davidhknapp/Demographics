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
#     between years. If notm we can just use the 2019 demo data as the master set

#load data
demo_19 <- read.csv("2019 Demo.csv", header = TRUE)
demo_20 <- read.csv("2020 Demo.csv", header = TRUE)
demo_21 <- read.csv("2021 Demo.csv", header = TRUE)
demo_22 <- read.csv("2022 Demo.csv", header = TRUE)
demo_23 <- read.csv("2023 Demo.csv", header = TRUE)

#select only the neccessary data
# Ensure all datasets have the same structure, and make colnames match
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
CCMR_demo19_comb <- as.factor(CCMR_demo19_comb$sex)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Courses Data
