### THIS SCRIPT IS FOR INITIAL DATA EXPLORATION OF DATA FROM REQUEST 2 ###

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### LIBRARIES
library(dplyr)
library(tidyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Working Directory
setwd("/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw/data_rq2/Dallas No. 2")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CO TEA CCMR Student Listing Files

#   These set of data consistes of 5 files. One for each year 2019 - 2023

#~~ see stata.do file for CO TEA CCMR exploration
#     Stata is just better and much easier for exploration

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEMO DATA

#   The Demo Data is to big for my version of stata to run
#install.packages("haven")
library(haven)

demo22_23 <- read_sav("FSU_DEMO_2223.sav")

glimpse(demo22_23)

#install.packages("Hmisc")
library(Hmisc)
describe(demo22_23$idnr)
table(demo22_23$tea)
describe(demo22_23$teastr)#only has 8943 likely just seniors?
table(demo22_23$race)#34059 missing
table(demo22_23$fedEthnicity)#34059 missing
table(demo22_23$sex)#35049 missing
table(demo22_23$LEP)#37411 missing
table(demo22_23$ProgramCode)#37515
table(demo22_23$bil_prog)#37515
table(demo22_23$lang_s2)#34059
table(demo22_23$lang_s3)#34059
describe(demo22_23$years_lep)#39760

setwd("/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw")
demo_19 <- read.csv("2019 Demo.csv", header = TRUE)
demo_20 <- read.csv("2020 Demo.csv", header = TRUE)
demo_21 <- read.csv("2021 Demo.csv", header = TRUE)
demo_22 <- read.csv("2022 Demo.csv", header = TRUE)
demo_23 <- read.csv("2023 Demo.csv", header = TRUE)

describe(demo_23$id)
describe(demo_23$tea)
describe(demo_23$teastr)#only has 8943 likely just seniors?
table(demo22_23$race)#34059 missing
table(demo22_23$fedEthnicity)#34059 missing
table(demo22_23$sex)#35049 missing
table(demo22_23$LEP)#37411 missing
table(demo22_23$ProgramCode)#37515
table(demo22_23$bil_prog)#37515
table(demo22_23$lang_s2)#34059
table(demo22_23$lang_s3)#34059
describe(demo22_23$years_lep)#39760

setwd("/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw/data_rq2/Dallas No. 2")

crs_2223_dq2a <- read_sav("FSU_SEC_CRS_2223.sav")

length(unique(crs_2223_dq2a$idnr))
glimpse(crs_2223_dq2a)
table(crs_2223_dq2a$GRADE)

# collapsing just to look at counts
crs_2223_dq2a <- crs_2223_dq2a %>% distinct(idnr, .keep_all = TRUE)
glimpse(crs_2223_dq2a)
table(crs_2223_dq2a$GRADE)
# all 12th graders

setwd("/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw/data_rq2/Dallas No. 2/March 2025")

crs_2223_dq2b <- read_sav("SEC_CRS_2223.sav")
glimpse(crs_2223_dq2b)
# collapsing just to look at counts
crs_2223_dq2b <- crs_2223_dq2b %>% distinct(IDNR, .keep_all = TRUE)
#colnames changes suggesting these are different data sets
glimpse(crs_2223_dq2b)
length(unique(crs_2223_dq2b$IDNR))
table(crs_2223_dq2b$GRADE)
# FULL DATA WOO
#perhaps its only the data in this new folder

demo_2223_drq2 <- read_sav("demo_2223.sav")
#73,815 = full data wooh
glimpse(demo_2223_drq2)

gpa_2223_drq2 <- read_sav("gpa_eoy_2223.sav")
glimpse(gpa_2223_drq2)

attend_2223_drq2 <- read_sav("attend_2223.sav")
glimpse(attend_2223_drq2)
length(unique(attend_2223_drq2$idnr))
# I don't understand how there are so many id's here, 73k is a full 6-12,
#   45k is 9-12 but this data set has 153k unique student id's?

