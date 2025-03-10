### THIS SCRIPT IS FOR INITIAL DATA EXPLORATION OF DATA FROM REQUEST 2 ###

#Overall notes
##I have note done a DEEP exploration of all data, only a few from each set as
          #it will take me quite a while to get through all 36 data sets
#   CCMR data is only missing because it doesn't exist
#   all of our demo data should probably come from the demo sets now to be consistent
#   We don't have 2324 data, may inquire into its availability as well as then 24-25 will be available.
#   We years_lep is missing in demo_1819
#   I'm unsure if demo$tea is the name of the school for a student or where they took the test?
      #best I can tell, demo$tea and demo$teastr are the same thing, although tea is labeled, but neither has a label
      # for "997" is withdrawals? ' - apparently there is no missing data in tea? but the school sizes seem too small?
#   (around line 200) - CRS data does not seem to match the same numbers as demo data???
#       CRS and demo grade data doesn't match entirely???? this is consistant in all data sets
# Course data has room #'s and periods which is important for teacher matchings
# ~r250 examining table$tea for crs and demo data reveals differences in school enrollments,
#     one possible explanation is transfer/moving?
            # this might be explained by the 3368 students in 1516 who have different school codes in crs vs demo
            # I'm still seeing just over 10k students in 1516 without school code or grade (not the same amount of students)
            # this could be caused by the separation of semester 1 and 2 data sets? haven't explore that yet
            # it is also possible that the data was removed by my collapsing the data to 1 row perstudent, this takes
                  # the first row of each student and gets rid of the rest, but if information was full an correct
                  # then that tea and grade code should exist for every row of the student and this shouldn't be the problem
#   we don't have the same amount of final exam/ quarter finals in every data set
        # see glimpse(crs_1516_drq2) vs glimpse(crs_1819_drq2)
# ~r400sh : gpa data is very strange, mostly only high schoolers, but about 12% of freshmen missing gpas (give or take based on year)
      # the real weird part is that we have more student id's in gpa than in the demographic files, and when you
      # match the gpa and demo files, you find all the 8th grade and below students are missing gpas, but
      # we have tens of thousands of student id's and gpa remaining that do not match those students
# missing roughly 5 - 7% of students attendance data in 1516, havent been able to check all years
#     evenly distributed missings across all grades


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### LIBRARIES
library(dplyr)
library(tidyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Working Directory
setwd("/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw/data_rq2/Dallas No. 2/March 2025")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CO TEA CCMR Student Listing Files

# There is no new CCMR data sicne we have all of the CCMR data which exists, with the exception of 2023-2024, 
#   we should also inquire as to when 2024-2025 will be available

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEMO DATA

#   The Demo Data is to big for my version of stata to run
#install.packages("haven") to read sav files
library(haven)

# 1516
demo_1516_drq2 <- read_sav("demo_1516.sav")

str(demo_1516_drq2)
glimpse(demo_1516_drq2)

length((unique(demo_1516_drq2$idnr)))

table(demo_1516_drq2$gradenum)
attr(demo_1516_drq2$gradenum, "labels")
# contains students EC, PK KN
demo_1516_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))
#Demo data seems to be complete on students EC - 12

attr(demo_1516_drq2$lang_s3, "label")
attr(demo_1516_drq2$lang_s2, "label")

table(demo_1516_drq2$lang_s2)

attr(demo_1516_drq2$ProgramCode, "labels")
table(demo_1516_drq2$ProgramCode)

attr(demo_1516_drq2$bil_prog, "labels")

table(demo_1516_drq2$sex)
demo_1516_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))

table(demo_1516_drq2$tea)
demo_1516_drq2 %>%
  summarise(missing_count = sum(is.na(tea)))

#1617

demo_1617_drq2 <- read_sav("demo_1617.sav")

glimpse(demo_1617_drq2)

length(unique(demo_1617_drq2$idnr))

table(demo_1617_drq2$gradenum)
attr(demo_1617_drq2$gradenum, "labels")#no val lab for -3?
demo_1617_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_1617_drq2$sex)
demo_1617_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))

# 1718

demo_1718_drq2 <- read_sav("demo_1718.sav")

glimpse(demo_1718_drq2)
### NEW VARIABLE SEX 504
attr(demo_1718_drq2$sec504, "label")
attr(demo_1718_drq2$sec504, "labels")
# special disability act protection, not sure why it was included here but not prior as its from the 1970's
table(demo_1718_drq2$sec504)

length(unique(demo_1718_drq2$idnr))

table(demo_1718_drq2$gradenum)
attr(demo_1718_drq2$gradenum, "labels")#no val lab for -3?
demo_1718_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_1718_drq2$sex)
demo_1718_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))

# 1819

demo_1819_drq2 <- read_sav("demo_1819.sav")

glimpse(demo_1819_drq2)

length(unique(demo_1819_drq2$idnr))
# one var is missing
colnames(demo_1516_drq2)
colnames(demo_1819_drq2)
#MISSING YEARS LEP

table(demo_1819_drq2$gradenum)
attr(demo_1819_drq2$gradenum, "labels")#no val lab for -3?
demo_1819_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_1819_drq2$sex)
demo_1819_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))
attr(demo_1819_drq2$sex, "labels")
#suggests 3 " " which may be missentry

# 1920

demo_1920_drq2 <- read_sav("demo_1920.sav")

glimpse(demo_1920_drq2)

length(unique(demo_1920_drq2$idnr))

table(demo_1920_drq2$gradenum)
demo_1920_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_1920_drq2$sex)
demo_1920_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))

# 2021

demo_2021_drq2 <- read_sav("demo_2021.sav")

glimpse(demo_2021_drq2)

length(unique(demo_2021_drq2$idnr))

table(demo_2021_drq2$gradenum)
demo_2021_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_2021_drq2$sex)
demo_2021_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))

# 2122

demo_2122_drq2 <- read_sav("demo_2122.sav")

glimpse(demo_2122_drq2)

length(unique(demo_2122_drq2$idnr))

table(demo_2122_drq2$gradenum)
demo_2122_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_2122_drq2$sex)
demo_2122_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))
# probably 4 missentry sex

# 2223

demo_2223_drq2 <- read_sav("demo_2223.sav")

glimpse(demo_2223_drq2)

length(unique(demo_2223_drq2$idnr))

table(demo_2223_drq2$gradenum)
demo_2223_drq2 %>%
  summarise(missing_count = sum(is.na(gradenum)))

table(demo_2223_drq2$sex)
demo_2223_drq2 %>%
  summarise(missing_count = sum(is.na(sex)))
# proabbly 6 miss entry

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Course Data

#1516 

crs_1516_drq2 <- read_sav("SEC_CRS_1516_SEM_1.sav")
glimpse(crs_1516_drq2)
# collapsing just to look at counts
crs_1516_drq2 <- crs_1516_drq2 %>% distinct(idnr, .keep_all = TRUE)

glimpse(crs_1516_drq2)
length(unique(crs_1516_drq2$idnr))
table(crs_1516_drq2$grade)
# double check against 1516 demo data
table(demo_1516_drq2$gradenum)
# why do these not match??

demo_1516_check <- demo_1516_drq2 %>%
  select(idnr, gradenum)

demo_1516_check <- demo_1516_check %>%
  rename(grade_demo = gradenum)

crs_1516_check <- crs_1516_drq2 %>%
  select(idnr, grade)

crs_1516_check <- crs_1516_check %>%
  rename(grade_crs = grade)

demo_vs_crs_1516 <- demo_1516_check %>%
  full_join(crs_1516_check, by = "idnr")
  
demo_vs_crs_1516 <- demo_vs_crs_1516 %>%
  filter(grade_demo >= 6)

demo_vs_crs_1516_missentry <- demo_vs_crs_1516 %>%
  filter(!grade_demo == grade_crs)
# shows 187 missentries?

demo_vs_crs_1516_missing <- demo_vs_crs_1516 %>%
  filter(is.na(grade_crs))
# shows 10,123 missings?

# in the data we have now, schools are given a id which is inconsistant in labels, but i can apply it nbd,
  #ASSUMING ITS THE SAME LABEL

table(crs_1516_drq2$tea)
attr(demo_1516_drq2$tea, "labels")#examines labels from demo
table(demo_1516_drq2$tea)
#THERES A DIFFERENCE BETWEEN DEMO AND CRS SCHOOL ENROLLMENTS

#look at them side by side
demo_1516_drq2 <- demo_1516_drq2 %>%
  rename(tea_demo = tea)

demo_1516_drq2_temp <- demo_1516_drq2 %>%
  filter(gradenum >= 6)

crs_1516_drq2 <- crs_1516_drq2 %>%
  rename(tea_crs = tea)

school_inconsistency_1516 <- demo_1516_drq2_temp %>%
  full_join(crs_1516_drq2)

school_inconsistency_1516 <- school_inconsistency_1516 %>%
  select(idnr, tea_demo, tea_crs)

school_inconsistency_1516 %>%
  summarise(
    num_mismatches = sum(tea_demo != tea_crs, na.rm = TRUE),
    num_NA_issues = sum(is.na(tea_demo) | is.na(tea_crs)),
    num_both_NA = sum(is.na(tea_demo) & is.na(tea_crs))
  )

rm(demo_1516_drq2_temp)

# shows 3363 students with miss matching schools id's, 10731 students with no crs school



## 1819
# skipping ahead to where it is not separate by semester to examine for similar problems

crs_1819_drq2 <- read_sav("SEC_CRS_1819.sav")
glimpse(crs_1819_drq2)
# collapsing just to look at counts
crs_1819_drq2 <- crs_1819_drq2 %>% distinct(IDNR, .keep_all = TRUE)

glimpse(crs_1819_drq2)
length(unique(crs_1819_drq2$IDNR))
table(crs_1819_drq2$GRADE)
attr(crs_1819_drq2$GRADE, "labels")# no explanation of the crazies
# double check against 1819 demo data
table(demo_1819_drq2$gradenum)

# Seems to have the same problem as 1516, crs and demo data don't entirely match


crs_2223_dq2 <- read_sav("SEC_CRS_2223.sav")
glimpse(crs_2223_dq2)
# collapsing just to look at counts
crs_2223_drq2 <- crs_2223_drq2 %>% distinct(IDNR, .keep_all = TRUE)
#colnames changes suggesting these are different data sets
glimpse(crs_2223_drq2)
length(unique(crs_2223_drq2$IDNR))
table(crs_2223_drq2$GRADE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GPA Data

gpa_2223_drq2 <- read_sav("gpa_eoy_2223.sav")
glimpse(gpa_2223_drq2)
length(unique(gpa_2223_drq2$idnr))
#45554?

gpa_2122_drq2 <- read_sav("gpa_eoy_2122.sav")
glimpse(gpa_2122_drq2)
length(unique(gpa_2122_drq2$idnr))
#46520?

gpa_2021_drq2 <- read_sav("gpa_eoy_2021.sav")
glimpse(gpa_2021_drq2)
length(unique(gpa_2021_drq2$idnr))
#44146

gpa_1920_drq2 <- read_sav("gpa_eoy_1920.sav")
glimpse(gpa_1920_drq2)
length(unique(gpa_1920_drq2$idnr))
#45019

gpa_1819_drq2 <- read_sav("gpa_eoy_1819.sav")
glimpse(gpa_1819_drq2)
length(unique(gpa_1819_drq2$idnr))
#165590
gpa_1819_drq2 %>%
  summarise(na_gpa = sum(is.na(gpa_1819_eoy)))

gpa_1718_drq2 <- read_sav("gpa_eoy_1718.sav")
glimpse(gpa_1718_drq2)
length(unique(gpa_1718_drq2$idnr))
#282930?? this is more GPAs than people in 1718 demo, no repeats either

gpa_1617_drq2 <- read_sav("gpa_eoy_1617.sav")
glimpse(gpa_1617_drq2)
length(unique(gpa_1617_drq2$idnr))
#270353

gpa_1516_drq2 <- read_sav("gpa_eoy_1516.sav")
glimpse(gpa_1516_drq2)
length(unique(gpa_1516_drq2$idnr))
#256803

#looking for who we have and don't have GPA for
gpa_demo_2223 <- demo_2223_drq2 %>%
  select(idnr, tea, gradenum)

gpa_demo_2223 <- gpa_demo_2223 %>%
  full_join(gpa_2223_drq2)

gpa_missing_2223 <- gpa_demo_2223 %>%
  filter(is.na(gpa_eoy_2223))

glimpse(gpa_missing_2223)
table(gpa_missing_2223$gradenum)
#looks like it is mostly high school data we have, except some are still missin
table(gpa_missing_2223$tea)

# 1819 missings
gpa_demo_1819 <- demo_1819_drq2 %>%
  select(idnr, tea, gradenum)

gpa_demo_1819 <- gpa_demo_1819 %>%
  full_join(gpa_1819_drq2)

gpa_missing_1819 <- gpa_demo_1819 %>%
  filter(is.na(gpa_1819_eoy))

glimpse(gpa_missing_1819)
table(gpa_missing_1819$gradenum)
# doesn't make sens that there are about 40k missing from high school but the data is full for 165k?

# 1718 missings
gpa_1718_drq2 %>%
  summarise(missing = sum(is.na(gpa_1718_eoy)))

gpa_demo_1718 <- demo_1718_drq2 %>%
  select(idnr, tea, gradenum)

gpa_demo_1718 <- gpa_demo_1718 %>%
  full_join(gpa_1718_drq2)

gpa_missing_1718 <- gpa_demo_1718 %>%
  filter(is.na(gpa_1718_eoy))

glimpse(gpa_missing_1718)
table(gpa_missing_1718$gradenum)

# there are still uneven numbers here, gpa has ~250k unique id's
#                                       demo has ~ 160k unique id's
#     the above shows that we have most high school gpas except about 20% of 9th grade
#     if we filter by a variable in demo everyone has such as tea, we can see how many gpa id's were not in the demo
gpa_extras_1718 <- gpa_demo_1718 %>%
  filter(is.na(tea))
glimpse(gpa_extras_1718)
#240k extra gpas

# can we match these left overs to other years?
#match to 2223
gpa1718_demo_2223 <- demo_2223_drq2 %>%
  select(idnr, tea, gradenum)

gpa1718_demo_2223 <- gpa1718_demo_2223 %>%
  rename(tea2223 = tea)

gpa1718_demo_2223 <- gpa1718_demo_2223 %>%
  full_join(gpa_extras_1718)

gpa1718_demo_2223 <- gpa1718_demo_2223 %>%
  filter(is.na(gpa_1718_eoy))

glimpse(gpa1718_demo_2223)
table(gpa1718_demo_2223$gradenum)
# match to 1516
gpa1718_demo_1516 <- demo_1516_drq2 %>%
  select(idnr, tea_demo, gradenum)
glimpse(gpa1718_demo_1516)

gpa1718_demo_1516 <- gpa1718_demo_1516 %>%
  rename(tea1516 = tea_demo)

gpa1718_demo_1516 <- gpa1718_demo_1516 %>%
  full_join(gpa_extras_1718)

gpa1718_demo_1516 <- gpa1718_demo_1516 %>%
  filter(!is.na(gpa_1718_eoy))

glimpse(gpa1718_demo_1516)
table(gpa1718_demo_1516$gradenum)
#no matches

# match to 1617
gpa1718_demo_1617 <- demo_1617_drq2 %>%
  select(idnr, gradenum)
glimpse(gpa1718_demo_1617)#177,036

gpa1718_demo_1617 <- gpa1718_demo_1617 %>%
  full_join(gpa_extras_1718)
glimpse(gpa1718_demo_1617)#417,553 indicating no matches again

gpa1718_demo_1617 <- gpa1718_demo_1617 %>%
  filter(!is.na(gpa_1718_eoy))

glimpse(gpa1718_demo_1617)
table(gpa1718_demo_1617$gradenum)

#match to 1819
gpa1718_demo_1819 <- demo_1819_drq2 %>%
  select(idnr, gradenum)
glimpse(gpa1718_demo_1819)#174,770

gpa1718_demo_1819 <- gpa1718_demo_1819 %>%
  full_join(gpa_extras_1718)
glimpse(gpa1718_demo_1819)#415,287 indicating no matches again

gpa1718_demo_1819 <- gpa1718_demo_1819 %>%
  filter(!is.na(gpa_1718_eoy))

glimpse(gpa1718_demo_1819)
table(gpa1718_demo_1819$gradenum)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attendance Data

attend_2223_drq2 <- read_sav("attend_2223.sav")
glimpse(attend_2223_drq2)
length(unique(attend_2223_drq2$idnr))
# I don't understand how there are so many id's here, 73k is a full 6-12,
#   45k is 9-12 but this data set has 153k unique student id's?
# could be k - 12, there are 159,416 in the demo_2223 and 153,499 in the att_2223

# Join and check if they are the same students
attend_demo_2223check <- demo_2223_drq2 %>%
  select(idnr, tea, gradenum)

attend_demo_2223check <- attend_demo_2223check %>%
  full_join(attend_2223_drq2)

length(unique(attend_demo_2223check$idnr))
# 166140, this indicates of the 159,416 in demo and 153,499 in att, 6724 were not 
#   matched in addition to the 5917 students already in surplus from demo to att

#original
attend_2223_drq2 %>%
  summarise(missingatt = sum(is.na(fall2223_enr)))
# no missings

#post join
attend_demo_2223check %>%
  summarise(missingatt = sum(is.na(fall2223_enr)))
# 12641 students with missing attendance data
# this is representative of the 6724 students in att_2223 and not in demo_2223
#   in addition to the 5917 students in demo_2223 not in att_2223
att_2223_mi <- attend_demo_2223check %>%
  filter(is.na(fall2223_enr))
table(att_2223_mi$gradenum)
# evenly distributed k-12

#2122
att_2122_drq2 <- read_sav("attend_2122.sav")
glimpse(att_2122_drq2)
length(unique(att_2122_drq2$idnr))

#2021
att_2021_drq2 <- read_sav("attend_2021.sav")
glimpse(att_2021_drq2)
length(unique(att_2021_drq2$idnr))

#1920
att_1920_drq2 <- read_sav("attend_1920.sav")
glimpse(att_1920_drq2)
length(unique(att_1920_drq2$idnr))

#1819
att_1819_drq2 <- read_sav("attend_1819.sav")
glimpse(att_1819_drq2)
length(unique(att_1819_drq2$idnr))

#1718
att_1718_drq2 <- read_sav("attend_1718.sav")
glimpse(att_1718_drq2)
length(unique(att_1718_drq2$idnr))

#1617
att_1617_drq2 <- read_sav("attend_1617.sav")
glimpse(att_1617_drq2)
length(unique(att_1617_drq2$idnr))

#1516
att_1516_drq2 <- read_sav("attend_1516.sav")
glimpse(att_1516_drq2)
length(unique(att_1516_drq2$idnr))
