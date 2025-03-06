clear all
capture log close

cd "/Users/collinclark/Dallas_Data_workspace/Demorgraphics/data_raw"

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*EXPLORING COURSES DATA


import delimited "FSU_SEC_CRS_SEM1_1516.csv", clear

duplicates r idnr crsname period
* note: this is the level of the dataset

preserve
duplicates drop idnr, force
bysort grade: count
restore 

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "FSU_SEC_CRS_SEM2_1516.csv", clear

duplicates r idnr crsname period

preserve
duplicates drop idnr, force
bysort grade: count
restore 

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "FSU_SEC_CRS_SEM1_1617.csv", clear

duplicates r idnr crsname period
* note quite the right level, but close

preserve
duplicates drop idnr, force
bysort grade: count
restore 

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "FSU_SEC_CRS_SEM2_1617.csv", clear

duplicates r idnr crsname period
*not quite the right level, but close

preserve
duplicates drop idnr, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "FSU_SEC_CRS_SEM1_1718.csv", clear

duplicates r idnr crsname period
*not quite

preserve
duplicates drop idnr, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "FSU_SEC_CRS_SEM2_1718.csv", clear

duplicates r idnr crsname period
*not quite

preserve
duplicates drop idnr, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "2019 Courses.csv", clear

duplicates r idnr crsnum crs_term grade sect statecrsnum crsname

preserve
duplicates drop idnr, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "2020 Courses.csv", clear

duplicates r id crsnum crs_term grade sect statecrsnum crsname
*not it, but real close

preserve
duplicates drop id, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2021 Courses.csv", clear

duplicates r id crsnum
*the only one that is the way it probably should be

preserve
duplicates drop id, force
bysort grade: count
restore 

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clear
import delimited "2022 Courses.csv", clear

duplicates r id crsnum
*also, how it should be

preserve
duplicates drop id, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2023 Courses.csv", clear

duplicates r id crsnum
*also, how it should be

preserve
duplicates drop id, force
bysort grade: count
restore 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*EXPLORING DEMOGRAPHIC DATA

clear
import delimited "2016 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2017 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2018 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2019 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2020 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2021 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2022 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clear
import delimited "2023 Demo.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*EXPLORING CCMR DATA

clear
import delimited "2019 CCMR.csv", clear

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	  
