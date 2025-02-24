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
# and match student ids for the 2020 course enrollment data
# To get CCMR_demo19_cum in your environment please run the code cleaning and 
#merging the data set in 2019_dallas_data_script

courses_20 <- read.csv("2020 Courses.csv", header = TRUE)

# Examining course_20 data for missings

str(courses_20)
length(unique(courses_20$id))

missing_id_courses_23 <- courses_23 %>%
  filter(if_all(-id, is.na)) %>%
  select(id) 

View(missing_id_courses_23)#None - yay!

# removed students with missin course info
courses20_unique_ids_2.0 <- courses_20 %>%
  filter(!(is.na(CRSNUM) | CRSNUM == ""))

length(unique(courses20_unique_ids_2.0$id))
# 33137

courses_20 <- courses20_unique_ids_2.0

#remove non high schoolers

courses_20 <- courses_20 %>%
  filter(GRADE %in% c(9, 10, 11, 12))

#remove unnecessary columns

courses_20 <- courses_20 %>%
  select(id, GRADE, crs_area, CRSNUM, crsname)

str(courses_20)
table(courses_20$GRADE)#The grade data does not make sense, but we aren't really using this
table(courses_20$crs_area)# There are 799 Uncategorized course entries
length(unique(courses_20$id))#33038 valid student observations remaining

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Create Music Course List and Adding Music Course Indicators

unique_courses_20 <- courses_20 %>%
  distinct(crsname, CRSNUM) %>%
  arrange(crsname)

View(unique_courses_20)

write.csv(unique_courses_20, "unique_courses_filtered_2020.csv", row.names = FALSE)
# export the unique_courses_20 to excel to manually clean 

music_courses_list_20 <- read.csv("music_courses_filtered_2020_v2.csv", header = TRUE)

# ensure columns are numeric
music_courses_list_20$CRSNUM <- as.integer(music_courses_list_20$CRSNUM)

# Check work
View(music_courses_list_20)

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
#     "Chamber ensemble" was allocated to band to be consisted with other analysis
#     music production was placed in modern band to be consistant
#     handbells was again placed in other
#     conducting was placed in other
#     composition was placed in theory
#     world music mesitersingers was placed in choir
#     music and media was placed in other
#     mariachi was placed in orchestra to be consistant

# Add the music_indicator column to the original course list by matching CRSNUM
courses_20 <- merge(courses_20, 
                    music_courses_list_20[, c("CRSNUM", "music_indicator")], 
                    by = "CRSNUM", 
                    all.x = TRUE)

# View the updated dataframe
View(courses_20)

# Change NA music_indicator values to 0
courses_20 <- courses_20 %>%
  mutate(music_indicator = ifelse(is.na(music_indicator), 0, music_indicator))

# Verify changes
table(courses_20$music_indicator)

### ^This Counts the number of students in each Category
#                   (note: this assumes 1 enrollment per student 
#                   in each category which may be incorrect for some students 
#                   since the data set is long)
#     There are...
#           3821 Band
#           308 Choir
#           503 Orchestra
#           773 Modern Band
#           513 Music Theory
#           242 Jazz
#           3551 Music Appreciation
#           40 Guitar
#           88 Other
#           90 Piano

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Combine Courses_data and CCMR_demo19_comb to cumaltive_master_20 before analysis

cumulative_master_20 <- courses_20 %>%
  left_join(CCMR_demo19_comb, by = "id")

str(cumulative_master_20)

length(unique(cumulative_master_20$id))

table(cumulative_master_20$GRADE)

# look for any missentry in id
sum(is.na(cumulative_master_20$id) | cumulative_master_20$id == "" | trimws(cumulative_master_20$id) == "" | cumulative_master_20$id == ".", na.rm = TRUE)

#look for missing demographics to double check our merging
levels(cumulative_master_20$ELL_cum)
levels(cumulative_master_20$IEP_cum)
levels(cumulative_master_20$Race_cum)
levels(cumulative_master_20$sex)
levels(cumulative_master_20$Eco_Dis_Cum)

#sex is still uncleaned, I think I cleaned it in the cumulative set during the 19_script
#   we can use the missing_sex data sets formed in the 19_script to remove them here too

cumulative_master_20 <- cumulative_master_20 %>%
  filter(!id %in% missing_sex_ids$id)

#Observe amount of students with sex data that can be coalesced and imputed
table(cumulative_master_20$sex)

cumulative_master_20 <- cumulative_master_20 %>%
  mutate(sex = na_if(trimws(sex), "")) %>%
  left_join(missing_sexXyear_coalesced %>%
              select(id, sex), by = "id", suffix = c("", "_new")) %>%
  mutate(sex = coalesce(sex, sex_new)) %>%
  select(-sex_new)

#Verify sex is now 2 level factor
table(cumulative_master_20$sex)
# verify the sum of observations remains the same before imputing coalesced values
(28949 + 168577 + 153221) - (183861 + 166761) #before - after
# at first I was concerned that the values not == 0 meant we did something wrong 
# but it is actually fine. The 125 unchanged values represent people who had no sex in 19 but did have it in 20
# and were consistant and therefore coalescable 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Main Analysis : Data Prep

##Prep data with binary indicators for each category

# Create a binary column for overall music enrollment
cumulative_master_20$music_binary <- ifelse(cumulative_master_20$music_indicator >= 1, 1, 0)

# Create a binary column for Band
cumulative_master_20$band_binary <- ifelse(cumulative_master_20$music_indicator == 1, 1, 0)

# Create a binary column for Choir
cumulative_master_20$choir_binary <- ifelse(cumulative_master_20$music_indicator == 2, 1, 0)

# Create a binary column for Orchestra
cumulative_master_20$orch_binary <- ifelse(cumulative_master_20$music_indicator == 3, 1, 0)

# Create a binary column for Modern Band
cumulative_master_20$modband_binary <- ifelse(cumulative_master_20$music_indicator == 4, 1, 0)

# Create a binary column for Music Theory
cumulative_master_20$mut_binary <- ifelse(cumulative_master_20$music_indicator == 5, 1, 0)

# Create a binary column for Jazz
cumulative_master_20$jazz_binary <- ifelse(cumulative_master_20$music_indicator == 6, 1, 0)

# Create a binary column for Music Appreciation
cumulative_master_20$musicapp_binary <- ifelse(cumulative_master_20$music_indicator == 7, 1, 0)

str(cumulative_master_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## The data set was converted to a long data set when merged with courses. 
#     Convert back to student level data while preserving binary indicators

cum_data_for_analysis_20 <- cumulative_master_20 %>%
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

str(cum_data_for_analysis_20)

# sex is no longer a factor? not sure where that happened
cum_data_for_analysis_20 <- cum_data_for_analysis_20 %>%
  mutate(sex = as.factor(sex))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Set reference levels for regression
# Set "White" as the reference level for Race
cum_data_for_analysis_20$Race_cum <- relevel(cum_data_for_analysis_20$Race_cum, ref = "White")

# Set "Male" as the reference level for Sex
cum_data_for_analysis_20$sex <- relevel(cum_data_for_analysis_20$sex, ref = "M")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Main Analysis Music Enrollment Model
# many assumptions tests in R like to use the models after they have been run so we
#   will run the regressions first then return to check assumptions
# although as we discussed, we are really only concerned with collinearity

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Music Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Music_Enrollment_Analysis_20 <- glm(music_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                 data = cum_data_for_analysis_20, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Music_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

library(car)
vif(Music_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
library(pscl)
pR2(Music_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence_measures_20 <- influence.measures(Music_Enrollment_Analysis_20)
print(influence_measures)

# Plot Cook's Distance
plot(cooks.distance(Music_Enrollment_Analysis), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions
table(cum_data_for_analysis_20$music_binary)
prop.table(table(cum_data_for_analysis_20$music_binary))

##Model Fit, Goodness of Fit Test
library(ResourceSelection)

hoslem.test(Music_Enrollment_Analysis_20$y, fitted(Music_Enrollment_Analysis_20), g = 9)
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
Band_Enrollment_Analysis_20 <- glm(band_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis_20, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(Band_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Band_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
pR2(Band_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence.measures(Band_Enrollment_Analysis_20)

# Plot Cook's Distance
plot(cooks.distance(Band_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$band_binary)
prop.table(table(cum_data_for_analysis_20$band_binary))
#Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(Band_Enrollment_Analysis_20$y, fitted(Band_Enrollment_Analysis_20), g = 6)
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
Choir_Enrollment_Analysis_20 <- glm(choir_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                 data = cum_data_for_analysis_20, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Choir_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Choir_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
pR2(Choir_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence.measures(Choir_Enrollment_Analysis_20)


# Plot Cook's Distance
plot(cooks.distance(Choir_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$choir_binary)
prop.table(table(cum_data_for_analysis_20$choir_binary))
#Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(Choir_Enrollment_Analysis_20$y, fitted(Choir_Enrollment_Analysis_20), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## I think this is the first one we've passed

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Orchestra Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Orchestra Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Orchestra_Enrollment_Analysis_20 <- glm(orch_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                     data = cum_data_for_analysis_20, 
                                     family = binomial(link = "logit"))

# Summarize the model results
summary(Orchestra_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Orchestra_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

##Testing for outliers usings cook's distance
influence.measures(Orchestra_Enrollment_Analysis_20)

#McFaddens R^2 assessing model fit
pR2(Orchestra_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

# Plot Cook's Distance
plot(cooks.distance(Orchestra_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$orch_binary)
prop.table(table(cum_data_for_analysis_20$orch_binary))
# Very Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(Orchestra_Enrollment_Analysis_20$y, fitted(Orchestra_Enrollment_Analysis_20), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## We have a good fit?!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Main Analysis Modern Band Enrollment Model

### Multivariable Binary Logistic Regression for
#       Outcome Variable : Modern Band Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
ModBand_Enrollment_Analysis_20 <- glm(modband_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                   data = cum_data_for_analysis_20, 
                                   family = binomial(link = "logit"))

# Summarize the model results
summary(ModBand_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(ModBand_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
pR2(ModBand_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence.measures(ModBand_Enrollment_Analysis_20)

# Plot Cook's Distance
plot(cooks.distance(ModBand_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$modband_binary)
prop.table(table(cum_data_for_analysis_20$modband_binary))
# Very Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(ModBand_Enrollment_Analysis_20$y, fitted(ModBand_Enrollment_Analysis_20), g = 8)
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
MusicTheory_Enrollment_Analysis_20 <- glm(mut_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                       data = cum_data_for_analysis_20, 
                                       family = binomial(link = "logit"))

# Summarize the model results
summary(MusicTheory_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(MusicTheory_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
pR2(MusicTheory_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence_measures <- influence.measures(MusicTheory_Enrollment_Analysis_20)

# Plot Cook's Distance
plot(cooks.distance(MusicTheory_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$mut_binary)
prop.table(table(cum_data_for_analysis_20$mut_binary))
#Very Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(MusicTheory_Enrollment_Analysis_20$y, fitted(MusicTheory_Enrollment_Analysis_20), g = 7)
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
Jazz_Enrollment_Analysis_20 <- glm(jazz_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                data = cum_data_for_analysis_20, 
                                family = binomial(link = "logit"))

# Summarize the model results
summary(Jazz_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(Jazz_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
pR2(Jazz_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence.measures(Jazz_Enrollment_Analysis_20)

# Plot Cook's Distance
plot(cooks.distance(Jazz_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$jazz_binary)
prop.table(table(cum_data_for_analysis_20$jazz_binary))
# Possibly not even viable

##Model Fit, Goodness of Fit Test

hoslem.test(Jazz_Enrollment_Analysis_20$y, fitted(Jazz_Enrollment_Analysis_20), g = 8)
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
MusicAppreciation_Enrollment_Analysis_20 <- glm(musicapp_binary ~ Race_cum + sex + Eco_Dis_Cum + ELL_cum + IEP_cum, 
                                             data = cum_data_for_analysis_20, 
                                             family = binomial(link = "logit"))

# Summarize the model results
summary(MusicAppreciation_Enrollment_Analysis_20)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assumptions

## Multicollinearity

vif(MusicAppreciation_Enrollment_Analysis_20)
#   VIF > 5 or 10 → High multicollinearity (consider removing or combining variables).
#   VIF < 5 → No serious multicollinearity issues.
#We passed the test

#McFaddens R^2 assessing model fit
pR2(MusicAppreciation_Enrollment_Analysis_20)
#R² > 0.2 → Decent fit.
#R² < 0.1 → Weak model (consider adding more predictors).

##Testing for outliers usings cook's distance
influence.measures(MusicAppreciation_Enrollment_Analysis_20)


# Plot Cook's Distance
plot(cooks.distance(MusicAppreciation_Enrollment_Analysis_20), type = "h", main = "Cook's Distance")
# Pass

## Testing for highly imbalanced class
# a balance of 90% : 10% is a cause for concern in binary regressions, 5% is really concerning
table(cum_data_for_analysis_20$musicapp_binary)
prop.table(table(cum_data_for_analysis_20$musicapp_binary))
#Concerning

##Model Fit, Goodness of Fit Test

hoslem.test(MusicAppreciation_Enrollment_Analysis_20$y, fitted(MusicAppreciation_Enrollment_Analysis_20), g = 8)
#p > 0.05 → Model fits well (good calibration).
#p < 0.05 → Poor model fit.
## Concerning

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Extracting Main Analysis Results to CSV

# List of saved regression models in your environment
model_list_20 <- list(
  Music_Enrollment_20 = Music_Enrollment_Analysis_20,
  Band_Enrollment_20 = Band_Enrollment_Analysis_20,
  Choir_Enrollment_20 = Choir_Enrollment_Analysis_20,
  Orchestra_Enrollment_20 = Orchestra_Enrollment_Analysis_20,
  Jazz_Enrollment_20 = Jazz_Enrollment_Analysis_20,
  MusicTheory_Enrollment_20 = MusicTheory_Enrollment_Analysis_20,
  MusicAppreciation_Enrollment_20 = MusicAppreciation_Enrollment_Analysis_20,
  ModBand_Enrollment_20 = ModBand_Enrollment_Analysis_20
)

# Function to extract model summaries
extract_2020_enrollment_model_results <- function(model_name, model) {
  summary_data <- summary(model)
  coef_data <- as.data.frame(coef(summary_data))
  coef_data$Predictor <- rownames(coef_data)
  coef_data$Model <- model_name
  
  # Reorder columns for better presentation
  coef_data <- coef_data[, c("Model", "Predictor", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
  
  return(coef_data)
}

# Apply function to all models and combine into one dataframe
enrollmentXdemographic_2020_results <- do.call(rbind, lapply(names(model_list), function(name) {
  extract_2020_enrollment_model_results(name, model_list[[name]])
}))

# View the combined results
head(enrollmentXdemographic_2020_results)

# Adding a column which interprets the log-odds as Odd Ratios
enrollmentXdemographic_2020_results$Odds_Ratio <- exp(enrollmentXdemographic_2020_results$Estimate)

# Verify the new column
head(enrollmentXdemographic_2020_results)

# Export to a CSV file
write.csv(enrollmentXdemographic_2020_results, "EnrollmentXDemographic_Analysis_Results_20.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Visualizations

# Marginal Model Plots
Music_Enrollment_model_plot_20 <- marginalModelPlot(Music_Enrollment_Analysis_20)
Band_Enrollment_model_plot_20 <- marginalModelPlot(Band_Enrollment_Analysis_20)
Choir_Enrollment_model_plot_20 <- marginalModelPlot(Choir_Enrollment_Analysis_20)
Orchestra_Enrollment_model_plot_20 <- marginalModelPlot(Orchestra_Enrollment_Analysis_20)
MusicTheory_Enrollment_model_plot_20 <- marginalModelPlot(MusicTheory_Enrollment_Analysis_20)
ModBand_Enrollment_model_plot_20 <- marginalModelPlot(ModBand_Enrollment_Analysis_20)
MusicAppreciation_Enrollment_model_plot_20 <- marginalModelPlot(MusicAppreciation_Enrollment_Analysis_20)
