### THIS SCRIPT CREATES A BINARY LOGISTIC REGRESSION FOR SPECIFIC COURSES ###
### WITH SELECTED DEMOGRAPHIC VARIABLES AS THE PREDICTOR.                 ###

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### LIBRARIES
library(dplyr)
library(naniar)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DATA
#setwd("R Source Data")
setwd("data_raw")
CCMR_data <- read.csv("2020 CCMR.csv", header = TRUE)
Demo_data <- read.csv("2020 Demo.csv", header = TRUE)
Courses_data <- read.csv("2020 Courses.csv", header = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### EXPLORE DATA

## CCMR_data
str(CCMR_data)
head(CCMR_data)
summary(CCMR_data$Race)
summary(CCMR_data$EcoDis)
summary(CCMR_data$ELL)
# THERE IS NO IEP DATA FOR THE 2020 DATA SET
##Transfrom SpecEd -> IEP
CCMR_data <- CCMR_data %>%
  rename(IEP = SpecEd)
summary(CCMR_data$IEP) 

count_ids_courses <- n_distinct(Courses_data$id)
count_ids_ccmr <- n_distinct(CCMR_data$id)

## Course Data
fa_data <- Courses_data %>%
  filter(crs_area == "Fine Arts") ### Filter Fine Arts courses

fa_courses <- fa_data %>%
  distinct(crsname, CRSNUM) ### Create a list of FA courses and numbers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### COURSES DATA

##Examining Courses
unique_courses <- Courses_data %>%
  distinct(crsname, CRSNUM) %>%
  arrange(crsname) # Optional: Arrange by course name

print(unique_courses)

# I've put this section in notes so we can reference your old list and compare to my new one if needed.
## Filter Music Courses
# music_courses <- Courses_data %>% 
#   filter(CRSNUM %in% 
#            c("8813", "8190", "8401", "8169", "8176", "8446", "8835", "8434", 
#              "8468", "8555", "8179", "8194", "8545", "8469", "8181", "8399", 
#              "8609", "8689", "8559", "8413", "8171", "8409", "8558", "8417", 
#              "8548", "8204", "8205", "8554", "8432", "8694", "8425", "8563", 
#              "8546", "8428", "8711", "8551", "8175", "8611", "8678", "8173", 
#              "8561", "4413", "8779", "8780", "8408", "8444", "8445", "8567", 
#              "8547", "8544", "8429", "8182", "8201", "8601", "8419", "8198", 
#              "8590", "8484", "8191", "8202", "8552", "8423", "8200", "8203", 
#              "8433", "8398", "8439", "8174", "8170", "8639", "8565", "8615", 
#              "4204", "8597", "6509", "6510", "8553", "8703", "8177", "8193", 
#              "8549", "8704", "6507", "6508", "8624", "8564", "8638", "8557", 
#              "8566", "8180", "8418", "8172", "8183", "8560", "8951", "8953", 
#              "8310", "8479", "8550", "8618", "8438"))

music_courses_list <- read.csv("music_courses_filtered_2019.csv", header = TRUE)

# Convert numeric columns to appropriate types
music_courses_list$crs_row_num <- as.integer(music_courses_list$crs_row_num)
music_courses_list$CRSNUM <- as.integer(music_courses_list$CRSNUM)

# View the final data frame
View(music_courses_list)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DEMOGRAPHIC DATA

## Data Transformation

# Re-Categorize Data
CCMR_data$ELL[CCMR_data$ELL == "C"] <- "Y" ###Current to Yes
CCMR_data$ELL[CCMR_data$ELL == "F"] <- "N" ###Former to No
CCMR_data$ELL[CCMR_data$ELL == "M"] <- "N" ###Monitoring to No
CCMR_data$ELL[CCMR_data$ELL == ""] <- "N"  ###Empty value to No
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rename CCMR_data column Speced to IEP
CCMR_data <- CCMR_data %>%
  rename(IEP = SpecEd)

CCMR_data$IEP[CCMR_data$IEP == "S"] <- "Y" ###Special to Yes
CCMR_data$IEP[CCMR_data$IEP == "F"] <- "N" ###Former to No
CCMR_data$IEP[CCMR_data$IEP == ""] <- "N"  ###Empty value to No
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert Strings to factors
CCMR_data$Race <- as.factor(CCMR_data$Race)
CCMR_data$EcoDis <- as.factor(CCMR_data$EcoDis)
CCMR_data$ELL <- as.factor(CCMR_data$ELL)
CCMR_data$IEP <- as.factor(CCMR_data$IEP)

Demo_data$sex <- as.factor(Demo_data$sex)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###DATA MERGE

## Merge Demographic Data

### the student ID column in Demo_data was named "idnr"? so I renamed it ID assuming these are the same variable in both CCMR_data and Demo_data

Demo_data <- Demo_data %>%
  rename(id = idnr)

combined_demo <- merge(CCMR_data, Demo_data, by = "id", all.x = TRUE) ### Left join
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I'm putting this next section in notes because I think its no longer needed but want to hold onto it in case.
# ## Merge Demographic Data into Course Data
# 
# music_course_demo_merge <- merge(music_courses_2ed......., combined_demo, by = "id", all.x = TRUE) ### Left join
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Adding Music Course Enrollment indicators to students

## Create indicator columns
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
#     I put conducting in music appreciation?
#     I put handbells in Other
#     I put Mariachi in Other
#     I put music business in music appreciation
#     I put music history in appreciation
#     I put music production in Modern Band, feel free to change it
#     There was a "Partner Band Coach" I left this in Band category but we may change it
#     I put world music ensembles in other

# Add the music_indicator column by matching CRSNUM
Courses_data <- merge(Courses_data, 
                      music_courses_list[, c("CRSNUM", "music_indicator")], 
                      by = "CRSNUM", 
                      all.x = TRUE)

# View the updated dataframe
View(Courses_data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Change NA music_indicator values to 0

# Replace NA values with 0
Courses_data <- Courses_data %>%
  mutate(music_indicator = ifelse(is.na(music_indicator), 0, music_indicator))

# Verify changes
table(Courses_data$music_indicator)

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

### Combine Courses_data and combined_demo before analysis

# Perform a left join to keep all rows from Courses_data
cumulative_data <- Courses_data %>%
  left_join(combined_demo, by = "id")

# View the first few rows of the combined data
head(cumulative_data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Create a binary column for overall music enrollment
cumulative_data$music_binary <- ifelse(cumulative_data$music_indicator >= 1, 1, 0)

### Create a binary column for Band
cumulative_data$band_binary <- ifelse(cumulative_data$music_indicator == 1, 1, 0)

### Create a binary column for Choir
cumulative_data$choir_binary <- ifelse(cumulative_data$music_indicator == 2, 1, 0)

### Create a binary column for Orchestra
cumulative_data$orch_binary <- ifelse(cumulative_data$music_indicator == 3, 1, 0)

### Create a binary column for Modern Band
cumulative_data$modband_binary <- ifelse(cumulative_data$music_indicator == 4, 1, 0)

### Create a binary column for Music Theory
cumulative_data$mut_binary <- ifelse(cumulative_data$music_indicator == 5, 1, 0)

### Create a binary column for Jazz
cumulative_data$jazz_binary <- ifelse(cumulative_data$music_indicator == 6, 1, 0)

### Create a binary column for Music Appreciation
cumulative_data$musicapp_binary <- ifelse(cumulative_data$music_indicator == 7, 1, 0)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Main Analysis : Multivariate

###### I'm not familiar with the assumptions testing for binary logistic regressions. What would you like done?
###### What about assessing model performance? odds ratio, goodness of fit, confusion matrix?

### Binary Logistic Regression for
#       Outcome Variable : Music Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Music_Enrollment_Analysis <- glm(music_binary ~ Race + sex + EcoDis + ELL + IEP, 
             data = cumulative_data, 
             family = binomial(link = "logit"))

# Summarize the model results
summary(Music_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Band
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Band_Enrollment_Analysis <- glm(band_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Band_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Choir
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Choir_Enrollment_Analysis <- glm(choir_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Choir_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Orchestra
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Orchestra_Enrollment_Analysis <- glm(orch_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Orchestra_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Modern Band
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
ModernBand_Enrollment_Analysis <- glm(modband_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(ModernBand_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Music Theory
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
MusicTheory_Enrollment_Analysis <- glm(mut_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(MusicTheory_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Music Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
Jazz_Enrollment_Analysis <- glm(jazz_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))
#returns an error saying probability is close to 0 or 1. Indicating collinearity?

# Summarize the model results
summary(Jazz_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Binary Logistic Regression for
#       Outcome Variable : Music Enrollment
#       Predictors : Race, Sex, SES (EcoDis), ELL, IEP

# Run logistic regression model
MusicApp_Enrollment_Analysis <- glm(musicapp_binary ~ Race + sex + EcoDis + ELL + IEP, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(MusicApp_Enrollment_Analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Visualization
##### Notes for discussion : When creating the predictors for visualization I 
#     noticed that there are 94630 rows of predictors but 362163 rows of 
#     Courses_data, I think this is because Courses_data is long data with repeated
#     student id's, but I'm not sure. Regardless we should consider whether some
#     observations or rows were left out or what happened.

library(ggplot2)

# Add predicted probabilities to the dataframe
music_enrollment_predicted_prob <- predict(Music_Enrollment_Analysis, type = "response")

# Convert predictions to dataframe and reset row names
music_enrollment_predicted_df <- data.frame(predicted_prob_music_enrollment = music_enrollment_predicted_prob)

# Add row numbers as a column to align with original data
music_enrollment_predicted_df$row_num <- as.numeric(rownames(music_enrollment_predicted_df))

# Merge predictions back to cumulative_data by row index
cumulative_data <- cumulative_data %>%
  mutate(row_num = row_number()) %>%  # Add row index to the original data
  left_join(music_enrollment_predicted_df, by = "row_num")

# Remove the row_num column after merging
cumulative_data <- cumulative_data %>%
  select(-row_num)

# View updated dataframe with predictions
head(cumulative_data)


### Histogram

ggplot(cumulative_data, aes(x = predicted_prob_music_enrollment, fill = music_binary)) +
  geom_histogram(binwidth = 0.05, position = "dodge") +
  labs(title = "Predicted Probabilities of Music Enrollment")




### Scatter Plot

# Scatter plot of predicted probabilities vs actual enrollment
ggplot(cumulative_data, aes(x = predicted_prob_music_enrollment, y = as.numeric(music_binary))) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.6) +
  labs(title = "Predicted Probability vs Actual Enrollment",
       x = "Predicted Probability",
       y = "Actual Enrollment (0=No, 1=Yes)") +
  theme_minimal()




### ROC Curve

library(pROC)

# Generate ROC curve
roc_curve <- roc(cumulative_data$music_binary, cumulative_data$predicted_prob_music_enrollment)

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Reference line

# Display AUC
auc(roc_curve)




### Odds Ratio

library(broom)

# Extract coefficients and confidence intervals
music_enrollment_odds_ratios <- exp(coef(Music_Enrollment_Analysis))
music_enrollment_conf_int <- exp(confint(Music_Enrollment_Analysis))

# Prepare a dataframe
music_enrollment_odds_data <- data.frame(
  Variable = names(music_enrollment_odds_ratios),
  OddsRatio = music_enrollment_odds_ratios,
  LowerCI = music_enrollment_conf_int[, 1],
  UpperCI = music_enrollment_conf_int[, 2]
)

# Plot the odds ratios with confidence intervals
ggplot(music_enrollment_odds_data, aes(x = reorder(Variable, OddsRatio), y = OddsRatio)) +
  geom_point() +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Odds Ratios with 95% CI",
       x = "Predictor Variable",
       y = "Odds Ratio (95% CI)") +
  theme_minimal()


### Boxplots of predictors by indicator
ggplot(cumulative_data, aes(x = as.factor(music_binary), y = EcoDis, fill = as.factor(music_binary))) +
  geom_boxplot() +
  labs(title = "Distribution of SES (EcoDis) by Music Enrollment",
       x = "Music Enrollment",
       y = "SES (EcoDis)",
       fill = "Music Enrollment") +
  theme_minimal()

## Interactions
glm(music_binary ~ Race * sex, 
                         data = cumulative_data, 
                         family = binomial(link = "logit"))





### Main Analysis : Univariate

## Race
# Run logistic regression model
Music_Enrollment_Analysis_Race <- glm(music_binary ~ Race, 
                                 data = cumulative_data, 
                                 family = binomial(link = "logit"))

# Summarize the model results
summary(Music_Enrollment_Analysis_Race)


### Visualization
# Add predicted probabilities to the dataframe
music_enrollment_race_predicted_prob <- predict(Music_Enrollment_Analysis_Race, type = "response")

# Convert predictions to dataframe and reset row names
music_enrollment_race_predicted_df <- data.frame(predicted_prob_byRace = music_enrollment_race_predicted_prob)

# Add row numbers as a column to align with original data
music_enrollment_race_predicted_df$row_num <- as.numeric(rownames(music_enrollment_race_predicted_df))

# Merge predictions back to cumulative_data by row index
cumulative_data <- cumulative_data %>%
  mutate(row_num = row_number()) %>%  # Add row index to the original data
  left_join(music_enrollment_race_predicted_df, by = "row_num")

# Remove the row_num column after merging
cumulative_data <- cumulative_data %>%
  select(-row_num)


# Bar Graph

ggplot(cumulative_data, aes(x = Race, y = predicted_prob_byRace, fill = Race)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average Predicted Probability of Music Enrollment by Race",
       x = "Race",
       y = "Predicted Probability") +
  theme_minimal()


