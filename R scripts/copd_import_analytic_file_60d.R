# Author: Marleen Bokern
# Date: 03/2023
# Purpose: basic data formatting for analytic file, generate binary treatment group variable

library(dplyr)
library(tidyverse)

setwd(Datadir_copd)

#read in csv dataset
df <- read.csv("copd_analytic_file_w1_60d.csv", colClasses = c("patid" = "character"), header = T)


# Reformatting variables --------------------------------------------------

#create binary treatment group variable. Set to 1 if ICS group, set to 0 if LABA/LAMA
df$treatgroup <- ifelse(df$baseline_ics == 1, 1, NA)
df$treatgroup[df$baseline_control == 1] <- 0
df$treatgroup <- as.factor(df$treatgroup)

#convert covariates coded as date variables to dates 
date_cols <- c("regstart", "deathdate", "regend", "dob", "do35bday", "lcd", "enddate", "diabetes_date", "hypertension_date", "asthma_date", "cvd_date", "allcancers_date", "kidney_date", "immunosuppression_date", "smokdate", "flu_vacc_date", "pneumo_vacc_date", "pos_covid_test_date", "covid_hes_date", "covid_death_date")
df[date_cols] <- lapply(df[date_cols], as.Date, format = "%d%b%Y")

#create binary variables for each covariate
df$diabetes_present <- factor(ifelse(!is.na(df$diabetes_date), "Yes", "No"))
df$hypertension_present <- factor(ifelse(!is.na(df$hypertension_date), "Yes", "No"))
df$cvd_present <- factor(ifelse(!is.na(df$cvd_date), "Yes", "No"))
df$allcancers_present <- factor(ifelse(!is.na(df$allcancers_date), "Yes", "No"))
df$asthma_present <- factor(ifelse(!is.na(df$asthma_date), "Yes", "No"))
df$covid_present <- factor(ifelse(!is.na(df$pos_covid_test_date), "Yes", "No"))
df$kidney_present <- factor(ifelse(!is.na(df$kidney_date), "Yes", "No"))
df$immunosuppression_present <- factor(ifelse(!is.na(df$immunosuppression_date), "Yes", "No"))
df$flu_vacc_present <- factor(ifelse(!is.na(df$flu_vacc_date), "Yes", "No"))
df$pneumo_vacc_present <- factor(ifelse(!is.na(df$pneumo_vacc_date), "Yes", "No"))

df$exacerbations <- ifelse(is.na(df$exacerbations), 0, df$exacerbations)
df$exacerb_present <- factor(ifelse(df$exacerbations != 0, "Yes", "No"))

#recode smoking status
df$smok <- recode(df$smokstatus,
                  "current smoker" = "Current smoking",
                  "current/ex-smoker" = "Current/Former smoking",
                  "ex-smoker" = "Former smoking",
                  "nonspecified - depends on quantity" = "Unclear",
                  .default = "Unclear")

# Convert "smok" to a factor
df$smok <- factor(df$smok)

# Convert "imd" to a factor
df$imd <- factor(df$imd)

# Define the value labels for treatgroup, gender and smoking
labels_treat <- c("LABA/LAMA", "ICS/LABA")
labels_gender <- c("Male", "Female")
#labels_smoking <- c("Current smoking", "Current/Former smoking", "Former smoking", "Unclear")
labels_ics_ever <- c("ICS/LABA")
labels_control_ever <- c("LABA/LAMA")

# Convert the variable to a factor with the specified labels
df$treat <- factor(df$treatgroup, levels = c(0,1), labels = labels_treat)
df$gender <- factor(df$gender, levels = c(1,2), labels = labels_gender)
#df$smok <- factor(df$smokstatus, labels = labels_smoking)
df$ics_ever <- factor(df$ics_ever, labels = labels_ics_ever)
df$control_ever <- factor(df$control_ever, labels = labels_control_ever)

# Recode ethnicity 
# create a new variable 'eth' in the data frame
df$eth <- NA_integer_

# replace eth values based on eth5 values
df$eth[df$eth5 == "0. White"] <- 0
df$eth[df$eth5 == "1. South Asian"] <- 1
df$eth[df$eth5 == "2. Black"] <- 2
df$eth[df$eth5 == "4. Mixed"] <- 3
df$eth[df$eth5 == "5. Not Stated" | df$eth5 == "3. Other" | df$eth5 == ""] <- 4

#This also works
# df$ethn <- ifelse(df$eth5 == "", NA, df$eth5)
# df$ethn <- factor(df$ethn)
# df$ethn <- ifelse(df$eth5 == 6, NA, df$ethn)

labels_eth <- c("White", "South Asian", "Black", "Mixed", "Unknown")
df$eth <- factor(df$eth, levels = c(0,1,2,3,4), labels = labels_eth)

# recode BMI as categorical
df$bmicat <- NA

# Categorize BMI values
df$bmicat[df$bmi < 18.5] <- 1
df$bmicat[df$bmi >= 18.5 & df$bmi < 25] <- 2
df$bmicat[df$bmi >= 25 & df$bmi < 30] <- 3
df$bmicat[df$bmi >= 30] <- 4

# Assign category labels
df$bmicat <- factor(df$bmicat, levels = c(1, 2, 3, 4),
                    labels = c("Underweight (<18.5)", "Normal (18.5-24.9)",
                               "Overweight (25-29.9)", "Obese (>=30)"))

# Create a less granular categorization
df$obese4cat <- NA
df$obese4cat[df$bmicat %in% c("Underweight (<18.5)", "Normal (18.5-24.9)", "Overweight (25-29.9)")] <- 1
df$obese4cat[df$bmicat == "Obese I (30-34.9)"] <- 2
df$obese4cat[df$bmicat == "Obese II (35-39.9)"] <- 3
df$obese4cat[df$bmicat == "Obese III (40+)"] <- 4

# Assign category labels
df$obese4cat <- factor(df$obese4cat, levels = c(1, 2, 3, 4),
                       labels = c("No record of obesity", "Obese I (30-34.9)",
                                  "Obese II (35-39.9)", "Obese III (40+)"))


# code end of follow ups
#create new variables denoting the length of time until the outcome of interest. 
#generate a timeout variable for all cause deaths as a sense check.
df$timeout1 <- df$pos_covid_test_date
df$timeout2 <- df$covid_hes_date
df$timeout3 <- df$covid_death_date
df$timeout_death_any <- df$deathdate

#WORK WITH OUTCOME DATES
timeout_variables <- c("timeout1", "timeout2", "timeout3", "timeout_death_any")

for (var_name in timeout_variables) {
  
  df[[var_name]][is.na(df[[var_name]])] <- df$enddate[is.na(df[[var_name]])]

  # Replace values greater than "2020-08-31" with "2020-08-31"
  df[[var_name]][df[[var_name]] > as.Date("2020-08-31")] <- as.Date("2020-08-31")
  
  # Calculate the sum of values that are equal to "2020-08-31"
  sum_result <- sum(df[[var_name]] == as.Date("2020-08-31"), na.rm = TRUE)
  
  cat("Sum for", var_name, ":", sum_result, "\n")
} 

# Generate a variable that is 1 if outcome is present, and 0 if outcome date is empty
df$pos_covid_test_present <- factor(ifelse(!is.na(df$pos_covid_test_date), "Yes", "No"))
df$pos_covid_test_present <- ifelse(is.na(df$pos_covid_test_date), 0, 1)
 
df$covid_hes_present <- factor(ifelse(!is.na(df$covid_hes_date), "Yes", "No"))
df$covid_hes_present <- ifelse(is.na(df$covid_hes_date), 0, 1)

df$covid_death_present <- factor(ifelse(!is.na(df$covid_death_date), "Yes", "No"))
df$covid_death_present <- ifelse(is.na(df$covid_death_date), 0, 1)

df$any_death_present <- factor(ifelse(df$deathdate < as.Date("2020-09-01"), "Yes", "No"))
df$any_death_present <- ifelse(df$deathdate < as.Date("2020-09-01"), 1, 0)
df$any_death_present[is.na(df$any_death_present)] <- 0
df$any_death_present[df$deathdate > as.Date("2020-08-31")] <- 0

#assert that there are more all cause deaths than covid deaths
sum(df$any_death_present == 1 & df$deathdate < as.Date("2020-09-01"), na.rm = TRUE) > sum(df$covid_death_present == 1 & df$deathdate < as.Date("2020-09-01"), na.rm = TRUE)

# Set the time origin to 01 Mar 2020
df$time_origin <- as.Date("2020-03-01")

# Convert the exit time variable to a survival time object
df$time_origin = as.numeric(df$time_origin, "%d%b%Y")

df[timeout_variables] <- lapply(df[timeout_variables], as.numeric, format = "%d%b%Y")

df$timeinstudy1 <- df$timeout1 - df$time_origin
df$timeinstudy2 <- df$timeout2 - df$time_origin
df$timeinstudy3 <- df$timeout3 - df$time_origin
df$timeinstudy_death_any <- df$timeout_death_any - df$time_origin

#export as parquet file
arrow::write_parquet(df, "copd_wave1_60d.parquet")

