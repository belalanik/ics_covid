# Author: Marleen Bokern
# Date: 03/2023
# Purpose: basic data formatting for analytic file

library(dplyr)
library(tidyverse)

setwd(Datadir_copd)

#read in csv dataset
df <- read.csv("copd_analytic_file_w1_60d.csv", colClasses = c("patid" = "character"), header = T)

#create binary treatment group variable. Set to 1 if ics_group, set to 0 if laba_lama
df$treatgroup <- ifelse(df$baseline_ics == 1, 1, NA)
df$treatgroup <- ifelse(df$baseline_triple == 1, NA, df$treatgroup)
df$treatgroup[df$baseline_control == 1] <- 0
df$treatgroup <- as.factor(df$treatgroup)

#convert covariates coded as date variables to dates 
date_cols <- c("regstart", "deathdate", "regend", "dob", "do35bday", "lcd", "enddate", "diabetes_date", "hypertension_date", "asthma_date", "cvd_date", "allcancers_date", "kidney_date", "smokdate", "pos_covid_test_date")
df[date_cols] <- lapply(df[date_cols], as.Date, format = "%d%b%Y")


df$smok <- recode(df$smokstatus,
                  "current smoker" = "Current smoking",
                  "current/ex-smoker" = "Current/Former smoking",
                  "ex-smoker" = "Former smoking",
                  "nonspecified - depends on quantity" = "Unclear",
                  .default = "Unclear")

# Convert "smok" to a factor
df$smok <- factor(df$smok)

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
df$eth[df$eth5 == "5. Not Stated" | df$eth5 == "3. Other" | df$eth5 == ""] <- NA

#This also works
# df$ethn <- ifelse(df$eth5 == "", NA, df$eth5)
# df$ethn <- factor(df$ethn)
# df$ethn  <- ifelse(df$eth5 == 6, NA, df$ethn)

labels_eth <- c("White", "South Asian", "Black", "Mixed")
df$eth <- factor(df$eth, levels = c(0,1,2,3), labels = labels_eth)

# Create a new variable called 'timeout' based on the value of 'pos_covid_test_date'
df$timeout <- df$pos_covid_test_date
# Replace missing values in 'timeout' with the value of 'enddate'
df$timeout[is.na(df$timeout)] <- df$enddate
# Replace values in 'timeout' that are greater than '2020-08-31' with the value of '2020-08-31'
df$timeout[df$timeout > as.Date("2020-08-31")] <- as.Date("2020-08-31")
sum(df$timeout == as.Date("2020-08-31"))

# Generate a variable that is 1 if outcome is present, and 0 if outcome date is empty
df$pos_covid_test_present <- factor(ifelse(!is.na(df$pos_covid_test_date), "Yes", "No"))
df$pos_covid_test_present <- ifelse(is.na(df$pos_covid_test_date), 0, 1)

df$covid_hes_present <- factor(ifelse(!is.na(df$covid_hes_date), "Yes", "No"))
df$covid_hes_present <- ifelse(is.na(df$covid_hes_date), 0, 1)

df$covid_death_present <- factor(ifelse(!is.na(df$covid_death_date), "Yes", "No"))
df$covid_death_present <- ifelse(is.na(df$covid_death_date), 0, 1)

# Set the time origin to 01 Mar 2020
df$time_origin <- as.Date("2020-03-01")

# Convert the exit time variable to a survival time object
df$timeout = as.numeric(df$timeout, "%d%b%Y")
df$time_origin = as.numeric(df$time_origin, "%d%b%Y")
df$timeinstudy <- df$timeout - df$time_origin


arrow::write_parquet(df, "copd_wave1_60d_no_triple.parquet")