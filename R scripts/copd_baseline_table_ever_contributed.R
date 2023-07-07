# Author: Marleen Bokern
# Date: 03/2023
# Purpose: creates baseline table for COPD cohort, wave 1 

#install.packages("gtsummary")
#install.packages("flextable")
#install.packages("arrow")
library(arrow)
library(htmltools)
library(gtsummary)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(flextable)

setwd(Datadir_copd)

# ICS GROUP - EVER ----------------------------------------------------
#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

#create binary variables for each covariate
df$diabetes_present <- factor(ifelse(!is.na(df$diabetes_date), "Yes", "No"))
df$hypertension_present <- factor(ifelse(!is.na(df$hypertension_date), "Yes", "No"))
df$cvd_present <- factor(ifelse(!is.na(df$cvd_date), "Yes", "No"))
df$allcancers_present <- factor(ifelse(!is.na(df$allcancers_date), "Yes", "No"))
df$asthma_present <- factor(ifelse(!is.na(df$asthma_date), "Yes", "No"))
df$covid_present <- factor(ifelse(!is.na(df$pos_covid_test_date), "Yes", "No"))
df$kidney_present <- factor(ifelse(!is.na(df$kidney_date), "Yes", "No"))

df$exacerbations <- ifelse(is.na(df$exacerbations), 0, df$exacerbations)
df$exacerb_present <- factor(ifelse(df$exacerbations != 0, "Yes", "No"))

# sort on positive covid test
df <- df %>% arrange(pos_covid_test_date)

#some people have positive covid test before 01mar2020. Drop these people.
df <- df[df$pos_covid_test_date >= as.Date("2020-03-01") | is.na(df$pos_covid_test_date),]


#crosstab gender and treatment group
ctab <- table(df$gender, df$treatgroup,
              dnn = list("Gender", "Treatment Group"))
ctab

#subset patients who are either using laba/lama or ics/laba
df_subset <- df[!is.na(df$treatgroup),]

#easy histogram of age and treatment group
ggplot(df_subset, aes(x = age_index)) + 
  geom_histogram(aes(fill = factor(treatgroup)),
                 binwidth = 5, alpha = 0.5) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  labs(title = "Distribution of Age Index by Treatgroup",
       x = "Age Index", y = "Count")

#MAIN TABLE
tab1<-tbl_summary(df %>% select(age_index, gender, bmi, eth, smok, diabetes_present, hypertension_present, cvd_present, allcancers_present, asthma_present, kidney_present, exacerbations, exacerb_present, covid_present, ics_ever),
                  by = ics_ever,
                  label = list(age_index ~ "Age",
                               gender ~ "Gender",
                               eth ~ "Ethnicity",
                               bmi ~ "BMI",
                               diabetes_present ~ "Diabetes",
                               hypertension_present ~ "Hypertension",
                               cvd_present ~ "Cardiovascular disease",
                               allcancers_present ~ "Cancer",
                               asthma_present ~ "Past asthma",
                               kidney_present ~ "Kidney impairment",
                               smok ~ "Smoking",
                               covid_present ~ "Positive COVID-19 test",
                               exacerbations ~ "Number of COPD exacerbations \n in past 12 months", 
                               exacerb_present ~ "Any exacerbation in past 12 months"),
                  percent = "column",
                  digits = all_continuous() ~ 2,
                  missing = "ifany",
                  missing_text = "Missing",
                  statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                    bmi ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                    all_categorical() ~ "{n} ({p}%)"
                  ),
                  type = list(
                    c(age_index, bmi) ~ "continuous2"
                  )
)  %>% 
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1), align = "right") %>% 
  italicize_levels()

tab1

#export to word
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, "copd_baseline_w1_60d_ics_ever.docx"), align = "left")

# need to add subheadings --> group observations


# CONTROL GROUP - EVER ----------------------------------------------------

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

#create binary variables for each covariate
df$diabetes_present <- factor(ifelse(!is.na(df$diabetes_date), "Yes", "No"))
df$hypertension_present <- factor(ifelse(!is.na(df$hypertension_date), "Yes", "No"))
df$cvd_present <- factor(ifelse(!is.na(df$cvd_date), "Yes", "No"))
df$allcancers_present <- factor(ifelse(!is.na(df$allcancers_date), "Yes", "No"))
df$asthma_present <- factor(ifelse(!is.na(df$asthma_date), "Yes", "No"))
df$covid_present <- factor(ifelse(!is.na(df$pos_covid_test_date), "Yes", "No"))
df$kidney_present <- factor(ifelse(!is.na(df$kidney_date), "Yes", "No"))

df$exacerbations <- ifelse(is.na(df$exacerbations), 0, df$exacerbations)
df$exacerb_present <- factor(ifelse(df$exacerbations != 0, "Yes", "No"))

# sort on positive covid test
df <- df %>% arrange(pos_covid_test_date)

#some people have positive covid test before 01mar2020. Drop these people.
df <- df[df$pos_covid_test_date >= as.Date("2020-03-01") | is.na(df$pos_covid_test_date),]


#crosstab gender and treatment group
ctab <- table(df$gender, df$treatgroup,
              dnn = list("Gender", "Treatment Group"))
ctab

#subset patients who are either using laba/lama or ics/laba
df_subset <- df[!is.na(df$treatgroup),]

#easy histogram of age and treatment group
ggplot(df_subset, aes(x = age_index)) + 
  geom_histogram(aes(fill = factor(treatgroup)),
                 binwidth = 5, alpha = 0.5) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  labs(title = "Distribution of Age Index by Treatgroup",
       x = "Age Index", y = "Count")

#MAIN TABLE
tab1<-tbl_summary(df %>% select(age_index, gender, bmi, eth, smok, diabetes_present, hypertension_present, cvd_present, allcancers_present, asthma_present, kidney_present, exacerbations, exacerb_present, covid_present, control_ever),
                  by = control_ever,
                  label = list(age_index ~ "Age",
                               gender ~ "Gender",
                               eth ~ "Ethnicity",
                               bmi ~ "BMI",
                               diabetes_present ~ "Diabetes",
                               hypertension_present ~ "Hypertension",
                               cvd_present ~ "Cardiovascular disease",
                               allcancers_present ~ "Cancer",
                               asthma_present ~ "Past asthma",
                               kidney_present ~ "Kidney impairment",
                               smok ~ "Smoking",
                               covid_present ~ "Positive COVID-19 test",
                               exacerbations ~ "Number of COPD exacerbations \n in past 12 months", 
                               exacerb_present ~ "Any exacerbation in past 12 months"),
                  percent = "column",
                  digits = all_continuous() ~ 2,
                  missing = "ifany",
                  missing_text = "Missing",
                  statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                    bmi ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                    all_categorical() ~ "{n} ({p}%)"
                  ),
                  type = list(
                    c(age_index, bmi) ~ "continuous2"
                  )
)  %>% 
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1), align = "right") %>% 
  italicize_levels()

tab1

#export to word
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, "copd_baseline_w1_60d_control_ever.docx"), align = "left")

# need to add subheadings --> group observations
