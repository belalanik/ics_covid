# Author: Marleen Bokern
# Date: 03/2023
# Purpose: creates baseline table for COPD cohort, wave 1 

packages <- c("tidyverse", "ggplot2", "gtsummary", "arrow", "flextable", "dplyr", "htmltools")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

setwd(Datadir_copd)

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

#some people have outcomes before 01mar2020. Set these date to missing.
outcome_variables <- c("pos_covid_test_date", "covid_hes_date", "covid_death_date", "death_date")

for (var_name in outcome_variables) {
  #count people with outcomes before index date
  sum_result <- sum(df[[var_name]] < as.Date("2020-03-01"), na.rm = TRUE)
  cat("Sum for", var_name, ":", sum_result, "\n")
  
  df <- df[df[[var_name]] >= as.Date("2020-03-01") | is.na(df[[var_name]]),]
  
  #count people with outcomes before index date. Should now be 0
  sum_result <- sum(df[[var_name]] < as.Date("2020-03-01"), na.rm = TRUE)
  cat("Sum for", var_name, ":", sum_result, "\n")
}


# ICS GROUP - EVER ----------------------------------------------------

# sort on positive covid test
df <- df %>% arrange(pos_covid_test_date)

#some people have positive covid test before 01mar2020. Drop these people.
df <- df[df$pos_covid_test_date >= as.Date("2020-03-01") | is.na(df$pos_covid_test_date),]


#subset patients who are either using laba/lama or ics/laba
df_subset <- df[!is.na(df$treatgroup),]

#MAIN TABLE
tab1 <- tbl_summary(df %>% dplyr::select(age_index, gender, bmicat, imd, eth, smok, diabetes_present, hypertension_present, cvd_present, allcancers_present, asthma_present, kidney_present, exacerb_present, ics_ever),
                  by = ics_ever,
                  label = list(age_index ~ "Age",
                               gender ~ "Gender",
                               eth ~ "Ethnicity",
                               bmicat ~ "BMI",
                               diabetes_present ~ "Diabetes",
                               hypertension_present ~ "Hypertension",
                               cvd_present ~ "Cardiovascular disease",
                               allcancers_present ~ "Cancer",
                               asthma_present ~ "Past asthma",
                               kidney_present ~ "Kidney impairment",
                               smok ~ "Smoking",
                               imd ~ "Index of Multiple Deprivation",
                               exacerb_present ~ "Any exacerbation in past 12 months"),
                  percent = "column",
                  digits = all_continuous() ~ 2,
                  missing = "ifany",
                  missing_text = "Missing",
                  statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                    all_categorical() ~ "{n} ({p}%)"
                  ),
                  type = list(
                    c(age_index) ~ "continuous2"
                  )
)  %>% 
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1), align = "right") %>% 
  italicize_levels()

tab1

# Check if the file exists
if (file.exists(paste0(Tables, "copd_baseline_w1_60d_ics_ever.docx"))) {
  # If the file exists, delete it
  file.remove(paste0(Tables, "copd_baseline_w1_60d_ics_ever.docx"))
  print("File deleted")
} else {
  print("No file found")
}


#export to word
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, "copd_baseline_w1_60d_ics_ever.docx"), align = "left")

# need to add subheadings --> group observations


# CONTROL GROUP - EVER ----------------------------------------------------

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")


# sort on positive covid test
df <- df %>% arrange(pos_covid_test_date)

#some people have positive covid test before 01mar2020. Drop these people.
df <- df[df$pos_covid_test_date >= as.Date("2020-03-01") | is.na(df$pos_covid_test_date),]


#subset patients who are either using laba/lama or ics/laba
df_subset <- df[!is.na(df$treatgroup),]

#MAIN TABLE
tab1 <- tbl_summary(df %>% dplyr::select(age_index, gender, bmicat, imd, eth, smok, diabetes_present, hypertension_present, cvd_present, allcancers_present, asthma_present, kidney_present, exacerb_present, control_ever),
                    by = control_ever,
                    label = list(age_index ~ "Age",
                                 gender ~ "Gender",
                                 eth ~ "Ethnicity",
                                 bmicat ~ "BMI",
                                 diabetes_present ~ "Diabetes",
                                 hypertension_present ~ "Hypertension",
                                 cvd_present ~ "Cardiovascular disease",
                                 allcancers_present ~ "Cancer",
                                 asthma_present ~ "Past asthma",
                                 kidney_present ~ "Kidney impairment",
                                 smok ~ "Smoking",
                                 imd ~ "Index of Multiple Deprivation",
                                 exacerb_present ~ "Any exacerbation in past 12 months"),
                    percent = "column",
                    digits = all_continuous() ~ 2,
                    missing = "ifany",
                    missing_text = "Missing",
                    statistic = list(
                      all_continuous() ~ "{mean} ({sd})",
                      age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    type = list(
                      c(age_index) ~ "continuous2"
                    )
)  %>% 
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1), align = "right") %>% 
  italicize_levels()

tab1

# Check if the file exists
if (file.exists(paste0(Tables, "copd_baseline_w1_60d_control_ever.docx"))) {
  # If the file exists, delete it
  file.remove(paste0(Tables, "copd_baseline_w1_60d_control_ever.docx"))
  print("File deleted")
} else {
  print("No file found")
}

#export to word
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, "copd_baseline_w1_60d_control_ever.docx"), align = "left")


