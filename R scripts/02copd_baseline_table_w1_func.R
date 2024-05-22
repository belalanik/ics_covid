# Author: Marleen Bokern
# Date: 03/2023
# Purpose: creates baseline table for COPD cohort, wave 1 

packages <- c("tidyverse", "ggplot2", "gtsummary", "arrow", "flextable", "dplyr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#######################################################
setwd(Datadir_copd)

create_tables <- function(inputfile, output1, output2) {
  
#read in parquet dataset
df <- read_parquet(inputfile)

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


#subset patients who are either using laba/lama or ics/laba
subset_df <- df[!is.na(df$treatgroup),]

#MAIN TABLE
tab1 <- tbl_summary(subset_df %>%  dplyr::select(age_index, gender, bmicat, eth, smok, imd, diabetes_present, hypertension_present, cvd_present, allcancers_present, past_asthma_present, kidney_present, immunosuppression_present, flu_vacc_present, pneumo_vacc_present,  exacerb_present, treat),
                    by = treat,
                    label = list(age_index ~ "Age",
                                 gender ~ "Gender",
                                 eth ~ "Ethnicity",
                                 bmicat ~ "BMI",
                                 diabetes_present ~ "Diabetes",
                                 hypertension_present ~ "Hypertension",
                                 cvd_present ~ "Cardiovascular disease",
                                 allcancers_present ~ "Cancer",
                                 past_asthma_present ~ "Past asthma",
                                 kidney_present ~ "Kidney impairment",
                                 immunosuppression_present ~ "Immunosuppression",
                                 smok ~ "Smoking",
                                 imd ~ "Index of Multiple Deprivation",
                                 flu_vacc_present ~ "Influenza vaccine",
                                 pneumo_vacc_present ~ "Pneumococcal vaccine",
                                 exacerb_present ~ "Any exacerbation in past 12 months"),
                    percent = "column",
                    digits = all_continuous() ~ 2,
                    missing = "ifany",
                    missing_text = "Missing",
                    statistic = list(
                      all_continuous() ~ "{mean} ({sd})",
                      age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                      #bmi ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                      all_categorical() ~ "{n} ({p}%)"),
                      type = list(
                      c(age_index) ~ "continuous2"))  %>% 
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels()

if (file.exists(paste0(Tables, output1))) {
  # Remove the file if it exists
  file.remove(paste0(Tables, output1))
  cat("File removed successfully.\n")
} else {
  cat("File does not exist.\n")
}

#export to word
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, output1), align = "left")

#Table of outcomes
tab2 <- tbl_summary(subset_df %>% dplyr::select(covid_hes_present, covid_death_present, any_death_present, treat),
                    by = treat,
                    label = list(covid_hes_present ~ "COVID-19 hospitalisation",
                                 covid_death_present ~ "COVID-19 death",
                                 any_death_present ~ "All-cause mortality"),
                    percent = "column",
                    statistic = list(
                      all_categorical() ~ "{n} ({p}%)"))  %>% 
  add_p() %>%
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels()


if (file.exists(paste0(Tables, output2))) {
  # Remove the file if it exists
  file.remove(paste0(Tables, output2))
  cat("File removed successfully.\n")
} else {
  cat("File does not exist.\n")
}

#export to word
tab2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, output2), align = "left")

}

inputfile <- c("copd_wave1_60d.parquet", "copd_wave1_6m.parquet")
output1 <- c("copd_baseline_w1_60d.docx", "copd_baseline_w1_6m.docx")
output2 <- c("copd_outcomes_w1_60d.docx", "copd_outcomes_w1_6m.docx")

#apply function
mapply(create_tables, inputfile, output1, output2)

