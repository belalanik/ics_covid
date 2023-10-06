# Author: Marleen Bokern
# Date: 03/2023
# Purpose: generate propensity scores and IPTW weights for COPD cohort. Fit Cox models for all three outcomes.

packages <- c("tidyverse", "MetBrewer", "arrow", "MatchThem", "mice", "parallelly", "furrr", "survey", "cobalt", "ggplot2", "twang", "ipw", "WeightIt", "gtsummary", "dplyr", "flextable", "openxlsx", "flexsurv", "survival", "jskm")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#########################################################
setwd(Datadir_copd)

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

palette <- met.brewer("Greek")

df$treatgroup_binary <- as.integer(df$treatgroup == "1")

#keep only people who are in one of the treatment groups, remove all unnecessary variables
subset_df <- df[!is.na(df$treatgroup),]
subset_df <- subset_df[, c("patid", "age_index", "gender", "deathdate", "imd", "bmicat", "eth", 
                           "diabetes_present", "hypertension_present", 
                           "cvd_present", "allcancers_present", 
                           "asthma_present", "kidney_present", 
                           "immunosuppression_present", "flu_vacc_present", 
                           "pneumo_vacc_present", "exacerb_present", 
                           "smok","timeinstudy1", "timeinstudy2", "timeinstudy3", "timeinstudy_death_any", "timeout1", "timeout2", "timeout3", "timeout_death_any", "pos_covid_test_present", "covid_hes_present", "covid_death_present", "any_death_present", "treat", "treatgroup", "treatgroup_binary")]

#check missingness patterns
md.pattern(subset_df, rotate.names = TRUE)

subset_df$bmicat[is.na(subset_df$bmicat)] <- "Normal (18.5-24.9)"

table(subset_df$bmicat)

#check number of missings per variable in model
missing_counts <- colSums(is.na(subset_df[, c("age_index", "gender", "imd", "bmicat","eth", "diabetes_present", "hypertension_present", "cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present", "pneumo_vacc_present", "exacerb_present", "smok")]))
missing_counts 

# Define the propensity score formula
ps_formula <- treatgroup_binary ~ age_index + gender + eth + bmicat + diabetes_present + hypertension_present + cvd_present + allcancers_present + asthma_present + kidney_present + immunosuppression_present + flu_vacc_present + pneumo_vacc_present + exacerb_present + smok

# Generating propensity scores as specified in Austin (2011)
ps1 <- glm(treatgroup_binary ~ age_index + gender + eth + bmicat + diabetes_present + hypertension_present + cvd_present + allcancers_present + asthma_present + kidney_present + immunosuppression_present + flu_vacc_present + pneumo_vacc_present + exacerb_present + smok,
                    data = subset_df,
                    family = "binomial")

subset_df$ps <- ps1$fitted.values

# Create a summary of the model
model_summary <- summary(ps1)

# Define the file path for the Excel file
file_path <- file.path(Graphdir, "iptw_diagnostics", "ps_model_results.xlsx")

# Extract the coefficients from the summary and write them to Excel
write.xlsx(data.frame(Coefficient = rownames(model_summary$coefficients), model_summary$coefficients), file_path)
rm("model_summary")

tbl_ps <- ps1 %>%
  tbl_regression(exponentiate = TRUE,
                 label = list(
                   age_index = "Age",
                   gender = "Gender",
                   eth = "Ethnicity",
                   bmicat = "BMI",
                   diabetes_present = "Diabetes",
                   hypertension_present = "Hypertension",
                   cvd_present = "Cardiovascular disease",
                   allcancers_present = "Cancer",
                   asthma_present = "Past asthma",
                   kidney_present = "Kidney impairment",
                   immunosuppression_present = "Immunosuppression",
                   smok = "Smoking",
                   flu_vacc_present = "Influenza vaccine",
                   pneumo_vacc_present = "Pneumococcal vaccine",
                   exacerb_present = "Any exacerbation in past 12 months")) %>%
  add_n(location = "level")

tbl_ps %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, "copd_ps_model.docx"), align = "left")

#save min and max propensity scores in each treatment group, save limits of area of common support
ps_trim <- subset_df %>% 
  select(treat, ps) %>% 
  group_by(treat) %>% 
  summarise(min = min(ps), max = max(ps)) %>% 
  ungroup() %>% 
  summarise(min = max(min), max = min(max))

#exclude people outside of common support
subset_df <- subset_df %>% 
  filter(ps >= ps_trim$min & ps <= ps_trim$max)

# generate ATE weights
ate_stabilised <- weightit(formula = ps_formula,
                         data = subset_df,
                         method = "glm",
                         estimand = "ATE",
                         stabilize = TRUE,
                         focal = NULL,
                         by = NULL,
                         s.weights = NULL,
                         ps = NULL,
                         moments = NULL,
                         int = FALSE,
                         subclass = NULL,
                         missing = NULL,
                         verbose = FALSE,
                         include.obj = FALSE)

ate_unstabilised <- weightit(formula = ps_formula,
                           data = subset_df,
                           method = "glm",
                           estimand = "ATE",
                           stabilize = FALSE,
                           focal = NULL,
                           by = NULL,
                           s.weights = NULL,
                           ps = NULL,
                           moments = NULL,
                           int = FALSE,
                           subclass = NULL,
                           missing = NULL,
                           verbose = FALSE,
                           include.obj = FALSE)

# generate ATT weights
att_unstabilised <- weightit(formula = ps_formula,
                         data = subset_df,
                         method = "glm",
                         estimand = "ATT",
                         stabilize = FALSE,
                         focal = NULL,
                         by = NULL,
                         s.weights = NULL,
                         ps = NULL,
                         moments = NULL,
                         int = FALSE,
                         subclass = NULL,
                         missing = NULL,
                         verbose = FALSE,
                         include.obj = FALSE)

# add ATE and ATT weights to dataset
subset_df$ate_weight_stab <- ate_stabilised[[1]]
subset_df$ate_weight_unstab <- ate_unstabilised[[1]]
subset_df$att_weight_unstab <- att_unstabilised[[1]]

# IPTW Diagnostics --------------------------------------------------------

#PS distributions
ggplot(subset_df, aes(x = ps, color = factor(treatgroup))) +
  geom_density(alpha = 1, size = 0.8) +
  labs(x = "Propensity Score",
       y = "Density",
       title = "Distribution of Propensity Scores by Treatment Group",
       color = NULL) +
  scale_color_manual(values = c(palette[12], palette[9]), labels = c("LABA/LAMA", "ICS/LABA")) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(fill = c(palette[12], palette[9]))))

file_path <- file.path(Graphdir, "iptw_diagnostics", "ps_dist_unweighted.png")
ggsave(file_path, width = 8, height = 4)

#distribution of ATE weighted propensity scores (unstabilised)
ggplot(subset_df, aes(x = ps, color = factor(treatgroup))) +
  geom_density(aes(weight = ate_weight_unstab), size = 0.8) +
  labs(x = "Propensity Score",
       y = "Density",
       title = "Distribution of unstabilised ATE-Weighted Propensity Scores by Treatment Group",
       color = NULL) +
  scale_color_manual(values = c(palette[12], palette[9]), labels = c("LABA/LAMA", "ICS/LABA")) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(fill = c(palette[12], palette[9]))))

file_path <- file.path(Graphdir, "iptw_diagnostics", "ps_dist_ate_unstab.png")
ggsave(file_path, width = 8, height = 4)

#distribution of ATE weighted propensity scores (stabilised)
ggplot(subset_df, aes(x = ps, color = factor(treatgroup))) +
  geom_density(aes(weight = ate_weight_stab), size = 0.8) +
  labs(x = "Propensity Score",
       y = "Density",
       title = "Distribution of stabilised ATE-Weighted Propensity Scores by Treatment Group",
       color = NULL) +
  scale_color_manual(values = c(palette[12], palette[9]), labels = c("LABA/LAMA", "ICS/LABA")) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(fill = c(palette[12], palette[9]))))

file_path <- file.path(Graphdir, "iptw_diagnostics", "ps_dist_ate_stab.png")
ggsave(file_path, width = 8, height = 4)

#distribution of ATT weighted propensity scores
ggplot(subset_df, aes(x = ps, color = factor(treatgroup))) +
  geom_density(aes(weight = att_weight_unstab), size = 0.8) +
  labs(x = "Propensity Score",
       y = "Density",
       title = "Distribution of ATT-Weighted Propensity Scores by Treatment Group",
       color = NULL) +
  scale_color_manual(values = c(palette[12], palette[9]), labels = c("LABA/LAMA", "ICS/LABA")) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(fill = c(palette[12], palette[9]))))

file_path <- file.path(Graphdir, "iptw_diagnostics", "ps_dist_att.png")
ggsave(file_path, width = 8, height = 4)

############
#summarise mean and median ate and att weights by treatment group, with IQR and range
weight_summary <- subset_df %>% 
  group_by(treat) %>% 
  summarise(mean_att_weight_unstab = mean(att_weight_unstab, na.rm = TRUE),
            median_att_weight_unstab = median(att_weight_unstab, na.rm = TRUE),
            p25_att_weight_unstab = quantile(att_weight_unstab, 0.25, na.rm = TRUE),
            p75_att_weight_unstab = quantile(att_weight_unstab, 0.75, na.rm = TRUE),
            min_att_weight_unstab = min(att_weight_unstab, na.rm = TRUE),
            max_att_weight_unstab = max(att_weight_unstab, na.rm = TRUE),
            mean_ate_weight_stab = mean(ate_weight_stab, na.rm = TRUE),
            median_ate_weight_stab = median(ate_weight_stab, na.rm = TRUE),
            p25_ate_weight_stab = quantile(ate_weight_stab, 0.25, na.rm = TRUE),
            p75_ate_weight_stab = quantile(ate_weight_stab, 0.75, na.rm = TRUE),
            min_ate_weight_stab = min(ate_weight_stab, na.rm = TRUE),
            max_ate_weight_stab = max(ate_weight_stab, na.rm = TRUE),
            mean_ate_weight_unstab = mean(ate_weight_unstab, na.rm = TRUE),
            median_ate_weight_unstab = median(ate_weight_unstab, na.rm = TRUE),
            p25_ate_weight_unstab = quantile(ate_weight_unstab, 0.25, na.rm = TRUE),
            p75_ate_weight_unstab = quantile(ate_weight_unstab, 0.75, na.rm = TRUE),
            min_ate_weight_unstab = min(ate_weight_unstab, na.rm = TRUE),
            max_ate_weight_unstab = max(ate_weight_unstab, na.rm = TRUE))
#export this to excel
file_path <- file.path(Graphdir,  "iptw_diagnostics", "weights_summary.xlsx")
write.xlsx(weight_summary, file_path, rowNames = FALSE)


covariates <- c("age_index", "gender", "bmicat", "eth", "diabetes_present", "hypertension_present","cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present",  "pneumo_vacc_present",  "exacerb_present", "smok" )

#assess SMDs
data_ate_unstab <- subset_df[c(covariates, "treatgroup", "ate_weight_unstab")]
data_ate_stab <- subset_df[c(covariates, "treatgroup", "ate_weight_stab")]
data_att_unstab <- subset_df[c(covariates, "treatgroup", "att_weight_unstab")]

#generate bal.tab tables
ate_weighted_table_unstab <- bal.tab(data_ate_unstab, treat = data_ate_unstab$treatgroup, weights = data_ate_unstab$ate_weight_unstab)[[1]]
ate_weighted_table_stab <- bal.tab(data_ate_stab, treat = data_ate_stab$treatgroup, weights = data_ate_stab$ate_weight_stab)[[1]]
att_weighted_table_unstab <- bal.tab(data_att_unstab, treat = data_att_unstab$treatgroup, weights = data_att_unstab$att_weight_unstab)[[1]]
bal_unweighted_table <- bal.tab(data_ate_unstab, treat = data_ate_unstab$treatgroup)[[1]]

#remove the treatgroup and weight rows from the table
ate_weighted_table_unstab <- ate_weighted_table_unstab[!rownames(ate_weighted_table_unstab) %in% c("treatgroup", "ate_weight_unstab"), ]
ate_weighted_table_stab <- ate_weighted_table_stab[!rownames(ate_weighted_table_stab) %in% c("treatgroup", "ate_weight_stab"), ]
att_weighted_table_unstab <- att_weighted_table_unstab[!rownames(att_weighted_table_unstab) %in% c("treatgroup", "att_weight_unstab"), ]
bal_unweighted_table <- bal_unweighted_table[!rownames(bal_unweighted_table) %in% c("treatgroup", "ate_weight_unstab"), ]

#check that rownames are the same
all.equal(rownames(ate_weighted_table_unstab), rownames(ate_weighted_table_stab), rownames(att_weighted_table_unstab), rownames(bal_unweighted_table))

# generate a dataframe with the SMDs, unweighted, ATT and ATE weighted
smd_data <- data.frame(variable = rownames(ate_weighted_table_unstab),
                       smd_att_unstab = att_weighted_table_unstab$Diff.Adj,
                       smd_ate_unstab = ate_weighted_table_unstab$Diff.Adj,
                       smd_ate_stab = ate_weighted_table_stab$Diff.Adj,
                       smd_unweighted = bal_unweighted_table$Diff.Un)

#generate absolute SMDs
smd_data$abs_smd_att_unstab <- abs(smd_data$smd_att_unstab)
smd_data$abs_smd_ate_unstab <- abs(smd_data$smd_ate_unstab)
smd_data$abs_smd_ate_stab <- abs(smd_data$smd_ate_stab)
smd_data$abs_smd_unweighted <- abs(smd_data$smd_unweighted)

#export this to excel
file_path <- file.path(Graphdir,  "iptw_diagnostics", "smd_table.xlsx")
write.xlsx(smd_data, file_path, rowNames = FALSE)

# Create plot of SMDs
ggplot(smd_data, aes(x = abs_smd_ate_stab, y = variable)) +
  geom_point(aes(shape = "ATE", fill = "ATE"), color = palette[1], size = 3) +
  geom_point(data = smd_data, aes(x = abs_smd_att_unstab, shape = "ATT", fill = "ATT"), color = palette[3], size = 2) +
  geom_point(data = smd_data, aes(x = abs_smd_unweighted, shape = "unweighted", fill = "unweighted"), color = palette[5], size = 2) +
  scale_shape_manual(values = c("ATE" = 23, "ATT" = 21, "unweighted" = 24)) +
  scale_fill_manual(values = c("ATE" = palette[1], "ATT" = palette[3], "unweighted" = palette[5])) +
  labs(title = "Comparison of Absolute SMDs",
       x = "Absolute Standardized Mean Difference (SMD)",
       y = "Variable") +
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(color = c(palette[1], palette[3], palette[5])))) +
  labs(fill = NULL, shape = NULL)


file_path <- file.path(Graphdir, "iptw_diagnostics", "SMD_plot.png")
ggsave(file_path, width = 8, height = 4)


#comparing baseline weighted tables: ATE (stabilised)
tab_data <- subset_df %>% select(c("age_index", "gender", "bmicat", "eth", "diabetes_present", "hypertension_present","cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present",  "pneumo_vacc_present",  "exacerb_present", "smok", "treat", "pos_covid_test_present", "covid_hes_present", "covid_death_present", "ate_weight_stab"))

#comparing baseline covariates in weighted and unweighted samples
design <- svydesign(ids = ~1, weights = ~ate_weight_stab, data = tab_data)

tab2 <- tbl_svysummary(data = design,
                       by = treat,
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
                                    immunosuppression_present ~ "Immunosuppression",
                                    smok ~ "Smoking",
                                    flu_vacc_present ~ "Influenza vaccine",
                                    pneumo_vacc_present ~ "Pneumococcal vaccine",
                                    pos_covid_test_present ~ "Positive COVID-19 test",
                                    covid_hes_present ~ "COVID-19 hospitalisation",
                                    covid_death_present ~ "COVID-19 death",
                                    exacerb_present ~ "Any exacerbation in past 12 months",
                                    ate_weight_stab ~ "ATE weight (stabilised)"),
                       percent = "column",
                       digits = all_continuous() ~ 2,
                       missing = "ifany",
                       missing_text = "Missing",
                       statistic = list(
                         all_continuous() ~ "{mean} ({sd})",
                         age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                         all_categorical() ~ "{n} ({p}%)"),
                       type = list(
                         c(age_index) ~ "continuous2"))  %>% 
  add_p() %>%
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels() 

#export to word
file_path <- file.path(Graphdir, "iptw_diagnostics", "baseline_table_ate_stab.docx")
tab2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = file_path, align = "left")

#ATE (unstabilised)
tab_data <- subset_df %>% select(c("age_index", "gender", "bmicat", "eth", "diabetes_present", "hypertension_present","cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present",  "pneumo_vacc_present",  "exacerb_present", "smok", "treat", "pos_covid_test_present", "covid_hes_present", "covid_death_present", "ate_weight_unstab"))

#comparing baseline covariates in weighted and unweighted samples
design <- svydesign(ids = ~1, weights = ~ate_weight_unstab, data = tab_data)

tab2 <- tbl_svysummary(data = design,
                       by = treat,
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
                                    immunosuppression_present ~ "Immunosuppression",
                                    smok ~ "Smoking",
                                    flu_vacc_present ~ "Influenza vaccine",
                                    pneumo_vacc_present ~ "Pneumococcal vaccine",
                                    pos_covid_test_present ~ "Positive COVID-19 test",
                                    covid_hes_present ~ "COVID-19 hospitalisation",
                                    covid_death_present ~ "COVID-19 death",
                                    exacerb_present ~ "Any exacerbation in past 12 months",
                                    ate_weight_unstab ~ "ATE weight (unstabilised)"),
                       percent = "column",
                       digits = all_continuous() ~ 2,
                       missing = "ifany",
                       missing_text = "Missing",
                       statistic = list(
                         all_continuous() ~ "{mean} ({sd})",
                         age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                         all_categorical() ~ "{n} ({p}%)"),
                       type = list(
                         c(age_index) ~ "continuous2"))  %>% 
  add_p() %>%
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels() 

#export to word
file_path <- file.path(Graphdir, "iptw_diagnostics", "baseline_table_ate_unstab.docx")
tab2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = file_path, align = "left")

#ATT weighted baseline table

tab_data <- subset_df %>% select(c("age_index", "gender", "bmicat", "eth", "diabetes_present", "hypertension_present","cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present",  "pneumo_vacc_present",  "exacerb_present", "smok", "treat", "pos_covid_test_present", "covid_hes_present", "covid_death_present", "att_weight_unstab"))

#comparing baseline covariates in weighted and unweighted samples
design <- svydesign(ids = ~1, weights = ~att_weight_unstab, data = tab_data)

tab3 <- tbl_svysummary(data = design,
                       by = treat,
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
                                    immunosuppression_present ~ "Immunosuppression",
                                    smok ~ "Smoking",
                                    flu_vacc_present ~ "Influenza vaccine",
                                    pneumo_vacc_present ~ "Pneumococcal vaccine",
                                    pos_covid_test_present ~ "Positive COVID-19 test",
                                    covid_hes_present ~ "COVID-19 hospitalisation",
                                    covid_death_present ~ "COVID-19 death",
                                    exacerb_present ~ "Any exacerbation in past 12 months",
                                    att_weight_unstab ~ "ATT weight"),
                       percent = "column",
                       digits = all_continuous() ~ 2,
                       missing = "ifany",
                       missing_text = "Missing",
                       statistic = list(
                         all_continuous() ~ "{mean} ({sd})",
                         age_index ~ c("{mean} ({sd})", "{median}  \n ({p25}-{p75})"),
                         all_categorical() ~ "{n} ({p}%)"),
                       type = list(
                         c(age_index) ~ "continuous2"))  %>% 
  add_p() %>%
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels() 

#export to word
file_path <- file.path(Graphdir, "iptw_diagnostics", "baseline_table_att_unstab.docx")
tab3 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = file_path, align = "left")


# Create an empty summary dataframe
iptw_dist <- data.frame(
  Treat = character(),
  Mean = numeric(),
  SD = numeric(),
  Median = numeric(),
  Q25 = numeric(),
  Q75 = numeric(),
  min = numeric(),
  max = numeric())

#specify the weight variable
weight_var <- "ate_weight_stab"

# Create a function to plot IPTW weight distributions
plot_weight_distribution <- function(weights) {
  ggplot(data.frame(Weights = weights), aes(x = Weights)) +
    geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black") +
    labs(x = "IPTW Weights", y = "Frequency", title = "Distribution of IPTW Weights")
}

# Loop through each treatment level
for (treat_level in unique(subset_df$treat)) {
  
  weights_subset <- unlist(subset_df[subset_df$treat == treat_level,"ate_weight_stab"])
  
  # Calculate statistics
  mean_weight <- mean(weights_subset, na.rm = TRUE)
  sd_weight <- sd(weights_subset, na.rm = TRUE)
  median_weight <- median(weights_subset, na.rm = TRUE)
  q25 <- quantile(weights_subset, 0.25, na.rm = TRUE)
  q75 <- quantile(weights_subset, 0.75, na.rm = TRUE)
  min_weight <- min(weights_subset, na.rm = TRUE)
  max_weight <- max(weights_subset, na.rm = TRUE)
  
  # Add statistics to the summary dataframe
  iptw_dist <- rbind(iptw_dist, data.frame(
    Treat = treat_level,
    Mean = mean_weight,
    SD = sd_weight,
    Median = median_weight,
    Q25 = q25,
    Q75 = q75,
    min = min_weight,
    max = max_weight))
  # 
  # Print statistics
  cat("Treatment:", treat_level, "\n")
  cat("Mean:", mean_weight, "\n")
  cat("Standard Deviation:", sd_weight, "\n")
  cat("Median:", median_weight, "\n")
  cat("IQR (25th percentile):", q25, "\n")
  cat("IQR (75th percentile):", q75, "\n\n")

  # Call the plot function with correct argument name
  print(plot_weight_distribution(weights_subset))
  
  rm(list = c("weights_subset", mean_weight, sd_weight, median_weight, q25, q75, min_weight, max_weight))
}

overall_mean <- mean(subset_df$ate_weight_stab, na.rm = TRUE)
overall_sd <- sd(subset_df$ate_weight_stab, na.rm = TRUE)
overall_median <- median(subset_df$ate_weight_stab, na.rm = TRUE)
overall_q25 <- quantile(subset_df$ate_weight_stab, 0.25, na.rm = TRUE)
overall_q75 <- quantile(subset_df$ate_weight_stab, 0.75, na.rm = TRUE)
overall_min <- min(subset_df$ate_weight_stab, na.rm = TRUE)
overall_max <- max(subset_df$ate_weight_stab, na.rm = TRUE)

# Add overall statistics to the summary dataframe
iptw_dist <- rbind(iptw_dist, data.frame(
  Treat = "Overall",
  Mean = overall_mean,
  SD = overall_sd,
  Median = overall_median,
  Q25 = overall_q25,
  Q75 = overall_q75,
  min = overall_min,
  max = overall_max))

print(iptw_dist)

rm(list = c("overall_mean", "overall_sd", "overall_median", "overall_q25", "overall_q75", "overall_min", "overall_max"))

#save subset_df as a parquet file
write_parquet(subset_df, "copd_wave1_60d_subset_iptw_ate_stab.parquet")

# COX REGRESSION (stabilised)----------------------------------------------------------

#create object to save results
estimates <- data.frame()

# Define svy design for unadjusted 
unadj <- svydesign(ids = ~ 1,
                   data = subset_df)

# Define svy design for IPTW 
iptw <- svydesign(ids = ~ 1,
                  data = subset_df,
                  weights = ~ ate_weight_unstab)

outcomes <- c("pos_covid_test_present", "covid_hes_present", "covid_death_present", "any_death_present")

#kaplan meier plot for iptw weighted data
km1 <- svykm(as.formula(paste("Surv(", "timeinstudy1", ", ", "pos_covid_test_present", ") ~ treat")), design = iptw)
km1_plot <- svyjskm(km1, xlabs = "Time-to-event (days)",
                    ylabs = "Survival probability", ylims = c(0.95, 1), legend = TRUE)
km1_plot <- km1_plot + scale_color_manual(values = c(palette[1], palette[5])) + guides(color = FALSE) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.5))
#save this plot
file_path <- file.path(Graphdir, "cox_regression", "km1_iptw.png")
png(file_path)
print(km1_plot)
dev.off()

#kaplan meier plot for iptw weighted data
km2 <- svykm(as.formula(paste("Surv(", "timeinstudy2", ", ", "covid_hes_present", ") ~ treat")), design = iptw)
km2_plot <- svyjskm(km2, xlabs = "Time-to-event (days)",
                    ylabs = "Survival probability", ylims = c(0.9, 1), legend = TRUE)
km2_plot <- km2_plot + scale_color_manual(values = c(palette[1], palette[5])) + guides(color = FALSE) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.5))
#save this plot
file_path <- file.path(Graphdir, "cox_regression", "km2_iptw.png")
png(file_path)
print(km2_plot)
dev.off()

#kaplan meier plot for iptw weighted data
km3 <- svykm(as.formula(paste("Surv(", "timeinstudy3", ", ", "covid_death_present", ") ~ treat")), design = iptw)
km3_plot <- svyjskm(km3, xlabs = "Time-to-event (days)",
                    ylabs = "Survival probability", ylims = c(0.9, 1), legend = TRUE)
km3_plot <- km3_plot + scale_color_manual(values = c(palette[1], palette[5])) + guides(color = FALSE) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.5))
#save this plot
file_path <- file.path(Graphdir, "cox_regression", "km3_iptw.png")
png(file_path)
print(km3_plot)
dev.off()


# Loop through each outcome and fit the unadjusted and IPTW models
for (j in seq_along(outcomes)){
  outcome_event <- outcomes[j]
  print(j)
  
  # Determine the position of the current outcome in the 'outcomes' vector
  outcome_position <- which(outcomes == outcome_event)
  
  # Use the 'switch' function to select the appropriate time-in-study variable
  timeinstudy_var <- switch(outcome_position,
                            as.name("timeinstudy1"),
                            as.name("timeinstudy2"),
                            as.name("timeinstudy3"),
                            as.name("timeinstudy_death_any"))
  print(timeinstudy_var)
  
  outcome_label <- switch(outcome_position,
                          as.name("positive COVID-19 test"),
                          as.name("COVID-19 hospitalisation"),
                          as.name("COVID-19 death"),
                          as.name("all-cause mortality"))
  print(outcome_label)
  # Formula for unadjusted model using the selected time-in-study variable
  formula_unadj <- as.formula(paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
  
  # Fit the unadjusted model
  model_unadj <- coxph(formula_unadj, 
                       data = subset_df)
  
  summary_unadj <- summary(model_unadj)

  #plot schoenfeld residuals
   schoenfeld_resid <- cox.zph(model_unadj)
   file_path <- file.path(Graphdir, "cox_regression", paste0("schoenfeld_resid_",outcome_event,"_unadj",".png"))
   png(file_path)
   
   plot(schoenfeld_resid, main = paste("Schoenfeld residuals for", outcome_label, "unadjusted model"))
   dev.off()

   # Formula for IPTW model using the selected time-in-study variable
   formula_iptw <- as.formula(paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
   
   # Fit the IPTW model
   model_iptw <- coxph(formula_iptw, 
                       data = subset_df,
                       weights = subset_df$ate_weight_unstab)   

  # #plot schoenfeld residuals
  schoenfeld_resid <- cox.zph(model_iptw, transform = 'identity')

  # #save plot
  file_path <- file.path(Graphdir, "cox_regression", paste0("schoenfeld_resid_",outcome_event,"_IPTW",".png"))
  png(file_path)
  plot(schoenfeld_resid, main = paste("Schoenfeld residuals for", outcome_label, "IPTW model"))
  dev.off()

    # Extract coefficients and confidence intervals for unadjusted model
    summary_unadj <- summary(model_unadj)
    print(summary_unadj)
    
    coef_unadj <- model_unadj$coefficients
    print(paste("Coefficient unadjusted:", coef_unadj))
          
    hr_unadj <- coef_unadj %>% exp()
    print(paste("HR unadjusted:", hr_unadj))
    
    #extract the standard error of the coefficient
    se_unadj <- summary_unadj$coef["treatICS/LABA", "se(coef)"]
    print(paste("se unadj:", se_unadj))
    
    #calculate the CI using the normal SE
    ci_unadj <- (coef_unadj + c(-1, 1) * qnorm(0.975) * se_unadj) %>% exp()
    print(paste("ci unadj:", ci_unadj))

    # Extract coefficients and confidence intervals for IPTW model
    summary_iptw <- summary(model_iptw)
    print(summary_iptw)
    
    coef_iptw <- model_iptw$coefficients
    print(paste("Coefficient IPTW:", coef_iptw))
    hr_iptw <- coef_iptw %>% exp()
    print(paste("HR IPTW:", hr_iptw))
    
    se_iptw <- summary_iptw$coef["treatICS/LABA", "se(coef)"]
    print(paste("se iptw:", se_iptw))
    
    se_iptw_robust <- summary_iptw$coef["treatICS/LABA", "robust se"]
    print(paste("se unadj:", se_iptw_robust))
    
    #calculate the CI using the robust SE
    ci_iptw <- (coef_iptw + c(-1, 1) * qnorm(0.975) * se_iptw_robust) %>% exp()
    print(paste("ci iptw:", ci_iptw))

    # Save results in the 'estimates' dataframe
    result_position <- j  # Starting position for the outcome
    
    #save estimates to dataframe
    estimates[result_position, "outcome_event"] <- gsub("_present", "", outcome_event)
    estimates[result_position, "coef_unadj"] <- coef_unadj
    estimates[result_position, "hr_unadj"] <- hr_unadj
    estimates[result_position, "se_unadj"] <- se_unadj
    estimates[result_position, "ci_unadj_lower"] <- ci_unadj[1]
    estimates[result_position, "ci_unadj_upper"] <- ci_unadj[2]
    estimates[result_position, "coef_iptw"] <- coef_iptw
    estimates[result_position, "hr_iptw"] <- hr_iptw
    estimates[result_position, "se_iptw_normal"] <- se_iptw
    estimates[result_position, "se_iptw_robust"] <- se_iptw_robust
    estimates[result_position, "ci_iptw_lower"] <- ci_iptw[1]
    estimates[result_position, "ci_iptw_upper"] <- ci_iptw[2]
    

  rm(list = c("coef_unadj", "hr_unadj", "se_unadj", "ci_unadj", "coef_iptw", "hr_iptw", "se_iptw", "se_iptw_robust", "ci_iptw"))
  
}

#save estimates to parquet
write_parquet(estimates, "cox_regression_estimates.parquet")


# COX REGRESSION (stabilised)----------------------------------------------------------

#create object to save results
estimates <- data.frame()

# Define svy design for unadjusted 
unadj <- svydesign(ids = ~ 1,
                   data = subset_df)

# Define svy design for IPTW 
iptw <- svydesign(ids = ~ 1,
                  data = subset_df,
                  weights = ~ ate_weight_unstab)

outcomes <- c("pos_covid_test_present", "covid_hes_present", "covid_death_present", "any_death_present")

#kaplan meier plot for iptw weighted data
km1 <- svykm(as.formula(paste("Surv(", "timeinstudy1", ", ", "pos_covid_test_present", ") ~ treat")), design = iptw)
km1_plot <- svyjskm(km1, xlabs = "Time-to-event (days)",
                    ylabs = "Survival probability", ylims = c(0.95, 1), legend = TRUE)
km1_plot <- km1_plot + scale_color_manual(values = c(palette[1], palette[5])) + guides(color = FALSE) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.5))

file_path <- file.path(Graphdir, "cox_regression", "km1_iptw.png")
png(file_path)
print(km1_plot)
dev.off()

#kaplan meier plot for iptw weighted data
km2 <- svykm(as.formula(paste("Surv(", "timeinstudy2", ", ", "covid_hes_present", ") ~ treat")), design = iptw)
km2_plot <- svyjskm(km2, xlabs = "Time-to-event (days)",
                    ylabs = "Survival probability", ylims = c(0.95, 1), legend = TRUE)
km2_plot <- km2_plot + scale_color_manual(values = c(palette[1], palette[5])) + guides(color = FALSE) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.5))

file_path <- file.path(Graphdir, "cox_regression", "km2_iptw.png")
png(file_path)
print(km2_plot)
dev.off()

#kaplan meier plot for iptw weighted data
km3 <- svykm(as.formula(paste("Surv(", "timeinstudy3", ", ", "covid_death_present", ") ~ treat")), design = iptw)
km3_plot <- svyjskm(km3, xlabs = "Time-to-event (days)",
                    ylabs = "Survival probability", ylims = c(0.95, 1), legend = TRUE)
km3_plot <- km3_plot + scale_color_manual(values = c(palette[1], palette[5])) + guides(color = FALSE) + theme(legend.title = element_blank(), legend.position = c(0.9, 0.5))

file_path <- file.path(Graphdir, "cox_regression", "km3_iptw.png")
png(file_path)
print(km3_plot)
dev.off()

# Loop through each outcome and fit the unadjusted and IPTW models
for (j in seq_along(outcomes)){
  outcome_event <- outcomes[j]
  print(j)
  
  # Determine the position of the current outcome in the 'outcomes' vector
  outcome_position <- which(outcomes == outcome_event)
  
  # Use the 'switch' function to select the appropriate time-in-study variable
  timeinstudy_var <- switch(outcome_position,
                            as.name("timeinstudy1"),
                            as.name("timeinstudy2"),
                            as.name("timeinstudy3"),
                            as.name("timeinstudy_death_any"))
  print(timeinstudy_var)
  
  outcome_label <- switch(outcome_position,
                          as.name("positive COVID-19 test"),
                          as.name("COVID-19 hospitalisation"),
                          as.name("COVID-19 death"),
                          as.name("all-cause mortality"))
  print(outcome_label)
  # Formula for unadjusted model using the selected time-in-study variable
  formula_unadj <- as.formula(paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
  
  # Fit the unadjusted model
  model_unadj <- coxph(formula_unadj, 
                       data = subset_df)
  
  summary_unadj <- summary(model_unadj)

  #plot schoenfeld residuals
   schoenfeld_resid <- cox.zph(model_unadj)
   file_path <- file.path(Graphdir, "cox_regression", paste0("schoenfeld_resid_",outcome_event,"_unadj",".png"))
   png(file_path)
   
   plot(schoenfeld_resid, main = paste("Schoenfeld residuals for", outcome_label, "unadjusted model"))
   dev.off()

   # Formula for IPTW model using the selected time-in-study variable
   formula_iptw <- as.formula(paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
   
   # Fit the IPTW model
   model_iptw <- coxph(formula_iptw, 
                       data = subset_df,
                       weights = subset_df$ate_weight_unstab)   

  # #plot schoenfeld residuals
  schoenfeld_resid <- cox.zph(model_iptw, transform = 'identity')

  # #save plot
  file_path <- file.path(Graphdir, "cox_regression", paste0("schoenfeld_resid_",outcome_event,"_IPTW",".png"))
  png(file_path)
  plot(schoenfeld_resid, main = paste("Schoenfeld residuals for", outcome_label, "IPTW model"))
  dev.off()

    # Extract coefficients and confidence intervals for unadjusted model
    summary_unadj <- summary(model_unadj)
    print(summary_unadj)
    
    coef_unadj <- model_unadj$coefficients
    print(paste("Coefficient unadjusted:", coef_unadj))
          
    hr_unadj <- coef_unadj %>% exp()
    print(paste("HR unadjusted:", hr_unadj))
    
    #extract the standard error of the coefficient
    se_unadj <- summary_unadj$coef["treatICS/LABA", "se(coef)"]
    print(paste("se unadj:", se_unadj))
    
    #calculate the CI using the normal SE
    ci_unadj <- (coef_unadj + c(-1, 1) * qnorm(0.975) * se_unadj) %>% exp()
    print(paste("ci unadj:", ci_unadj))

    # Extract coefficients and confidence intervals for IPTW model
    summary_iptw <- summary(model_iptw)
    print(summary_iptw)
    
    coef_iptw <- model_iptw$coefficients
    print(paste("Coefficient IPTW:", coef_iptw))
    hr_iptw <- coef_iptw %>% exp()
    print(paste("HR IPTW:", hr_iptw))
    
    se_iptw <- summary_iptw$coef["treatICS/LABA", "se(coef)"]
    print(paste("se iptw:", se_iptw))
    
    se_iptw_robust <- summary_iptw$coef["treatICS/LABA", "robust se"]
    print(paste("se unadj:", se_iptw_robust))
    
    #calculate the CI using the robust SE
    ci_iptw <- (coef_iptw + c(-1, 1) * qnorm(0.975) * se_iptw_robust) %>% exp()
    print(paste("ci iptw:", ci_iptw))

    # Save results in the 'estimates' dataframe
    result_position <- j  # Starting position for the outcome
    
    #save estimates to dataframe
    estimates[result_position, "outcome_event"] <- gsub("_present", "", outcome_event)
    estimates[result_position, "coef_unadj"] <- coef_unadj
    estimates[result_position, "hr_unadj"] <- hr_unadj
    estimates[result_position, "se_unadj"] <- se_unadj
    estimates[result_position, "ci_unadj_lower"] <- ci_unadj[1]
    estimates[result_position, "ci_unadj_upper"] <- ci_unadj[2]
    estimates[result_position, "coef_iptw"] <- coef_iptw
    estimates[result_position, "hr_iptw"] <- hr_iptw
    estimates[result_position, "se_iptw_normal"] <- se_iptw
    estimates[result_position, "se_iptw_robust"] <- se_iptw_robust
    estimates[result_position, "ci_iptw_lower"] <- ci_iptw[1]
    estimates[result_position, "ci_iptw_upper"] <- ci_iptw[2]
    

  rm(list = c("coef_unadj", "hr_unadj", "se_unadj", "ci_unadj", "coef_iptw", "hr_iptw", "se_iptw", "se_iptw_robust", "ci_iptw"))
  
}

#save estimates to parquet
write_parquet(estimates, "cox_regression_estimates.parquet")


# check ps model ----------------------------------------------------------


#check of PS model: #restrict population to those with PS ~0.7 to see why the dip in propensity score occurs.

#restrict population to those with PS between 0.65 and 0.75
subset_df <- subset_df %>% 
  filter(ps >= 0.68 & ps <= 0.73)

tab1 <- tbl_summary(subset_df %>% select(age_index, gender, bmicat, eth, smok,  diabetes_present, hypertension_present, cvd_present, allcancers_present, asthma_present, kidney_present, immunosuppression_present, flu_vacc_present, pneumo_vacc_present,  exacerb_present, pos_covid_test_present, covid_hes_present, covid_death_present, treat),
                    by = treat,
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
                                 immunosuppression_present ~ "Immunosuppression",
                                 smok ~ "Smoking",
                                 flu_vacc_present ~ "Influenza vaccine",
                                 pneumo_vacc_present ~ "Pneumococcal vaccine",
                                 pos_covid_test_present ~ "Positive COVID-19 test",
                                 covid_hes_present ~ "COVID-19 hospitalisation",
                                 covid_death_present ~ "COVID-19 death",
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
  add_p() %>%
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  # modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels()

tab1
#export to word
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(Tables, "copd_baseline_w1_60d_ps0.7.docx"), align = "left")


####check SMDs
install.packages("tidysmd")
library(tidysmd)
smd_result <- tidy_smd(.df = subset_df, .vars = diabetes_present, .group = treat, .wts = ate_weight_stab)
