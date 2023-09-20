# Author: Marleen Bokern
# Date: 03/2023
# Purpose: generate propensity scores and IPTW weights for COPD cohort. Fit Cox models for all three outcomes.

packages <- c("tidyverse", "MetBrewer", "arrow", "MatchThem", "mice", "parallelly", "furrr", "survey", "cobalt", "ggplot2", "twang", "ipw", "WeightIt", "gtsummary", "dplyr", "flextable", "openxlsx")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#########################################################
setwd(Datadir_copd)

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

palette <- met.brewer("Renoir")

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
subset_df$ps <- glm(treatgroup_binary ~ age_index + gender + eth + bmicat + diabetes_present + hypertension_present + cvd_present + allcancers_present + asthma_present + kidney_present + immunosuppression_present + flu_vacc_present + pneumo_vacc_present + exacerb_present + smok,
                    data = subset_df,
                    family = "binomial")$fitted.values

# generate ATE weights
ate_weightit <- weightit(formula = ps_formula,
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

# generate ATT weights
att_weightit <- weightit(formula = ps_formula,
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
subset_df$att_weight <- att_weightit[[1]]
subset_df$ate_weight <- ate_weightit[[1]]

# IPTW Diagnostics --------------------------------------------------------

#PS distributions
ggplot(subset_df, aes(x = ps, color = factor(treatgroup))) +
  geom_density(alpha = 0.6) +
  labs(x = "Propensity Score",
       y = "Density",
       title = "Distribution of Propensity Scores by Treatment Group",
       color = NULL) +
  scale_color_manual(values = c(palette[12], palette[9]), labels = c("LABA/LAMA", "ICS/LABA")) +
  theme_minimal()

file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "ps_dist_unweighted.png")
ggsave(file_path, width = 8, height = 4)

#distribution of weighted propensity scores
ggplot(subset_df, aes(x = ps, color = factor(treatgroup))) +
  geom_density(aes(weight = ate_weight), alpha = 0.6) +
  labs(x = "Propensity Score",
       y = "Density",
       title = "Distribution of Weighted Propensity Scores by Treatment Group",
       color = NULL) +
  scale_color_manual(values = c(palette[12], palette[9]), labels = c("LABA/LAMA", "ICS/LABA")) +
  theme_minimal()

file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "ps_dist_weighted.png")
ggsave(file_path, width = 8, height = 4)


#assess distribution of IPTWs
ggplot(subset_df, aes(x = att_weight, fill = factor(treatgroup))) +
  geom_density(alpha = 0.5) +
  labs(x = "IPTW",
       y = "Density",
       title = "Density Plot of IPTWs by Treatment Group") +
  coord_cartesian(xlim = c(0.5,1.5)) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5)) + 
  scale_fill_manual(values = c(palette[5], palette[12]), labels = c("LABA/LAMA", "ICS/LABA")) +
  guides(fill = guide_legend(title = "Treatment Group")) +
  theme_minimal()

file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "att_iptw_dist.png")
ggsave(file_path, width = 8, height = 4)

#assess distribution of IPTWs
ggplot(subset_df, aes(x = ate_weight, fill = factor(treatgroup))) +
  geom_density(alpha = 0.5) +
  labs(x = "IPTW",
       y = "Density",
       title = "Density Plot of IPTWs by Treatment Group") +
  coord_cartesian(xlim = c(0,3)) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5)) + 
  scale_fill_manual(values = c(palette[5], palette[12]), labels = c("LABA/LAMA", "ICS/LABA")) +
  guides(fill = guide_legend(title = "Treatment Group")) +
  theme_minimal()

file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "ate_iptw_dist.png")
ggsave(file_path, width = 8, height = 4)


covariates <- c("age_index", "gender", "bmicat", "eth", "diabetes_present", "hypertension_present","cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present",  "pneumo_vacc_present",  "exacerb_present", "smok" )

#assess SMDs
data_ate <- subset_df[c(covariates, "treatgroup", "ate_weight")]
data_att <- subset_df[c(covariates, "treatgroup", "att_weight")]

#generate bal.tab tables
ate_weighted_table <- bal.tab(data_ate, treat = data_ate$treatgroup, weights = data_ate$ate_weight)[[1]]
att_weighted_table <- bal.tab(data_att, treat = data_att$treatgroup, weights = data_att$att_weight)[[1]]
bal_unweighted_table <- bal.tab(data_ate, treat = data_ate$treatgroup)[[1]]

#remove the treatgroup and ate_weight rows from the table
ate_weighted_table <- ate_weighted_table[!rownames(ate_weighted_table) %in% c("treatgroup", "ate_weight"), ]
att_weighted_table <- att_weighted_table[!rownames(att_weighted_table) %in% c("treatgroup", "att_weight"), ]
bal_unweighted_table <- bal_unweighted_table[!rownames(bal_unweighted_table) %in% c("treatgroup", "ate_weight"), ]

#check that rownames are the same
all.equal(rownames(ate_weighted_table), rownames(att_weighted_table), rownames(att_weighted_table), rownames(bal_unweighted_table))

# generate a dataframe with the SMDs, unweighted, ATT and ATE weighted
smd_data <- data.frame(variable = rownames(ate_weighted_table),
                       smd_att = att_weighted_table$Diff.Adj,
                       smd_ate = ate_weighted_table$Diff.Adj,
                       smd_unweighted = bal_unweighted_table$Diff.Un)

#generate absolute SMDs
smd_data$abs_smd_att <- abs(smd_data$smd_att)
smd_data$abs_smd_ate <- abs(smd_data$smd_ate)
smd_data$abs_smd_unweighted <- abs(smd_data$smd_unweighted)

#export this to excel
file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "smd_table.xlsx")
write.xlsx(smd_data, file_path, row.names = FALSE)

#create plot of SMDs
ggplot(smd_data, aes(x = abs_smd_ate, y = variable)) +
  geom_point(aes(shape = "ATE", fill = "ATE"), color = palette[1], size = 3) +
  geom_point(data = smd_data, aes(x = abs_smd_att, shape = "ATT", fill = "ATT"), color = palette[4], size = 2) +
  geom_point(data = smd_data, aes(x = abs_smd_unweighted, shape = "unweighted", fill = "unweighted"), color = palette[5], size = 2) +
  scale_shape_manual(values = c("ATT" = 21, "ATE" = 23, "unweighted" = 24)) +
  scale_fill_manual(values = c("ATE" = palette[1], "ATT" = palette[4], "unweighted" = palette[5])) +
  labs(title = "Comparison of Absolute SMDs",
       x = "Absolute Standardized Mean Difference (SMD)",
       y = "Variable") +
  theme_minimal() +
  guides(guides(fill=FALSE, shape = guide_legend(override.aes = list(shape = c(23,21, 24), color = c(palette[1], palette[4], palette[5]), fill = c(palette[1], palette[4], palette[5]))))) +
  labs(fill = NULL, shape = NULL)

file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "SMD_plot.png")
ggsave(file_path, width = 8, height = 4)


tab_data <- subset_df %>% select(c("age_index", "gender", "bmicat", "eth", "diabetes_present", "hypertension_present","cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present",  "pneumo_vacc_present",  "exacerb_present", "smok", "treat", "pos_covid_test_present", "covid_hes_present", "covid_death_present", "ate_weight"))

#comparing baseline covariates in weighted and unweighted samples
design <- svydesign(ids = ~1, weights = ~ate_weight, data = tab_data)

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
                                    ate_weight ~ "ATE weight"),
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
  add_p() %>%
  modify_header(label ~ "", all_stat_cols() ~ "**{level}**  \n N = {n}")  %>%
  modify_caption("Patient Characteristics") %>%
  modify_column_alignment(columns = c(stat_1, stat_2), align = "right") %>% 
  italicize_levels() 

#export to word
file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "baseline_table_ate.docx")
tab2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = file_path, align = "left")


ggplot(subset_df, aes(x = age_index, color = treat)) +
  stat_ecdf(geom = "line", size = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  scale_color_manual(values = c("LABA/LAMA" = palette[1], "ICS/LABA" = palette[8])) +
  labs(title = "ECDF of age at baseline by Treatment Group",
       x = "Age at baseline",
       y = "Cumulative Probability") +
  theme_minimal() +
  guides(color = guide_legend(title = NULL))

file_path <- file.path(Github_folder, "output", "iptw_diagnostics", "age_cdf_plot.png")
ggsave(file_path, width = 8, height = 4)



subset_df %>%
  group_by(treatgroup) %>%
  summarize(Min_Propensity = min(ps),
            Max_Propensity = max(ps))

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

# Create a function to plot IPTW weight distributions
plot_weight_distribution <- function(weights) {
  ggplot(data.frame(Weights = weights), aes(x = Weights)) +
    geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black") +
    labs(x = "IPTW Weights", y = "Frequency", title = "Distribution of IPTW Weights")
}

# Loop through each treatment level
for (treat_level in unique(subset_df$treat)) {
  
  weights_subset <- subset_df$ate_weight[subset_df$treat == treat_level]
  
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

overall_mean <- mean(subset_df$ate_weight, na.rm = TRUE)
overall_sd <- sd(subset_df$ate_weight, na.rm = TRUE)
overall_median <- median(subset_df$ate_weight, na.rm = TRUE)
overall_q25 <- quantile(subset_df$ate_weight, 0.25, na.rm = TRUE)
overall_q75 <- quantile(subset_df$ate_weight, 0.75, na.rm = TRUE)
overall_min <- min(subset_df$ate_weight, na.rm = TRUE)
overall_max <- max(subset_df$ate_weight, na.rm = TRUE)

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
write_parquet(subset_df, "copd_wave1_60d_subset_iptw.parquet")

# COX REGRESSION ----------------------------------------------------------

#create object to save results
estimates <- data.frame()

# Define svy design for unadjusted 
unadj <- svydesign(ids = ~ 1,
                   data = subset_df)

# Define svy design for IPTW 
iptw <- svydesign(ids = ~ 1,
                  data = subset_df,
                  weights = ~ att_weight)

outcomes <- c("pos_covid_test_present", "covid_hes_present", "covid_death_present", "any_death_present")

# Loop through each outcome and fit the unadjusted and IPTW models
for (j in seq_along(outcomes)){
  outcome_event <- outcomes[j]
  
  # Determine the position of the current outcome in the 'outcomes' vector
  outcome_position <- which(outcomes == outcome_event)
  
  # Use the 'switch' function to select the appropriate time-in-study variable
  timeinstudy_var <- switch(outcome_position,
                            as.name("timeinstudy1"),
                            as.name("timeinstudy2"),
                            as.name("timeinstudy3"),
                            as.name("timeinstudy_death_any"))
  
  # Formula for unadjusted model using the selected time-in-study variable
  formula_unadj <- as.formula(paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
  
  # Fit the unadjusted model
  model_unadj <- safely(
    .f = ~ svycoxph(formula_unadj, 
                    design = unadj, 
                    data = subset_df))
  
  result_unadj <- model_unadj()
  
  # Formula for IPTW model using the selected time-in-study variable
  formula_iptw <- as.formula(paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
  
  # Fit the IPTW model
  model_iptw <- safely(
    .f = ~ svycoxph(formula_iptw, 
                    design = iptw, 
                    data = subset_df,
                    weights = ate_weight))    
  
  result_iptw <- model_iptw()
  
  if (!is.null(result_unadj$error)) {
    print(paste("Error in IPTW model for ", "outcome", j))
    next
  }
  
  if (!is.null(result_unadj$error)) {
    
  } else {
    # Extract coefficients and confidence intervals for unadjusted model
    summary_unadj <- summary(result_unadj$result)
    print(summary_unadj)
    
    unadj_coef <- result_unadj$result$coefficients
    print(paste("Coefficient unadjusted:", unadj_coef))
    unadj_hr <- unadj_coef %>% exp()
    
    se_unadj_normal <- summary_unadj$coef["treatICS/LABA", "se(coef)"]
    print(paste("se unadj:", se_unadj_normal))
    
    # se_unadj_robust <- summary_unadj$coef["treatICS/LABA", "robust se"]
    # print(paste("se unadj:", se_unadj_robust))
    
    ci_unadj <- (unadj_coef + c(-1, 1) * qnorm(0.975) * se_unadj_normal) %>% exp()
    print(paste("ci unadj:", ci_unadj))
  }
  
  if (!is.null(result_iptw$error)) {
    print(paste("Error in IPTW model for", "outcome", j))
    next
  }
  if (!is.null(result_iptw$error)) {
  } else {
    # Extract coefficients and confidence intervals for IPTW model
    summary_iptw <- summary(result_iptw$result)
    print(summary_iptw)
    
    iptw_coef <- result_iptw$result$coefficients
    print(paste("Coefficient IPTW:", unadj_coef))
    iptw_hr <- iptw_coef %>% exp()
    
    se_iptw_normal <- summary_iptw$coef["treatICS/LABA", "se(coef)"]
    print(paste("se iptw:", se_unadj_normal))
    
    se_iptw_robust <- summary_iptw$coef["treatICS/LABA", "robust se"]
    print(paste("se unadj:", se_iptw_robust))
    
    #calculate the CI using the robust SE
    ci_iptw <- (iptw_coef + c(-1, 1) * qnorm(0.975) * se_iptw_robust) %>% exp()
    print(paste("ci iptw:", ci_iptw))
  }
  # Save results in the 'estimates' dataframe
  result_position <- j  # Starting position for the outcome
  
  #save coefficients as hazard ratios
  estimates[result_position, "outcome_event"] <- gsub("_present", "", outcome_event)
  estimates[result_position, "unadj_coef"] <- unadj_coef
  estimates[result_position, "unadj_hr"] <- unadj_hr
  estimates[result_position, "se_unadj_normal"] <- se_unadj_normal
  estimates[result_position, "ci_unadj_lower"] <- ci_unadj[1]
  estimates[result_position, "ci_unadj_upper"] <- ci_unadj[2]
  estimates[result_position, "iptw_coef"] <- iptw_coef
  estimates[result_position, "iptw_hr"] <- iptw_hr
  estimates[result_position, "se_iptw_robust"] <- se_iptw_robust
  estimates[result_position, "se_iptw_normal"] <- se_iptw_normal
  estimates[result_position, "ci_iptw_lower"] <- ci_iptw[1]
  estimates[result_position, "ci_iptw_upper"] <- ci_iptw[2]
  
  rm(list = c("result_unadj", "result_iptw", "summary_unadj", "summary_iptw", "unadj_coef", "unadj_hr",  "se_unadj_normal", "ci_unadj",  "iptw_coef", "iptw_hr", "ci_iptw", "se_iptw_normal", "se_iptw_robust", "result_position"))  
}

