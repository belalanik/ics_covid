# Author: Marleen Bokern
# Date: 03/2023
# Purpose: generate propensity scores

packages <- c("tidyverse", "MetBrewer", "arrow", "mice", "MatchThem", "parallelly", "furrr", "survey", "cobalt", "ggplot2")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#########################################################
setwd(Datadir_copd)

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

start_time <- Sys.time()
palette <- met.brewer("Tiepolo")

#keep only people who are in one of the treatment groups, remove all unnecessary variables
subset_df <- df[!is.na(df$treatgroup),]
subset_df <- subset_df[, c("patid", "age_index", "gender", "imd", "bmi", "eth", 
                    "diabetes_present", "hypertension_present", 
                    "cvd_present", "allcancers_present", 
                    "asthma_present", "kidney_present", 
                    "immunosuppression_present", "flu_vacc_present", 
                    "pneumo_vacc_present", "exacerb_present", 
                    "smok","timeinstudy1", "timeinstudy2", "timeinstudy3", "timeout1", "timeout2", "timeout3", "pos_covid_test_present", "covid_hes_present", "covid_death_present", "treat", "treatgroup")]

#check missingness patterns
md.pattern(subset_df, rotate.names = TRUE)


#check number of missings per variable in model
missing_counts <- colSums(is.na(subset_df[, c("age_index", "gender", "imd", "bmi","eth", "diabetes_present", "hypertension_present", "cvd_present", "allcancers_present", "asthma_present", "kidney_present", "immunosuppression_present", "flu_vacc_present", "pneumo_vacc_present", "exacerb_present", "smok")]))
missing_counts 

nimp(is.na(subset_df))

#imputation using MICE 
start_time <- Sys.time()
imp_mids <- futuremice(data = subset_df, method = c("", "", "", "polr", "pmm", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), m = 10, maxit = 10, parallelseed = 1234, print = FALSE)
  
imp_mids
  
end_time <- Sys.time()
run_time <- end_time - start_time
run_time

print(imp_mids) #check if right method was used for imputation

plot(imp_mids)
imp_mids$imp$bmi
imp_mids$imp$imd

# 
plot_imd <- stripplot(imp_mids, imd, pch = 20, cex = 0.2)
par(cex.lab = 1.2, cex.axis = 1.0, cex.main = 1.2)
plot_bmi <- stripplot(imp_mids, bmi, pch = 20, cex = 0.2) #seems like the imputation is quite "confident". quite a few outliers. not sure if to reduce

# 
print(plot_imd)
png(file.path(Graphdir, "imd_stripplot.png"), width = 800, height = 600, units = "px", res = 300)
dev.off()

print(plot_bmi)
png(file.path(Graphdir,"bmi_stripplot.png"), width = 800, height = 600, units = "px", res = 300)  # Adjust dimensions and resolution as needed
dev.off()

#append all the imputed datasets. Generates a new variable .imp which denotes the number of the imputed dataset.
data <- complete(imp_mids, action = "long")

#create objects to stor results
summary_ps_list <- list()
plot_list <- list()
estimates <- matrix(nrow = 10, ncol = 21) %>% as.data.frame()
#10 imputed datasets x 3 outcomes = 30

colnames(estimates) <- 
  c("imputation", "HR1_unadj", "LCI1_unadj", "UCI1_unadj",  "HR1", "LCI1", "UCI1", "HR2_unadj", "LCI2_unadj", "UCI2_unadj", "HR2", "LCI2", "UCI2", "HR3_unadj", "LCI3_unadj", "UCI3_unadj", "HR3", "LCI3", "UCI3", "n", "n_after_restriction")

#filter to each imp dataset, fit ps model, iptw, get te, se, + diagnostics --> 10 rows of data with te, se for each dataset, overall te + CI using rubins rules

data$ps <- NA_real_
data$iptw <- NA_real_

# Define the formula
ps_formula <- treatgroup ~ age_index + gender + eth + bmi + diabetes_present + hypertension_present + cvd_present + allcancers_present + asthma_present + kidney_present + immunosuppression_present + flu_vacc_present + pneumo_vacc_present + exacerb_present + smok

# Fit the logistic regression model using "weightthem" with an approach and method

ps_model <- weightthem(ps_formula, data = imp_mids, approach = "within", method = "ps")
summary(ps_model)



for (imp_num in 1:10) {
  print(paste("Iteration", imp_num))
  
  # Filter the data for the current imputed dataset
  imp_data <- data[data$.imp == imp_num, ]
  print(paste("Dimensions of imp_data before:", dim(imp_data)))
  



  # Fit the specified propensity score model
#ps_model <- glm(treatgroup ~ age_index + gender + eth + bmi + diabetes_present + hypertension_present + cvd_present + allcancers_present + asthma_present + kidney_present + immunosuppression_present + flu_vacc_present + pneumo_vacc_present + exacerb_present + smok, 
                  #data = imp_data, 
                  #family = "binomial")
  
  
  imp_data$new_ps <- predict(ps_model, 
                         type = "response")


#check overlap of propensity scores by treatment group
  plot_name <- paste0("overlap", imp_num)
  assign(plot_name, imp_data %>% 
           ggplot(aes(x = new_ps, linetype = treat)) +
           geom_density(alpha = 0.5) +
           xlab('Probability of receiving treatment') +
           ylab('Density') +
           scale_fill_discrete('') +
           scale_x_continuous(breaks = seq(0, 1, 0.1)) +
           scale_color_identity(guide = "legend") +
           theme(
             strip.text = element_text(colour = 'black'),
             legend.title = element_blank(),
             legend.position = c(0.82, .8),
             legend.direction = 'vertical', 
             panel.background = element_rect(fill = "white", colour = "white"),
             axis.line = element_line(colour = "black"),
             panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank()
           )
  )
  
  # Save the plot
  ggsave(filename = paste0(Graphdir, plot_name, ".png"),
    plot = get(plot_name),
    width = 6,
    height = 4)

  data <- left_join(data, imp_data %>% select(patid, .imp, new_ps), by = c("patid", ".imp")) 
  data$ps <- ifelse(is.na(data$ps) & !is.na(data$new_ps), data$new_ps, data$ps)
  data <- data %>% select(-new_ps)
   
  ps_trim <- imp_data %>% 
  select(treat, new_ps) %>% 
  group_by(treat) %>% 
  summarise(min = min(new_ps), max = max(new_ps)) %>% 
  ungroup() %>% 
  summarise(min = max(min), max = min(max))
  
  # Restricted to observations within a PS range common to both treated and untreated persons— (i.e. exclude all patients in the non-overlapping parts of the PS distribution)
  
  imp_data_trimmed <- imp_data %>%
  filter(new_ps >= ps_trim$min[1] & new_ps <= ps_trim$max[1])

  imp_data_trimmed$weight <- ifelse(imp_data_trimmed$treatgroup == 1,
                                    1 / imp_data_trimmed$new_ps,
                                    1 / (1 - imp_data_trimmed$new_ps))

  data <- left_join(data, imp_data_trimmed %>% select(patid, .imp, weight), by = c("patid", ".imp"))
  data$iptw <- ifelse(is.na(data$iptw) & !is.na(data$weight), data$weight, data$iptw)
  data <- data %>% select(-weight)
}

par(mfrow = c(2, 1))
for (treatment in unique(data$treat)) {
  subset_data <- data[data$treat == treatment, ]  
  hist(subset_data$ps, breaks = 40, main = paste("Treatment", treatment), xlab = "Propensity Score")
}

par(mfrow = c(2, 1))
for (treatment in unique(data$treat)) {
  subset_data <- data[data$treat == treatment, ]  # Subset data for the current treatment group
  hist(subset_data$iptw, main = paste("Treatment", treatment), xlab = "Propensity Score")
}

# COX REGRESSION ----------------------------------------------------------


models <- c("iptw", "unadjusted")
outcome_models <- list() 
# Define svy design for IPTW 
iptw <- svydesign(ids = ~ 1,
                  data = imp_data_trimmed,
                  weights = ~ weight)

# Define svy design for unadjusted 
unadj <- svydesign(ids = ~ 1,
                  data = imp_data)

outcomes <- c("pos_covid_test_present", "covid_hes_present", "covid_death_present")

#loop through imputations and outcomes
for (imp_num in 1:10) {
  print(paste("Iteration", imp_num))
  
  # Filter the data for the current imputed dataset
  imp_data <- data[data$.imp == imp_num, ]

  for (j in seq_along(outcomes)){
    outcome_event <- outcomes[j]
    print(outcome_event)
      
      # Determine the position of the current outcome in the 'outcomes' vector
      outcome_position <- which(outcomes == outcome_event)
      print(paste("outcome", outcome_position))
      
      # Use the 'switch' function to select the appropriate time-in-study variable
      timeinstudy_var <- switch(outcome_position,
                                as.name("timeinstudy1"),
                                as.name("timeinstudy2"),
                                as.name("timeinstudy3"))
      
      print(timeinstudy_var)
      
      # Now construct the formula using the selected time-in-study variable
      formula_unadj <- as.formula(
        paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
      
      # Fit the unadjusted model
      model_unadj <- safely(
        .f = ~ svycoxph(formula_unadj, 
                        design = unadj, 
                        data = imp_data))
      
      result_unadj <- model_unadj()
      
      if (!is.null(result_unadj$error)) {
        print(paste("Error in unadjusted model for imp", imp_num, "outcome", j))
        next
      }
      
      if (!is.null(result_unadj$error)) {
        
      } else {
        summary_unadj <- summary(result_unadj$result)
       
        unadj_coef <- result_unadj$result$coefficients
        print(paste("Coefficient unadjusted:", unadj_coef))
        
        se_unadj <- summary_unadj$coef["treatICS/LABA", "se(coef)"]
        print(paste("se unadj:", se_unadj))
        
        ci_unadj <- (unadj_coef + c(-1, 1) * qnorm(0.975) * se_unadj) %>% exp()
        print(paste("ci unadj:", ci_unadj))
      }        
      
      formula_iptw <- as.formula(
        paste("Surv(", timeinstudy_var, ", ", outcome_event, ") ~ treat"))
      
      # Fit the IPTW model
      model_iptw <- safely(
        .f = ~ svycoxph(formula_iptw, 
                        design = iptw, 
                        data = imp_data_trimmed))
      
      result_iptw <- model_iptw() 
      
      if (!is.null(result_iptw$error)) {
        print(paste("Error in IPTW model for imp", imp_num, "outcome", j))
        next
      }
      
      if (!is.null(result_iptw$error)) {
      } else {
        summary_iptw <- summary(result_iptw$result)
        
        iptw_coef <- result_iptw$result$coefficients
        print(paste("Coefficient IPTW:", unadj_coef))
        
        se_iptw <- summary_iptw$coef["treatICS/LABA", "robust se"]
        print(paste("se iptw:", se_iptw))
  
        ci_iptw <- (iptw_coef + c(-1, 1) * qnorm(0.975) * se_iptw) %>% exp()
        print(paste("ci iptw:", ci_iptw))
      } 
    
      
      outcome_models[[paste("imp", imp_num, "_outcome", j)]] <- list(
        unadj = model_unadj,
        iptw = model_iptw)

      # Save results in the 'estimates' dataframe
      result_position <- 2 + (j - 1) * 6  # Starting position for the outcome
    
      # Point estimate and confidence intervals for unadjusted model
      print(paste("Saving estimate at position:", result_position))

      # Save values in the 'estimates' dataframe
      estimates[imp_num, result_position] <- unadj_coef
      estimates[imp_num, c(result_position + 1, result_position + 2)] <- ci_unadj
      estimates[imp_num, result_position + 3] <- iptw_coef
      estimates[imp_num, c(result_position + 4, result_position + 5)] <- ci_iptw
      
      # # Save n in 'estimates' after trimming
      estimates[imp_num, "n_after_restriction"] <- nrow(imp_data_trimmed)
      estimates[imp_num, "n"] <- nrow(imp_data)
      estimates[imp_num, "imputation"] <- imp_num
}
}

# Create a function to plot IPTW weight distributions
plot_weight_distribution <- function(weights) {
  ggplot(data.frame(Weights = weights), aes(x = Weights)) +
    geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black") +
    labs(x = "IPTW Weights", y = "Frequency", title = "Distribution of IPTW Weights")
}

# Create an empty summary dataframe
iptw_dist <- data.frame(
  Treat = character(),
  Imp_val = numeric(),
  Mean = numeric(),
  SD = numeric(),
  Median = numeric(),
  Q25 = numeric(),
  Q75 = numeric(),
  min = numeric(),
  max = numeric())

# Loop through each treatment level
for (imp_num in unique(data$.imp)) {
  for (treat_level in unique(data$treat)) {
  
    weights_subset <- data$iptw[data$.imp == imp_num & data$treat == treat_level]
    
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
      Imp_val = imp_num,
      Mean = mean_weight,
      SD = sd_weight,
      Median = median_weight,
      Q25 = q25,
      Q75 = q75,
      min = min_weight,
      max = max_weight
    ))
    
    # Print statistics
    cat("Treatment:", treat_level, "\n")
    cat("Imputation:", imp_num, "\n")
    cat("Mean:", mean_weight, "\n")
    cat("Standard Deviation:", sd_weight, "\n")
    cat("Median:", median_weight, "\n")
    cat("IQR (25th percentile):", q25, "\n")
    cat("IQR (75th percentile):", q75, "\n\n")
    
    # Call the plot function with correct argument name
    print(plot_weight_distribution(weights_subset))
  }
  
  imp_data <- data$iptw[data$.imp == imp_num]
  
  # Calculate overall summary statistics
  overall_mean <- mean(imp_data, na.rm = TRUE)
  overall_sd <- sd(imp_data, na.rm = TRUE)
  overall_median <- median(imp_data, na.rm = TRUE)
  overall_q25 <- quantile(imp_data, 0.25, na.rm = TRUE)
  overall_q75 <- quantile(imp_data, 0.75, na.rm = TRUE)
  overall_min <- min(imp_data, na.rm = TRUE)
  overall_max <- max(imp_data, na.rm = TRUE)
  
  # Add overall statistics to the summary dataframe
  iptw_dist <- rbind(iptw_dist, data.frame(
    Treat = "Overall",
    Imp_val = imp_num,
    Mean = overall_mean,
    SD = overall_sd,
    Median = overall_median,
    Q25 = overall_q25,
    Q75 = overall_q75,
    min = overall_min,
    max = overall_max))
}


print(iptw_dist)

# IPTW Diagnostics --------------------------------------------------------

#comparison of baseline covariates in the weighted sample

balance_table <- bal.tab(data = data,
                         treat = "treat",  # Replace with the actual treatment column name
                         weight = "weight",  # Replace with the actual IPTW column name
                         method = "weighting",
                         imp = ".imp")

#distribution of weights

# Create a histogram with separate facets for each treatment group

ggplot(subset_df, aes(x = ps)) +
  geom_histogram(binwidth = 0.005, color = "black", fill = "blue", alpha = 0.7) +
  facet_wrap(~ factor(treat), ncol = 1) +
  labs(x = "Propensity Score", y = "Frequency") +
  ggtitle("Histogram of Propensity Scores by Treatment Group")

ggplot(subset_df, aes(x = ps, color = factor(treat))) +
  geom_density() +
  labs(x = "Propensity Score", y = "Density") +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treatment")) +
  ggtitle("Density Plot of Propensity Scores by Treatment Group")


#Covariate balance checking




#histogram of iptw weight
