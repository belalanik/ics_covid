# Author: Marleen Bokern
# Date: 03/2023
# Purpose: survival analysis, COPD wave 1
#to update packages: remove package (remove.packages("tidyr")) and reinstall 

library(arrow)
library(tidyverse)
library(ggplot2)
library(survival)
library(flexsurv)
library(MetBrewer)
library(survminer)
library(ggsurvfit)
library(tidycmprsk)

setwd(Datadir_copd)

#read in parquet dataset
df <- read_parquet("copd_wave1_60d.parquet")

start_time <- Sys.time()
palette <- met.brewer("Tiepolo")
#palette <- c(palette[1], palette[3], palette[5], palette[7])

subset_df <- df[!is.na(df$treatgroup),]
# OUTCOME: POSITIVE COVID TEST. Generate the Kaplan-Meier curve
surv1 <- Surv(subset_df$timeinstudy1, subset_df$pos_covid_test_present) 
km_curve1 <- survfit(surv1 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot(km_curve1, data = subset_df, conf.int = T, censor = F, ylim = c(0.98, 1), xlab = "Time in days", risk.table = TRUE, legend.labs = c("LABA/LAMA group", "ICS group"), legend.title = "", palette = c(palette[1], palette[8]))


# OUTCOME: COVID HOSPITALISATION. Generate the Kaplan-Meier curve
surv2 <- Surv(subset_df$timeinstudy2, subset_df$covid_hes_present) 
km_curve2 <- survfit(surv2 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot(km_curve2, data = subset_df, conf.int = T, censor = F, ylim = c(0.98, 1), xlab = "Time in days", risk.table = TRUE, legend.labs = c("LABA/LAMA group", "ICS group"), legend.title = "", palette = c(palette[1], palette[4]))

# OUTCOME: COVID DEATH. Generate the Kaplan-Meier curve
surv3 <- Surv(subset_df$timeinstudy3, subset_df$covid_death_present) 
km_curve3 <- survfit(surv3 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot(km_curve3, data = subset_df, conf.int = T, censor = F, ylim = c(0.98, 1), xlab = "Time in days", risk.table = TRUE, legend.labs = c("LABA/LAMA group", "ICS group"), legend.title = "", palette = c(palette[1], palette[7]))


factor_cols <- c("pos_covid_test_present", "covid_hes_present", "covid_death_present")
subset_df[factor_cols] <- lapply(subset_df[factor_cols], factor)

# CUMULATIVE INCIDENCE CURVES ---------------------------------------------

cuminc1 <- cuminc(Surv(timeinstudy1, pos_covid_test_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of Positive COVID-19 Test by Treatment Group") +
  theme(text = element_text(size = 12))
ggsave(filename = "cuminc1_pos_covid_test.png", plot = cuminc1, path = Tables, width = 10, height = 6, units = "in", dpi = 300)

cuminc2 <- cuminc(Surv(timeinstudy2, covid_hes_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of COVID-19 Hospitalisation by Treatment Group") +
  theme(text = element_text(size = 12))
ggsave(filename = "cuminc2_covid_hes.png", plot = cuminc2, path = Tables, width = 10, height = 6, units = "in", dpi = 300)

cuminc3 <- cuminc(Surv(timeinstudy3, covid_death_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of COVID-19 Death by Treatment Group") +
  theme(text = element_text(size = 12))
ggsave(filename = "cuminc3_covid_death.png", plot = cuminc3, path = Tables, width = 10, height = 6, units = "in", dpi = 300)


cox1 <- coxph(surv1 ~ treat, data = subset_df)
sch.resid1 = cox.zph(cox1, transform ='identity')
plot(sch.resid1)

cox2 <- coxph(surv2 ~ treat, data = subset_df)
sch.resid2 = cox.zph(cox2, transform ='identity')
plot(sch.resid2)

cox3 <- coxph(surv3 ~ treat, data = subset_df)
sch.resid3 = cox.zph(cox3, transform ='identity')
plot(sch.resid3)

cox1a <- coxph(surv1 ~ treat + age_index + bmi + eth5 + diabetes_present, data = subset_df)
mgale_res1 <- resid(cox1, type = "martingale")
plot(subset_df$age_index, mgale_res1)


cox2a <- coxph(surv2 ~ treat, data = subset_df)

cox3a <- coxph(surv3 ~ treat, data = subset_df)


subset_df <- df[!is.na(df$treatgroup),]

#histogram of covid positive test dates
hist(subset_df$pos_covid_test_date, breaks = 183, 
     main = "Histogram of Positive COVID Test Dates",
     xlab = "Date", ylab = "Count")

#histogram of covid HES dates
hist(subset_df$covid_hes_date, breaks = 183, 
     main = "Histogram of COVID Hospitalisation Dates",
     xlab = "Date", ylab = "Count")

#histogram of covid death dates
hist(subset_df$covid_death_date, breaks = 183, 
     main = "Histogram of COVID Death Dates",
     xlab = "Date", ylab = "Count")


end_time <- Sys.time()
run_time <- end_time - start_time
run_time
