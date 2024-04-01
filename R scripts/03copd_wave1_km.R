# Author: Marleen Bokern
# Date: 03/2023
# Purpose: generate KM plots and Cuminc plots for all outcomes.
#to update packages: remove package (remove.packages("tidyr")) and reinstall 

packages <- c("tidyverse", "MetBrewer", "arrow", "ggplot2", "flexsurv", "survival", "survminer", "ggsurvfit", "tidycmprsk")
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
palette <- met.brewer("Cassatt2")

subset_df <- df[!is.na(df$treatgroup),]

median_follow_up_hosp <- subset_df %>%
  group_by(treatgroup) %>%
  summarise(median_follow_up_time_hosp = median(timeinstudy3, na.rm = TRUE))

median_follow_up_death <- subset_df %>%
  group_by(treatgroup) %>%
  summarise(median_follow_up_time_death = median(timeinstudy3, na.rm = TRUE))

# OUTCOME: POSITIVE COVID TEST. Generate the Kaplan-Meier curve
surv1 <- Surv(subset_df$timeinstudy1, subset_df$pos_covid_test_present) 
km_curve1 <- survfit(surv1 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot <- ggsurvplot(km_curve1, data = subset_df, conf.int = T, censor = F, ylim = c(0.95, 1), xlab = "Time in days", risk.table = "absolute", risk.table.title = "Number at risk (%)",  cumevents = TRUE, fontsize = 13, tables.height = 0.15, legend.labs = c("ICS", "LABA/LAMA"), legend.title = "", palette = c(palette[9], palette[4]), xlim = c(0, 185)) 
ggsurvplot$plot <- ggsurvplot$plot + scale_x_continuous(breaks = c(0, 50, 100, 150, 183))
ggsurvplot$table$theme$axis.text.y$colour <- "black"
ggsurvplot$table$theme$axis.text.y$size <- 40
ggsurvplot$table$theme$axis.text.x$size <- 40
ggsurvplot$table$labels$x <- ""
ggsurvplot$table$theme$plot.title$size <- 44
ggsurvplot$cumevents$theme$axis.text.y$colour <- "black"
ggsurvplot$cumevents$theme$axis.text.y$size <- 40
ggsurvplot$cumevents$theme$axis.text.x$size <- 40
ggsurvplot$cumevents$labels$x <- ""
ggsurvplot$cumevents$theme$plot.title$size <- 44
ggsurvplot$plot$theme$axis.title.x$size <- 44
ggsurvplot$plot$theme$axis.title.y$size <- 44
ggsurvplot$plot$theme$axis.text.x$size <- 44
ggsurvplot$plot$theme$axis.text.y$size <- 44
ggsurvplot$plot$theme$legend.text$size <- 44
ggsurvplot$plot$theme$legend.key.size <- unit(4, "lines")
print(ggsurvplot)

file_path <- file.path(Graphdir, "cox_regression", "km_pos_test.png")

png(file_path, width = 2000, height = 1500)
print(ggsurvplot)
dev.off()

# OUTCOME: COVID HOSPITALISATION. Generate the Kaplan-Meier curve
surv2 <- Surv(subset_df$timeinstudy2, subset_df$covid_hes_present) 
km_curve2 <- survfit(surv2 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot <- ggsurvplot(km_curve2, data = subset_df, conf.int = T, censor = F, ylim = c(0.95, 1), xlab = "Time in days", risk.table = "absolute", risk.table.title = "Number at risk (%)",  cumevents = TRUE, fontsize = 13, tables.height = 0.15, legend.labs = c("ICS", "LABA/LAMA"), legend.title = "", palette = c(palette[9], palette[4]), xlim = c(0, 185)) 
ggsurvplot$plot <- ggsurvplot$plot + scale_x_continuous(breaks = c(0, 50, 100, 150, 183))
ggsurvplot$table$theme$axis.text.y$colour <- "black"
ggsurvplot$table$theme$axis.text.y$size <- 40
ggsurvplot$table$theme$axis.text.x$size <- 40
ggsurvplot$table$labels$x <- ""
ggsurvplot$table$theme$plot.title$size <- 44
ggsurvplot$cumevents$theme$axis.text.y$colour <- "black"
ggsurvplot$cumevents$theme$axis.text.y$size <- 40
ggsurvplot$cumevents$theme$axis.text.x$size <- 40
ggsurvplot$cumevents$labels$x <- ""
ggsurvplot$cumevents$theme$plot.title$size <- 44
ggsurvplot$plot$theme$axis.title.x$size <- 44
ggsurvplot$plot$theme$axis.title.y$size <- 44
ggsurvplot$plot$theme$axis.text.x$size <- 44
ggsurvplot$plot$theme$axis.text.y$size <- 44
ggsurvplot$plot$theme$legend.text$size <- 44
ggsurvplot$plot$theme$legend.key.size <- unit(4, "lines")
print(ggsurvplot)

file_path <- file.path(Graphdir, "cox_regression", "km_hosp.png")
png(file_path, width = 2000, height = 1500)
print(ggsurvplot)
dev.off()

# OUTCOME: COVID HOSPITALISATION. Generate the Kaplan-Meier curve
surv3 <- Surv(subset_df$timeinstudy3, subset_df$covid_death_present) 
km_curve3 <- survfit(surv3 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot <- ggsurvplot(km_curve3, data = subset_df, conf.int = T, censor = F, ylim = c(0.95, 1), xlab = "Time in days", risk.table = "absolute", risk.table.title = "Number at risk (%)",  cumevents = TRUE, fontsize = 13, tables.height = 0.15, legend.labs = c("ICS", "LABA/LAMA"), legend.title = "", palette = c(palette[9], palette[4]), xlim = c(0, 185)) 
ggsurvplot$plot <- ggsurvplot$plot + scale_x_continuous(breaks = c(0, 50, 100, 150, 183))
ggsurvplot$table$theme$axis.text.y$colour <- "black"
ggsurvplot$table$theme$axis.text.y$size <- 40
ggsurvplot$table$theme$axis.text.x$size <- 40
ggsurvplot$table$labels$x <- ""
ggsurvplot$table$theme$plot.title$size <- 44
ggsurvplot$cumevents$theme$axis.text.y$colour <- "black"
ggsurvplot$cumevents$theme$axis.text.y$size <- 40
ggsurvplot$cumevents$theme$axis.text.x$size <- 40
ggsurvplot$cumevents$labels$x <- ""
ggsurvplot$cumevents$theme$plot.title$size <- 44
ggsurvplot$plot$theme$axis.title.x$size <- 44
ggsurvplot$plot$theme$axis.title.y$size <- 44
ggsurvplot$plot$theme$axis.text.x$size <- 44
ggsurvplot$plot$theme$axis.text.y$size <- 44
ggsurvplot$plot$theme$legend.text$size <- 44
ggsurvplot$plot$theme$legend.key.size <- unit(4, "lines")
print(ggsurvplot)

file_path <- file.path(Graphdir, "cox_regression", "km_covid_death.png")
png(file_path, width = 2000, height = 1500)
print(ggsurvplot)
dev.off()

# OUTCOME: ANY DEATH. Generate the Kaplan-Meier curve
surv4 <- Surv(subset_df$timeinstudy_death_any, subset_df$any_death_present) 
km_curve4 <- survfit(surv4 ~ subset_df$treat, data = subset_df)

# Plot the Kaplan-Meier curve
ggsurvplot <- ggsurvplot(km_curve4, data = subset_df, conf.int = T, censor = F, ylim = c(0.95, 1), xlab = "Time in days", risk.table = "absolute", risk.table.title = "Number at risk (%)",  cumevents = TRUE, fontsize = 13, tables.height = 0.15, legend.labs = c("ICS", "LABA/LAMA"), legend.title = "", palette = c(palette[9], palette[4]), xlim = c(0, 185)) 
ggsurvplot$plot <- ggsurvplot$plot + scale_x_continuous(breaks = c(0, 50, 100, 150, 183))
ggsurvplot$table$theme$axis.text.y$colour <- "black"
ggsurvplot$table$theme$axis.text.y$size <- 40
ggsurvplot$table$theme$axis.text.x$size <- 40
ggsurvplot$table$labels$x <- ""
ggsurvplot$table$theme$plot.title$size <- 44
ggsurvplot$cumevents$theme$axis.text.y$colour <- "black"
ggsurvplot$cumevents$theme$axis.text.y$size <- 40
ggsurvplot$cumevents$theme$axis.text.x$size <- 40
ggsurvplot$cumevents$labels$x <- ""
ggsurvplot$cumevents$theme$plot.title$size <- 44
ggsurvplot$plot$theme$axis.title.x$size <- 44
ggsurvplot$plot$theme$axis.title.y$size <- 44
ggsurvplot$plot$theme$axis.text.x$size <- 44
ggsurvplot$plot$theme$axis.text.y$size <- 44
ggsurvplot$plot$theme$legend.text$size <- 44
ggsurvplot$plot$theme$legend.key.size <- unit(4, "lines")
print(ggsurvplot)

file_path <- file.path(Graphdir, "cox_regression", "km_any_death.png")
png(file_path, width = 2000, height = 1500)
print(ggsurvplot)
dev.off()

factor_cols <- c("pos_covid_test_present", "covid_hes_present", "covid_death_present", "any_death_present")
subset_df[factor_cols] <- lapply(subset_df[factor_cols], factor)

table(cut(subset_df$timeinstudy2, breaks = c(-Inf, 182, Inf), labels = c("<183", "183")), subset_df$covid_hes_present)
table(cut(subset_df$timeinstudy3, breaks = c(-Inf, 182, Inf), labels = c("<183", "183")), subset_df$covid_death_present)
table(cut(subset_df$timeinstudy_death_any, breaks = c(-Inf, 182, Inf), labels = c("<183", "183")), subset_df$any_death_present)
# CUMULATIVE INCIDENCE CURVES ---------------------------------------------

cuminc1 <- cuminc(Surv(timeinstudy1, pos_covid_test_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of Positive COVID-19 Test by Treatment Group") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c(palette[9], palette[4])) +
  scale_fill_manual(values = c(palette[9], palette[4]))

file_path <- file.path(Graphdir, "cox_regression", "cuminc_pos_covid_test.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)

cuminc2 <- cuminc(Surv(timeinstudy2, covid_hes_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of COVID-19 Hospitalisation by Treatment Group") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c(palette[9], palette[4])) +
  scale_fill_manual(values = c(palette[9], palette[4]))

file_path <- file.path(Graphdir, "cox_regression", "cuminc_covid_hes.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)

cuminc3 <- cuminc(Surv(timeinstudy3, covid_death_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of COVID-19 Death by Treatment Group") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c(palette[9], palette[4])) +
  scale_fill_manual(values = c(palette[9], palette[4]))

file_path <- file.path(Graphdir, "cox_regression", "cuminc_covid_death.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)


cuminc4 <- cuminc(Surv(timeinstudy_death_any, any_death_present) ~ treat, data = subset_df) %>% 
  ggcuminc(outcome = "1") +
  add_confidence_interval() +
  add_risktable(size = 4, risktable_height = 0.2) +
  ggtitle("Cumulative Incidence of All-cause Death by Treatment Group") +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c(palette[9], palette[4])) +
  scale_fill_manual(values = c(palette[9], palette[4]))

file_path <- file.path(Graphdir, "cox_regression", "cuminc_any_death.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)


#histogram of covid positive test dates

ggplot(subset_df, aes(x = pos_covid_test_date)) +
  geom_histogram(bins = 183, fill = palette[6], color = "black") +
  labs(
    title = "Histogram of Positive COVID Test Dates",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()

file_path <- file.path(Graphdir, "cox_regression", "hist_pos_covid_test_date.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)


#histogram of covid HES dates
ggplot(subset_df, aes(x = covid_hes_date)) +
  geom_histogram(bins = 183, fill = palette[6], color = "black") +
  labs(
    title = "Histogram of COVID Hospitalisation Dates",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()
file_path <- file.path(Graphdir, "cox_regression", "hist_covid_hes_date.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)

#histogram of covid death dates
ggplot(subset_df, aes(x = covid_death_date)) +
  geom_histogram(bins = 183, fill = palette[6], color = "black") +
  labs(
    title = "Histogram of COVID Death Dates",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()
file_path <- file.path(Graphdir, "cox_regression", "hist_covid_death_date.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)

#histogram of all death dates
ggplot(subset_df, aes(x = death_date)) +
  geom_histogram(bins = 183, fill = palette[6], color = "black") +
  labs(
    title = "Histogram of All-cause Death Dates",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()
file_path <- file.path(Graphdir, "cox_regression", "hist_any_death_date.png")
ggsave(file_path, width = 10, height = 6, dpi = 300)

end_time <- Sys.time()
run_time <- end_time - start_time
run_time


