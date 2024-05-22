# Author: Marleen Bokern
# Date: 03/2023
# Purpose: make forest plot from cox regression results. cox regression is done in copd_wave1.R

packages <- c("tidyverse", "ggplot2", "gt", "arrow", "patchwork", "forestplot")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#########################################################
setwd(Datadir_copd)

#open parquet file with estimates
filepath <- file.path(Tables, "QBA", "SA_cox_log_regression_estimates_6m.parquet")

estimates_crude <- read_parquet(filepath) 
estimates_crude <- estimates_crude %>% dplyr::select(-contains("coef_"), -contains("se_"), -contains("res_p"), -contains("cox"), -contains("att"), -contains("unstab"))
#drop any_covid_hes row
estimates_crude <- estimates_crude %>% filter(!stringr::str_detect(outcome_event, "any_"))

filepath <- file.path(Tables, "QBA", "qba_death_summary_results_6m.parquet")
estimates_death <- read_parquet(filepath)

filepath <- file.path(Tables, "QBA", "qba_hosp_summary_results_6m.parquet")
estimates_hosp <- read_parquet(filepath)

#bind rows of estimates_death and estimates_hosp
estimates_qba <- bind_cols(estimates_death, estimates_hosp)


results_table <- estimates_crude %>% 
  pivot_longer(!outcome_event, names_to = "weight", values_to = "value") %>%
  mutate(weightlabel = case_when(
    stringr::str_detect(weight, "ate_weight_stab") ~ "IPTW",
    stringr::str_detect(weight, "unadj") ~ "Unweighted",
    TRUE ~ weight )) %>%
  mutate(outcome_event = case_when(
    stringr::str_detect(outcome_event, "_death") ~ "covid_death",
    stringr::str_detect(outcome_event, "_hes") ~ "covid_hosp",
    TRUE ~ outcome_event)) %>% 
  mutate(weight = case_when(
    stringr::str_detect(weight, "or") ~ "point_estimate",
    stringr::str_detect(weight, "hr") ~ "point_estimate",
    stringr::str_detect(weight, "ci_lower") ~ "ci_lower",
    stringr::str_detect(weight, "ci_upper") ~ "ci_upper",
    TRUE ~ weight ))  %>%
  pivot_wider(values_from = value, names_from = weight)
#add string column that is qba = No QBA
results_table <- results_table %>% mutate(qba = "No QBA", reg = "OR")

results_table <- results_table %>% mutate(outcome = fct_recode(outcome_event,
                                                               "COVID-19 death" = "covid_death",
                                                               "COVID-19 hospitalisation" = "covid_hosp"))
#remove the 4th and 5th row
estimates_qba <- estimates_qba[-c(4,5),]

#transpose the data frame
estimates_qba <- t(estimates_qba)
#convert estimates to df
estimates_qba <- as.data.frame(estimates_qba)

#rename the columns v1, v2, v3
estimates_qba <- estimates_qba %>%
  rename(
    point_estimate = V2,
    ci_lower = V1,
    ci_upper = V3
  )

#add rownames as a column
estimates_qba <- estimates_qba %>% 
  rownames_to_column("estimate")

estimates_qba <- estimates_qba %>% 
  mutate(reg = case_when(
    stringr::str_detect(tolower(as.character(estimate)), "rr_") ~ "RR",
    stringr::str_detect(tolower(as.character(estimate)), "or_") ~ "OR",
    TRUE ~ estimate)) %>% 
  mutate(outcome_event = case_when(
    stringr::str_detect(tolower(as.character(estimate)), "hosp_") ~ "covid_hosp",
    stringr::str_detect(tolower(as.character(estimate)), "death_") ~ "covid_death",
    TRUE ~ estimate)) %>%
  mutate(qba = case_when(
    stringr::str_detect(tolower(as.character(estimate)), "summary") ~ "Summary",
    TRUE ~ estimate)) %>% 
  mutate(weightlabel = case_when(
    stringr::str_detect(tolower(as.character(estimate)), "ate_") ~ "IPTW",
    TRUE ~ "Unweighted"))
#remove the variable estimate
estimates_qba <- estimates_qba %>% dplyr::select(-estimate)

#combine the two data frames
results_table <- bind_rows(results_table, estimates_qba)


forest_table <- results_table %>%
  mutate(across(
    c(point_estimate, ci_lower, ci_upper),
    ~ sprintf("%6.2f", .x)
  ),
  estimate_lab = paste0(trimws(point_estimate), " (", trimws(ci_lower), "-", trimws(ci_upper), ")"))

forest_table <- forest_table %>%
  mutate(outcome = fct_recode(outcome_event,
                              "COVID-19 death" = "covid_death",
                              "COVID-19 hospitalisation" = "covid_hosp"
  ))

#generate forest plot
forest_table <- forest_table %>%
  arrange(outcome, desc(qba), weightlabel) %>% 
  mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(ci_lower),
    upper_CI = as.numeric(ci_upper)) %>%
  group_by(outcome) %>%
  mutate(outcomeno = n() - row_number() + 1) %>%
  ungroup() %>% 
  mutate(outcomeno = ifelse(outcomeno == 1, as.character(outcome), ""))

#generate order variable for plotting
forest_table$order <- 1:nrow(forest_table)
forest_table$order <- as.factor(forest_table$order)

forest_plot <- ggplot(forest_table, aes(x = point_estimate, y = order)) + 
  geom_point(aes(x = point_estimate), shape = 16, size = 3) +
  geom_linerange(aes(xmin = lower_CI, xmax = upper_CI)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Effect estimate", y = "") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(trans='log10', breaks = c(1, 2, 4, 8))

print(forest_plot)

est <- ggplot(forest_table, aes(y = order)) +
  geom_text(aes(x = 0, label = outcomeno), hjust = 0, fontface = "bold", size = 20/.pt) +
  geom_text(aes(x = 1, label = qba), hjust = 0, size = 20/.pt) +
  geom_text(aes(x = 1.4, label = reg), hjust = 0, size = 20/.pt) +
  geom_text(aes(x = 1.7, label = weightlabel), hjust = 0, size = 20/.pt) +
  geom_text(aes(x = 2.2, label = estimate_lab), hjust = 0, size = 20/.pt) +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

#est <- est + theme(text = element_text(size = 16))

# sims <- ggplot(forest_table, aes(y = order)) +
#   geom_text(aes(x = 0, label = simulations), hjust = 0, size = 16/.pt) +
#   theme_void() +
#   coord_cartesian(xlim = c(0, 0.3))

layout <- c(
  area(t = 0, l = 0, b = 30, r = 75),
  area(t = 0, l = 70, b = 30, r = 100))

final_forest <- est + forest_plot + plot_layout(design = layout)

final_forest

#save plot
file_path <- file.path(Graphdir, "QBA", "forest_plot_qba_6m.png")
ggsave(filename = file_path, plot = final_forest, width = 16, height = 5, units = "in", dpi = 1000)


