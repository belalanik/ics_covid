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
estimates <- read_parquet(filepath)
estimates <- estimates %>% dplyr::select(-contains("coef_"), -contains("se_"), -contains("res_p"), -contains("cox"), -contains("hr"))

#drop any_covid_hes row
estimates <- estimates %>% filter(!stringr::str_detect(outcome_event, "any_covid_hes"))

results_table <- estimates %>% 
  pivot_longer(!outcome_event, names_to = "weight", values_to = "value") %>%
  mutate(model_type = ifelse(str_detect(weight, 'cox'), 'cox', 'log'),
         weightlabel = case_when(
           stringr::str_detect(weight, "ate_weight_stab") ~ "IPTW",
           stringr::str_detect(weight, "unadj") ~ "Unweighted",
           TRUE ~ weight )) %>%
  mutate(weight = case_when(
    stringr::str_detect(weight, "or") ~ "point_estimate", 
    stringr::str_detect(weight, "ci_lower") ~ "ci_lower",
    stringr::str_detect(weight, "ci_upper") ~ "ci_upper",
    TRUE ~ weight ))  %>%
  pivot_wider(values_from = value, names_from = weight)

forest_table <- results_table %>%
  mutate(across(
    c(point_estimate, ci_lower, ci_upper),
    ~ sprintf("%6.2f", .x)
  ),
  estimate_lab = paste0(trimws(point_estimate), " (", trimws(ci_lower), "-", trimws(ci_upper), ")"))

forest_table <- forest_table %>%
  mutate(outcome_event = fct_recode(outcome_event,
                                    "COVID-19 hospitalisation" = "covid_hes",
                                    "COVID-19 death" = "covid_death",
                                    "All-cause mortality" = "any_death"
  ))

#generate forest plot
forest_table <- forest_table %>%
  mutate(weightlabel = factor(weightlabel, levels = c("IPTW", "Unweighted"))) %>%
  arrange(outcome_event, desc(model_type), weightlabel) %>% 
  mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(ci_lower),
    upper_CI = as.numeric(ci_upper)) %>%
  group_by(outcome_event) %>%
  mutate(outcome = n() - row_number() + 1) %>%
  ungroup() %>% 
  mutate(outcome = ifelse(outcome == 1, as.character(outcome_event), ""))

#generate order variable for plotting
forest_table$order <- 1:nrow(forest_table)
forest_table$order <- as.factor(forest_table$order)
#remove rows where point estimate is NA
forest_table <- forest_table %>% filter(!is.na(point_estimate))

forest_plot <- ggplot(forest_table, aes(x = point_estimate, y = order)) + 
  geom_point(aes(x = point_estimate), shape = 16, size = 3) +
  geom_linerange(aes(xmin = lower_CI, xmax = upper_CI)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds ratio", y = "") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),  # Increase x-axis label font size
        axis.text.x = element_text(size = 12),   # Increase x-axis tick label font size
        axis.ticks.x = element_line(size = 0.5)) + # Increase x-axis tick size
  scale_x_continuous(trans='log10', breaks = c(1, 2, 4), limits = c(0.75, 4))

est <- ggplot(forest_table, aes(y = order)) +
  geom_text(aes(x = 0, label = outcome), size = 5.5, hjust = 0, fontface = "bold") + 
  geom_text(aes(x = 1.95, label = estimate_lab), size = 5.5, hjust = 0) +
  geom_text(aes(x = 1.3, label = weightlabel), size = 5.5, hjust = 0) +  theme_void() +
  coord_cartesian(xlim = c(0, 3))


layout <- c(
  area(t = 0, l = 0, b = 30, r = 50),
  area(t = 0, l = 45, b = 30, r = 70))

final_forest <- est + forest_plot + plot_layout(design = layout)

#save plot
file_path <- file.path(Graphdir, "cox_regression", "forest_plot_crude_6m.png")
ggsave(filename = file_path, plot = final_forest, width = 10, height = 4, units = "in", dpi = 300)
