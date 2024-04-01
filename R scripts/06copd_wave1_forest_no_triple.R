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
filepath <- file.path(Tables, "QBA", "SA_cox_log_regression_estimates_no_triple.parquet")
estimates <- read_parquet(filepath)
estimates <- estimates %>% select(-contains("coef_"), -contains("se_"), -contains("res_p"), -contains("cox"), -contains("hr"), -contains("att"), -contains("unstab"))


results_table <- estimates %>% 
  pivot_longer(!outcome_event, names_to = "weight", values_to = "value") %>%
  mutate(model_type = ifelse(str_detect(weight, 'cox'), 'cox', 'log'),
         weightlabel = case_when(
           stringr::str_detect(weight, "ate_weight_stab") ~ "IPTW",
           stringr::str_detect(weight, "unadj") ~ "Crude",
           TRUE ~ weight )) %>%
  mutate(weight = case_when(
    stringr::str_detect(weight, "or") ~ "point_estimate", 
    stringr::str_detect(weight, "ci_lower") ~ "ci_lower",
    stringr::str_detect(weight, "ci_upper") ~ "ci_upper",
    TRUE ~ weight ))  %>%
  pivot_wider(values_from = value, names_from = weight)

forest_table <- results_table %>% 
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(point_estimate, ci_lower, ci_upper),
    ~ str_pad(
      round(.x, 3),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
  estimate_lab = paste0(point_estimate, " (", ci_lower, "-", ci_upper, ")")) 

forest_table <- forest_table %>%
  mutate(outcome_event = fct_recode(outcome_event,
                                    "COVID-19 hospitalisation" = "covid_hes",
                                    "COVID-19 death" = "covid_death",
                                    "All-cause mortality" = "any_death"
  ))

#generate forest plot
forest_table <- forest_table %>%
  mutate(weightlabel = factor(weightlabel, levels = c("IPTW", "Crude"))) %>%
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
  labs(x = "Hazard Ratio", y = "") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) + 
  scale_x_continuous(breaks = seq(0, 2.5, 0.5), limits = c(0, 3))

print(forest_plot)

est <- ggplot(forest_table, aes(y = order)) +
  geom_text(aes(x = 0, label = outcome), hjust = 0, fontface = "bold") + 
  geom_text(aes(x = 1.4, label = estimate_lab, fontface = ifelse(estimate_lab == "Hazard Ratio (95% CI)", "bold", "plain")),
            hjust = 0) +
  geom_text(aes(x = 0.9, label = weightlabel),
            hjust = 0) +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

est <- est + theme(text = element_text(size = 7))
print(est)

layout <- c(
  area(t = 0, l = 0, b = 30, r = 50),
  area(t = 0, l = 35, b = 30, r = 55))

final_forest <- est + forest_plot + plot_layout(design = layout)
print(final_forest)

#save plot
file_path <- file.path(Graphdir, "cox_regression", "SA_forest_plot_crude_no_triple.png")
ggsave(filename = file_path, plot = final_forest, width = 12, height = 6, units = "in", dpi = 300)
