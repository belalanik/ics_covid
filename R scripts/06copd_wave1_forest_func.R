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

create_forest <- function(inputfile, output_ext) {
  
#open parquet file with estimates
filepath <- file.path(Tables, "QBA", inputfile)
estimates <- read_parquet(filepath) 
estimates <- estimates %>% dplyr::select(-contains("coef_"), -contains("se_"), -contains("res_p"), -contains("att"), -contains("unstab"))

results_table <- estimates %>% 
  pivot_longer(!outcome_event, names_to = "weight", values_to = "value") %>%
  mutate(weightlabel = case_when(
    stringr::str_detect(weight, "ate_weight_stab") ~ "IPTW",
    stringr::str_detect(weight, "unadj") ~ "Unweighted",
    TRUE ~ weight )) %>%
  mutate(regression = case_when(
    stringr::str_detect(weight, "or") | str_detect(weight, "log") ~ "Logistic",
    stringr::str_detect(weight, "hr") | str_detect(weight, "cox") ~ "Cox",
    TRUE ~ weight))   %>% 
  filter(weightlabel != "res_p") %>%
  dplyr::select(-contains("res_p")) %>%
  mutate(weight = case_when(
    stringr::str_detect(weight, "or") ~ "point_estimate",
    stringr::str_detect(weight, "hr") ~ "point_estimate",
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
  arrange(outcome_event, weightlabel, regression) %>% 
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

#move the entries for outcome up by one row
forest_table <- forest_table %>%
  fill(outcome, .direction = "up")
forest_table <- forest_table %>% filter(!is.na(point_estimate))

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
        axis.title.x = element_text(size = 18),  # Increase x-axis label font size
        axis.text.x = element_text(size = 16),   # Increase x-axis tick label font size
        axis.ticks.x = element_line(size = 0.5)) + # Increase x-axis tick size
  scale_x_continuous(trans='log10', breaks = c(0.75, 1, 2, 4), limits = c(0.75, 4))

est <- ggplot(forest_table, aes(y = order)) +
  geom_text(aes(x = 0, label = outcome), hjust = 0, size = 6.5, fontface = "bold") + 
  geom_text(aes(x = 1.7, label = regression), hjust = 0, size = 6.5) +
  geom_text(aes(x = 1.1, label = weightlabel), hjust = 0, size = 6.5) +
  geom_text(aes(x = 2.15, label = estimate_lab, fontface = ifelse(estimate_lab == "Effect estimate (96% CI)", "bold", "plain")), size = 6.5, hjust = 0) +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

#est <- est + theme(text = element_text(size = 20))

layout <- c(
  area(t = 0, l = 0, b = 50, r = 60),
  area(t = 0, l = 55, b = 50, r = 80))

final_forest <- est + forest_plot + plot_layout(design = layout)

#save plot
file_path <- file.path(Graphdir, "cox_regression", paste0("forest_plot_full_crude", output_ext, ".png"))

ggsave(filename = file_path, plot = final_forest, width = 14, height = 6, units = "in", dpi = 300)

}

inputfile <- c("cox_log_regression_estimates.parquet", "SA_cox_log_regression_estimates_no_triple.parquet", "SA_cox_log_regression_estimates_6m.parquet", "SA_cox_log_regression_estimates_all.parquet")
output_ext <- c("", "_no_triple",  "_6m", "_all")

#apply function
mapply(create_forest, inputfile, output_ext)


