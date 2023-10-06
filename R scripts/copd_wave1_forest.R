# Author: Marleen Bokern
# Date: 03/2023
# Purpose: make forest plot from cox regression results. cox regression is done in copd_wave1.R

packages <- c("tidyverse", "MetBrewer", "ggplot2", "gt", "arrow", "patchwork")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

#########################################################
setwd(Datadir_copd)

#open parquet file with estimates
estimates <- read_parquet("cox_regression_estimates.parquet")


#reformat the estimates dataframe to make it easier to plot
results_table <- estimates %>% 
  pivot_longer(cols = c("hr_unadj", "ci_unadj_lower", "ci_unadj_upper", "hr_iptw", "ci_iptw_lower", "ci_iptw_upper"), 
               names_to = "variable", values_to = "value") %>% 
  # mutate(variable = case_when(
  #   str_detect(variable, "unadj_hr") ~ "HR",
  #   str_detect(variable, "ci_unadj_lower") ~ "ci_lower",
  #   str_detect(variable, "ci_unadj_upper") ~ "ci_upper",
  #   str_detect(variable, "iptw_hr") ~ "HR (IPTW)",
  #   str_detect(variable, "ci_iptw_lower") ~ "CI lower (IPTW)",
  #   str_detect(variable, "ci_iptw_upper") ~ "CI upper (IPTW)"
  # )) %>% 
  mutate(estimate_type = ifelse(str_detect(variable, "iptw"), "IPTW", "Unadjusted")) %>%
  select(outcome_event, estimate_type, variable, value) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  mutate(outcome_event = factor(outcome_event, levels = c("pos_covid_test", "covid_hes", "covid_death")),
         HR = coalesce(hr_unadj, `hr_iptw`),
         `ci_lower` = coalesce(`ci_unadj_lower`, `ci_iptw_lower`),
         `ci_upper` = coalesce(`ci_unadj_upper`, `ci_iptw_upper`)) %>%
  select(-`hr_iptw`, -`ci_iptw_lower`, -`ci_iptw_upper`, -`hr_unadj`, -`ci_unadj_lower`, -`ci_unadj_upper`)


forest_table <- results_table %>% 
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(HR, ci_lower, ci_upper),
    ~ str_pad(
      round(.x, 2),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
  estimate_lab = paste0(HR, " (", ci_lower, "-", ci_upper, ")")) 


# #forest_table <- forest_table %>%
#   bind_rows(
#     data.frame(
#       outcome_event = "Outcome",
#       estimate_type = "Model",
#       estimate_lab = "Hazard Ratio (95% CI)",
#       ci_lower = "",
#       ci_upper = ""
#     )
#   )

forest_table <- forest_table %>%
  mutate(outcome_event = fct_recode(outcome_event,
                                    "Positive COVID-19 test" = "pos_covid_test",
                                    "COVID-19 hospitalisation" = "covid_hes",
                                    "COVID-19 death" = "covid_death"
  ))

#generate order variable for plotting
forest_table$order <- c(1,2,3,4,5,6)
forest_table$order <- as.factor(forest_table$order)

#generate forest plot
forest_table <- forest_table %>%
  mutate(
    hazard_ratio = as.numeric(HR),
    lower_CI = as.numeric(ci_lower),
    upper_CI = as.numeric(ci_upper)
  )

forest_plot <- ggplot(forest_table, aes(x = hazard_ratio, y = fct_rev(order))) + 
  geom_point(aes(x = hazard_ratio), shape = 16, size = 3) +
  geom_linerange(aes(xmin = lower_CI, xmax = upper_CI)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Hazard Ratio", y = "") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

print(forest_plot)


est <- ggplot(forest_table, aes(y = fct_rev(order))) +
  geom_text(aes(x = 0, label = outcome_event), hjust = 0, fontface = "bold") + 
  geom_text(aes(x = 1.5, label = estimate_lab, fontface = ifelse(estimate_lab == "Hazard Ratio (95% CI)", "bold", "plain")),
    hjust = 0) +
  geom_text(aes(x = 1, label = estimate_type),
    hjust = 0) +
  theme_void() +
  coord_cartesian(xlim = c(0, 3))

est <- est + theme(text = element_text(size = 7))
print(est)



layout <- c(
  area(t = 0, l = 0, b = 30, r = 50),
  area(t = 0, l = 40, b = 30, r = 70)
)

final_plot <- est + forest_plot + plot_layout(design = layout)
print(final_plot)
