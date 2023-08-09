# Author: Marleen Bokern
# Date: 03/2023
# Purpose: make forest plot from cox regression results. cox regression is done in copd_wave1.R

library(broom)
library(ggplot2)
library(tidyverse)
library(gt)

#create summary table of regression results
tidy_results <- bind_rows(
  broom::tidy(cox1) %>% mutate(model_name = "cox1"),
  broom::tidy(cox2) %>% mutate(model_name = "cox2"),
)

tidy_results <- tidy_results %>%
  mutate(
    hazard_ratio = exp(estimate),
    lower_CI = exp(estimate - 1.96 * std.error),  # 95% confidence interval
    upper_CI = exp(estimate + 1.96 * std.error)
  ) %>%
  select(model_name, everything())



est_table <- tidy_results %>% 
  # round estimates and 95% CIs to 2 decimal places for journal specifications
  mutate(across(
    c(estimate, hazard_ratio, lower_CI, upper_CI),
    ~ str_pad(
      round(.x, 2),
      width = 4,
      pad = "0",
      side = "right"
    )
  ),
  # add an "-" between HR estimate confidence intervals
  estimate_lab = paste0(hazard_ratio, " (", lower_CI, "-", upper_CI, ")")) %>% 
  # round p-values to two decimal places, except in cases where p < .001
  mutate(p.value = case_when(
    p.value < 0.001 ~ "< 0.001",
    round(p.value, 2) == .05 ~ as.character(round(p.value,3)),
    p.value < .01 ~ str_pad( # if less than .01, go one more decimal place
      as.character(round(p.value, 3)),
      width = 4,
      pad = "0",
      side = "right"
    ),
    TRUE ~ str_pad( # otherwise just round to 2 decimal places and pad string so that .2 reads as 0.20
      as.character(round(p.value, 2)),
      width = 4,
      pad = "0",
      side = "right"
    )
  ) 
)  


est_table <- est_table %>%
  bind_rows(
    data.frame(
      model_name = "Model",
      estimate_lab = "Hazard Ratio (95% CI)",
      lower_CI = "",
      upper_CI = "",
      p.value = "p-value"
    )
  )

est_table <- rbind(est_table[3,],est_table[1:2,])
est_table
est_table$order <- c("Model 0", "Model 1", "Model 2")

est <- ggplot(est_table, aes(y = fct_rev(order))) +
  geom_text(aes(x = 0, label = model_name), hjust = 0, fontface = "bold") + 
  geom_text(
    aes(x = 1, label = estimate_lab, fontface = ifelse(estimate_lab == "Hazard Ratio (95% CI)", "bold", "plain")),
    hjust = 0
  ) +
  theme_void() +
  coord_cartesian(xlim = c(0, 4))

est


#make the forest plot
forest <- ggplot(est_table, aes(x = hazard_ratio, y = fct_rev(order))) + 
  geom_point(aes(x = hazard_ratio), shape = 16, size = 3) +
  geom_linerange(aes(xmin = lower_CI, xmax = upper_CI)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Hazard Ratio", y = "") + 
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

forest 

library(patchwork)

layout <- c(
  area(t = 0, l = 0, b = 30, r = 3),
  area(t = 1, l = 4, b = 30, r = 9)
)

est + forest + plot_layout(design = layout)
