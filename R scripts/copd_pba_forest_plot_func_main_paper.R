# Author: Marleen Bokern
# Date: 12/2023
# Purpose: Combine all effect estimates from 

packages <- c("tidyverse", "ggplot2", "gt", "arrow", "patchwork", "forestplot")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

setwd(Datadir_copd)

forest_plot_qba <- function(hosp_summary_res, hosp_record_res, death_summary_res, death_record_res, output_ext) {
  
  hosp_summary <- read_parquet(file.path(Tables, "QBA", hosp_summary_res)) %>% dplyr::select(-contains("hr_"), -contains("rr_"))
  hosp_record <- read_parquet(file.path(Tables, "QBA", hosp_record_res)) %>%  dplyr::select(-contains("hr_"), -contains("rr_"))
  death_summary <- read_parquet(file.path(Tables, "QBA", death_summary_res)) %>%  dplyr::select(-contains("hr_"), -contains("rr_"))
  death_record <- read_parquet(file.path(Tables, "QBA", death_record_res)) %>%  dplyr::select(-contains("hr_"), -contains("rr_"))
  
  # Combine all effect estimates and keep rownames
  estimates <- bind_cols(hosp_summary, hosp_record, death_summary, death_record)
  
  #transpose the data frame
  estimates <- t(estimates)
  #convert estimates to df
  estimates <- as.data.frame(estimates)
  
  #display v4 not in scientific notation
  estimates$V4 <- format(estimates$V4, scientific = FALSE)
  
  #rename the columns v1, v2, v3
  estimates <- estimates %>%
    rename(
      point_estimate = V2,
      ci_lower = V1,
      ci_upper = V3, 
      simulations = V5
    )
  
  #add rownames as a column
  estimates <- estimates %>% 
    rownames_to_column("estimate")
  
  estimates <- estimates %>% 
    mutate(reg = case_when(
      stringr::str_detect(tolower(as.character(estimate)), "or_") ~ "OR",
      TRUE ~ estimate)) %>% 
    mutate(outcome = case_when(
      stringr::str_detect(tolower(as.character(estimate)), "death_") ~ "Death",
      stringr::str_detect(tolower(as.character(estimate)), "hosp_") ~ "Hosp",
      TRUE ~ estimate)) %>%
    mutate(qba = case_when(
      stringr::str_detect(tolower(as.character(estimate)), "summary") ~ "Summary",
      stringr::str_detect(tolower(as.character(estimate)), "record") ~ "Record",
      TRUE ~ estimate)) %>% 
    mutate(estimate_type = case_when(
      stringr::str_detect(tolower(as.character(estimate)), "ate_") ~ "IPTW",
      TRUE ~ "Unweighted"))
  
  
  forest_table <- estimates %>%
    mutate(across(
      c(point_estimate, ci_lower, ci_upper),
      ~ sprintf("%6.2f", .x)
    ),
    estimate_lab = paste0(trimws(point_estimate), " (", trimws(ci_lower), "-", trimws(ci_upper), ")")
    )
  
  forest_table <- forest_table %>%
    mutate(outcome = fct_recode(outcome,
                                "COVID-19 hospitalisation" = "Hosp",
                                "COVID-19 death" = "Death"
    ))
  
  #generate forest plot
  forest_table <- forest_table %>%
    arrange(outcome, estimate_type, qba) %>% 
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
    geom_point(aes(x = point_estimate), shape = 16, size = 5) +
    geom_linerange(aes(xmin = lower_CI, xmax = upper_CI)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    labs(x = "Effect estimate", y = "") + 
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 22),
          axis.title.x = element_text(size = 22)) +
    scale_x_continuous(trans='log10', breaks = c(1, 2, 4, 8, 10), limits = c(0.75, 8))
  
  print(forest_plot)
  
  est <- ggplot(forest_table, aes(y = order)) +
    geom_text(aes(x = 0, label = outcomeno), hjust = 0, fontface = "bold", size = 23/.pt) +
    geom_text(aes(x = 1, label = qba), hjust = 0, size = 23/.pt) +
    geom_text(aes(x = 1.5, label = estimate_type), hjust = 0, size = 23/.pt) +
    geom_text(aes(x = 2, label = estimate_lab), hjust = 0, size = 23/.pt) +
    theme_void() +
    coord_cartesian(xlim = c(0, 3))
  
  #est <- est + theme(text = element_text(size = 8))
  print(est)
  
  # sims <- ggplot(forest_table, aes(y = order)) +
  #   geom_text(aes(x = 0, label = simulations), hjust = 0, size = 12/.pt) +
  #   theme_void() +
  #   coord_cartesian(xlim = c(0, 0.3))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 75),
    area(t = 0, l = 65, b = 30, r = 100))
  
  final_forest <- est + forest_plot + plot_layout(design = layout)
  
  final_forest
  
  #save plot
  file_path <- file.path(Graphdir, "QBA", paste0("forest_plot_qba_main_paper", output_ext, ".png"))
  ggsave(filename = file_path, plot = final_forest, width = 18, height = 7, units = "in", dpi = 1000)
  
}


hosp_summary_res <- c("qba_hosp_summary_results.parquet", "qba_hosp_summary_results_no_triple.parquet")
hosp_record_res <- c("qba_hosp_record_results.parquet", "qba_hosp_record_results_no_triple.parquet")
death_summary_res <- c("qba_death_summary_results.parquet", "qba_death_summary_results_no_triple.parquet")
death_record_res <- c("qba_death_record_results.parquet", "qba_death_record_results_no_triple.parquet")
output_ext <- c("", "_no_triple")

# hosp_summary_res <- c("qba_hosp_summary_results_no_triple.parquet")
# hosp_record_res <- c("qba_hosp_record_results_no_triple.parquet")
# death_summary_res <- c("qba_death_summary_results_no_triple.parquet")
# death_record_res <- c("qba_death_record_results_no_triple.parquet")
# output_ext <- c("_no_triple")
# 

mapply(forest_plot_qba, hosp_summary_res, hosp_record_res, death_summary_res, death_record_res, output_ext)
