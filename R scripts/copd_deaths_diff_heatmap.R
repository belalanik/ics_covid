############################
# Project: Outcome Misclassification adjustment -- SUMMARY LEVEL
# Programmer name: Marleen Bokern
# Date Started:  01/2024
#############################  


packages <- c("tidyverse", "ggplot2", "gt", "arrow", "MetBrewer", "data.table", "progress")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

setwd(Datadir_copd)

set.seed(123)

D <- read_parquet("copd_wave1_60d_iptw.parquet")
D <- D %>% dplyr::select(patid, treat, treatgroup, covid_death_present, any_death_present, timeinstudy3, ate_weight_stab)

D$covid_death_present <- as.numeric(D$covid_death_present)

D$treatgroup_num <- ifelse(D$treat == "ICS", 1,
                           ifelse(D$treat == "LABA/LAMA", 0, NA))

#create cell counts for later in program
#from analysis without QBA, ATE (stab) weighted Cox model:
total_exp <- sum(D$treat=="ICS")
total_unexp <- sum(D$treat=="LABA/LAMA")

death_exp <- sum(D$treat=="ICS" & D$any_death_present ==1)
death_unexp <- sum(D$treat=="LABA/LAMA" & D$any_death_present ==1)

a1 <- sum(D$treat=="ICS" & D$covid_death_present ==1)
b1 <- sum(D$treat=="LABA/LAMA" & D$covid_death_present ==1)
c1 <- death_exp - a1 #non-covid deaths among exposed
d1 <- death_unexp - b1 #non-covid deaths among unexposed

#among all deaths
n_case <- a1 + b1 #total covid deaths
n_ctrl <- c1 + d1 #total non-covid deaths
n_exp  <- a1 + c1 #total deaths among ICS
n_unexp <- b1 + d1 #total deaths among LABA/LAMA

sensitivities_E1 <- seq(0.5, 1, 0.02)
sensitivities_E0 <- seq(0.5, 1, 0.02)

sp <- 0.97

start_time <- Sys.time()

I <- 100000

pb <- progress_bar$new(total = I*length(sensitivities_E1)*length(sensitivities_E0))
results_list <- list()

for (i in 1:length(sensitivities_E1)) {
  se_E1 <- sensitivities_E1[i]
  
  for (j in 1:length(sensitivities_E0)) {
    se_E0 <- sensitivities_E0[j]
    iteration_results <- vector("list", length = I)
    
    for (k in 1:I) {
      pb$tick()
      
      # calculate bias-adjusted cell frequencies: only among deaths
      ac1 <- round((a1 - death_exp*(1-sp))/(se_E1 - (1-sp))) #bias-adjusted cases, exposed 
      cc1 <- round(death_exp - ac1) #bias-adjusted cases, unexposed
      bc1 <- round((b1 - death_unexp*(1-sp))/(se_E0 - (1-sp))) #bias-adjusted controls, exposed
      dc1 <- round(death_unexp - bc1) #bias-adjusted controls, unexposed
      
      #calculate prevalence of exposure in cases and controls, accounting for sampling error
      PrevD_exp <- rbeta(1, ac1,cc1)
      PrevD_unexp <- rbeta(1, bc1, dc1)
      
      #calculate PPV and NPV of exposure classification in cases and controls
      #these must be calculated separately for exposed and unexposed
      PPV_exp <- (se_E1*PrevD_exp)/((se_E1*PrevD_exp)+(1-sp)*(1-PrevD_exp))
      PPV_unexp <- (se_E0*PrevD_unexp)/((se_E0*PrevD_unexp)+(1-sp)*(1-PrevD_unexp))
      NPV_exp <- (sp*(1-PrevD_exp))/((1-se_E1)*PrevD_exp+sp*(1-PrevD_exp))
      NPV_unexp <- (sp*(1-PrevD_unexp))/((1-se_E0)*PrevD_unexp+sp*(1-PrevD_unexp))
      
      #calculate the expected number of cases among exp and unexp
      #this incorporates error from the misclassification process by using binomial trials
      ab <- rbinom(1,ac1,PPV_exp)+rbinom(1,cc1,1-NPV_exp) ## exposed with outcome
      cb <- n_exp-ab ##exposed without outcome
      bb <- rbinom(1,bc1,PPV_unexp)+rbinom(1,dc1,1-NPV_unexp) ###unexposed with outcome
      db <- n_unexp-bb ###unexposed without outcome
      
      # add back in the people who survived in the unexposed group
      ac <- ab   
      bc <- bb   
      cc <- total_exp - ac  # add back in the people who survived in the exposed group
      dc <- total_unexp - bc # add back in the people who survived in the unexposed group
      
      #calculate bias adjusted RR with second source of uncertainty
      # this incorporates the uncertainty stemming from the misclassification process
      
      or_bb <- (ac*dc)/(bc*cc)
      # calculate bias-adjusted risk ratios, third source of uncertainty, bias-adjusted standard error
      se_bb_or <- sqrt(1/ac+1/bc+1/cc+1/dc)
      #draw from normal distribution to incorporate standard error
      z <- rnorm(1)
      
      or_bb_cb <- exp(log(or_bb) - (z*se_bb_or))
      
      iteration_results[[k]] <- data.frame(Sensitivity_E1 = se_E1, Sensitivity_E0 = se_E0, OR = or_bb_cb)
      
    }
    
    iteration_results <- do.call(rbind, iteration_results)
    
    #calculate median OR and 95% SI
    median_OR <- median(iteration_results$OR)
    lower_CI <- quantile(iteration_results$OR, 0.025)
    upper_CI <- quantile(iteration_results$OR, 0.975)
    
    #add new row to results list for each combination of i and j
    results_list <- c(results_list, list(data.frame(Se_E1 = se_E1, Se_E0 = se_E0, Median_OR = median_OR, Lower_CI = lower_CI, Upper_CI = upper_CI)))
  }
}

# combine all the data frames into a single data frame
all_results <- as.data.frame(do.call(rbind, results_list))

#turn results into a dataframe
results_df <- setDT(all_results)

# Create the Median_OR_factor column based on conditions
results_df$Median_OR_factor <- with(results_df, ifelse(Upper_CI < 1, "lightgreen", ifelse(Lower_CI > 1, "lightpink", "lightyellow")))

# Define the colors and legend labels
colors <- c("lightgreen", "lightpink", "lightyellow")
legend_labels <- c("95% SI < 1", "95% SI > 1", "SI crosses 1")

# Explicitly set the levels of the Median_OR_factor factor to match the order of colors and legend_labels
levels(results_df$Median_OR_factor) <- c("lightgreen", "lightpink", "lightyellow")

# Now, create the plot
plot <- ggplot(data = results_df, aes(x = Se_E1, y = Se_E0, fill = Median_OR_factor)) +
  geom_tile() +
  geom_text(aes(label = round(Median_OR, 2)), size = 6) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 16)) +
  scale_fill_manual(values = colors,
                    labels = legend_labels) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Sensitivity (ICS)", y = "Sensitivity (control group)")

print(plot)
# Save the plot
file_path <- file.path(Graphdir, "QBA", "copd_death_w1", "heatmap_copd_death_differential.png")
ggsave(file_path, plot, height = 12, width = 20, dpi = 300)

end_time <- Sys.time()
print(end_time - start_time)
