rm(list = ls())
source(here::here("20250206_Functions.R"))

# read the anonymized data
df_alias_target <- read.csv(here::here("df_alias_target_anonymized.csv"))

raw_graffiti_instances <- nrow(df_alias_target)

# Descriptive statistics --------------------
# filter the offender who appered more than once
df_final <- df_alias_target |>
  dplyr::group_by(alias) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::select(-X, alias, target)

# Number of unique offenders (alias)
num_unique_offenders <- length(unique(df_final$alias))
num_unique_offenders

# Number of target types
num_target_types <- nlevels(df_final$target)
num_target_types

total_graffiti_instances <- nrow(df_final)
total_graffiti_instances

total_aliases <- dplyr::n_distinct(df_final$alias)
total_aliases

# Calculate the merged table in a single streamlined process
prop_table <- df_final |>
  # Calculate counts and proportions for each target type
  dplyr::count(target, name = "Counts") |>
  dplyr::mutate(
    Total_Incidents = sum(Counts),  
    Percentage_of_Total_Incidents = (round((Counts / Total_Incidents) * 100, 2)),
    Proportion_of_Total_Incidents = round(Counts / Total_Incidents, 3)  # Use the newly defined total
  ) |>
  # Add unique alias counts and related statistics
  dplyr::left_join(
    df_final |>
      dplyr::group_by(target) |>
      dplyr::summarise(
        Counts_Unique_Graffiti_Writers = dplyr::n_distinct(alias),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Total_Aliases = dplyr::n_distinct(df_final$alias),
        Percentage_of_Unique_Writers = round(
          (Counts_Unique_Graffiti_Writers / Total_Aliases) * 100, 2),
        Proportions_Unique_Graffiti_Writers = round(
          Counts_Unique_Graffiti_Writers / Total_Aliases, 3)
      ),
    by = "target"
  ) |>
  # Recode target names for readability
  dplyr::mutate(
    target = dplyr::recode(
      target,
      "bench" = "Bench",
      "bridge" = "Bridge",
      "bus_shelter" = "Bus shelter",
      "facade" = "Exposed wall",
      "garage_door" = "Garage door",
      "garbage_can" = "Garbage can",
      "parking_meter" = "Parking meter",
      "shutter" = "Shutter",
      "traffic_sign" = "Traffic sign",
      "transformers" = "Transformers",
      "door" = "Door",
      "window" = "Window"
    )
  ) |>
  # Rename columns and arrange for presentation
  dplyr::rename(
    `Target Type` = target,
    "Counts" = Counts,
    "Percentage (Total Incidents)" = Percentage_of_Total_Incidents,
    "Proportion" = Proportion_of_Total_Incidents,
    "Counts (Unique writers)" = Counts_Unique_Graffiti_Writers,
    "Percentage (Unique Writers)" = Percentage_of_Unique_Writers,
    "Proportions (Unique writers)" = Proportions_Unique_Graffiti_Writers
  ) |>
  dplyr::select(
    `Target Type`,
    Counts,
    "Percentage (Total Incidents)",
    "Proportion",
    "Percentage (Unique Writers)",
    "Counts (Unique writers)",
    "Proportions (Unique writers)"
  ) |>
  dplyr::arrange(desc(Counts))

print(prop_table)


# Calculate individual offenses
individual_offenses <- table(df_final$alias)
avg_offenses <- round(mean(individual_offenses), 3)
sd_offenses <- round(sd(individual_offenses), 3)

# Calculate cumulative offenses and top offenders
sorted_offenses <- sort(individual_offenses, decreasing = TRUE)
cumulative_offenses <- cumsum(sorted_offenses)
top_offenders <- which(cumulative_offenses <= (0.5 * sum(individual_offenses)))
proportion_top_offenders <- round(length(top_offenders) / length(individual_offenses), 3)

# Calculate individual targets and target statistics
individual_targets <- df_final |>
  dplyr::group_by(alias) |>
  dplyr::summarise(num_targets = dplyr::n_distinct(target), .groups = "drop")

avg_target_types <- round(mean(individual_targets$num_targets), 3)
sd_target_types <- round(sd(individual_targets$num_targets), 3)

summary_table <- data.frame(
  Title = c(
    "Raw Graffiti Instances",
    "Total Graffiti Instances (>1)",
    "Total Individuals",
    "Average Offenses",
    "SD Offenses",
    "50 Percent Offenders",
    "Proportion of 50 Percent Offenders",
    "Average Target Types",
    "SD Target Types"
  ),
  Value = c(
    raw_graffiti_instances,
    total_graffiti_instances,
    length(individual_offenses),
    avg_offenses,
    sd_offenses,
    length(top_offenders),
    proportion_top_offenders,
    avg_target_types,
    sd_target_types
  )
)

# View the final combined summary table
print(summary_table)

calculate_top_offenders <- function(individual_offenses) {
  sorted_offenses <- sort(individual_offenses, decreasing = TRUE)
  cumulative_offenses <- cumsum(sorted_offenses)
  percentages <- seq(0.1, 1, by = 0.1)
  
  data_list <- lapply(percentages, function(percentage) {
    top_offenders <- which(cumulative_offenses <= (percentage * sum(individual_offenses)))
    data.frame(
      "Percentage of offenses" = 100 * percentage,
      "Num offenders" = length(top_offenders),
      "Total offenses" = length(individual_offenses),
      "Proportion of offenders" = round(length(top_offenders)/length(individual_offenses),3),
      "Percentage of Offenders" = round(100 * (length(top_offenders)/length(individual_offenses)), 2)
    )
  })
  
  do.call(rbind, data_list)
}

# offense and offender propotation
offense_offonder_prop <- calculate_top_offenders(individual_offenses)
offense_offonder_prop$Percentage.of.offenses <- as.numeric(offense_offonder_prop$Percentage.of.offenses)
print(offense_offonder_prop)

# plot lorentz curve
Figure_1 <- ggplot2::ggplot(offense_offonder_prop, ggplot2::aes(x = Percentage.of.Offenders, y = Percentage.of.offenses, group = 1)) +
  ggplot2::geom_line(color = "black", linewidth = 1.2) +  
  ggplot2::geom_point(color = "black", size = 3) +   
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray", size = 1) +  # Thicker gray dashed line
  ggplot2::labs(title = "",
                x = "Cumulative Percentage of Illegal Taggers",
                y = "Cumulative Percentage of Illegal Tags") +
  ggplot2::theme_minimal() +
  custom_theme()

Figure_1
ggsave_jpeg(Figure_1, output = folder_name)

# Combine all the steps
target_distribution <- df_final |>
  dplyr::group_by(alias) |>
  dplyr::summarise(num_targets = dplyr::n_distinct(target)) |>
  dplyr::group_by(num_targets) |>
  dplyr::summarise(Number_of_Individuals = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Number_of_Targets = num_targets,
    Percentage_of_Individuals = (Number_of_Individuals / total_aliases) * 100,
    Percentage_of_Individuals = round(Percentage_of_Individuals, 2),
    Proportion_of_Individuals = Number_of_Individuals / total_aliases,
    Proportion_of_Individuals = round(Proportion_of_Individuals, 3)
  ) |>
  dplyr::select(Number_of_Targets, Number_of_Individuals, Percentage_of_Individuals, Proportion_of_Individuals)

print(target_distribution)

# get total no of graffiti writers
total_aliases <- sum(target_distribution$Number_of_Individuals)

# plot target distribution plot 
Figure_2 <- ggplot2::ggplot(target_distribution, 
                            ggplot2::aes(x = factor(Number_of_Targets), 
                                         y = Proportion_of_Individuals)) +
  ggplot2::geom_bar(stat = "identity", fill = "gray", color = "black", width = 1) +  # Set width to 1 to remove gaps
  ggplot2::scale_x_discrete(breaks = as.character(1:7), 
                            labels = as.character(1:7)) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(), 
                              breaks = seq(0, 0.5, by = 0.1), 
                              limits = c(0, 0.5)) +  # Adjust to show up to 50%
  ggplot2::labs(title = "",
                x = "Number of Target Types Exploited",
                y = "Percentage of Illegal Taggers") +
  ggplot2::theme_minimal() +
  custom_theme()

Figure_2
ggsave_jpeg(Figure_2, output = folder_name)

# Run simulations --------------------------------------------------------------
n_iterations <- 1000
# Simulate HGDI under
HGDI_min_2 <- Simulate_HGDI(df_alias_target, n_iterations, 2)
HGDI_min_3 <- Simulate_HGDI(df_alias_target, n_iterations, 3)
HGDI_min_4 <- Simulate_HGDI(df_alias_target, n_iterations, 4)
HGDI_min_5 <- Simulate_HGDI(df_alias_target, n_iterations, 5)

# Function to simulate WNODF
WNODF_min_2 <- Simulate_WNODF(df_alias_target, n_iterations, 2)
WNODF_min_3 <- Simulate_WNODF(df_alias_target, n_iterations, 3)
WNODF_min_4 <- Simulate_WNODF(df_alias_target, n_iterations, 4)
WNODF_min_5 <- Simulate_WNODF(df_alias_target, n_iterations, 5)


# Comparison df ----------------------------

# Add datasets to simulated lists
simulated_dataset_list <- list(
  HGDI = list(HGDI_min_2, HGDI_min_3, HGDI_min_4, HGDI_min_5)
)

simulated_WNODF_list <- list(
  WNODF = list(WNODF_min_2, WNODF_min_3, WNODF_min_4, WNODF_min_5)
)

# Add observed values for 
observed_HGDI_values_list <- list(
  Observed_HGDI_min_2 = compute_HGDI(df_alias_target, min_alias = 2, summarize = TRUE),
  Observed_HGDI_min_3 = compute_HGDI(df_alias_target, min_alias = 3, summarize = TRUE),
  Observed_HGDI_min_4 = compute_HGDI(df_alias_target, min_alias = 4, summarize = TRUE),
  Observed_HGDI_min_5 = compute_HGDI(df_alias_target, min_alias = 5, summarize = TRUE)
)

observed_wnodf_values_list <- list(
  Observed_WNODF_min_2 = compute_nestednodf(with(df_alias_target, table(alias, target)), min_alias = 2)$WNODF,
  Observed_WNODF_min_3 = compute_nestednodf(with(df_alias_target, table(alias, target)), min_alias = 3)$WNODF,
  Observed_WNODF_min_4 = compute_nestednodf(with(df_alias_target, table(alias, target)), min_alias = 4)$WNODF,
  Observed_WNODF_min_5 = compute_nestednodf(with(df_alias_target, table(alias, target)), min_alias = 5)$WNODF
)


# Create the comparison data
df_comparison <- purrr::map_dfr(2:5, function(min_alias) {
  generate_comparison_row(min_alias)
}) |> dplyr::distinct()

print(df_comparison)


# Observed HGDI mean
observed_mean_HGDI <- round(mean(observed_HGDI_values_list$Observed_HGDI_min_2$HGDI), 3)

# HGDI individual plot 
observed_HGDI_individual <- compute_HGDI(df_alias_target, min_alias = 2, individual_level = TRUE)
sd_observed_HGDI_individual <- round(sd(observed_HGDI_individual$HGDI),3)

# Simulated HGDI values
simulated_HGDI <- round(simulated_dataset_list$HGDI[[1]]$HGDI, 3)

# Compute means and standard deviations
simulated_mean_HGDI <- round(mean(simulated_HGDI), 3)
simulated_std_dev_HGDI <- round(sd(simulated_HGDI), 3)

# Compute Z-scores, CIs, and p-values
Z_score_HGDI <- round((observed_mean_HGDI - simulated_mean_HGDI) / simulated_std_dev_HGDI, 3)
CI_lower_HGDI <- round(simulated_mean_HGDI - 1.96 * (simulated_std_dev_HGDI / sqrt(length(simulated_HGDI))), 3)
CI_upper_HGDI <- round(simulated_mean_HGDI + 1.96 * (simulated_std_dev_HGDI / sqrt(length(simulated_HGDI))), 3)
p_value_HGDI <- calculate_p_value(simulated_HGDI, observed_mean_HGDI)

# Observed and simulated WNODF
observed_WNODF <- round(observed_wnodf_values_list$Observed_WNODF_min_2, 3)
simulated_WNODF <- simulated_WNODF_list$WNODF[[1]]$WNODF
mean_simulated_WNODF <- round(mean(simulated_WNODF), 3)
std_dev_simulated_WNODF <- round(sd(simulated_WNODF), 3)
Z_score_WNODF <- round((observed_WNODF - mean_simulated_WNODF) / std_dev_simulated_WNODF, 3)
CI_lower_WNODF <- round(mean_simulated_WNODF - 1.96 * (std_dev_simulated_WNODF / sqrt(length(simulated_WNODF))), 3)
CI_upper_WNODF <- round(mean_simulated_WNODF + 1.96 * (std_dev_simulated_WNODF / sqrt(length(simulated_WNODF))), 3)
p_value_WNODF <- round(2 * pnorm(-abs(Z_score_WNODF)), 3)
p_value_WNODF <- calculate_p_value(simulated_WNODF, observed_WNODF)

# Combine statistics into a single data frame
df_stats_combined <- tibble::tibble(
  Measure = c(
    "Observed_Mean_HGDI","sd_observed_HGDI",  "Simulated_Mean_HGDI", "Simulated_SD_HGDI", "Z_score_HGDI", "CI_Lower_HGDI", "CI_Upper_HGDI", "P_value_HGDI",
    "Observed_WNODF", "Simulated_Mean_WNODF", "Simulated_SD_WNODF", "Z_score_WNODF", "CI_Lower_WNODF", "CI_Upper_WNODF", "P_value_WNODF"
  ),
  Value = c(
    observed_mean_HGDI, sd_observed_HGDI_individual, simulated_mean_HGDI, simulated_std_dev_HGDI, Z_score_HGDI, CI_lower_HGDI, CI_upper_HGDI, p_value_HGDI,
    observed_WNODF, mean_simulated_WNODF, std_dev_simulated_WNODF, Z_score_WNODF, CI_lower_WNODF, CI_upper_WNODF, p_value_WNODF
  )
)

df_stats_combined

# Plots ----
Figure_3 <- plot_diversity(
  simulation_name = "HGDI",
  dataframe = df_alias_target,
  measure_str = HGDI,
  index_type = "HGDI",
  x_limits = c(0.575, 0.775),
  annot1 = list(
    text_x = 0.735, text_y = 20, label = "Lower Consistency", angle = 0,
    seg_x_start = 0.717, seg_x_end = 0.755, seg_y = 19.3, arrow_length = 0.3,
    text_size = 4
  ),
  annot2 = list(
    text_x = 0.608, text_y = 20, label = "Higher Consistency", angle = 0,
    seg_x_start = 0.627, seg_x_end = 0.59, seg_y = 19.3, arrow_length = 0.3,
    text_size = 4
  ),
  title = ""
)
print(Figure_3)
ggsave_jpeg(Figure_3, output = folder_name)


# Create the histogram for Between-Person Variability in HGDI values
Figure_4 <- ggplot2::ggplot(observed_HGDI_individual, ggplot2::aes(x = HGDI)) +
  ggplot2::geom_histogram(binwidth = 0.05, fill = "gray", color = "black") +
  ggplot2::labs(title = "",
                x = "HGDI Value",
                y = "Number of Illegal Taggers") +
  ggplot2::theme_minimal()+
  custom_theme()

Figure_4
ggsave_jpeg(Figure_4, output = folder_name)

Figure_5 <- plot_diversity(
  simulation_name = "WNODF",
  dataframe = df_alias_target,
  measure_str = WNODF,
  index_type = "WNODF",
  x_limits = c(25, 35),
  annot1 = list(
    text_x = 26.7, text_y = 0.4, label = "Stronger Specificity", angle = 0,
    seg_x_start = 27.6, seg_x_end = 25.8, seg_y = 0.385, arrow_length = 0.3,
    text_size = 4
  ),
  annot2 = list(
    text_x = 33.5, text_y = 0.4, label = "Weaker Specificity", angle = 0,
    seg_x_start = 32.6, seg_x_end = 34.4, seg_y = 0.385, arrow_length = 0.3,
    text_size = 4
  ),
  fill = "grey",
  title = ""
)
print(Figure_5)
ggsave_jpeg(Figure_5, output = folder_name)

# assign df to sheets and create a excel
all_results <- list(
  "Summary_table" = summary_table,
  "Prop_table" = prop_table,
  "Percentage_of_offenses" = offense_offonder_prop,
  "Num_of_targets_chosen" = target_distribution,
  "Stats_combined" = df_stats_combined,
  "Simulation_comparison" = df_comparison)


# assign df to sheets and create a excel
file_path_all_results <- here::here(folder_name, "all_results.xlsx")
writexl::write_xlsx(all_results, file_path_all_results)

