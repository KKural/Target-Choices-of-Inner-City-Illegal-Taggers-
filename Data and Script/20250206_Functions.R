# Call library for required fonts----- 
library(extrafont)
# Load fonts into R
loadfonts(device = "win") 

# Function: create sub folder ---------------------------------------------------
# Function to create a folder with a date argument---------
make_folder <- function(date = Sys.Date()) {
  # Convert the provided date to "YYYYMMDD" format
  folder_name <- format(as.Date(date), "%Y%m%d")
  
  # Define the full folder name with additional text
  full_folder_name <- paste0(folder_name, "_output_target_consistency")
  
  # Check if the folder exists, and create it if it doesn't
  if (!dir.exists(here::here(full_folder_name))) {
    dir.create(here::here(full_folder_name))
    message("Folder created: ", full_folder_name)
  } else {
    message("Folder already exists: ", full_folder_name)
  }
  
  return(full_folder_name)  # Return the folder name to use later
}

# Create the folder
folder_name <- make_folder()

# custom theme for ggplot 
custom_theme <- function() {
  ggplot2::theme(
    legend.position = "none",  # Remove legend
    plot.background = ggplot2::element_rect(fill = "white", color = NA),  # White background
    text = ggplot2::element_text(family = "Times New Roman", size = 12),  # Times New Roman font for all text
    axis.title = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),  # Bold axis titles
    axis.text = ggplot2::element_text(size = 12, family = "Times New Roman"),  # Consistent axis text font size
    axis.line = ggplot2::element_line(color = "black"),  # Black lines for x and y axes
    plot.title = ggplot2::element_text(family = "Times New Roman", size = 12, face = "bold", hjust = 0.5)  # Bold, centered title
  )
}

# plot function 
# A helper operator: if a value is NULL, use the default provided
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# Function for both HGDI and WNODF plots 
plot_diversity <- function(simulation_name, dataframe, measure_str,
                           index_type = c("HGDI", "WNODF"),
                           x_limits,          # vector: c(xmin, xmax)
                           annot1,            # first annotation list
                           annot2,            # second annotation list
                           density_size = 1,
                           seg_linewidth = 1.5,
                           text_size = 4,
                           fill = "grey",     # used only for WNODF
                           title = "") {
  
  index_type <- match.arg(index_type)
  simulation <- get(paste0(simulation_name, "_min_2"))
  measure <- rlang::as_name(rlang::enquo(measure_str))
  
  if (index_type == "HGDI") {
    # Compute observed HGDI using your custom function
    observed_mean <- dataframe |> 
      compute_HGDI(min_alias = 2, summarize = TRUE) |> 
      dplyr::select(!!rlang::sym(measure)) |> 
      dplyr::pull()
    
    simulated_mean <- mean(simulation[[measure]])
    max_y <- max(table(simulation[[measure]])) * 1.1
    
    p <- simulation |>
      ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(measure))) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..), fill = "grey", color = "black", bins = 30) +
      ggplot2::geom_density(color = "black", linetype = "solid", size = density_size) +
      # Observed HGDI vertical line
      ggplot2::annotate("segment", x = observed_mean, xend = observed_mean,
                        y = 0, yend = max_y * 20.5,
                        color = "black", linetype = "dashed", linewidth = seg_linewidth) +
      # Observed and simulated HGDI labels
      ggplot2::annotate("text", x = observed_mean, y = 0,
                        label = paste("Observed HGDI:", round(observed_mean, 3)),
                        color = "black", hjust = 0.4, vjust = 1.5,
                        size = text_size, family = "Times New Roman") +
      ggplot2::annotate("text", x = simulated_mean, y = 0,
                        label = sprintf("Simulated HGDI: %.3f", simulated_mean),
                        color = "black", hjust = 0.5, vjust = 1.5,
                        size = text_size, family = "Times New Roman") +
      ggplot2::geom_point(ggplot2::aes(x = simulated_mean, y = 0), color = "black", size = 1) +
      # First annotation (e.g., "Lower Consistency")
      ggplot2::annotate("text", x = annot1$text_x, y = annot1$text_y,
                        label = annot1$label, angle = annot1$angle, color = "black",
                        size = annot1$text_size %||% text_size, family = "Times New Roman") +
      ggplot2::annotate("segment", x = annot1$seg_x_start, xend = annot1$seg_x_end,
                        y = annot1$seg_y, yend = annot1$seg_y,
                        arrow = ggplot2::arrow(length = grid::unit(annot1$arrow_length, "cm")), color = "black") +
      # Second annotation (e.g., "Higher Consistency")
      ggplot2::annotate("text", x = annot2$text_x, y = annot2$text_y,
                        label = annot2$label, angle = annot2$angle, color = "black",
                        size = annot2$text_size %||% text_size, family = "Times New Roman") +
      ggplot2::annotate("segment", x = annot2$seg_x_start, xend = annot2$seg_x_end,
                        y = annot2$seg_y, yend = annot2$seg_y,
                        arrow = ggplot2::arrow(length = grid::unit(annot2$arrow_length, "cm")), color = "black") +
      ggplot2::xlim(x_limits[1], x_limits[2]) +
      ggplot2::labs(title = title, x = "HGDI Value", y = "Frequency") +
      custom_theme()
    
  } else if (index_type == "WNODF") {
    # Compute observed WNODF using your custom nested NODF function
    observed_mean <- compute_nestednodf(table(dataframe$alias, dataframe$target), min_alias = 2)
    observed_mean <- as.numeric(observed_mean)
    simulated_mean <- mean(as.numeric(simulation[[measure]]))
    std_dev_simulated <- sd(as.numeric(simulation[[measure]]))
    max_y <- max(table(simulation[[measure]])) * 1.1
    
    p <- simulation |>
      ggplot2::ggplot(ggplot2::aes(x = as.numeric(!!rlang::sym(measure)))) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..), fill = fill, color = "black", bins = 30) +
      ggplot2::geom_segment(ggplot2::aes(x = observed_mean, xend = observed_mean,
                                y = 0, yend = 0.45),
                            color = "black", linetype = "dashed", linewidth = seg_linewidth) +
      ggplot2::geom_density(ggplot2::aes(x = as.numeric(!!rlang::sym(measure))),
                            color = "black", linetype = "solid", adjust = 1, size = density_size) +
      ggplot2::annotate("text", x = observed_mean, y = 0,
                        label = paste("Observed WNODF:", sprintf("%.3f", observed_mean)),
                        color = "black", hjust = 0.35, vjust = 1.5,
                        size = text_size, family = "Times New Roman") +
      ggplot2::annotate("text", x = simulated_mean, y = 0,
                        label = paste("Simulated WNODF:", sprintf("%.3f", simulated_mean)),
                        color = "black", hjust = 0.5, vjust = 1.5,
                        size = text_size, family = "Times New Roman") +
      ggplot2::geom_point(ggplot2::aes(x = simulated_mean, y = 0), color = "black", size = 1) +
      # First annotation (e.g., "Stronger Specificity")
      ggplot2::annotate("text", x = annot1$text_x, y = annot1$text_y,
                        label = annot1$label, angle = annot1$angle, color = "black",
                        size = annot1$text_size %||% text_size, family = "Times New Roman") +
      ggplot2::annotate("segment", x = annot1$seg_x_start, xend = annot1$seg_x_end,
                        y = annot1$seg_y, yend = annot1$seg_y,
                        arrow = ggplot2::arrow(length = grid::unit(annot1$arrow_length, "cm")), color = "black") +
      # Second annotation (e.g., "Weaker Specificity")
      ggplot2::annotate("text", x = annot2$text_x, y = annot2$text_y,
                        label = annot2$label, angle = annot2$angle, color = "black",
                        size = annot2$text_size %||% text_size, family = "Times New Roman") +
      ggplot2::annotate("segment", x = annot2$seg_x_start, xend = annot2$seg_x_end,
                        y = annot2$seg_y, yend = annot2$seg_y,
                        arrow = ggplot2::arrow(length = grid::unit(annot2$arrow_length, "cm")), color = "black") +
      ggplot2::scale_x_continuous(limits = x_limits, breaks = seq(x_limits[1], x_limits[2], by = 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 0.45)) +
      ggplot2::labs(title = title, x = "WNODF Value", y = "Frequency") +
      custom_theme()
  }
  
  return(p)
}


# Function: quick-and-easy ggplot saver ----------------------------------------
ggsave_jpeg <- function(ggp, output = "output", width = 8, height = 6, dpi = 600) {
  ggplot2::ggsave(
    filename = paste0(deparse(substitute(ggp)), ".jpeg", sep = ""),
    device = "png",
    plot = ggp,
    path = output,
    width = width,
    height = height,
    dpi = dpi,
    limitsize = TRUE
    
  )
}

# Function: Calculates HGDI index ---------------------
compute_HGDI <- function(df, min_alias = 2, summarize = TRUE, individual_level = FALSE) {
  # Filter out aliases with fewer than `min_alias` observations
  processed_df <- df |>
    dplyr::group_by(alias) |>
    dplyr::filter(dplyr::n() >= min_alias)
  
  # Calculate HGDI for each alias
  processed_df <- processed_df |>
    dplyr::mutate(target = as.factor(target)) |>
    dplyr::summarize(
      HGDI = {
        freqtab <- table(target)
        numerator <- sum(freqtab * (freqtab - 1))
        freqsum <- sum(freqtab)
        denominator <- freqsum * (freqsum - 1)
        1 - (numerator / denominator)
      },
      .groups = "drop"
    )
  
  # Return individual-level results if requested
  if (individual_level) {
    return(processed_df)
  }
  
  # Summarize the mean HGDI across aliases if `summarize` is TRUE
  if (summarize) {
    processed_df <- processed_df |>
      dplyr::summarize(HGDI = mean(HGDI))
  }
  
  return(processed_df)
}

# Function: Calculate WNODF--------
compute_nestednodf <- function(matrix, min_alias = 2) {
  # Apply row filter based on minimum aliases
  filtered_matrix <- matrix[rowSums(matrix) >= min_alias, ]
  
  # Compute the nestednodf metric
  result <- vegan::nestednodf(filtered_matrix, order = TRUE, weighted = TRUE)
  
  # Return the result as a tibble containing the NODF statistic
  nodf_result <- result$statistic["NODF"]
  
  # Format the result with the desired number of decimal places
  formatted_result <- format(nodf_result, nsmall = 10, scientific = FALSE)
  
  return(tibble::tibble(WNODF = as.numeric(formatted_result)))
}

# Previous randomize function-------
set.seed(1234)
randomize <- function(df, fix_alias = TRUE, fix_target = TRUE) {
  df |>
    dplyr::mutate(target = sample(target, replace = !fix_target),
                  alias = sample(alias, replace = !fix_alias)
    ) 
}


# Simulation function HGDI------
Simulate_HGDI <- function(data, n_iterations, min_alias_value) {
  1:n_iterations |> 
    purrr::map(~ {
      data |>
        randomize(fix_target = FALSE, fix_alias = TRUE) |>
        compute_HGDI(min_alias = min_alias_value, summarize = TRUE)
    }) |> purrr::list_rbind(names_to = "iteration")
}

# Function to simulate WNODF
Simulate_WNODF <- function(data, n_iterations, min_alias_value) {
  1:n_iterations |> 
    purrr::map(~ {
      randomized_df <- randomize(data, fix_target = FALSE, fix_alias = TRUE)
      randomized_matrix <- table(randomized_df$alias, randomized_df$target)
      compute_nestednodf(randomized_matrix, min_alias = min_alias_value)
    }) |> purrr::list_rbind(names_to = "iteration")
}

calculate_p_value <- function(simulated_data, observed_value) {
  proportion <- sum(simulated_data < observed_value) / length(simulated_data)
  return(proportion)
}

# comparison row function
generate_comparison_row <- function(minimum_targets) {
  # Retrieve observed values
  observed_key <- paste0("Observed_HGDI_min_", minimum_targets)
  observed_HGDI <- observed_HGDI_values_list[[observed_key]]$HGDI
  observed_HGDI <- round(observed_HGDI, 3)
  
  # Compute observed WNODF
  observed_wnodf_key <- paste0("Observed_WNODF_min_", minimum_targets)
  observed_WNODF <- observed_wnodf_values_list[[observed_wnodf_key]]
  observed_WNODF <- round(observed_WNODF, 3)
  
  # Simulated mean values
  simulated_HGDI <- mean(simulated_dataset_list[["HGDI"]][[minimum_targets - 1]]$HGDI)
  simulated_HGDI <- round(simulated_HGDI, 3)
  simulated_WNODF <- simulated_WNODF_list[["WNODF"]][[minimum_targets - 1]]$WNODF
  simulated_WNODF <- round(mean(simulated_WNODF), 3)
  
  # P-values
  p_value_HGDI <- calculate_p_value(simulated_dataset_list[["HGDI"]][[minimum_targets - 1]]$HGDI, observed_HGDI)
  p_value_HGDI <- round(p_value_HGDI, 3)
  p_value_WNODF <- calculate_p_value(simulated_WNODF, observed_WNODF)
  p_value_WNODF <- round(p_value_WNODF, 3)
  
  # Return a tibble with all the computed values
  tibble::tibble(
    "Minimum Targets" = minimum_targets,
    "Observed HGDI" = observed_HGDI,
    "Simulated HGDI" = simulated_HGDI,
    "p-value HGDI" = p_value_HGDI,
    "Observed WNODF" = observed_WNODF,
    "Simulated WNODF" = mean(simulated_WNODF),
    "p-value WNODF" = p_value_WNODF
  )
}