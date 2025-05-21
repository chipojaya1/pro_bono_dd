# Silent package installation and loading
suppressMessages({
  if (!require("moments", quietly = TRUE)) install.packages("moments", quiet = TRUE)
  if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse", quiet = TRUE)
  if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2", quiet = TRUE)
  if (!require("ggrepel", quietly = TRUE)) install.packages("ggrepel", quiet = TRUE)
  library(moments)
  library(tidyverse)
  library(ggplot2)
  library(ggrepel)
})

setwd("~/Pro Bono")
data <- read.csv("Direct Development Working File.csv")
str(data)

##### Skewness/Kurtosis #####
projects_data <- data %>% 
  filter(Category == "Project") %>% 
  select(CompareRevHour)

projects_skewness <- skewness(projects_data$CompareRevHour, na.rm = TRUE)
projects_kurtosis <- kurtosis(projects_data$CompareRevHour, na.rm = TRUE)

retainers_data <- data %>% 
  filter(Category == "Retainer") %>% 
  select(CompareRevHour)

retainers_skewness <- skewness(retainers_data$CompareRevHour, na.rm = TRUE)
retainers_kurtosis <- kurtosis(retainers_data$CompareRevHour, na.rm = TRUE)

##### Results #####
results <- data.frame(
  Category = c("Project", "Retainer"),
  Skewness = c(projects_skewness, retainers_skewness),
  Kurtosis = c(projects_kurtosis, retainers_kurtosis)
)

print(results)

##### Printouts #####
analyze_category <- function(category_name, df) {
  cat_data <- df %>% 
    filter(Category == category_name) %>% 
    pull(CompareRevHour) %>% 
    na.omit()
  
  skew <- skewness(cat_data)
  kurt <- kurtosis(cat_data)
  var_5 <- quantile(cat_data, probs = 0.05)
  
  cvar_5 <- mean(cat_data[cat_data <= var_5])
  
  cat("\nCategory:", category_name, "\n")
  cat("Skewness:", round(skew, 4), "\n")
  cat("Kurtosis:", round(kurt, 4), "\n")
  cat("VaR at 5%:", round(var_5, 4), "\n")
  cat("CVaR at 5%:", round(cvar_5, 4), "\n")
  
  invisible(list(skewness = skew, kurtosis = kurt, var_5 = var_5, cvar_5 = cvar_5))
}

# Analyze both categories
project_stats <- analyze_category("Project", data)
retainer_stats <- analyze_category("Retainer", data)

# CompareRevHour analysis - preserves all data
cat("\n=== COMPARE REV HOUR ANALYSIS ===\n")

analyze_metric <- function(metric_name, df = data) {
  df %>%
    filter(Category %in% c("Project", "Retainer")) %>%
    group_by(Category) %>%
    summarise(
      N = n(),
      N_NA = sum(is.na(.data[[metric_name]])),
      Mean = mean(.data[[metric_name]], na.rm = TRUE),
      SD = sd(.data[[metric_name]], na.rm = TRUE),
      Skewness = skewness(.data[[metric_name]], na.rm = TRUE),
      Kurtosis = kurtosis(.data[[metric_name]], na.rm = TRUE),
      VaR_5 = quantile(.data[[metric_name]], 0.05, na.rm = TRUE),
      CVaR_5 = mean(.data[[metric_name]][.data[[metric_name]] <= VaR_5], na.rm = TRUE)
    ) %>%
    mutate(across(Mean:CVaR_5, ~round(., 4))) %>%
    knitr::kable(caption = paste(metric_name, "Distribution Metrics"))
}

# Run analyses
print(analyze_metric("CompareRevHour"))
print(analyze_metric("Hours_Budget"))
print(analyze_metric("CompareProfitHour"))

# Enhanced visualization function with both histogram and density options
create_revenue_plot <- function(plot_type = "histogram") {
  plot_data <- data %>% 
    filter(Category %in% c("Project", "Retainer"),
           !is.na(CompareRevHour))
  
  stats_data <- plot_data %>%
    group_by(Category) %>%
    summarise(
      Mean = mean(CompareRevHour),
      Median = median(CompareRevHour),
      VaR_5 = quantile(CompareRevHour, 0.05),
      CVaR_5 = mean(CompareRevHour[CompareRevHour <= VaR_5]),
      Max_Density = max(hist(CompareRevHour, breaks = 30, plot = FALSE)$density)
    ) %>%
    mutate(Mean_Label = paste0("Mean: ", round(Mean, 1)),
           CVaR_Label = paste0("CVaR (5%): ", round(CVaR_5, 1)))
  
  p <- ggplot(plot_data, aes(x = CompareRevHour, fill = Category)) +
    # Add distribution
    (if(plot_type == "histogram") {
      geom_histogram(aes(y = after_stat(count)), alpha = 0.6, 
                     position = "identity", bins = 30)
    } else {
      geom_density(aes(y = after_stat(density)), alpha = 0.6)
    }) +
    # Add mean lines and labels
    geom_vline(data = stats_data,
               aes(xintercept = Mean, color = Category),
               linetype = "solid", linewidth = 1) +
    geom_label_repel(data = stats_data,
                     aes(x = Mean, y = ifelse(plot_type == "histogram", Inf, Max_Density * 0.9), 
                         label = Mean_Label, color = Category),
                     vjust = 1.5, fill = "white", direction = "y",
                     show.legend = FALSE) +
    # Add CVaR lines and labels
    geom_vline(data = stats_data,
               aes(xintercept = CVaR_5, color = Category),
               linetype = "dashed", linewidth = 1) +
    geom_label_repel(data = stats_data,
                     aes(x = CVaR_5, y = ifelse(plot_type == "histogram", Inf, Max_Density * 0.7), 
                         label = CVaR_Label, color = Category),
                     vjust = 1.5, fill = "white", direction = "y",
                     show.legend = FALSE) +
    labs(title = paste("Revenue per Hour Distribution -", 
                       ifelse(plot_type == "histogram", "Count", "Density")),
         x = "Revenue per Hour",
         y = ifelse(plot_type == "histogram", "Count", "Density")) +
    scale_fill_manual(values = c("Project" = "#041e45", "Retainer" = "#ea6115")) +
    scale_color_manual(values = c("Project" = "#041e45", "Retainer" = "#ea6115")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# Generate and display both plot versions
hist_plot <- create_revenue_plot("histogram")
density_plot <- create_revenue_plot("density")

print(hist_plot)
print(density_plot)

# Original plotting function for other metrics
create_distribution_plot <- function(metric_name) {
  plot_data <- data %>% 
    filter(Category %in% c("Project", "Retainer"),
           !is.na(.data[[metric_name]]))
  
  ggplot(plot_data, aes(x = .data[[metric_name]], fill = Category)) +
    geom_density(alpha = 0.6) +
    geom_vline(data = plot_data %>% 
                 group_by(Category) %>% 
                 summarise(Mean = mean(.data[[metric_name]])),
               aes(xintercept = Mean, color = Category), 
               linetype = "dashed", linewidth = 1) +
    labs(title = paste(metric_name, "Distribution"),
         x = metric_name,
         y = "Density") +
    scale_fill_manual(values = c("Project" = "#041e45", "Retainer" = "#ea6115")) +
    scale_color_manual(values = c("Project" = "#041e45", "Retainer" = "#ea6115")) +
    theme_minimal()
}

# Generate plots for other metrics
if("Hours_Budget" %in% names(data)) {
  hours_plot <- create_distribution_plot("Hours_Budget")
  print(hours_plot)
}

if("CompareProfitHour" %in% names(data)) {
  profit_plot <- create_distribution_plot("CompareProfitHour")
  print(profit_plot)
}
