# Silent package installation and loading
suppressMessages({
  if (!require("reshape2", quietly = TRUE)) install.packages("reshape2", quiet = TRUE)
  if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse", quiet = TRUE)
  if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2", quiet = TRUE)
  library(reshape2)
  library(tidyverse)
  library(ggplot2)
})

# Read the CSV file
df <- read_csv("Direct Development Working File.csv")

# Define the employee columns
employee_cols <- c(
  "Project_Owner", "Senior_Consulting", "CST", "CS", "CW",
  "Content", "Design Lead", "Tech", "Digital", "Ads"
)

# Process data with employee frequency filtering
heatmap_data <- df %>%
  # Convert to long format
  pivot_longer(
    cols = all_of(employee_cols),
    names_to = "Role",
    values_to = "Employee"
  ) %>%
  # Remove rows with missing employees
  filter(!is.na(Employee)) %>%
  # Count total items per employee
  group_by(Employee) %>%
  mutate(Employee_Count = n()) %>%
  ungroup() %>%
  # Filter employees with fewer than 10 items
  filter(Employee_Count >= 10) %>%
  # Calculate average profit per employee-category
  group_by(Employee, Category) %>%
  summarise(Avg_ProfitHour = mean(CompareProfitHour, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert to wide format for heatmap
  pivot_wider(
    names_from = Category,
    values_from = Avg_ProfitHour
  )

# Create the heatmap
ggplot(
  heatmap_data %>%
    pivot_longer(
      cols = c("Project", "Retainer"),
      names_to = "Category",
      values_to = "Avg_ProfitHour"
    ),
  aes(x = Category, y = Employee, fill = Avg_ProfitHour)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(Avg_ProfitHour), "", round(Avg_ProfitHour, 1))),
    color = "black",
    size = 3
  ) +
  scale_fill_gradient(
    low = "red",
    high = "green",
    na.value = "white",
    name = "Avg Profit/Hour"
  ) +
  labs(
    title = "Average Profit per Hour (Employees with ≥10 Assignments)",
    x = "Category",
    y = "Employee"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
