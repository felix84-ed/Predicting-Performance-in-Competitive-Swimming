# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load your data
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Varianzkoeffizienten_und_andere_Parameter_pro_Disziplin_und_Alter.csv")

# Create a mapping for shorter discipline names for the legend
data$Discipline_Short <- ifelse(grepl("50m F", data$Disziplin_Kombi), "50m FR",
                                ifelse(grepl("50m R", data$Disziplin_Kombi), "50m BA",
                                       ifelse(grepl("50m B", data$Disziplin_Kombi), "50m BR",
                                              ifelse(grepl("50m S", data$Disziplin_Kombi), "50m FL",
                                                     ifelse(grepl("100m F", data$Disziplin_Kombi), "100m FR",
                                                            ifelse(grepl("100m R", data$Disziplin_Kombi), "100m BA",
                                                                   ifelse(grepl("100m B", data$Disziplin_Kombi), "100m BR",
                                                                          ifelse(grepl("100m S", data$Disziplin_Kombi), "100m FL",
                                                                                 ifelse(grepl("200m F", data$Disziplin_Kombi), "200m FR",
                                                                                        ifelse(grepl("200m R", data$Disziplin_Kombi), "200m BA",
                                                                                               ifelse(grepl("200m B", data$Disziplin_Kombi), "200m BR",
                                                                                                      ifelse(grepl("200m S", data$Disziplin_Kombi), "200m FL",
                                                                                                             ifelse(grepl("200m L", data$Disziplin_Kombi), "200m IM",
                                                                                                                    ifelse(grepl("400m F", data$Disziplin_Kombi), "400m FR",
                                                                                                                           ifelse(grepl("400m L", data$Disziplin_Kombi), "400m IM",
                                                                                                                                  ifelse(grepl("800m F", data$Disziplin_Kombi), "800m FR",
                                                                                                                                         ifelse(grepl("1500m F", data$Disziplin_Kombi), "1500m FR", "Other")))))))))))))))))

# Filter data for the different distance categories
data_50m <- data %>% filter(grepl("50m", Disziplin_Kombi))
data_100m <- data %>% filter(grepl("100m", Disziplin_Kombi))
data_200m <- data %>% filter(grepl("200m", Disziplin_Kombi))
data_400m_plus <- data %>% filter(grepl("400m|800m|1500m", Disziplin_Kombi))

# Function to create the plot for a given dataset
plot_variation <- function(data) {
  ggplot(data, aes(x = Alter, y = Varianzkoeffizient, color = Discipline_Short)) +
    geom_line() +
    geom_point() +
    labs(x = "Age (years)", y = "Average Coefficient of Variation") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 24),    # Increase axis title size
      axis.text = element_text(size = 24),     # Increase axis text size
      legend.text = element_text(size = 24),   # Increase legend text size
      legend.position = c(0.5, 0.9),           # Position the legend at the bottom right
      legend.title = element_blank()           # Remove legend title
    )
}

# Create and display plots
plot_50m <- plot_variation(data_50m)
plot_100m <- plot_variation(data_100m)
plot_200m <- plot_variation(data_200m)
plot_400m_plus <- plot_variation(data_400m_plus)

# Define the path for saving plots
save_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/BachelorArbeit"

# Function to save the plot to the specified path
save_plot <- function(plot, filename) {
  ggsave(filename = file.path(save_path, filename), plot = plot, width = 10, height = 8, dpi = 300)
}

# Save each plot
save_plot(plot_50m, "plot_50m.png")
save_plot(plot_100m, "plot_100m.png")
save_plot(plot_200m, "plot_200m.png")
save_plot(plot_400m_plus, "plot_400m_plus.png")

# Print plots to display in R console (optional)
print(plot_50m)
print(plot_100m)
print(plot_200m)
print(plot_400m_plus)
