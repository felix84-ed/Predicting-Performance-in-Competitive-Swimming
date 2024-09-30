# Load necessary library
library(dplyr)

# Define the file path
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"

# Read the dataset
data <- read.csv(file_path)

# Calculate the cumulative number of years in Top 100 for each athlete
data <- data %>%
  group_by(unique_id) %>%
  arrange(Alter) %>%
  mutate(years_in_top100_cumulative = sapply(Alter, function(x) sum(unique(Alter) <= x)))

# Calculate the cumulative number of years in Top 100 for each athlete within each discipline
data <- data %>%
  group_by(unique_id, Disziplin) %>%
  arrange(Alter) %>%
  mutate(years_in_top100_cumulative_discipline = sapply(Alter, function(x) sum(unique(Alter) <= x)))

# Save the updated dataset to the same CSV file
write.csv(data, file = file_path, row.names = FALSE)

# Print the first few rows to verify the changes
head(data)
