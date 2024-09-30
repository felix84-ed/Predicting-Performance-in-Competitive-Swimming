# Load necessary library
library(dplyr)

# Define the file path
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"

# Read the dataset
data <- read.csv(file_path)

# Rename columns
data <- data %>%
  rename(
    "#Nr_Disziplinen(Jahr)" = Anzahl_Disziplinen,
    "#Nr_Dist_Kat(Jahr)" = Anzahl_Distanz_Kategorien,
    "#Nr_Jahre_Top100(Allg)" = Anzahl_Einzigartige_Altersgruppen,
    "#Nr_Jahre_Top100(Disz)" = years_in_top100
  )

# Create new "Event" column
data <- data %>%
  mutate(Event = paste(Streckenlänge, Disziplin, sep = " "))

# Move columns "Streckenlänge" and "Disziplin" to be the third and fourth last columns
data <- data %>%
  select(-Streckenlänge, -Disziplin, everything(), Streckenlänge, Disziplin, name, Jg)

# Move "Event" column after "Geschlecht"
data <- data %>%
  select(everything(), -Event, Geschlecht, Event, everything())

# Save the updated dataset to the same CSV file
write.csv(data, file = file_path, row.names = FALSE)

# Print the first few rows to verify the changes
head(data)
