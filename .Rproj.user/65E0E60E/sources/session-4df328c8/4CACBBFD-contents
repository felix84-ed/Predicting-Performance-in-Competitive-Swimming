# Notwendige Bibliotheken laden
library(dplyr)
library(tidyr)

# Daten einlesen
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/gesamt_daten.csv"
data <- read.csv(file_path)

# Erzeugen einer eindeutigen Schwimmer-ID
data <- data %>%
  mutate(swimmer_id = paste(name, Jg, sep = "_"))

# Kombinieren von "Streckenlänge" und "Disziplin", um die Events/Disziplinen zu erhalten
data <- data %>%
  mutate(event = paste(Streckenlänge, Disziplin, sep = " "))

# Shapiro-Wilk Test für jede Kombination aus Event und Altersgruppe
results <- data %>%
  group_by(event, Alter) %>%
  summarise(
    Sample_Size = n(),
    W_Statistic = ifelse(Sample_Size > 3, shapiro.test(Zeit)$statistic, NA),
    p_Value = ifelse(Sample_Size > 3, shapiro.test(Zeit)$p.value, NA)
  ) %>%
  filter(Sample_Size > 3)

# Ergebnisse anzeigen
print(results)

# Ergebnisse als CSV speichern
write.csv(results, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/shapiro_test_results.csv", row.names = FALSE)
