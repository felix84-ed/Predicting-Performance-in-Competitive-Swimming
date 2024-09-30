# Installieren des Pakets dplyr, falls es noch nicht installiert ist
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

# Laden des Datensatzes
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv")

# Entfernen der nicht benötigten Spalten
data <- data %>%
  select(-c(Monat, Jahr, Rang, Gesamtrang, Geschlecht, baseline_time_11y,
            percent_individual_improvement_11y, percent_general_improvement_11y,
            percent_diff_improvement_11y, name, Jg, Streckenlänge, Disziplin,
            years_in_top100_cumulative, years_in_top100_cumulative_discipline, Distanz_Kategorie))

# Optional: Überprüfen der verbleibenden Spalten
print(colnames(data))

# Berechnung der Verbesserungsrate 12-16y ---------------------------------
# Berechnung der kumulativen jährlichen Verbesserungsrate von 12 bis 16 Jahren
data_improvement <- data %>%
  filter(Alter >= 12 & Alter <= 16) %>%
  group_by(unique_id, Event) %>%
  mutate(improvement_rate = ifelse(
    all(!is.na(Zeit[Alter == 12]) & !is.na(Zeit[Alter == 16])),
    (Zeit[Alter == 12] - Zeit[Alter == 16]) / Zeit[Alter == 12] * 100,
    NA
  )) %>%
  distinct(unique_id, Event, .keep_all = TRUE)  # Sicherstellen, dass jede Kombination aus unique_id und Event einzigartig ist

# Zusammenführen der berechneten Verbesserungsrate mit dem ursprünglichen Datensatz
data <- left_join(data, data_improvement %>% select(unique_id, Event, improvement_rate), by = c("unique_id", "Event"))

# Optional: Überprüfen der Ergebnisse
print(head(data))

# Max. Z-Time Rate --------------------------------------------------------
# Berechnung der maximalen Z-Time-Rate von Jahr zu Jahr für den Altersbereich 12 bis 16 Jahre
data_z_time <- data %>%
  filter(Alter >= 12 & Alter <= 16) %>%
  arrange(unique_id, Event, Alter) %>%
  group_by(unique_id, Event) %>%
  mutate(z_time_rate = (Z_time - lag(Z_time)) / abs(lag(Z_time)) * 100) %>%
  summarize(max_z_time_rate = ifelse(all(is.na(z_time_rate)), NA, max(z_time_rate, na.rm = TRUE))) %>%
  ungroup()

# Zusammenführen der berechneten Z-Time-Rate mit dem ursprünglichen Datensatz
data <- left_join(data, data_z_time, by = c("unique_id", "Event"))

# Optional: Überprüfen der Ergebnisse
print(head(data))
