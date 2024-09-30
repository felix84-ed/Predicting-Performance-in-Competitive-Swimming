# Installiere und lade die notwendigen Pakete
install.packages("dplyr")
library(dplyr)

# Lade die Daten aus der CSV-Datei
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/updated_gesamt_daten.csv"
data <- read.csv(file_path)

# Berechne die Ztime-Scores
data <- data %>%
  group_by(Geschlecht, Disziplin, Streckenlänge) %>%
  mutate(mean_time = mean(Zeit, na.rm = TRUE),
         sd_time = sd(Zeit, na.rm = TRUE),
         Ztime = (Zeit - mean_time) / sd_time) %>%
  ungroup() %>%
  select(-mean_time, -sd_time)

# Pfad für die neue Datei festlegen
output_file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final.csv"

# Speichere die aktualisierten Daten in einer neuen CSV-Datei
write.csv(data, output_file_path, row.names = FALSE)

# Ausgabe: Bestätigungsnachricht
cat("Die Datei wurde erfolgreich erstellt und gespeichert: ", output_file_path)

