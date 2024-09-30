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
  select(-c(Monat, Jahr, Rang, Gesamtrang, Geschlecht, baseline_time_11y, baseline_time_12y,
            baseline_time_13y, baseline_time_14y, 
            percent_individual_improvement_11y, percent_general_improvement_11y,
            percent_diff_improvement_11y, 
            percent_individual_improvement_12y, percent_general_improvement_12y,
            percent_diff_improvement_12y,
            percent_individual_improvement_13y, percent_general_improvement_13y,
            percent_diff_improvement_13y,
            percent_individual_improvement_14y, percent_general_improvement_14y,
            percent_diff_improvement_14y,
            name, Jg, Streckenlänge, Disziplin,
            years_in_top100_cumulative, years_in_top100_cumulative_discipline, drei_jaehrige_verbesserung_prozent, Distanz_Kategorie))

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


# Z-Time ratio ------------------------------------------------------------
# Berechnung des Verhältnisses der Z-Times 12-16y ---------------------------------
data_z_ratio <- data %>%
  filter(Alter == 12 | Alter == 16) %>%  # Filtern der Altersstufen 12 und 16 Jahre
  group_by(unique_id, Event) %>%
  summarize(z_time_12 = first(Z_time[Alter == 12]),  # Z-Time im Alter von 12 Jahren
            z_time_16 = first(Z_time[Alter == 16])) %>%  # Z-Time im Alter von 16 Jahren
  mutate(z_time_ratio = z_time_16 / z_time_12)  # Berechnung des Verhältnisses

# Zusammenführen des berechneten Z-Time-Verhältnisses mit dem ursprünglichen Datensatz
data <- left_join(data, data_z_ratio %>% select(unique_id, Event, z_time_ratio), by = c("unique_id", "Event"))

# Optional: Überprüfen der Ergebnisse
print(head(data))


# Max_yearly_improvement --------------------------------------------------
# Berechnung der maximalen jährlichen Leistungsverbesserung (Max Prog)
data_max_prog <- data %>%
  filter(Alter >= 12 & Alter <= 16) %>%
  arrange(unique_id, Event, Alter) %>%
  group_by(unique_id, Event) %>%
  summarize(max_yearly_improvement = ifelse(all(is.na(jaehrliche_verbesserung_prozent)), 
                                            NA, 
                                            max(jaehrliche_verbesserung_prozent, na.rm = TRUE))) %>%
  ungroup()

# Zusammenführen der berechneten maximalen jährlichen Verbesserungsrate mit dem ursprünglichen Datensatz
data <- left_join(data, data_max_prog %>% select(unique_id, Event, max_yearly_improvement), by = c("unique_id", "Event"))

# Optional: Überprüfen der Ergebnisse
print(head(data))


# years_top100 ------------------------------------------------------------
# Berechnung der Anzahl der Jahre in den Top 100 in dieser Disziplin (12 bis 16 Jahre)
data_years_top100 <- data %>%
  filter(Alter >= 12 & Alter <= 16) %>%  # Filtern der relevanten Altersstufen
  group_by(unique_id, Event) %>%
  summarize(years_in_top100 = n_distinct(Alter)) %>%  # Zählen der unterschiedlichen Altersstufen (Jahre)
  ungroup()

# Zusammenführen der berechneten Anzahl der Jahre in den Top 100 mit dem ursprünglichen Datensatz
data <- left_join(data, data_years_top100 %>% select(unique_id, Event, years_in_top100), by = c("unique_id", "Event"))

# Optional: Überprüfen der Ergebnisse
print(head(data))

# Max_Dist_Kat ------------------------------------------------------------
# Berechnung der maximalen Anzahl der verschiedenen Distanzkategorien im Bereich 12-16 Jahre
data_max_dist_categories <- data %>%
  filter(Alter >= 12 & Alter <= 16) %>%  # Filtern der relevanten Altersstufen
  group_by(unique_id) %>%
  summarize(max_dist_categories = max(X.Nr_Dist_Kat.Jahr., na.rm = TRUE)) %>%  # Bestimmung des maximalen Werts
  ungroup()

# Zusammenführen der berechneten maximalen Distanzkategorien mit dem ursprünglichen Datensatz
data <- left_join(data, data_max_dist_categories %>% select(unique_id, max_dist_categories), by = "unique_id")

# Optional: Überprüfen der Ergebnisse
print(head(data))

# Max_Disc ----------------------------------------------------------------
# Berechnung der maximalen Anzahl der Disziplinen im Bereich 12-16 Jahre
data_max_disciplines <- data %>%
  filter(Alter >= 12 & Alter <= 16) %>%
  group_by(unique_id) %>%
  summarize(max_disciplines = max(X.Nr_Disziplinen.Jahr., na.rm = TRUE)) %>%  # Bestimmung des maximalen Werts
  ungroup()

# Zusammenführen der berechneten maximalen Anzahl der Disziplinen mit dem ursprünglichen Datensatz
data <- left_join(data, data_max_disciplines %>% select(unique_id, max_disciplines), by = "unique_id")

# Optional: Überprüfen der Ergebnisse
print(head(data))


# Löschen -----------------------------------------------------------------
# Entfernen der Spalten, die zur Berechnung verwendet wurden
data <- data %>%
  select(-c(X.Nr_Dist_Kat.Jahr., X.Nr_Disziplinen.Jahr.))

# Optional: Überprüfen der verbleibenden Spalten
print(colnames(data))


# Fina16y -----------------------------------------------------------------

# Extrahieren der FINA-Punkte im Alter von 16 Jahren
data_fina_16 <- data %>%
  filter(Alter == 16) %>%  # Filtern auf das Alter von 16 Jahren
  group_by(unique_id, Event) %>%
  summarize(fina_16 = first(Fina)) %>%  # Extrahieren der FINA-Punkte
  ungroup()

# Zusammenführen der FINA-Punkte mit dem ursprünglichen Datensatz
data <- left_join(data, data_fina_16 %>% select(unique_id, Event, fina_16), by = c("unique_id", "Event"))

# Optional: Überprüfen der Ergebnisse
print(head(data))

# Speichern des bearbeiteten Datensatzes als neue CSV-Datei
write.csv(data, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset_processed.csv", row.names = FALSE)

# Optional: Überprüfen, ob die Datei erfolgreich gespeichert wurde
print("Datei erfolgreich gespeichert.")




