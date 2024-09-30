# Install and load necessary packages
install.packages("dplyr")
library(dplyr)

# Read the CSV file
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/gesamt_daten_bereinigt.csv")


# frühestes Einstiegalter Top100 ------------------------------------------

# Berechne das früheste Einstiegsalter für jeden Sportler und füge das dem Datensatz hinzu.
data <- data %>%
  group_by(name, Jg) %>%
  mutate(Frühestes_Alter = min(Alter)) %>%
  ungroup()


# Schwimmstile pro Sportler pro Alter -------------------------------------

# Zähle die verschiedenen Lagen pro Sportler in jedem Alter
data <- data %>%
  group_by(name, Jg, Alter) %>%
  mutate(Anzahl_Disziplinen = n_distinct(Disziplin)) %>%
  ungroup()

# Distanzkategorien -------------------------------------------------------

# Definiere eine Funktion um Streckenlänge zu kategorisieren
categorize_distance <- function(distance) {
  if (distance == 50) {
    return('Sprint')
  } else if (distance %in% c(100, 200)) {
    return('Mittel')
  } else if (distance %in% c(400, 800, 1500)) {
    return('Lang')
  } else {
    return('Other')
  }
}

# Funktion zum Datensatz beifügen
data$Distanz_Kategorie <- sapply(data$Streckenlänge, categorize_distance)

# Gruppieren by name, Jg, and Alter and zähle Distanzkategorie jeweils
data <- data %>%
  group_by(name, Jg, Alter) %>%
  mutate(Anzahl_Distanz_Kategorien = n_distinct(Distanz_Kategorie)) %>%
  ungroup()

# Temporäre Spalte entfernen
#data$Distanz_Kategorie <- NULL

# Häufigkeit Jahre in Top100 ----------------------------------------------

# Anzahl der Altersgruppen für jeden Schwimmer berechnen, in denen er in den Top100 erscheint
data <- data %>%
  group_by(name, Jg) %>%
  mutate(Anzahl_Einzigartige_Altersgruppen = n_distinct(Alter)) %>%
  ungroup()

# Abspeichern:
# Pfad, wo die CSV-Datei gespeichert werden soll
output_file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/updated_gesamt_daten.csv"

# Speichern des Datenrahmens als CSV
#write.csv(data, file = output_file_path, row.names = FALSE)


# Median Top 10 -----------------------------------------------------------


# Read the CSV file
median_top10 <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Daten_weltspitze/median_values_reshaped.csv")

# Zuweisen der neuen Spaltennamen
colnames(median_top10) <- c("Zeitraum","50 B", "50 S", "50 F", "50 R",
                            "100 B", "100 S", "100 F", "100 R",
                            "200 B", "200 S", "200 F", "200 R",
                            "200 L", "400 F", "400 L", "800 F", "1500 F")

# Ersetzen "median_Top10" durch den tatsächlichen Namen Ihres DataFrames
median_top10 <- median_top10 %>%
  mutate(Zeitraum = case_when(
    Zeitraum == "Zeitraum 1 (Median)" ~ "2000",
    Zeitraum == "Zeitraum 2 (Median)" ~ "2004",
    Zeitraum == "Zeitraum 3 (Median)" ~ "2008",
    Zeitraum == "Zeitraum 4 (Median)" ~ "2012",
    Zeitraum == "Zeitraum 5 (Median)" ~ "2016",
    Zeitraum == "Zeitraum 6 (Median)" ~ "2021",
    TRUE ~ Zeitraum # behält den aktuellen Wert bei, falls keine Übereinstimmung gefunden wurde
  ))

#head(median_top10)

# Zeitraum zuordnen
data <- data %>%
  mutate(Zeitraum = case_when(
    Jahr >= 2000 & Jahr < 2004 ~ "2000",
    Jahr >= 2004 & Jahr < 2008 ~ "2004",
    Jahr >= 2008 & Jahr < 2012 ~ "2008",
    Jahr >= 2012 & Jahr < 2016 ~ "2012",
    Jahr >= 2016 & Jahr < 2020 ~ "2016",
    Jahr >= 2020 ~ "2021"
  ))

# Kombinationsspalte erstellen
data <- data %>%
  mutate(Streckenlänge_Disziplin = paste(Streckenlänge, Disziplin))

# Verknüpfen der Tabellen
data <- data %>%
  left_join(median_top10, by = c("Zeitraum" = "Zeitraum"))


# Medianwertspalte auswählen
data <- data %>%
  rowwise() %>%
  mutate(Medianwert = get(Streckenlänge_Disziplin))

# Relativen Wert berechnen
data <- data %>%
  mutate(Relativer_Wert = Medianwert/ Zeit)

# Auswahl relevanter Spalten für die finale Tabelle
# Laden des dplyr-Pakets
library(dplyr)

# Entfernen der Spalten aus dem DataFrame 'data'
data <- data %>%
  select(
    -Zeitraum, 
    -Streckenlänge_Disziplin, 
    -`50 B`, -`50 S`, -`50 F`, -`50 R`, 
    -`100 B`, -`100 S`, -`100 F`, -`100 R`, 
    -`200 B`, -`200 S`, -`200 F`, -`200 R`, -`200 L`, 
    -`400 F`, -`400 L`, 
    -`800 F`, 
    -`1500 F`, 
    -Medianwert
  )

#write.csv(data, file = output_file_path, row.names = FALSE)
