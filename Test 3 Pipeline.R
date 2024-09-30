# Liste der benötigten Pakete
pakete <- c("dplyr", "tidyr", "ggplot2", "lubridate", "readr")

# Installiere fehlende Pakete
neue_pakete <- pakete[!(pakete %in% installed.packages()[,"Package"])]
if(length(neue_pakete)) install.packages(neue_pakete)

# Lade die Pakete
lapply(pakete, require, character.only = TRUE)



# Format mm:ss,00 in Sek --------------------------------------------------

# Funktion zum Umwandeln von Zeitformat mm:ss,0 oder mm:ss,00 in Sekunden (mit Komma und zwei Dezimalstellen)
convert_to_seconds <- function(time_str) {
  # Ersetzen des Kommas durch einen Punkt, um Dezimalzahlen in R zu unterstützen
  time_str <- gsub(",", ".", time_str)
  # Aufteilen der Zeit in Minuten und Sekunden
  time_parts <- strsplit(time_str, ":")[[1]]
  minutes <- as.numeric(time_parts[1])
  seconds <- as.numeric(time_parts[2])
  # Umwandeln in reine Sekunden
  total_seconds <- minutes * 60 + seconds
  # Formatierung mit zwei Dezimalstellen und Komma als Dezimaltrennzeichen
  formatted_seconds <- formatC(total_seconds, format = "f", digits = 2, decimal.mark = ',')
  return(formatted_seconds)
}

# Schleife ----------------------------------------------------------------

# Basispfad
basis_pfad <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/200_L_m"

# Leere Liste zum Speichern der Daten
gesamtdaten <- list()

# Daten sammeln?
antwort <- readline(prompt = "Möchten Sie Daten sammeln? (j/n): ")

# Liste zum Speichern der Daten für 100F_m erstellen
total <- list()

while (antwort == "j") {
  # Alter und Geschlecht eingeben
  alter <- as.integer(readline(prompt = "Bitte geben Sie das Alter ein (11 bis 19): "))
  geschlecht <- readline(prompt = "Bitte geben Sie das Geschlecht ein (m oder w): ")
  
  # Ordnerpfad zusammensetzen
  ordner_pfad <- file.path(basis_pfad, paste0(alter, "_", geschlecht))
  
  # Überprüfen, ob der Ordner existiert
  if (file.exists(ordner_pfad)) {
    # CSV-Dateien importieren und bereinigen
    csv_dateien <- list.files(path = ordner_pfad, pattern = "*.csv", full.names = TRUE)
    daten <- lapply(csv_dateien, function(file) {
      data <- read.csv(file, sep = ";", fileEncoding = "UTF-8")
     
       # Entfernen der Spalten "Verein" und Spalte 7 und Platz
      data <- data %>% select(-Verein, -7, -1)
      
      # Monatsabkürzungen in Zahlen umwandeln
      monate_dict <- c("Jan" = 1, "Feb" = 2, "Mär" = 3, "Apr" = 4, "Mai" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9, "Okt" = 10, "Nov" = 11, "Dez" = 12)
      data$Monat <- sapply(strsplit(as.character(data$Datum), " "), function(x) monate_dict[x[1]])
      data$Jahr <- sapply(strsplit(as.character(data$Datum), " "), function(x) as.integer(paste0("20", x[2])))
     
       # Spalte "Datum" entfernen
      data <- data %>% select(-Datum)
      
      # Hinzufügen der Spalte "Alter" und Überprüfung von "Alter" + "Jg"
      data$Alter <- alter
      data <- data %>% filter(Alter + Jg == Jahr)
      
      # Umwandeln der Zeitwerte in Sekunden
      data$Zeit <- sapply(data$Zeit, convert_to_seconds)
      
      # Erstellen der Rangspalte basierend auf der "Zeit" für jedes Jahr
      data$Rang <- ave(data$Zeit, data$Jahr, FUN = function(x) rank(x, ties.method = "average"))
      
  
      
      return(data)
      
    })
    
    # Daten zusammenfügen und Gesamtrang berechnen
    daten_zusammengeführt <- do.call(rbind, daten)
    daten_zusammengeführt$Gesamtrang <- rank(daten_zusammengeführt$Zeit, ties.method = "average")
    total[[paste0(alter, "_", geschlecht)]] <- daten_zusammengeführt
    
    #Alter_Geschlecht Datei speichern
    speicher_pfad <- file.path(basis_pfad, paste0(alter, "_", geschlecht, ".csv"))
    write.csv(daten_zusammengeführt, speicher_pfad, row.names = FALSE)
    
  } else {
    cat("Es konnte kein entsprechender Ordner gefunden werden\n")
  }
  
  
  # Erneut fragen, ob Daten gesammelt werden sollen
  antwort <- readline(prompt = "Möchten Sie Daten sammeln? (j/n): ")
}


# Gesamtdatei speichern
final_data <- do.call(rbind, total)
speicher_pfad_total <- file.path(basis_pfad, "total.csv")
write.csv(final_data, speicher_pfad_total, row.names = FALSE)


cat("Datenpipeline geschlossen.\n")




