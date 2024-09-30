library(dplyr)

# Funktion zum Umwandeln von Zeitformat mm:ss,0 oder mm:ss,00 in Sekunden
convert_to_seconds <- function(time_str) {
  time_str <- gsub(",", ".", time_str)
  time_parts <- strsplit(time_str, ":")[[1]]
  minutes <- as.numeric(time_parts[1])
  seconds <- as.numeric(time_parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

# Basispfad
basis_pfad <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data"

# Daten sammeln?
antwort <- readline(prompt = "Möchten Sie Daten sammeln? (j/n): ")

gesamtdaten_liste <- list()  # Liste zum Speichern der Daten für jeden Hauptordner

while (antwort == "j") {
  # A) Benutzer nach Streckenlänge, Schwimmlage und Geschlecht fragen
  streckenlänge <- readline(prompt = "Bitte geben Sie die Streckenlänge ein (z.B. 100): ")
  schwimmlage <- readline(prompt = "Bitte geben Sie die Schwimmlage ein (S, R, B, F oder L): ")
  geschlecht <- readline(prompt = "Bitte geben Sie das Geschlecht ein (m oder w): ")
  
  # Ordnerpfad zusammensetzen
  ordner_pfad <- file.path(basis_pfad, paste0(streckenlänge, "_", schwimmlage, "_", geschlecht))
  
  # Überprüfen, ob der Hauptordner existiert
  if (file.exists(ordner_pfad)) {
    alter_ordner <- list.dirs(ordner_pfad, full.names = TRUE, recursive = FALSE)
    
    daten_liste <- list()  # Liste zum Speichern der Daten für jeden Altersordner
    
    for (alter_pfad in alter_ordner) {
      alter <- as.integer(strsplit(basename(alter_pfad), "_")[[1]][1])
      
      # CSV-Dateien importieren und bereinigen
      csv_dateien <- list.files(path = alter_pfad, pattern = "*.csv", full.names = TRUE)
      daten <- lapply(csv_dateien, function(file) {
        data <- read.csv(file, sep = ";", fileEncoding = "UTF-8")
        
        # Monatsabkürzungen in Zahlen umwandeln und Alter hinzufügen
        monate_dict <- c("Jan" = 1, "Feb" = 2, "Mär" = 3, "Apr" = 4, "Mai" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9, "Okt" = 10, "Nov" = 11, "Dez" = 12)
        data$Monat <- sapply(strsplit(as.character(data$Datum), " "), function(x) monate_dict[x[1]])
        data$Jahr <- sapply(strsplit(as.character(data$Datum), " "), function(x) as.integer(paste0("20", x[2])))
        data$Alter <- alter
        data <- data %>% select(-Datum) %>% filter(Alter + Jg == Jahr)
        
        # Umwandeln der Zeitwerte in Sekunden und Rangspalte hinzufügen
        data$Zeit <- sapply(data$Zeit, convert_to_seconds)
        data$Rang <- ave(data$Zeit, data$Jahr, FUN = function(x) rank(x, ties.method = "average"))
        
        return(data)
      })
      
      # Daten in der Liste für Altersordner speichern
      daten_liste[[paste0(alter, "_", geschlecht)]] <- do.call(rbind, daten)
    }
    
    # Daten für alle Altersordner zusammenführen und in der Gesamtliste speichern
    daten_gesamt <- do.call(rbind, daten_liste)
    
    # Gesamtrang für die Schwimmleistung pro Disziplin/Streckenlänge/Geschlecht und pro Alter berechnen
    daten_gesamt <- daten_gesamt %>%
      group_by(Alter) %>%
      mutate(Gesamtrang = rank(Zeit, ties.method = "average"))
    
    #Neue Spalten hinzufügen
    daten_gesamt$Geschlecht <- geschlecht
    daten_gesamt$Streckenlänge <- streckenlänge
    daten_gesamt$Disziplin <- schwimmlage
    
    gesamtdaten_liste[[paste0(streckenlänge, "_", schwimmlage, "_", geschlecht)]] <- daten_gesamt
    
  } else {
    cat("Es konnte kein entsprechender Hauptordner gefunden werden\n")
  }
  
  # Erneut fragen, ob Daten gesammelt werden sollen
  antwort <- readline(prompt = "Möchten Sie Daten sammeln? (j/n): ")
}

# Endgültige Datenbereinigung und Speichern
final_data <- do.call(rbind, gesamtdaten_liste)
final_data <- final_data %>% select(-c(Verein, 7))
speicher_pfad_total <- file.path(basis_pfad, "gesamt_daten.csv")
write.csv(final_data, speicher_pfad_total, row.names = FALSE)

cat("Datenpipeline geschlossen.\n")
