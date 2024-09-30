# Bibliotheken laden
library(dplyr)

# Datei laden
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final_with_years_in_top100.csv"
df <- read.csv(file_path)

# Daten nach Name, Jahrgang, Streckenlänge, Disziplin und Alter sortieren
df <- df %>%
  arrange(name, Jg, Streckenlänge, Disziplin, Alter)

# Funktion zur Berechnung der jährlichen und 3-jährigen Verbesserung
calculate_improvements <- function(df) {
  df$jaehrliche_verbesserung_prozent <- NA  # Neue Spalte für jährliche Verbesserung
  df$drei_jaehrige_verbesserung_prozent <- NA  # Neue Spalte für 3-jährige Verbesserung
  
  for (i in 2:nrow(df)) {
    current_row <- df[i, ]
    previous_row <- df[i - 1, ]
    
    # Berechnung der jährlichen Verbesserung
    if (current_row$name == previous_row$name &&
        current_row$Jg == previous_row$Jg &&
        current_row$Streckenlänge == previous_row$Streckenlänge &&
        current_row$Disziplin == previous_row$Disziplin &&
        current_row$Alter == previous_row$Alter + 1) {
      
      current_time <- current_row$Zeit
      previous_time <- previous_row$Zeit
      
      if (!is.na(previous_time) && previous_time != 0) {
        improvement <- ((previous_time - current_time) / previous_time) * 100
        df$jaehrliche_verbesserung_prozent[i] <- improvement
      } else {
        df$jaehrliche_verbesserung_prozent[i] <- NA
      }
    }
    
    # Berechnung der 3-jährigen Verbesserung
    if (i > 3) {  # Sicherstellen, dass es genügend vorherige Datenpunkte gibt
      three_years_ago_row <- df[i - 3, ]
      
      if (current_row$name == three_years_ago_row$name &&
          current_row$Jg == three_years_ago_row$Jg &&
          current_row$Streckenlänge == three_years_ago_row$Streckenlänge &&
          current_row$Disziplin == three_years_ago_row$Disziplin &&
          current_row$Alter == three_years_ago_row$Alter + 3) {
        
        three_years_ago_time <- three_years_ago_row$Zeit
        
        if (!is.na(three_years_ago_time) && three_years_ago_time != 0) {
          improvement <- ((three_years_ago_time - current_time) / three_years_ago_time) * 100
          df$drei_jaehrige_verbesserung_prozent[i] <- improvement
        } else {
          df$drei_jaehrige_verbesserung_prozent[i] <- NA
        }
      }
    }
  }
  return(df)
}

# Berechnung der Verbesserungen
df <- calculate_improvements(df)

# Speichern der aktualisierten Datei
output_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final_with_years_in_top100_updated.csv"
write.csv(df, output_path, row.names = FALSE)

# Anzeige der ersten Zeilen des aktualisierten Datensatzes
head(df)
