# Bibliotheken laden
library(dplyr)
library(readr)


# Datei laden
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final_with_years_in_top100_updated.csv"
df <- read.csv(file_path)

# Daten nach name, Jg, Streckenlänge, Disziplin und Alter sortieren
df <- df %>%
  arrange(name, Jg, Streckenlänge, Disziplin, Alter)

# Berechnung der prozentualen Verbesserung basierend auf dem Alter 11
calculate_percent_improvement <- function(df) {
  df <- df %>%
    group_by(name, Jg, Streckenlänge, Disziplin) %>%
    mutate(baseline_time = ifelse(11 %in% Alter, Zeit[Alter == 11], NA)) %>%
    ungroup() %>%
    mutate(percent_improvement = ifelse(!is.na(baseline_time) & baseline_time != 0, 
                                        ((baseline_time - Zeit) / baseline_time) * 100, 
                                        NA))
  return(df)
}

# Berechnung der prozentualen Verbesserung
df <- calculate_percent_improvement(df)

# Funktion zur Berechnung der Deskriptoren für jede Disziplin und Streckenlänge
calculate_descriptors <- function(df, streckenlänge, disziplin) {
  # Filterung der Daten für die spezifische Disziplin und Streckenlänge
  disziplin_df <- df %>%
    filter(Streckenlänge == streckenlänge, Disziplin == disziplin, !is.na(percent_improvement))
  
  if (nrow(disziplin_df) > 0) {
    # Prozentuale Verbesserung von 11 Jahren bis zum Höchstleistungsalter
    peak_age_df <- disziplin_df %>%
      filter(percent_improvement == max(percent_improvement, na.rm = TRUE))
    
    peak_age <- peak_age_df$Alter[1]
    improvement_to_peak <- peak_age_df$percent_improvement[1]
    
    # Prozentuale Verbesserung von 11 Jahren bis 18 Jahren
    improvement_to_18 <- disziplin_df %>%
      filter(Alter == 18) %>%
      select(percent_improvement) %>%
      unlist() %>%
      .[1]
    
    # Leistungszeit im Schwellenalter
    performance_at_peak <- peak_age_df$Zeit[1]
    
    return(data.frame(
      Streckenlänge = streckenlänge,
      Disziplin = disziplin,
      Improvement_to_peak = improvement_to_peak,
      Improvement_to_18 = improvement_to_18,
      Peak_age = peak_age,
      Performance_at_peak = performance_at_peak
    ))
  }
  return(NULL)
}

# Einzigartige Kombinationen von Streckenlänge und Disziplin
unique_combinations <- df %>%
  select(Streckenlänge, Disziplin) %>%
  distinct()

# Berechnen der Deskriptoren für jede Disziplin und Streckenlänge
descriptors_list <- lapply(1:nrow(unique_combinations), function(i) {
  calculate_descriptors(df, unique_combinations$Streckenlänge[i], unique_combinations$Disziplin[i])
})

# Zusammenführen aller Deskriptoren in einen DataFrame
descriptors <- do.call(rbind, descriptors_list)

# Anzeige der Deskriptoren
print(descriptors)


# Export der Deskriptoren als CSV
write_csv(descriptors, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/descriptors.csv")