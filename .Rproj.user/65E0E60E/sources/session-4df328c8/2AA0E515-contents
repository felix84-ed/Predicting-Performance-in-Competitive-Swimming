# Bibliotheken laden
library(dplyr)
library(ggplot2)
library(tidyr)  # Für die spread-Funktion

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

# Funktion zur Generierung der Kurvendaten für jede Disziplin und Streckenlänge
generate_curve_data <- function(df, streckenlänge, disziplin) {
  # Filterung der Daten für die spezifische Disziplin und Streckenlänge
  disziplin_df <- df %>%
    filter(Streckenlänge == streckenlänge, Disziplin == disziplin, !is.na(percent_improvement))
  
  if (nrow(disziplin_df) > 0) {
    # Vorbereitung der Daten für die quadratische Regression
    ages <- disziplin_df$Alter
    improvements <- disziplin_df$percent_improvement
    
    if (length(unique(ages)) > 2) {
      # Quadratische Regression
      model <- lm(improvements ~ poly(ages, 2, raw = TRUE))
      
      # Vorhersage der Verbesserungen basierend auf dem Modell
      ages_range <- seq(11, 18, by = 1)
      improvements_pred <- predict(model, newdata = data.frame(ages = ages_range))
      
      # Erstellen eines DataFrames für die Vorhersagedaten
      pred_df <- data.frame(Alter = ages_range, percent_improvement = improvements_pred, 
                            Streckenlänge = as.factor(streckenlänge), Disziplin = disziplin)
      
      return(pred_df)
    }
  }
  return(NULL)
}

# Einzigartige Kombinationen von Streckenlänge und Disziplin
unique_combinations <- df %>%
  select(Streckenlänge, Disziplin) %>%
  distinct()

# Generieren der Kurvendaten für jede Disziplin und Streckenlänge
curve_data_list <- lapply(1:nrow(unique_combinations), function(i) {
  generate_curve_data(df, unique_combinations$Streckenlänge[i], unique_combinations$Disziplin[i])
})

# Zusammenführen aller Kurvendaten in einen DataFrame
curve_data <- do.call(rbind, curve_data_list)

# Festlegen der Farben für die Disziplinen
disziplin_colors <- c("blue", "green", "red", "purple", "orange")

# Festlegen der Linienstile für die Streckenlängen
line_types <- c("50" = "solid", "100" = "dotted", "200" = "dashed", "400" = "dotdash", "800" = "longdash", "1500" = "twodash")

# Visualisierung der quadratischen Kurven für alle Disziplinen und Streckenlängen
ggplot(curve_data, aes(x = Alter, y = percent_improvement, color = Disziplin, linetype = Streckenlänge)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = disziplin_colors) +
  scale_linetype_manual(values = line_types) +
  scale_y_continuous(breaks = seq(floor(min(curve_data$percent_improvement, na.rm = TRUE) / 5) * 5, 
                                  ceiling(max(curve_data$percent_improvement, na.rm = TRUE) / 5) * 5, by = 5)) +
  labs(x = 'Alter', y = 'Prozentuale Verbesserung', 
       title = 'Quadratische Kurven der prozentualen Verbesserung für verschiedene Disziplinen',
       color = 'Disziplin', linetype = 'Streckenlänge') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray') +
  theme(legend.position = "right")

# Erstellen einer Tabelle mit Altersgruppen als Zeilen und Disziplin/Streckenlänge-Kombinationen als Spalten
curve_data <- curve_data %>%
  unite("Strecken_Disziplin", Streckenlänge, Disziplin, sep = "_") %>%
  spread(key = Strecken_Disziplin, value = percent_improvement)

# Anzeigen der Tabelle
print(curve_data)

# Speichern der Tabelle als CSV-Datei
output_file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/curve_data.csv"
write.csv(curve_data, output_file_path, row.names = FALSE)


# Vergleich Individuelle und allg. Verbesserungsrate ----------------------
# Schritt 1: Berechnen der theoretischen Verbesserungsraten
# Bereits durch die Funktion generate_curve_data gemacht und in curve_data gespeichert

# Schritt 2: Vergleich mit den individuellen Verbesserungsraten
df <- calculate_percent_improvement(df)  # Ihre Funktion zur Berechnung der individuellen Verbesserungsraten

# Verbinden der theoretischen Verbesserungsraten aus curve_data mit den individuellen Daten
df <- df %>%
  left_join(curve_data, by = c("Alter", "Streckenlänge", "Disziplin"))

# Berechnung der Differenz zwischen theoretischer und tatsächlicher Verbesserung
df <- df %>%
  mutate(improvement_diff = percent_improvement - percent_improvement.y)

# Schritt 3: Integration in den ursprünglichen Datensatz
# Speichern des erweiterten Datensatzes
output_file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"
write.csv(df, output_file_path, row.names = FALSE)


