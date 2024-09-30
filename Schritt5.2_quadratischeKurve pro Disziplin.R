# Bibliotheken laden
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)

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

# Festlegen der Farben für die Disziplinen in Englisch
disziplin_colors <- c("blue", "green", "red", "purple", "orange")

# Festlegen der Linienstile für die Streckenlängen
line_types <- c("50" = "solid", "100" = "dotted", "200" = "dashed", "400" = "dotdash", "800" = "longdash", "1500" = "twodash")

# Visualisierung der quadratischen Kurven für alle Disziplinen und Streckenlängen mit schmaleren Linien
ggplot(curve_data, aes(x = Alter, y = percent_improvement, color = Disziplin, linetype = Streckenlänge)) +
  geom_line(linewidth = 0.5) +  # Hier können Sie den Wert für schmalere oder dickere Linien anpassen
  scale_color_manual(values = disziplin_colors) +
  scale_linetype_manual(values = line_types) +
  scale_y_continuous(breaks = seq(floor(min(curve_data$percent_improvement, na.rm = TRUE) / 5) * 5, 
                                  ceiling(max(curve_data$percent_improvement, na.rm = TRUE) / 5) * 5, by = 5)) +
  labs(x = 'Age (years)', y = '% improvement in performance time from baseline', 
       color = 'stroke', linetype = 'Length') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray') +
  theme(legend.position = "right")

# Erstellen einer Tabelle mit Altersgruppen als Zeilen und Disziplin/Streckenlänge-Kombinationen als Spalten
table_data <- curve_data %>%
  pivot_wider(names_from = interaction(Streckenlänge, Disziplin), values_from = percent_improvement)

# Anzeigen der Tabelle
print(table_data)
