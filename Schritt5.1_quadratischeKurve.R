# Bibliotheken laden
library(dplyr)

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

# Filterung der Daten, um nur valide Verbesserungen zu behalten
filtered_df <- df %>%
  filter(!is.na(percent_improvement))

head(df)

# Vorbereitung der Daten für die quadratische Regression
ages <- filtered_df$Alter
improvements <- filtered_df$percent_improvement

# Quadratische Regression
model <- lm(improvements ~ poly(ages, 2, raw = TRUE))

# Vorhersage der Verbesserungen basierend auf dem Modell
ages_range <- seq(11, 18, by = 1)
improvements_pred <- predict(model, newdata = data.frame(ages = ages_range))

# Erstellen eines DataFrames für die Vorhersagedaten
pred_df <- data.frame(Alter = ages_range, percent_improvement = improvements_pred)


# Bibliothek ggplot2 laden
library(ggplot2)

# Visualisierung der Daten und der quadratischen Kurve
ggplot(filtered_df, aes(x = Alter, y = percent_improvement)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_line(data = pred_df, aes(x = Alter, y = percent_improvement), color = 'red') +
  scale_y_continuous(breaks = seq(floor(min(filtered_df$percent_improvement, na.rm = TRUE) / 5) * 5, 
                                  ceiling(max(filtered_df$percent_improvement, na.rm = TRUE) / 5) * 5, by = 5)) +
  labs(x = 'Alter', y = 'Prozentuale Verbesserung', title = 'Quadratische Kurve der prozentualen Verbesserung basierend auf dem Alter 11') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'gray')

# Speichern der Ergebnisse
output_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final_with_yearly_improvement_quadratic_curve.csv"
write.csv(df, output_path, row.names = FALSE)

# Anzeige der ersten Zeilen des aktualisierten Datensatzes
head(df)
