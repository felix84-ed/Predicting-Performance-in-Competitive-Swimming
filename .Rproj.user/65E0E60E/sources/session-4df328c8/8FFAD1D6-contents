# Laden der erforderlichen Pakete
library(tidyverse)

# Funktion zur Berechnung der Pearson-Korrelation und Rückgabe der Daten
compute_correlation_data <- function(streckenlaenge, disziplinen) {
  # Filtern der Daten für die spezifische Streckenlänge und Disziplinen
  data_filtered <- swim_data %>%
    filter(Streckenlänge == streckenlaenge & Disziplin %in% disziplinen)
  
  # Initialisieren eines leeren DataFrames, um die Korrelationsergebnisse zu speichern
  corr_results <- data.frame()
  
  # Schleife durch jede Disziplin und Berechnung der Pearson-Korrelationen
  for (disziplin in disziplinen) {
    # Filtern der Daten für die spezifische Disziplin
    discipline_data <- data_filtered %>%
      filter(Disziplin == disziplin) %>%
      # Gruppieren nach Name, Jg und Alter und Durchschnitt der Zeiten berechnen
      group_by(name, Jg, Alter) %>%
      summarize(Zeit = mean(as.numeric(Zeit), na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    # Pivot-Tabelle erstellen, wobei die Zeilen die Kombination aus name und Jg sind,
    # die Spalten das Alter und die Werte die Zeiten
    pivot_table <- discipline_data %>%
      unite("ID", name, Jg, remove = FALSE) %>%
      spread(Alter, Zeit)
    
    # Sicherstellen, dass alle Zeitspalten numerisch sind
    pivot_table <- pivot_table %>%
      mutate(across(where(is.character), as.numeric))
    
    # Berechnung der Korrelationen für Altersgruppen 11-17 im Vergleich zu 18
    corr_values <- sapply(11:17, function(age) {
      if (!is.null(pivot_table[[as.character(age)]])) {  # Überprüfe, ob die Spalte existiert
        cor(pivot_table[[as.character(age)]], pivot_table[["18"]], use = "complete.obs")
      } else {
        NA  # Falls die Spalte nicht existiert, NA zurückgeben
      }
    })
    
    # Speichern der Ergebnisse in einem DataFrame
    corr_results <- rbind(corr_results, data.frame(
      Age = 11:17,
      Correlation = corr_values,
      Discipline = paste0(streckenlaenge, "m ", switch(disziplin, F = "FR", B = "BR", R = "BA", S = "FL", L = "IM"))
    ))
  }
  
  return(corr_results) # Rückgabe der Korrelationsergebnisse
}

# Funktion zur Erstellung des Plots
plot_correlation_data <- function(corr_data, file_name) {
  # Erstellen des Plots
  plot <- ggplot(corr_data, aes(x = Age, y = Correlation, color = Discipline, group = Discipline)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      y = "Pearson correlation coefficient",
      x = "Age (years)"
    ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 24),
      axis.title = element_text(size = 24),
      axis.text = element_text(size = 24),
      legend.position = c(0.9, 0.1) # Positioniert die Legende im Koordinatensystem (rechts unten)
    )
  
  # Speichern des Bildes mit den gewünschten Abmessungen
  ggsave(paste0(file_name, ".png"), plot, width = 2496 / 300, height = 1872 / 300, dpi = 300)
  
  return(plot) # Rückgabe des Plots
}

# Daten einlesen
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/gesamt_daten_bereinigt.csv"
swim_data <- read.csv(file_path)

# Erstellen der Korrelationsergebnisse für die 50m, 100m und 200m Strecken
corr_50m <- compute_correlation_data(50, c('F', 'B', 'R', 'S'))
corr_100m <- compute_correlation_data(100, c('F', 'B', 'R', 'S'))
corr_200m <- compute_correlation_data(200, c('F', 'B', 'R', 'S', 'L'))

# Plot für 50m
plot_correlation_data(corr_50m, "50mPearson")

# Plot für 100m
plot_correlation_data(corr_100m, "100mPearson")

# Plot für 200m
plot_correlation_data(corr_200m, "200mPearson")
