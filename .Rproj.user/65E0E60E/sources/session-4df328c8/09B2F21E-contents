# Notwendige Bibliotheken laden
library(dplyr)
library(ggplot2)

# Daten einlesen
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/gesamt_daten.csv"
data <- read.csv(file_path)

# Erzeugen einer eindeutigen Schwimmer-ID
data <- data %>%
  mutate(swimmer_id = paste(name, Jg, sep = "_"))

# Kombinieren von "Streckenlänge" und "Disziplin", um die Events/Disziplinen zu erhalten
data <- data %>%
  mutate(event = paste(Streckenlänge, Disziplin, sep = " "))

# Verfügbare Events anzeigen
print(unique(data$event))

# Funktion zur Erstellung von Histogramm und QQ-Plot
create_plots <- function(event_input, age_group) {
  print(paste("Erstelle Plots für Event:", event_input, "und Alter:", age_group))
  
  # Filtern der Daten für das spezifische Event und Altersgruppe
  subset <- data %>%
    filter(event == event_input & Alter == age_group)
  
  # Überprüfen der gefilterten Daten
  if (nrow(subset) == 0) {
    print(paste("Keine Daten für Event:", event_input, "und Alter:", age_group))
    return(NULL)
  }
  
  print(head(subset))
  print(unique(subset$event))
  print(summary(subset$Fina))
  
  # Histogramm erstellen
  hist_plot <- ggplot(subset, aes(x = Fina)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    ggtitle(paste("Histogramm für", event_input, "Alter", age_group)) +
    xlab("Fina Punkte") +
    ylab("Dichte") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  # QQ-Plot erstellen
  qq_plot <- ggplot(subset, aes(sample = Fina)) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    ggtitle(paste("QQ-Plot für", event_input, "Alter", age_group)) +
    xlab("Theoretische Quantile") +
    ylab("Beobachtete Quantile") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  # Plots anzeigen
  print(hist_plot)
  print(qq_plot)
}

# Beispielaufruf der Funktion
create_plots("100 S", 18)
create_plots("200 F", 18)

