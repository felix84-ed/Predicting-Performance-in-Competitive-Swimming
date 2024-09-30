# Notwendige Bibliotheken laden
install.packages("lme4")
library(dplyr)
library(ggplot2)
library(lme4)

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

# Beispielaufruf der Funktion für Histogramm und QQ-Plot
create_plots("100 B", 18)
create_plots("100 F", 18)

# Anwendung von Mixed Linear Models (MLMs)
# Modellierung der Fina-Punkte anhand des Alters und eines zufälligen Effekts für individuelle Schwimmer
mlm_model <- lmer(Fina ~ Alter + (1 | swimmer_id), data = data)

# Zusammenfassung des Modells
summary(mlm_model)

# Modellvalidierung
# Residuenplot
plot(mlm_model)

# QQ-Plot der Residuen
qqnorm(resid(mlm_model))
qqline(resid(mlm_model))

# Histogramm der Residuen
hist(resid(mlm_model), breaks = 30, main = "Histogramm der Residuen", xlab = "Residuen", col = "orange")


# Notwendige Bibliotheken laden
library(lme4)
library(dplyr)

# Neue Daten für Vorhersagen erstellen (für die spezifische Disziplin "100 F")
new_data <- data.frame(
  Alter = rep(18, 10),  # Beispielhaft 10 Vorhersagen für Alter 18
  swimmer_id = rep('new_swimmer', 10),  # Dummy-Schwimmer-ID, da diese nicht verwendet wird
  event = rep("100 F", 10)  # Spezifische Disziplin
)

# Filtere die originalen Daten für die spezifische Disziplin
data_specific_event <- data %>% filter(event == "100 F")

# Modell neu anpassen, falls notwendig
mlm_model_specific <- lmer(Fina ~ Alter + (1 | swimmer_id), data = data_specific_event)

# Vorhersagen treffen
predictions <- predict(mlm_model_specific, newdata = new_data, re.form = NA)
print(predictions)

# Daten für die Vorhersagen vorbereiten
data_specific_event$Prediction <- predict(mlm_model_specific, newdata = data_specific_event, re.form = NULL)

# Scatterplot der Fina-Punkte vs. Alter mit den Vorhersagen
ggplot(data_specific_event, aes(x = Alter, y = Fina)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_line(aes(y = Prediction), color = "red") +
  labs(title = "Vorhersagen der Fina-Punkte basierend auf dem Alter für 100 F",
       x = "Alter",
       y = "Fina Punkte") +
  theme_minimal()
