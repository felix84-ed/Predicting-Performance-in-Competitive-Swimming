# Schritt 1: Daten laden und vorverarbeiten

# Laden der benötigten Bibliotheken
library(dplyr)

# Pfad zur Datei
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"

# Daten laden
data <- read.csv(file_path)

# Relevante Merkmale auswählen
features <- c('Alter', 'Streckenlänge', 'Disziplin', 'Frühestes_Alter', 
              'Anzahl_Disziplinen', 'Distanz_Kategorie', 'Anzahl_Distanz_Kategorien', 
              'Anzahl_Einzigartige_Altersgruppen', 'years_in_top100', 'jaehrliche_verbesserung_prozent')

# Zielvariable definieren
target <- 'Zeit'

# Datensatz auf relevante Merkmale und Zielvariable reduzieren
data_relevant <- data %>% select(all_of(c(features, target)))

# Überprüfen auf fehlende Werte
sum(is.na(data_relevant))

# Fehlende Werte in der Spalte "jaehrliche_verbesserung_prozent" durch den Medianwert ersetzen
median_value <- median(data_relevant$jaehrliche_verbesserung_prozent, na.rm = TRUE)
data_relevant$jaehrliche_verbesserung_prozent[is.na(data_relevant$jaehrliche_verbesserung_prozent)] <- median_value

# Überprüfen, ob alle fehlenden Werte behandelt wurden
sum(is.na(data_relevant))

# Erste Zeilen des vorverarbeiteten Datensatzes anzeigen
head(data_relevant)

# Schritt 2 ---------------------------------------------------------------
# Schritt 2: Kategorische Variablen kodieren

# Bibliothek für One-Hot-Encoding laden
library(caret)

# One-Hot-Encoding für die Variablen "Disziplin" und "Distanz_Kategorie"
data_encoded <- dummyVars("~ .", data = data_relevant)
data_transformed <- data.frame(predict(data_encoded, newdata = data_relevant))

# Überprüfen der kodierten Daten
head(data_transformed)

# Schritt 3 ---------------------------------------------------------------
# Schritt 3: Daten skalieren

# Bibliothek für das Skalieren der Daten laden
library(scales)

# Numerische Merkmale skalieren
data_scaled <- data_transformed %>% 
  mutate_if(is.numeric, rescale)

# Überprüfen der skalierten Daten
head(data_scaled)

# Schritt 4 ---------------------------------------------------------------
# Schritt 4: Daten in Trainings- und Testdatensätze aufteilen

# Bibliothek für das Aufteilen der Daten laden
library(caTools)

# Setzen eines Seed für die Reproduzierbarkeit
set.seed(123)

# Aufteilen der Daten in Trainings- und Testdatensätze
split <- sample.split(data_scaled$Zeit, SplitRatio = 0.8)
train_data <- subset(data_scaled, split == TRUE)
test_data <- subset(data_scaled, split == FALSE)

# Überprüfen der Dimensionen der aufgeteilten Datensätze
dim(train_data)
dim(test_data)

# Schritt 5 ---------------------------------------------------------------
# Paket installieren
install.packages("gbm")

# Paket laden
library(gbm)
# AdaBoost-ähnliches Modell (Gradient Boosting) trainieren
# Zeit als Zielvariable, alle anderen als Prädiktoren
gbm_model <- gbm(Zeit ~ ., 
                 data = train_data, 
                 distribution = "gaussian", 
                 n.trees = 100, 
                 interaction.depth = 3, 
                 shrinkage = 0.1, 
                 cv.folds = 5)

# Vorhersagen auf dem Testdatensatz durchführen
predictions <- predict(gbm_model, newdata = test_data, n.trees = gbm.perf(gbm_model, method = "cv"))

# Vorhersagen anzeigen
head(predictions)

# Schritt 6 ---------------------------------------------------------------
# Schritt 6: Modellbewertung

# Berechnung des Mean Squared Error (MSE)
mse <- mean((test_data$Zeit - predictions)^2)

# Berechnung des R²-Scores
sst <- sum((test_data$Zeit - mean(test_data$Zeit))^2)
sse <- sum((test_data$Zeit - predictions)^2)
r_squared <- 1 - (sse / sst)

# Ausgabe der Bewertung
mse
r_squared

# Schritt 7 ---------------------------------------------------------------
# Schritt 7: Überprüfung der Skalen

# Zurückskalieren der Vorhersagen und der tatsächlichen Werte (falls skaliert)
# Hier angenommen, dass die ursprünglichen Zeitwerte in Sekunden sind

# Vergleich der ersten Vorhersagen mit den tatsächlichen Werten
comparison <- data.frame(
  Actual = test_data$Zeit,
  Predicted = predictions
)

# Anzeigen der ersten Zeilen der Vergleichsdaten
head(comparison)

# Schritt 8 ---------------------------------------------------------------
# Schritt 8: Visualisierung der Ergebnisse

# Bibliothek für die Visualisierung laden
library(ggplot2)

# Scatterplot der tatsächlichen vs. vorhergesagten Werte
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "Tatsächliche vs. vorhergesagte Schwimmzeiten",
       x = "Tatsächliche Zeiten (Sekunden)",
       y = "Vorhergesagte Zeiten (Sekunden)") +
  theme_minimal()


# Schritt 9 ---------------------------------------------------------------

# Schritt 9: Zurückskalieren der Daten

# Annahme: Ursprüngliche Daten wurden zwischen 0 und 1 skaliert
# Extrahieren der ursprünglichen Skalenparameter
original_min <- min(data_relevant$Zeit)
original_max <- max(data_relevant$Zeit)

# Funktion zum Zurückskalieren
rescale_back <- function(x, min, max) {
  x * (max - min) + min
}

# Zurückskalieren der Vorhersagen und der tatsächlichen Werte
comparison$Actual_unscaled <- rescale_back(comparison$Actual, original_min, original_max)
comparison$Predicted_unscaled <- rescale_back(comparison$Predicted, original_min, original_max)

# Anzeigen der zurückskalierten Vergleichsdaten
head(comparison)

# Bewertung der Modellleistung mit den zurückskalierten Daten

# Berechnung des Mean Squared Error (MSE) für die zurückskalierten Daten
mse_unscaled <- mean((comparison$Actual_unscaled - comparison$Predicted_unscaled)^2)

# Berechnung des R²-Scores für die zurückskalierten Daten
sst_unscaled <- sum((comparison$Actual_unscaled - mean(comparison$Actual_unscaled))^2)
sse_unscaled <- sum((comparison$Actual_unscaled - comparison$Predicted_unscaled)^2)
r_squared_unscaled <- 1 - (sse_unscaled / sst_unscaled)

# Ausgabe der Bewertung
mse_unscaled
r_squared_unscaled

# Visualisierung der zurückskalierten Daten

# Bibliothek für die Visualisierung laden
library(ggplot2)

# Scatterplot der tatsächlichen vs. vorhergesagten zurückskalierten Werte
ggplot(comparison, aes(x = Actual_unscaled, y = Predicted_unscaled)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "Tatsächliche vs. vorhergesagte Schwimmzeiten (zurückskaliert)",
       x = "Tatsächliche Zeiten (Sekunden)",
       y = "Vorhergesagte Zeiten (Sekunden)") +
  theme_minimal()


# Schritt 10 Kreuzvalidierung ---------------------------------------------
# Schritt 1: Kreuzvalidierung mit angepassten Modellparametern

# AdaBoost-ähnliches Modell (Gradient Boosting) trainieren mit angepassten Parametern
gbm_model_cv <- gbm(Zeit ~ ., 
                    data = train_data, 
                    distribution = "gaussian", 
                    n.trees = 50,  # Reduzierte Anzahl der Bäume
                    interaction.depth = 2,  # Geringere Tiefe der Interaktionen
                    shrinkage = 0.05,  # Erhöhter Shrinkage-Wert
                    cv.folds = 5)

# Optimale Anzahl der Bäume basierend auf Kreuzvalidierung
best_trees <- gbm.perf(gbm_model_cv, method = "cv")

# Vorhersagen auf dem Testdatensatz durchführen mit der optimalen Anzahl der Bäume
predictions_cv <- predict(gbm_model_cv, newdata = test_data, n.trees = best_trees)

# Zurückskalieren der Vorhersagen
comparison_cv <- data.frame(
  Actual = test_data$Zeit,
  Predicted = predictions_cv
)

comparison_cv$Actual_unscaled <- rescale_back(comparison_cv$Actual, original_min, original_max)
comparison_cv$Predicted_unscaled <- rescale_back(comparison_cv$Predicted, original_min, original_max)

# Bewertung der zurückskalierten Daten
mse_unscaled_cv <- mean((comparison_cv$Actual_unscaled - comparison_cv$Predicted_unscaled)^2)
sst_unscaled_cv <- sum((comparison_cv$Actual_unscaled - mean(comparison_cv$Actual_unscaled))^2)
sse_unscaled_cv <- sum((comparison_cv$Actual_unscaled - comparison_cv$Predicted_unscaled)^2)
r_squared_unscaled_cv <- 1 - (sse_unscaled_cv / sst_unscaled_cv)

# Ausgabe der Bewertung
mse_unscaled_cv
r_squared_unscaled_cv


# Schritt 11 Feature Importance Analyse -----------------------------------
# Schritt 11: Feature Importance analysieren

# Bestimmung der Feature-Wichtigkeit
importance <- summary(gbm_model_cv, n.trees = best_trees, plotit = FALSE)

# Anzeige der Feature-Wichtigkeit
print(importance)

# Visualisierung der Feature-Wichtigkeit
library(ggplot2)

# Umwandlung in ein DataFrame für ggplot
importance_df <- data.frame(
  Feature = rownames(importance),
  Importance = importance[,1]
)

# Plot der Feature-Wichtigkeit
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature-Wichtigkeit im gbm-Modell",
       x = "Merkmale",
       y = "Wichtigkeit") +
  theme_minimal()

# Schritt 12: Warnungen anzeigen

# Anzeigen der aufgetretenen Warnungen
warnings()


# Schritt 13 --------------------------------------------------------------

# Schritt 13: Datentypen überprüfen und korrigieren

# Überprüfen der Datentypen der relevanten Merkmale
str(data_scaled)

# Sicherstellen, dass alle numerischen Merkmale als numerische Typen kodiert sind
data_scaled <- data_scaled %>%
  mutate(
    Alter = as.numeric(Alter),
    Streckenlänge = as.numeric(Streckenlänge),
    Frühestes_Alter = as.numeric(Frühestes_Alter),
    Anzahl_Disziplinen = as.numeric(Anzahl_Disziplinen),
    Anzahl_Distanz_Kategorien = as.numeric(Anzahl_Distanz_Kategorien),
    Anzahl_Einzigartige_Altersgruppen = as.numeric(Anzahl_Einzigartige_Altersgruppen),
    years_in_top100 = as.numeric(years_in_top100),
    jaehrliche_verbesserung_prozent = as.numeric(jaehrliche_verbesserung_prozent),
    Zeit = as.numeric(Zeit)
  )

# Erneutes Trainieren des Modells und Berechnung der Feature-Wichtigkeit
gbm_model_cv <- gbm(Zeit ~ ., 
                    data = train_data, 
                    distribution = "gaussian", 
                    n.trees = 50,  
                    interaction.depth = 2,  
                    shrinkage = 0.05,  
                    cv.folds = 5)

# Optimale Anzahl der Bäume basierend auf Kreuzvalidierung
best_trees <- gbm.perf(gbm_model_cv, method = "cv")

# Vorhersagen auf dem Testdatensatz durchführen mit der optimalen Anzahl der Bäume
predictions_cv <- predict(gbm_model_cv, newdata = test_data, n.trees = best_trees)

# Zurückskalieren der Vorhersagen
comparison_cv <- data.frame(
  Actual = test_data$Zeit,
  Predicted = predictions_cv
)

comparison_cv$Actual_unscaled <- rescale_back(comparison_cv$Actual, original_min, original_max)
comparison_cv$Predicted_unscaled <- rescale_back(comparison_cv$Predicted, original_min, original_max)

# Bewertung der zurückskalierten Daten
mse_unscaled_cv <- mean((comparison_cv$Actual_unscaled - comparison_cv$Predicted_unscaled)^2)
sst_unscaled_cv <- sum((comparison_cv$Actual_unscaled - mean(comparison_cv$Actual_unscaled))^2)
sse_unscaled_cv <- sum((comparison_cv$Actual_unscaled - comparison_cv$Predicted_unscaled)^2)
r_squared_unscaled_cv <- 1 - (sse_unscaled_cv / sst_unscaled_cv)

# Ausgabe der Bewertung
mse_unscaled_cv
r_squared_unscaled_cv

# Feature-Wichtigkeit neu berechnen
importance <- summary(gbm_model_cv, n.trees = best_trees, plotit = FALSE)

# Anzeige der Feature-Wichtigkeit
print(importance)

# Visualisierung der Feature-Wichtigkeit
importance_df <- data.frame(
  Feature = rownames(importance),
  Importance = importance[,1]
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature-Wichtigkeit im gbm-Modell",
       x = "Merkmale",
       y = "Wichtigkeit") +
  theme_minimal()








