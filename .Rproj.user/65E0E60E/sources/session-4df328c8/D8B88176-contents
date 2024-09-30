# Daten filtern-------------------------------------------------------------------------

# Laden der benötigten Bibliotheken
library(dplyr)
library(caret)
library(gbm)

# Daten laden
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"
data <- read.csv(file_path)

# Daten filtern für die Disziplin "100 F"
data_100F <- data %>%
  filter(Streckenlänge == 100 & Disziplin == "F")

# Auswahl der relevanten Merkmale und der Zielvariable
features <- c('Frühestes_Alter', 'Anzahl_Disziplinen', 'Anzahl_Distanz_Kategorien',
              'Anzahl_Einzigartige_Altersgruppen', 'years_in_top100', 'percent_diff_improvement_11y')
target <- 'Zeit'

# Datensatz auf relevante Merkmale und Zielvariable reduzieren
data_relevant <- data_100F %>%
  select(all_of(features), all_of(target), Alter)

# Fehlende Werte in der Spalte "percent_diff_improvement_11y" durch den Medianwert ersetzen
median_value <- median(data_relevant$percent_diff_improvement_11y, na.rm = TRUE)
data_relevant$percent_diff_improvement_11y[is.na(data_relevant$percent_diff_improvement_11y)] <- median_value

# Überprüfen der Daten
head(data_relevant)
median_value

# Daten aufteilen ---------------------------------------------------------
# Setzen eines Seed für die Reproduzierbarkeit
set.seed(123)

# Aufteilen der Daten in Trainings- und Testdatensätze
split <- sample.split(data_relevant$Zeit, SplitRatio = 0.8)
train_data <- subset(data_relevant, split == TRUE)
test_data <- subset(data_relevant, split == FALSE)

# Überprüfen der Dimensionen der aufgeteilten Datensätze
dim(train_data)
dim(test_data)

# Modelltraining ----------------------------------------------------------
# AdaBoost-ähnliches Modell (Gradient Boosting) trainieren
gbm_model <- gbm(Zeit ~ Frühestes_Alter + Anzahl_Disziplinen + Anzahl_Distanz_Kategorien +
                   Anzahl_Einzigartige_Altersgruppen + years_in_top100 + percent_diff_improvement_11y + Alter, 
                 data = train_data, 
                 distribution = "gaussian", 
                 n.trees = 100, 
                 interaction.depth = 3, 
                 shrinkage = 0.1, 
                 cv.folds = 5)

# Optimale Anzahl der Bäume basierend auf Kreuzvalidierung
best_trees <- gbm.perf(gbm_model, method = "cv")

# Vorhersagen auf dem Testdatensatz durchführen
predictions <- predict(gbm_model, newdata = test_data, n.trees = best_trees)

# Vorhersagen anzeigen
head(predictions)

# Modellbewertung ---------------------------------------------------------
# Bewertung der Modellleistung mit den zurückskalierten Daten

# Berechnung des Mean Squared Error (MSE)
mse <- mean((test_data$Zeit - predictions)^2)

# Berechnung des R²-Scores
sst <- sum((test_data$Zeit - mean(test_data$Zeit))^2)
sse <- sum((test_data$Zeit - predictions)^2)
r_squared <- 1 - (sse / sst)

# Ausgabe der Bewertung
mse
r_squared

# Modellvisualisierung ----------------------------------------------------
# Visualisierung der tatsächlichen vs. vorhergesagten Zeiten

# Bibliothek für die Visualisierung laden
library(ggplot2)

# Vergleich der tatsächlichen und vorhergesagten Zeiten
comparison <- data.frame(
  Actual = test_data$Zeit,
  Predicted = predictions
)

# Scatterplot der tatsächlichen vs. vorhergesagten Werte
ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = "Tatsächliche vs. vorhergesagte Schwimmzeiten (100 F)",
       x = "Tatsächliche Zeiten (Sekunden)",
       y = "Vorhergesagte Zeiten (Sekunden)") +
  theme_minimal()

# Feature Wichtigkeit -----------------------------------------------------
# Bestimmung der Feature-Wichtigkeit
importance <- summary(gbm_model, n.trees = best_trees, plotit = FALSE)

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
  labs(title = "Feature-Wichtigkeit im gbm-Modell (100 F)",
       x = "Merkmale",
       y = "Wichtigkeit") +
  theme_minimal()


