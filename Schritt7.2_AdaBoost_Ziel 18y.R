# Laden der benötigten Bibliotheken
library(dplyr)
library(caret)
library(gbm)
library(caTools)
library(ggplot2)

# Daten laden
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"
data <- read.csv(file_path)

# Annahme, dass die gefilterte Disziplin in einer Variablen gespeichert ist
filtered_discipline <- "100 F"  # Dies sollte durch den tatsächlich verwendeten Wert ersetzt werden

# Daten filtern für die Disziplin
data_filter <- data %>%
  filter(Event == filtered_discipline)

# Auswahl der relevanten Merkmale und der Zielvariable
features <- c('Frühestes_Alter', 'X.Nr_Disziplinen.Jahr.', 'X.Nr_Dist_Kat.Jahr.',
              'X.Nr_Jahre_Top100.Allg.', 'X.Nr_Jahre_Top100.Disz.', 'percent_diff_improvement_14y', 'Alter', 'unique_id')
target <- 'Zeit'

# Datensatz auf relevante Merkmale und Zielvariable reduzieren
data_relevant <- data_filter %>%
  select(all_of(features), all_of(target))

# Fehlende Werte in der Spalte "percent_diff_improvement_14y" durch den Medianwert ersetzen
median_value <- median(data_relevant$percent_diff_improvement_14y, na.rm = TRUE)
data_relevant$percent_diff_improvement_14y[is.na(data_relevant$percent_diff_improvement_14y)] <- median_value

# Daten für Training und Test vorbereiten
# Daten filtern für Schwimmer, die im Alter von 18 Jahren angetreten sind
data_18 <- data_relevant %>%
  filter(Alter == 18)

# Historische Daten der gleichen Schwimmer für Training verwenden
data_train <- data_relevant %>%
  filter(unique_id %in% data_18$unique_id & Alter < 18)

# Train/Test Split auf den historischen Daten
set.seed(123)
split <- sample.split(data_train$Zeit, SplitRatio = 0.8)
train_data <- subset(data_train, split == TRUE)
test_data <- data_18

# Modelltraining ----------------------------------------------------------
# AdaBoost-ähnliches Modell (Gradient Boosting) trainieren
gbm_model <- gbm(Zeit ~ Frühestes_Alter + X.Nr_Disziplinen.Jahr. + X.Nr_Dist_Kat.Jahr. +
                   X.Nr_Jahre_Top100.Allg. + X.Nr_Jahre_Top100.Disz. + percent_diff_improvement_14y + Alter, 
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
# Vergleich der tatsächlichen und vorhergesagten Zeiten
comparison <- data.frame(
  Actual = test_data$Zeit,
  Predicted = predictions
)

# Scatterplot der tatsächlichen vs. vorhergesagten Werte erstellen
plot <- ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = paste("Tatsächliche vs. vorhergesagte Schwimmzeiten (", filtered_discipline, ")", sep = ""),
       x = "Tatsächliche Zeiten (Sekunden)",
       y = "Vorhergesagte Zeiten (Sekunden)") +
  theme_minimal()

# Define the output file path for the plot
plot_file_path <- paste0("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/BachelorArbeit/Ergebnisse/Schwimmzeiten_Vorhersage_", gsub(" ", "_", filtered_discipline), "_Plot.png")

# Save the plot as a PNG file
ggsave(filename = plot_file_path, plot = plot, width = 10, height = 6)

# Überprüfung des Dateipfads
print(plot_file_path)

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
  labs(title = paste("Feature-Wichtigkeit im gbm-Modell (", filtered_discipline, ")", sep = ""),
       x = "Merkmale",
       y = "Wichtigkeit") +
  theme_minimal()

# Ergebnisse speichern ---------------------------------------------------
# Datenrahmen erstellen mit den gewünschten Werten
results <- data.frame(
  Metric = c("mse", "R²", "Alter", "percent_diff_improvement_14y", "Anzahl Distanz Kategorie im Jahr", 
             "Anzahl Jahre in Top100 in Disziplin", "Anzahl Disziplinen im Jahr", "Anzahl Jahre in Top100", "Frühestes Alter"),
  Value = c(
    mse, 
    r_squared, 
    importance["Alter", "rel.inf"], 
    importance["percent_diff_improvement_14y", "rel.inf"], 
    importance["X.Nr_Dist_Kat.Jahr.", "rel.inf"], 
    importance["X.Nr_Jahre_Top100.Disz.", "rel.inf"], 
    importance["X.Nr_Disziplinen.Jahr.", "rel.inf"], 
    importance["X.Nr_Jahre_Top100.Allg.", "rel.inf"], 
    importance["Frühestes_Alter", "rel.inf"]
  )
)

# Define the output file path for the results
results_file_path <- paste0("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/BachelorArbeit/Ergebnisse/Schwimmzeiten_Vorhersage_Ergebnisse_", gsub(" ", "_", filtered_discipline), ".csv")

# Save the results to a CSV file
write.csv(results, file = results_file_path, row.names = FALSE)

# Überprüfung der gespeicherten Ergebnisse
print(results_file_path)
print(results)
