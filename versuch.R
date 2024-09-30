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

# Daten für die relevanten Altersgruppen filtern und umbenennen
data_11 <- data_filter %>%
  filter(Alter == 11) %>%
  rename_with(~ paste0(., "_11"), -c(unique_id, Alter))

data_12 <- data_filter %>%
  filter(Alter == 12) %>%
  rename_with(~ paste0(., "_12"), -c(unique_id, Alter))

data_13 <- data_filter %>%
  filter(Alter == 13) %>%
  rename_with(~ paste0(., "_13"), -c(unique_id, Alter))

data_14 <- data_filter %>%
  filter(Alter == 14) %>%
  rename_with(~ paste0(., "_14"), -c(unique_id, Alter))

data_18 <- data_filter %>%
  filter(Alter == 18) %>%
  rename_with(~ paste0(., "_18"), -c(unique_id, Alter))

# Verknüpfen der Daten für die Altersgruppen
merged_data <- data_18 %>%
  left_join(data_11, by = "unique_id") %>%
  left_join(data_12, by = "unique_id") %>%
  left_join(data_13, by = "unique_id") %>%
  left_join(data_14, by = "unique_id")

# Berechnung der Verbesserungsrate für jede Altersgruppe
merged_data <- merged_data %>%
  mutate(improvement_11 = ifelse(!is.na(Zeit_11), (Zeit_11 - Zeit_18) / Zeit_11, NA),
         improvement_12 = ifelse(!is.na(Zeit_12), (Zeit_12 - Zeit_18) / Zeit_12, NA),
         improvement_13 = ifelse(!is.na(Zeit_13), (Zeit_13 - Zeit_18) / Zeit_13, NA),
         improvement_14 = ifelse(!is.na(Zeit_14), (Zeit_14 - Zeit_18) / Zeit_14, NA))

# Ersetzen fehlender Verbesserungsraten mit dem Median
median_improvement_11 <- median(merged_data$improvement_11, na.rm = TRUE)
median_improvement_12 <- median(merged_data$improvement_12, na.rm = TRUE)
median_improvement_13 <- median(merged_data$improvement_13, na.rm = TRUE)
median_improvement_14 <- median(merged_data$improvement_14, na.rm = TRUE)

merged_data <- merged_data %>%
  mutate(improvement_11 = ifelse(is.na(improvement_11), median_improvement_11, improvement_11),
         improvement_12 = ifelse(is.na(improvement_12), median_improvement_12, improvement_12),
         improvement_13 = ifelse(is.na(improvement_13), median_improvement_13, improvement_13),
         improvement_14 = ifelse(is.na(improvement_14), median_improvement_14, improvement_14))

# Auswahl der relevanten Merkmale und der Zielvariablen
features <- c('Frühestes_Alter_18',
              'X.Nr_Disziplinen.Jahr._14', 
               'X.Nr_Dist_Kat.Jahr._14',
              'X.Nr_Jahre_Top100.Allg._18',
              'X.Nr_Jahre_Top100.Disz._18',
                'percent_diff_improvement_13y_14',
               'Zeit_14')
target <- 'improvement_14'  # Beispielhaft für 14 Jahre, kann für andere Altersgruppen angepasst werden

# Datensatz auf relevante Merkmale und Zielvariable reduzieren
model_data <- merged_data %>%
  select(all_of(features), all_of(target))

# Train/Test Split auf den Daten
set.seed(123)
split <- sample.split(model_data[[target]], SplitRatio = 0.8)
train_data <- subset(model_data, split == TRUE)
test_data <- subset(model_data, split == FALSE)

# Modelltraining ----------------------------------------------------------
# AdaBoost-ähnliches Modell (Gradient Boosting) trainieren
gbm_model <- gbm(as.formula(paste(target, "~ .")), 
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
# Bewertung der Modellleistung

# Berechnung des Mean Squared Error (MSE)
mse <- mean((test_data[[target]] - predictions)^2)

# Berechnung des R²-Scores
sst <- sum((test_data[[target]] - mean(test_data[[target]]))^2)
sse <- sum((test_data[[target]] - predictions)^2)
r_squared <- 1 - (sse / sst)

# Ausgabe der Bewertung
mse
r_squared

# Modellvisualisierung ----------------------------------------------------
# Vergleich der tatsächlichen vs. vorhergesagten Werte
comparison <- data.frame(
  Actual = test_data[[target]],
  Predicted = predictions
)

# Scatterplot der tatsächlichen vs. vorhergesagten Werte erstellen
plot <- ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(title = paste("Tatsächliche vs. vorhergesagte Verbesserungsraten (", filtered_discipline, ")", sep = ""),
       x = "Tatsächliche Verbesserungsraten",
       y = "Vorhergesagte Verbesserungsraten") +
  theme_minimal()

# Define the output file path for the plot
plot_file_path <- paste0("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/BachelorArbeit/Ergebnisse/Verbesserungsrate_Vorhersage_", gsub(" ", "_", filtered_discipline), "_Plot.png")

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
  Metric = c("mse", "R²", "Frühestes_Alter", "Zeit_14", "Anzahl Distanz Kategorie im Jahr", 
             "Anzahl Jahre in Top100 in Disziplin", "Anzahl Disziplinen im Jahr", "Anzahl Jahre in Top100"),
  Value = c(
    mse, 
    r_squared, 
    importance["Frühestes_Alter_18", "rel.inf"], 
    importance["Zeit_14", "rel.inf"], 
    importance["X.Nr_Dist_Kat.Jahr._18", "rel.inf"], 
    importance["X.Nr_Jahre_Top100.Disz._18", "rel.inf"], 
    importance["X.Nr_Disziplinen.Jahr._18", "rel.inf"], 
    importance["X.Nr_Jahre_Top100.Allg._18", "rel.inf"]
  )
)

# Define the output file path for the results
results_file_path <- paste0("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/BachelorAr
