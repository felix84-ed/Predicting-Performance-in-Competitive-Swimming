# 1. Laden der notwendigen Pakete
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# 2. Laden des Datensatzes
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset_processed.csv")

# 3. Definieren der Zielvariable basierend auf den FINA-Punkten im Alter von 18 Jahren (nur zwei Gruppen)
data <- data %>%
  group_by(unique_id, Event) %>%
  mutate(group = case_when(
    Fina < 750 & Alter == 18 ~ 2,    # Gruppe 2: FINA < 750
    Fina >= 750 & Alter == 18 ~ 1,   # Gruppe 1: FINA >= 750
    TRUE ~ NA_real_  # Setzt NA für alle anderen Altersgruppen
  )) %>%
  fill(group, .direction = "downup") %>%
  ungroup()

# Entfernen von Zeilen mit fehlenden Gruppenwerten
data_clean <- data %>% filter(!is.na(group))

# 4. Behandlung fehlender Werte: Ersetzen von NA-Werten in numerischen Variablen durch den Median
numeric_features <- c("Frühestes_Alter", "improvement_rate", "max_yearly_improvement", 
                      "years_in_top100", "max_dist_categories", "max_disciplines", 
                      "fina_16", "max_z_time_rate", "z_time_ratio")

data_clean[numeric_features] <- data_clean[numeric_features] %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Wandle die Zielvariable in einen Faktor um
data_clean$group <- as.factor(data_clean$group)

# NEU: Aufteilen des Datensatzes in Trainings- und Testdaten (70% Training, 30% Test)
set.seed(123)  # Zufallsgenerator für Reproduzierbarkeit
split <- sample.split(data_clean$group, SplitRatio = 0.7)
train_data <- subset(data_clean, split == TRUE)
test_data <- subset(data_clean, split == FALSE)

# 5. Trainieren des Random Forest Modells auf den Trainingsdaten
set.seed(123)
rf_model <- randomForest(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                           years_in_top100 + max_dist_categories + max_disciplines + 
                           fina_16 + max_z_time_rate + z_time_ratio, 
                         data = train_data, importance = TRUE)

# 6. Ausgabe der Feature Importance
print("Feature Importance:")
importance(rf_model)

# 7. Visualisierung der Feature Importance
varImpPlot(rf_model)

# 8. Evaluierung des Modells auf den Testdaten
predictions <- predict(rf_model, newdata = test_data)

# 9. Erstellen der Konfusionsmatrix
confusion_matrix <- table(test_data$group, predictions)
print("Konfusionsmatrix:")
print(confusion_matrix)

# 10. Berechnen der Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))

# 11. Berechnen von Precision, Recall und F1-Score
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

print("Precision für jede Gruppe:")
print(precision)
print("Recall für jede Gruppe:")
print(recall)
print("F1-Score für jede Gruppe:")
print(f1_score)

# 12. Optional: Klassengewichtung hinzufügen und Modell erneut trainieren
class_weights <- table(train_data$group)  # Häufigkeit jeder Gruppe
class_weights <- max(class_weights) / class_weights  # Umgekehrte Gewichtung

set.seed(123)
rf_model_weighted <- randomForest(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                                    years_in_top100 + max_dist_categories + max_disciplines + 
                                    fina_16 + max_z_time_rate + z_time_ratio, 
                                  data = train_data, importance = TRUE, classwt = class_weights)

# Ausgabe der Modell-Performance mit Klassengewichtung
print("Modell mit Klassengewichtung:")
print(rf_model_weighted)

# Feature Importance mit Klassengewichtung
importance(rf_model_weighted)
varImpPlot(rf_model_weighted)


# ROC ---------------------------------------------------------------------
# 1. Laden der notwendigen Pakete
if(!require(pROC)) {
  install.packages("pROC")
  library(pROC)
}

# 2. Vorhersagen auf Wahrscheinlichkeitsbasis für das Random Forest Modell
rf_probabilities <- predict(rf_model, newdata = test_data, type = "prob")

# 3. Berechnen der ROC-Kurve und des AUC-Werts für Gruppe 1 vs. Gruppe 2
roc_curve <- roc(test_data$group, rf_probabilities[,2], levels = c(1, 2), direction = "<")

# 4. Extrahieren von True Positive Rate (Sensitivity) und False Positive Rate (1 - Specificity)
true_positive_rate <- roc_curve$sensitivities
false_positive_rate <- 1 - roc_curve$specificities

# 5. Plotten der ROC-Kurve: True Positive Rate gegen False Positive Rate
plot(false_positive_rate, true_positive_rate, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)",
     main = "ROC Curve for Random Forest (Group 1 vs Group 2)")

# 6. Berechnen und Anzeigen des AUC-Werts
auc_value <- auc(roc_curve)
print(paste("AUC-Wert: ", round(auc_value, 4)))

# 7. Optional: Hinzufügen einer Diagonalen als Referenzlinie (Chance Line)
abline(a = 0, b = 1, col = "red", lty = 2)
legend("bottomright", legend = c("ROC Curve", "Chance Line"), col = c("blue", "red"), lwd = 2, lty = 1:2)

# 13. Optional: Feature Importance analysieren und anzeigen
importance(rf_model)
# Anpassung der Ränder des Plots (unten, links, oben, rechts)
par(mar = c(5, 5, 4, 2))  # Passe die Werte an, um die Grafik zu verkleinern

# Erneutes Erstellen des varImpPlots
varImpPlot(rf_model)

# Grafik in eine Datei speichern
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/feature_importance.png", width = 800, height = 600)
varImpPlot(rf_model)
dev.off()