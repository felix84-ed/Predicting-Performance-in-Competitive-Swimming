# 1. Laden der benötigten Pakete
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

if(!require(caTools)) {
  install.packages("caTools")
  library(caTools)
}

# 2. Laden des Datensatzes (Pfad beachten)
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset_processed.csv")

# 3. Definieren der Zielvariable basierend auf den FINA-Punkten im Alter von 18 Jahren (nur zwei Gruppen)
data <- data %>%
  group_by(unique_id, Event) %>%
  mutate(group = case_when(
    Fina >= 750 & Alter == 18 ~ 1,   # Gruppe 1: FINA >= 750
    Fina < 750 & Alter == 18 ~ 2,    # Gruppe 2: FINA < 750
    TRUE ~ NA_real_  # Setzt NA für alle anderen Altersgruppen
  )) %>%
  fill(group, .direction = "downup") %>%
  ungroup()

# 4. Entfernen von Zeilen mit fehlenden Gruppenwerten (Schwimmer, die nicht mit 18 Jahren in der Liste sind)
data_clean <- data %>% filter(!is.na(group))

# 5. Behandlung fehlender Werte in den relevanten numerischen Features durch Median
numeric_features <- c("Frühestes_Alter", "improvement_rate", "max_yearly_improvement", 
                      "years_in_top100", "max_dist_categories", "max_disciplines", 
                      "max_z_time_rate", "z_time_ratio")
data_clean <- data_clean %>%
  mutate(across(all_of(numeric_features), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# 6. Aufteilen des Datensatzes in Trainings- und Testdaten (70% Training, 30% Test)
set.seed(123)  # Zufallsgenerator für Reproduzierbarkeit
split <- sample.split(data_clean$group, SplitRatio = 0.7)
train_data <- subset(data_clean, split == TRUE)
test_data <- subset(data_clean, split == FALSE)

# 7. Training des Random Forest Modells für zwei Gruppen (Klassifikation)
# Die Zielvariable `group` wird in einen Faktor umgewandelt, um eine Klassifikation zu erzwingen
train_data$group <- as.factor(train_data$group)
set.seed(123)
rf_model <- randomForest(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                           years_in_top100 + max_dist_categories + max_disciplines + 
                           max_z_time_rate + z_time_ratio, 
                         data = train_data, importance = TRUE)

# 8. Modell-Performance überprüfen
print(rf_model)

# Optional: Speichern des Random Forest Modells
saveRDS(rf_model, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/rf_model_2groups_fina.rds")

# 9. Evaluierung des Modells
# Vorhersagen auf den Testdaten
predictions <- predict(rf_model, newdata = test_data)

# 10. Erstellen der Konfusionsmatrix
confusion_matrix <- table(test_data$group, predictions)
print("Konfusionsmatrix:")
print(confusion_matrix)

# 11. Berechnen der Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))

# 12. Berechnen von Precision, Recall und F1-Score
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

print("Precision für jede Gruppe:")
print(precision)
print("Recall für jede Gruppe:")
print(recall)
print("F1-Score für jede Gruppe:")
print(f1_score)

# 13. Optional: Feature Importance analysieren und anzeigen
importance(rf_model)
# Anpassung der Ränder des Plots (unten, links, oben, rechts)
par(mar = c(5, 5, 4, 2))  # Passe die Werte an, um die Grafik zu verkleinern

# Erneutes Erstellen des varImpPlots
varImpPlot(rf_model)

# Grafik in eine Datei speichern
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/feature_importance.png", width = 800, height = 600)
varImpPlot(rf_model)
dev.off()  # Schließen der Grafikdatei



# Korrelationsanaylse -----------------------------------------------------
# Laden der notwendigen Pakete
if(!require(ggcorrplot)) {
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

# Auswahl der relevanten Spalten für die Korrelationsanalyse
correlation_features <- c("Frühestes_Alter", "improvement_rate", "max_yearly_improvement",
                          "years_in_top100", "max_dist_categories", "max_disciplines",
                          "fina_16", "max_z_time_rate", "z_time_ratio", "group")

# Erstellen eines Dataframes nur mit diesen Spalten
correlation_data <- data_clean[, correlation_features]

# Berechnen der Korrelationsmatrix
cor_matrix <- cor(correlation_data, use = "complete.obs")

# Visualisierung der Korrelationsmatrix
ggcorrplot(cor_matrix, lab = TRUE, title = "Korrelationsmatrix der Features", lab_size = 3)



