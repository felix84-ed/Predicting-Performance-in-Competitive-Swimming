# 1. Laden der benötigten Pakete
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(caTools)) {
  install.packages("caTools")
  library(caTools)
}

if(!require(gbm)) {
  install.packages("gbm")
  library(gbm)
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

# 7. Umwandeln der Zielvariable in einen Faktor für Klassifikation
train_data$group <- as.factor(train_data$group)

# 8. Training des Gradient Boosting Modells
set.seed(123)
gbm_model <- gbm(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                   years_in_top100 + max_dist_categories + max_disciplines + 
                   max_z_time_rate + z_time_ratio,
                 data = train_data,
                 distribution = "multinomial",  # Da es eine Klassifikation ist
                 n.trees = 5000,  # Anzahl der Bäume
                 interaction.depth = 3,  # Maximale Tiefe der Interaktionen
                 shrinkage = 0.01,  # Lernrate
                 cv.folds = 5,  # Cross-Validation mit 5 Folds
                 n.minobsinnode = 10,  # Minimale Anzahl der Beobachtungen pro Knoten
                 verbose = TRUE)

# 9. Optimale Anzahl von Bäumen auswählen (basierend auf Cross-Validation)
best_iter <- gbm.perf(gbm_model, method = "cv")

# 10. Modell-Performance überprüfen
print(gbm_model)

# Optional: Speichern des Gradient Boosting Modells
saveRDS(gbm_model, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/gbm_model_2groups_fina.rds")

# 11. Vorhersagen auf den Testdaten
predictions <- predict(gbm_model, newdata = test_data, n.trees = best_iter, type = "response")
predictions_class <- apply(predictions, 1, which.max)

# 12. Erstellen der Konfusionsmatrix
confusion_matrix <- table(test_data$group, predictions_class)
print("Konfusionsmatrix:")
print(confusion_matrix)

# 13. Berechnen der Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))

# 14. Berechnen von Precision, Recall und F1-Score
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

print("Precision für jede Gruppe:")
print(precision)
print("Recall für jede Gruppe:")
print(recall)
print("F1-Score für jede Gruppe:")
print(f1_score)
