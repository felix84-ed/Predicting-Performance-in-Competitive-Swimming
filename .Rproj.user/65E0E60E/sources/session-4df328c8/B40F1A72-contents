# 1. Laden der notwendigen Pakete
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(glmnet)) {
  install.packages("glmnet")
  library(glmnet)
}

if(!require(pROC)) {
  install.packages("pROC")
  library(pROC)
}

if(!require(caret)) {
  install.packages("caret")
  library(caret)
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
                      "fina_16", "max_z_time_rate", "z_time_ratio", "jaehrliche_verbesserung_prozent")

data_clean[numeric_features] <- data_clean[numeric_features] %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Wandle die Zielvariable in einen Faktor um
data_clean$group <- as.factor(data_clean$group)

# 5. Stratifizierte Folds erstellen (sicherstellen, dass jede Gruppe in den Folds vertreten ist)
set.seed(123)
folds <- createFolds(data_clean$group, k = 5, list = TRUE, returnTrain = TRUE)

# 6. Folds durchlaufen und Metriken berechnen
accuracy_values <- c()
precision_values <- list()
recall_values <- list()
f1_scores <- list()

for (i in 1:5) {
  # Trainings- und Testdaten für den Fold erstellen
  fold_train <- data_clean[folds[[i]], ]
  fold_test <- data_clean[-folds[[i]], ]
  
  # 7. Logistisches Regressionsmodell trainieren
  X_train <- model.matrix(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                            years_in_top100 + max_dist_categories + max_disciplines + 
                            fina_16 + max_z_time_rate + z_time_ratio, data = fold_train)[, -1]
  
  y_train <- fold_train$group
  
  # Trainiere das logistische Regressionsmodell
  log_reg_model <- glmnet(X_train, as.numeric(y_train) - 1, family = "binomial")
  
  # Bestes Lambda für logistische Regression auswählen (Kreuzvalidierung)
  cv_model <- cv.glmnet(X_train, as.numeric(y_train) - 1, family = "binomial")
  best_lambda <- cv_model$lambda.min
  
  # 8. Vorhersagen für die Testdaten
  X_test <- model.matrix(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                           years_in_top100 + max_dist_categories + max_disciplines + 
                           fina_16 + max_z_time_rate + z_time_ratio, data = fold_test)[, -1]
  
  # Wahrscheinlichkeiten berechnen und in Klassen konvertieren
  probabilities <- predict(log_reg_model, newx = X_test, s = best_lambda, type = "response")
  predictions <- ifelse(probabilities > 0.5, 1, 2)  # Schwellenwert 0.5
  
  # 9. Erstellen der Konfusionsmatrix
  confusion_matrix <- table(fold_test$group, predictions)
  print(paste("Konfusionsmatrix für Fold", i, " (Logistische Regression):"))
  print(confusion_matrix)
  
  # 10. Berechnen der Accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  accuracy_values[i] <- accuracy
  print(paste("Accuracy für Fold", i, ": ", round(accuracy, 4)))
  
  # 11. Berechnen von Precision, Recall und F1-Score
  precision <- diag(confusion_matrix) / colSums(confusion_matrix)
  recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Speichern von Precision, Recall und F1-Score für jeden Fold
  precision_values[[i]] <- precision
  recall_values[[i]] <- recall
  f1_scores[[i]] <- f1_score
  
  print(paste("Precision für Fold", i, ":"))
  print(precision)
  
  print(paste("Recall für Fold", i, ":"))
  print(recall)
  
  print(paste("F1-Score für Fold", i, ":"))
  print(f1_score)
}

# 12. Durchschnittswerte für Accuracy, Precision, Recall und F1-Score berechnen
mean_accuracy <- mean(accuracy_values)
mean_precision <- sapply(precision_values, mean)
mean_recall <- sapply(recall_values, mean)
mean_f1_score <- sapply(f1_scores, mean)

print(paste("Durchschnittliche Accuracy: ", round(mean_accuracy, 4)))
print(paste("Durchschnittliche Precision: ", round(mean(mean_precision), 4)))
print(paste("Durchschnittlicher Recall: ", round(mean(mean_recall), 4)))
print(paste("Durchschnittlicher F1-Score: ", round(mean(mean_f1_score), 4)))

# 13. Speichern der Ergebnisse
results <- data.frame(Fold = 1:5, Accuracy = accuracy_values)
results$Mean_Accuracy <- mean_accuracy
results$Mean_Precision <- mean(mean_precision)
results$Mean_Recall <- mean(mean_recall)
results$Mean_F1_Score <- mean(mean_f1_score)

write.csv(results, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/logistic_regression_results.csv", row.names = FALSE)
