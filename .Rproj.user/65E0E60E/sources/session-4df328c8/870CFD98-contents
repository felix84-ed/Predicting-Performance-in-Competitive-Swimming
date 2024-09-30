# Notwendige Pakete laden
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
                      "fina_16", "z_time_ratio", "jaehrliche_verbesserung_prozent")

data_clean[numeric_features] <- data_clean[numeric_features] %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Wandle die Zielvariable in einen Faktor um
data_clean$group <- as.factor(data_clean$group)

# 5. Stratifizierte Folds erstellen (sicherstellen, dass jede Gruppe in den Folds vertreten ist)
set.seed(123)
folds <- createFolds(data_clean$group, k = 5, list = TRUE, returnTrain = TRUE)

# 6. Klassengewichtung basierend auf der Anzahl der Beispiele pro Klasse
class_weights <- table(data_clean$group)
class_weights <- max(class_weights) / class_weights  # Umgekehrte Gewichtung


# 7. Folds durchlaufen und ROC-Kurven erstellen
roc_curves <- list()
auc_values <- c()
accuracy_values <- c()
precision_values <- list()
recall_values <- list()
f1_scores <- list()

for (i in 1:5) {
  # Trainings- und Testdaten für den Fold erstellen
  fold_train <- data_clean[folds[[i]], ]
  fold_test <- data_clean[-folds[[i]], ]
  
  # Random Forest mit Klassengewichtung trainieren
  rf_model <- randomForest(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                             years_in_top100 + max_dist_categories + max_disciplines + 
                             fina_16 + z_time_ratio, 
                           data = fold_train, importance = TRUE, classwt = class_weights)
  
  # Vorhersagen auf Wahrscheinlichkeitsbasis für den aktuellen Fold
  rf_probabilities <- predict(rf_model, newdata = fold_test, type = "prob")
  
  # ROC-Kurve und AUC für Gruppe 1 vs. Gruppe 2
  roc_curve <- roc(fold_test$group, rf_probabilities[, 2], levels = c(1, 2), direction = "<")
  auc_values[i] <- auc(roc_curve)
  
  # Speichern der ROC-Kurve
  roc_curves[[i]] <- roc_curve
  
  # 10. Vorhersagen für die Testdaten (Klassen)
  rf_class_predictions <- predict(rf_model, newdata = fold_test)  # Vorhersage der Klassen (1 oder 2)
  
  # 11. Erstellen der Konfusionsmatrix
  confusion_matrix_rf <- table(fold_test$group, rf_class_predictions)
  print(paste("Konfusionsmatrix für Fold", i, " (Random Forest):"))
  print(confusion_matrix_rf)
  
  # 12. Berechnen der Accuracy
  accuracy <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)
  accuracy_values[i] <- accuracy
  print(paste("Accuracy für Fold", i, ": ", round(accuracy, 4)))
  
  # 13. Berechnen von Precision, Recall und F1-Score
  precision <- diag(confusion_matrix_rf) / colSums(confusion_matrix_rf)
  recall <- diag(confusion_matrix_rf) / rowSums(confusion_matrix_rf)
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
# 8. Durchschnittswerte für Accuracy, Precision, Recall und F1-Score berechnen
mean_accuracy <- mean(accuracy_values)
mean_precision <- sapply(precision_values, mean)
mean_recall <- sapply(recall_values, mean)
mean_f1_score <- sapply(f1_scores, mean)

print(paste("Durchschnittliche Accuracy: ", round(mean_accuracy, 4)))
print(paste("Durchschnittliche Precision: ", round(mean(mean_precision), 4)))
print(paste("Durchschnittlicher Recall: ", round(mean(mean_recall), 4)))
print(paste("Durchschnittlicher F1-Score: ", round(mean(mean_f1_score), 4)))

# 8. Durchschnitts-ROC und Standardabweichung berechnen
fpr_grid <- seq(0, 1, length.out = 100)  # Gemeinsame FPR-Achse
tpr_interpolated <- matrix(0, nrow = 5, ncol = length(fpr_grid))

for (i in 1:5) {
  # Interpolieren der TPR-Werte auf dem gemeinsamen FPR-Gitter (verwende 1 - Specificities)
  tpr_interpolated[i, ] <- approx(1 - roc_curves[[i]]$specificities, roc_curves[[i]]$sensitivities, xout = fpr_grid, ties = mean)$y
}

# Durchschnitt und Standardabweichung der TPR-Werte berechnen
mean_tpr <- colMeans(tpr_interpolated)
std_tpr <- apply(tpr_interpolated, 2, sd)

# 9. Plotten der ROC-Kurven für jeden Fold und der Durchschnitts-ROC
# Grafik sowohl in R anzeigen als auch als Datei speichern
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/random_forest_roc_curve.png", 
    width = 6.5, height = 4.5, units = "in", res = 300)

# Zeichne die ROC-Kurven und die Durchschnitts-ROC
plot(fpr_grid, mean_tpr, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)",
     xlim = c(0, 1), ylim = c(0, 1))

# Füge die ROC-Kurven der einzelnen Folds hinzu
cols <- c("red", "green", "cyan", "purple", "pink")
for (i in 1:5) {
  lines(1 - roc_curves[[i]]$specificities, roc_curves[[i]]$sensitivities, col = adjustcolor(cols[i], alpha.f = 0.4))
}

# Hinzufügen der Standardabweichung (grau schattiert)
polygon(c(fpr_grid, rev(fpr_grid)), c(mean_tpr + std_tpr, rev(mean_tpr - std_tpr)), col = adjustcolor("gray", alpha.f = 0.3), border = NA)

# Hinzufügen der Chance Line (rote gestrichelte Linie)
abline(a = 0, b = 1, col = "red", lty = 2)

# Bestimme die Position der Legende ohne sie zu zeichnen
legend_info <- legend("bottomright", legend = c(paste("ROC fold 1 (AUC = ", round(auc_values[1], 2), ")", sep = ""),
                                                paste("ROC fold 2 (AUC = ", round(auc_values[2], 2), ")", sep = ""),
                                                paste("ROC fold 3 (AUC = ", round(auc_values[3], 2), ")", sep = ""),
                                                paste("ROC fold 4 (AUC = ", round(auc_values[4], 2), ")", sep = ""),
                                                paste("ROC fold 5 (AUC = ", round(auc_values[5], 2), ")", sep = ""),
                                                "Mean ROC", "Chance Line"),
                      col = c("red", "green", "cyan", "purple", "pink", "blue", "red"), lwd = 2, lty = c(1, 1, 1, 1, 1, 1, 2),
                      cex = 0.8,  # Kleinere Schriftgröße
                      plot = FALSE)

# Zeichne ein halbtransparentes Rechteck hinter die Legende
rect(legend_info$rect$left, legend_info$rect$top, legend_info$rect$left + legend_info$rect$w, legend_info$rect$top - legend_info$rect$h, 
     col = adjustcolor("white", alpha.f = 0.5), border = NA)

# Zeichne die Legende mit einem schwarzen Rahmen und kleineren Schriften
legend("bottomright", legend = c(paste("ROC fold 1 (AUC = ", round(auc_values[1], 2), ")", sep = ""),
                                 paste("ROC fold 2 (AUC = ", round(auc_values[2], 2), ")", sep = ""),
                                 paste("ROC fold 3 (AUC = ", round(auc_values[3], 2), ")", sep = ""),
                                 paste("ROC fold 4 (AUC = ", round(auc_values[4], 2), ")", sep = ""),
                                 paste("ROC fold 5 (AUC = ", round(auc_values[5], 2), ")", sep = ""),
                                 "Mean ROC", "Chance Line"),
       col = c("red", "green", "cyan", "purple", "pink", "blue", "red"), lwd = 2, lty = c(1, 1, 1, 1, 1, 1, 2),
       bty = "o",  # Zeichnet einen schwarzen Rahmen um die Legende
       cex = 0.8)  # Kleinere Schriftgröße

# Schließe das PNG-Gerät und speichere die Datei
dev.off()

# Jetzt die gleiche Grafik in R anzeigen
plot(fpr_grid, mean_tpr, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)",
     xlim = c(0, 1), ylim = c(0, 1))

# Füge die ROC-Kurven der einzelnen Folds hinzu
for (i in 1:5) {
  lines(1 - roc_curves[[i]]$specificities, roc_curves[[i]]$sensitivities, col = adjustcolor(cols[i], alpha.f = 0.4))
}

# Hinzufügen der Standardabweichung (grau schattiert)
polygon(c(fpr_grid, rev(fpr_grid)), c(mean_tpr + std_tpr, rev(mean_tpr - std_tpr)), col = adjustcolor("gray", alpha.f = 0.3), border = NA)

# Hinzufügen der Chance Line (rote gestrichelte Linie)
abline(a = 0, b = 1, col = "red", lty = 2)

# Zeichne ein halbtransparentes Rechteck hinter die Legende
rect(legend_info$rect$left, legend_info$rect$top, legend_info$rect$left + legend_info$rect$w, legend_info$rect$top - legend_info$rect$h, 
     col = adjustcolor("white", alpha.f = 0.5), border = NA)

# Zeichne die Legende mit einem schwarzen Rahmen und kleineren Schriften
legend("bottomright", legend = c(paste("ROC fold 1 (AUC = ", round(auc_values[1], 2), ")", sep = ""),
                                 paste("ROC fold 2 (AUC = ", round(auc_values[2], 2), ")", sep = ""),
                                 paste("ROC fold 3 (AUC = ", round(auc_values[3], 2), ")", sep = ""),
                                 paste("ROC fold 4 (AUC = ", round(auc_values[4], 2), ")", sep = ""),
                                 paste("ROC fold 5 (AUC = ", round(auc_values[5], 2), ")", sep = ""),
                                 "Mean ROC", "Chance Line"),
       col = c("red", "green", "cyan", "purple", "pink", "blue", "red"), lwd = 2, lty = c(1, 1, 1, 1, 1, 1, 2),
       bty = "o",  # Zeichnet einen schwarzen Rahmen um die Legende
       cex = 0.8)  # Kleinere Schriftgröße

# 10. Speichern der Tabelle mit AUC-Werten als CSV
results <- data.frame(Fold = 1:5, AUC = auc_values)
results$Mean_AUC <- mean(auc_values)
results$Std_AUC <- sd(auc_values)
results$Mean_Accuracy <- mean_accuracy
results$Mean_Precision <- mean(mean_precision)
results$Mean_Recall <- mean(mean_recall)
results$Mean_F1_Score <- mean(mean_f1_score)
write.csv(results, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/random_forest_results.csv", row.names = FALSE)

# 13. Optional: Feature Importance analysieren und anzeigen
importance(rf_model)
# Anpassung der Ränder des Plots (unten, links, oben, rechts)
par(mar = c(5, 5, 4, 2))  # Passe die Werte an, um die Grafik zu verkleinern

# Erneutes Erstellen des varImpPlots
varImpPlot(rf_model)

# Grafik in eine Datei speichern
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/feature_importance_Rf.png", width = 800, height = 600)
varImpPlot(rf_model)
dev.off()
