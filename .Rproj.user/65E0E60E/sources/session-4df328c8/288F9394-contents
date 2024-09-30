# 1. Laden der notwendigen Pakete
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(class)) {
  install.packages("class")
  library(class)
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
roc_curves <- list()  # Für die ROC-Kurven
auc_values <- c()

for (i in 1:5) {
  # Trainings- und Testdaten für den Fold erstellen
  fold_train <- data_clean[folds[[i]], ]
  fold_test <- data_clean[-folds[[i]], ]
  
  # Erstellen von Trainings- und Testdatensätzen
  x_train <- as.matrix(fold_train[, numeric_features])
  x_test <- as.matrix(fold_test[, numeric_features])
  y_train <- as.factor(fold_train$group)
  y_test <- as.factor(fold_test$group)
  
  # Kleine Störung zu den Trainings- und Testdaten hinzufügen, um Ties zu vermeiden
  x_train <- x_train + matrix(runif(n = length(x_train), min = -1e-6, max = 1e-6), nrow = nrow(x_train))
  x_test <- x_test + matrix(runif(n = length(x_test), min = -1e-6, max = 1e-6), nrow = nrow(x_test))
  
  # Normalisiere die Daten
  x_train <- scale(x_train)
  x_test <- scale(x_test)
  
  # KNN-Modell mit k = 10
  k <- 10
  knn_predictions <- knn(train = x_train, test = x_test, cl = y_train, k = k, prob = TRUE)
  
  # Wahrscheinlichkeiten für KNN ermitteln
  knn_probabilities <- attr(knn_predictions, "prob")
  if (is.null(knn_probabilities)) {
    print(paste("Fold", i, ": Keine Wahrscheinlichkeiten berechnet"))
    next  # Falls keine Wahrscheinlichkeiten berechnet werden, überspringe den Fold
  }
  
  # Wahrscheinlichkeiten zu Gruppe 2 (positiv) zuordnen
  knn_probabilities <- ifelse(knn_predictions == 2, knn_probabilities, 1 - knn_probabilities)
  
  # ROC-Kurve und AUC berechnen
  roc_curve <- roc(y_test, knn_probabilities, levels = c(1, 2), direction = "<")
  auc_value <- auc(roc_curve)
  
  # Speichern der ROC-Kurve und AUC-Werte
  roc_curves[[i]] <- roc_curve
  auc_values[i] <- auc_value
  
  # Vorhersagen als Klassen (1 oder 2)
  knn_class_predictions <- knn(train = x_train, test = x_test, cl = y_train, k = k)
  
  # Konfusionsmatrix
  confusion_matrix_knn <- table(y_test, knn_class_predictions)
  
  # Accuracy
  accuracy <- sum(diag(confusion_matrix_knn)) / sum(confusion_matrix_knn)
  accuracy_values[i] <- accuracy
  
  # Precision, Recall, F1-Score
  precision <- diag(confusion_matrix_knn) / colSums(confusion_matrix_knn)
  recall <- diag(confusion_matrix_knn) / rowSums(confusion_matrix_knn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Speichern der Metriken
  precision_values[[i]] <- precision
  recall_values[[i]] <- recall
  f1_scores[[i]] <- f1_score
}

# 7. Durchschnitts-ROC und Standardabweichung berechnen
fpr_grid <- seq(0, 1, length.out = 100)  # Gemeinsame FPR-Achse
tpr_interpolated <- matrix(0, nrow = length(roc_curves), ncol = length(fpr_grid))

for (i in 1:length(roc_curves)) {
  tpr_interpolated[i, ] <- approx(1 - roc_curves[[i]]$specificities, roc_curves[[i]]$sensitivities, xout = fpr_grid, ties = mean)$y
}

mean_tpr <- colMeans(tpr_interpolated)
std_tpr <- apply(tpr_interpolated, 2, sd)

# 8. Plotten der ROC-Kurven für jeden Fold und der Durchschnitts-ROC
# Grafik sowohl in R anzeigen als auch als Datei speichern
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/knn_roc_curve.png", 
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
                      cex = 0.8, plot = FALSE)

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
       bty = "o", cex = 0.8)

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
       bty = "o", cex = 0.8)


# 13. Optional: Feature Importance analysieren und anzeigen
importance(knn_probabilities)
# Anpassung der Ränder des Plots (unten, links, oben, rechts)
par(mar = c(5, 5, 4, 2))  # Passe die Werte an, um die Grafik zu verkleinern

# Erneutes Erstellen des varImpPlots
varImpPlot(knn_probabilities)

# Grafik in eine Datei speichern
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/feature_importance_knn.png", width = 800, height = 600)
varImpPlot(knn_probabilities)
dev.off()

# 10. Speichern der Tabelle mit AUC-Werten als CSV
results <- data.frame(Fold = 1:5, AUC = auc_values)
results$Mean_AUC <- mean(auc_values)
results$Std_AUC <- sd(auc_values)
results$Mean_Accuracy <- mean_accuracy
results$Sd_Accuracy <- sd(mean_accuracy)
results$Mean_Precision <- mean(mean_precision)
results$Mean_Recall <- mean(mean_recall)
results$Mean_F1_Score <- mean(mean_f1_score)
write.csv(results, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/knn_neu_results.csv", row.names = FALSE)
