# 1. Laden der notwendigen Pakete
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(e1071)) {
  install.packages("e1071")  # Enthält Naive Bayes
  library(e1071)
}

if(!require(pROC)) {
  install.packages("pROC")
  library(pROC)
}

if(!require(caret)) {
  install.packages("caret")
  library(caret)
}

if(!require(doParallel)) {
  install.packages("doParallel")
  library(doParallel)
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

# 6. Paralleles Rechnen einrichten
num_cores <- detectCores() - 1  # Nutze n-1 Kerne
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# 7. Folds parallel durchlaufen und ROC-Kurven erstellen
roc_curves <- vector("list", 5)  # Initialisiere eine leere Liste mit 5 Elementen
auc_values <- foreach(i = 1:5, .combine = c, .packages = c('e1071', 'pROC', 'dplyr')) %dopar% {
  
  # Trainings- und Testdaten für den Fold erstellen
  fold_train <- data_clean[folds[[i]], ]
  fold_test <- data_clean[-folds[[i]], ]
  
  # Naive Bayes Modell trainieren
  nb_model <- naiveBayes(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                           years_in_top100 + max_dist_categories + max_disciplines + 
                           fina_16 + max_z_time_rate + z_time_ratio, 
                         data = fold_train)
  
  # Vorhersagen auf Wahrscheinlichkeitsbasis für den aktuellen Fold
  nb_probabilities <- predict(nb_model, newdata = fold_test, type = "raw")
  
  # ROC-Kurve und AUC für Gruppe 1 vs. Gruppe 2
  roc_curve <- roc(fold_test$group, nb_probabilities[,2], levels = c(1, 2), direction = "<")
  auc_value <- auc(roc_curve)
  
  # Speichern der ROC-Kurve und AUC-Werte
  roc_curves[[i]] <- roc_curve
  
  return(auc_value)
}

# Stoppe den Cluster
stopCluster(cl)

# 8. Durchschnitts-ROC und Standardabweichung berechnen
fpr_grid <- seq(0, 1, length.out = 100)  # Gemeinsame FPR-Achse
tpr_interpolated <- matrix(0, nrow = length(roc_curves), ncol = length(fpr_grid))

valid_curves <- 0  # Zähler für gültige ROC-Kurven

for (i in 1:length(roc_curves)) {
  # Überprüfen, ob genügend Daten für die Interpolation vorhanden sind
  if (length(roc_curves[[i]]$specificities) > 1 && length(roc_curves[[i]]$sensitivities) > 1) {
    # Interpolieren der TPR-Werte auf dem gemeinsamen FPR-Gitter (verwende 1 - Specificities)
    tpr_interpolated[i, ] <- approx(1 - roc_curves[[i]]$specificities, roc_curves[[i]]$sensitivities, xout = fpr_grid, ties = mean)$y
    valid_curves <- valid_curves + 1  # Zähler erhöhen
  } else {
    print(paste("Fold", i, "hat keine gültige ROC-Kurve"))
  }
}

if (valid_curves > 0) {
  # Durchschnitt und Standardabweichung der TPR-Werte berechnen, wenn gültige ROC-Kurven vorhanden sind
  mean_tpr <- colMeans(tpr_interpolated[1:valid_curves, ], na.rm = TRUE)
  std_tpr <- apply(tpr_interpolated[1:valid_curves, ], 2, sd, na.rm = TRUE)
} else {
  print("Keine gültigen ROC-Kurven vorhanden, keine Berechnung möglich.")
}

# 9. Plotten der ROC-Kurven für jeden Fold und der Durchschnitts-ROC
png("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/naive_bayes_roc_curve.png", 
    width = 6.5, height = 4.5, units = "in", res = 300)

plot(fpr_grid, mean_tpr, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)",
     xlim = c(0, 1), ylim = c(0, 1))

# Füge die ROC-Kurven der einzelnen Folds hinzu
cols <- c("red", "green", "cyan", "purple", "pink")
for (i in 1:5) {
  if (!is.null(roc_curves[[i]])) {
    lines(1 - roc_curves[[i]]$specificities, roc_curves[[i]]$sensitivities, col = cols[i], lwd = 2)
  }
}

# Hinzufügen der Standardabweichung (grau schattiert)
polygon(c(fpr_grid, rev(fpr_grid)), c(mean_tpr + std_tpr, rev(mean_tpr - std_tpr)), col = adjustcolor("gray", alpha.f = 0.3), border = NA)

# Hinzufügen der Chance Line (rote gestrichelte Linie)
abline(a = 0, b = 1, col = "red", lty = 2)

legend("bottomright", legend = c(paste("ROC fold 1 (AUC = ", round(auc_values[1], 2), ")", sep = ""),
                                 paste("ROC fold 2 (AUC = ", round(auc_values[2], 2), ")", sep = ""),
                                 paste("ROC fold 3 (AUC = ", round(auc_values[3], 2), ")", sep = ""),
                                 paste("ROC fold 4 (AUC = ", round(auc_values[4], 2), ")", sep = ""),
                                 paste("ROC fold 5 (AUC = ", round(auc_values[5], 2), ")", sep = ""),
                                 "Mean ROC", "Chance Line"),
       col = c("red", "green", "cyan", "purple", "pink", "blue", "red"), lwd = 2, lty = c(1, 1, 1, 1, 1, 1, 2),
       cex = 0.8)

dev.off()

# 10. Speichern der Tabelle mit AUC-Werten als CSV
results <- data.frame(Fold = 1:5, AUC = auc_values)
results$Mean_AUC <- mean(auc_values)
results$Std_AUC <- sd(auc_values)
write.csv(results, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/Ergebnisse/naive_bayes_results.csv", row.names = FALSE)
