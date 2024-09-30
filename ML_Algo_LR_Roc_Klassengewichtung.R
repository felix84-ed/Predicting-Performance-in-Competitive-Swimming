# 1. Laden der notwendigen Pakete
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if(!require(nnet)) {
  install.packages("nnet")
  library(nnet)
}

if(!require(pROC)) {
  install.packages("pROC")
  library(pROC)
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

# 4. Entfernen von Zeilen mit fehlenden Gruppenwerten
data_clean <- data %>% filter(!is.na(group))

# 5. Behandlung fehlender Werte und Skalierung der numerischen Features (ohne Fina)
numeric_features <- c("Frühestes_Alter", "improvement_rate", "max_yearly_improvement", 
                      "years_in_top100", "max_dist_categories", "max_disciplines", 
                      "fina_16", "max_z_time_rate", "z_time_ratio")

data_clean[numeric_features] <- data_clean[numeric_features] %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Wandle die Zielvariable in einen Faktor um
data_clean$group <- as.factor(data_clean$group)

# 6. Aufteilen des Datensatzes in Trainings- und Testdaten (70% Training, 30% Test)
set.seed(123)
split <- sample.split(data_clean$group, SplitRatio = 0.7)
train_data <- subset(data_clean, split == TRUE)
test_data <- subset(data_clean, split == FALSE)

# 7. Klassengewichtung basierend auf der Anzahl der Beispiele pro Klasse
class_weights <- table(train_data$group)
class_weights <- max(class_weights) / class_weights

# 8. Training des logistischen Regressionsmodells mit Klassengewichten
logistic_model <- multinom(
  group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
    years_in_top100 + max_dist_categories + max_disciplines + 
    fina_16 + max_z_time_rate + z_time_ratio, 
  data = train_data, 
  weights = class_weights[train_data$group]  # Klassengewichte anwenden
)

# 9. Überprüfen der Modell-Performance
summary(logistic_model)

# 10. Vorhersagen auf den Testdaten
predictions <- predict(logistic_model, newdata = test_data)

# 11. Erstellen der Konfusionsmatrix
confusion_matrix <- table(test_data$group, predictions)

print("Konfusionsmatrix:")
print(confusion_matrix)

# 12. Berechnen der Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))

# 13. Berechnen von Precision, Recall und F1-Score
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

print("Precision für jede Gruppe:")
print(precision)
print("Recall für jede Gruppe:")
print(recall)
print("F1-Score für jede Gruppe:")
print(f1_score)

# ROC-Kurve für die logistische Regression erstellen
# 1. Vorhersagen auf Wahrscheinlichkeitsbasis für das Logistic Regression Modell
logistic_probabilities <- predict(logistic_model, newdata = test_data, type = "probs")

# Überprüfen, ob logistic_probabilities ein Vektor oder eine Matrix ist
if (is.vector(logistic_probabilities)) {
  # Wenn es ein Vektor ist, nehmen wir diesen direkt für die ROC-Kurve
  roc_curve_logistic <- roc(test_data$group, logistic_probabilities, levels = c(1, 2), direction = "<")
} else if (is.matrix(logistic_probabilities) || is.data.frame(logistic_probabilities)) {
  # Wenn es eine Matrix oder ein DataFrame ist, nehmen wir die Wahrscheinlichkeiten der ersten Spalte
  roc_curve_logistic <- roc(test_data$group, logistic_probabilities[,1], levels = c(1, 2), direction = "<")
} else {
  stop("Unerwarteter Datentyp für logistic_probabilities.")
}

# 2. Extrahieren von True Positive Rate (Sensitivity) und False Positive Rate (1 - Specificity)
true_positive_rate_logistic <- roc_curve_logistic$sensitivities
false_positive_rate_logistic <- 1 - roc_curve_logistic$specificities

# 3. Plotten der ROC-Kurve: True Positive Rate gegen False Positive Rate
plot(false_positive_rate_logistic, true_positive_rate_logistic, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)",
     main = "ROC Curve for Logistic Regression (Group 1 vs Group 2)")

# 4. Berechnen und Anzeigen des AUC-Werts
auc_value_logistic <- auc(roc_curve_logistic)
print(paste("AUC-Wert: ", round(auc_value_logistic, 4)))

# 5. Optional: Hinzufügen einer Diagonalen als Referenzlinie (Chance Line)
abline(a = 0, b = 1, col = "red", lty = 2)
legend("bottomright", legend = c("ROC Curve", "Chance Line"), col = c("blue", "red"), lwd = 2, lty = 1:2)

