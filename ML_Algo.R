# Laden der Pakete dplyr und tidyr, falls noch nicht geladen
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
} else {
  library(tidyr)
}

# Laden des bearbeiteten Datensatzes
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset_processed.csv")

# Group18 -----------------------------------------------------------------

# Definieren der Zielvariable basierend auf den FINA-Punkten im Alter von 18 Jahren und Anwenden auf alle Kombinationen
data <- data %>%
  group_by(unique_id, Event) %>%
  mutate(group = case_when(
    Fina >= 750 & Alter == 18 ~ 1,
    Fina >= 650 & Fina < 750 & Alter == 18 ~ 2,
    Fina < 650 & Alter == 18 ~ 3,
    TRUE ~ NA_real_  # Setzt NA für alle anderen Altersgruppen
  )) %>%
  fill(group, .direction = "downup") %>%  # Füllt die Gruppenzugehörigkeit für alle Kombinationen aus ID und Event
  ungroup()

# Optional: Überprüfen der ersten Zeilen des Datensatzes
print(head(data))


# Kategorisierung ---------------------------------------------------------

# Laden der Pakete dplyr und tidyr, falls noch nicht geladen
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

# Überprüfen auf fehlende Werte und optionales Behandeln
print(sum(is.na(data)))

# Option 1: Entfernen von Zeilen mit fehlenden Werten
data_clean <- na.omit(data)

# Option 2: Ersetzen von fehlenden Werten (z.B. durch den Mittelwert)
data_clean <- data_clean %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Normierung/Skalierung der numerischen Features
numeric_features <- c("Fina", "max_yearly_improvement", "max_disciplines", "max_dist_categories")  # Wähle die relevanten numerischen Features aus
data_clean[numeric_features] <- scale(data_clean[numeric_features])

# One-Hot-Encoding von kategorialen Variablen (z.B. Event) mit `model.matrix`
if("Event" %in% colnames(data_clean)) {
  event_dummies <- model.matrix(~ Event + 0, data = data_clean) %>%  # Erstellen der Dummy-Variablen
    as.data.frame()  # Umwandeln in DataFrame
  data_clean <- bind_cols(data_clean, event_dummies)  # Hinzufügen der Dummy-Variablen zu den ursprünglichen Daten
}

# Optional: Entfernen der ursprünglichen Event-Spalte nach Encoding
data_clean <- data_clean %>%
  select(-Event)

# Überprüfen der vorbereiteten Daten
print(head(data_clean))

# TrainingTest ------------------------------------------------------------
# Laden des Pakets caTools, falls noch nicht geladen
if(!require(caTools)) {
  install.packages("caTools")
  library(caTools)
} else {
  library(caTools)
}

# Setzen des Zufallsgenerators für Reproduzierbarkeit
set.seed(123)  # Du kannst eine beliebige Zahl verwenden

# Aufteilen des Datensatzes in Trainings- und Testdaten (z.B. 70% Training, 30% Test)
split <- sample.split(data_clean$group, SplitRatio = 0.7)
train_data <- subset(data_clean, split == TRUE)
test_data <- subset(data_clean, split == FALSE)

# Optional: Überprüfen der Dimensionen der aufgeteilten Datensätze
print(dim(train_data))
print(dim(test_data))


# Logistische Regression --------------------------------------------------
# Laden des Pakets nnet, falls noch nicht geladen
if(!require(nnet)) {
  install.packages("nnet")
  library(nnet)
} else {
  library(nnet)
}

# Training des multinomialen logistischen Regressionsmodells
multinom_model <- multinom(group ~ ., data = train_data)

# Überprüfen der Modell-Performance auf den Trainingsdaten
summary(multinom_model)

# Vorhersagen auf den Testdaten
predictions <- predict(multinom_model, newdata = test_data)

# Überprüfung der Vorhersagen mit einer Konfusionsmatrix
confusion_matrix <- table(test_data$group, predictions)
print(confusion_matrix)

# Berechnen der Accuracy auf den Testdaten
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))

# Überprüfen der Struktur des Trainingsdatensatzes
print(str(train_data))

# Überprüfen der Anzahl der Koeffizienten und Variablen im Modell
print(colnames(coefficients))
print(length(all_variables))

# Überprüfen der Modellzusammenfassung
summary(multinom_model)

# Tabelle erstellen -------------------------------------------------------
# Alle verwendeten Prädiktoren extrahieren
used_variables <- colnames(coefficients)

# Berechnung der Z-Werte
z_values <- coefficients / std_errors

# Berechnung der p-Werte
p_values <- 2 * pnorm(-abs(z_values))

# Berechnung der Odds Ratios (e^B)
odds_ratios <- exp(coefficients)

# Berechnung der Konfidenzintervalle für die Koeffizienten (B)
confint_lower <- coefficients - 1.96 * std_errors
confint_upper <- coefficients + 1.96 * std_errors

# Berechnung der Konfidenzintervalle für die Odds Ratios (e^B)
confint_lower_odds <- exp(confint_lower)
confint_upper_odds <- exp(confint_upper)

# Erstellen der Tabelle
results_table <- data.frame(
  Variable = used_variables,
  
  Coefficient_Group2 = coefficients[, 1],
  StdError_Group2 = std_errors[, 1],
  ZValue_Group2 = z_values[, 1],
  PValue_Group2 = p_values[, 1],
  OddsRatio_Group2 = odds_ratios[, 1],
  CI_Lower_Odds_Group2 = confint_lower_odds[, 1],
  CI_Upper_Odds_Group2 = confint_upper_odds[, 1],
  
  Coefficient_Group3 = coefficients[, 2],
  StdError_Group3 = std_errors[, 2],
  ZValue_Group3 = z_values[, 2],
  PValue_Group3 = p_values[, 2],
  OddsRatio_Group3 = odds_ratios[, 2],
  CI_Lower_Odds_Group3 = confint_lower_odds[, 2],
  CI_Upper_Odds_Group3 = confint_upper_odds[, 2]
)

# Überprüfen, ob alle Variablen korrekt dargestellt wurden
print(results_table)

# Speichern der Tabelle als CSV-Datei
write.csv(results_table, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/multinom_results_detailed_table.csv", row.names = FALSE)
