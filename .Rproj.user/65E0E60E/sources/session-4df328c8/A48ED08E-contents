# Laden der Pakete dplyr und tidyr, falls noch nicht geladen
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

# Laden des bearbeiteten Datensatzes
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset_processed.csv")

# 1. Definieren der Zielvariable basierend auf den FINA-Punkten im Alter von 18 Jahren und Anwenden auf alle Kombinationen
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
print("Erste Zeilen nach Definition der Zielvariable:")
print(head(data))


# 2. Überprüfen auf fehlende Werte

# Überprüfen der Anzahl fehlender Werte in jeder Spalte
missing_data_summary <- colSums(is.na(data))
print("Anzahl der fehlenden Werte in jeder Spalte:")
print(missing_data_summary)

# 3. Entfernen von Zeilen mit fehlenden Werten
# Entfernen von Zeilen, bei denen 'group' NA ist
data_clean <- data %>% filter(!is.na(group))

# Überprüfung, wie viele Zeilen entfernt wurden
initial_row_count <- nrow(data)
cleaned_row_count <- nrow(data_clean)
removed_rows <- initial_row_count - cleaned_row_count

print(paste("Anzahl der entfernten Zeilen aufgrund fehlender Daten:", removed_rows))
print(paste("Verbleibende Zeilen nach dem Entfernen der fehlenden Daten:", cleaned_row_count))

#erneute Überprüfung:
# Überprüfen der Anzahl fehlender Werte in jeder Spalte
missing_data_summary <- colSums(is.na(data_clean))
print("Anzahl der fehlenden Werte in jeder Spalte:")
print(missing_data_summary)

# Ersetzen von fehlenden Werten in numerischen Variablen durch den Median der Spalte
data_clean <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Überprüfen der verbleibenden fehlenden Werte nach dem Ersetzen
missing_data_summary_after <- colSums(is.na(data_clean))
print("Anzahl der fehlenden Werte nach dem Ersetzen:")
print(missing_data_summary_after)


# 4. Normierung/Skalierung der numerischen Features

# Wählen der numerischen Features, die skaliert werden sollen
numeric_features <- c("Fina", "max_yearly_improvement", "max_disciplines", "max_dist_categories")

# Skalieren der numerischen Features
data_clean[numeric_features] <- scale(data_clean[numeric_features])

# Optional: Überprüfen der ersten Zeilen nach der Skalierung
print("Erste Zeilen nach der Skalierung der numerischen Features:")
print(head(data_clean))


# 5. Optional: One-Hot-Encoding für kategoriale Variablen (z.B. Event)

if("Event" %in% colnames(data_clean)) {
  # Erstellen der Dummy-Variablen für Event
  event_dummies <- model.matrix(~ Event + 0, data = data_clean) %>%
    as.data.frame()
  
  # Hinzufügen der Dummy-Variablen zum ursprünglichen Datensatz
  data_clean <- bind_cols(data_clean, event_dummies)
  
  # Entfernen der ursprünglichen Event-Spalte nach dem Encoding
  data_clean <- data_clean %>% select(-Event)
}

# Optional: Überprüfen der ersten Zeilen nach dem One-Hot-Encoding
print("Erste Zeilen nach dem One-Hot-Encoding:")
print(head(data_clean))


# 6. Aufteilen des Datensatzes in Trainings- und Testdaten
set.seed(123)  # Zufallsgenerator für Reproduzierbarkeit

# Aufteilen des Datensatzes in 70% Trainingsdaten und 30% Testdaten
split <- sample.split(data_clean$group, SplitRatio = 0.7)
train_data <- subset(data_clean, split == TRUE)
test_data <- subset(data_clean, split == FALSE)

# Überprüfen der Dimensionen der Trainings- und Testdaten
print(paste("Anzahl der Zeilen im Trainingsdatensatz:", nrow(train_data)))
print(paste("Anzahl der Zeilen im Testdatensatz:", nrow(test_data)))

# Optional: Speichern des bereinigten Datensatzes als CSV-Datei
write.csv(data_clean, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/cleaned_train_data.csv", row.names = FALSE)

print("Der bereinigte und aufgeteilte Datensatz wurde gespeichert.")

# Multinom Model ----------------------------------------------------------

# Laden des Pakets nnet, falls noch nicht geladen
if(!require(nnet)) {
  install.packages("nnet")
  library(nnet)
}

# Training des multinomialen logistischen Regressionsmodells
multinom_model <- multinom(group ~ ., data = train_data)

# Überprüfen der Modell-Performance auf den Trainingsdaten
summary(multinom_model)

# Optional: Speichern des trainierten Modells
saveRDS(multinom_model, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/multinom_model.rds")


# Evaluierung -------------------------------------------------------------

# Vorhersagen auf den Testdaten
predictions <- predict(multinom_model, newdata = test_data)

# Erstellen der Konfusionsmatrix
confusion_matrix <- table(test_data$group, predictions)
print("Konfusionsmatrix:")
print(confusion_matrix)

# Berechnen der Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))

# Berechnen von Precision, Recall und F1-Score
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

print("Precision für jede Gruppe:")
print(precision)
print("Recall für jede Gruppe:")
print(recall)
print("F1-Score für jede Gruppe:")
print(f1_score)


