# Laden der benötigten Bibliotheken
library(dplyr)
library(caret)
library(gbm)
library(caTools)

# Daten laden
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"
data <- read.csv(file_path)

# Daten filtern für die Disziplin "100 F"
data_100F <- data %>%
  filter(Streckenlänge == 100 & Disziplin == "F")

# Berechnung der prozentualen Verbesserung gegenüber dem Vorjahr
data_100F <- data_100F %>%
  arrange(name, Jahr) %>%
  group_by(name) %>%
  mutate(percent_improvement = (Zeit - lag(Zeit)) / lag(Zeit) * 100) %>%
  filter(!is.na(percent_improvement))

# Auswahl der relevanten Merkmale und der Zielvariable
features <- c('Frühestes_Alter', 'Anzahl_Disziplinen', 'Anzahl_Distanz_Kategorien',
              'Anzahl_Einzigartige_Altersgruppen', 'years_in_top100', 'percent_diff_improvement_11y')
target <- 'percent_improvement'

# Datensatz auf relevante Merkmale und Zielvariable reduzieren
data_relevant <- data_100F %>%
  select(all_of(features), all_of(target), Alter)

# Fehlende Werte in der Spalte "percent_diff_improvement_11y" durch den Medianwert ersetzen
median_value <- median(data_relevant$percent_diff_improvement_11y, na.rm = TRUE)
data_relevant$percent_diff_improvement_11y[is.na(data_relevant$percent_diff_improvement_11y)] <- median_value

# Überprüfen der Daten
head(data_relevant)
median_value

# Daten aufteilen
set.seed(123)
split <- sample.split(data_relevant$percent_improvement, SplitRatio = 0.8)
train_data <- subset(data_relevant, split == TRUE)
test_data <- subset(data_relevant, split == FALSE)

# Überprüfen der Dimensionen der aufgeteilten Datensätze
dim(train_data)
dim(test_data)

# Modelltraining
gbm_model <- gbm(percent_improvement ~ Frühestes_Alter + Anzahl_Disziplinen + Anzahl_Distanz_Kategorien +
                   Anzahl_Einzigartige_Altersgruppen + years_in_top100 + percent_diff_improvement_11y + Alter, 
                 data = train_data, 
                 distribution = "gaussian", 
                 n.trees = 100, 
                 interaction.depth = 3, 
                 shrinkage = 0.1, 
                 cv.folds = 5)

# Optimale Anzahl der Bäume basierend auf Kreuzvalidierung
best_trees <- gbm.perf(gbm_model, method = "cv")

# Vorhersagen auf dem Testdatensatz durchführen
predictions <- predict(gbm_model, newdata = test_data, n.trees = best_trees)

# Ausgabe der Vorhersagen
head(predictions)

# Beispielhafte Eingabedaten für einen Schwimmer mit 14 Jahren
current_age <- 16
current_time <- 50.7  # Beispielzeit des Schwimmers im Alter von 14 Jahren

# Werte, die du als 14-Jähriger kennst
new_data <- data.frame(
  Alter = c(17),  # Zukünftiges Alter, für das die Vorhersage gemacht wird
  Frühestes_Alter = rep(11, 1),  # Alter, in dem der Schwimmer erstmals in den Top 100 war
  Anzahl_Disziplinen = rep(5, 1),  # Anzahl der Disziplinen in den Top 100 mit 14 Jahren
  Anzahl_Distanz_Kategorien = rep(3, 1),  # Anzahl der Distanzkategorien in den Top 100 mit 14 Jahren
  Anzahl_Einzigartige_Altersgruppen = rep(8, 1),  # Anzahl der Jahre, in denen der Schwimmer in den Top 100 war
  years_in_top100 = rep(8, 1),  # Anzahl der Jahre in den Top 100 für die aktuelle Disziplin
  percent_diff_improvement_11y = rep(5.1, 1)  # Unterschied zur allgemeinen Verbesserung im Vergleich zu 11 Jahren
)

# Vorhersage der prozentualen Verbesserung für das Alter 16
predicted_improvement <- predict(gbm_model, newdata = new_data, n.trees = best_trees)

# Berechnung der zukünftigen Schwimmzeit basierend auf der aktuellen Zeit und der vorhergesagten prozentualen Verbesserung
future_time <- current_time + (current_time * predicted_improvement / 100)

# Ausgabe der zukünftigen Schwimmzeit
data.frame(
  Alter = new_data$Alter,
  Vorhergesagte_Zeit = future_time
)
