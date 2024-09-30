# Installieren und Laden der benötigten Pakete
install.packages(c("tidyverse", "caret"))
library(tidyverse)
library(caret)

# Daten einlesen
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Daten/100_F_m/Endergebnis_with_Rudolph.csv")

# Doppelte Einträge entfernen
data <- data %>% distinct(name, Alter, Zeit, .keep_all = TRUE)

# Erstellen einer kombinierten Spalte aus 'name' und 'Jahrgang'
data$unique_id <- paste(data$name, data$Jahrgang, sep="_")

# Generiere die Zeit im Alter von 18 Jahren für jeden eindeutigen Schwimmer
time_at_18 <- data %>% filter(Alter == 18) %>% select(unique_id, Zeit) %>% distinct()
data <- left_join(data, time_at_18, by = "unique_id", suffix = c("", "_at_18"))


duplicate_18 <- data %>% 
  filter(Alter == 18) %>% 
  group_by(name) %>% 
  count() %>% 
  filter(n > 1)

print(duplicate_18)




# Filtere die Daten, um nur die Zeilen zu behalten, für die wir die Zeit im Alter von 18 kennen
filtered_data <- data %>% filter(!is.na(Zeit_at_18))

# Features und Zielvariable definieren
X <- filtered_data %>% select(Alter, Zeit)
y <- filtered_data$Zeit_at_18

# Daten aufteilen in Trainings- und Testdaten
set.seed(42)
splitIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[splitIndex, ]
y_train <- y[splitIndex]
X_test <- X[-splitIndex, ]
y_test <- y[-splitIndex]

# Lineares Regressionsmodell trainieren
training_data <- data.frame(Zeit_at_18=y_train, X_train)
model <- lm(Zeit_at_18 ~ ., data = training_data)

# Modell-Performance auf Testdaten
predictions <- predict(model, newdata = X_test)
mse <- mean((predictions - y_test)^2)

# Ausgabe des MSE
print(paste("Mean Squared Error:", mse))
