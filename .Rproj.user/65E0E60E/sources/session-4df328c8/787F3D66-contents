# 1. Laden der Pakete
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

# 2. Laden des Datensatzes
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

# 4. Entfernen von Zeilen mit fehlenden Gruppenwerten
data_clean <- data %>% filter(!is.na(group))

# 5. Behandlung fehlender Werte und Skalierung der numerischen Features (ohne Fina)
numeric_features <- c("Frühestes_Alter", "improvement_rate", "max_yearly_improvement", 
                      "years_in_top100", "max_dist_categories", "max_disciplines", "fina_16",
                      "max_z_time_rate", "z_time_ratio")
data_clean[numeric_features] <- scale(data_clean[numeric_features])

# 6. One-Hot-Encoding für kategoriale Variablen (z.B. Event)
if("Event" %in% colnames(data_clean)) {
  event_dummies <- model.matrix(~ Event + 0, data = data_clean) %>% as.data.frame()
  data_clean <- bind_cols(data_clean, event_dummies)
  data_clean <- data_clean %>% select(-Event)
}

# 7. Aufteilen des Datensatzes in Trainings- und Testdaten (70% Training, 30% Test)
set.seed(123)
split <- sample.split(data_clean$group, SplitRatio = 0.7)
train_data <- subset(data_clean, split == TRUE)
test_data <- subset(data_clean, split == FALSE)

# 8. Training des logistischen Regressionsmodells (ohne Fina)
logistic_model <- multinom(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                             years_in_top100 + max_dist_categories + max_disciplines + fina_16 +
                             max_z_time_rate + z_time_ratio, data = train_data)

# 9. Überprüfen der Modell-Performance
summary(logistic_model)


print("Recall für jede Gruppe:")
print(recall)
print("F1-Score für jede Gruppe:")
print(f1_score)
