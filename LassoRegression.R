# Notwendige Pakete laden
library(dplyr)
library(glmnet)

# 1. Laden des Datensatzes
data <- read.csv("/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset_processed.csv")

# 2. Definieren der Zielvariable basierend auf den FINA-Punkten im Alter von 18 Jahren (nur zwei Gruppen)
# FINA >= 750 -> Gruppe 1, FINA < 750 -> Gruppe 2
data <- data %>%
  group_by(unique_id, Event) %>%
  mutate(group = case_when(
    Fina >= 750 & Alter == 18 ~ 1,   # Gruppe 1: FINA >= 750
    Fina < 750 & Alter == 18 ~ 0,    # Gruppe 2: FINA < 750
    TRUE ~ NA_real_  # Setzt NA für alle anderen Altersgruppen
  )) %>%
  fill(group, .direction = "downup") %>%
  ungroup()

# 3. Entfernen von Zeilen mit NA in den gewählten Features und der Zielvariable
train_data_filtered <- data %>%
  filter(complete.cases(Frühestes_Alter, improvement_rate, max_yearly_improvement,
                        years_in_top100, max_dist_categories, max_disciplines, 
                        fina_16, max_z_time_rate, z_time_ratio, group))

# 4. Überprüfen der Anzahl der Beobachtungen nach Filterung
cat("Anzahl der Beobachtungen nach Filterung:", nrow(train_data_filtered), "\n")

# 5. Erstellen der Design-Matrix und der Zielvariable (y)
X_train <- model.matrix(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
                          years_in_top100 + max_dist_categories + max_disciplines + 
                          fina_16 + max_z_time_rate + z_time_ratio, 
                        data = train_data_filtered)[, -1]

y_train <- train_data_filtered$group

# 6. Überprüfen der Dimensionen von X_train und y_train
cat("Anzahl der Beobachtungen in X_train:", nrow(X_train), "\n")
cat("Anzahl der Beobachtungen in y_train:", length(y_train), "\n")

# 7. Lasso-Regression mit Kreuzvalidierung durchführen
# Lasso: alpha = 1, Kreuzvalidierung zur Bestimmung des besten Lambdas
lasso_model <- cv.glmnet(X_train, as.numeric(y_train), family = "binomial", alpha = 1)

# 8. Bestes Lambda aus der Kreuzvalidierung anzeigen
best_lambda <- lasso_model$lambda.min
cat("Bestes Lambda: ", best_lambda, "\n")

# 9. Training des finalen Lasso-Modells mit dem besten Lambda
final_model <- glmnet(X_train, as.numeric(y_train), family = "binomial", alpha = 1, lambda = best_lambda)

# 10. Vorhersagen auf den Testdaten (du müsstest das Testset vorbereiten, hier nur als Hinweis):
# X_test <- model.matrix(group ~ Frühestes_Alter + improvement_rate + max_yearly_improvement +
#                         years_in_top100 + max_dist_categories + max_disciplines + 
#                         fina_16 + max_z_time_rate + z_time_ratio, data = test_data)[, -1]
# predictions <- predict(final_model, newx = X_test, type = "response")

# 11. Anzeige der Koeffizienten des finalen Modells
lasso_coefficients <- coef(final_model)
print(lasso_coefficients)

# Optional: Speichern des Modells
save(final_model, file = "lasso_model.RData")


# Plot the cross-validation curve (MSE by lambda value)
plot(lasso_model)


# Plot the path of Lasso regression coefficients as lambda increases
plot(lasso_model$glmnet.fit, "lambda", label = TRUE)


