# Installiere und lade notwendige Pakete
install.packages("lme4")
library(lme4)

# Definiere den Pfad zur CSV-Datei
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/gesamt_daten_bereinigt.csv"

# Lese die CSV-Datei ein
data <- read.csv(file_path)

# Erstelle eine Liste aller einzigartigen Kombinationen von Streckenlänge und Disziplin
combinations <- unique(data[, c("Streckenlänge", "Disziplin")])

# Leere DataFrame für die Ergebnisse
results_df <- data.frame(
  Streckenlänge = character(),
  Disziplin = character(),
  Predictor = character(),
  Mean = numeric(),
  P = numeric(),
  SE = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  ICC = numeric(),
  stringsAsFactors = FALSE
)

# Schleife über alle Kombinationen von Streckenlänge und Disziplin
for (i in 1:nrow(combinations)) {
  streckenlaenge <- combinations$Streckenlänge[i]
  disziplin <- combinations$Disziplin[i]
  
  # Filtere die Daten für die aktuelle Kombination
  filtered_data <- subset(data, Streckenlänge == streckenlaenge & Disziplin == disziplin)
  
  # Erstelle eine eindeutige Sportler-ID durch Kombination von Name und Jahrgang
  filtered_data$Sportler_ID <- paste(filtered_data$name, filtered_data$Jg, sep = "_")
  
  # Erstelle eine quadratische Variable für das Alter
  filtered_data$Alter_squared <- filtered_data$Alter^2
  
  # Fitte ein Mixed-Effects Modell mit random intercept für Sportler und fixed effects für Alter und Alter_squared
  model <- lmer(Zeit ~ Alter + Alter_squared + (1 | Sportler_ID), data = filtered_data, REML = TRUE)
  
  # Berechnung der Intraklassenkorrelation (ICC)
  var_intercept <- as.numeric(VarCorr(model)$Sportler_ID[1,1])
  var_residual <- attr(VarCorr(model), "sc")^2
  icc <- var_intercept / (var_intercept + var_residual)
  
  # Feste Effekte, SE, p-Wert und Konfidenzintervalle berechnen
  for (param in c("(Intercept)", "Alter", "Alter_squared")) {
    coef_value <- fixef(model)[param]
    se_value <- sqrt(vcov(model)[param, param])
    ci_lower <- coef_value - 1.96 * se_value
    ci_upper <- coef_value + 1.96 * se_value
    p_value <- summary(model)$coefficients[param, "Pr(>|t|)"]
    
    # Ergebnisse in den DataFrame speichern
    results_df <- rbind(results_df, data.frame(
      Streckenlänge = streckenlaenge,
      Disziplin = disziplin,
      Predictor = param,
      Mean = coef_value,
      P = p_value,
      SE = se_value,
      CI_lower = ci_lower,
      CI_upper = ci_upper,
      ICC = ifelse(param == "(Intercept)", icc, NA),  # ICC nur einmal pro Modell speichern
      stringsAsFactors = FALSE
    ))
  }
}

# Ergebnisse anzeigen
print(results_df)
