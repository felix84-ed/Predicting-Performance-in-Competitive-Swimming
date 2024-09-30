# Bibliotheken laden
library(dplyr)
library(ggplot2)
library(tidyr)

# Datei laden
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final_with_years_in_top100_updated.csv"
df <- read.csv(file_path)

# Funktion zur Berechnung der prozentualen Verbesserung basierend auf einem gegebenen Basisalter
calculate_percent_improvement <- function(df, baseline_age) {
  baseline_col <- paste0("baseline_time_", baseline_age, "y")
  improvement_col <- paste0("percent_individual_improvement_", baseline_age, "y")
  
  df <- df %>%
    group_by(name, Jg, Streckenlänge, Disziplin) %>%
    mutate(!!baseline_col := ifelse(baseline_age %in% Alter, Zeit[Alter == baseline_age], NA)) %>%
    ungroup() %>%
    mutate(!!improvement_col := ifelse(!is.na(!!sym(baseline_col)) & !!sym(baseline_col) != 0, 
                                       ((!!sym(baseline_col) - Zeit) / !!sym(baseline_col)) * 100, 
                                       NA))
  return(df)
}

# Funktion zur Generierung der Kurvendaten für jede Disziplin und Streckenlänge
generate_curve_data <- function(df, streckenlänge, disziplin, baseline_age) {
  improvement_col <- paste0("percent_individual_improvement_", baseline_age, "y")
  
  # Filterung der Daten für die spezifische Disziplin und Streckenlänge
  disziplin_df <- df %>%
    filter(Streckenlänge == streckenlänge, Disziplin == disziplin, !is.na(!!sym(improvement_col)))
  
  if (nrow(disziplin_df) > 0) {
    # Vorbereitung der Daten für die quadratische Regression
    ages <- disziplin_df$Alter
    improvements <- disziplin_df[[improvement_col]]
    
    if (length(unique(ages)) > 2) {
      # Quadratische Regression
      model <- lm(improvements ~ poly(ages, 2, raw = TRUE))
      
      # Vorhersage der Verbesserungen basierend auf dem Modell
      ages_range <- seq(baseline_age, 18, by = 1)
      improvements_pred <- predict(model, newdata = data.frame(ages = ages_range))
      
      # Erstellen eines DataFrames für die Vorhersagedaten
      pred_df <- data.frame(Alter = ages_range, 
                            percent_improvement = improvements_pred, 
                            Streckenlänge = streckenlänge, 
                            Disziplin = disziplin)
      
      return(pred_df)
    }
  }
  return(NULL)
}

# Einzigartige Kombinationen von Streckenlänge und Disziplin
unique_combinations <- df %>%
  select(Streckenlänge, Disziplin) %>%
  distinct()

# Basisalter, für die die Berechnungen durchgeführt werden sollen
baseline_ages <- 11:14

# Initialisieren des erweiterten Datensatzes
extended_df <- df

for (baseline_age in baseline_ages) {
  improvement_col <- paste0("percent_individual_improvement_", baseline_age, "y")
  general_improvement_col <- paste0("percent_general_improvement_", baseline_age, "y")
  diff_improvement_col <- paste0("percent_diff_improvement_", baseline_age, "y")
  
  # Berechnung der prozentualen Verbesserung für das gegebene Basisalter
  extended_df <- calculate_percent_improvement(extended_df, baseline_age)
  
  # Generieren der Kurvendaten für jede Disziplin und Streckenlänge
  curve_data_list <- lapply(1:nrow(unique_combinations), function(i) {
    generate_curve_data(extended_df, unique_combinations$Streckenlänge[i], unique_combinations$Disziplin[i], baseline_age)
  })
  
  # Zusammenführen aller Kurvendaten in einen DataFrame
  curve_data <- do.call(rbind, curve_data_list)
  
  # Umwandlung von Streckenlänge in numerisch für den Join
  curve_data$Streckenlänge <- as.numeric(as.character(curve_data$Streckenlänge))
  
  # Schritt zur Wiederherstellung der Spalten für das Join
  curve_data <- curve_data %>%
    unite("Strecken_Disziplin", Streckenlänge, Disziplin, sep = "_") %>%
    spread(key = Strecken_Disziplin, value = percent_improvement)
  
  # Sicherstellen, dass die neue Spalte general_improvement_col korrekt benannt wird
  colnames(curve_data)[colnames(curve_data) == "percent_improvement"] <- general_improvement_col
  
  # Umwandlung von Streckenlänge in numerisch für den Join
  curve_data <- curve_data %>%
    gather(key = "Strecken_Disziplin", value = !!sym(general_improvement_col), -Alter) %>%
    separate(Strecken_Disziplin, into = c("Streckenlänge", "Disziplin"), sep = "_")
  
  # Umwandlung von Streckenlänge zurück in Faktor für den Join
  curve_data$Streckenlänge <- as.numeric(curve_data$Streckenlänge)
  
  # Hinzufügen der Verbesserungswerte zu den individuellen Daten
  extended_df <- extended_df %>%
    left_join(curve_data, by = c("Alter", "Streckenlänge", "Disziplin"))
  
  # Berechnung der Differenz zwischen theoretischer und tatsächlicher Verbesserung
  extended_df <- extended_df %>%
    mutate(!!diff_improvement_col := !!sym(improvement_col) - !!sym(general_improvement_col))
}

# Speichern des erweiterten Datensatzes
output_file_path_extended <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"
write.csv(extended_df, output_file_path_extended, row.names = FALSE)
