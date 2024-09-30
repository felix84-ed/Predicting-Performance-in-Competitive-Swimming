library(ggplot2)


# Prozent 11jähriger ------------------------------------------------------

# Schwimmer extrahieren, die im Alter von 11 in den Top-100 waren
age_11_swimmers <- data %>% 
  filter(Alter == 11) %>% 
  distinct(name) %>% 
  pull(name)

# Berechnen Sie den Prozentsatz für jedes Alter
percentages <- sapply(11:18, function(age) {
  current_age_swimmers <- data %>% 
    filter(Alter == age) %>% 
    distinct(name) %>% 
    pull(name)
  
  swimmers_in_both <- intersect(age_11_swimmers, current_age_swimmers)
  length(swimmers_in_both) / length(age_11_swimmers) * 100
})

# Altersbereiche für die Grafik
ages <- 11:18

# Daten für die Grafik vorbereiten
plot_data <- data.frame(Age = ages, Percentage = percentages)

# Grafik erstellen
ggplot(plot_data, aes(x = Age, y = Percentage)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Verbleib der elfjährigen Schwimmer in den Top 100 Listen",
       x = "Alter",
       y = "Verbleibende elfjährige Schwimmer (%)") +
  theme_minimal()


# Relative Wahrscheinlichkeit Top100 über Jahre ---------------------------

# Berechnung der relativen Wahrscheinlichkeiten für jedes folgende Jahr
relative_probabilities <- c(100)  # Start mit 100% für 11-Jährige
for (alter in 12:18) {
  # Schwimmer in den Top 100 für das aktuelle Alter
  schwimmer_dieses_alter <- data %>% filter(Alter == alter, name %in% age_11_swimmers) %>% .$name
  
  # Berechnung der relativen Wahrscheinlichkeit
  relative_probability <- length(unique(schwimmer_dieses_alter)) / length(unique(age_11_swimmers)) * 100
  relative_probabilities <- c(relative_probabilities, relative_probability)
}


# Jahre für die x-Achse (1 bis 8 Jahre)
Jahre <- 1:8

# Daten für die Grafik vorbereiten
plot_data <- data.frame(Jahr = Jahre, Wahrscheinlichkeit = relative_probabilities)

# Überprüfen der Datenstruktur
print(plot_data)

# Erstellen des Liniendiagramms
ggplot(plot_data, aes(x = Jahr, y = Wahrscheinlichkeit)) +
  geom_line(color = 'blue') +
  geom_point(color = 'red') +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(title = 'Relative Wahrscheinlichkeit der Top-100-Platzierung über aufeinanderfol
gende Jahre',
       x = 'Anzahl der Jahre in den Top 100',
       y = 'Relative Wahrscheinlichkeit (%)') +
  theme_minimal()