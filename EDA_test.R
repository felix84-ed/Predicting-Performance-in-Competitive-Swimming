
# Pakete installieren und laden -------------------------------------------
# Notwendige Pakete laden
library(dplyr)
library(readr)
library(ggplot2)



# total Datei importieren -------------------------------------------------

# Benutzer nach der Streckenlänge fragen
streckenlaenge <- as.integer(readline(prompt = "Geben Sie die Streckenlänge an (50, 100, 200, 400, 800, 1500): "))

# Überprüfen Sie die Eingabe der Streckenlänge
while (!(streckenlaenge %in% c(50, 100, 200, 400, 800, 1500))) {
  cat("Ungültige Eingabe. Bitte erneut eingeben.\n")
  streckenlaenge <- as.integer(readline(prompt = "Geben Sie die Streckenlänge an (50, 100, 200, 400, 800, 1500): "))
}

# Benutzer nach der Disziplin fragen, abhängig von der Streckenlänge
if (streckenlaenge %in% c(50, 100)) {
  disziplin <- readline(prompt = "Bitte geben Sie die Disziplin an (S, R, B, F): ")
  while (!(disziplin %in% c("S", "R", "B", "F"))) {
    cat("Ungültige Eingabe. Bitte erneut eingeben.\n")
    disziplin <- readline(prompt = "Bitte geben Sie die Disziplin an (S, R, B, F): ")
  }
} else if (streckenlaenge == 200) {
  disziplin <- readline(prompt = "Bitte geben Sie die Disziplin an (S, R, B, F, L): ")
  while (!(disziplin %in% c("S", "R", "B", "F", "L"))) {
    cat("Ungültige Eingabe. Bitte erneut eingeben.\n")
    disziplin <- readline(prompt = "Bitte geben Sie die Disziplin an (S, R, B, F, L): ")
  }
} else if (streckenlaenge == 400) {
  disziplin <- readline(prompt = "Bitte geben Sie die Disziplin an (F, L): ")
  while (!(disziplin %in% c("F", "L"))) {
    cat("Ungültige Eingabe. Bitte erneut eingeben.\n")
    disziplin <- readline(prompt = "Bitte geben Sie die Disziplin an (F, L): ")
  }
} else {
  disziplin <- "F"
}

# Benutzer nach dem Geschlecht fragen
geschlecht <- readline(prompt = "Bitte geben Sie das Geschlecht an (m oder w): ")
while (!(geschlecht %in% c("m", "w"))) {
  cat("Ungültige Eingabe. Bitte erneut eingeben.\n")
  geschlecht <- readline(prompt = "Bitte geben Sie das Geschlecht an (m oder w): ")
}

# Kombinieren Sie die Eingaben, um den Unterordnernamen zu erstellen
unterordner_name <- paste0(streckenlaenge, "_", disziplin, "_", geschlecht)

# Erstellen Sie den Pfad zur total.csv-Datei im entsprechenden Unterordner
basis_pfad <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Daten"
pfad <- file.path(basis_pfad, unterordner_name, "total.csv")

# CSV-Datei mit Komma als Dezimaltrennzeichen importieren
daten <- read.csv(pfad, sep = ",", dec = ",")

# Überprüfung der Datenformate
str(daten)

# Konvertieren Sie die Spalten Rang und Gesamtrang in Numeric
daten$Rang <- as.numeric(daten$Rang)
daten$Gesamtrang <- as.numeric(daten$Gesamtrang)

# Erneute Überprüfung der Datenformate
str(daten)


# Überprüfen Sie, ob es NA-Werte in der gesamten Datenstruktur gibt
if (anyNA(daten)) {
  cat("Es gibt NA-Werte im Datensatz.\n")
  
  # Überprüfen Sie spezifisch die Spalten Rang und Gesamtrang
  if (anyNA(daten$Rang)) {
    cat("Es gibt NA-Werte in der Spalte 'Rang'.\n")
  }
  if (anyNA(daten$Gesamtrang)) {
    cat("Es gibt NA-Werte in der Spalte 'Gesamtrang'.\n")
  }
} else {
  cat("Es gibt keine NA-Werte im Datensatz.\n")
}

# beschreibende Statistik -------------------------------------------------

# Beschreibende Statistik für jede Altersgruppe erstellen und ausgeben
for (alter in unique(daten$Alter)) {
  subset <- daten[daten$Alter == alter, ]
  statistik <- subset %>%
    group_by(Jahr) %>%
    summarise(
      count = n(),
      mean = mean(Zeit, na.rm = TRUE),
      std = sd(Zeit, na.rm = TRUE),
      min = min(Zeit, na.rm = TRUE),
      `25%` = quantile(Zeit, 0.25, na.rm = TRUE),
      `50%` = median(Zeit, na.rm = TRUE),
      `75%` = quantile(Zeit, 0.75, na.rm = TRUE),
      max = max(Zeit, na.rm = TRUE)
    )
  
  cat(paste("Statistik für", alter, "Jahre:\n"))
  print(statistik)
  cat("\n\n")
}

# Gesamtrangsummen --------------------------------------------------------
# Gesamtrangsummen für jede Altersgruppe pro Jahr berechnen
gesamtrang_summen <- daten %>%
  group_by(Alter, Jahr) %>%
  summarise(Gesamtrang_Summe = sum(Gesamtrang, na.rm = TRUE))

# Liniendiagramm der Gesamtrangsummen über die Jahre für jede Altersgruppe
ggplot(gesamtrang_summen, aes(x = Jahr, y = Gesamtrang_Summe, color = as.factor(Alter))) +
  geom_line() +
  labs(title = "Gesamtrangsummen über die Jahre nach Alter",
       x = "Jahr",
       y = "Gesamtrang Summe",
       color = "Alter") +
  theme_minimal()

# Balkendiagramm der Gesamtrangsummen über die Jahre für jede Altersgruppe, angeordnet in einem 3x3 Gitter
ggplot(gesamtrang_summen, aes(x = as.factor(Jahr), y = Gesamtrang_Summe, fill = as.factor(Jahr))) +
  geom_bar(stat = "identity") +
  labs(title = "Gesamtrangsummen über die Jahre nach Alter",
       x = "Jahr",
       y = "Gesamtrang Summe",
       fill = "Jahr") +
  facet_wrap(~Alter, ncol = 3) +
  theme_minimal()

# Gesamtrangsummen für jede Altersgruppe pro Jahrgang berechnen
gesamtrang_summen <- daten %>%
  group_by(Alter, Jg) %>%
  summarise(Gesamtrang_Summe = sum(Gesamtrang, na.rm = TRUE))

# Balkendiagramm der Gesamtrangsummen über die Jahrgänge für jede Altersgruppe, angeordnet in einem 3x3 Gitter
ggplot(gesamtrang_summen, aes(x = as.factor(Jg), y = Gesamtrang_Summe, fill = as.factor(Jg))) +
  geom_bar(stat = "identity") +
  labs(title = "Gesamtrangsummen über die Jahrgänge nach Alter",
       x = "Jahrgang",
       y = "Gesamtrang Summe",
       fill = "Jahrgang") +
  facet_wrap(~Alter, ncol = 3) +
  theme_minimal()

# Boxplot -----------------------------------------------------------------
#vermutlich sinnlos

# Boxplot erstellen
p <- ggplot(daten, aes(x = as.factor(Jg), y = Zeit, fill = as.factor(Jg))) + 
  geom_boxplot() + 
  facet_wrap(~ Alter, ncol = 3) + 
  labs(title = "Verteilung der Zeiten über Jahrgänge", 
       x = "Jahrgang", 
       y = "Zeit in Sekunden") + 
  theme_minimal() +
  theme(legend.position = "none")

print(p)


# Clustering --------------------------------------------------------------

install.packages("tidyr")

library(tidyr)
# Die Top 10 Schwimmer pro Alter und Jahrgang zählen
top10_count <- daten %>%
  group_by(Alter, Jg) %>%
  summarise(Top10 = sum(Gesamtrang <= 10)) %>%
  spread(Alter, Top10)

# Die Tabelle ausgeben
print(top10_count)

# Die Daten visualisieren
ggplot(daten, aes(x = factor(Jg), fill = factor(Alter))) +
  geom_bar(aes(y = ..prop.., group = 1), position = "dodge") +
  labs(y = "Prozent der Top 10 Schwimmer", x = "Jahrgang", fill = "Alter") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


# Zeitreihenanalyse -------------------------------------------------------

# Daten für 18-Jährige filtern
daten_18 <- daten[daten$Alter == 18,]

# Durchschnittliche Zeiten für verschiedene Top-Gruppen berechnen
avg_times <- daten_18 %>%
  arrange(Jahr, Gesamtrang) %>%
  group_by(Jahr) %>%
  summarise(
    Top100 = mean(Zeit, na.rm = TRUE),
    Top25 = if(n() >= 25) mean(Zeit[1:25], na.rm = TRUE) else NA,
    Top10 = if(n() >= 10) mean(Zeit[1:10], na.rm = TRUE) else NA,
    Top3 = if(n() >= 3) mean(Zeit[1:3], na.rm = TRUE) else NA
  )

# Daten in langes Format konvertieren für die Visualisierung
avg_times_long <- tidyr::pivot_longer(avg_times, -Jahr, names_to = "Group", values_to = "AvgTime")

# Liniendiagramm erstellen
plot <- ggplot(avg_times_long, aes(x = Jahr, y = AvgTime, color = Group)) +
  geom_line() +
  labs(title = "Durchschnittszeiten für 18-Jährige über die Jahre", y = "Durchschnittliche Zeit (s)", x = "Jahr") +
  theme_minimal()

print(plot)

# Zwischenzeitiges Exportieren --------------------------------------------


# Den Pfad für die neue Datei erstellen
neuer_pfad <- file.path(dirname(pfad), "Zwischenergebnis.csv")

# Die Daten in der neuen Datei speichern
write.csv(daten, file = neuer_pfad, row.names = FALSE, sep = ",", dec = ".")


# Verbesserungsraten ------------------------------------------------------

# Pakete installieren und laden
library(dplyr)

# Daten sortieren
daten <- daten %>% arrange(name, Alter)

# Zeit des vorherigen Jahres und das Alter des vorherigen Jahres hinzufügen
daten <- daten %>% 
  group_by(name) %>%
  mutate(
    prev_year_time = lag(Zeit),
    prev_year = lag(Alter)
  )

# Relative jährliche Verbesserungsrate berechnen
# Dabei wird überprüft, ob der Unterschied zwischen den Jahren genau 1 beträgt
daten <- daten %>% 
  mutate(
    verbesserungsrate = ifelse(Alter - prev_year == 1, (Zeit - prev_year_time) / prev_year_time * 100, NA)
  )

# Setzt die Verbesserungsrate auf 0, wenn das Alter 11 ist, da erster Datensatz
daten <- daten %>%
  mutate(verbesserungsrate = ifelse(Alter == 11, 0, verbesserungsrate))

# Temporäre Spalten entfernen
daten <- daten %>% select(-prev_year_time, -prev_year)

# grafische Darstellung der Verbesserungsraten:
library(ggplot2)

# Histogramm der Verbesserungsraten
hist_plot <- ggplot(daten, aes(x = verbesserungsrate)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Verteilung der Verbesserungsraten", 
       x = "Verbesserungsrate (%)", 
       y = "Anzahl der Schwimmer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(linetype = 'dashed', colour = 'grey80')) +
  coord_cartesian(clip = "off")

print(hist_plot)

# Boxplot der Verbesserungsraten nach Alter
box_plot <- ggplot(daten, aes(x = as.factor(Alter), y = verbesserungsrate)) +
  geom_boxplot(fill = "#0072B2", color = "black", alpha = 0.8) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Verbesserungsraten nach Alter", 
       x = "Alter", 
       y = "Verbesserungsrate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(linetype = 'dashed', colour = 'grey80')) +
  coord_cartesian(clip = "off")

print(box_plot)

#Berechnung über Verbesserungsraten:
library(dplyr)

# Durchschnittliche Verbesserungsrate für jedes Alter berechnen
avg_improvement_by_age <- daten %>%
  group_by(Alter) %>%
  summarise(
    avg_rate = mean(verbesserungsrate, na.rm = TRUE),
    sd_rate = sd(verbesserungsrate, na.rm = TRUE)
  )

# Differenz zwischen der Verbesserungsrate eines Schwimmers und dem Durchschnitt seines Alters berechnen
daten <- daten %>%
  left_join(avg_improvement_by_age, by = "Alter") %>%
  mutate(diff_from_avg = verbesserungsrate - avg_rate)

# z-Wert für die Verbesserungsrate eines Schwimmers basierend auf dem Durchschnitt und der Standardabweichung seines Alters berechnen
daten <- daten %>%
  mutate(z_value = (verbesserungsrate - avg_rate) / sd_rate)

daten

#Korrelation hohe z-Werte in jungen Jahren zu Top-Leistungen mit 18 Jahren.

# 1. Filtern Sie die Daten für die Altersgruppen 11-14 Jahre und extrahieren Sie die Z-Werte
z_values_11_14 <- daten %>%
  filter(Alter %in% c(11, 12, 13, 14)) %>%
  select(name, Alter, z_value)

# 2. Filtern Sie die Daten für das Alter von 18 Jahren und extrahieren Sie die Leistungen
performance_at_18 <- daten %>%
  filter(Alter == 18) %>%
  select(name, Zeit)

# 3. Kombinieren Sie die beiden Dataframes anhand des Schwimmer-Felds
combined_data <- merge(z_values_11_14, performance_at_18, by = "name")

# 4. Berechnen Sie die Korrelation zwischen den Z-Werten und den Leistungen bei 18 Jahren
correlations <- combined_data %>%
  group_by(Alter) %>%
  summarise(correlation = cor(z_value, Zeit, method = "spearman", use = "pairwise.complete.obs"))

print(correlations)

# Scatter-Plots erstellen
for (age in c(12, 13, 14)) {
  age_data <- combined_data[combined_data$Alter == age,]
  
  p <- ggplot(age_data, aes(x=z_value, y=Zeit)) + 
    geom_point(alpha=0.5) +
    labs(title=paste("Alter", age),
         x="z-Wert",
         y="Zeit bei 18 Jahren") +
    theme_minimal() +
    theme(panel.grid.major = element_line(linetype = "dashed", color="grey50"))
  print(p)
}

# Anzahl der Top100-Platzierungen für jeden Schwimmer berechnen
top100_counts <- daten %>%
  group_by(name) %>%
  summarise(top100_appearances = n())

# Das Alter, in dem der Schwimmer zum ersten Mal in der Top 100 erschien
earliest_top100 <- daten %>%
  group_by(name) %>%
  summarise(earliest_top100_age = min(Alter))

# Die beiden neuen Merkmale zum Hauptdatensatz hinzufügen
daten <- daten %>%
  left_join(top100_counts, by = "name") %>%
  left_join(earliest_top100, by = "name")

# Zwischenzeitiges Exportieren 2 --------------------------------------------


# Den Pfad für die neue Datei erstellen
neuer_pfad2 <- file.path(dirname(pfad), "Endergebnis.csv")

# Die Daten in der neuen Datei speichern
write.csv(daten, file = neuer_pfad2, row.names = FALSE)
