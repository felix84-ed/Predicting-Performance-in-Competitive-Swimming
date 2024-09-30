# Laden der erforderlichen Bibliotheken
library(dplyr)

# Laden der CSV-Datei
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/ergebnisse_final.csv"
df <- read.csv(file_path)

# Kombinieren der Streckenlänge und Disziplin zu einem Ereignis
df$Ereignis <- paste(df$Streckenlänge, df$Disziplin, sep = " ")

# Berechnung der deskriptiven Statistiken
descriptive_stats <- df %>%
  group_by(Ereignis, Alter) %>%
  summarise(
    Mean = mean(Zeit, na.rm = TRUE),
    Median = median(Zeit, na.rm = TRUE),
    SD = sd(Zeit, na.rm = TRUE),
    Min = min(Zeit, na.rm = TRUE),
    Max = max(Zeit, na.rm = TRUE)
  )

# Ausgabe der deskriptiven Statistiken
print(descriptive_stats)

# Export der Modelle als CSV
write_csv(descriptive_stats, "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/descriptive_stats1.csv")

# Visualisierung der Verteilung der Schwimmzeiten nach Streckenlänge und Disziplin
ggplot(df, aes(x = Zeit)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Streckenlänge + Disziplin, scales = "free") +
  labs(title = "Verteilung der Schwimmzeiten nach Streckenlänge und Disziplin", x = "Schwimmzeit (s)", y = "Häufigkeit") +
  theme_minimal()

# Visualisierung der Schwimmzeiten mit Boxplot nach Streckenlänge und Disziplin
ggplot(df, aes(x = Ereignis, y = Zeit)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Boxplot der Schwimmzeiten nach Streckenlänge und Disziplin", x = "Ereignis", y = "Schwimmzeit (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Streckenlänge + Disziplin, scales = "free")


# Filtern der Daten für die spezifische Kombination aus 100m und Freistil
df_filtered <- df %>% filter(Streckenlänge == 1500 & Disziplin == "F")

# Visualisierung der Schwimmzeiten mit Boxplot für die Kombination aus 100m und Freistil über den Verlauf im Alter
ggplot(df_filtered, aes(x = factor(Alter), y = Zeit)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Boxplot der Schwimmzeiten für 100m Freistil nach Altersgruppen", x = "Alter", y = "Schwimmzeit (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
