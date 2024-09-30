# Laden des Datensatzes
file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Data/extended_dataset.csv"
df <- read.csv(file_path)

# Z-Standardisierung der 'Zeit' Spalte nach 'Event' (falls noch nicht vorhanden)
df$Zeit_z <- ave(df$Zeit, df$Event, FUN = function(x) (x - mean(x)) / sd(x))

# Löschen der Spalten "Relativer_Wert" und "Ztime"
df <- df[, !(names(df) %in% c("Relativer_Wert", "Ztime"))]

# Umbenennen der Spalte 'Zeit_z' in 'Z_time'
names(df)[names(df) == "Zeit_z"] <- "Z_time"

# Verschieben der Spalte 'Z_time' an die dritte Stelle
df <- df[, c(names(df)[1:2], "Z_time", names(df)[3:(ncol(df)-1)])]

# Speichern des Datensatzes unter dem gleichen Namen und Pfad
write.csv(df, file_path, row.names = FALSE)

# Bestätigung der Speicherung
print("Die Tabelle wurde erfolgreich gespeichert.")
