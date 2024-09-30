# Laden des notwendigen Pakets
library(tools)

# Pfad zum Verzeichnis
directory_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Daten_weltspitze"

# Liste aller Dateien im Verzeichnis
files <- list.files(path = directory_path, full.names = TRUE)

# Funktion, um .csv Endung hinzuzufügen, falls nicht vorhanden
add_csv_extension <- function(filename) {
  if (!grepl("\\.csv$", filename)) {
    file.rename(filename, paste0(filename, ".csv"))
  }
}

# Anwenden der Funktion auf jede Datei
lapply(files, add_csv_extension)

library(dplyr)
library(readr)

# Verzeichnispfad
directory_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Daten_weltspitze"

# Dateien auflisten
files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

# Zyklus-Disziplin-Mapping und Ergebnismatrix
cycle_map <- c("2000" = "2000-2003", "2004" = "2004-2007", "2008" = "2008-2011", "2012" = "2012-2015", "2016" = "2016-2020", "2021" = "2021-2023")
results <- matrix(NA, nrow = 6, ncol = 17)
colnames(results) <- c("50F", "50B", "50R", "50S", "100F", "100B", "100R", "100S", "200F", "200B", "200R", "200S", "200L", "400F", "400L", "800F", "1500F")
rownames(results) <- names(cycle_map)

# Funktion, um Zeitstring in Sekunden umzuwandeln
convert_time_to_seconds <- function(time_str) {
  time_parts <- unlist(strsplit(time_str, "[:.,]"))
  if (length(time_parts) == 3) {
    # Format mm:ss,00
    return(as.numeric(time_parts[1]) * 60 + as.numeric(time_parts[2]) + as.numeric(time_parts[3])/100)
  } else {
    # Format ss.00
    return(as.numeric(time_parts[1]) + as.numeric(time_parts[2])/100)
  }
}

for (file in files) {
  file_info <- strsplit(basename(file), "[_.]")[[1]]
  cycle_key <- file_info[1]
  discipline <- paste0(file_info[2], file_info[3])
  
  cycle <- cycle_map[cycle_key]
  
  if (!is.null(cycle) && discipline %in% colnames(results)) {
    data <- read_csv(file, col_types = cols(swim_time = col_character())) %>%
      mutate(swim_time = sapply(swim_time, convert_time_to_seconds)) %>%
      slice_head(n = 10) %>%
      summarise(avg_time = mean(swim_time, na.rm = TRUE)) %>%
      pull(avg_time)
    
    results[cycle, discipline] <- avg_time
  }
}

# Ergebnisse als CSV speichern
output_file_path <- "/Users/fbongarz/Library/Mobile Documents/com~apple~CloudDocs/02_Studium/Bachelor-Arbeit/Daten_weltspitze/ergebnis.csv"
write.csv(results, file = output_file_path, row.names = TRUE)
