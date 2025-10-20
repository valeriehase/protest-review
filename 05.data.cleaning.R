########################
#
# Data Cleaning of Final Coded Date
# Author: Miriam Milzner
# Date: 2025-10-20
#
########################
#
# Notes:
# - Input file: data/raw/df_full_sample_coded.xlsx
# - Comment-Spalte wurde vor Upload manuell vereinheitlicht (Format: "Vxx: Text; Vyy: Text")
# - Schritte im Skripts:
#   1. Vergibt eindeutige row_id (1:n)
#   2. Identifiziert doppelte id_unique 
#      → manuelle Entscheidung in duplicates_check.xlsx (Spalte 'decision')
#      → entfernt Duplikate anhand row_id
#   3. Validiert Variablenwerte (V7, V10, V11, V12, V13)
#      → prüft pro Variable nur gültige Codes laut Codebuch
#      → zeigt ungültige Werte in der Konsole
#      → keine automatischen Korrekturen, nur gezielte manuelle Fixes
#   4. Loggt alle manuellen Änderungen einzeln mit Zeitstempel
#
# - Log:
#   → Format: TSV (Tab-separiert)
#   → Pfad: logs/data_cleaning_log.tsv
#   → Enthält: timestamp | step | action | note
#
# - Alle Ausgaben liegen unter data/processed/
# - Skript ist deterministisch und reproduzierbar
#
# Packages -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(tidycomm)
  library(janitor)
  library(stringr)
  library(openxlsx)
  library(flextable)
  library(officer)
  library(glue)
  library(fs)
  library(readr)
})

# Simple Log System for Documentation ------------------------------------------

# empty log table
log_df <- tibble::tibble(
  timestamp = character(),
  step = character(),
  action = character(),
  note = character()
)

# log function
log_event <- function(step, action, note = "") {
  new_row <- tibble::tibble(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    step = step,
    action = action,
    note = note
  )
  assign("log_df", dplyr::bind_rows(get("log_df", envir = .GlobalEnv), new_row),
         envir = .GlobalEnv)
  invisible(new_row)
}

# save logs
write_log <- function(path = "logs/data_cleaning_log.tsv") {
  fs::dir_create(dirname(path))
  readr::write_tsv(log_df, path)
  message("Log saved: ", path)
}


# Step 1: Import ---------------------------------------------------------------

raw_data <- "data/raw/full_paper_sample_coded.xlsx"
df_raw <- readxl::read_excel(raw_data)

log_event(
  step = "01_import",
  action = "standardization",
  note = "Comment column manually standardized to format 'Vxx: text; Vyy: text' before import"
)

# Step 2: Inspect Duplicates manually ------------------------------------------
#
# Note: Duplicates have not been deleted before coding

df_raw <- df_raw %>%
  mutate(row_id = row_number())

dupes <- df_raw %>%
  group_by(id_unique) %>%
  mutate(dup_count = n()) %>%
  ungroup() %>%
  filter(dup_count > 1) %>%
  arrange(id_unique, row_id)

# write.xlsx(dupes, "data/processed/duplicates.xlsx")

log_event(
  step   = "02_duplicates",
  action = "export_duplicates",
  note   = paste0("Duplikate in id_unique gefunden: ", n_distinct(dupes$id_unique),
                  " IDs, ", nrow(dupes), " Zeilen exportiert nach data/processed/duplicates.xlsx, um sie manuell zu inspizieren")
)

### here: inspected duplicates manually here

duplicates_path <- "data/processed/duplicates_check.xlsx"
dupes_decided <- read_excel(duplicates_path)

to_remove <- dupes_decided %>%
  filter(decision == 0) %>%
  pull(row_id)

to_remove

df_deduplicated <- df_raw %>%
  filter(!row_id %in% to_remove)

removed_n <- nrow(df_raw) - nrow(df_deduplicated)

# save deduplicated df
# write.xlsx(df_deduplicated, "data/processed/df_deduplicated.xlsx")

# save removed cases (documentation)
removed_duplicates <- df_raw %>% filter(row_id %in% to_remove)
# write.xlsx(removed_duplicates, "data/processed/duplicates_removed.xlsx")

# log
log_event(
  step   = "02_deduplication",
  action = "remove_duplicates_by_manual_decision",
  note   = paste0("Gesamt entfernt: ", removed_n,
                  " Zeilen (row_ids: ", toString(sort(unique(to_remove))),
                  ") | Ergebnis: data/processed/df_deduplicated.xlsx und data/processed/duplicates_removed.xlsx")
)

# Step 3: Basic Cleaning - Value Validation ------------------------------------

df_clean <- df_deduplicated %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),  # anpassung variablennamen
    starts_with("V")) 

### V7

allowed_v7 <- as.character(1:10) |> c("NA")  

invalid_v7 <- df_clean %>%
  filter(!is.na(V7)) %>%                        # "echte" NAs sind auch ok
  separate_rows(V7, sep = ";\\s*") %>%          # mehrfachcodierungen splitten
  mutate(V7 = str_trim(V7)) %>%
  filter(!(V7 %in% allowed_v7))  

print(invalid_v7)
# Typo bei ID366

df_clean <- df_clean %>%
  mutate(V7 = if_else(id_unique == "ID366", "1; 3", V7))

log_event(
  step   = "03_V7_value_fix",
  action = "manual_edit",
  note   = "id_unique ID366: V7 geändert von '1.3' zu '1; 3'"
)

### V10

allowed_v10 <- as.character(c(100, 110:114, 120:124, 130:134, 140:142, 150:153, 160:162, 200:204, 300, 400, "NA"))

invalid_v10 <- df_clean %>%
  filter(!is.na(V10)) %>%                        # echte NAs auch ok
  separate_rows(V10, sep = ";\\s*") %>%          # mehrfachcodierung splitten
  mutate(V10 = str_trim(V10)) %>%
  filter(!(V10 %in% allowed_v10))

print(invalid_v10)

df_clean <- df_clean %>% 
  mutate(V10 = if_else(id_unique == "ID1680", "100", V10), # survey on use of ICT
         V10 = if_else(id_unique == "ID822", "300; 113; 112; 111; 100; 110", V10), # media types included traditional media (or affiliates), online partisan media, online nonpartisan media, activism/advocacy media, social media, and ephemeral websites
         V10 = if_else(id_unique == "ID98", "141; 131; 132; 124", V10)) # Twitter, Facebook, Instagram, and Reddit data were collected using Synthesio

#log
log_event(
  step   = "03_V10_value_fix",
  action = "manual_edit",
  note   = "id_unique ID1680: V10 geändert von '99' zu '100' (survey on use of ICT)"
)
log_event(
  step   = "03_V10_value_fix",
  action = "manual_edit",
  note   = "id_unique ID822: V10 geändert von ' ' zu '300; 113; 112; 111; 100; 110' (mehrere Medientypen)"
)
log_event(
  step   = "03_V10_value_fix",
  action = "manual_edit",
  note   = "id_unique ID98: V10 geändert von ' ' zu '141; 131; 132; 124' (Twitter, Facebook, Instagram, Reddit via Synthesio)"
)


### V11

allowed_v11 <- as.character(c(10:18, 20:26, 99, "NA"))

invalid_v11 <- df_clean %>%
  filter(!is.na(V11)) %>%                 # echte NAs auch ok
  separate_rows(V11, sep = ";\\s*") %>%   # mehrfachcodierung splitten
  mutate(V11 = str_trim(V11)) %>%         
  filter(!(V11 %in% allowed_v11))         

print(invalid_v11)

df_clean <- df_clean %>% 
  mutate(V11 = if_else(id_unique == "ID1494", "24", V11), 
         V11 = if_else(id_unique == "ID2437", "21; 22", V11),
         V11 = if_else(id_unique == "ID83", "18; 12", V11)) 

#log
log_event(
  step   = "03_V11_value_fix",
  action = "manual_edit",
  note   = "id_unique ID1494: V11 geändert von '24.' zu '24'"
)
log_event(
  step   = "03_V11_value_fix",
  action = "manual_edit",
  note   = "id_unique ID2437: V11 geändert von ' ' zu '21; 22'"
)
log_event(
  step   = "03_V11_value_fix",
  action = "manual_edit",
  note   = "id_unique ID83: V11 geändert von '18, 12' zu '18; 12'"
)


### V12

allowed_v12 <- c("0", "1")

invalid_v12 <- df_clean %>%
  filter(!is.na(V12)) %>%               #  NAs sind nicht erlaubt
  mutate(V12 = str_trim(as.character(V12))) %>%
  filter(!(V12 %in% allowed_v12))       

print(invalid_v12)

### V13

allowed_v13 <- c("0", "1")

invalid_v13 <- df_clean %>%
  filter(!is.na(V13)) %>%               #  NAs sind nicht erlaubt
  mutate(V13 = str_trim(as.character(V13))) %>%
  filter(!(V13 %in% allowed_v13))      

print(invalid_v12)


# Step 4: Konsistenzprüfung ----------------------------------------------------

### Konsistenzprüfung: Wenn method == "0", dürfen in V11 keine 10:18 stehen

method_col <- "method"

invalid_v11_method0_ids <- df_clean %>%
  filter(.data[[method_col]] == "0") %>%    
  separate_rows(V11, sep = ";\\s*") %>%                   
  mutate(V11 = str_trim(V11)) %>%                         
  filter(V11 %in% as.character(10:18)) %>%                
  distinct(id_unique) %>%                                 
  arrange(id_unique)

invalid_v11_method0 <- df_clean %>%
  filter(id_unique %in% invalid_v11_method0_ids$id_unique)

print(invalid_v11_method0)

# "ID1199", "12; 21"
#"ID1462", "13; 21" 
#"ID1656", "13; 21"
#"ID19", "21; 22; 18"
#"ID1956", "17; 21"
#"ID202", "24; 16"
#"ID2152", "13; 99"
#"ID2327", "21; 22; 18"
#"ID239", "12; 24"
#"ID267", "13; 16; 17; 26"
#"ID366", "13; 21"
#"ID413", "21;22;13;16;18"
#"ID94", "12; 18; 24"


# df_clean <- df_clean %>% 
#  mutate(V11 = if_else(id_unique == "ID1199", "12; 21", V11), 
#         V11 = if_else(id_unique == "ID1462", "13; 21", V11), 
#         V11 = if_else(id_unique == "ID1656", "13; 21", V11),
#         V11 = if_else(id_unique == "ID19", "21; 22; 18", V11),
#         V11 = if_else(id_unique == "ID1956", "17; 21", V11),
#         V11 = if_else(id_unique == "ID202", "24; 16", V11),
#         V11 = if_else(id_unique == "ID2152", "13; 99", V11),
#         V11 = if_else(id_unique == "ID2327", "21; 22; 18", V11),
#        V11 = if_else(id_unique == "ID239", "12; 24", V11),
#         V11 = if_else(id_unique == "ID267", "13; 16; 17; 26", V11),
#         V11 = if_else(id_unique == "ID366", "13; 21", V11),
#         V11 = if_else(id_unique == "ID413", "21;22;13;16;18", V11),
#         V11 = if_else(id_unique == "ID94", "12; 18; 24", V11),
#         ) 

#log
# log_event(
#  step   = "04_consistency",
#  action = "manual_edit",
#  note   = "id_unique : V11 geändert von ' ' zu ' '")

#tbc


# Step 5: Kommentare -----------------------------------------------------------


comments_long <- df_clean %>%
  filter(!is.na(Comment_CODER)) %>%
  mutate(Comment = str_squish(Comment_CODER)) %>%             
  separate_rows(Comment, sep = ";\\s*") %>%               
  extract(Comment, into = c("variable", "comment_text"),
          regex = "^(V\\d{1,2}):\\s*(.*)$", remove = TRUE) %>% 
  mutate(variable = str_trim(variable),
         comment_text = str_trim(comment_text)) %>%
  filter(!is.na(variable), variable != "")

print(comments_long)

# manually inspected all comments and resolved only the comments with active questions for now.
# tbc


# Write Log --------------------------------------------------------------------

write_log()
log <- readr::read_tsv("logs/data_cleaning_log.tsv")

