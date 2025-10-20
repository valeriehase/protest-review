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
# - The 'Comment' column was **manually standardized** to the format:
#       e.g. V10: no method section; V11: unclear coding
#   → This allows consistent parsing of multiple comments per variable
#   → DO NOT edit the 'Comment' column format manually anymore unless
#     the same pattern (Vxx: text; Vyy: text) is preserved.
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

# Simple Log System ------------------------------------------------------------

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
write_log <- function(path = "logs/data_cleaning_log.csv") {
  fs::dir_create(dirname(path))
  readr::write_csv(log_df, path)
  message("Log saved: ", path)
}


# Step 1: Import ---------------------------------------------------------------

raw_data <- "data/raw/full_paper_sample_coded.xlsx"
df_raw <- readxl::read_excel(raw_data)

log_event(
  step = "01_import",
  action = "read_excel",
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

write.xlsx(dupes, "data/processed/duplicates.xlsx")

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
write.xlsx(df_deduplicated, "data/processed/df_deduplicated.xlsx")

# save removed cases (documentation)
removed_duplicates <- df_raw %>% filter(row_id %in% to_remove)
write.xlsx(removed_duplicates, "data/processed/duplicates_removed.xlsx")

# log
log_event(
  step   = "02_deduplication",
  action = "remove_duplicates_by_decision",
  note   = paste0("Gesamt entfernt: ", removed_n,
                  " Zeilen (IDs: ", length(ids_to_remove),
                  ") | Ergebnis: data/processed/df_deduplicated.xlsx und data/processed/duplicates_removed.xlsx")
)


# Step 3: Basic Cleaning - Value Validation -----------------------------------

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
  step   = "03_V7_fix",
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
  mutate(V10 = if_else(row_id == 142, "100", V10), # survey on use of ICT
         V10 = if_else(row_id == 413, "300; 113; 112; 111; 100; 110", V10), # media types included traditional media (or affiliates), online partisan media, online nonpartisan media, activism/advocacy media, social media, and ephemeral websites
         V10 = if_else(row_id == 458, "141; 131; 132; 124", V10)) # Twitter, Facebook, Instagram, and Reddit data were collected using Synthesio
# log
log_event(
  step   = "03_V10_fix",
  action = "manual_edit_batch",
  note   = paste(
    "3 Änderungen an V10:",
    "row_id 142 -> '100' (survey on use of ICT)",
    "row_id 413 -> '300; 113; 112; 111; 100; 110' (mehrere Medientypen)",
    "row_id 458 -> '141; 131; 132; 124' (Twitter, Facebook, Instagram, Reddit via Synthesio)",
    sep = " | "
  )
)

# Write Log --------------------------------------------------------------------

write_log()
