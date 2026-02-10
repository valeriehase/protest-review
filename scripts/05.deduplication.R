#
# Deduplication 
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/packages.R"))
source(here("R/paths.R"))
source(here("R/config.R"))

# 1. Import Coded Data Mask ----------------------------------------------------

df_raw <- readxl::read_excel(PATHS$raw_coded_full_sample) %>%
  dplyr::mutate(row_id = dplyr::row_number)

# 2. Export Duplicates for Manual Decision -------------------------------------
# Note: Duplicates have not been deleted before coding, this is why we delete duplicates here after inspecting the coding duplicates manually.
# We keep the dupe that has the "better" coding.

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

### what happened here: inspected duplicates manually here

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









