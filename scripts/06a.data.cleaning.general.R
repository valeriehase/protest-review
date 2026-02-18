#
# Data Cleaning of Final Coded Data - General
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Performs systematic post-coding cleaning of the deduplicated full-paper dataset.
# Includes (1) value checks and normalization for selected variables and (2) cross-variable consistency checks.
# Manual corrections are done by senior coder (MM) using consistent decision rules and fully logged.
#
# Setup ------------------------------------------------------------------------

source(here::here("R/paths.R"))
source(here::here("R/config.R"))
source(here::here("R/logging.R"))
source(here::here("R/helpers.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

log_df <- init_log()
log_df <- log_event(log_df, "06a_start", "script_started")

# Load Input -------------------------------------------------------------------

input_file <- require_file(file.path(PATHS$int, "05_deduplication_full_paper_sample_deduplicated.xlsx"), "deduplicated coded full-paper sample (output of step 05)")

message("Reading deduplicated coded full sample from: ", input_file)
df <- readxl::read_excel(input_file)

df_clean <- df %>%
  rename_with(~ str_extract(.x, "^V\\d+"), starts_with("V"))%>%
  mutate(
    method = as.character(method),
    V12 = as.character(V12),
    V13 = as.character(V13)
  )

# Sanity check
stopifnot(n_distinct(df_clean$id_unique) == nrow(df_clean))

# Check Values -------------------------------------------------------------

# --- V6 ----------------------------------------------------------------------

invalid_v6_ids <- find_invalid_token_ids(df_clean, "V6", max_tokens = 3, numeric_ok = c("1"))
invalid_v6 <- df_clean %>% dplyr::filter(id_unique %in% invalid_v6_ids$id_unique)
print(invalid_v6)

v6_edits <- tibble::tribble(
  ~id_unique, ~var, ~new_value, ~note,
  "ID413", "V6", "18 Million Rising (18MR); Asian American Feminist Collective (AAFC); Black Women Radicals (BWR)", "too many strings; reduced to first three",
  "ID44",  "V6", "Euromaidan; Occupy Wall Street (OWS; unionsq); Gezi park", "too many strings; reduced to first three",
  "ID94",  "V6", "1", "protest general recoded as protest cases"
)

tmp <- apply_manual_edits(df_clean, v6_edits, log_df, step = "06a_V6_value_fix", action = "manual_edit")
df_clean <- tmp$df; log_df <- tmp$log_df

# --- V7 ----------------------------------------------------------------------

allowed_v7 <- c(as.character(1:10), "NA")
invalid_v7 <- find_invalid_codes(df_clean, "V7", allowed = allowed_v7)
print(invalid_v7)

v7_edits <- tibble::tribble(
  ~id_unique, ~var, ~new_value, ~note,
  "ID366", "V7", "1; 3", "typo fix: '1.3' -> '1; 3'"
)

tmp <- apply_manual_edits(df_clean, v7_edits, log_df, step = "06a_V7_value_fix")
df_clean <- tmp$df; log_df <- tmp$log_df

# --- V8 ----------------------------------------------------------------------

invalid_v8_ids <- find_invalid_token_ids(df_clean, "V8", max_tokens = 3, numeric_ok = c("NA"))
invalid_v8 <- df_clean %>% dplyr::filter(id_unique %in% invalid_v8_ids$id_unique)
print(invalid_v8)

v8_edits <- tibble::tribble(
  ~id_unique, ~var, ~new_value, ~note,
  "ID1224", "V8", "Iraq; Egypt; Yemen", "too many strings; reduced to first three",
  "ID202",  "V8", "Ireland; Malta; Netherlands", "too many strings; reduced to first three",
  "ID44",   "V8", "Turkey; Ukraine; United States of America", "too many strings; reduced to first three"
)

tmp <- apply_manual_edits(df_clean, v8_edits, log_df, step = "06a_V8_value_fix", action = "manual_edit")
df_clean <- tmp$df; log_df <- tmp$log_df

# --- V9 ----------------------------------------------------------------------

invalid_v9_ids <- find_invalid_token_ids(df_clean, "V9", max_tokens = 3, numeric_ok = c("NA"))
invalid_v9 <- df_clean %>% dplyr::filter(id_unique %in% invalid_v9_ids$id_unique)
print(invalid_v9)

# --- V10 ---------------------------------------------------------------------

allowed_v10 <- as.character(c(100, 110:114, 120:124, 130:134, 140:142, 150:153, 160:162, 200:204, 300, 400, "NA"))
invalid_v10 <- find_invalid_codes(df_clean, "V10", allowed = allowed_v10)
print(invalid_v10)

v10_edits <- tibble::tribble(
  ~id_unique, ~var,  ~new_value,                 ~note,
  "ID1680",   "V10", "100",                      "survey on use of ICT; '99' -> '100'",
  "ID822",    "V10", "300; 113; 112; 111; 100; 110", "filled missing coding based on methods described",
  "ID98",     "V10", "141; 131; 132; 124",       "Synthesio used for Twitter/Facebook/Instagram/Reddit"
)

tmp <- apply_manual_edits(df_clean, v10_edits, log_df, step = "06a_V10_value_fix")
df_clean <- tmp$df; log_df <- tmp$log_df

# --- V11 ---------------------------------------------------------------------

allowed_v11 <- as.character(c(10:18, 20:26, 99, "NA"))
invalid_v11 <- find_invalid_codes(df_clean, "V11", allowed = allowed_v11)
print(invalid_v11)

v11_edits <- tibble::tribble(
  ~id_unique, ~var,  ~new_value, ~note,
  "ID1494",   "V11", "24",       "removed trailing dot",
  "ID2437",   "V11", "21; 22",    "filled missing coding",
  "ID83",     "V11", "18; 12",    "comma to semicolon"
)

tmp <- apply_manual_edits(df_clean, v11_edits, log_df, step = "06a_V11_value_fix")
df_clean <- tmp$df; log_df <- tmp$log_df

# --- V12 ------------------------------------

allowed_v12 <- as.character(c(0:1))
invalid_v12 <- find_invalid_codes(df_clean, "V12", allowed = allowed_v12)
print(invalid_v12)

# --- V13 ------------------------------------

allowed_v13 <- as.character(c(0:1))
invalid_v13 <- find_invalid_codes(df_clean, "V13", allowed = allowed_v13)
print(invalid_v13)

# --- Standardization ------------------------------------

df_clean <- df_clean %>%
  dplyr::mutate(
    V6  = dplyr::if_else(is.na(V6),  V6,  normalize_semicolon_field(V6, to_lower = FALSE)),
    V7  = dplyr::if_else(is.na(V7),  V7,  normalize_semicolon_field(V7)),
    V8  = dplyr::if_else(is.na(V8),  V8,  normalize_semicolon_field(V8)),
    V9  = dplyr::if_else(is.na(V9),  V9,  normalize_semicolon_field(V9, to_lower = TRUE)),
    V10 = dplyr::if_else(is.na(V10), V10, normalize_semicolon_field(V10)),
    V11 = dplyr::if_else(is.na(V11), V11, normalize_semicolon_field(V11)),
    V12 = stringr::str_trim(as.character(V12)),
    V13 = stringr::str_trim(as.character(V13))
  )

log_df <- log_event(
  log_df,
  step   = "06a_normalize_multi_code_fields",
  action = "format_standardization",
  note   = "Normalized delimiters to '; ' and trimmed whitespace/semicolons"
)

# Check Consistency ----------------------------------------------------

# If method == "0", V11 must NOT contain any codes 10:18
# If method == "1", V11 MUST contain at least one code 10:18

target_codes <- as.character(10:18)

v11_flags <- df_clean %>%
  dplyr::select(id_unique, method, V11) %>%
  tidyr::separate_rows(V11, sep = ";\\s*") %>%
  dplyr::mutate(V11 = stringr::str_trim(V11)) %>%
  dplyr::group_by(id_unique, method) %>%
  dplyr::summarise(has_10_18 = any(V11 %in% target_codes, na.rm = TRUE), .groups = "drop")

invalid_v11_method0_ids <- v11_flags %>% dplyr::filter(method == "0", has_10_18) %>% dplyr::pull(id_unique)
missing_v11_method1_ids <- v11_flags %>% dplyr::filter(method == "1", !has_10_18) %>% dplyr::pull(id_unique)

invalid_v11_method0 <- df_clean %>% dplyr::filter(id_unique %in% invalid_v11_method0_ids)
missing_v11_method1 <- df_clean %>% dplyr::filter(id_unique %in% missing_v11_method1_ids)

consistency_edits <- tibble::tribble(
  ~id_unique, ~var,  ~old_value,           ~new_value,    ~note,
  "ID1462",   "V11", "13; 21",             "21",         "no evidence of automated content analysis",
  "ID1656",   "V11", "13; 21",             "21",         "no evidence of automated content analysis",
  "ID19",     "V11", "21; 22; 18",         "21; 22",     "removed code 18 (data collection without specification)",
  "ID1956",   "V11", "17; 21",             "21",         "tracking code was misunderstood",
  "ID2327",   "V11", "21; 22; 18",         "21; 22",     "removed code 18 (no scraping documented)",
  "ID239",    "V11", "12; 24",             "24",         "removed code 12 (API not used independently)",
  "ID413",    "V11", "21; 22; 13; 16; 18", "21",         "purely qualitative Instagram analysis; removed other codes",
  "ID94",     "V11", "12; 18; 24",         "24",         "removed 12/18",
  
  "ID13",     "V11", "21; 24; 25",         "13; 24",     "added 13 because of Content and computer-mediated discourse analysis",
  "ID1474",   "V11", "21",                 "21; 18",     "added 18 because web scraping was used as a method",
  "ID1494",   "V11", "24",                 "24; 18; 12", "added 18 because a website scraper was used; added 12 for API use",
  "ID1716",   "V11", "26",                 "26; 16",     "added 16 because network analysis was present",
  "ID489",    "V11", "23; 21",             "23; 21; 18", "added 18 because MAXQDA Web Collector was used as a manual scraping tool"
)

tmp <- apply_manual_edits(df_clean, consistency_edits, log_df, step = "06a_consistency_CSS_in_method0")
df_clean <- tmp$df
log_df   <- tmp$log_df


log_df <- log_event(
  log_df,
  step   = "06a_consistency_CSS_in_method0",
  action = "no_change_documented",
  note   = "Checked IDs ID1199, ID202, ID2152, ID267, ID366, ID109: no edits; CSS methods appear in method == 0"
)
ids_method_css <- c("ID1199", "ID202", "ID2152", "ID267", "ID366", "ID109")

log_df <- log_event(
  log_df,
  step   = "06a_consistency_CSS_in_method0",
  action = "no_change_documented",
  note   = "Checked ID2382: no edits; no CSS methods appear even though method == 1"
)
ids_method_non_css <- c("ID2382")

df_clean <- df_clean %>%
  dplyr::mutate(
    method = dplyr::if_else(id_unique %in% ids_method_css, "1", method),
    method = dplyr::if_else(id_unique %in% ids_method_non_css, "0", method)
  )

# Resampling ----------------------------------------------------

set.seed(SEED)

table(df_clean$method)

ids_to_delete <- df_clean %>%
  dplyr::filter(method == "1") %>%
  dplyr::distinct(id_unique) %>%
  dplyr::slice_sample(n = 10) %>%
  dplyr::pull(id_unique)

df_clean <- df_clean %>%
  filter(!id_unique %in% ids_to_delete)

df_clean %>% count(method)

log_df <- log_event(
  log_df,
  step   = "06a_consistency_correction",
  action = "random_deletion",
  note   = paste0(
    "Randomly removed 10 cases from method == 1 to restore equal group sizes ",
    "(n = 221 each). Draw reproducible via predefined seed. IDs removed: ",
    paste(ids_to_delete, collapse = ", ")
  )
)

# Output -----------------------------------------------------------------------

out_dir <- PATHS$int
log_dir <- PATHS$logs

out_df_cleaned <- file.path(out_dir, "06a_full_paper_sample_deduplicated_cleaned.xlsx")
openxlsx::write.xlsx(df_clean, out_df_cleaned, overwrite = TRUE)

log_file <- file.path(log_dir, paste0("06a_cleaning_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv"))
write_log(log_df, log_file)

message("06a completed.")
message("- Randomly deleted IDs: ", paste(ids_to_delete, collapse = ", "))
message("- Cleaned dataset at: ", out_df_cleaned)
message("- Log written to: ", log_file)

