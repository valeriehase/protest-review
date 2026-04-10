#
# Data Cleaning of Final Coded Data - Distribution of Rare Codes
# Date: 2026-30-03
#
# Setup ------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

source(here::here("helper functions/paths.R"))
source(here::here("helper functions/config.R"))
source(here::here("helper functions/logging.R"))
source(here::here("helper functions/helpers.R"))
source(here::here("helper functions/codebook.R"))

log_df <- init_log()
log_df <- log_event(log_df, "06c_start", "script_started")

# Load Input -------------------------------------------------------------------

# Load data from 06b, if necessary

if (!exists("coding_paper_clean_6b", inherits = FALSE)) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^06b_full_paper_sample_deduplicated_cleaned_comments_checked(_\\d{8}_\\d{4})?\\.xlsx$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching cleaned and comment addressed codings of full papers found, please run script 06b.cleaning.comments first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.xlsx$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading cleaned, deduplicated codings of full papers from: ", timestamps[which.max(timestamps)])
  
  coding_paper_clean_6c <- readxl::read_excel(latest_file)
  
  #clean house
  rm(timestamps, dir_path, files, latest_file)
}


# 6.1 Check Parameters -------------------------------------------------------------------

coder_col <- "V5"
vars_to_check <- c("V10","V11","V12","V13")

codes_long <- purrr::map_dfr(vars_to_check, ~explode_codes(coding_paper_clean_6c, .x, coder_col))

levels_long <- bind_rows(
  levels_V7 %>% transmute(variable = "V7", code = as.character(V7), label = V7_label),
  levels_V10 %>% transmute(variable = "V10", code = as.character(V10), label = V10_label),
  levels_V11 %>% transmute(variable = "V11", code = as.character(V11), label = V11_label),
  levels_V12 %>% transmute(variable = "V12", code = as.character(V12), label = V12_label),
  levels_V13 %>% transmute(variable = "V13", code = as.character(V13), label = V13_label)
)

# 6.2 Check Distributions ----------------------------------------------------------------

dist_by_coder <- codes_long %>%
  count(variable, coder, code, name = "n") %>%
  left_join(levels_long, by = c("variable","code")) %>%
  arrange(variable, code, coder)

dist_total <- codes_long %>%
  count(variable, code, name = "n_total") %>%
  group_by(variable) %>%
  mutate(
    total_codes_in_var = sum(n_total),
    share_total = n_total / total_codes_in_var
  ) %>%
  ungroup() %>%
  left_join(levels_long, by = c("variable","code")) %>%
  arrange(variable, n_total)

# 6.3 Manually identified codes to double-check ------------------------------------

suspect_codes <- tibble::tribble(
  ~variable, ~code, ~label,                    
  "V11",     "14",  "Data donation", 
  "V11",     "17",  "Tracking",    
  "V11",     "25",  "Quantitative observation"
)

log_df <- log_event(
  log_df,
  step   = "06c_manual_definition",
  action = "define_suspect_codes",
  note   = paste0("Manually defined suspect codes for inspection: ", paste(paste0(suspect_codes$variable, "=", suspect_codes$code), collapse = ", "))
)

suspect_cases <- codes_long %>%
  inner_join(suspect_codes, by = c("variable","code")) %>%
  left_join(coding_paper_clean_6c, by = "id_unique") %>%
  arrange(variable, code, coder, id_unique)

# 6.4 Manual fixes -------------------------------------------------------------

fixes <- tibble::tribble(
  ~id_unique, ~var,   ~or_value,               ~new_value,               ~note,
  "ID69",     "V11",  "12; 13; 16; 17; 24",    "12; 13; 16; 24",         "Study doesn't do tracking, removed code; they 'identify and track social media communities on social media",
  "ID825",    "V11",  "17; 16; 18",            "16; 18",                 "Misunderstanding, study doesn't do tracking; they track social media communities",
  "ID26",     "V11",  "13; 16; 17",            "13; 16",                 "Misunderstanding, study doenst do tracking; use a method to monitor Twitter activity around the protests",
  "ID267",    "V11",  "13; 16; 17; 26",        "13; 16; 26",             "they use a custom-built software to track Facebook’s tracking",
  "ID1367",   "V11",  "13; 14",                "13",                     "data was provided by bit.ly (platform operator) to the researchers",
  "ID625",    "V11",  "25; 99",                "99",                     "controlled online field experiment",
  "ID156",    "V11",  "25",                    "99",                     "one-shot public goods game laboratory experiment undertaken",
  "ID993",    "V11",  "21; 13; 26; 12; 25; 23","22; 23; 26; 12; 21; 13", "must be a misunderstanding/typo; they did surveys",
  "ID2142",   "V11",  "21; 22; 25",            "21; 22; 26",             "they did surveys; typo",
  "ID1970",   "V11",  "21; 25; 26",            "26",                     "All experiment analyses rely on the same data source, an online survey"
)

fixes_apply <- fixes %>% dplyr::select(id_unique, var, new_value, note)

tmp <- apply_manual_edits(
  df = coding_paper_clean_6c,
  edits = fixes_apply,
  log_df = log_df,
  step = "06c_code_review",
  action = "manual_edit"
)
coding_paper_clean_6c <- tmp$df; log_df <- tmp$log_df

# 6.5 Reviewed, no change ------------------------------------------------------

no_change <- tibble::tribble(
  ~id_unique,  ~or_value,       ~new_value,      ~note,
)

for (i in seq_len(nrow(no_change))) {
  log_df <- log_event(
    log_df,
    step   = "06c_code_review",
    action = "no_change_documented",
    note   = paste0(no_change$id_unique[i], ": ", no_change$note[i])
  )
}

# 6.6 Export -----------------------------------------------------------------------

out_dir <- PATHS$final
log_dir <- PATHS$logs
stamp <- format(Sys.time(), "%Y%m%d_%H%M")

out_coding_paper_cleaned_6c <- file.path(out_dir, paste0("full_paper_sample_final_", stamp, ".xlsx"))
openxlsx::write.xlsx(coding_paper_clean_6c, out_coding_paper_cleaned_6c, overwrite = TRUE)

log_file <- file.path(log_dir, paste0("06c_code_check_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv"))
write_log(log_df, log_file)

message("06c completed.")
message("- Final dataset at: ", out_coding_paper_cleaned_6c)
message("- Log written to: ", log_file)
