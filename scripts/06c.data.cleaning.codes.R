#
# Data Cleaning of Final Coded Data - Distribution of Rare Codes
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

if (!exists("PATHS", inherits = TRUE)) source(here::here("R/paths.R"))
if (!exists("IN",    inherits = TRUE)) source(here::here("R/config.R"))
if (!exists("write_log", inherits = TRUE)) source(here::here("R/logging.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

log_df <- init_log()
log_df <- log_event(log_df, "06c_start", "script_started")

# Load Input -------------------------------------------------------------------

input_file <- require_file(file.path(PATHS$int, "full_paper_sample_deduplicated_cleaned_comments_checked.xlsx"), "cleaned full-paper sample with comments checked (output of step 06b)")

message("Reading cleaned dataset (comments checked) from: ", input_file)
df <- readxl::read_excel(input_file)

# 6.4 Check code distributions -----------------------------------------

coder_col <- "V5"   
vars_to_check <- paste0("V", 10:13)

code_distribution_by_coder <- purrr::map_dfr(vars_to_check, function(var) {
  
  df_clean %>%
    select(id_unique, all_of(coder_col), all_of(var)) %>%
    mutate(
      variable = var,
      value = as.character(.data[[var]]),
      coder = as.character(.data[[coder_col]])
    ) %>%
    filter(!is.na(coder)) %>%
    filter(!is.na(value), value != "") %>%
    separate_rows(value, sep = ";\\s*") %>%
    mutate(
      code = str_trim(value)
    ) %>%
    filter(code != "") %>%
    count(variable, coder, code, name = "n") %>%
    arrange(variable, code, coder)
})

print(code_distribution_by_coder)

# Inspect specific V11 codes

v11_codes_of_interest <- c("14", "17", "25", "99")

v11_flagged <- df_clean %>%
  select(id_unique, V5, V11, everything()) %>%
  mutate(V11_chr = as.character(V11)) %>%
  filter(
    is.na(V11_chr) |
      str_detect(
        V11_chr,
        paste0("(^|;\\s*)(", paste(v11_codes_of_interest, collapse = "|"), ")(;|$)")
      )
  )

print(v11_flagged)

log_event(
  step   = "06_code_distributions_by_coder",
  action = "check_code_frequencies",
  note   = paste0(
    "Code-Verteilungen V10–V13 getrennt nach Kodiererin auf Auffälligkeiten geprüft."
  )
)

#tbc
