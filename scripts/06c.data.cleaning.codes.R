#
# Data Cleaning of Final Coded Data - Distribution of Rare Codes
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/paths.R"))
source(here("R/config.R"))
source(here("R/logging.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

log_df <- init_log()

# Load Input -------------------------------------------------------------------

input_file <- if (exists("OUT") && !is.null(OUT$coded_full_sample_deduplicated_cleaned_comments)) {
  OUT$coded_full_sample_deduplicated_cleaned_comments
} else {
  here("data", "out", "coded_full_sample_deduplicated_cleaned_comments.xlsx")
}
stopifnot(file.exists(input_file))
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
