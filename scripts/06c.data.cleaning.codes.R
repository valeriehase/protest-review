#
# Data Cleaning of Final Coded Data - Distribution of Rare Codes
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

source(here::here("R/paths.R"))
source(here::here("R/config.R"))
source(here::here("R/logging.R"))
source(here::here("R/helpers.R"))
source(here::here("R/codebook.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

log_df <- init_log()
log_df <- log_event(log_df, "06c_start", "script_started")

# Load Input -------------------------------------------------------------------

input_file <- require_file(file.path(PATHS$int, "06b_full_paper_sample_deduplicated_cleaned_comments_checked.xlsx"), "cleaned full-paper sample with comments checked (output of step 06b)")

message("Reading cleaned dataset (comments checked) from: ", input_file)
df <- readxl::read_excel(input_file)

# Parameters -------------------------------------------------------------------

coder_col <- "V5"
vars_to_check <- c("V10","V11","V12","V13")

codes_long <- purrr::map_dfr(vars_to_check, ~explode_codes(df, .x, coder_col))

levels_long <- bind_rows(
  levels_V7 %>% transmute(variable = "V7", code = as.character(V7), label = V7_label),
  levels_V10 %>% transmute(variable = "V10", code = as.character(V10), label = V10_label),
  levels_V11 %>% transmute(variable = "V11", code = as.character(V11), label = V11_label),
  levels_V12 %>% transmute(variable = "V12", code = as.character(V12), label = V12_label),
  levels_V13 %>% transmute(variable = "V13", code = as.character(V13), label = V13_label)
)

# Distributions ----------------------------------------------------------------

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

# Manually identified codes to double-check ------------------------------------

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
  left_join(df, by = "id_unique") %>%
  arrange(variable, code, coder, id_unique)

# --- Manual fixes -------------------------------------------------------------

fixes <- tibble::tribble(
  ~id_unique,  ~or_value,             ~new_value,        ~note,
  "ID69",      "12; 13; 16; 17; 24",  "12; 13; 16; 24",  "Study doesn't do tracking, removed code; they 'identify and track social media communities on social media",
  "ID825",     "17; 16; 18",          "16; 18",          "Misunderstanding, study doesn't do tracking; they track social media communities",
  "ID26",      "13; 16; 17",          "13; 16",          "Misunderstanding, study doenst do tracking; use a method to monitor Twitter activity around the protests",
  "ID267",     "13; 16; 17; 26",      "13; 16; 26",      "they use a custom-built software to track Facebook’s tracking",
  "ID13",      "21; 24; 25",          "13; 24",          "content and computer-mediated discourse analysis on the one hand and correlational and logistic regression analysis",
  "ID156",     "25",                  "  ",     
  
)

for (i in seq_len(nrow(fixes))) {
  id  <- fixes$id_unique[i]
  var <- fixes$var[i]
  val <- fixes$new_value[i]
  
  df[[var]] <- ifelse(df$id_unique == id, val, as.character(df[[var]]))
}

for (i in seq_len(nrow(fixes))) {
  log_df <- log_event(
    log_df,
    step   = "06c_code_review",
    action = "manual_edit",
    note   = paste0(fixes$id_unique[i], ": ", fixes$var[i], " | ", fixes$or_value[i], " -> ", fixes$new_value[i], " | ", fixes$note[i])
  )
}

# --- Reviewed, no change ------------------------------------------------------

no_change <- tibble::tribble(
  ~id_unique,  ~or_value,       ~new_value,      ~note,
  "ID1367",   "14",             "14",            "data was provided by bit.ly (platform operator) to the researchers",

)

for (i in seq_len(nrow(no_change))) {
  log_df <- log_event(
    log_df,
    step   = "06c_code_review",
    action = "no_change_documented",
    note   = paste0(no_change$id_unique[i], ": ", no_change$note[i])
  )
}

# Output -----------------------------------------------------------------------

out_dir <- PATHS$final
log_dir <- PATHS$logs

out_df_final <- file.path(out_dir, "full_paper_sample_final.xlsx")
openxlsx::write.xlsx(df, out_df_final, overwrite = TRUE)

log_file <- file.path(log_dir, paste0("06c_code_check_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv"))
write_log(log_df, log_file)

message("06c completed.")
message("- Final dataset at: ", out_df_final)
message("- Log written to: ", log_file)








