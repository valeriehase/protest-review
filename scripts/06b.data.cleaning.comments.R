#
# Data Cleaning of Final Coded Data - Comments Check
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

if (!exists("PATHS", inherits = TRUE)) source(here::here("R/paths.R"))
if (!exists("IN",    inherits = TRUE)) source(here::here("R/config.R"))
source(here::here("R/logging.R"))
source(here::here("R/helpers.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(openxlsx)

log_df <- init_log()
log_df <- log_event(log_df, "06b_start", "script_started")

# Load Input -------------------------------------------------------------------

input_file <- require_file(file.path(PATHS$int, "06a_full_paper_sample_deduplicated_cleaned.xlsx"), "cleaned deduplicated coded full-paper sample (output of step 06a)")

message("Reading cleaned deduplicated sample from: ", input_file)
df <- readxl::read_excel(input_file)

# 6.3 Check Comments -----------------------------------------------------------

if ("Comment_CODER" %in% names(df)) {
  comments_long <- df %>%
    dplyr::filter(!is.na(Comment_CODER), stringr::str_trim(Comment_CODER) != "") %>%
    dplyr::mutate(Comment = stringr::str_squish(Comment_CODER)) %>%
    tidyr::separate_rows(Comment, sep = ";\\s*") %>%
    tidyr::extract(
      Comment,
      into = c("variable", "comment_text"),
      regex = "^(V\\d{1,2}):\\s*(.*)$",
      remove = TRUE
    ) %>%
    dplyr::mutate(
      variable = stringr::str_trim(variable),
      comment_text = stringr::str_trim(comment_text)
    ) %>%
    dplyr::filter(!is.na(variable), variable != "")
  
  print(comments_long)
}

# --- Manual fixes -------------------------------------------------------

fixes <- tibble::tribble(
  ~id_unique, ~var,  ~value,        ~note,
  "ID11",     "V13", "1",           "quasi-experiment meets experiment criterion", 
  "ID1072",   "V10", "100",         "focus on digital comm (memes); not platform-specific analysis",
  "ID1703",   "V7",  "10",          "global/diaspora focus; no clear regional assignment",
  "ID1703",   "V8",  "NA",          "no clear country assignment",
  "ID248",    "V11", "13; 16",      "semantic network with Gephi -> network analysis added",
  "ID83",     "V11", "13; 16; 99",  "archive/database + network analysis; removed scraping/API codes",
  "ID131",    "V11", "20",          "theoretical discussion of case studies",
  "ID1355",   "V11", "20",          "theoretical discussion of case studies",
  "ID1390",   "V11", "20",          "theoretical case-study discussion"
)

for (i in seq_len(nrow(fixes))) {
  id  <- fixes$id_unique[i]
  var <- fixes$var[i]
  val <- fixes$value[i]
  
  df[[var]] <- ifelse(df$id_unique == id, val, as.character(df[[var]]))
}

for (i in seq_len(nrow(fixes))) {
  log_df <- log_event(
    log_df,
    step   = "06b_comment_review",
    action = "manual_edit",
    note   = paste0(fixes$id_unique[i], ": ", fixes$var[i], " -> ", fixes$value[i], " | ", fixes$note[i])
  )
}

# --- Reviewed, no change --------------------------------------------

no_change <- tibble::tribble(
  ~id_unique, ~note,
  "ID1033", "method checked; keep coding",
  "ID1092", "experiment checked; evidence in text; keep coding",
  "ID1174", "method checked; keep coding",
  "ID12",   "qualitative frame analysis clear; keep coding",
  "ID1273", "platform assignment supported; keep coding",
  "ID2054", "region assignment supported; keep coding",
  "ID2449", "UK single-country; not cross-national; keep coding",
  "ID391",  "website analysis fits code 111; keep coding",
  "ID13",   "pan-european fits; keep coding",
  "ID1463", "global/diaspora fits; keep coding"
)

for (i in seq_len(nrow(no_change))) {
  log_df <- log_event(
    log_df,
    step   = "06b_comment_review",
    action = "no_change_documented",
    note   = paste0(no_change$id_unique[i], ": ", no_change$note[i])
  )
}


# --- Clear reviewed comments --------------------------------------------------

checked_ids <- unique(c(fixes$id_unique, no_change$id_unique))

df <- df %>%
  dplyr::mutate(
    Comment_CODER = dplyr::if_else(id_unique %in% checked_ids, NA_character_, Comment_CODER)
  )

log_df <- log_event(
  log_df,
  step   = "06b_comment_review",
  action = "clear_reviewed_comments",
  note   = paste0("Comment_CODER emptied for checked IDs: ", toString(checked_ids))
)

# Output -----------------------------------------------------------------------

out_dir <- PATHS$int
log_dir <- PATHS$logs

out_df_comments <- file.path(out_dir, "06b_full_paper_sample_deduplicated_cleaned_comments_checked.xlsx")
openxlsx::write.xlsx(df, out_df_comments, overwrite = TRUE)

log_file <- file.path(log_dir, paste0("06b_comments_check_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv"))
write_log(log_df, log_file)

message("06b completed.")
message("- Cleaned dataset (comments checked) at: ", out_df_cleaned_comments)
message("- Log written to: ", log_file)




