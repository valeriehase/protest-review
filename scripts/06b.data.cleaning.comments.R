#
# Data Cleaning of Final Coded Data - Comments Check
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
log_df <- log_event(log_df, "06b_start", "script_started")

# Load Input -------------------------------------------------------------------

input_file <- require_file(file.path(PATHS$int, "full_paper_sample_deduplicated_cleaned.xlsx"), "cleaned deduplicated coded full-paper sample (output of step 06a)")

message("Reading cleaned deduplicated sample from: ", input_file)
df <- readxl::read_excel(input_file)

# 6.3 Check Comments --------------------------------------------------------

comments_long <- df_clean %>%
  filter(!is.na(Comment_CODER)) %>%
  mutate(Comment = str_squish(Comment_CODER)) %>%
  separate_rows(Comment, sep = ";\\s*") %>%
  extract(Comment, into = c("variable", "comment_text"),
          regex = "^(V\\d{1,2}):\\s*(.*)$", remove = TRUE) %>%
  mutate(
    variable = str_trim(variable),
    comment_text = str_trim(comment_text)
  ) %>%
  filter(!is.na(variable), variable != "")

print(comments_long)


# --- Manual adjustments based on comments_long review -------------------------

df_clean <- df_clean %>%
  mutate(
    V13 = if_else(id_unique == "ID11",   "1",        as.character(V13)),
    V10 = if_else(id_unique == "ID1072", "100",      as.character(V10)),
    V7  = if_else(id_unique == "ID1703", "10",       as.character(V7)),
    V8  = if_else(id_unique == "ID1703", "NA",       as.character(V8)),
    V11 = if_else(id_unique == "ID248",  "13; 16",   as.character(V11)),
    V11 = if_else(id_unique == "ID83",   "13; 16; 99", as.character(V11)),
    V11 = if_else(id_unique == "ID131",  "20",       as.character(V11)),
    V11 = if_else(id_unique == "ID1355", "20",       as.character(V11)),
    V11 = if_else(id_unique == "ID1390", "20",       as.character(V11))
  )

log_df <- log_event(
  log_df,
  step   = "06b_comment_review",
  action = "review_comments",
  note   = "Kommentare aus Comment_CODER extrahiert und manuell geprüft."
)

# --- Logging: changes ---------------------------------------------------------

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID11: V13 0 -> 1 | Grund: quasi-experimentelles Design erfüllt Experiment-Kriterium")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID1072: V10 NA -> 100 | Grund: Fokus auf digitale Kommunikation (Memes), keine plattformspezifische Analyse")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID1703: V7 8 -> 10; V8 Israel/Palästina -> NA | Grund: global/Diaspora-Fokus, keine eindeutige Länderzuordnung")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID248: V11 13 -> 13; 16 | Grund: semantisches Netzwerk mit Gephi, Netzwerkanalyse ergänzt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID83: V11 18; 12 -> 13; 16; 99 | Grund: Datenbank/Archiv + Netzwerkanalyse; Scraping/API-Codes entfernt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID131: V11 NA -> 20 | Grund: theoretische Diskussion von Case Studies")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID1355: V11 NA -> 20 | Grund: theoretische Case-Study-Diskussion")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "manual_edit",
          "ID1390: V11 21 -> 20 | Grund: theoretische Case-Study-Modell-Diskussion")


# --- Logging: reviewed, no change --------------------------------------------

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID1033: method geprüft | Grund: Methodenteil vorhanden, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID1092: experiment geprüft | Grund: Beleg im Text vorhanden, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID1174: method geprüft | Grund: Methodik ausreichend beschrieben, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID12: method geprüft | Grund: qualitative Frame-Analyse klar beschrieben, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID1273: plattform geprüft | Grund: Plattformzuordnung ausreichend belegt, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID2054: V7 geprüft | Grund: MEA-/Regionenbezug passt zu Code 10, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID2449: V12 geprüft | Grund: nationaler Fall (UK), nicht cross-national, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID391: V10 geprüft | Grund: Website-Analyse passt zu Code 111, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID13: V7 geprüft | Grund: pan-european passt, Kodierung bleibt")

log_df <- log_event(
  log_df,
  step   = "06b_comment_review", "no_change_documented",
          "ID1463: V7 geprüft | Grund: global/diaspora passt, Kodierung bleibt")


# --- Clear reviewed comments --------------------------------------------------

checked_ids <- c(
  "ID11","ID1072","ID1703","ID248","ID83",
  "ID131","ID1355","ID1390",
  "ID1033","ID1092","ID1174","ID12","ID1273","ID2054","ID2449","ID391",
  "ID13","ID1463"
)

df_clean <- df_clean %>%
  mutate(
    Comment_CODER = if_else(id_unique %in% checked_ids, NA_character_, Comment_CODER)
  )

log_df <- log_event(
  log_df,
  step   = "06b_comment_review",
  action = "clear_reviewed_comments",
  note   = paste0("Comment_CODER geleert für geprüfte Fälle: ", toString(checked_ids))
)

# Output -----------------------------------------------------------------------

out_dir <- PATHS$int
log_dir <- PATHS$logs

out_df_cleaned_comments <- file.path(out_dir, "full_paper_sample_deduplicated_cleaned_comments_checked.xlsx")
openxlsx::write.xlsx(df_clean, out_df_cleaned_comments, overwrite = TRUE)

log_file <- file.path(log_dir, paste0("06b_comments_check_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv"))
write_log(log_df, log_file)

message("06b completed.")
message("- Cleaned dataset (comments checked) at: ", out_df_cleaned_comments)
message("- Log written to: ", log_file)




