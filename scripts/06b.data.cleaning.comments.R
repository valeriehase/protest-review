#
# Data Cleaning of Final Coded Data - Comments Check
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Performs post-coding clarification based on coder comments indicating uncertainty or codebook misinterpretation. 
# Revisions were made through centralized adjudication by the senior coder (MM) using consistent decision rules.
# All changes are logged for transparency.
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

# --- Manual fixes -------------------------------------------------------------

fixes <- tibble::tribble(
  ~id_unique, ~var,      ~or_value,              ~new_value,   ~note,
  "ID11",     "V13",     "0",                    "1",          "quasi-experiment meets experiment criterion", 
  "ID1072",   "V10",     "NA",                   "100",        "focus on digital comm (memes); no platform-specific analysis",
  "ID1703",   "V7",      "5; 7",                 "10",         "global/diaspora focus",
  "ID1703",   "V8",      "Sri Lanka; Palestine", "NA",         "no clear country assignment",
  "ID248",    "V11",     "13",                   "13; 16",     "semantic network with Gephi -> network analysis added",
  "ID83",     "V11",     "18; 12",               "13; 16; 99", "archive/database + network analysis; removed scraping/API codes",
  "ID131",    "V11",     "NA",                   "20",         "no method section; theoretical discussion of case studies; change code to 20",
  "ID1355",   "V11",     "NA",                   "20",         "no method section; theoretical discussion of case studies; change code to 20",
  "ID1390",   "V11",     "21",                   "20",         "no method section; theoretical case-study discussion; change code to 20",
  "ID1846",   "V11",     "21",                   "20; 21",     "uses cyber-cartography and evaluate images; added code 20",
  "ID206",    "V11",     "23",                   "20",         "no method section; theoretical discussion of feminist literature; change code to 20",
  "ID2394",   "V13",     "1",                    "0",          "agent-based modeling no experiment; change code to 0",
  "ID866",    "V11",     "23",                   "20",         "no method section; historical and theoretical article; change code to 20",
  "ID907",    "V11",     "23",                   "21",         "no method section; broad qual analysis of online posts; change code to 21",
  "ID2438",   "V11",     "23",                   "21",         "no method section; analyzes  public  statements,  media accounts,  and  secondary  literature; change code from 23 to 21",
  "ID306",    "V11",     "24; 13",               "13",         "no method section; method article using forecasting (machine learning), human coding as training data; delete 24 and keep only code 13 for V11",
  "ID447",    "V10",     "151",                  "100",        "no method section; refers to transmedia testimonio through social media (general); change code 151 to 100 for V10; keep code for V11",
  "ID700",    "V10",     "100",                  "131",        "comment says 'no method section', but there is; change V10 coding from 100 to 131 (Facebook) accordingly; keep code '12; 21' for V11", 
  "ID2286",   "V11",     "23",                   "20",         "no method section; theoretical discussion of case studies; change code to 20",
  "ID2281",   "V11",     "21; 22",               "20; 21; 22", "no comment section; but describes clearly what it does 'theoretical piece with data/analysis from previous studies; add code 20"
  )

fixes_apply <- fixes %>% dplyr::select(id_unique, var, new_value, note)

tmp <- apply_manual_edits(
  df = df,
  edits = fixes_apply,
  log_df = log_df,
  step = "06b_comment_review",
  action = "manual_edit"
)
df <- tmp$df; log_df <- tmp$log_df

# --- Reviewed, no change ------------------------------------------------------

no_change <- tibble::tribble(
  ~id_unique, ~note,
  "ID1033",  "no method section; method checked; keep coding for V10/V11",
  "ID1092",  "experiment checked; evidence in text; keep coding",
  "ID1174",  "no method section; method checked; keep coding for V10/V11",
  "ID12",    "V11 qualitative frame analysis clear; keep coding",
  "ID1273",  "V10 platform assignment supported; keep coding",
  "ID2054",  "V7 region assignment supported; keep coding",
  "ID2449",  "UK single-country; not cross-national; keep coding",
  "ID391",   "website analysis fits code V10 = 111; keep coding",
  "ID13",    "V7 pan-european fits; keep coding",
  "ID1463",  "V7 global/diaspora fits; keep coding",
  "ID1468",  "no method section; method/platform checked; keep coding for V10/V11",
  "ID1501",  "no method section; method/platform checked; keep coding for V10/V11",
  "ID1523",  "no method section; method/platform checked; keep coding for V10/V11",
  "ID1543",  "no method section but analyzes 'how humour is discursively constituted'; keep coding for V10/V11",
  "ID1553",  "comment is explanation of V6 protest case coding; keep coding",
  "ID1616",  "no method section; discussion of two researchers that could be labeled 'ethnographic observation'; keep coding for V10/V11",
  "ID162",   "comment is explanation for V7 coding; agent-based modeling of non-real-world data (no region present) = NA",
  "ID1621",  "no method section; qualitative interviews and articles/pictures; keep coding for V10/V11",
  "ID1723",  "no method section; moral evaluation of the question 'Can we think of Anonymous as being good/bad?'; keep coding for V10/V11",
  "ID1810",  "comment gives full list of countries 'Germany, Greece, Spain, France, Italy, Poland, Switzerland, the United Kingdom, Sweden'; keep coding",
  "ID1835",  "no method section; ethnographic field work and qualitative eval of lyrics/material/song distributed on Facebook; keep coding for V10/V11",
  "ID184",   "no method section; but method is very well documented; keep coding for V10/V11",
  "ID1956",  "no method section; but qualitative analysis of image on social media is present; keep coding for V10/V11",
  "ID1957",  "no methiod section; but qual analysis of two audio memes from TikTok; keep coding for V10/V11",
  "ID2086",  "no method section; but examines mediated discourse; keep coding for V10/V11",
  "ID2141",  "no method section; examines key texts; keep coding for V10/V11",
  "ID2217",  "no method section; theoretical discussion and illustration; keep code 20 for V10/V11",
  "ID347",   "no method section; commentary; keep code 20 for V11 and code 100 for V10",
  "ID403",   "comment is explanatin for V7 coding; 'rallies showing solidarity with the movement were held in more than 64 cities across the world'; keep code 10 for V7", 
  "ID423",   "comment is explanation for V12 coding; transnational = crossnational; keep code 1 for V12",
  "ID495",   "explanation of code 21 in V11 = argumentative analysis; keep coding",
  "ID50",    "explanation for V7 coding; case involves not only local protests but also international grievances; keep code 10 for V7",
  "ID561",   "no method section; theory article; keep code 20 for V11 and code 100 for V10",
  "ID631",   "this article reviews sampling strategies; keep code 20 for V11",
  "ID82",    "comment is explanation for V11 coding; spatial analysis as network = 16; keep coding",
  "ID947",   "explanation for V11 coding; conceptual paper but relies on findings from a previous project and methods; keep code",
  "ID109",   "comment is note on CSS code in non-CSS sample; checked in 06a; no changes here",
  "ID879",   "comment is explanation for V12 coding; environmental websites from several countries but not analyzed as cross-national; keep code 0 for V12",
  "ID23",    "no method section; but abstract describes method clearly; keep coding for V10 and V11",
  "ID2130",  "no method section; but abstract describes method clearly; keep coding for V10 and V11"
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

if ("Comment_CODER" %in% names(df)) {
  
  df <- df %>%
    dplyr::mutate(
      Comment_CODER = dplyr::if_else(
        id_unique %in% checked_ids, NA_character_, Comment_CODER)
      )
  
  if (all(is.na(df$Comment_CODER))) {
    
    df <- df %>% dplyr::select(-Comment_CODER)
    
    message("All coder comments have been reviewed, logged, and the Comment_CODER column was removed.")
    
    log_df <- log_event(
      log_df,
      step   = "06b_comment_review",
      action = "drop_comment_column",
      note   = "All Comment_CODER entries are NA after review; column removed.")
  }
}

# Output -----------------------------------------------------------------------

out_dir <- PATHS$int
log_dir <- PATHS$logs

out_df_comments <- file.path(out_dir, "06b_full_paper_sample_deduplicated_cleaned_comments_checked.xlsx")
openxlsx::write.xlsx(df, out_df_comments, overwrite = TRUE)

log_file <- file.path(log_dir, paste0("06b_comments_check_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".tsv"))
write_log(log_df, log_file)

message("06b completed.")
message("- Cleaned dataset (comments checked) at: ", out_df_comments)
message("- Log written to: ", log_file)




