#
# Final Analysis - Tables
# Author: Miriam Milzner, Valerie Hase
# Date: 2025-10-24
#
# Setup ------------------------------------------------------------------------

if (!exists("PATHS", inherits = TRUE)) source(here::here("R/paths.R"))
if (!exists("IN",    inherits = TRUE)) source(here::here("R/config.R"))
if (!exists("require_file", inherits = TRUE)) source(here::here("R/helpers.R"))
source(here::here("R/codebook.R"))

library(readxl)
library(tidyverse)
library(tidycomm)
library(janitor)
library(stringr)
library(openxlsx)
library(flextable)
library(officer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Input -------------------------------------------------------------------

input_file <- require_file(file.path(PATHS$int, "full_paper_sample_deduplicated_cleaned_comments_checked_codes_checked.xlsx"), "analysis input dataset (output of step 06b)")

message("Reading analysis dataset from: ", input_file)
df <- readxl::read_excel(input_file)

# 7.1 Prepare Data -----------------------------------------------------------------

df <- df %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),  # anpassung variablennamen
    starts_with("V")) 

str(df)

vars <- c("V7", "V10", "V10_agg", "V11", "V12", "V13", "method")

df <- df %>%
  mutate(method = str_trim(method)) %>%    
  filter(method %in% c("0", "1")) %>%
  filter(!is.na(method)) %>%
  distinct(id_unique, .keep_all = TRUE) # remove duplicates. Note to self double check duplicates while cleaning the data

df_V10agg <- make_df_V10agg(df)

# 7.2 Frequency Tables -------------------------------------------------

N_total <- nrow(df)
apa_note <- paste0(
  "Note. CSS = computational social science; n = frequency; % = percentage.\n",
  "Percentages are calculated within each method group.\n",
  "N = ", N_total, " studies."
)

table_specs <- list(
  V7       = "Frequencies of Regions",
  V10      = "Frequencies of Platforms",
  V10_agg  = "Frequencies of Platforms (aggregated)",
  V11      = "Frequencies of Analysis Methods",
  V12      = "Frequency of Cross-National Research Designs",
  V13      = "Frequency of Experimental Research Designs"
)

apa_tables <- lapply(names(table_specs), function(var) {
  df_used <- if (var == "V10_agg") df_V10agg else df
  make_complete_table(
    df_used, var, get(paste0("levels_", var)),
    apa = TRUE, title = table_specs[[var]], note = apa_note
  )
})
names(apa_tables) <- names(table_specs)

# 7.3 Chi-Square ------------------------------------------------------

res_V7  <- chi_method_table(
  dataset = df, dep_var = "V7",
  table_caption = "Chi-square Test: Method (CSS vs. non-CSS) × V7 (Regions)",
  note_text = "Note. Expanded rows after splitting multi-coded V7; N counts region mentions (not unique studies). Excluding NA. Method: 0 = Non-CSS, 1 = CSS.",
  multi = TRUE
)
ft_chi_v7  <- res_V7$ft

res_V10 <- chi_method_table(
  dataset = df_V10agg, dep_var = "V10_agg",
  table_caption = "Chi-square Test: Method (CSS vs. non-CSS) × V10_agg (Aggregated Platforms)",
  note_text = "Note. Expanded rows after splitting multi-coded V10; N counts platform mentions (not unique studies). Excluding NA. Method: 0 = Non-CSS, 1 = CSS."
)
ft_chi_v10 <- res_V10$ft

res_V12 <- chi_method_table(
  dataset = df, dep_var = "V12",
  table_caption = "Chi-square Test: Method (CSS vs. non-CSS) × V12 (Cross-national Design)",
  note_text = "Note. V12: 1 = cross-national, 0 = not cross-national. Excluding NA. Method: 0 = Non-CSS, 1 = CSS."
)
ft_chi_v12 <- res_V12$ft

res_V13 <- chi_method_table(
  dataset = df, dep_var = "V13",
  table_caption = "Chi-square Test: Method (CSS vs. non-CSS) × V13 (Experimental Design)",
  note_text = "Note. V13: 1 = experiment, 0 = not experiment. Excluding NA. Method: 0 = Non-CSS, 1 = CSS."
)
ft_chi_v13 <- res_V13$ft

# 7.4 Cross Tables ----------------------------------------------------------------

selected_platforms <- c("100","110","130","140")
levels_V11_subset <- levels_V11 %>% filter(V11 %in% c("21","22","23","24","25","26"))

note_platforms <- "Note. Table limited to the four most frequently used platform types. Percentages are calculated within each platform column, separately for CSS and Non-CSS studies."
note_designs   <- "Note. Columns show cross-national research design. Percentages are calculated within each design column, separately for CSS and Non-CSS studies."

# V11 × V10_agg (CSS split; nur 6 V11 & 4 Plattformen)
ft_V11_platforms <- build_crosstab_pct(
  df_rows = df_V10agg,
  row_var = "V11", levels_row_df = levels_V11_subset,
  col_var = "V10_agg", levels_col_df = levels_V10_agg,
  selected_rows = levels_V11_subset$V11,
  selected_cols = selected_platforms,
  split_by_method = TRUE,
  method_var = "method", method_levels = c("0","1"),
  method_labels = c("Non-CSS %","CSS %"),
  title = "Analysis Methods × Major Platform Types — % within platform",
  note  = note_platforms
)

# V11 × V12 (CSS split; gleiche V11-Auswahl)
ft_V11_V12 <- build_crosstab_pct(
  df_rows = df,
  row_var = "V11", levels_row_df = levels_V11_subset,
  col_var = "V12", levels_col_df = levels_V12,
  selected_rows = levels_V11_subset$V11,
  selected_cols = c("1","0"),
  split_by_method = TRUE,
  method_var = "method", method_levels = c("0","1"),
  method_labels = c("Non-CSS %","CSS %"),
  title = "Analysis Methods × Cross-National Design — % within design",
  note  = note_designs
)

# V12 × V10_agg (OVERALL; ohne CSS-Splitting)
ft_V12_platforms <- build_crosstab_pct(
  df_rows = df_V10agg,
  row_var = "V12", levels_row_df = levels_V12,
  col_var = "V10_agg", levels_col_df = levels_V10_agg,
  selected_cols = selected_platforms,
  split_by_method = FALSE,
  title = "Cross-National Designs × Major Platform Types — % within platform (overall)",
  note  = "Note. Column percentages are computed overall (CSS and Non-CSS combined)."
)

# 7.5 MethodCombination Table ----------------------------------------------------------------

qual_codes  <- c("21","22","23")
quant_codes <- c("24","25","26")
css_codes   <- c("10","11","12","13","14","15","16","17","18")

df_method_combo <- df %>%
  mutate(V11 = as.character(V11),
         method = as.character(method)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = stringr::str_trim(V11)) %>%
  group_by(id_unique, method) %>%
  summarise(
    has_qual  = any(V11 %in% qual_codes,  na.rm = TRUE),
    has_quant = any(V11 %in% quant_codes, na.rm = TRUE),
    has_css   = any(V11 %in% css_codes,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MethodCombo = dplyr::case_when(
      has_qual  & !has_quant & !has_css ~ "Q_ONLY",
      !has_qual &  has_quant & !has_css ~ "QT_ONLY",
      !has_qual & !has_quant &  has_css ~ "CSS_ONLY",
      has_qual  &  has_quant & !has_css ~ "MIX_QUAL_QUANT",
      !has_qual &  has_quant &  has_css ~ "MIX_QUANT_CSS",
      has_qual  & !has_quant &  has_css ~ "MIX_QUAL_CSS",
      has_qual  &  has_quant &  has_css ~ "MIX_ALL",
      TRUE ~ "Uncoded"
    )
  )

levels_MethodCombo <- tibble::tibble(
  MethodCombo = c(
    "Q_ONLY","QT_ONLY","CSS_ONLY",
    "MIX_QUAL_QUANT","MIX_QUANT_CSS","MIX_QUAL_CSS","MIX_ALL",
    "Uncoded"
  ),
  MethodCombo_label = c(
    "Qualitative only",
    "Quantitative only",
    "CSS only",
    "Mixed Qual + Quant",
    "Mixed Quant + CSS",
    "Mixed Qual + CSS",
    "Fully Mixed (Qual + Quant + CSS)",
    "Not mentioned"
  )
)

N_combo <- nrow(df_method_combo)
apa_note_combo <- paste0(
  "Note. CSS = computational social science; n = frequency; % = percentage.\n",
  "Percentages are calculated within each method group.\n",
  "N = ", N_combo, " studies."
)

ft_methodcombo <- make_complete_table(
  df         = df_method_combo,
  var        = "MethodCombo",
  levels_df  = levels_MethodCombo,
  apa        = TRUE,
  title      = "Frequencies of Method Combination Types",
  note       = apa_note_combo
)

res_combo <- chi_method_table(
  dataset       = df_method_combo,
  dep_var       = "MethodCombo",
  table_caption = "Chi-square Test: Method (CSS vs. non-CSS) × Method Combination Type",
  note_text     = "Note. One combination per study. Method: 0 = Non-CSS, 1 = CSS."
)
ft_chi_methodcombo <- res_combo$ft

# 7.6 CrossPlatform Table ----------------------------------------------------------------

df_platform_scope <- df %>%
  dplyr::mutate(V10 = as.character(V10),
                method = as.character(method)) %>%
  tidyr::separate_rows(V10, sep = ";") %>%
  dplyr::mutate(
    V10 = stringr::str_trim(V10),
    V10 = dplyr::na_if(V10, ""),
    V10 = dplyr::if_else(V10 == "NA", NA_character_, V10)
  ) %>%
  dplyr::group_by(id_unique, method) %>%
  dplyr::summarise(
    n_platforms = dplyr::n_distinct(V10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    PlatformScope = dplyr::case_when(
      n_platforms == 0 ~ "NA",
      n_platforms == 1 ~ "SINGLE",
      n_platforms  > 1 ~ "CROSS",
      TRUE ~ "NA"
    )
  )

levels_PlatformScope <- tibble::tibble(
  PlatformScope        = c("SINGLE", "CROSS", "NA"),
  PlatformScope_label  = c("Single platform", "Cross platform", "Not mentioned")
)

N_scope <- nrow(df_platform_scope)
apa_note_scope <- paste0(
  "Note. CSS = computational social science; n = frequency; % = percentage.\n",
  "Percentages are calculated within each method group.\n",
  "N = ", N_scope, " studies."
)

ft_platform_scope <- make_complete_table(
  df        = df_platform_scope,
  var       = "PlatformScope",
  levels_df = levels_PlatformScope,
  apa       = TRUE,
  title     = "Frequencies of Study Platform Scope (Single vs. Cross)",
  note      = apa_note_scope
)

res_scope <- chi_method_table(
  dataset       = df_platform_scope,
  dep_var       = "PlatformScope",
  table_caption = "Chi-square Test: Method (CSS vs. non-CSS) × Platform Scope",
  note_text     = "Note. Platform scope derived from V10. Method: 0 = Non-CSS, 1 = CSS.",
  multi         = FALSE
)
ft_chi_scope <- res_scope$ft

# 7.7 Prepare Word Document ----------------------------------------------------

landscape <- officer::prop_section(page_size = page_size(orient = "landscape"))

doc <- officer::read_docx() %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_set_default_section(landscape)

chi_tables <- list(
  V7      = ft_chi_v7,
  V10_agg = ft_chi_v10,
  V12     = ft_chi_v12,
  V13     = ft_chi_v13
)

for (i in seq_along(apa_tables)) {
  var <- names(apa_tables)[i]
  
  doc <- apa_table(
    doc    = doc,
    number = i,
    title  = table_specs[[var]],
    ft     = apa_tables[[i]]
  )
  
  if (var %in% names(chi_tables)) {
    doc <- doc %>%
      officer::body_add_flextable(chi_tables[[var]]) %>%
      officer::body_add_break()
  }
}

# Supplement: Cross Tables
doc <- doc %>%
  body_add_par("Table (supplement): V11 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V12 × V10_agg (overall %)", style = "Normal") %>%
  body_add_flextable(ft_V12_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V11 × V12 (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_V12) %>%
  body_add_break()

# Supplement: MethodCombo
doc <- doc %>%
  body_add_par("Table: Frequencies of Method Combination Types", style = "Normal") %>%
  body_add_flextable(ft_methodcombo) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): Method × Method Combination (Chi-square)", style = "Normal") %>%
  body_add_flextable(ft_chi_methodcombo) %>%
  body_add_break()

# Supplement: CrossPlatform
 doc <- doc %>%
   body_add_par("Table: Frequencies of Study Platform Scope", style = "Normal") %>%
   body_add_flextable(ft_platform_scope) %>%
   body_add_break() %>%
   body_add_par("Table (supplement): Method × Platform Scope (Chi-square)", style = "Normal") %>%
   body_add_flextable(ft_chi_scope) %>%
   body_add_break()

 # Output ----------------------------------------------------------------------

 out_dir <- PATHS$final
 stamp   <- format(Sys.time(), "%Y%m%d_%H%M")
 out_file <- file.path(out_dir, paste0("07_analysis_tables_", stamp, ".docx"))
 
 print(doc, target = out_file)
 
 message("07 tables completed.")
 message("- Word document saved to: ", out_file)
 
 