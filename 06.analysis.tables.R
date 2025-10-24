########################
#
# Tables
# Author: Miriam Milzner, Valerie Hase
# Date: 2025-10-24
#
########################
#
# Packages ---------------------------------------------------------------------

suppressPackageStartupMessages({
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
})

# 1. Prepare Data -----------------------------------------------------------------

df <- read_excel("data/processed/full_paper_sample_coded_clean.xlsx")

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

levels_V7 <- tibble(
  V7 = c(as.character(1:10), "NA"),
  V7_label = c(
    "North America",
    "South America",
    "Europe",
    "Russia and former Soviet Republics",
    "Middle East and North Africa",
    "East Asia",
    "Central and South (East) Asia",
    "Sub-Saharan Africa",
    "Oceania",
    "Global",
    "Not mentioned"
  )
)

levels_V10 <- tibble(
  V10 = c(
    "100", "110", "111", "112", "113", "114",
    "120", "121", "122", "124",
    "130", "131", "132", "133", "134",
    "140", "141", "142",
    "150", "151", "152", "153",
    "160", "161", "162",
    "200", "201", "202", "203", "204",
    "300", "400", "NA"
  ),
  V10_label = c(
    "Internet / online / social media (general)", "Websites (general)", "Websites of activist groups / NGOs / parties / companies", "News Media (online)", "Partisan news outlets", "Blogs",
    "Knowledge communities & discussion", "Specific discussion forum(s)", "Discord", "Reddit",
    "Social networking sites (general)", "Facebook", "Instagram", "TikTok", "VKontakte",
    "Microblogs", "X/Twitter", "Weibo", 
    "Video-sharing sites", "YouTube", "Vimeo", "Twitch",
    "Photo-sharing sites", "Flickr", "Tumblr",
    "Messengers (general)", "WhatsApp", "Telegram", "Signal", "WeChat",
    "News Media (offline)", "Other", "Not mentioned"
  )
)

df_V10agg <- df %>%
  tidyr::separate_rows(V10, sep = ";") %>%
  dplyr::mutate(V10 = stringr::str_trim(as.character(V10))) %>%
  dplyr::mutate(
    V10_agg = dplyr::case_when(
      V10 == "100" ~ "100",
      V10 %in% c("110","111","112","113","114") ~ "110",
      V10 %in% c("120","121","122","124") ~ "120",
      V10 %in% c("130","131","132","133","134") ~ "130",
      V10 %in% c("140","141","142") ~ "140",
      V10 %in% c("150","151","152","153") ~ "150",
      V10 %in% c("160","161","162") ~ "160",
      V10 %in% c("200","201","202","203","204") ~ "200",
      V10 == "300" ~ "300",
      V10 == "400" ~ "400",
      V10 %in% c("NA", NA) ~ "NA",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::mutate(V10_agg = as.character(V10_agg))

levels_V10_agg <- tibble(
  V10_agg = c("100", "110", "120", "130", "140", "150", "160", "200", "300", "400", "NA"),
  V10_agg_label = c(
    "Internet / online / social media (general)",
    "Websites generally",
    "Knowledge communities & discussion",
    "Social networking sites",
    "Microblogs",
    "Video-sharing sites",
    "Photo-sharing sites",
    "Messengers",
    "News Media (offline)",
    "Other",
    "Not mentioned"
  )
)


levels_V11 <- tibble(
  V11 = c(
    "10", "11","12","13","14","15","16","17","18",
    "20", "21","22","23","24","25","26",
    "99","NA"
  ),
  V11_label = c(
    "CSS-related (general)", 
    "Agent-based models / simulations", "API access", "Automated content analysis", "Data donation", "Eye-tracking", "Network analysis", "Tracking", "Web scraping",
    "Not-CSS-related (general)",
    "Qualitative content analysis", "Qualitative interviews / focus groups", "Qualitative observation", "Quantitative content analysis", "Quantitative observation", "Quantitative survey",
    "Other", "Not mentioned"
  )
)

levels_V12 <- tibble(
  V12 = c(
    "1",
    "0"
  ),
  V12_label = c(
    "cross-national",
    "not cross-national"
  )
)

levels_V13 <- tibble(
  V13 = c(
    "1",
    "0"
  ),
  V13_label = c(
    "experiment",
    "not experiment"
  )
)

# 2.1 Helper APA Tables  ------------------------------------------------------------

make_complete_table <- function(df, var, levels_df, 
                                apa = FALSE, title = NULL, note = NULL) {
  var_sym <- rlang::sym(var)
  lab_col <- paste0(var, "_label")
  
  df <- df %>% dplyr::mutate(!!var_sym := as.character(.data[[var]]))
  levels_df <- levels_df %>% dplyr::mutate(!!var_sym := as.character(.data[[var]]))
  
  # frequency table
  tab <- df %>%
    tidyr::separate_rows(!!var_sym, sep = ";") %>%
    dplyr::mutate(!!var_sym := dplyr::coalesce(trimws(!!var_sym), "Missing")) %>%
    dplyr::count(method, !!var_sym, name = "n") %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()
  
  # totals per method (for headers)
  totals <- tab %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(N = sum(n), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = method, values_from = N, names_prefix = "N_")
  
  n0 <- totals$N_0 %||% 0
  n1 <- totals$N_1 %||% 0
  
  # cross table with all categories and both methods
  tab_complete <- tidyr::expand_grid(!!var_sym := levels_df[[var]], method = c("0", "1")) %>%
    dplyr::left_join(tab, by = c(var, "method")) %>%
    dplyr::mutate(dplyr::across(c(n, pct), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::left_join(levels_df, by = var) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = c(n, pct),
      names_glue = "{.value}_{method}"
    ) %>%
    dplyr::rename(
      Code = !!var_sym,
      Category = !!rlang::sym(lab_col),
      !!paste0("Non-CSS n (n=", n0, ")") := n_0,
      "Non-CSS %" = pct_0,
      !!paste0("CSS n (n=", n1, ")") := n_1,
      "CSS %" = pct_1
    ) %>%
    dplyr::select(Code, Category, tidyselect::everything())
  
  if (!apa) return(tab_complete)
  
  ft <- tab_complete %>%
    flextable::flextable() %>%
    flextable::theme_vanilla() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = "Category", align = "left", part = "body") %>%
    flextable::set_caption(title %||% paste("Table for", var)) %>%
    flextable::autofit()
  
  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(ft, values = note)
  }
  ft
}

apa_table <- function(doc, number, title, ft) {
  ft <- ft %>% autofit() %>% width(width = 1)
  doc %>%
    body_add_par(paste0("Table ", number), style = "Normal") %>%
    body_add_par(title, style = "Normal") %>%
    body_add_flextable(ft) %>%
    body_add_break()
}

# 2.2 Chi-Square helper (mit Cramér's V) ------------------------------------------
chi_method_table <- function(dataset, dep_var, table_caption, note_text, multi = FALSE){
  dep_sym <- rlang::sym(dep_var)
  data_use <- dataset
  
  if (multi) {
    data_use <- data_use %>%
      tidyr::separate_rows(!!dep_sym, sep = ";") %>%
      mutate(
        !!dep_sym := str_trim(as.character(!!dep_sym)),
        !!dep_sym := case_when(is.na(!!dep_sym) ~ "NA", !!dep_sym == "" ~ "NA", TRUE ~ !!dep_sym),
        method = as.character(method)
      ) %>%
      filter(.data[[dep_var]] != "NA")
  }
  
  chi_obj <- data_use %>%
    mutate(
      method   = as.character(method),
      !!dep_sym := as.character(.data[[dep_var]]),
      !!dep_sym := case_when(is.na(!!dep_sym) ~ "NA", .data[[dep_var]] == "" ~ "NA", TRUE ~ !!dep_sym)
    ) %>%
    filter(.data[[dep_var]] != "NA") %>%
    tidycomm::crosstab(method, !!dep_sym, chi_square = TRUE)
  
  mod       <- attr(chi_obj, "model") %>% purrr::pluck(1)
  chi_sq    <- as.numeric(mod$statistic)
  obs       <- mod$observed
  n_total   <- sum(obs)
  r         <- nrow(obs); c <- ncol(obs)
  k         <- max(min(r, c) - 1, 1)       # Guard gegen k=0
  cramers_v <- sqrt(chi_sq / (n_total * k))
  
  chi_tib <- tibble::tibble(
    Metric = c("Test", "df", "χ²", "p", "N", "Cramér's V"),
    Value  = c(
      paste0("Method (CSS vs. non-CSS) × ", dep_var),
      as.character(mod$parameter),
      formatC(chi_sq, format = "f", digits = 3),
      formatC(as.numeric(mod$p.value), format = "f", digits = 3),
      as.character(n_total),
      formatC(cramers_v, format = "f", digits = 3)
    )
  )
  
  ft <- flextable(chi_tib) %>%
    theme_vanilla() %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "body") %>%
    set_caption(table_caption) %>%
    autofit() %>%
    add_footer_lines(values = note_text)
  
  list(ft = ft, chi_table = chi_tib, model = mod, chi_sq = chi_sq, cramers_v = cramers_v)
}

# 2.3 Crosstab Builder ----------------------------------------
build_crosstab_pct <- function(
    df_rows,
    row_var, levels_row_df,
    col_var, levels_col_df,
    selected_cols   = NULL,
    selected_rows   = NULL,
    split_by_method = TRUE,
    method_var      = "method",
    method_levels   = c("0","1"),
    method_labels   = c("Non-CSS %","CSS %"),
    title           = NULL,
    note            = NULL,
    percent_digits  = 1
) {
  row_sym <- rlang::sym(row_var); col_sym <- rlang::sym(col_var); meth_sym <- rlang::sym(method_var)
  row_lab <- paste0(row_var, "_label"); col_lab <- paste0(col_var, "_label")
  
  if (is.null(selected_cols)) selected_cols <- as.character(levels_col_df[[col_var]])
  if (is.null(selected_rows)) selected_rows <- as.character(levels_row_df[[row_var]])
  selected_cols <- as.character(selected_cols); selected_rows <- as.character(selected_rows)
  
  col_levels_df <- levels_col_df %>%
    filter(.data[[col_var]] %in% selected_cols) %>%
    mutate(!!col_sym := factor(.data[[col_var]], levels = selected_cols))
  
  base <- df_rows %>%
    mutate(!!row_sym := as.character(.data[[row_var]]),
           !!col_sym := as.character(.data[[col_var]])) %>%
    tidyr::separate_rows(!!row_sym, sep = ";") %>%
    tidyr::separate_rows(!!col_sym, sep = ";") %>%
    mutate(!!row_sym := str_trim(!!row_sym),
           !!col_sym := str_trim(!!col_sym)) %>%
    filter(.data[[col_var]] %in% selected_cols,
           .data[[row_var]] %in% selected_rows)
  
  value_cols <- character(0)
  
  if (split_by_method) {
    base <- base %>% filter(.data[[method_var]] %in% method_levels)
    
    tab <- base %>%
      count(!!meth_sym, !!row_sym, !!col_sym, name = "n") %>%
      group_by(!!meth_sym, !!col_sym) %>%
      mutate(pct = round(100 * n / sum(n), percent_digits)) %>%
      ungroup() %>%
      select(-n)
    
    full_grid <- tidyr::expand_grid(
      !!row_sym := selected_rows,
      !!col_sym := selected_cols,
      !!meth_sym := factor(method_levels, levels = method_levels)
    )
    
    out_long <- full_grid %>%
      left_join(tab, by = c(row_var, col_var, method_var)) %>%
      mutate(pct = tidyr::replace_na(pct, 0)) %>%
      left_join(levels_row_df, by = setNames(row_var, row_var)) %>%
      left_join(col_levels_df, by = setNames(col_var, col_var)) %>%
      mutate(MethodLabel = dplyr::recode(as.character(.data[[method_var]]),
                                         !!!stats::setNames(method_labels, method_levels)))
    
    wide <- out_long %>%
      select(RowLabel = !!sym(row_lab), ColLabel = !!sym(col_lab), MethodLabel, pct) %>%
      tidyr::pivot_wider(id_cols = RowLabel, names_from = c(ColLabel, MethodLabel), values_from = pct)
    
    col_labels <- col_levels_df[[col_lab]]
    ordered <- c("RowLabel", as.vector(rbind(
      paste0(col_labels, "_", method_labels[1]),
      paste0(col_labels, "_", method_labels[2])
    )))
    existing <- intersect(ordered, names(wide))
    wide <- wide[, c("RowLabel", existing[existing != "RowLabel"]), drop = FALSE]
    value_cols <- setdiff(names(wide), "RowLabel")
    
    header_top    <- c("", rep(col_labels, each = length(method_labels)))
    header_bottom <- c("", rep(method_labels, times = length(col_labels)))
    colkeys <- names(wide)
    header_df <- data.frame(col_keys = colkeys, Group = header_top, Method = header_bottom, stringsAsFactors = FALSE)
    
    ft <- flextable(wide, col_keys = colkeys) %>%
      set_header_df(mapping = header_df, key = "col_keys") %>%
      merge_h(part = "header") %>%
      align(align = "center", part = "all") %>%
      align(j = "RowLabel", align = "left", part = "body") %>%
      theme_vanilla() %>%
      set_caption(title %||% paste(row_var, "×", col_var, "— % within column")) %>%
      colformat_num(j = value_cols, digits = percent_digits, big.mark = "") %>%
      fontsize(size = 9, part = "all") %>%
      padding(padding = 2, part = "all") %>%
      compose(part = "header", i = 1, j = "RowLabel", value = as_paragraph("")) %>%
      compose(part = "header", i = 2, j = "RowLabel", value = as_paragraph(""))
    
  } else {
    tab <- base %>%
      count(!!row_sym, !!col_sym, name = "n") %>%
      group_by(!!col_sym) %>%
      mutate(pct = round(100 * n / sum(n), percent_digits)) %>%
      ungroup() %>%
      select(-n)
    
    full_grid <- tidyr::expand_grid(!!row_sym := selected_rows, !!col_sym := selected_cols)
    
    out_long <- full_grid %>%
      left_join(tab, by = c(row_var, col_var)) %>%
      mutate(pct = tidyr::replace_na(pct, 0)) %>%
      left_join(levels_row_df, by = setNames(row_var, row_var)) %>%
      left_join(col_levels_df, by = setNames(col_var, col_var))
    
    wide <- out_long %>%
      select(RowLabel = !!sym(row_lab), ColLabel = !!sym(col_lab), pct) %>%
      tidyr::pivot_wider(id_cols = RowLabel, names_from = ColLabel, values_from = pct)
    
    col_labels <- col_levels_df[[col_lab]]
    keep <- intersect(col_labels, names(wide))
    wide <- wide[, c("RowLabel", keep), drop = FALSE]
    value_cols <- setdiff(names(wide), "RowLabel")
    
    ft <- flextable(wide) %>%
      theme_vanilla() %>%
      align(align = "center", part = "all") %>%
      align(j = "RowLabel", align = "left", part = "body") %>%
      set_caption(title %||% paste(row_var, "×", col_var, "— % within column (overall)")) %>%
      colformat_num(j = value_cols, digits = percent_digits, big.mark = "") %>%
      fontsize(size = 9, part = "all") %>%
      padding(padding = 2, part = "all")
  }
  
  total_width <- 9.5; label_w <- 3.8
  other_w <- (total_width - label_w - 0.4) / max(1, length(value_cols))
  ft <- ft %>% width(j = "RowLabel", width = label_w) %>% width(j = value_cols, width = other_w)
  
  if (!is.null(note)) ft <- add_footer_lines(ft, values = note)
  ft
}


# 3. Frequency Tables -------------------------------------------------

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

# 4. Chi-Square ------------------------------------------------------
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

# 5. Cross Tables ----------------------------------------------------------------

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

# 6. MethodCombination Table ----------------------------------------------------------------

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

# 6. CrossPlatform Table ----------------------------------------------------------------

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

# Export to Word  -------------------------------------------

landscape <- prop_section(page_size = page_size(orient = "landscape"))

doc <- read_docx() %>%
  body_add_par("", style = "Normal") %>%
  body_set_default_section(landscape)

for (i in seq_along(apa_tables)) {
  var <- names(apa_tables)[i]
  doc <- apa_table(doc, number = i, title = table_specs[[var]], ft = apa_tables[[i]])
  
  # Supplement: Chi-Square
  if (var == "V7")      doc <- doc %>% body_add_flextable(ft_chi_v7)  %>% body_add_break()
  if (var == "V10_agg") doc <- doc %>% body_add_flextable(ft_chi_v10) %>% body_add_break()
  if (var == "V12")     doc <- doc %>% body_add_flextable(ft_chi_v12) %>% body_add_break()
  if (var == "V13")     doc <- doc %>% body_add_flextable(ft_chi_v13) %>% body_add_break()
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
 
print(doc, target = "output/tables/All_Tables.docx")