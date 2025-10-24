########################
#
# Analysis
# Author: Miriam Milzner, Valerie Hase
# Date: 2025-10-23
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
})

# Prepare Data -----------------------------------------------------------------

df_full_sample_coded <- read_excel("data/processed/full_paper_sample_coded_clean.xlsx")

df <- df_full_sample_coded %>%
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

# Prepare chi-Square test for cross table  ----------------------------------------------------------------

# Regions (V7) within CSS and non-CSS methods
chi_v7_method <- df %>%
  
  #we split multiple regions mentioned in the same paper (e.g., 1;10)
  tidyr::separate_rows(V7, sep = ";") %>%
  dplyr::mutate(V7 = stringr::str_trim(as.character(V7)),
                V7     = dplyr::case_when(is.na(V7) ~ "NA", V7 == "" ~ "NA", TRUE ~ V7),
                method = as.character(method)) %>%
  
  #remove NAs in V7, since this is something that will be cleaned
  filter(V7 != "NA") %>%
  
  #run chi square test, for now basic without bootstrapping etc.
  tidycomm::crosstab(method, V7, chi_square = TRUE)

# --- compute Cramér's V from the stored model ---
mod <- attr(chi_v7_method, "model") %>% purrr::pluck(1)
chi_sq <- as.numeric(mod$statistic)
obs    <- mod$observed
n      <- sum(obs)
r      <- nrow(obs)
c      <- ncol(obs)
k      <- min(r, c) - 1
cramers_v <- sqrt(chi_sq / (n * k))

# Assemble a compact results table (APA-style)
chi_v7_method <- tibble::tibble(
  Metric = c("Test", "df", "χ²", "p", "N", "Cramér's V"),
  Value  = c(
    #Test
    "Method (CSS vs. non-CSS) X V7 (Regions)",
    #df
    as.character(mod$parameter),
    #Chi-Square test statistic
    formatC(chi_sq, format = "f", digits = 3),
    #p
    formatC(as.numeric(mod$p.value), format = "f", digits = 3),
    #N
    as.character(n),
    #Cramers V
    formatC(cramers_v, format = "f", digits = 3)
  )
)

chi_note <- "Note. Chi-square computed on expanded rows after splitting multi-coded V7 entries (matching the figure’s approach). 'N (expanded)' counts region mentions, not unique studies. Excluding Region = NA. Method: 0 = Non-CSS, 1 = CSS."

ft_chi_v7 <- flextable::flextable(chi_v7_method) %>%
  flextable::theme_vanilla() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "body") %>%
  flextable::set_caption("Chi-square Test: Method (CSS vs. non-CSS) X V7 (Regions)") %>%
  flextable::autofit() %>%
  flextable::add_footer_lines(values = chi_note)

#clean house
rm(mod, chi_sq, obs, n, r, c, k, cramers_v, chi_note)

# Helper Functions  ------------------------------------------------------------

make_complete_table <- function(df, var, levels_df, 
                                apa = FALSE, title = NULL, note = NULL) {
  var_sym <- rlang::sym(var)
  df <- df %>% dplyr::mutate(!!var_sym := as.character(.data[[var]]))
  levels_df <- levels_df %>% dplyr::mutate(!!var_sym := as.character(.data[[var]]))
  
  # frequency table
  tab <- df %>%
    tidyr::separate_rows(!!var_sym, sep = ";") %>%
    dplyr::mutate(!!var_sym := dplyr::coalesce(trimws(!!var_sym), "Missing")) %>%
    dplyr::count(method, !!var_sym) %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()
  
  # totals per method (for headers)
  totals <- tab %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(N = sum(n), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = method, values_from = N,
                       names_prefix = "N_")
  
  # cross table with all categories and both methods
  tab_complete <- tidyr::expand_grid(!!var_sym := levels_df[[var]], method = c("0", "1")) %>%
    dplyr::left_join(tab, by = c(var, "method")) %>%
    dplyr::mutate(dplyr::across(c(n, pct), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::left_join(levels_df, by = var) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = c(n, pct),
      names_glue = "{.value}_{method}"
    ) %>%
    dplyr::rename(
      Code = !!var_sym,
      Category = paste0(var, "_label"),
      !!paste0("Non-CSS n (n=", totals$N_0, ")") := n_0,
      "Non-CSS %" = pct_0,
      !!paste0("CSS n (n=", totals$N_1, ")") := n_1,
      "CSS %" = pct_1
    ) %>%
    dplyr::select(Code, Category, tidyselect::everything())
  
  if (apa) {
    ft <- tab_complete %>%
      flextable::flextable() %>%
      flextable::theme_vanilla() %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::set_caption(title %||% paste("Table for", var)) %>%
      flextable::autofit()
    
    if (!is.null(note)) {
      ft <- flextable::add_footer_lines(ft, values = note)
    }
    return(ft)
  }
  
  tab_complete
}

apa_table <- function(doc, number, title, ft) {
  ft <- ft %>%
    autofit() %>%
    width(width = 1)
  
  doc %>%
    body_add_par(paste0("Table ", number), style = "Normal") %>%
    body_add_par(title, style = "Normal") %>%
    body_add_flextable(ft) %>%
    body_add_break()
}

apa_figure <- function(doc, number, title, df, category_var, levels_vec = NULL) {
  df_long <- df %>%
    dplyr::select(Category, dplyr::matches("%$")) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("%$"),
      names_to = "Method", values_to = "Percentage"
    ) %>%
    dplyr::mutate(Method = dplyr::recode(Method,
                                         "Non-CSS %" = "Non-CSS",
                                         "CSS %"     = "CSS"))
  
  # enforce level order
  if (!is.null(levels_vec)) {
    df_long$Category <- factor(df_long$Category, levels = levels_vec)
  }
  
  p <- ggplot(df_long, aes(x = Category, y = Percentage, fill = Method)) +
    geom_col(position = position_dodge(width = 0.8),
             width = 0.7, color = "black") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.8,
              size = 2) +
    scale_fill_manual(values = c("grey70","white")) +
    labs(x = category_var, y = "Percentage", fill = "Method Group") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 50, hjust = 1),
      plot.margin = margin(10, 10, 10,100)
    )
  
  doc %>%
    body_add_par(paste0("Figure ", number), style = "Normal") %>%
    body_add_par(title, style = "Normal") %>%
    body_add_gg(value = p, width = 9, height = 6) %>%
    body_add_break()
}



# Frequency Tables APA Export  -------------------------------------------------

apa_note <- "Note. CSS = computational social science; n = frequency; % = percentage. 
Percentages are calculated within each method group. 
N = 445 studies."

table_specs <- list(
  V7  = "Frequencies of Regions",
  V10 = "Frequencies of Platforms",
  V10_agg = "Frequencies of Platforms (aggregated)",
  V11 = "Frequencies of Analysis Methods",
  V12 = "Frequency of Cross-National Research Designs",
  V13 = "Frequency of Experimental Research Designs"
)

# generate all tables (flextables)
apa_tables <- lapply(names(table_specs), function(var) {
  df_used <- if (var == "V10_agg") df_V10agg else df
  
  make_complete_table(
    df_used, var, get(paste0("levels_", var)),
    apa   = TRUE,
    title = table_specs[[var]],
    note  = apa_note
  )
})

names(apa_tables) <- names(table_specs)

# Export to Word (Tables + Figures)  -------------------------------------------

landscape <- prop_section(
  page_size = page_size(orient = "landscape")
)

doc <- read_docx() %>% 
  body_add_par("", style = "Normal") %>% 
  body_set_default_section(landscape)

for (i in seq_along(apa_tables)) {
  var <- names(apa_tables)[i]
  
  df_used <- if (var == "V10_agg") df_V10agg else df
  
  # add APA table
  doc <- apa_table(doc,
                   number = i,
                   title  = table_specs[[var]],
                   ft     = apa_tables[[i]])
  
  # insert chi-square table for Method X V7 Regions right after Table 1
  if (i == 1) {
    doc <- doc %>%
      body_add_par("Table (supplement): Method (CSS vs. non-CSS) × V7 (Regions)", style = "Normal") %>%
      body_add_flextable(ft_chi_v7) %>%
      body_add_break()
  }
  
  # add APA figure
  tab_raw <- make_complete_table(df_used, var, get(paste0("levels_", var)))
  
  doc <- apa_figure(doc,
                    number = i,
                    title = paste("Distribution of", table_specs[[var]], "by Method Group"),
                    df    = tab_raw,
                    category_var = table_specs[[var]],
                    levels_vec   = get(paste0("levels_", var))[[paste0(var, "_label")]])
}



# Combined figure: Cross-national & Experimental --------------------------------

# helper to compute share of "1" within method group
share_one <- function(.df, var) {
  .df %>%
    dplyr::mutate(val = as.character(.data[[var]])) %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_one   = sum(val == "1", na.rm = TRUE),
      pct_one = round(100 * n_one / n_total, 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(variable = var)
}

v12_share <- share_one(df, "V12")
v13_share <- share_one(df, "V13")

design_share <- dplyr::bind_rows(v12_share, v13_share) %>%
  dplyr::mutate(
    Design = dplyr::recode(variable,
                           "V12" = "Cross-national",
                           "V13" = "Experimental"),
    Method = dplyr::recode(method, "0" = "Non-CSS", "1" = "CSS")
  )

p_design <- ggplot(design_share, aes(x = Design, y = pct_one, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "Percentage", fill = "Method Group") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Cross-national and Experimental Designs", style = "Normal") %>%
  body_add_par("Share of studies with cross-national and experimental research designs by method group (CSS vs. Non-CSS)", style = "Normal") %>%
  body_add_gg(value = p_design, width = 9, height = 5) %>%
  body_add_break()

# Cross Tables ----------------------------------------------------------------

selected_platforms <- c("100","110","130","140")

note_platforms <- "Note. Table limited to the four most frequently used platform types: Internet / online / social media (general); Websites general; Social Networking Sites; Microblogs.
Percentages are calculated within each platform column, separately for CSS and Non-CSS studies.
CSS = computational social science."

note_designs <- "Note. Columns show research design categories (cross-national; experimental).
Percentages are calculated within each research design column, separately for CSS and Non-CSS studies.
CSS = computational social science."

# Builder function
# Spaltenprozente getrennt nach CSS / non-CSS

build_pct_table_both_methods <- function(df_rows,
                                         row_var, levels_row_df,
                                         col_var, levels_col_df,
                                         selected_cols = NULL,
                                         title = NULL, note = NULL) {
  row_sym <- rlang::sym(row_var)
  col_sym <- rlang::sym(col_var)
  row_lab <- paste0(row_var, "_label")
  col_lab <- paste0(col_var, "_label")
  
  if (is.null(selected_cols)) selected_cols <- as.character(levels_col_df[[col_var]])
  selected_cols <- as.character(selected_cols)
  
  col_levels <- levels_col_df %>%
    dplyr::filter(.data[[col_var]] %in% selected_cols) %>%
    dplyr::mutate(!!col_sym := factor(.data[[col_var]], levels = selected_cols))
    
  # grid
  full_grid <- tidyr::expand_grid(
    !!row_sym := as.character(levels_row_df[[row_var]]),
    !!col_sym := selected_cols,
    method    = c("0","1")
  )
  
  # method, col; first cast then split
  tab <- df_rows %>%
    dplyr::mutate(
      !!row_sym := as.character(.data[[row_var]]),
      !!col_sym := as.character(.data[[col_var]])
    ) %>%
    tidyr::separate_rows(!!row_sym, sep = ";") %>%
    tidyr::separate_rows(!!col_sym, sep = ";") %>%
    dplyr::mutate(
      !!row_sym := stringr::str_trim(!!row_sym),
      !!col_sym := stringr::str_trim(!!col_sym)
    ) %>%
    dplyr::filter(method %in% c("0","1"),
                  .data[[col_var]] %in% selected_cols) %>%
    dplyr::count(method, !!row_sym, !!col_sym, name = "n") %>%
    dplyr::group_by(method, !!col_sym) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n)
  
  # zusammensetzen
  out_long <- full_grid %>%
    dplyr::left_join(tab, by = c("method", row_var, col_var)) %>%
    dplyr::mutate(pct = tidyr::replace_na(pct, 0)) %>%
    dplyr::left_join(levels_row_df, by = setNames(row_var, row_var)) %>%
    dplyr::left_join(col_levels,     by = setNames(col_var, col_var)) %>%
    dplyr::mutate(MethodLabel = dplyr::recode(method, "0" = "Non-CSS %", "1" = "CSS %"))
  
  # wide format
  out <- out_long %>%
    dplyr::select(RowLabel = !!rlang::sym(row_lab),
                  ColLabel = !!rlang::sym(col_lab),
                  MethodLabel, pct) %>%
    tidyr::pivot_wider(
      id_cols    = c(RowLabel),
      names_from = c(ColLabel, MethodLabel),
      values_from = pct
    ) %>%
    dplyr::left_join(levels_row_df %>% dplyr::select(RowLabel = !!rlang::sym(row_lab)),
                     by = "RowLabel") %>%
    dplyr::distinct(RowLabel, .keep_all = TRUE)

  col_labels <- col_levels[[col_lab]]
  ordered_cols <- c(
    "RowLabel",
    as.vector(rbind(
      paste0(col_labels, "_Non-CSS %"),
      paste0(col_labels, "_CSS %")
    ))
  )
  existing_cols <- names(out)
  ordered_cols <- c("RowLabel", intersect(ordered_cols[-1], existing_cols))
  out <- out[, ordered_cols, drop = FALSE]
  
  # header
  header_top    <- c("", rep(col_labels, each = 2))
  header_bottom <- c("", rep(c("Non-CSS %", "CSS %"), times = length(col_labels)))
  colkeys <- names(out)
  header_df <- data.frame(
    col_keys = colkeys,
    Group    = header_top,
    Method   = header_bottom,
    stringsAsFactors = FALSE
  )
  
  ft <- flextable::flextable(out, col_keys = colkeys) %>%
    flextable::set_header_df(mapping = header_df, key = "col_keys") %>%
    flextable::merge_h(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = "RowLabel", align = "left", part = "body") %>%
    flextable::theme_vanilla() %>%
    flextable::set_caption(title %||% "Row × Column — % within column") %>%
    flextable::colformat_num(j = setdiff(names(out), "RowLabel"), digits = 1, big.mark = "") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::padding(padding = 2, part = "all") %>%
    flextable::compose(part = "header", i = 1, j = "RowLabel", value = flextable::as_paragraph("")) %>%
    flextable::compose(part = "header", i = 2, j = "RowLabel", value = flextable::as_paragraph(""))
  
  # A4 format
  total_width <- 9.5
  label_w <- 3.8
  n_value_cols <- ncol(out) - 1
  other_w <- (total_width - label_w - 0.4) / max(1, n_value_cols)
  ft <- ft %>%
    flextable::width(j = "RowLabel", width = label_w) %>%
    flextable::width(j = setdiff(names(out), "RowLabel"), width = other_w)
  
  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(ft, values = note)
  }
  ft
}


# Methods (V11) × Major Platform Types (V10_agg)
ft_V11_platforms <- build_pct_table_both_methods(
  df_rows = df_V10agg,
  row_var = "V11", levels_row_df = levels_V11,
  col_var = "V10_agg", levels_col_df = levels_V10_agg,
  selected_cols = selected_platforms,
  title = "Analysis Methods × Major Platform Types — % within platform",
  note  = note_platforms
)

# Regions (V7) × Major Platform Types (V10_agg)
ft_V7_platforms <- build_pct_table_both_methods(
  df_rows = df_V10agg,
  row_var = "V7", levels_row_df = levels_V7,
  col_var = "V10_agg", levels_col_df = levels_V10_agg,
  selected_cols = selected_platforms,
  title = "Regions × Major Platform Types — % within platform",
  note  = note_platforms
)

# Cross-National (V12) × Major Platform Types (V10_agg)
ft_V12_platforms <- build_pct_table_both_methods(
  df_rows = df_V10agg,
  row_var = "V12", levels_row_df = levels_V12,
  col_var = "V10_agg", levels_col_df = levels_V10_agg,
  selected_cols = selected_platforms,
  title = "Cross-National Designs × Major Platform Types — % within platform",
  note  = note_platforms
)

# Experimental (V13) × Major Platform Types (V10_agg)
ft_V13_platforms <- build_pct_table_both_methods(
  df_rows = df_V10agg,
  row_var = "V13", levels_row_df = levels_V13,
  col_var = "V10_agg", levels_col_df = levels_V10_agg,
  selected_cols = selected_platforms,
  title = "Experimental Designs × Major Platform Types — % within platform",
  note  = note_platforms
)

# Methods (V11) × Cross-National (V12)
ft_V11_V12 <- build_pct_table_both_methods(
  df_rows = df,  # V12 liegt im Grund-df
  row_var = "V11", levels_row_df = levels_V11,
  col_var = "V12", levels_col_df = levels_V12,
  selected_cols = c("1","0"),     # Reihenfolge: cross-national, not cross-national
  title = "Analysis Methods × Cross-National Design — % within design",
  note  = note_designs
)

# Methods (V11) × Experimental (V13)
ft_V11_V13 <- build_pct_table_both_methods(
  df_rows = df,  # V13 liegt im Grund-df
  row_var = "V11", levels_row_df = levels_V11,
  col_var = "V13", levels_col_df = levels_V13,
  selected_cols = c("1","0"),     # Reihenfolge: experiment, not experiment
  title = "Analysis Methods × Experimental Design — % within design",
  note  = note_designs
)

# in word doc
doc <- doc %>%
  body_add_par("Table (supplement): V11 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V7 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V7_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V12 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V12_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V13 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V13_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V11 × V12 (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_V12) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V11 × V13 (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_V13) %>%
  body_add_break()

# PRINT -----------------------------------------------------------------------

print(doc, target = "output/tables/All_Tables_and_Figures.docx")