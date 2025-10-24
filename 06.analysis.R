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

# Helper Functions  ------------------------------------------------------------

#we create a function to exclude specific categories for figures
exclude <- function(df) {
  df %>%
    dplyr::filter(!Category %in% c("Other", "Not mentioned"))
}
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
                                         "CSS %"     = "CSS")) %>%
    #exclude specific cases for visualization
    exclude(.)
  
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

#build a function to create chi-square test tables

chi_method_table <- function(dataset,        # dataset we used
                             dep_var,        # dependent variable
                             table_caption,  # for flextable caption
                             note_text,      # note to put under table
                             multi = FALSE   # does dep_var have multiple codes like "1;10"?) 
){ 
  
  # turn var names into symbols
  dep_sym <- rlang::sym(dep_var)
  
  #include splitting into multiple variables (only for V7 regions)
  if (multi) {
    dataset <- dataset %>%
      tidyr::separate_rows(!!dep_sym, sep = ";") %>%
      dplyr::mutate(
        !!dep_sym := stringr::str_trim(as.character(!!dep_sym)),
        !!dep_sym := dplyr::case_when(
          is.na(!!dep_sym) ~ "NA",
          !!dep_sym == ""  ~ "NA",
          TRUE ~ !!dep_sym
        ),
        method = as.character(method)
      )
  }
  
  # prep data for chi-square:
  # - make sure both columns are character
  chi_obj <- dataset %>%
    dplyr::mutate(
      method   = as.character(method),
      !!dep_sym := as.character(.data[[dep_var]]),
      !!dep_sym := dplyr::case_when(
        is.na(!!dep_sym) ~ "NA",
        !!dep_sym == ""  ~ "NA",
        TRUE ~ !!dep_sym
      )
    ) %>%
    
    #run the test using tidycomm
    tidycomm::crosstab(method, !!dep_sym, chi_square = TRUE)
  
  # extract model + compute Cramér's V (not stored in crosstab command)
  mod        <- attr(chi_obj, "model") %>% purrr::pluck(1)
  chi_sq     <- as.numeric(mod$statistic)
  obs        <- mod$observed
  n_total    <- sum(obs)
  r          <- nrow(obs)
  c          <- ncol(obs)
  k          <- min(r, c) - 1
  cramers_v  <- sqrt(chi_sq / (n_total * k))
  
  # build APA-style summary tibble
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
  
  # build flextable
  ft <- flextable::flextable(chi_tib) %>%
    flextable::theme_vanilla() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::set_caption(table_caption) %>%
    flextable::autofit() %>%
    flextable::add_footer_lines(values = note_text)
  
  # return list 
  list(
    ft         = ft,
    chi_table  = chi_tib,
    model      = mod,
    chi_sq     = chi_sq,
    cramers_v  = cramers_v
  )
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

#additionally generate tables for Chi-Square

# Chi-square table for Method X V7 Regions
res_V7 <- chi_method_table(
  dataset         = df,
  dep_var         = "V7",
  table_caption   = "Chi-square Test: Method (CSS vs. non-CSS) X V7 (Regions)",
  note_text       = "Note. Chi-square computed on expanded rows after splitting multi-coded V7 entries (matching the figure’s approach). 'N (expanded)' counts region mentions, not unique studies. Excluding Region = NA. Method: 0 = Non-CSS, 1 = CSS.",
  multi           = T
)

ft_chi_v7 <- res_V7$ft

# Chi-square table for Method X V10 (Platforms, but aggregated)
res_V10 <- chi_method_table(
  dataset         = df_V10agg,
  dep_var         = "V10_agg",
  table_caption   = "Chi-square Test: Method (CSS vs. non-CSS) X V10 (platform aggregated)",
  note_text       = "Note. Chi-square computed on expanded rows after splitting multi-coded V10 entries (matching the figure’s approach). 'N (expanded)' counts platform mentions, not unique studies. Excluding Platform = NA. Method: 0 = Non-CSS, 1 = CSS."
)

ft_chi_v10 <- res_V10$ft

# Chi-square table for Method X V12 Cross-National
res_V12 <- chi_method_table(
  dataset         = df,
  dep_var         = "V12",
  table_caption   = "Chi-square Test: Method (CSS vs. non-CSS) × V12 (Cross-national Design)",
  note_text       = "Note. V12 coded as 1 = cross-national, 0 = not cross-national. Chi-square computed after excluding V12 = NA. Method: 0 = Non-CSS, 1 = CSS."
)

ft_chi_v12 <- res_V12$ft

# Chi-square table for Method X V13 Experimental
res_V13 <- chi_method_table(
  dataset         = df,
  dep_var         = "V13",
  table_caption   = "Chi-square Test: Method (CSS vs. non-CSS) × V13 (Experimental Design)",
  note_text       = "Note. V13 coded as 1 = experiment, 0 = not experiment. Chi-square computed after excluding V13 = NA. Method: 0 = Non-CSS, 1 = CSS."
)

ft_chi_v13 <- res_V13$ft

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
  
  # insert chi-square table for Method X V10_aggregated platforms right after Table 3
  if (i == 3) {
    doc <- doc %>%
      body_add_par("Table (supplement): Method (CSS vs. non-CSS) × V10_agg (Aggregated Platforms)", style = "Normal") %>%
      body_add_flextable(ft_chi_v10) %>%
      body_add_break()
  }
  
  # insert chi-square table for Method X design (experimental, cross-national) right after Table 5
  if (i == 5) {
    doc <- doc %>%
      body_add_par("Table (supplement): Method (CSS vs. non-CSS) × V12 (Cross-national)", style = "Normal") %>%
      body_add_flextable(ft_chi_v12) %>%
      body_add_break() %>%
      body_add_par("Table (supplement): Method (CSS vs. non-CSS) × V13 (Experimental)", style = "Normal") %>%
      body_add_flextable(ft_chi_v13) %>%
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
# helper: 1 per method grpoup
share_one <- function(.df, var) {
  .df %>%
    dplyr::mutate(val = as.character(.data[[var]]),
                  method = as.character(method)) %>%
    dplyr::filter(method %in% c("0","1")) %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_one   = sum(val == "1", na.rm = TRUE),
      pct_one = dplyr::if_else(n_total > 0, round(100 * n_one / n_total, 1), 0),
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
    Method = dplyr::recode(method, "0" = "Non-CSS", "1" = "CSS"),
    # zero percent labels
    y_lab  = dplyr::if_else(pct_one == 0, 0.5, pct_one)
  )

p_design <- ggplot(design_share, aes(x = Design, y = pct_one, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f%%", pct_one)),
            position = position_dodge(width = 0.8),
            vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "Percentage", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(design_share$y_lab, na.rm = TRUE) + 6)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 0, hjust = 0.5),
    plot.margin        = margin(10, 10, 10, 10)
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Cross-national and Experimental Designs", style = "Normal") %>%
  body_add_par("Share of studies with cross-national and experimental research designs by method group (CSS vs. Non-CSS)", style = "Normal") %>%
  body_add_gg(value = p_design, width = 9, height = 5) %>%
  body_add_break()

# Cross Tables ----------------------------------------------------------------

selected_platforms <- c("100","110","130","140")
# Entspricht:
# 100 = Internet / social media / online
# 110 = Websites generally
# 130 = Social Networking Sites
# 140 = Microblogs

levels_V11_subset <- levels_V11 %>%
  dplyr::filter(V11 %in% c("21","22","23","24","25","26"))
# Entspricht:
# 21 = Qualitative content analysis
# 22 = Qualitative interviews / focus groups
# 23 = Qualitative observation
# 24 = Quantitative content analysis
# 25 = Quantitative observation
# 26 = Quantitative survey

note_platforms <- "Note. Table limited to the four most frequently used platform types: Internet / online / social media (general); Websites general; Social Networking Sites; Microblogs.
Percentages are calculated within each platform column, separately for CSS and Non-CSS studies.
CSS = computational social science."

note_designs <- "Note. Columns show research design categories (cross-national; experimental).
Percentages are calculated within each research design column, separately for CSS and Non-CSS studies.
CSS = computational social science."

# Builder function

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
  row_sym <- rlang::sym(row_var)
  col_sym <- rlang::sym(col_var)
  meth_sym <- rlang::sym(method_var)
  row_lab <- paste0(row_var, "_label")
  col_lab <- paste0(col_var, "_label")
  
  if (is.null(selected_cols)) selected_cols <- as.character(levels_col_df[[col_var]])
  if (is.null(selected_rows)) selected_rows <- as.character(levels_row_df[[row_var]])
  selected_cols <- as.character(selected_cols)
  selected_rows <- as.character(selected_rows)
  
  col_levels_df <- levels_col_df %>%
    dplyr::filter(.data[[col_var]] %in% selected_cols) %>%
    dplyr::mutate(!!col_sym := factor(.data[[col_var]], levels = selected_cols))
  
  base <- df_rows %>%
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
    dplyr::filter(.data[[col_var]] %in% selected_cols,
                  .data[[row_var]] %in% selected_rows)
  
  value_cols <- character(0)
  
  if (split_by_method) {
    base <- base %>% dplyr::filter(.data[[method_var]] %in% method_levels)
    
    tab <- base %>%
      dplyr::count(!!meth_sym, !!row_sym, !!col_sym, name = "n") %>%
      dplyr::group_by(!!meth_sym, !!col_sym) %>%
      dplyr::mutate(pct = round(100 * n / sum(n), percent_digits)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n)
    
    full_grid <- tidyr::expand_grid(
      !!row_sym := selected_rows,
      !!col_sym := selected_cols,
      !!meth_sym := factor(method_levels, levels = method_levels)
    )
    
    out_long <- full_grid %>%
      dplyr::left_join(tab, by = c(row_var, col_var, method_var)) %>%
      dplyr::mutate(pct = tidyr::replace_na(pct, 0)) %>%
      dplyr::left_join(levels_row_df, by = setNames(row_var, row_var)) %>%
      dplyr::left_join(col_levels_df, by = setNames(col_var, col_var)) %>%
      dplyr::mutate(
        MethodLabel = dplyr::recode(as.character(.data[[method_var]]),
                                    !!!stats::setNames(method_labels, method_levels))
      )
    
    wide <- out_long %>%
      dplyr::select(RowLabel = !!rlang::sym(row_lab),
                    ColLabel = !!rlang::sym(col_lab),
                    MethodLabel, pct) %>%
      tidyr::pivot_wider(
        id_cols = RowLabel,
        names_from = c(ColLabel, MethodLabel),
        values_from = pct
      )
    
    col_labels <- col_levels_df[[col_lab]]
    ordered <- c("RowLabel",
                 as.vector(rbind(
                   paste0(col_labels, "_", method_labels[1]),
                   paste0(col_labels, "_", method_labels[2])
                 )))
    existing <- intersect(ordered, names(wide))
    wide <- wide[, c("RowLabel", existing[existing != "RowLabel"]), drop = FALSE]
    
    # value_cols direkt aus dem Dataframe
    value_cols <- setdiff(names(wide), "RowLabel")
    
    header_top    <- c("", rep(col_labels, each = length(method_labels)))
    header_bottom <- c("", rep(method_labels, times = length(col_labels)))
    colkeys <- names(wide)
    header_df <- data.frame(
      col_keys = colkeys,
      Group    = header_top,
      Method   = header_bottom,
      stringsAsFactors = FALSE
    )
    
    ft <- flextable::flextable(wide, col_keys = colkeys) %>%
      flextable::set_header_df(mapping = header_df, key = "col_keys") %>%
      flextable::merge_h(part = "header") %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::align(j = "RowLabel", align = "left", part = "body") %>%
      flextable::theme_vanilla() %>%
      flextable::set_caption(title %||% paste(row_var, "×", col_var, "— % within column")) %>%
      flextable::colformat_num(j = value_cols, digits = percent_digits, big.mark = "") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::padding(padding = 2, part = "all") %>%
      flextable::compose(part = "header", i = 1, j = "RowLabel", value = flextable::as_paragraph("")) %>%
      flextable::compose(part = "header", i = 2, j = "RowLabel", value = flextable::as_paragraph(""))
    
  } else {
    tab <- base %>%
      dplyr::count(!!row_sym, !!col_sym, name = "n") %>%
      dplyr::group_by(!!col_sym) %>%
      dplyr::mutate(pct = round(100 * n / sum(n), percent_digits)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n)
    
    full_grid <- tidyr::expand_grid(
      !!row_sym := selected_rows,
      !!col_sym := selected_cols
    )
    
    out_long <- full_grid %>%
      dplyr::left_join(tab, by = c(row_var, col_var)) %>%
      dplyr::mutate(pct = tidyr::replace_na(pct, 0)) %>%
      dplyr::left_join(levels_row_df, by = setNames(row_var, row_var)) %>%
      dplyr::left_join(col_levels_df, by = setNames(col_var, col_var))
    
    wide <- out_long %>%
      dplyr::select(RowLabel = !!rlang::sym(row_lab),
                    ColLabel = !!rlang::sym(col_lab),
                    pct) %>%
      tidyr::pivot_wider(id_cols = RowLabel,
                         names_from = ColLabel,
                         values_from = pct)
    
    col_labels <- col_levels_df[[col_lab]]
    keep <- intersect(col_labels, names(wide))
    wide <- wide[, c("RowLabel", keep), drop = FALSE]
    
    # value_cols direkt aus dem Dataframe
    value_cols <- setdiff(names(wide), "RowLabel")
    
    ft <- flextable::flextable(wide) %>%
      flextable::theme_vanilla() %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::align(j = "RowLabel", align = "left", part = "body") %>%
      flextable::set_caption(title %||% paste(row_var, "×", col_var, "— % within column (overall)")) %>%
      flextable::colformat_num(j = value_cols, digits = percent_digits, big.mark = "") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::padding(padding = 2, part = "all")
  }
  
  total_width <- 9.5
  label_w    <- 3.8
  n_value_cols <- length(value_cols)
  other_w <- (total_width - label_w - 0.4) / max(1, n_value_cols)
  
  ft <- ft %>%
    flextable::width(j = "RowLabel", width = label_w) %>%
    flextable::width(j = value_cols, width = other_w)
  
  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(ft, values = note)
  }
  ft
}



# 1) V11 × V10_agg mit CSS-Splitting, nur 6 V11-Methoden und 4 Plattformtypen
levels_V11_subset <- levels_V11 %>%
  dplyr::filter(V11 %in% c("21","22","23","24","25","26"))
selected_platforms <- c("100","110","130","140")

ft_V11_platforms <- build_crosstab_pct(
  df_rows       = df_V10agg,
  row_var       = "V11", levels_row_df = levels_V11_subset,
  col_var       = "V10_agg", levels_col_df = levels_V10_agg,
  selected_rows = levels_V11_subset$V11,    
  selected_cols = selected_platforms,       
  split_by_method = TRUE,                   
  method_var    = "method",
  method_levels = c("0","1"),
  method_labels = c("Non-CSS %","CSS %"),
  title = "Analysis Methods × Major Platform Types — % within platform",
  note  = note_platforms
)

# 2) V11 × V12 mit CSS-Splitting, gleiche V11-Auswahl
ft_V11_V12 <- build_crosstab_pct(
  df_rows       = df,
  row_var       = "V11", levels_row_df = levels_V11_subset,
  col_var       = "V12", levels_col_df = levels_V12,
  selected_rows = levels_V11_subset$V11,
  selected_cols = c("1","0"),               
  split_by_method = TRUE,
  method_var    = "method",
  method_levels = c("0","1"),
  method_labels = c("Non-CSS %","CSS %"),
  title = "Analysis Methods × Cross-National Design — % within design",
  note  = note_designs
)

# 3) V12 × V10_agg OVERALL (ohne CSS-Splitting), 4 Plattformtypen
ft_V12_platforms <- build_crosstab_pct(
  df_rows       = df_V10agg,
  row_var       = "V12", levels_row_df = levels_V12,
  col_var       = "V10_agg", levels_col_df = levels_V10_agg,
  selected_cols = selected_platforms,
  split_by_method = FALSE,                 
  title = "Cross-National Designs × Major Platform Types — % within platform (overall)",
  note  = "Note. Column percentages are computed overall (CSS and Non-CSS combined)."
)


# in word doc
doc <- doc %>%
  body_add_par("Table (supplement): V11 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V12 × V10_agg (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V12_platforms) %>%
  body_add_break() %>%
  body_add_par("Table (supplement): V11 × V12 (Non-CSS/CSS %)", style = "Normal") %>%
  body_add_flextable(ft_V11_V12) %>%
  body_add_break() 

# Cross Tables Figures ----------------------------------------------------------------

method_levels_vec <- c("0","1")  # Non-CSS, CSS

cross_counts <- df %>%
  dplyr::mutate(
    V11    = as.character(V11),
    V12    = as.character(V12),
    method = as.character(method)
  ) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  dplyr::mutate(V11 = stringr::str_trim(V11)) %>%
  dplyr::filter(
    method %in% method_levels_vec,
    V12 == "1",
    V11 %in% levels_V11_subset$V11
  ) %>%
  dplyr::count(method, V11, name = "n")

totals <- cross_counts %>%
  dplyr::group_by(method) %>%
  dplyr::summarise(total = sum(n), .groups = "drop")

v11_cross_share <- tidyr::expand_grid(
  method = method_levels_vec,
  V11    = levels_V11_subset$V11
) %>%
  dplyr::left_join(cross_counts, by = c("method","V11")) %>%
  dplyr::left_join(totals,      by = "method") %>%
  dplyr::mutate(
    n     = tidyr::replace_na(n, 0),
    total = tidyr::replace_na(total, 0),
    pct   = dplyr::if_else(total > 0, round(100 * n / total, 1), 0)
  ) %>%
  dplyr::left_join(levels_V11_subset, by = "V11") %>%
  dplyr::mutate(
    MethodGrp = dplyr::recode(method, "0" = "Non-CSS", "1" = "CSS"),
    V11_label = factor(V11_label, levels = levels_V11_subset$V11_label),
    y_lab     = dplyr::if_else(pct == 0, 0.5, pct)
  )

# Plot
p_v11_cross <- ggplot(v11_cross_share, aes(x = V11_label, y = pct, fill = MethodGrp)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)),
            position = position_dodge(width = 0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "% within design (cross-national)", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(v11_cross_share$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 35, hjust = 1)
  )


doc <- doc %>%
  body_add_par("Figure (supplement): Analysis Methods — % within design (cross-national only)", style = "Normal") %>%
  body_add_par("Bar chart of cross-national design by analysis method, split by CSS vs. Non-CSS. Matches the left block of the V11 × V12 table.", style = "Normal") %>%
  body_add_gg(value = p_v11_cross, width = 9, height = 5) %>%
  body_add_break()

# Figure 2

platform_cross_counts <- df_V10agg %>%
  dplyr::mutate(
    V10_agg = as.character(V10_agg),
    V12     = as.character(V12),
    method  = as.character(method)
  ) %>%
  dplyr::filter(
    method %in% method_levels_vec,   
    V12 == "1",                       
    V10_agg %in% selected_platforms
  ) %>%
  dplyr::count(method, V10_agg, name = "n")

# summe über plattformen, nur V12==1
platform_totals <- platform_cross_counts %>%
  dplyr::group_by(method) %>%
  dplyr::summarise(total = sum(n), .groups = "drop")

# raster
v10_cross_share <- tidyr::expand_grid(
  method  = method_levels_vec,
  V10_agg = selected_platforms
) %>%
  dplyr::left_join(platform_cross_counts, by = c("method","V10_agg")) %>%
  dplyr::left_join(platform_totals,      by = "method") %>%
  dplyr::mutate(
    n     = tidyr::replace_na(n, 0),
    total = tidyr::replace_na(total, 0),
    pct   = dplyr::if_else(total > 0, round(100 * n / total, 1), 0)
  ) %>%
  dplyr::left_join(levels_V10_agg, by = "V10_agg") %>%
  dplyr::mutate(
    MethodGrp   = dplyr::recode(method, "0" = "Non-CSS", "1" = "CSS"),
    V10_agg_label = factor(V10_agg_label,
                           levels = levels_V10_agg %>%
                             dplyr::filter(V10_agg %in% selected_platforms) %>%
                             dplyr::pull(V10_agg_label)),
    y_lab = dplyr::if_else(pct == 0, 0.5, pct)  # für sichtbare 0%-Labels
  )

# Plot
p_v10_cross <- ggplot(v10_cross_share, aes(x = V10_agg_label, y = pct, fill = MethodGrp)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)),
            position = position_dodge(width = 0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70", "white")) +
  labs(x = NULL, y = "% within design (cross-national)", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(v10_cross_share$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),   
    plot.margin        = margin(10, 30, 10, 30)                            
  )


doc <- doc %>%
  body_add_par("Figure (supplement): Platforms — % within design (cross-national only)", style = "Normal") %>%
  body_add_par("Bar chart of cross-national design by platform type (V10_agg), split by CSS vs. Non-CSS. Matches the left block of the V12 × V10_agg logic.", style = "Normal") %>%
  body_add_gg(value = p_v10_cross, width = 9, height = 5) %>%
  body_add_break()

# PRINT -----------------------------------------------------------------------

print(doc, target = "output/tables/All_Tables_and_Figures.docx")

