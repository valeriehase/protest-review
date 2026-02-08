# Helper APA Tables  ------------------------------------------------------------

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

# Chi-Square helper (mit Cramér's V) ------------------------------------------
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

# Crosstab Builder ----------------------------------------
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

