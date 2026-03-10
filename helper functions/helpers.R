# helpers.R --------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Small utilities 
# ------------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y
`%!in%` <- Negate(`%in%`)   # if you use it in multiple scripts, define it here

# ------------------------------------------------------------------------------
# 1) Input / Output helpers
# ------------------------------------------------------------------------------

require_file <- function(path, what = "input file") {
  if (is.null(path) || length(path) == 0) {
    stop("Missing ", what, ": path is NULL/empty.", call. = FALSE)
  }
  missing <- path[!file.exists(path)]
  if (length(missing) > 0) {
    stop("Missing ", what, ":\n- ", paste(missing, collapse = "\n- "), call. = FALSE)
  }
  invisible(path)
}

# ------------------------------------------------------------------------------
# 2) Data cleaning / transformations
# ------------------------------------------------------------------------------

exclude_other_not_mentioned <- function(df) {
  df %>% dplyr::filter(!Category %in% c("Other", "Not mentioned"))
}

make_df_V10agg <- function(df) {
  stopifnot(is.data.frame(df))
  if (!"V10" %in% names(df)) {
    stop("make_df_V10agg(): df has no column 'V10'.")
  }
  
  df %>%
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
}

explode_codes <- function(data, var, coder_col) {
  data %>%
    transmute(
      id_unique,
      coder = as.character(.data[[coder_col]]),
      variable = var,
      raw_value = as.character(.data[[var]])
    ) %>%
    filter(!is.na(coder), coder != "") %>%
    filter(!is.na(raw_value), str_trim(raw_value) != "") %>%
    separate_rows(raw_value, sep = ";\\s*") %>%
    mutate(code = str_trim(raw_value)) %>%
    filter(code != "") %>%
    select(id_unique, coder, variable, code)
}

normalize_semicolon_field <- function(x, to_lower = FALSE) {
  if (isTRUE(to_lower)) x <- stringr::str_to_lower(x)
  
  x %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\\s*[,/|]+\\s*", "; ") %>%  # alternative separators -> '; '
    stringr::str_replace_all("\\s*;\\s*", "; ") %>%       # standardize spaces around ;
    stringr::str_replace_all("^(;\\s*)+", "") %>%         # leading ;
    stringr::str_replace_all("(;\\s*)+$", "") %>%         # trailing ;
    stringr::str_replace_all("(;\\s*){2,}", "; ")         # collapse repeats
}

make_long_tokens <- function(df, var, sep = ";\\s*") {
  df %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    tidyr::separate_rows(!!rlang::sym(var), sep = sep) %>%
    dplyr::mutate(!!rlang::sym(var) := stringr::str_trim(.data[[var]]))
}

find_invalid_token_ids <- function(df, var, max_tokens = 3, numeric_ok = c("NA")) {
  long <- make_long_tokens(df, var)
  
  counts <- long %>% dplyr::count(.row, name = "n_tokens")
  empty  <- long %>% dplyr::filter(.data[[var]] == "")
  num_weird <- long %>%
    dplyr::filter(stringr::str_detect(.data[[var]], "^[0-9]+$")) %>%
    dplyr::filter(!(.data[[var]] %in% numeric_ok))
  
  df %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::left_join(counts, by = ".row") %>%
    dplyr::mutate(n_tokens = dplyr::coalesce(.data$n_tokens, 0L)) %>%
    dplyr::filter(.data$n_tokens > max_tokens) %>%
    dplyr::select(id_unique) %>%
    dplyr::bind_rows(empty %>% dplyr::select(id_unique)) %>%
    dplyr::bind_rows(num_weird %>% dplyr::select(id_unique)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(id_unique)
}

find_invalid_codes <- function(df, var, allowed, sep = ";\\s*") {
  df %>%
    dplyr::filter(!is.na(.data[[var]])) %>%
    tidyr::separate_rows(!!rlang::sym(var), sep = sep) %>%
    dplyr::mutate(!!rlang::sym(var) := stringr::str_trim(.data[[var]])) %>%
    dplyr::filter(!(.data[[var]] %in% allowed))
}

apply_manual_edits <- function(df, edits, log_df, step, action = "manual_edit") {
  
  for (i in seq_len(nrow(edits))) {
    id  <- edits$id_unique[i]
    var <- edits$var[i]
    val <- edits$new_value[i]
    
    df[[var]] <- dplyr::if_else(df$id_unique == id, val, as.character(df[[var]]))
    
    log_df <- log_event(
      log_df,
      step   = step,
      action = action,
      note   = paste0(id, ": ", var, " -> ", val, if (!is.na(edits$note[i])) paste0(" | ", edits$note[i]) else "")
    )
  }
  list(df = df, log_df = log_df)
}

# ------------------------------------------------------------------------------
# 3) Tables (frequency tables, APA export)
# ------------------------------------------------------------------------------

make_complete_table_figures <- function(df, var, levels_df, apa = FALSE, title = NULL, note = NULL) {
  var_sym <- rlang::sym(var)
  lab_col <- paste0(var, "_label")
  df <- df %>% mutate(!!var_sym := as.character(.data[[var]]))
  levels_df <- levels_df %>% mutate(!!var_sym := as.character(.data[[var]]))
  
  tab <- df %>%
    
    ##TO CHECK: this create several observations for each study with, e.g., several regions: Do we want that?
    tidyr::separate_rows(!!var_sym, sep = ";") %>%
    mutate(!!var_sym := dplyr::coalesce(trimws(!!var_sym), "Missing")) %>%
    count(method, !!var_sym, name = "n") %>%
    group_by(method) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  
  totals <- tab %>%
    group_by(method) %>%
    summarise(N = sum(n), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = method, values_from = N, names_prefix = "N_")
  
  tab_complete <- tidyr::expand_grid(!!var_sym := levels_df[[var]], method = c("0","1")) %>%
    left_join(tab, by = c(var, "method")) %>%
    mutate(across(c(n, pct), ~ tidyr::replace_na(.x, 0))) %>%
    left_join(levels_df, by = var) %>%
    tidyr::pivot_wider(names_from = method, values_from = c(n, pct), names_glue = "{.value}_{method}") %>%
    rename(
      Code = !!var_sym,
      Category = !!sym(lab_col),
      !!paste0("Non-CSS n (n=", totals$N_0, ")") := n_0,
      "Non-CSS %" = pct_0,
      !!paste0("CSS n (n=", totals$N_1, ")") := n_1,
      "CSS %" = pct_1
    ) %>%
    select(Code, Category, tidyselect::everything())
  
  if (!apa) return(tab_complete)
  
  ft <- tab_complete %>% flextable() %>% theme_vanilla() %>%
    align(align = "center", part = "all") %>% set_caption(title %||% paste("Table for", var)) %>% autofit()
  if (!is.null(note)) ft <- add_footer_lines(ft, values = note)
  ft
}

make_complete_table_table <- make_complete_table <- function(df, var, levels_df, 
                                                             apa = FALSE, title = NULL, note = NULL) {
  var_sym <- rlang::sym(var)
  lab_col <- paste0(var, "_label")
  
  df <- df %>% dplyr::mutate(!!var_sym := as.character(.data[[var]]))
  levels_df <- levels_df %>% dplyr::mutate(!!var_sym := as.character(.data[[var]]))
  
  tab <- df %>%
    tidyr::separate_rows(!!var_sym, sep = ";") %>%
    dplyr::mutate(!!var_sym := dplyr::coalesce(trimws(!!var_sym), "Missing")) %>%
    dplyr::count(method, !!var_sym, name = "n") %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()
  
  totals <- tab %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(N = sum(n), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = method, values_from = N, names_prefix = "N_")
  
  n0 <- totals$N_0 %||% 0
  n1 <- totals$N_1 %||% 0

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
  ft <- ft %>% flextable::autofit() %>% flextable::width(width = 1)
  
  doc %>%
    officer::body_add_par(paste0("Table ", number), style = "Normal") %>%
    officer::body_add_par(title, style = "Normal") %>%
    officer::body_add_flextable(ft) %>%
    officer::body_add_break()
}

build_crosstab_pct <- function(
    df_rows,
    row_var, levels_row_df,
    col_var, levels_col_df,
    selected_cols = NULL,
    selected_rows = NULL,
    split_by_method = TRUE,
    method_var = "method",
    method_levels = c("0", "1"),
    method_labels = c("Non-CSS %", "CSS %"),
    title = NULL,
    note = NULL,
    percent_digits = 1
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
    dplyr::filter(.data[[col_var]] %in% selected_cols, .data[[row_var]] %in% selected_rows)
  
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
        MethodLabel = dplyr::recode(
          as.character(.data[[method_var]]),
          !!!stats::setNames(method_labels, method_levels)
        )
      )
    
    wide <- out_long %>%
      dplyr::select(RowLabel = !!rlang::sym(row_lab), ColLabel = !!rlang::sym(col_lab), MethodLabel, pct) %>%
      tidyr::pivot_wider(id_cols = RowLabel, names_from = c(ColLabel, MethodLabel), values_from = pct)
    
    col_labels <- col_levels_df[[col_lab]]
    ordered <- c("RowLabel", as.vector(rbind(
      paste0(col_labels, "_", method_labels[1]),
      paste0(col_labels, "_", method_labels[2])
    )))
    
    existing <- intersect(ordered, names(wide))
    wide <- wide[, c("RowLabel", existing[existing != "RowLabel"]), drop = FALSE]
    
    value_cols <- setdiff(names(wide), "RowLabel")
    
    header_top <- c("", rep(col_labels, each = length(method_labels)))
    header_bottom <- c("", rep(method_labels, times = length(col_labels)))
    colkeys <- names(wide)
    
    header_df <- data.frame(
      col_keys = colkeys,
      Group = header_top,
      Method = header_bottom,
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
    
    full_grid <- tidyr::expand_grid(!!row_sym := selected_rows, !!col_sym := selected_cols)
    
    out_long <- full_grid %>%
      dplyr::left_join(tab, by = c(row_var, col_var)) %>%
      dplyr::mutate(pct = tidyr::replace_na(pct, 0)) %>%
      dplyr::left_join(levels_row_df, by = setNames(row_var, row_var)) %>%
      dplyr::left_join(col_levels_df, by = setNames(col_var, col_var))
    
    wide <- out_long %>%
      dplyr::select(RowLabel = !!rlang::sym(row_lab), ColLabel = !!rlang::sym(col_lab), pct) %>%
      tidyr::pivot_wider(id_cols = RowLabel, names_from = ColLabel, values_from = pct)
    
    col_labels <- col_levels_df[[col_lab]]
    keep <- intersect(col_labels, names(wide))
    wide <- wide[, c("RowLabel", keep), drop = FALSE]
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
  label_w <- 3.8
  other_w <- (total_width - label_w - 0.4) / max(1, length(value_cols))
  
  ft <- ft %>%
    flextable::width(j = "RowLabel", width = label_w) %>%
    flextable::width(j = value_cols, width = other_w)
  
  if (!is.null(note)) ft <- flextable::add_footer_lines(ft, values = note)
  ft
}

# ------------------------------------------------------------------------------
# 4) Statistics helpers
# ------------------------------------------------------------------------------

chi_method_table <- function(dataset, dep_var, table_caption, note_text, multi = FALSE) {
  dep_sym <- rlang::sym(dep_var)
  data_use <- dataset
  
  if (multi) {
    data_use <- data_use %>%
      tidyr::separate_rows(!!dep_sym, sep = ";") %>%
      dplyr::mutate(
        !!dep_sym := stringr::str_trim(as.character(!!dep_sym)),
        !!dep_sym := dplyr::case_when(is.na(!!dep_sym) ~ "NA", !!dep_sym == "" ~ "NA", TRUE ~ !!dep_sym),
        method = as.character(method)
      ) %>%
      dplyr::filter(.data[[dep_var]] != "NA")
  }
  
  chi_obj <- data_use %>%
    dplyr::mutate(
      method = as.character(method),
      !!dep_sym := as.character(.data[[dep_var]]),
      !!dep_sym := dplyr::case_when(is.na(!!dep_sym) ~ "NA", .data[[dep_var]] == "" ~ "NA", TRUE ~ !!dep_sym)
    ) %>%
    dplyr::filter(.data[[dep_var]] != "NA") %>%
    tidycomm::crosstab(method, !!dep_sym, chi_square = TRUE)
  
  mod <- attr(chi_obj, "model") %>% purrr::pluck(1)
  
  chi_sq <- as.numeric(mod$statistic)
  obs <- mod$observed
  n_total <- sum(obs)
  r <- nrow(obs)
  c <- ncol(obs)
  k <- max(min(r, c) - 1, 1)
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
  
  ft <- flextable::flextable(chi_tib) %>%
    flextable::theme_vanilla() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::set_caption(table_caption) %>%
    flextable::autofit() %>%
    flextable::add_footer_lines(values = note_text)
  
  list(ft = ft, chi_table = chi_tib, model = mod, chi_sq = chi_sq, cramers_v = cramers_v)
}

share_one <- function(.df, var) {
  .df %>%
    dplyr::mutate(val = as.character(.data[[var]]), method = as.character(method)) %>%
    dplyr::filter(method %in% c("0", "1")) %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_one   = sum(val == "1", na.rm = TRUE),
      pct_one = dplyr::if_else(n_total > 0, round(100 * n_one / n_total, 1), 0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(variable = var)
}

# ------------------------------------------------------------------------------
# 5) Figures helpers
# ------------------------------------------------------------------------------

apa_figure <- function(doc, number, title, df, category_var, levels_vec = NULL) {
  df_long <- df %>%
    dplyr::select(Category, dplyr::matches("%$")) %>%
    tidyr::pivot_longer(cols = dplyr::matches("%$"), names_to = "Method", values_to = "Percentage") %>%
    dplyr::mutate(Method = dplyr::recode(Method, "Non-CSS %" = "Non-CSS", "CSS %" = "CSS")) %>%
    exclude_other_not_mentioned()
  
  if (!is.null(levels_vec)) df_long$Category <- factor(df_long$Category, levels = levels_vec)
  
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Category, y = Percentage, fill = Method)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.7, color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(Percentage, 1), "%")),
      position = ggplot2::position_dodge(0.8),
      vjust = -0.8,
      size = 2
    ) +
    ggplot2::scale_fill_manual(values = c("grey70", "white")) +
    ggplot2::labs(x = category_var, y = "Percentage", fill = "Method Group") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 50, hjust = 1),
      plot.margin = ggplot2::margin(10, 10, 10, 100)
    )
  
  doc %>%
    officer::body_add_par(paste0("Figure ", number), style = "Normal") %>%
    officer::body_add_par(title, style = "Normal") %>%
    officer::body_add_gg(value = p, width = 9, height = 6) %>%
    officer::body_add_break()
}
