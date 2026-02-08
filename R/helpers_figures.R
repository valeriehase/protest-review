exclude <- function(df) df %>% filter(!Category %in% c("Other", "Not mentioned"))

make_complete_table <- function(df, var, levels_df, apa = FALSE, title = NULL, note = NULL) {
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

apa_figure <- function(doc, number, title, df, category_var, levels_vec = NULL) {
  df_long <- df %>%
    select(Category, matches("%$")) %>%
    tidyr::pivot_longer(cols = matches("%$"), names_to = "Method", values_to = "Percentage") %>%
    mutate(Method = recode(Method, "Non-CSS %" = "Non-CSS", "CSS %" = "CSS")) %>%
    exclude()
  
  if (!is.null(levels_vec)) df_long$Category <- factor(df_long$Category, levels = levels_vec)
  
  p <- ggplot(df_long, aes(x = Category, y = Percentage, fill = Method)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_dodge(0.8), vjust = -0.8, size = 2) +
    scale_fill_manual(values = c("grey70","white")) +
    labs(x = category_var, y = "Percentage", fill = "Method Group") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 50, hjust = 1),
      plot.margin = margin(10, 10, 10, 100)
    )
  
  doc %>%
    body_add_par(paste0("Figure ", number), style = "Normal") %>%
    body_add_par(title, style = "Normal") %>%
    body_add_gg(value = p, width = 9, height = 6) %>%
    body_add_break()
}

share_one <- function(.df, var) {
  .df %>%
    mutate(val = as.character(.data[[var]]), method = as.character(method)) %>%
    filter(method %in% c("0","1")) %>%
    group_by(method) %>%
    summarise(
      n_total = n(),
      n_one   = sum(val == "1", na.rm = TRUE),
      pct_one = if_else(n_total > 0, round(100 * n_one / n_total, 1), 0),
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}
