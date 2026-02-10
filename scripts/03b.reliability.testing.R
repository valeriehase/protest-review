#
# Reliability Tests
# Author: Miriam Milzner
# Date: 2025-08-15
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/paths.R"))
source(here("R/config.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(openxlsx)
library(tidycomm)

# 1. Read Coded Reliability Test Samples ---------------------------------------

reli_dir <- if (exists("OUT") && !is.null(OUT$reliability)) {
  OUT$reliability
} else {
  here("data", "out", "reliability")
}

if (!dir.exists(reli_dir)) {
  message("03b skipped: reliability folder does not exist: ", reli_dir)
  return(invisible(NULL))
}

files <- list.files(
  path = reli_dir,
  pattern = "^relitest_full_paper_codebook_R\\d+(?:_.+)?\\.xlsx$",
  full.names = TRUE
)

if (length(files) == 0) {
  message("03b skipped: no reliability files found in: ", reli_dir)
  return(invisible(NULL))
}

df_all <- files %>%
  purrr::set_names() %>%
  purrr::map_dfr(
    ~ readxl::read_excel(.x, .name_repair = "unique_quiet") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
    .id = "source"
  ) %>%
  dplyr::rename_with(
    ~ stringr::str_extract(.x, "^V\\d+"),
    dplyr::starts_with("V")
  ) %>%
  dplyr::select(
    -dplyr::any_of(c("source", "authors", "title", "abstract", "keywords", "link", "method", "Comments"))
  )

needed_cols <- c("id_unique", "V5", "V7", "V8", "V10", "V11", "V12", "V13", "V14", "V15", "V16")
missing_cols <- setdiff(needed_cols, names(df_all))
if (length(missing_cols) > 0) {
  stop(
    "03b: missing required column(s) in reliability sheets: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

# 2. Calculate Reliability Values  ---------------------------------------------

# V10 Platform (multi-label)

df_v10_long <- df_all %>%
  dplyr::select(id_unique, V5, V10) %>%
  tidyr::separate_rows(V10, sep = ";\\s*") %>%
  dplyr::mutate(V10 = as.numeric(V10))

df_v10_dummy <- df_v10_long %>%
  dplyr::mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = V10,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "cat_"
  )

for (val in c(100, 110:114, 120:122, 124, 130:134, 140:142, 150:153, 160:162,
              200:204, 300, 400, NA)) {
  col_name <- paste0("cat_", val)
  if (!col_name %in% names(df_v10_dummy)) df_v10_dummy[[col_name]] <- 0
}

df_v10_expanded <- df_v10_dummy %>%
  tidyr::pivot_longer(starts_with("cat_"), names_to = "category", values_to = "dummy") %>%
  dplyr::mutate(unit_id = paste(id_unique, category, sep = "_"),
                V10 = dummy)

icr_v10 <- tidycomm::test_icr(
  data = df_v10_expanded,
  unit_var = unit_id,
  coder_var = V5,
  V10,
  kripp_alpha = TRUE,
  brennan_prediger = TRUE,
  holsti = TRUE
)

# V11 Methods (multi-label)

df_v11_long <- df_all %>%
  dplyr::select(id_unique, V5, V11) %>%
  tidyr::separate_rows(V11, sep = ";\\s*") %>%
  dplyr::mutate(V11 = as.numeric(V11))

df_v11_dummy <- df_v11_long %>%
  dplyr::mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = V11,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "cat_"
  )

for (val in c(10:18, 20:26, 99)) {
  col_name <- paste0("cat_", val)
  if (!col_name %in% names(df_v11_dummy)) df_v11_dummy[[col_name]] <- 0
}

df_v11_expanded <- df_v11_dummy %>%
  tidyr::pivot_longer(starts_with("cat_"), names_to = "category", values_to = "dummy") %>%
  dplyr::mutate(unit_id = paste(id_unique, category, sep = "_"),
                V11 = dummy)

icr_v11 <- tidycomm::test_icr(
  data = df_v11_expanded,
  unit_var = unit_id,
  coder_var = V5,
  V11,
  kripp_alpha = TRUE,
  brennan_prediger = TRUE,
  holsti = TRUE
)

### V7, V8, V12-V16 (single-label)

icr_v7v16 <- tidycomm::test_icr(
  data = df_all,
  unit_var = id_unique,
  coder_var = V5,
  na.omit = TRUE,
  brennan_prediger = TRUE,
  V7, V8, V12, V13, V14, V15, V16
)

icr <- bind_rows(icr_v10, icr_v11, icr_v7v16)

# 4. Output --------------------------------------------------------------------

out_dir <- reli_dir
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_date <- format(Sys.Date(), "%Y-%m-%d")
out_xlsx <- file.path(PATHS$out_reliability, paste0("reli_values_", out_date, ".xlsx"))
out_rds  <- file.path(PATHS$out_reliability, paste0("reli_values_", out_date, ".rds"))

openxlsx::write.xlsx(icr, out_xlsx, overwrite = TRUE)
saveRDS(icr, out_rds)

message("Step 03b completed. Output: ", out_xlsx)

