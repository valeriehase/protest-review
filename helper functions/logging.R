# Simple log system ------------------------------------------------------------
#
# Usage:
# source(here("R/logging.R"))
# log_df <- init_log()
# log_df <- log_event(log_df, "01_import", "read_excel", "loaded raw file")
# write_log(log_df, PATHS$log_cleaning)

init_log <- function() {
  tibble::tibble(
    timestamp = character(),
    step = character(),
    action = character(),
    note = character()
  )
}

log_event <- function(log_df, step, action, note = "") {
  dplyr::bind_rows(
    log_df,
    tibble::tibble(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      step = step,
      action = action,
      note = note
    )
  )
}

write_log <- function(log_df, path) {
  fs::dir_create(dirname(path))
  readr::write_tsv(log_df, path)
  message("Log saved: ", path)
}
