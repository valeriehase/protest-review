# Central project paths --------------------------------------------------------

library(here)

PATHS <- list(
  
  # Root folders --------------------------------------------------------------
  
  data_raw        = here("data", "raw"),
  data_processed  = here("data", "processed"),
  data_reliability= here("data", "reliability"),
  logs            = here("logs"),
  
  # Raw data ------------------------------------------------------------------
  
  raw_full_paper          = here("data", "raw", "full_paper"),
  raw_codings            = here("data", "raw", "codings"),
  raw_final_coding_masks = here("data", "raw", "full_paper", "final_coding_masks"),
  
  raw_coded_full_sample  = here("data", "raw", "full_paper", "full_paper_sample_coded.xlsx"),
  
  # Deduplication -------------------------------------------------------------
  
  dupes_export          = here("data", "processed", "duplicates.xlsx"),
  dupes_check           = here("data", "processed", "duplicates_check.xlsx"),
  dupes_removed_export  = here("data", "processed", "duplicates_removed.xlsx"),
  
  deduped_export        = here("data", "processed", "full_paper_sample_coded_deduped.xlsx"),
  deduped_sample12      = here("data", "processed", "full_paper_sample_coded_deduped_sample12.xlsx"),
  
  # Clean data ----------------------------------------------------------------
  
  cleaned_export        = here("data", "processed", "full_paper_sample_coded_clean.xlsx"),
  
  # Logs ----------------------------------------------------------------------
  
  log_cleaning          = here("logs", "data_cleaning_log.tsv")
)

# Auto-create directories only -------------------------------------------------

dirs <- PATHS[!grepl("\\.[a-zA-Z0-9]+$", PATHS)]

invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
