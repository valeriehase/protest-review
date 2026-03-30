#
# Deduplication 
# Date: 2026-03-30
#
# Setup ------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(openxlsx)

source(here::here("helper functions/paths.R"))
source(here::here("helper functions/config.R"))
source(here::here("helper functions/helpers.R"))

# Input data -------------------------------------------------------------------

# Load data from 01, if necessary

if (!exists("wos_abstracts", inherits = TRUE)) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^01_wos_abstracts_clean(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching WoS abstracts files found, please run script 01.load.wos.data first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading cleaned WoS abstracts from: ", timestamps[which.max(timestamps)])
  
  wos_abstracts <- readRDS(latest_file)
  
  #clean house
  rm(timestamps, dir_path, files, latest_file, data)
}

# Load data from 02, if necessary

objs <- c("icr_abstracts", "validation_abstracts", "coding_abstracts", "coding_abstracts_relevant")

# run if at least one of these files is missing
if (!all(sapply(objs, exists, inherits = TRUE))) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^02_abstract_screening_clean(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching cleaned abstract coding found, please run script 02.abstract.screening first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading abstract codings from: ", timestamps[which.max(timestamps)])
  
  data <- readRDS(latest_file)
  
  # separate objects
  icr_abstracts <- data$intercoder_abstracts
  validation_abstracts  <- data$validation_abstracts
  coding_abstracts <- data$coding_abstracts
  coding_abstracts_relevant <- data$coding_abstracts_relevant
  
  #clean house
  rm(objs, timestamps, dir_path, files, latest_file, data)
}

# Load input data from full paper coding

input_file <- require_file(IN$coded_full_sample, "coded full-paper sample")

message("Reading coded full sample from: ", input_file)
coding_full_paper <- readxl::read_excel(input_file)

# 5.1 Identify Remaining Duplicates --------------------------------------------

coding_full_paper <- coding_full_paper %>%
  dplyr::add_count(id_unique, name = "n") %>%
  dplyr::group_by(id_unique) %>%
  dplyr::mutate(
    id_unique_dup = dplyr::if_else(
      n > 1,
      paste0(id_unique, "__", dplyr::row_number()),
      id_unique
    )
  ) %>%
  dplyr::select(-n) %>%
  dplyr::ungroup()

dupes <- coding_full_paper %>%
  dplyr::group_by(id_unique) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(id_unique, id_unique_dup)

n_dupe_ids <- dplyr::n_distinct(dupes$id_unique)

# 5.2 Export Duplicates --------------------------------------------------------

out_dir <- PATHS$int
stamp <- format(Sys.time(), "%Y%m%d_%H%M")
out_dupes <- file.path(out_dir, paste0("05_full_paper_sample_coded_dupes_", stamp, ".xlsx"))

openxlsx::write.xlsx(dupes, out_dupes, overwrite = TRUE)

message("05: ", n_dupe_ids, " duplicated IDs detected.")
message("Duplicates exported to: ", out_dupes)
message("Next step: decide which duplicates to keep and save as duplicates_check.xlsx in data/in/")

# 5.3 Apply manual decision about duplicates -----------------------------------

input_dupes_checked <- IN$dupes_checked

if (!file.exists(input_dupes_checked)) {
  stop(
    "Missing duplicate decision file: ", input_dupes_checked, "\n",
    "Expected: manual keep/remove decisions for duplicate IDs.\n",
    "Save the file in data/in/ or update config.R.",
    call. = FALSE)
}

message("Reading duplicate decisions from: ", input_dupes_checked)
dupes_checked <- readxl::read_excel(input_dupes_checked)

required_cols <- c("id_unique_dup", "decision")
if (!all(required_cols %in% names(dupes_checked))) {
  stop(
    "Duplicate decision file must contain columns: ",
    paste(required_cols, collapse = ", "),
    call. = FALSE)
}

to_remove <- dupes_checked %>%
  dplyr::filter(decision == 0) %>%
  dplyr::pull(id_unique_dup)

coding_full_paper_deduplicated <- coding_full_paper %>%
  dplyr::filter(!id_unique_dup %in% to_remove)

removed_duplicates <- coding_full_paper %>%
  dplyr::filter(id_unique_dup %in% to_remove)

# 5.4 Export -----------------------------------------------------------------------

out_dir <- PATHS$int
stamp <- format(Sys.time(), "%Y%m%d_%H%M")

out_removed_dupes <- file.path(out_dir, paste0("05_deduplication_removed_dupes_", stamp, ".xlsx"))
out_coding_full_paper_deduplicated <- file.path(out_dir, paste0("05_deduplication_full_paper_sample_deduplicated_", stamp, ".xlsx"))

openxlsx::write.xlsx(coding_full_paper_deduplicated, out_coding_full_paper_deduplicated, overwrite = T)
openxlsx::write.xlsx(removed_duplicates, out_removed_dupes, overwrite = T)

message("05 completed.")
message("- Duplicated IDs detected: ", n_dupe_ids)
message("- Deduplicated dataset at: ", out_coding_full_paper_deduplicated)
message("- Removed duplicates at: ", out_removed_dupes)