#
# Coding Masks for Final Coding 
# Date: 2026-03-30
#
# Setup ---------------------------------------------------------------------

library(readxl)
library(dplyr)
library(openxlsx)

if (!exists("PATHS")) source(here::here("R/paths.R"))
if (!exists("IN")) source(here::here("R/config.R"))

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
  rm(timestamps, dir_path, files, latest_file)
}

# Load data from 02, if necessary

objs <- c("intercoder_abstracts", "validation_abstracts", "coding_abstracts", "coding_abstracts_relevant")

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
  intercoder_abstracts <- data$intercoder_abstracts
  validation_abstracts  <- data$validation_abstracts
  coding_abstracts <- data$coding_abstracts
  coding_abstracts_relevant <- data$coding_abstracts_relevant
  
  #clean house
  rm(objs, timestamps, dir_path, files, latest_file)
}

# Load input data for full paper coding

input_file <- require_file(IN$full_paper_sample, "full paper sample (Excel)")
message("Reading full paper sample from: ", input_file)
coding_full_paper <- readxl::read_excel(input_file)

coded_reli_path <- require_file(IN$coded_reli, "coded reliability dataset (Excel)")
message("Reading coded reliability data from: ", coded_reli_path)
coded_reli <- readxl::read_excel(coded_reli_path)

# 4.1 Create Coding Masks  -------------------------------------------------
#
# Remove cases coded during pretests and reliability testing.
# Draw two stratified samples (by method) for two coders.
# Remaining cases coded by MM.

coding_full_paper_reduced <- coding_full_paper %>%
  dplyr::filter(!(id_unique %in% coded_reli$id_unique))

set.seed(SEED)

coding_full_paper_labeled <- coding_full_paper_reduced %>%
  dplyr::group_by(method) %>%
  dplyr::mutate(
    rand = sample.int(dplyr::n()),
    group = dplyr::case_when(
      rand <= 75 ~ "sample1",
      rand <= 150 ~ "sample2",
      TRUE ~ "rest"
    )
  ) %>%
  dplyr::ungroup()

coding_full_paper_sample1 <- coding_full_paper_labeled %>% dplyr::filter(group == "sample1")
coding_full_paper_sample2 <- coding_full_paper_labeled %>% dplyr::filter(group == "sample2")
coding_full_paper_rest    <- coding_full_paper_labeled %>% dplyr::filter(group == "rest")

# 4.2 Double Check Coding Masks --------------------------------------------------

# Extract IDs
ids_full <- coding_full_paper$id_unique
ids_1    <- coding_full_paper_sample1$id_unique
ids_2    <- coding_full_paper_sample2$id_unique
ids_rest <- coding_full_paper_rest$id_unique
ids_reli <- coded_reli$id_unique

ids_union <- union(union(union(ids_1, ids_2), ids_rest), ids_reli)

# Missing IDs
all_match <- setequal(ids_full, ids_union)
cat("Exakter Match zwischen Full Sample und Splits? ", all_match, "\n")

missing_in_union <- setdiff(ids_full, ids_union)   
extra_in_union   <- setdiff(ids_union, ids_full)

cat("IDs fehlen in den Sub-Samples (sind in Full Sample, aber nicht in 1/2/rest/reli):\n")
print(missing_in_union)

cat("\nIDs tauchen in Sub-Samples auf, sind aber NICHT im Full Sample:\n")
print(extra_in_union)

# Overlapping IDs
overlap_1_2   <- intersect(ids_1, ids_2)
overlap_1_rest   <- intersect(ids_1, ids_rest)
overlap_1_reli <- intersect(ids_1, ids_reli)
overlap_2_rest   <- intersect(ids_2, ids_rest)
overlap_2_reli <- intersect(ids_2, ids_reli)
overlap_rest_reli <- intersect(ids_rest, ids_reli)

cat("\nÜberschneidungen zwischen Sample 1 und Sample 2:\n");   print(overlap_1_2)
cat("\nÜberschneidungen zwischen Sample 1 und Sample Rest:\n");   print(overlap_1_rest)
cat("\nÜberschneidungen zwischen Sample 1 und Sample Reli:\n"); print(overlap_1_reli)
cat("\nÜberschneidungen zwischen Sample 2 und Sample Rest:\n");   print(overlap_2_rest)
cat("\nÜberschneidungen zwischen Sample 2 und Sample Reli:\n"); print(overlap_2_reli)
cat("\nÜberschneidungen zwischen Sample Rest und Sample Reli:\n"); print(overlap_rest_reli)

# Duplicates
duplicates_1 <- coding_full_paper_sample1 %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_2 <- coding_full_paper_sample2 %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_rest <- coding_full_paper_rest %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_reli <- coded_reli %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_full <- coding_full_paper %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)

cat("\nDoppelte IDs in Sample 1:\n");   print(duplicates_1)
cat("\nDoppelte IDs in Sample 2:\n");   print(duplicates_2)
cat("\nDoppelte IDs in Sample Rest:\n");   print(duplicates_rest)
cat("\nDoppelte IDs in Sample Reli:\n"); print(duplicates_reli)
cat("\nDoppelte IDs in Full Sample:\n"); print(duplicates_full)

# Output -----------------------------------------------------------------------

out_dir <- PATHS$int
stamp   <- format(Sys.time(), "%Y%m%d_%H%M")

out_files <- c(
  AZ = file.path(out_dir, paste0("04_final_coding_mask_AZ_", stamp, ".xlsx")),
  VK = file.path(out_dir, paste0("04_final_coding_mask_VK_", stamp, ".xlsx")),
  MM = file.path(out_dir, paste0("04_final_coding_mask_MM_", stamp, ".xlsx"))
)

#blocked for now, not necessary for replication
#openxlsx::write.xlsx(coding_full_paper_sample1, out_files["AZ"], overwrite = TRUE)
#openxlsx::write.xlsx(coding_full_paper_sample2, out_files["VK"], overwrite = TRUE)
#openxlsx::write.xlsx(coding_full_paper_rest,    out_files["MM"], overwrite = TRUE)

message("04 completed. Final coding masks written to:")
message("- ", paste(out_files, collapse = "\n- "))