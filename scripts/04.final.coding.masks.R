#
# Coding Masks for Final Coding 
# Author: Miriam Milzner
# Date: 2025-09-11
#
# Setup ---------------------------------------------------------------------

source(here::here("R/paths.R"))
source(here::here("R/config.R"))

library(readxl)
library(dplyr)
library(openxlsx)

# Load Input -------------------------------------------------------------------

input_file <- IN$full_paper_sample
if (!file.exists(input_file)) {
  stop(
    "Missing required input file: ", input_file, "\n",
    "Expected: full paper sample.\n",
    "Check paths.R -> IN$full_paper_sample",
    call. = FALSE
  )
}
message("Reading full paper sample from: ", input_file)
df <- readxl::read_excel(input_file)


coded_reli_path <- IN$coded_reli
if (!file.exists(coded_reli_path)) {
  stop(
    "Missing required input file: ", coded_reli_path, "\n",
    "Expected: coded reliability dataset.\n",
    "Make sure they are placed in /data/in/.",
    call. = FALSE
  )
}
message("Reading coded reliability data from: ", coded_reli_path)
coded_reli <- readxl::read_excel(coded_reli_path)

# 4.1 Create Coding Masks  -------------------------------------------------
#
# Remove cases coded during pretests and reliability testing.
# Draw two stratified samples (by method) for two coders.
# Remaining cases coded by MM.

df_reduced <- df %>%
  dplyr::filter(!(id_unique %in% coded_reli$id_unique))

set.seed(SEED)

df_labeled <- df_reduced %>%
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

df_sample1 <- df_labeled %>% dplyr::filter(group == "sample1")
df_sample2 <- df_labeled %>% dplyr::filter(group == "sample2")
df_rest    <- df_labeled %>% dplyr::filter(group == "rest")

# 4.2 Double Check Coding Masks --------------------------------------------------

# Extract IDs
ids_full <- df$id_unique
ids_1    <- df_sample1$id_unique
ids_2    <- df_sample2$id_unique
ids_rest <- df_rest$id_unique
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
duplicates_1 <- df_sample1 %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_2 <- df_sample2 %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_rest <- df_rest %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_reli <- coded_reli %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_full <- df %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)

cat("\nDoppelte IDs in Sample 1:\n");   print(duplicates_1)
cat("\nDoppelte IDs in Sample 2:\n");   print(duplicates_2)
cat("\nDoppelte IDs in Sample Rest:\n");   print(duplicates_rest)
cat("\nDoppelte IDs in Sample Reli:\n"); print(duplicates_reli)
cat("\nDoppelte IDs in Full Sample:\n"); print(duplicates_full)

# Output -----------------------------------------------------------------------

out_dir <- file.path(PATHS$out_intermediate, "final_coding_masks")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M")
out_files <- c(
  AZ = file.path(out_dir, paste0("df_sample_clean_AZ_", stamp, ".xlsx")),
  VK = file.path(out_dir, paste0("df_sample_clean_VK_", stamp, ".xlsx")),
  MM = file.path(out_dir, paste0("df_sample_clean_MM_", stamp, ".xlsx"))
)
openxlsx::write.xlsx(df_sample1, out_files["AZ"])
openxlsx::write.xlsx(df_sample2, out_files["VK"])
openxlsx::write.xlsx(df_rest,    out_files["MM"])

message("Step 04 completed. Coding masks written to: ", out_dir)

