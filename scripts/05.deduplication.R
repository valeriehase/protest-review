#
# Deduplication 
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

source(here::here("R/paths.R"))
source(here::here("R/config.R"))

library(readxl)
library(dplyr)
library(openxlsx)

# Load Input -------------------------------------------------------------------

input_file <- IN$coded_full_sample

if (!file.exists(input_file)) {
  stop(
    "Missing required input file: ", input_file, "\n",
    "Expected: coded full paper sample.\n",
    "Check config.R -> IN$coded_full_sample",
    call. = FALSE)
}

message("Reading coded full sample from: ", input_file)
df <- readxl::read_excel(input_file)

# 5.1 Identify Remaining Duplicates --------------------------------------------

df <- df %>%
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


dupes <- df %>%
  dplyr::group_by(id_unique) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(id_unique, id_unique_dup)

n_dupe_ids <- dplyr::n_distinct(dupes$id_unique)

# 5.2 Export Duplicates --------------------------------------------------------

out_dir <- PATHS$out_dedup
out_dupes <- file.path(out_dir, "full_paper_sample_coded_dupes.xlsx")

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

df_deduplicated <- df %>%
  dplyr::filter(!id_unique_dup %in% to_remove)

removed_duplicates <- df %>%
  dplyr::filter(id_unique_dup %in% to_remove)

# Output -----------------------------------------------------------------------

out_dir <- PATHS$out_dedup

out_removed_dupes   <- file.path(out_dir, "removed_dupes.xlsx")
out_df_deduplicated <- file.path(out_dir, "full_paper_sample_deduplicated.xlsx")

openxlsx::write.xlsx(df_deduplicated, out_df_deduplicated, overwrite = TRUE)
openxlsx::write.xlsx(removed_duplicates, out_removed_dupes, overwrite = TRUE)

message("05 completed.")
message("- Duplicated IDs detected: ", n_dupe_ids)
message("- Deduplicated dataset overwritten at: ", out_df_deduplicated)
message("- Removed duplicates overwritten at: ", out_removed_dupes)




