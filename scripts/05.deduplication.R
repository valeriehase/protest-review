#
# Deduplication 
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/paths.R"))
source(here("R/config.R"))

library(readxl)
library(dplyr)
library(openxlsx)

# Load Input -------------------------------------------------------------------

input_file <- if (exists("IN") && !is.null(IN$coded_full_sample)) {
  IN$coded_full_sample
} else {
  here("data", "in", "full_paper_sample_coded.xlsx")
}
stopifnot(file.exists(input_file))

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

out_dir <- if (exists("OUT") && !is.null(OUT$intermediate)) {
  file.path(OUT$intermediate, "deduplication")
} else {
  here("data", "out", "intermediate", "deduplication")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_dupes <- file.path(out_dir, "full_paper_sample_coded_dupes.xlsx")
openxlsx::write.xlsx(dupes, out_dupes, overwrite = TRUE)

message("05: ", n_dupe_ids, " duplicated IDs detected.")
message("Duplicates exported to: ", out_dupes)
message("Made decision which duplicate to keep and saved as duplicates_check.xlsx in data/in/")

# 5.3 Apply manual decision about duplicates -----------------------------------

dupes_checked <- if (exists("IN") && !is.null(IN$dupes_checked)) {
  IN$dupes_checked
} else {
  here("data", "in", "dupes_checked.xlsx")
}
stopifnot(file.exists(dupes_checked))
dupes_checked <- readxl::read_excel(dupes_checked) 

stopifnot(all(c("id_unique_dup", "decision") %in% names(dupes_checked)))

to_remove <- dupes_checked %>%
  dplyr::filter(decision == 0) %>%
  dplyr::pull(id_unique_dup)

df_deduplicated <- df %>%
  dplyr::filter(!id_unique_dup %in% to_remove)

removed_duplicates <- df %>%
  dplyr::filter(id_unique_dup %in% to_remove)

# Outputs -------------------------------------------------------------

out_dir <- if (exists("OUT") && !is.null(OUT$intermediate)) {
  file.path(OUT$intermediate, "deduplication")
} else {
  here("data", "out", "intermediate", "deduplication")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_removed_dupes <- file.path(out_dir, "removed_dupes.xlsx")
out_df_deduplicated <- file.path(out_dir, "full_paper_sample_deduplicated.xlsx")

if (file.exists(out_removed_dupes) || file.exists(out_df_deduplicated)) {
  stop(
    "Duplicate check file(s) already exist. I will not overwrite them.\n",
    "Existing:\n",
    "- ", if (file.exists(out_removed_dupes)) out_removed_dupes else "(removed dupes)", "\n",
    "- ", if (file.exists(out_df_deduplicated)) out_df_deduplicated else "(deduplicated df)", "\n\n",
    "If you really need to regenerate them, delete the existing files first."
  )
}

openxlsx::write.xlsx(df_deduplicated, out_df_deduplicated, overwrite = TRUE)
openxlsx::write.xlsx(removed_duplicates, out_removed_dupes, overwrite = TRUE)
saveRDS(df_deduplicated, out_df_deduplicated)

message("05 completed.")
message("- Duplicated IDs detected: ", n_dupe_ids)
message("- Deduplicated dataset written to: ", out_df_deduplicated)



