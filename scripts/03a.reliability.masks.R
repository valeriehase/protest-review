#
# Reliability test subsamples
# Author: Miriam Milzner
# Date: 2025-08-15
#
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/paths.R"))
source(here("R/config.R"))

library(readxl)
library(dplyr)
library(openxlsx)

# Load Input -------------------------------------------------------------------

input_file <- if (exists("IN") && !is.null(IN$full_paper_sample)) {
  IN$full_paper_sample
} else {
  here("data", "in", "full_paper_sample.xlsx")
}

if (!file.exists(input_file)) {
  stop(
    "Missing input file: ", input_file, "\n",
    "Provide the full-paper sample Excel file before running 03a."
  )
}

df <- readxl::read_excel(input_file)

# 3.1 Draw Sample ------------------------------------------------------------------

set.seed(SEED)

stopifnot(nrow(df) >= 92)
sample_total <- dplyr::slice_sample(df, n = 92)

sample1 <- sample_total[1:46, ]
sample2 <- sample_total[47:92, ]

message("Seed used for sampling: ", SEED)
message("N in input df: ", nrow(df))

# Outpu ------------------------------------------------------------------------

reli_dir <- if (exists("OUT") && !is.null(OUT$reliability)) {
  OUT$reliability
} else {
  here("data", "out", "reliability")
}
dir.create(reli_dir, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M")
output_file1 <- file.path(reli_dir, paste0("relitest_full_paper_codebook_R1_", stamp, ".xlsx"))
output_file2 <- file.path(reli_dir, paste0("relitest_full_paper_codebook_R2_", stamp, ".xlsx"))

openxlsx::write.xlsx(sample1, output_file1, overwrite = TRUE)
openxlsx::write.xlsx(sample2, output_file2, overwrite = TRUE)

message("03a completed. Reliability masks created:")
message("- ", output_file1)
message("- ", output_file2)
message("Next step: coders fill in the sheets and save coded versions in the same folder.")
