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

sample_total <- df %>% dplyr::sample_n(92)
sample1 <- sample_total[1:46, ]
sample2 <- sample_total[47:92, ]

# 3.2 Export masks -----------------------------------------------------------------

reli_dir <- if (exists("OUT") && !is.null(OUT$reliability)) {
  OUT$reliability
} else {
  here("data", "out", "reliability")
}
dir.create(reli_dir, recursive = TRUE, showWarnings = FALSE)

output_file1 <- file.path(reli_dir, "relitest_full_paper_codebook_R1.xlsx")
output_file2 <- file.path(reli_dir, "relitest_full_paper_codebook_R2.xlsx")

if (file.exists(output_file1) || file.exists(output_file2)) {
  stop(
    "Reliability mask file(s) already exist. I will not overwrite them.\n",
    "Existing:\n",
    "- ", if (file.exists(output_file1)) output_file1 else "(missing R1)", "\n",
    "- ", if (file.exists(output_file2)) output_file2 else "(missing R2)", "\n\n",
    "If you really need to regenerate them, delete the existing files first."
  )
}

openxlsx::write.xlsx(sample1, output_file1, overwrite = TRUE)
openxlsx::write.xlsx(sample2, output_file2, overwrite = TRUE)

message("03a completed. Reliability masks created:")
message("- ", output_file1)
message("- ", output_file2)
message("Next step: coders fill in the sheets and save coded versions in the same folder.")
