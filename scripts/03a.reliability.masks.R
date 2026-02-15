#
# Reliability test subsamples
# Author: Miriam Milzner
# Date: 2025-08-15
#
# Setup ------------------------------------------------------------------------

if (!exists("PATHS")) source(here::here("R/paths.R"))
if (!exists("IN")) source(here::here("R/config.R"))

library(readxl)
library(dplyr)
library(openxlsx)

# Load Input -------------------------------------------------------------------

input_file <- require_file(IN$full_paper_sample, "full-paper sample (Excel)")

message("Reading full-paper sample from: ", input_file)
df <- readxl::read_excel(input_file)

# 3.1 Draw Sample --------------------------------------------------------------

set.seed(SEED)

stopifnot(nrow(df) >= 92)
sample_total <- dplyr::slice_sample(df, n = 92)

sample1 <- sample_total[1:46, ]
sample2 <- sample_total[47:92, ]

message("Seed used for sampling: ", SEED)
message("N in input df: ", nrow(df))

# Output -----------------------------------------------------------------------

out_dir <- PATHS$int
stamp   <- format(Sys.time(), "%Y%m%d_%H%M")

output_file1 <- file.path(out_dir, paste0("03a_reliability_mask_R1_", stamp, ".xlsx"))
output_file2 <- file.path(out_dir, paste0("03a_reliability_mask_R2_", stamp, ".xlsx"))

openxlsx::write.xlsx(sample1, output_file1, overwrite = TRUE)
openxlsx::write.xlsx(sample2, output_file2, overwrite = TRUE)

message("03a completed. Reliability masks created:")
message("- ", output_file1)
message("- ", output_file2)
message("Next step: coders fill in the sheets and save coded versions in the same folder.")
