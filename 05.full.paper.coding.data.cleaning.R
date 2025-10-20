# ---------------------------------------------------------------
# DATA CLEANING: Protest Review 
# ---------------------------------------------------------------
# Author: Miriam Milzner
# Date: 2025-10-20
#
# Notes:
# - Input file: data/raw/df_full_sample_coded.xlsx
# - The 'Comment' column was **manually standardized** to the format:
#       e.g. V10: no method section; V11: unclear coding
#   → This allows consistent parsing of multiple comments per variable
#   → DO NOT edit the 'Comment' column format manually anymore unless
#     the same pattern (Vxx: text; Vyy: text) is preserved.
#
# Packages -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(tidycomm)
  library(janitor)
  library(stringr)
  library(openxlsx)
  library(flextable)
  library(officer)
})


# Path -----------------------------------------------------------------

raw_data <- "data/raw/df_full_sample_coded.xlsx"
d_raw <- readxl::read_excel(raw_path)

