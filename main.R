#
# Main Script Systematic Literature Review "Online Protest"
# Author: Miriam Milzner, Valerie Hase
# Date: 2026-01-10 (updated 2026-02-09)
#
# Runs the full reproducible pipeline.
#
# Setup ------------------------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))
options(stringsAsFactors = FALSE)

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::activate()
renv::restore(prompt = FALSE)

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

stopifnot(file.exists(here::here("renv.lock")))

setwd(here::here())

source(here("R/paths.R"))
source(here("R/config.R"))
source(here("R/codebook.R"))
source(here("R/logging.R"))
source(here("R/helpers.R"))

message("Project root: ", here::here())
message("Seed: ", SEED)


# 01 WoS Import ----------------------------------------------------------------

# Loads Web of Science data (2019-2023, downloaded on 03.04.2024)
# Does some initial data cleaning (removing duplicates and other irrelevant abstracts)

source(here("scripts/01.load.wos.data.R"))

# 02 Abstract Screening --------------------------------------------------------

#samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
#draws sample for intercoder tests & coding of abstracts

source(here("scripts/02.abstract.screening.R"))

# 03a Reliability Test Masks (generate sheets) ---------------------------------

source(here("scripts/03a.reliability.masks.R"))

# 03b Reliability Tests (compute metrics) --------------------------------------

source(here("scripts/03b.reliability.tests.R"))

# 04 Final Coding Masks --------------------------------------------------------
# Creates final coding masks for coders (one-time script; still reproducible)

source(here("scripts/04.coding_masks_final.R"))

# 05 Deduplication + Sample out (from coded full sample) -----------------------
# Exports dupes table + apply keep-list

source(here("scripts/05.deduplication.resampling.R"))

# 06 Final Data Cleaning -------------------------------------------------------
# Uses deduplicated coded sample, applies cleaning + logs

source(here("scripts/06.final.data.cleaning.R"))

# 07 Analysis Output -----------------------------------------------------------

source(here("scripts/07.figures.R"))
source(here("scripts/08.tables.R"))

message("main completed.")




