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

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install it with install.packages('here').")
}
library(here)

stopifnot(file.exists(here::here("renv.lock")))

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("Package 'renv' is required. Install it with install.packages('renv').")
}

renv::restore(prompt = FALSE)

message("Project root: ", here::here())
message("Run started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# Source project files ---------------------------------------------------------

source(here("R/paths.R"))
source(here("R/config.R"))
source(here("R/codebook.R"))
source(here("R/logging.R"))
source(here("R/helpers.R"))

message("Seed: ", SEED)

# Pipeline switches ------------------------------------------------------------

RUN <- list(
  wos_import = FALSE,
  abstract_screening = FALSE,
  reliability_masks = TRUE,
  reliability_tests = TRUE,
  final_masks = TRUE,
  deduplication = TRUE,
  final_clean = TRUE,
  figures = TRUE,
  tables = TRUE
)

# 01 WoS Import ----------------------------------------------------------------
# Loads Web of Science data (2019-2023, downloaded on 03.04.2024)
# Does some initial data cleaning (removing duplicates and other irrelevant abstracts)

if (RUN$wos_import) source(here("scripts/01.load.wos.data.R"))

# 02 Abstract Screening --------------------------------------------------------

#samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
#draws sample for intercoder tests & coding of abstracts

if (RUN$abstract_screening) source(here("scripts/02.abstract.screening.R"))

# 03a Reliability Test Masks (generate sheets) ---------------------------------

if (RUN$reliability_masks) source(here("scripts/03a.reliability.masks.R"))

# 03b Reliability Tests (compute metrics) --------------------------------------

if (RUN$reliability_tests) source(here("scripts/03b.reliability.tests.R"))

# 04 Final Coding Masks --------------------------------------------------------
# Creates final coding masks for coders (one-time script; still reproducible)

if (RUN$final_masks) source(here("scripts/04.coding_masks_final.R"))

# 05 Deduplication + Sample out (from coded full sample) -----------------------
# Exports dupes table + apply keep-list

if (RUN$deduplication) source(here("scripts/05.deduplication.R"))

# 06 Final Data Cleaning -------------------------------------------------------
# Uses deduplicated coded sample, applies cleaning + logs

if (RUN$final_clean) source(here("scripts/06.final.data.cleaning.R"))

# 07 Analysis Output -----------------------------------------------------------

if (RUN$figures) source(here("scripts/07.analysis.figures.R"))
if (RUN$tables)  source(here("scripts/08.analysis.tables.R"))

message("main completed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))




