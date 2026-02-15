#
# Main Script Systematic Literature Review "Online Protest"
# Author: Miriam Milzner, Valerie Hase
# Date: 2026-01-10 (updated 2026-02-09)
#
# Setup ------------------------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"), stringsAsFactors = FALSE)

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install it with install.packages('here').")
}
library(here)

if (!file.exists(here::here("renv.lock"))) {
  stop("Missing 'renv.lock' in project root. Please run from the repository root directory.")
}
if (!requireNamespace("renv", quietly = TRUE)) {
  stop("Package 'renv' is required. Install it with install.packages('renv').")
}
renv::activate()

message("Project root: ", here::here())
message("Run started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# Source project files ---------------------------------------------------------

source(here("R/paths.R"))
source(here("R/config.R"))
source(here("R/codebook.R"))
source(here("R/logging.R"))
source(here("R/helpers.R"))

message("Seed: ", SEED)
set.seed(SEED)

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

# Samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
# Draws sample for intercoder tests & coding of abstracts
if (RUN$abstract_screening) source(here("scripts/02.abstract.screening.R"))

# 03 Reliability Tests ---------------------------------------------------------

# Generate sheets
if (RUN$reliability_masks) source(here("scripts/03a.reliability.masks.R"))

# Compute metrics
if (RUN$reliability_tests) source(here("scripts/03b.reliability.testing.R"))

# 04 Final Coding Masks --------------------------------------------------------

# Creates final coding masks for coders
if (RUN$final_masks) source(here("scripts/04.final.coding.masks.R"))

# 05 Deduplication -------------------------------------------------------------

# Exports dupes table + applies remove/keep-list
if (RUN$deduplication) source(here("scripts/05.deduplication.R"))

# 06 Data Cleaning ------------------------------------------------------------

# Uses deduplicated coded sample, applies general data cleaning + logs
if (RUN$final_clean) source(here("scripts/06a.data.cleaning.general.R"))

# Uses cleaned coded sample, checks all coders comments + logs
if (RUN$final_clean) source(here("scripts/06b.data.cleaning.comments.R"))

# Uses cleaned coded sample with checked comments, checks rare codes and code distributions + logs
if (RUN$final_clean) source(here("scripts/06c.data.cleaning.codes.R"))

# 07 Analysis Output -----------------------------------------------------------

# Generates Figures
if (RUN$figures) source(here("scripts/07a.analysis.figures.R"))
# Generates Tables
if (RUN$tables)  source(here("scripts/07b.analysis.tables.R"))

message("main completed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("---- SESSION INFO ----")
print(sessionInfo())
message("---- RENV STATUS ----")
renv::status()




