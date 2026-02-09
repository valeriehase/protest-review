#
# Main Script Systematic Literature Review "Online Protest"
# Author: Miriam Milzner, Valerie Hase
# Date: 2026-01-10 (updated 2026-02-09)
#
# Runs the full reproducible pipeline (no manual coding steps).
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/packages.R"))
source(here("R/paths.R"))
source(here("R/config.R"))
source(here("R/codebook.R"))

message("Project root: ", here::here())
message("Seed:, ", SEED)

# 01 WoS Import ----------------------------------------------------------------

# Loads data by Annett (Web of Science, 2019-2023, downloaded on 03.04.2024)
# Does some initial data cleaning (removing duplicates and other irrelevant abstracts)

#source("01.load.wos.data.R")
#save.image("working_spaces/01.load.data.RDATA")

# 02 Abstract Screening --------------------------------------------------------

#samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
#draws sample for intercoder tests & coding of abstracts

#load("working_spaces/01.load.data.RDATA")
#source("02.abstract.screening.R")
#save.image("working_spaces/02.abstract.screening.data.RDATA")

##### 2.1 Details Method Section / Appendix A2.1

#initial amount of identified studies
n_deduplicated

#reliability for inclusion/exclusion
intercoder

#validation of search string
validation

#initial sample of CSS vs. non-CSS studies
nrow(css.sample)
nrow(non.css.sample)

#coded articles until we had identified equally large CSS and non-CSS samples
rbind(coding_abstracts, coding_abstracts_2, coding_abstracts_3) %>%
  filter(!is.na(protest) & !is.na(method) & !is.na(type)) %>%
  nrow()

#CSS vs. non-CSS sample - relevant studies
sample_relevant %>%
  count(method)

# 03 Reliability Test Full-Paper Coding ----------------------------------------
# Generates reliability test sheets + reads coded reliability sheets (must exist)

source(here("scripts/03.reliability.testing.R"))

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




