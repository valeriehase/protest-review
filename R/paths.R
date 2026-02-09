# Central project paths --------------------------------------------------------

library(here)

PATHS <- list(
  raw_data        = here("data", "raw"),
  reliability     = here("data", "reliability"),
  processed       = here("data", "processed"),
  outputs         = here("outputs"),
  figures         = here("outputs", "figures"),
  tables          = here("outputs", "tables")
)

invisible(lapply(PATHS, dir.create, recursive = TRUE, showWarnings = FALSE))
