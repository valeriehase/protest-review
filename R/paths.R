# Central project paths --------------------------------------------------------

library(here)

IN <- list(
  root = here("data", "in"),
  
  wos_abstracts      = here("data", "in", "wos_abstracts_2009_2023.xlsx"),
  full_paper_sample = here("data", "in", "full_paper_sample.xlsx"),
  coded_full_sample  = here("data", "in", "full_paper_sample_coded.xlsx"),
  dedup_keep_ids     = here("data", "in", "dedup_keep_ids.xlsx")
)

OUT <- list(
  root         = here("data", "out"),
  intermediate = here("data", "out", "intermediate"),
  final        = here("data", "out", "final"),
  logs         = here("logs"),
  
  dupes_export = here("data", "out", "intermediate", "duplicates.xlsx"),
  cleaned_export = here("data", "out", "final", "full_paper_sample_coded_clean.xlsx")
)

RELIABILITY <- here("data", "reliability")

dir.create(OUT$intermediate, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT$final, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT$logs, recursive = TRUE, showWarnings = FALSE)
