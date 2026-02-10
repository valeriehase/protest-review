# Central project paths --------------------------------------------------------

library(here)

PATH <- list(
  "in"  = here("data", "in"),
  "out" = here("data", "out")
)

PATH$out_intermediate <- here(PATH$out, "intermediate")
PATH$out_final        <- here(PATH$out, "final")
PATH$out_logs         <- here(PATH$out, "logs")

dir.create(PATH$out_intermediate, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH$out_final, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH$out_logs, recursive = TRUE, showWarnings = FALSE)
