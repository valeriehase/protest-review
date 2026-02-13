# Central project paths --------------------------------------------------------

ROOT <- here::here()

PATHS <- list(
  IN  = file.path(ROOT, "data", "in"),
  OUT = file.path(ROOT, "data", "out")
)

PATHS$out_logs         <- file.path(PATHS$OUT, "logs")
PATHS$out_intermediate <- file.path(PATHS$OUT, "intermediate")
PATHS$out_final        <- file.path(PATHS$OUT, "final")

PATHS$out_reliability       <- file.path(PATHS$OUT, "reliability")
PATHS$out_reliability_raw   <- file.path(PATHS$out_reliability, "raw")
PATHS$out_reliability_coded <- file.path(PATHS$out_reliability, "coded")

PATHS$out_masks        <- file.path(PATHS$out_intermediate, "final_coding_masks")
PATHS$out_masks_drawn  <- file.path(PATHS$out_masks, "drawn")
PATHS$out_masks_coded  <- file.path(PATHS$out_masks, "coded")

PATHS$out_dedup <- file.path(PATHS$out_intermediate, "deduplication")
PATHS$out_cleaning <- file.path(PATHS$out_intermediate, "cleaning")

PATHS$raw_wos <- file.path(ROOT, "raw_data_wos")

purrr::walk(PATHS, dir.create, recursive = TRUE, showWarnings = FALSE)

message("Paths initialized.")
message("Project root: ", ROOT)
message("IN : ", PATHS$IN)
message("OUT: ", PATHS$OUT)


