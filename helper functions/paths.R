# Central project paths --------------------------------------------------------

ROOT <- here::here()

PATHS <- list(
  IN   = file.path(ROOT, "data", "in"),
  OUT  = file.path(ROOT, "data", "out"),
  logs = file.path(ROOT, "data", "out", "logs"),
  int  = file.path(ROOT, "data", "out", "intermediate"),
  final= file.path(ROOT, "data", "out", "final")
)

purrr::walk(PATHS, dir.create, recursive = TRUE, showWarnings = FALSE)

message("Paths initialized.")
message("Project root: ", ROOT)
message("IN : ", PATHS$IN)
message("OUT: ", PATHS$OUT)


