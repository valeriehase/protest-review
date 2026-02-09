# Central project packages -----------------------------------------------------

required_packages <- c(
  "tidyverse",
  "readxl",
  "openxlsx",
  "tidycomm",
  "janitor",
  "here"
)

invisible(lapply(required_packages, require, character.only = TRUE))
