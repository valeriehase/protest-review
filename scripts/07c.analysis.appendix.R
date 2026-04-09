#
# Final Analysis - Appendix
# Date: 2026-30-03
#
# Setup ------------------------------------------------------------------------

library(tidyverse)
library(tidycomm)

source(here::here("helper functions/paths.R"))
source(here::here("helper functions/helpers.R"))

# Load Input -------------------------------------------------------------------

# Load data from 01, if necessary

if (!exists("wos_abstracts", inherits = TRUE)) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^01_wos_abstracts_clean(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching WoS abstracts files found, please run script 01.load.wos.data first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading cleaned WoS abstracts from: ", timestamps[which.max(timestamps)])
  
  wos_abstracts <- readRDS(latest_file)
  
  #clean house
  rm(timestamps, dir_path, files, latest_file)
}

# Load data from 02, if necessary

objs <- c("icr_abstracts", "validation_abstracts", "coding_abstracts", "coding_abstracts_relevant", "css_sample", "non_css_sample")

# run if at least one of these files is missing
if (!all(sapply(objs, exists, inherits = TRUE))) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^02_abstract_screening_clean(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching cleaned abstract coding found, please run script 02.abstract.screening first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading abstract codings from: ", timestamps[which.max(timestamps)])
  
  data <- readRDS(latest_file)
  
  # separate objects
  icr_abstracts <- data$icr_abstracts
  validation_abstracts  <- data$validation_abstracts
  coding_abstracts <- data$coding_abstracts
  coding_abstracts_relevant <- data$coding_abstracts_relevant
  non_css_sample <- data$non_css_sample
  css_sample <- data$css_sample
  
  #clean house
  rm(objs, timestamps, dir_path, files, latest_file, data)
}

# Load data from 03b, if necessary

if (!exists("icr_paper", inherits = TRUE)) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^03b_reliability_values(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching reliability tests for full paper found, please run script 03a.reliability.testing first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading reliability scores for full paper from: ", timestamps[which.max(timestamps)])
  
  icr_paper <- readRDS(latest_file)
  
  #clean house
  rm(timestamps, dir_path, files, latest_file)
}

# Load data from 06c, if necessary

if (!exists("coding_paper_clean_6c", inherits = FALSE)) {
  
  # Directory
  dir_path <- PATHS$final
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^full_paper_sample_final(_\\d{8}_\\d{4})?\\.xlsx$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No final data set found, please run script 06c.cleaning.codes first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.xlsx$",
    "\\1",
    files
  )
  
  # Convert to POSIXct
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  # Select latest file
  latest_file <- files[which.max(timestamps)]
  
  message("Loading final sample from: ", timestamps[which.max(timestamps)])
  
  final_sample <- readxl::read_excel(latest_file)
  
  #clean house
  rm(timestamps, dir_path, files, latest_file)
}

# 7.1 Make Tables with necessary values -----------------------------------------------------------------------

# Appendix 2.1
appendix_2_1 <- tibble(
  
  # get units coded for intercoder values abstract inclusion
  intercoder_abstract_N = paste0("between N = ", min(icr_abstracts$n_Units), " and N = ", max(icr_abstracts$n_Units), " cases"),
  
  # get reli values for intercoder values abstract inclusion
  intercoder_abstract_reli = paste0("Holsti = ", round(icr_abstracts$Holstis_CR[icr_abstracts$Variable=="method"], 2), 
                                    " and Krippendorff = ", round(icr_abstracts$Krippendorffs_Alpha[icr_abstracts$Variable=="method"], 2)),
  
  # get validation for css detection via keywords
  precision = round(validation_abstracts$byClass["Precision"], 2),
  recall = round(validation_abstracts$byClass["Recall"], 2),
  f1 = round(validation_abstracts$byClass["F1"], 2),
  
  # get number of studies in CSS vs. non-CSS sample (before manual coding)
  css_sample = css_sample,
  non_css_sample = non_css_sample
)

# Appendix 2.2
appendix_2_2 <- tibble(
  
  # get reli values for intercoder values abstract inclusion
  intercoder_abstract_protest = paste0("Holsti = ", round(icr_abstracts$Holstis_CR[icr_abstracts$Variable=="protest"], 2), 
                                    " and Krippendorff = ", round(icr_abstracts$Krippendorffs_Alpha[icr_abstracts$Variable=="protest"], 2)),
  
  intercoder_abstract_method = paste0("Holsti = ", round(icr_abstracts$Holstis_CR[icr_abstracts$Variable=="method"], 2), 
                                       " and Krippendorff = ", round(icr_abstracts$Krippendorffs_Alpha[icr_abstracts$Variable=="method"], 2)),
  
  intercoder_abstract_type = paste0("Holsti = ", round(icr_abstracts$Holstis_CR[icr_abstracts$Variable=="type"], 2), 
                                       " and Krippendorff = ", round(icr_abstracts$Krippendorffs_Alpha[icr_abstracts$Variable=="type"], 2)),
  
  coded_studies = nrow(coding_abstracts),
  
  #N Studies final sample
  css_sample_paper = final_sample %>%
    count(method) %>%
    filter(method == 1) %>%
    pull(n),
  
  non_css_sample_paper = final_sample %>%
    count(method) %>%
    filter(method == 0) %>%
    pull(n)
)

# Appendix 3


# Appendix 3

appendix_3 <- icr_paper %>%
  dplyr::mutate(
    variable_label = dplyr::case_when(
      Variable == "V7"  ~ "Protest",
      Variable == "V8"  ~ "Type",
      Variable == "V10" ~ "Platform",
      Variable == "V11" ~ "Methods",
      Variable == "V12" ~ "Measurement",
      Variable == "V13" ~ "Cross-National",
      Variable == "V14" ~ "Longitudinal",
      Variable == "V15" ~ "Experiment",
      Variable == "V16" ~ "Level",
      TRUE ~ Variable
    ),
    Holstis_CR = round(Holstis_CR, 2),
    Krippendorffs_Alpha = round(Krippendorffs_Alpha, 2)
  ) %>%
  dplyr::select(
    variable_label,
    n_Units,
    Holstis_CR,
    Krippendorffs_Alpha
  )

# Export -----------------------------------------------------------------------

out_dir <- PATHS$final
stamp   <- format(Sys.time(), "%Y%m%d_%H%M")
out_file <- file.path(out_dir, paste0("07c.analysis.appendix.R", stamp, ".rds"))

#save all relevant output as list object
saveRDS(
  list(
    appendix_2_1 = appendix_2_1,
    appendix_2_2 = appendix_2_2,
    appendix_3 = appendix_3
  ),
  out_file
)

message("07c completed.")
message("- Results of appendix creation saved to: ", out_file)
