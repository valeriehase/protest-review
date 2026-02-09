#
# Reliability Testing
# Author: Miriam Milzner
# Date: 2025-08-15
#
# Setup ------------------------------------------------------------------------

library(here)

source(here("R/packages.R"))
source(here("R/paths.R"))
source(here("R/config.R"))

# 1. Reliability Test Samples Masks --------------------------------------------

input_file <- file.path(PATHS$raw_full_paper, "full_paper_sample.xlsx")
df <- readxl::read_excel(input_file)

set.seed(SEED)

sample_total <- df %>% dplyr::sample_n(92)
sample1 <- sample_total[1:46, ]
sample2 <- sample_total[47:92, ]

if (!file.exists(output_file1)) openxlsx::write.xlsx(sample1, output_file1, overwrite = TRUE) 
if (!file.exists(output_file2)) openxlsx::write.xlsx(sample2, output_file2, overwrite = TRUE)

# 2. Read Coded Reliability Test Samples ---------------------------------------

files <- list.files(
  path = PATHS$reliability,
  pattern = "^relitest_full_paper_codebook_R\\d+_.+\\.xlsx$",
  full.names = TRUE
)
if (length(files) == 0) {
  stop("No coded reliability files found.")
}

df_all <- files %>%
  purrr::set_names() %>%  
  purrr::map_dfr(
    ~ readxl::read_excel(.x, .name_repair = "unique_quiet") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
    .id = "source") %>%
  dplyr::rename_with(~ stringt::str_extract(.x, "^V\\d+"),  
    dplyr::starts_with("V")) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  dplyr::select(-dplyr::any_off(c("source", "authors", "title", "abstract", "keywords", "link", "method", "Comments")))


# 3. Calculate Reliability Values  ---------------------------------------------

### V10 Platform

df_v10_long <- df_all %>%
  dplyr::select(id_unique, V5, V10) %>%
  tidyr::separate_rows(V10, sep = ";\\s*") %>%
  dplyr::mutate(V10 = as.numeric(V10))

df_v10_dummy <- df_v10_long %>%
  dplyr::mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = V10,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "cat_"
  )

for (val in c(100, 110:114, 120:122, 124, 130:134, 140:142, 150:153, 160:162,
              200:204, 300, 400, NA)) {
  col_name <- paste0("cat_", val)
  if (!col_name %in% names(df_v10_dummy)) df_v10_dummy[[col_name]] <- 0
  }

df_v10_expanded <- df_v10_dummy %>%
  tidyr::pivot_longer(starts_with("cat_"), names_to = "category", values_to = "dummy") %>%
  dplyr::mutate(unit_id = paste(id_unique, category, sep = "_"),
         V10 = dummy)

icr_v10 <- tidycomm::test_icr(
  data = df_v10_expanded,
  unit_var = unit_id,
  coder_var = V5,
  V10,
  kripp_alpha = TRUE,
  brennan_prediger = TRUE,
  holsti = TRUE
)

### V11 Methods

df_v11_long <- df_all %>%
  dplyr::select(id_unique, V5, V11) %>%
  tidyr::separate_rows(V11, sep = ";\\s*") %>%
  dplyr::mutate(V11 = as.numeric(V11))

df_v11_dummy <- df_v11_long %>%
  dplyr::mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = V11,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "cat_"
  )

for (val in c(10:18, 20:26, 99)) {
  col_name <- paste0("cat_", val)
  if (!col_name %in% names(df_v11_dummy)) df_v11_dummy[[col_name]] <- 0
  }

df_v11_expanded <- df_v11_dummy %>%
  tidyr::pivot_longer(starts_with("cat_"), names_to = "category", values_to = "dummy") %>%
  dplyr::mutate(unit_id = paste(id_unique, category, sep = "_"),
         V11 = dummy)

icr_v11 <- tidycomm::test_icr(
  data = df_v11_expanded,
  unit_var = unit_id,
  coder_var = V5,
  V11,
  kripp_alpha = TRUE,
  brennan_prediger = TRUE,
  holsti = TRUE
)

### V12-V16

icr_v7v16 <- tidycomm::test_icr(
  data = df_all,
  unit_var = id_unique,
  coder_var = V5,
  na.omit = TRUE,
  brennan_prediger = TRUE,
  V7, V8, V12, V13, V14, V15, V16
)

icr <- bind_rows(icr_v10, icr_v11, icr_v7v16)

# 4. Output --------------------------------------------------------------------

out_xlsx <- file.path(PATHS$out_reliability, paste0("reli_values_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
openxlsx::write.xlsx(icr, out_xlsx, overwrite = TRUE)

saveRDS(icr, file.path(PATHS$out_reliability, paste0("reli_value_", format(Sys.Date(), "%Y-%m-%d"), ".rds")))

message("Step 03 completed. Output: ", out_xlsx)

