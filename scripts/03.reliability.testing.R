########################
#
# Reliability Testing
# Author: Miriam Milzner
# Date: 2025-08-15
#
########################

# Setup ------------------------------------------------------------------------

library(here)

source(here("R/packages.R"))
source(here("R/paths.R"))
source(here("R/config.R"))

# 1. Reliability Test Samples Masks --------------------------------------------

input_file <- file.path(PATH$raw_full_paper, "full_paper_sample.xlsx")
df <- readxl::read_excel(input_file)

set.seed(SEED)

sample_total <- df %>% sample_n(92)
sample1 <- sample_total[1:46, ]
sample2 <- sample_total[47:92, ]

output_file1 <- file.path(path, "reliability/relitest_full_paper_codebook_R1.xlsx")
output_file2 <- file.path(path, "reliability/relitest_full_paper_codebook_R2.xlsx")

write.xlsx(sample1, output_file1)
write.xlsx(sample2, output_file2)

# 2. Read Coded Reliability Test Samples ---------------------------------------

files <- list.files(
  path = "data/reliability",
  pattern = "^relitest_full_paper_codebook_R\\d+_.+\\.xlsx$",
  full.names = TRUE
)
if (length(files) == 0) {
  stop("No such files.")
}


df_all <- files %>%
  set_names() %>%  # damit "source" später dateipfad ist
  map_dfr(
    ~ read_excel(.x, .name_repair = "unique_quiet") %>%
      mutate(across(everything(), as.character)),
    .id = "source"
  )

df_all <- df_all %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),  # anpassung variablennamen
    starts_with("V")
  )

df_all <- df_all %>%
  mutate(across(everything(), as.character))

df_all <- df_all %>%
  select(-c(source, authors, title, abstract, keywords, link, method, Comments))


# 3. Calculate reliability values  ---------------------------------------------

# Reliability statistics (Krippendorff’s α) require the data to be in the format:
# coding unit × coder × code to allow direct comparison across coders.  
# For variables V10 and V11, a dummy (0/1) matrix is constructed before running
# the reliability tests.

### V10 Platform

df_v10_long <- df_all %>%
  select(id_unique, V5, V10) %>%
  separate_rows(V10, sep = ";\\s*") %>%
  mutate(V10 = as.numeric(V10))

df_v10_dummy <- df_v10_long %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = V10,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "cat_"
  )

for (val in c(100, 110:114, 120:122, 124, 130:134, 140:142, 150:153, 160:162,
              200:204, 300, 400, NA)) {
  col_name <- paste0("cat_", val)
  if (!col_name %in% names(df_v10_dummy)) {
    df_v10_dummy[[col_name]] <- 0
  }
}

df_v10_expanded <- df_v10_dummy %>%
  pivot_longer(
    starts_with("cat_"),
    names_to = "category",
    values_to = "dummy"
  ) %>%
  mutate(unit_id = paste(id_unique, category, sep = "_"),
         V10 = dummy)

icr_v10 <- test_icr(
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
  select(id_unique, V5, V11) %>%
  separate_rows(V11, sep = ";\\s*") %>%
  mutate(V11 = as.numeric(V11))

df_v11_dummy <- df_v11_long %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = V11,
    values_from = value,
    values_fill = list(value = 0),
    names_prefix = "cat_"
  )

for (val in c(10:18, 20:26, 99)) {
  col_name <- paste0("cat_", val)
  if (!col_name %in% names(df_v11_dummy)) {
    df_v11_dummy[[col_name]] <- 0
  }
}

df_v11_expanded <- df_v11_dummy %>%
  pivot_longer(
    starts_with("cat_"),
    names_to = "category",
    values_to = "dummy"
  ) %>%
  mutate(unit_id = paste(id_unique, category, sep = "_"),
         V11 = dummy)

icr_v11 <- test_icr(
  data = df_v11_expanded,
  unit_var = unit_id,
  coder_var = V5,
  V11,
  kripp_alpha = TRUE,
  brennan_prediger = TRUE,
  holsti = TRUE
)

### V12-V16

icr_v7v16 <- test_icr(
  data = df_all,
  unit_var = id_unique,
  coder_var = V5,
  na.omit = TRUE,
  brennan_prediger = TRUE,
  V7, V8, V12, V13, V14, V15, V16
)

####

icr <- bind_rows(icr_v10, icr_v11, icr_v7v16)

file_name <- paste0("output/reliability/reliwerte_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
write.xlsx(path = "output/reliability", icr, file_name)


