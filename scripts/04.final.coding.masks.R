#
# Coding Masks for Final Coding 
# Author: Miriam Milzner
# Date: 2025-09-11
#
# Setup ---------------------------------------------------------------------

library(here)

source(here("R/packages.R"))
source(here("R/paths.R"))
source(here("R/config.R"))

# 1. Create Coding Masks  -------------------------------------------------
#
# Remove cases used for pretests and reliability testing.
# Draw two stratified samples (by method) for two coders.
# Remaining cases coded by MM.

input_file <- file.path(PATHS$raw_full_paper, "full_paper_sample.xlsx")
df <- readxl::read_excel(input_file)

coded_reli <- readxl::read_excel(file.path(PATHS$raw_full_paper, "df_sample_coded_PRETEST_RELI.xlsx"))

df_reduced <- df %>%
  dplyr::filter(!(id_unique %in% coded_reli$id_unique))

set.seed(SEED)

df_labeled <- df_reduced %>%
  dplyr::group_by(method) %>%
  dplyr::mutate(rand = sample(row_number()),
         group = case_when(
           rand <= 75 ~ "sample1",
           rand <= 150 ~ "sample2",
           TRUE ~ "rest"
         )) %>%
  dplyr::ungroup()

df_sample1 <- df_labeled %>% dplyr::filter(group == "sample1")
df_sample2 <- df_labeled %>% dplyr::filter(group == "sample2")
df_rest    <- df_labeled %>% dplyr::filter(group == "rest")

# 2. Write Coding Masks --------------------------------------------------------

out_AZ <- file.path(PATHS$raw_final_coding_masks, "df_sample_clean_AZ_.xlsx")
out_VK <- file.path(PATHS$raw_final_coding_masks, "df_sample_clean_VK.xlsx")
out_MM <- file.path(PATHS$raw_final_coding_masks, "df_sample_clean_MM.xlsx")

if (!file.exists(out_AZ)) openxlsx::write.xlsx(df_sample1, out_AZ, overwrite = TRUE)
if (!file.exists(out_VK)) openxlsx::write.xlsx(df_sample2, out_VK, overwrite = TRUE)
if (!file.exists(out_MM)) openxlsx::write.xlsx(df_rest, out_MM, overwrite = TRUE)

saveRDS(list(df_sample1 = df_sample1, df_sample2 = df_sample2, df_rest = df_rest),
        file.path(PATHS$processed, "04_coding_masks.rds")
        )

# 3. Check Final Coding Masks --------------------------------------------------

df_sample_clean_AZ   <- readxl::read_excel(out_AZ)
df_sample_clean_VK   <- readxl::read_excel(out_VK)
df_sample_clean_MM   <- readxl::read_excel(out_MM)
df_sample_coded_RELI <- coded_reli
df_full_paper_sample <- df

# Extract IDs
ids_full <- df_full_paper_sample$id_unique
ids_AZ   <- df_sample_clean_AZ$id_unique
ids_VK   <- df_sample_clean_VK$id_unique
ids_MM   <- df_sample_clean_MM$id_unique
ids_RELI <- df_sample_coded_RELI$id_unique

ids_union <- union(union(union(ids_AZ, ids_VK), ids_MM), ids_RELI)

# Missing IDs
all_match <- setequal(ids_full, ids_union)
cat("Exakter Match zwischen Full Sample und Splits? ", all_match, "\n")

missing_in_union <- setdiff(ids_full, ids_union)   
extra_in_union   <- setdiff(ids_union, ids_full)

cat("IDs fehlen in den Sub-Samples (sind in Full Sample, aber nicht in AZ/VK/MM/RELI):\n")
print(missing_in_union)

cat("\nIDs tauchen in Sub-Samples auf, sind aber NICHT im Full Sample:\n")
print(extra_in_union)

# Overlapping IDs
overlap_AZ_VK   <- intersect(ids_AZ, ids_VK)
overlap_AZ_MM   <- intersect(ids_AZ, ids_MM)
overlap_AZ_RELI <- intersect(ids_AZ, ids_RELI)
overlap_VK_MM   <- intersect(ids_VK, ids_MM)
overlap_VK_RELI <- intersect(ids_VK, ids_RELI)
overlap_MM_RELI <- intersect(ids_MM, ids_RELI)

cat("\nÜberschneidungen zwischen AZ und VK:\n");   print(overlap_AZ_VK)
cat("\nÜberschneidungen zwischen AZ und MM:\n");   print(overlap_AZ_MM)
cat("\nÜberschneidungen zwischen AZ und RELI:\n"); print(overlap_AZ_RELI)
cat("\nÜberschneidungen zwischen VK und MM:\n");   print(overlap_VK_MM)
cat("\nÜberschneidungen zwischen VK und RELI:\n"); print(overlap_VK_RELI)
cat("\nÜberschneidungen zwischen MM und RELI:\n"); print(overlap_MM_RELI)

# Duplicates
duplicates_AZ <- df_sample_clean_AZ %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_VK <- df_sample_clean_VK %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_MM <- df_sample_clean_MM %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_RELI <- df_sample_coded_RELI %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)
duplicates_FULL <- df_full_paper_sample %>% dplyr::count(id_unique) %>% dplyr::filter(n > 1)

cat("\nDoppelte IDs in AZ:\n");   print(duplicates_AZ)
cat("\nDoppelte IDs in VK:\n");   print(duplicates_VK)
cat("\nDoppelte IDs in MM:\n");   print(duplicates_MM)
cat("\nDoppelte IDs in RELI:\n"); print(duplicates_RELI)
cat("\nDoppelte IDs in FULL:\n"); print(duplicates_FULL)

message("Step 04 completed. Coding masks written to: ", PATHS$raw_final_coding_masks)
