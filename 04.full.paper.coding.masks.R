# ---------------------------------------------------------------
# CODING MASKS: Protest Review 
# ---------------------------------------------------------------
# Author: Miriam Milzner
# Date: 2025-09-11
#
# === Packages =================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

# === Path =====================================================================

path <- "C:/Users/miriam57/Documents/WIP_Protest_Review"

# === Create Coding Masks =================================================================
# Remove the cases used for reliability testing, 
# then draw two random samples of 150 cases each for the two coders.  
# The remaining cases will be coded by MM.

df <- read_excel("full_paper_sample.xlsx")
coded_reli <- read_excel("df_sample_coded_RELI.xlsx")

df_reduced <- df %>%
  filter(!(id_unique %in% coded_reli$id_unique))

set.seed(123)
df_labeled <- df_reduced %>%
  group_by(method) %>%
  mutate(rand = sample(row_number()),
         group = case_when(
           rand <= 75 ~ "sample1",
           rand <= 150 ~ "sample2",
           TRUE ~ "rest"
         )) %>%
  ungroup()

df_sample1 <- df_labeled %>% filter(group == "sample1")
df_sample2 <- df_labeled %>% filter(group == "sample2")
df_rest    <- df_labeled %>% filter(group == "rest")

write.xlsx(df_sample1, "df_sample_clean_AZ.xlsx")
write.xlsx(df_sample2, "df_sample_clean_VK.xlsx")
write.xlsx(df_rest,    "df_sample_clean_MM.xlsx")


# === Check the final coding masks ===============================================

df_sample_clean_AZ   <- read_excel("df_sample_clean_AZ.xlsx")
df_sample_clean_VK   <- read_excel("df_sample_clean_VK.xlsx")
df_sample_clean_MM   <- read_excel("df_sample_clean_MM.xlsx")
df_sample_coded_RELI <- read_excel("df_sample_coded_RELI.xlsx")
df_full_paper_sample <- read_excel("full_paper_sample.xlsx")

# Extracting IDs
ids_full <- df_full_paper_sample$id_unique
ids_AZ   <- df_sample_clean_AZ$id_unique
ids_VK   <- df_sample_clean_VK$id_unique
ids_MM   <- df_sample_clean_MM$id_unique
ids_RELI <- df_sample_coded_RELI$id_unique

ids_union <- union(union(union(ids_AZ, ids_VK), ids_MM), ids_RELI)

# Missing IDs
all_match <- setequal(ids_full, ids_union)
cat("Exakter Match zwischen Full Sample und Splits? ", all_match, "\n")

missing_in_union <- setdiff(ids_full, ids_union)   # IDs fehlen in den Sub-Samples
extra_in_union   <- setdiff(ids_union, ids_full)   # IDs sind in Sub-Samples, aber nicht im Full Sample

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
duplicates_AZ <- df_sample_clean_AZ %>%
  count(id_unique) %>%
  filter(n > 1)
duplicates_VK <- df_sample_clean_VK %>%
  count(id_unique) %>%
  filter(n > 1)
duplicates_MM <- df_sample_clean_MM %>%
  count(id_unique) %>%
  filter(n > 1)
duplicates_RELI <- df_sample_coded_RELI %>%
  count(id_unique) %>%
  filter(n > 1)

duplicates_FULL <- df_full_paper_sample %>%
  count(id_unique) %>%
  filter(n > 1)

cat("\nDoppelte IDs in AZ:\n");   print(duplicates_AZ)
cat("\nDoppelte IDs in VK:\n");   print(duplicates_VK)
cat("\nDoppelte IDs in MM:\n");   print(duplicates_MM)
cat("\nDoppelte IDs in RELI:\n"); print(duplicates_RELI)
cat("\nDoppelte IDs in FULL:\n"); print(duplicates_FULL)


