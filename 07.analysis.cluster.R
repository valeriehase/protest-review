########################
#
# Cluster (spontaneous idea)
# Author: Miriam Milzner
# Date: 2025-10-24
#
########################
#
# Packages ---------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Var1: MethodCombo / MethodClass ----
qual_codes  <- c("21","22","23")
quant_codes <- c("24","25","26")
css_codes   <- c("10","11","12","13","14","15","16","17","18")

df_method_combo <- df %>%
  mutate(V11 = as.character(V11), method = as.character(method)) %>%
  separate_rows(V11, sep = ";") %>%
  mutate(V11 = str_trim(V11)) %>%
  group_by(id_unique, method, V12, V13) %>%
  summarise(
    has_qual  = any(V11 %in% qual_codes),
    has_quant = any(V11 %in% quant_codes),
    has_css   = any(V11 %in% css_codes),
    .groups = "drop"
  ) %>%
  mutate(
    MethodCombo = case_when(
      has_qual  & !has_quant & !has_css ~ "Qual only",
      !has_qual &  has_quant & !has_css ~ "Quant only",
      !has_qual & !has_quant &  has_css ~ "CSS only",
      has_qual  &  has_quant & !has_css ~ "Mixed Qual + Quant",
      !has_qual &  has_quant &  has_css ~ "Mixed Quant + CSS",
      has_qual  & !has_quant &  has_css ~ "Mixed Qual + CSS",
      has_qual  &  has_quant &  has_css ~ "Fully mixed",
      TRUE ~ "Unclassified"
    ),
    MethodClass = case_when(
      has_css   & !has_qual & !has_quant ~ "CSS",
      has_quant & !has_qual & !has_css   ~ "Quant",
      has_qual  & !has_quant & !has_css  ~ "Qual",
      has_qual  |  has_quant |  has_css  ~ "Mixed",
      TRUE ~ NA_character_
    )
  )

# Var2: PlatformType (V10_agg): "primäre" Plattformfamilie pro Studie ----

df_platform_primary <- df_V10agg %>%
  mutate(V10_agg = as.character(V10_agg)) %>%
  filter(!is.na(V10_agg), V10_agg != "NA") %>%
  count(id_unique, V10_agg, name = "n") %>%
  group_by(id_unique) %>%
  arrange(desc(n), V10_agg) %>%
  mutate(rank = row_number(),
         maxn = max(n)) %>%
  filter(n == maxn) %>%
  summarise(
    PlatformType = if (n() == 1) first(V10_agg) else "Mixed-platforms",
    .groups = "drop"
  )

# Var 3: GlobalSample (enthält Code "10") ----

df_global <- df %>%
  transmute(
    id_unique,
    GlobalSample = if_else(str_detect(as.character(V7), "(^|;)\\s*10(\\s*;|$)"),
                           "Global", "Non-Global")
  )

# Method (CSS vs Non-CSS) als Farbe, nicht in die Achsen ----
df_method <- df %>%
  distinct(id_unique, method) %>%
  mutate(Method = recode(as.character(method), "0" = "Non-CSS", "1" = "CSS")) %>%
  select(id_unique, Method)

# MCA-Input bauen ----

mca_input <- df_method_combo %>%
  select(id_unique, MethodCombo, MethodClass) %>%
  left_join(df_platform_primary, by = "id_unique") %>%
  left_join(df_global,            by = "id_unique") %>%
  left_join(df_method,            by = "id_unique") %>%

  mutate(
    across(c(MethodCombo, MethodClass, PlatformType, GlobalSample, Method), as.factor)
  ) %>%

  filter(!is.na(MethodCombo), !is.na(MethodClass),
         !is.na(PlatformType), !is.na(GlobalSample), !is.na(Method))

# MCA: aktive Variablen = Achsen; Method = supplementary (quali.sup = 5) ----

mca_res <- FactoMineR::MCA(
  mca_input %>% select(MethodCombo, MethodClass, PlatformType, GlobalSample, Method),
  quali.sup = 5,
  graph = FALSE
)

# Plot ----

plot_mca_clusters <- factoextra::fviz_mca_ind(
  mca_res,
  habillage   = "Method",     
  addEllipses = FALSE,        
  repel       = TRUE,
  palette     = c("grey30", "black")
) +
  ggplot2::labs(
    title = "MCA Map: Methodological Space of Online Protest Research",
    color = "Method Group"
  ) +
  ggplot2::theme_minimal(base_size = 13)

print(plot_mca_clusters)

# Clusteranalyse (3 Cluster), auf MCA basierend ----

cluster_res <- FactoMineR::HCPC(mca_res, nb.clust = 3, graph = FALSE)

plot_clusters <- factoextra::fviz_cluster(
  cluster_res, geom = "point", repel = TRUE
) +
  ggplot2::labs(title = "Methodological Prototypes of Online Protest Research") +
  ggplot2::theme_minimal(base_size = 13)

print(plot_clusters)

# Cluster Profiles ----

tbl_methodcombo   <- table(cluster_res$data.clust$clust, mca_input$MethodCombo)
tbl_methodclass   <- table(cluster_res$data.clust$clust, mca_input$MethodClass)
tbl_platformtype  <- table(cluster_res$data.clust$clust, mca_input$PlatformType)
tbl_globalsample  <- table(cluster_res$data.clust$clust, mca_input$GlobalSample)

tbl_methodcombo; tbl_methodclass; tbl_platformtype; tbl_globalsample
