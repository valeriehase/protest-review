########################
#
# Figures
# Author: Miriam Milzner, Valerie Hase
# Date: 2025-10-24
#
########################
#
# Packages ---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(tidycomm)
  library(janitor)
  library(stringr)
  library(openxlsx)
  library(flextable)
  library(officer)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

# 1. Prepare Data -----------------------------------------------------------------

df_full_sample_coded <- read_excel("data/processed/full_paper_sample_coded_clean.xlsx")

df <- df_full_sample_coded %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),  # anpassung variablennamen
    starts_with("V")) 

str(df)

vars <- c("V7", "V10", "V10_agg", "V11", "V12", "V13", "method")

df <- df %>%
  mutate(method = str_trim(method)) %>%    
  filter(method %in% c("0", "1")) %>%
  filter(!is.na(method)) %>%
  distinct(id_unique, .keep_all = TRUE) # remove duplicates. Note to self double check duplicates while cleaning the data

levels_V7 <- tibble(
  V7 = c(as.character(1:10), "NA"),
  V7_label = c(
    "North America",
    "South America",
    "Europe",
    "Russia and former Soviet Republics",
    "Middle East and North Africa",
    "East Asia",
    "Central and South (East) Asia",
    "Sub-Saharan Africa",
    "Oceania",
    "Global",
    "Not mentioned"
  )
)

levels_V10 <- tibble(
  V10 = c(
    "100", "110", "111", "112", "113", "114",
    "120", "121", "122", "124",
    "130", "131", "132", "133", "134",
    "140", "141", "142",
    "150", "151", "152", "153",
    "160", "161", "162",
    "200", "201", "202", "203", "204",
    "300", "400", "NA"
  ),
  V10_label = c(
    "Internet / online / social media (general)", "Websites (general)", "Websites of activist groups / NGOs / parties / companies", "News Media (online)", "Partisan news outlets", "Blogs",
    "Knowledge communities & discussion", "Specific discussion forum(s)", "Discord", "Reddit",
    "Social networking sites (general)", "Facebook", "Instagram", "TikTok", "VKontakte",
    "Microblogs", "X/Twitter", "Weibo", 
    "Video-sharing sites", "YouTube", "Vimeo", "Twitch",
    "Photo-sharing sites", "Flickr", "Tumblr",
    "Messengers (general)", "WhatsApp", "Telegram", "Signal", "WeChat",
    "News Media (offline)", "Other", "Not mentioned"
  )
)

df_V10agg <- df %>%
  tidyr::separate_rows(V10, sep = ";") %>%
  dplyr::mutate(V10 = stringr::str_trim(as.character(V10))) %>%
  dplyr::mutate(
    V10_agg = dplyr::case_when(
      V10 == "100" ~ "100",
      V10 %in% c("110","111","112","113","114") ~ "110",
      V10 %in% c("120","121","122","124") ~ "120",
      V10 %in% c("130","131","132","133","134") ~ "130",
      V10 %in% c("140","141","142") ~ "140",
      V10 %in% c("150","151","152","153") ~ "150",
      V10 %in% c("160","161","162") ~ "160",
      V10 %in% c("200","201","202","203","204") ~ "200",
      V10 == "300" ~ "300",
      V10 == "400" ~ "400",
      V10 %in% c("NA", NA) ~ "NA",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::mutate(V10_agg = as.character(V10_agg))

levels_V10_agg <- tibble(
  V10_agg = c("100", "110", "120", "130", "140", "150", "160", "200", "300", "400", "NA"),
  V10_agg_label = c(
    "Internet / online / social media (general)",
    "Websites generally",
    "Knowledge communities & discussion",
    "Social networking sites",
    "Microblogs",
    "Video-sharing sites",
    "Photo-sharing sites",
    "Messengers",
    "News Media (offline)",
    "Other",
    "Not mentioned"
  )
)


levels_V11 <- tibble(
  V11 = c(
    "10", "11","12","13","14","15","16","17","18",
    "20", "21","22","23","24","25","26",
    "99","NA"
  ),
  V11_label = c(
    "CSS-related (general)", 
    "Agent-based models / simulations", "API access", "Automated content analysis", "Data donation", "Eye-tracking", "Network analysis", "Tracking", "Web scraping",
    "Not-CSS-related (general)",
    "Qualitative content analysis", "Qualitative interviews / focus groups", "Qualitative observation", "Quantitative content analysis", "Quantitative observation", "Quantitative survey",
    "Other", "Not mentioned"
  )
)

levels_V12 <- tibble(
  V12 = c(
    "1",
    "0"
  ),
  V12_label = c(
    "cross-national",
    "not cross-national"
  )
)

levels_V13 <- tibble(
  V13 = c(
    "1",
    "0"
  ),
  V13_label = c(
    "experiment",
    "not experiment"
  )
)

# 2. Helper APA Figures  ----------------------------------------------------------

exclude <- function(df) df %>% filter(!Category %in% c("Other", "Not mentioned"))

make_complete_table <- function(df, var, levels_df, apa = FALSE, title = NULL, note = NULL) {
  var_sym <- rlang::sym(var)
  lab_col <- paste0(var, "_label")
  df <- df %>% mutate(!!var_sym := as.character(.data[[var]]))
  levels_df <- levels_df %>% mutate(!!var_sym := as.character(.data[[var]]))
  
  tab <- df %>%
    
    ##TO CHECK: this create several observations for each study with, e.g., several regions: Do we want that?
    tidyr::separate_rows(!!var_sym, sep = ";") %>%
    mutate(!!var_sym := dplyr::coalesce(trimws(!!var_sym), "Missing")) %>%
    count(method, !!var_sym, name = "n") %>%
    group_by(method) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  
  totals <- tab %>%
    group_by(method) %>%
    summarise(N = sum(n), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = method, values_from = N, names_prefix = "N_")
  
  tab_complete <- tidyr::expand_grid(!!var_sym := levels_df[[var]], method = c("0","1")) %>%
    left_join(tab, by = c(var, "method")) %>%
    mutate(across(c(n, pct), ~ tidyr::replace_na(.x, 0))) %>%
    left_join(levels_df, by = var) %>%
    tidyr::pivot_wider(names_from = method, values_from = c(n, pct), names_glue = "{.value}_{method}") %>%
    rename(
      Code = !!var_sym,
      Category = !!sym(lab_col),
      !!paste0("Non-CSS n (n=", totals$N_0, ")") := n_0,
      "Non-CSS %" = pct_0,
      !!paste0("CSS n (n=", totals$N_1, ")") := n_1,
      "CSS %" = pct_1
    ) %>%
    select(Code, Category, tidyselect::everything())
  
  if (!apa) return(tab_complete)
  
  ft <- tab_complete %>% flextable() %>% theme_vanilla() %>%
    align(align = "center", part = "all") %>% set_caption(title %||% paste("Table for", var)) %>% autofit()
  if (!is.null(note)) ft <- add_footer_lines(ft, values = note)
  ft
}

apa_figure <- function(doc, number, title, df, category_var, levels_vec = NULL) {
  df_long <- df %>%
    select(Category, matches("%$")) %>%
    tidyr::pivot_longer(cols = matches("%$"), names_to = "Method", values_to = "Percentage") %>%
    mutate(Method = recode(Method, "Non-CSS %" = "Non-CSS", "CSS %" = "CSS")) %>%
    exclude()
  
  if (!is.null(levels_vec)) df_long$Category <- factor(df_long$Category, levels = levels_vec)
  
  p <- ggplot(df_long, aes(x = Category, y = Percentage, fill = Method)) +
    geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_dodge(0.8), vjust = -0.8, size = 2) +
    scale_fill_manual(values = c("grey70","white")) +
    labs(x = category_var, y = "Percentage", fill = "Method Group") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 50, hjust = 1),
      plot.margin = margin(10, 10, 10, 100)
    )
  
  doc %>%
    body_add_par(paste0("Figure ", number), style = "Normal") %>%
    body_add_par(title, style = "Normal") %>%
    body_add_gg(value = p, width = 9, height = 6) %>%
    body_add_break()
}

share_one <- function(.df, var) {
  .df %>%
    mutate(val = as.character(.data[[var]]), method = as.character(method)) %>%
    filter(method %in% c("0","1")) %>%
    group_by(method) %>%
    summarise(
      n_total = n(),
      n_one   = sum(val == "1", na.rm = TRUE),
      pct_one = if_else(n_total > 0, round(100 * n_one / n_total, 1), 0),
      .groups = "drop"
    ) %>%
    mutate(variable = var)
}

# 3. Descriptive Figures  ----------------------------------------------------------

landscape <- prop_section(page_size = page_size(orient = "landscape"))
doc <- read_docx() %>% body_add_par("", style = "Normal") %>% body_set_default_section(landscape)

table_specs <- list(
  V7       = "Frequencies of Regions",
  V10      = "Frequencies of Platforms",
  V10_agg  = "Frequencies of Platforms (aggregated)",
  V11      = "Frequencies of Analysis Methods",
  V12      = "Frequency of Cross-National Research Designs",
  V13      = "Frequency of Experimental Research Designs"
)

fig_idx <- 1
for (var in names(table_specs)) {
  df_used <- if (var == "V10_agg") df_V10agg else df
  tab_raw <- make_complete_table(df_used, var, get(paste0("levels_", var)))
  doc <- apa_figure(
    doc          = doc,
    number       = fig_idx,
    title        = paste("Distribution of", table_specs[[var]], "by Method Group"),
    df           = tab_raw,
    category_var = table_specs[[var]],
    levels_vec   = get(paste0("levels_", var))[[paste0(var, "_label")]]
  )
  fig_idx <- fig_idx + 1
}

# Method Overall ohne split

v11_overall <- df %>%
  mutate(V11 = as.character(V11)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = stringr::str_trim(V11)) %>%
  count(V11, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  left_join(levels_V11, by = "V11") %>%
  filter(!V11_label %in% c("Other","Not mentioned")) %>%
  mutate(
    V11_label = factor(V11_label, levels = levels_V11$V11_label),
    y_lab = if_else(pct == 0, 0.5, pct)
  )

p_v11_overall <- ggplot(v11_overall, aes(x = V11_label, y = pct)) +
  geom_col(width = 0.7, color = "black", fill = "#7F7F7F") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)), vjust = -0.2, size = 3) +
  labs(x = NULL, y = "Percentage") +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(ylim = c(0, max(v11_overall$y_lab, na.rm = TRUE) + 5),
                  clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    axis.text.x        = element_text(angle = 35, hjust = 1),
    plot.margin        = margin(10, 50, 10, 30)  # mehr linker Rand
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Analysis Methods — % overall (no split)", style = "Normal") %>%
  body_add_par("Overall distribution of analysis methods (V11) across the full sample.", style = "Normal") %>%
  body_add_gg(value = p_v11_overall, width = 9, height = 5) %>%
  body_add_break()


# 4. World Map  ------------------------------------------------------------

#create the world map
worldmap_data <- df %>%
  
  #we split multiple regions mentioned in the same paper (e.g., 1;10)
  tidyr::separate_rows(V7, sep = ";") %>%
  dplyr::mutate(V7 = stringr::str_trim(as.character(V7)),
                V7 = dplyr::case_when(is.na(V7) ~ "NA", V7 == "" ~ "NA", TRUE ~ V7),
                
                #create string for CSS vs. non-CSS
                CSS = "non-CSS",
                CSS = replace(CSS, method == 1, "CSS")) %>%
  
  #turn to strings instead of numbers
  mutate(region = as.character(V7),
         region = case_when(region == 1 ~ "North America",
                            region == 2 ~ "South America",
                            region == 3 ~ "Europe",
                            region == 4 ~ "Russia and former Soviet Republics",
                            region == 5 ~ "Middle East and North Africa",
                            region == 6 ~ "East Asia",
                            region == 7 ~ "Central and South (East) Asia",
                            region == 8 ~ "Sub-Saharan Africa",
                            region == 9 ~ "Oceania",
                            region == 10 ~ NA)) %>% #global excluded here  
  
  #exclude global studies for this graph
  filter(!is.na(region)) %>%
  
  #exclude V7 - no longer needed
  select(-V7) %>%
  
  #create table
  count(CSS, region)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::select(iso_a3, name, region_un, subregion, continent, geometry)

#assign countries to region for mapping (in line with codebook)
region_map <- bind_rows(
  # (1) North America
  tibble(
    iso_a3 = c(
      "ATG","BHS","BRB","BLZ","CAN","CRI","CUB","DMA","SLV","GRD","GTM","HTI",
      "HND","JAM","MEX","NIC","PAN","KNA","LCA","VCT","TTO","USA"
    ),
    region = "North America"
  ),
  
  # (2) South America
  tibble(
    iso_a3 = c(
      "ARG","BOL","BRA","CHL","COL","ECU","FLK","GUF","GUY","PRY",
      "PER","SGS","SUR","URY","VEN"
    ),
    region = "South America"
  ),
  
  # (3) Europe (including Turkey and Armenia, excluding former Soviet Republics)
  tibble(
    iso_a3 = c(
      "ALB","AND","AUT","BEL","BIH","BGR","HRV","CYP","CZE","DNK","EST",
      "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA","LIE","LTU","LUX",
      "LVA","MLT","MCO","MNE","NLD","MKD","NOR","POL","PRT","ROU","SMR",
      "SRB","SVK","SVN","ESP","SWE","CHE","TUR","GBR","VAT","ARM"
    ),
    region = "Europe"
  ),
  
  # (4) Russia and former Soviet Republics
  tibble(
    iso_a3 = c(
      "RUS","UKR","BLR","AZE","KAZ","KGZ","TJK","TKM","UZB","EST","LVA","LTU"
    ),
    region = "Russia and former Soviet Republics"
  ),
  
  # (5) Middle East and North Africa
  tibble(
    iso_a3 = c(
      "DZA","BHR","EGY","IRN","IRQ","ISR","JOR","KWT","LBN","LBY",
      "MAR","OMN","QAT","SAU","SYR","TUN","ARE","YEM"
    ),
    region = "Middle East and North Africa"
  ),
  
  # (6) East Asia
  tibble(
    iso_a3 = c("CHN","JPN","MNG","PRK","KOR","HKG","TWN"),
    region = "East Asia"
  ),
  
  # (7) Central and South (East) Asia
  tibble(
    iso_a3 = c(
      "AFG","PAK","IND","THA","BGD","BTN","MDV","NPL","LKA",
      "BRN","KHM","TLS","IDN","LAO","MYS","MMR","PHL","SGP","VNM"
    ),
    region = "Central and South (East) Asia"
  ),
  
  # (8) Sub-Saharan Africa
  tibble(
    iso_a3 = c(
      "AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD","COM","COD",
      "COG","DJI","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB",
      "CIV","KEN","LSO","LBR","MDG","MWI","MLI","MRT","MUS","MOZ","NAM",
      "NER","NGA","RWA","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN",
      "TZA","TGO","UGA","ZMB","ZWE"
    ),
    region = "Sub-Saharan Africa"
  ),
  
  # (9) Oceania
  tibble(
    iso_a3 = c(
      "AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG",
      "WSM","SLB","TON","TUV","VUT"
    ),
    region = "Oceania"
  )
)

# attach our custom region to each country polygon
world_regions <- world %>%
  left_join(region_map, by = "iso_a3")

# add by method grouped study counts
world_counts <- world_regions %>%
  left_join(worldmap_data, by = c("region" = "region"), relationship = "many-to-many") %>%
  
  #for now, exclude countries without region
  filter(!is.na(region))

#In addition, save the number of global studies for facet labels
facet_labels <- df %>%
  
  #create global yes/no
  mutate(global = if_else(str_detect(V7, "10"), "Global", "Not-Global"),
         
         #create string for CSS vs. non-CSS
         CSS = "non-CSS",
         CSS = replace(CSS, method == 1, "CSS")) %>%
  
  # count studies per method and global/not-global
  count(CSS, global) %>%
  
  # within each CSS group, compute share that is global
  group_by(CSS) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  
  #keep only global count and create label for text annotation
  filter(global == "Global") %>%
  mutate(label_full = paste0(CSS, "\n(", percentage, "% of studies with global data)")) %>%
  
  # turn into a named vector: names = CSS, values = label_full
  select(CSS, label_full) %>%
  tibble::deframe()

worldmap <- ggplot(world_counts) +
  geom_sf(aes(fill = n), color = "grey30", linewidth = 0.1) +
  coord_sf(crs = "ESRI:54030") +
  scale_fill_gradient(
    name = "Studies (N)",
    low = "#f0f0f0",
    high = "#08306b",
    na.value = "white",
    guide = guide_colorbar(barwidth = 12, barheight = .4)) +
  facet_wrap(~ CSS, nrow = 1, labeller = as_labeller(facet_labels)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(linewidth = 0.1, color = "grey80"),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold"))

#clean house
rm(worldmap_data, world, region_map, world_regions, world_counts, facet_labels)

doc <- doc %>%
  body_add_par("Figure (supplement): Geographic distribution of studies across world regions", style = "Normal") %>%
  body_add_par("Shaded world maps by method group (CSS vs. Non-CSS). Darker shading indicates more studies in that region. Facet titles include % of studies using global samples.", style = "Normal") %>%
  body_add_gg(value = worldmap, width = 9, height = 5) %>%
  body_add_break()

# 5. Combined: Cross-national / Cross-Platform/ Experimental --------------------------------

df_platform_scope <- df %>%
  mutate(V10 = as.character(V10), method = as.character(method)) %>%
  tidyr::separate_rows(V10, sep = ";") %>%
  mutate(V10 = stringr::str_trim(V10),
         V10 = na_if(V10, ""),
         V10 = if_else(V10 == "NA", NA_character_, V10)) %>%
  group_by(id_unique, method) %>%
  summarise(n_platforms = n_distinct(V10, na.rm = TRUE), .groups = "drop") %>%
  mutate(PlatformScope = case_when(
    n_platforms == 0 ~ "NA",
    n_platforms == 1 ~ "SINGLE",
    n_platforms  > 1 ~ "CROSS",
    TRUE ~ "NA"
  ))

v12_share <- share_one(df, "V12") %>% mutate(Design = "Cross-national")
v13_share <- share_one(df, "V13") %>% mutate(Design = "Experimental")

ps_share <- df_platform_scope %>%
  mutate(method = as.character(method)) %>%
  count(method, PlatformScope, name = "n") %>%
  group_by(method) %>%
  mutate(pct_one = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(PlatformScope == "CROSS") %>%
  transmute(method, n_total = NA_integer_, n_one = n, pct_one, variable = "PlatformScope", Design = "Cross-platform")

design3_share <- bind_rows(v12_share %>% select(method, n_total, n_one, pct_one, variable, Design),
                           v13_share %>% select(method, n_total, n_one, pct_one, variable, Design),
                           ps_share %>% select(method, n_total, n_one, pct_one, variable, Design)) %>%
  mutate(
    Method = recode(method, "0" = "Non-CSS", "1" = "CSS"),
    y_lab  = if_else(pct_one == 0, 0.5, pct_one),
    Design = factor(Design, levels = c("Cross-national","Cross-platform","Experimental"))
  )

p_design3 <- ggplot(design3_share, aes(x = Design, y = pct_one, fill = Method)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f%%", pct_one)),
            position = position_dodge(0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "Percentage", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(design3_share$y_lab, na.rm = TRUE) + 6)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 0, hjust = 0.5),
    plot.margin        = margin(10, 10, 10, 10)
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Cross-national, Cross-platform, and Experimental Designs", style = "Normal") %>%
  body_add_par("Share of studies by design feature (cross-national, cross-platform, experimental), split by method group (CSS vs. Non-CSS).", style = "Normal") %>%
  body_add_gg(value = p_design3, width = 9, height = 5) %>%
  body_add_break()

# 6. Cross Tables Figures ----------------------------------------------------------------
method_levels_vec <- c("0","1")  # Non-CSS, CSS
selected_platforms <- c("100","110","130","140")

levels_V11_subset <- levels_V11 %>%
  filter(V11 %in% c("21","22","23","24","25","26"))

# Cross-national (V12==1) pro Methode (V11)
cross_counts <- df %>%
  mutate(V11 = as.character(V11), V12 = as.character(V12), method = as.character(method)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = str_trim(V11)) %>%
  filter(method %in% method_levels_vec, V12 == "1", V11 %in% levels_V11_subset$V11) %>%
  count(method, V11, name = "n")

totals <- cross_counts %>% group_by(method) %>% summarise(total = sum(n), .groups = "drop")

v11_cross_share <- tidyr::expand_grid(method = method_levels_vec, V11 = levels_V11_subset$V11) %>%
  left_join(cross_counts, by = c("method","V11")) %>%
  left_join(totals, by = "method") %>%
  mutate(n = tidyr::replace_na(n, 0), total = tidyr::replace_na(total, 0),
         pct = if_else(total > 0, round(100 * n / total, 1), 0)) %>%
  left_join(levels_V11_subset, by = "V11") %>%
  mutate(MethodGrp = recode(method, "0" = "Non-CSS", "1" = "CSS"),
         V11_label = factor(V11_label, levels = levels_V11_subset$V11_label),
         y_lab = if_else(pct == 0, 0.5, pct))

p_v11_cross <- ggplot(v11_cross_share, aes(x = V11_label, y = pct, fill = MethodGrp)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)),
            position = position_dodge(0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "% within design (cross-national)", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(v11_cross_share$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "top",
        legend.title       = element_text(face = "bold"),
        axis.text.x        = element_text(angle = 35, hjust = 1))

doc <- doc %>%
  body_add_par("Figure (supplement): Analysis Methods — % within design (cross-national only)", style = "Normal") %>%
  body_add_par("Bar chart of cross-national design by analysis method, split by CSS vs. Non-CSS. Matches the left block of the V11 × V12 table.", style = "Normal") %>%
  body_add_gg(value = p_v11_cross, width = 9, height = 5) %>%
  body_add_break()

# Cross-national (V12==1) pro Plattform (V10_agg)
platform_cross_counts <- df_V10agg %>%
  mutate(V10_agg = as.character(V10_agg), V12 = as.character(V12), method = as.character(method)) %>%
  filter(method %in% method_levels_vec, V12 == "1", V10_agg %in% selected_platforms) %>%
  count(method, V10_agg, name = "n")

platform_totals <- platform_cross_counts %>% group_by(method) %>% summarise(total = sum(n), .groups = "drop")

v10_cross_share <- tidyr::expand_grid(method = method_levels_vec, V10_agg = selected_platforms) %>%
  left_join(platform_cross_counts, by = c("method","V10_agg")) %>%
  left_join(platform_totals, by = "method") %>%
  mutate(n = tidyr::replace_na(n, 0), total = tidyr::replace_na(total, 0),
         pct = if_else(total > 0, round(100 * n / total, 1), 0)) %>%
  left_join(levels_V10_agg, by = "V10_agg") %>%
  mutate(MethodGrp = recode(method, "0" = "Non-CSS", "1" = "CSS"),
         V10_agg_label = factor(
           V10_agg_label,
           levels = levels_V10_agg %>% filter(V10_agg %in% selected_platforms) %>% pull(V10_agg_label)
         ),
         y_lab = if_else(pct == 0, 0.5, pct))

p_v10_cross <- ggplot(v10_cross_share, aes(x = V10_agg_label, y = pct, fill = MethodGrp)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)),
            position = position_dodge(0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70", "white")) +
  labs(x = NULL, y = "% within design (cross-national)", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(v10_cross_share$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "top",
        legend.title       = element_text(face = "bold"),
        axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin        = margin(10, 30, 10, 30))

doc <- doc %>%
  body_add_par("Figure (supplement): Platforms — % within design (cross-national only)", style = "Normal") %>%
  body_add_par("Bar chart of cross-national design by platform type (V10_agg), split by CSS vs. Non-CSS. Matches the left block of the V12 × V10_agg logic.", style = "Normal") %>%
  body_add_gg(value = p_v10_cross, width = 9, height = 5) %>%
  body_add_break()

# 7. MethodCombo ------------------------------------------------------------------------

qual_codes  <- c("21","22","23")
quant_codes <- c("24","25","26")
css_codes   <- c("10","11","12","13","14","15","16","17","18")

df_method_combo <- df %>%
  mutate(V11 = as.character(V11), method = as.character(method)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = stringr::str_trim(V11)) %>%
  group_by(id_unique, method) %>%
  summarise(
    has_qual  = any(V11 %in% qual_codes,  na.rm = TRUE),
    has_quant = any(V11 %in% quant_codes, na.rm = TRUE),
    has_css   = any(V11 %in% css_codes,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MethodCombo = case_when(
      has_qual  & !has_quant & !has_css ~ "Q_ONLY",
      !has_qual &  has_quant & !has_css ~ "QT_ONLY",
      !has_qual & !has_quant &  has_css ~ "CSS_ONLY",
      has_qual  &  has_quant & !has_css ~ "MIX_QUAL_QUANT",
      !has_qual &  has_quant &  has_css ~ "MIX_QUANT_CSS",
      has_qual  & !has_quant &  has_css ~ "MIX_QUAL_CSS",
      has_qual  &  has_quant &  has_css ~ "MIX_ALL",
      TRUE ~ "Uncoded"
    )
  )

levels_MethodCombo <- tibble::tibble(
  MethodCombo = c(
    "Q_ONLY","QT_ONLY","CSS_ONLY",
    "MIX_QUAL_QUANT","MIX_QUANT_CSS","MIX_QUAL_CSS","MIX_ALL","Uncoded"
  ),
  MethodCombo_label = c(
    "Qualitative only","Quantitative only","CSS only",
    "Mixed Qual + Quant","Mixed Quant + CSS","Mixed Qual + CSS","Fully Mixed (Qual + Quant + CSS)",
    "Not mentioned"
  )
)

mc_share <- df_method_combo %>%
  count(MethodCombo, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  right_join(
    expand_grid(MethodCombo = levels_MethodCombo$MethodCombo),
    by = c("MethodCombo")
  ) %>%
  mutate(n = replace_na(n, 0), pct = replace_na(pct, 0)) %>%
  left_join(levels_MethodCombo, by = "MethodCombo") %>%
  filter(MethodCombo_label != "Not mentioned") %>%  # wie bei exclude()
  mutate(
    MethodCombo_label = factor(MethodCombo_label, levels = levels_MethodCombo$MethodCombo_label),
    y_lab = if_else(pct == 0, 0.5, pct)
  )

p_methodcombo <- ggplot(mc_share, aes(x = MethodCombo_label, y = pct)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)),
            position = position_dodge(0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "Percentage") +
  coord_cartesian(ylim = c(0, max(mc_share$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 35, hjust = 1)
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Method combinations — overall %", style = "Normal") %>%
  body_add_par("Bar chart of method-combination types.", style = "Normal") %>%
  body_add_gg(value = p_methodcombo, width = 9, height = 5) %>%
  body_add_break()

# Export to Word ------------------------------------------------------------------------

print(doc, target = "output/figures/All_Figures.docx")
