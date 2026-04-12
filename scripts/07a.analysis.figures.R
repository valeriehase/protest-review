#
# Final Analysis - Figures
# Date: 2026-04-10
#
# Setup ------------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)
library(tidycomm)
library(here)
library(janitor)
library(stringr)
library(openxlsx)
library(flextable)
library(officer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

source(here::here("helper functions/paths.R"))
source(here::here("helper functions/config.R"))
source(here::here("helper functions/helpers.R"))
source(here::here("helper functions/codebook.R"))

# Load Input -------------------------------------------------------------------

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

# 7.1 Prepare Data ------------------------------------------------------------

df <- final_sample %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),  # anpassung variablennamen
    starts_with("V")) 

str(df)

vars <- c("V7", "V10", "V10_agg", "V11", "V12", "V13", "method")

# final check for any wrong data still being included
df <- df %>%
  mutate(method = str_trim(method)) %>%    
  filter(method %in% c("0", "1")) %>%
  filter(!is.na(method)) %>%
  distinct(id_unique, .keep_all = TRUE) 

df_V10agg <- make_df_V10agg(df)

# 7.2 Descriptive Figures  -----------------------------------------------------

landscape <- prop_section(page_size = page_size(orient = "landscape"))
doc <- read_docx() %>% 
  body_add_par("", style = "Normal") %>% 
  body_set_default_section(landscape)

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

# Method Overall without CSS- vs. non-CSS split

v11_overall <- df %>%
  mutate(V11 = as.character(V11)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = stringr::str_trim(V11)) %>%
  
  #count unique method per unique study
  distinct(id_unique, V11) %>%
  
  #create sum
  count(V11, name = "n") %>%
  
  # % per unique studies
  mutate(
    total_studies = n_distinct(df$id_unique),
    pct = round(100 * n / total_studies, 1)
  ) %>%
  
  # add method names as labels
  left_join(levels_V11, by = "V11") %>%
  
  #only keep relevant ones
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

# Method Overall by collection vs. analysis without CSS- vs. non-CSS split

v11_overall_type <- df %>%
  mutate(V11 = as.character(V11)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = stringr::str_trim(V11)) %>%
  
  #count unique method per unique study
  distinct(id_unique, V11) %>%
  
  #create sum
  count(V11, name = "n") %>%
  
  # add label
  mutate(type = "data collection",
         type = replace(type,
                        V11 %!in% c("12", "14", "15", "17", "18", "22", "23", "25", "26"),
                        "data analysis"),
         type = replace(type,
                        V11 %in% c("99", "20", "10"),
                        NA)) %>%
  
  # % per unique studies
  mutate(
    total_studies = n_distinct(df$id_unique),
    pct = round(100 * n / total_studies, 1)
  ) %>%
  
  # add method names as labels
  left_join(levels_V11, by = "V11") %>%
  
  #only keep relevant ones
  filter(!is.na(type)) %>%
  mutate(
    V11_label = factor(V11_label, levels = levels_V11$V11_label),
    y_lab = if_else(pct == 0, 0.5, pct)
  ) %>%
  
  #sort by type for order in plot
  mutate(
    type = factor(type, levels = c("data collection", "data analysis"))
  ) %>%
  arrange(type, V11_label) %>%
  mutate(
    V11_label = factor(V11_label, levels = unique(V11_label))
  )

p_v11_overall_type <- ggplot(v11_overall_type, aes(x = V11_label, y = pct)) +
  geom_col(aes(fill = type), width = 0.7, color = "black") +
  scale_fill_manual(
    values = c(
      "data collection" = "gray30",
      "data analysis"   = "gray70"
    ),
    name = NULL
  ) +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)), vjust = -0.2, size = 3) +
  labs(x = NULL, y = "Percentage") +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(ylim = c(0, max(v11_overall$y_lab, na.rm = TRUE) + 5),
                  clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    axis.text.x        = element_text(angle = 35, hjust = 1),
    plot.margin        = margin(10, 50, 10, 30)  # mehr linker Rand
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Analysis Methods — % overall (split by data collection & analysis)", style = "Normal") %>%
  body_add_par("Overall distribution of analysis methods (V11) across the full sample, by type of method.", style = "Normal") %>%
  body_add_gg(value = p_v11_overall_type, width = 9, height = 5) %>%
  body_add_break()

# Get frequent pairs of method combinations

pair_pct <- count_method_combinations(df, combo_sizes = c(2, 3, 4), var = "V11", top_n = 10) %>%
  
  #add labels
  mutate(
    label = case_when(
      combo  == "21;22" ~ "qual. interviews/focus groups &\nqual. content analysis/discourse analysis",
      combo  == "12;13" ~ "API access &\nautomated content analysis",
      combo  == "12;16" ~ "API access &\nnetwork analysis",
      combo  == "13;16" ~ "automated content analysis &\nnetwork analysis",
      combo  == "13;21" ~ "automated content analysis &\nqual. content analysis/discourse analysis",
      combo  == "16;21" ~ "qual. content analysis/discourse analysis &\nnetwork analysis",
      combo  == "12;21" ~ "API access &\nqual. content analysis/discourse analysis",
      combo  == "22;23" ~ "qual. interviews/focus groups &\nqual. observation",
      combo  == "12;13;16" ~ "API access, automated content analysis &\nnetwork analysis",
      combo  == "16;18" ~ "web scraping &\nnetwork analysis"
    )
  ) %>%
  
  #only keep those used in more than 10% of studies
  filter(pct >= 10)

#visualize
p_pairs <- ggplot(pair_pct, aes(x = reorder(label, pct), y = pct)) +
  geom_col(fill = "gray35", color = "black", width = 0.65) +
  
  geom_text(
    aes(label = sprintf("%.1f", pct)),
    hjust = -0.15,
    size = 3.2
  ) +
  
  coord_flip(clip = "off") +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  labs(
    x = NULL,
    y = "Percentage of studies"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "gray85"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.margin = margin(10, 40, 10, 10)
  )

doc <- doc %>%
  body_add_par("Figure (supplement): Combinations of methods — % overall", style = "Normal") %>%
  body_add_par("Overall combination of methods (V11) across the full sample, all above 10%", style = "Normal") %>%
  body_add_gg(value = p_pairs, width = 9, height = 5) %>%
  body_add_break()


# 7.3 World Map  ---------------------------------------------------------------

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
  dplyr::select(iso_a3, name, region_un, subregion, continent, geometry) %>%
  
  #correct some missing countries
  mutate(iso_a3 = replace(iso_a3,
                          name == "France",
                          "FRANCE"),
         
         iso_a3 = replace(iso_a3,
                          name == "Kosovo",
                          "KOSOVO"),
         
         iso_a3 = replace(iso_a3,
                          name == "Norway",
                          "NORWAY"),
         
         iso_a3 = replace(iso_a3,
                          name == "Somaliland",
                          "SOMALILAND"))


#assign countries to region for mapping (in line with codebook)
region_map <- bind_rows(
  # (1) North America
  tibble(
    iso_a3 = c(
      "ATG","BHS","BRB","BLZ","CAN","CRI","CUB","DMA","SLV","GRD","GTM","HTI",
      "HND","JAM","MEX","NIC","PAN","KNA","LCA","VCT","TTO","USA",
      
      #added later-on, as missing iso-codes in our codebook
      "VIR", "PRI", "AIA", "CYM", "BMU", "VGB", "TCA", "MSR", "ABW", "CUW",
      "SPM", "MAF", "BLM", "DOM", "GRL", "SXM"
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
      "SRB","SVK","SVN","ESP","SWE","CHE","TUR","GBR","VAT","ARM",
      
      #added later-on, as missing iso-codes in our codebook
      "JEY", "GGY", "IMN", "MDA", "ALA", "FRO",
      
      #added later-on, as missing iso-codes in the data
      "FRANCE", "KOSOVO", "NORWAY"
    ),
    region = "Europe"
  ),
  
  # (4) Russia and former Soviet Republics
  tibble(
    iso_a3 = c(
      "RUS","UKR","BLR","AZE","KAZ","KGZ","TJK","TKM","UZB","EST","LVA","LTU",
      
      #added later-on, as missing iso-codes in our codebook
      "GEO"
    ),
    region = "Russia and former Soviet Republics"
  ),
  
  # (5) Middle East and North Africa
  tibble(
    iso_a3 = c(
      "DZA","BHR","EGY","IRN","IRQ","ISR","JOR","KWT","LBN","LBY",
      "MAR","OMN","QAT","SAU","SYR","TUN","ARE","YEM",
      
      #added later-on, as missing iso-codes in our codebook
      "ESH", "PSE"
    ),
    region = "Middle East and North Africa"
  ),
  
  # (6) East Asia
  tibble(
    iso_a3 = c("CHN","JPN","MNG","PRK","KOR","HKG","TWN",
               
               #added later-on, as missing iso-codes in our codebook
               "MAC"
   ),
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
      "TZA","TGO","UGA","ZMB","ZWE",
      
      #added later-on, as missing iso-codes in the data
      "SOMALILAND"
    ),
    region = "Sub-Saharan Africa"
  ),
  
  # (9) Oceania
  tibble(
    iso_a3 = c(
      "AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG",
      "WSM","SLB","TON","TUV","VUT", 
      
      #added later-on, as missing iso-codes in our codebook
      "MNP", "GUM", "ASM", "PCN", "NIU", "COK", "WLF", "PYF", "NCL", "NFK"
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

# 7.3 Combined: Cross-national / Cross-Platform/ Experimental --------------------------------

# Amount of cross-platform studies across methods
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

# Amount of cross-national studies across methods
v12_share <- share_one(df, "V12") %>% 
  mutate(Design = "Cross-national")

# Amount of experimental studies across methods
v13_share <- share_one(df, "V13") %>% 
  mutate(Design = "Experimental")

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

# 7.4 Cross Tables Figures ----------------------------------------------------------------

# Cross-national (V12==1) per method (V11)

#define relevant methods
levels_V11_subset <- levels_V11 %>%
  filter(V11 %in% c("21","22","23","24","25","26"))

method_levels_vec <- c("0","1")  # Non-CSS, CSS

# number of cross-national designs in CSS- vs. non-CSS studies that employ specific method
cross_counts <- df %>%
  mutate(V11 = as.character(V11), V12 = as.character(V12)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = str_trim(V11)) %>%
  filter(V12 == "1", V11 %in% levels_V11_subset$V11) %>%
  count(method, V11, name = "n")

# out of all studies that employ specific method in CSS vs. non-CSS studies
totals <- df %>%
  mutate(V11 = as.character(V11), V12 = as.character(V12)) %>%
  tidyr::separate_rows(V11, sep = ";") %>%
  mutate(V11 = str_trim(V11)) %>%
  filter(V11 %in% levels_V11_subset$V11) %>%
  count(method, V11, name = "total")

v11_cross_share <- tidyr::expand_grid(V11 = levels_V11_subset$V11, method = method_levels_vec) %>%
  left_join(cross_counts, by = c("V11", "method")) %>%
  left_join(totals, by = c("method", "V11")) %>%
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
  body_add_par("Figure (supplement): Analysis Methods — % cross-national per method", style = "Normal") %>%
  body_add_par("Bar chart of cross-national designs by analysis method, split by CSS vs. Non-CSS.", style = "Normal") %>%
  body_add_gg(value = p_v11_cross, width = 9, height = 5) %>%
  body_add_break()

# 7.5 MethodCombo ------------------------------------------------------------------------

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
      has_css & (has_qual | has_quant) ~ "MIX_QUANT_QUAL_CSS",
      TRUE ~ "Other"
    )
  )

levels_MethodCombo <- tibble::tibble(
  MethodCombo = c(
    "Q_ONLY","QT_ONLY","CSS_ONLY",
    "MIX_QUAL_QUANT","MIX_QUANT_QUAL_CSS","Other"
  ),
  MethodCombo_label = c(
    "Qualitative only","Quantitative only","CSS only",
    "Mixed, not including CSS","Mixed, including CSS",
    "Other"
  )
)

mc_share <- df_method_combo %>%
  count(MethodCombo, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  left_join(levels_MethodCombo, by = "MethodCombo") %>%
  filter(MethodCombo_label != "Other") %>%  # wie bei exclude()
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



# 7.6 Over Time ----------------------------------------------------------------

# Load publication year from script 01 output
if (!exists("wos_abstracts_clean", inherits = FALSE)) {
  
  dir_path <- PATHS$int
  
  files <- list.files(
    dir_path,
    pattern = "^01_wos_abstracts_clean(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No cleaned WoS file found, please run script 01 first.")
  }
  
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
    "\\1",
    files
  )
  
  timestamps <- as.POSIXct(
    timestamps,
    format = "%Y%m%d_%H%M",
    tz = "UTC"
  )
  
  latest_file <- files[which.max(timestamps)]
  
  message("Loading cleaned WoS data from: ", timestamps[which.max(timestamps)])
  
  wos_abstracts_clean <- readRDS(latest_file)
  
  rm(dir_path, files, timestamps, latest_file)
}


df_time <- final_sample %>%
  rename_with(
    ~ str_extract(.x, "^V\\d+"),
    starts_with("V")
  ) %>%
  mutate(id_unique = as.character(id_unique)) %>%
  left_join(
    wos_abstracts_clean %>%
      transmute(
        id_unique = as.character(id_unique),
        year = as.integer(year)
      ),
    by = "id_unique"
  )

#sanity check
df_time %>%
  count(id_unique) %>%
  filter(n > 1)

df_time <- df_time %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year))

# Integrate graph for N publications over time
n_time <- df_time %>%
  group_by(year) %>%
  count(method) %>%
  ungroup() %>%
  mutate(method = factor(method,
                         levels = c(0, 1),
                         labels = c("Non-CSS", "CSS")))

p_n_time <- ggplot(n_time, aes(x = year, y = n, color = method, group = method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Non-CSS" = "gray70",
                                "CSS"     = "gray30")) +
  labs(x = NULL, y = "Number of studies", color = "Method for Studying Online Protest") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    axis.text.x        = element_text(angle = 0)
  )

doc <- doc %>%
  body_add_par("Figure: N Publications over time", style = "Normal") %>%
  body_add_par("Distribution of number of publications by method across time.", style = "Normal") %>%
  body_add_gg(value = p_n_time, width = 9, height = 5) %>%
  body_add_break()

# Switch to categorical time periods
df_time <- df_time %>%
  mutate(
    year = as.integer(year),
    period = case_when(
      year %in% 2009:2013 ~ "2009-2013",
      year %in% 2014:2016 ~ "2014-2016",
      year %in% 2017:2019 ~ "2017-2019",
      year %in% 2020:2021 ~ "2020-2021",
      year %in% 2022:2023 ~ "2022-2023",
      TRUE ~ NA_character_
    ),
    period = factor(
      period,
      levels = c("2009-2013", "2014-2016", "2017-2019", "2020-2021", "2022-2023")
    )
  ) %>%
  filter(!is.na(period))

# Method combinations over time

mc_time <- df_method_combo %>%
  left_join(df_time %>% select(id_unique, period), by = "id_unique") %>%
  count(period, MethodCombo, name = "n") %>%
  group_by(period) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  left_join(levels_MethodCombo, by = "MethodCombo") %>%
  filter(MethodCombo_label != "Other") %>%
  mutate(
    MethodCombo_label = factor(MethodCombo_label, levels = levels_MethodCombo$MethodCombo_label),
    y_lab = if_else(pct == 0, 0.5, pct)
  )

p_mc_time <- ggplot(mc_time, aes(x = period, y = pct, fill = MethodCombo_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f", pct)),
            position = position_dodge(0.8), vjust = -0.2, size = 3) +
  labs(x = NULL, y = "Percentage", fill = "Method Combination") +
  coord_cartesian(ylim = c(0, max(mc_time$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    axis.text.x        = element_text(angle = 0)
  )

doc <- doc %>%
  body_add_par("Figure: Method combinations over time", style = "Normal") %>%
  body_add_par("Distribution of method-combination types across time periods.", style = "Normal") %>%
  body_add_gg(value = p_mc_time, width = 9, height = 5) %>%
  body_add_break()

# CSS vs Non-CSS over time

css_time <- df_time %>%
  mutate(Method = recode(method, "0" = "Non-CSS", "1" = "CSS")) %>%
  count(period, Method, name = "n") %>%
  group_by(period) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(y_lab = if_else(pct == 0, 0.5, pct))

p_css_time <- ggplot(css_time, aes(x = period, y = pct, fill = Method)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_text(aes(y = y_lab, label = sprintf("%.1f%%", pct)),
            position = position_dodge(0.8), vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("grey70","white")) +
  labs(x = NULL, y = "Percentage", fill = "Method Group") +
  coord_cartesian(ylim = c(0, max(css_time$y_lab, na.rm = TRUE) + 5)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top"
  )

doc <- doc %>%
  body_add_par("Figure: CSS vs. Non-CSS over time", style = "Normal") %>%
  body_add_par("Share of CSS vs. non-CSS studies across time periods.", style = "Normal") %>%
  body_add_gg(value = p_css_time, width = 9, height = 5) %>%
  body_add_break()

# Platforms over time

selected_platforms <- c("100","110","130","140")

v10_time <- df_V10agg %>%
  left_join(df_time %>% select(id_unique, period), by = "id_unique") %>%
  filter(V10_agg %in% selected_platforms) %>%
  count(period, V10_agg, name = "n") %>%
  group_by(period) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  left_join(levels_V10_agg, by = "V10_agg") %>%
  mutate(
    V10_agg_label = factor(V10_agg_label,
                           levels = levels_V10_agg %>%
                             filter(V10_agg %in% selected_platforms) %>%
                             pull(V10_agg_label))
  )

p_v10_time <- ggplot(v10_time, aes(x = period, y = pct, fill = V10_agg_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  labs(x = NULL, y = "Percentage", fill = "Platform") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 0)
  )

doc <- doc %>%
  body_add_par("Figure: Platforms over time", style = "Normal") %>%
  body_add_par("Distribution of selected platforms across time periods.", style = "Normal") %>%
  body_add_gg(value = p_v10_time, width = 9, height = 5) %>%
  body_add_break()

# Methods over time

top_v11 <- c("21","22","23","24","25","26")

v11_time <- df_time %>%
  mutate(V11 = as.character(V11)) %>%
  separate_rows(V11, sep = ";") %>%
  mutate(V11 = str_trim(V11)) %>%
  filter(V11 %in% top_v11) %>%
  count(period, V11, name = "n") %>%
  group_by(period) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  left_join(levels_V11, by = "V11") %>%
  mutate(
    V11_label = factor(V11_label, levels = levels_V11$V11_label),
    y_lab = if_else(pct == 0, 0.5, pct)
  )

p_v11_time <- ggplot(v11_time, aes(x = period, y = pct, fill = V11_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  labs(x = NULL, y = "Percentage", fill = "Method") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 0)
  )

doc <- doc %>%
  body_add_par("Figure: Analysis methods over time", style = "Normal") %>%
  body_add_par("Distribution of selected analysis methods (V11) across time periods.", style = "Normal") %>%
  body_add_gg(value = p_v11_time, width = 9, height = 5) %>%
  body_add_break()

# Export -----------------------------------------------------------------------

out_dir <- PATHS$final
stamp   <- format(Sys.time(), "%Y%m%d_%H%M")
out_file <- file.path(out_dir, paste0("07_analysis_figures_", stamp, ".docx"))

print(doc, target = out_file)

message("07 figures completed.")
message("- Word document saved to: ", out_file)
