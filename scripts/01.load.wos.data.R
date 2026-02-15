#
# Load raw Web of Science data & check for duplicates
# Author: Valerie Hase
# Date: 2024-04-05
#
# Setup ------------------------------------------------------------------------

if (!exists("PATHS")) source(here::here("R/paths.R"))
if (!exists("IN"))    source(here::here("R/config.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(tidytext)
library(widyr)

# 1.1 Load WoS data from Excel sheet & do some cleaning ------------------------

if (file.exists(IN$wos_abstracts)) {
  input_file <- require_file(IN$wos_abstracts, "WoS abstracts (preferred export)")
  message("Loading WoS abstracts: ", input_file)
  wos.abstracts <- readxl::read_excel(input_file)
} else {
  purrr::walk(IN$wos_legacy, require_file, what = "WoS legacy file")
  message("Loading WoS abstracts (legacy files):\n- ", paste(basename(IN$wos_legacy), collapse = "\n- "))
  wos.abstracts <- purrr::map_dfr(IN$wos_legacy, readxl::read_excel)
}

# Inspect data
#glimpse(wos_abstracts)

table(wos.abstracts$Language)

# Clean data (e.g., rename variable, filter relevant variables, filter relevant cases)
wos.abstracts <- wos.abstracts %>%
  
  #reduce to relevant cases
  dplyr::filter(Language == "English") %>%
  
  #rename variable in dplyr style
  dplyr::rename(id_wos = `UT (Unique WOS ID)`,
         authors = `Author Full Names`,
         title = `Article Title`,
         source.title = `Source Title`,
         source.type = `Document Type`,
         source.conference = `Conference Title`,
         keywords = `Author Keywords`,
         keywords.plus = `Keywords Plus`,
         abstract = Abstract,
         author.addresses = Addresses,
         author.affiliations = Affiliations,
         year = `Publication Year`,
         volume = Volume,
         issue = Issue,
         doi = DOI) %>%
  
  #filter out non-English abstracts not idenfied as such
  dplyr::filter(id_wos != "WOS:000338130200005") %>%
  
  #create placeholder for unique, shorter id
  dplyr::mutate(id_unique = NA) %>%

  #reduce to relevant variables
  dplyr::select(id_unique, id_wos, authors, title, abstract, 
         source.title, source.type, source.conference,
         keywords, keywords.plus,
         author.addresses, author.affiliations,
         year, volume, issue, doi) 

#for articles with 2024 as year: were they originally published in 2023?
check <- wos.abstracts %>%
  dplyr::filter(year == 2024) %>%
  dplyr::select(title, id_wos, doi) %>%
  dplyr::mutate(link = paste0("https://doi.org/", doi))

#manually check their website
#browseURL(check$link[48])

#set years to online first year, set volume and issue to NA
wos.abstracts <- wos.abstracts %>%
  
  #all years to 2023
  dplyr::mutate(year = replace(year,
                        id_wos %in% check$id_wos,
                        2023),
         
         #all volumes/issues to NA
         volume = replace(volume,
                          id_wos %in% check$id_wos,
                          NA),
         issue = replace(issue,
                         id_wos %in% check$id_wos,
                          NA),
         
         #expect for those already published in 2021
         year = replace(year,
                        id_wos %in% c("WOS:000730401500001", "WOS:000621157300001", "WOS:000721544900001",
                                  "WOS:000715965000001"),
                        2021),
         #expect for those already publised in 2022
         year = replace(year,
                        id_wos %in% c("WOS:000882767000001", "WOS:000795946700001", "WOS:000797400200001",
                                  "WOS:000773468100001", "WOS:000781753400001"),
                        2022))

rm(check)

# 1.2: Check for duplicates ----------------------------------------------------

#by ID: looks good
length(unique(wos.abstracts$id_wos))

#by DOI
wos.abstracts %>%
  dplyr::filter(!is.na(doi)) %>%
  dplyr::count(doi) %>%
  dplyr::filter(n>1)

#we drop 1 article included both as early access and as regular article based on same doi
wos.abstracts <- wos.abstracts %>%
    dplyr::filter(id_wos != "WOS:000953595300001")

#by title similarity
titles <- wos.abstracts %>%
  dplyr::mutate(title = tolower(title)) %>% 
  dplyr::select(title, id_wos) %>%
  tidytext::unnest_tokens(word, title) %>%
  dplyr::count(id_wos, word) %>%
  dplyr::ungroup() 

similarity <- titles %>%
                  widyr::pairwise_similarity(id_wos, word, n) %>%
                  dplyr::arrange(desc(similarity)) %>% 
                  dplyr::filter(similarity >= .8)

duplicates <- wos.abstracts %>%
  dplyr::filter(id_wos %in% similarity$item1 | id_wos %in% similarity$item2)

#we drop 3 more articles included both as early access and as regular article based on same title
wos.abstracts <- wos.abstracts %>%
  dplyr::filter(id_wos != "WOS:000209845700001" & id_wos != "WOS:000209845600001" & id_wos != "WOS:000777856400001")

#clean house
rm(duplicates, similarity, titles)

# 1.3 Create unique ID per article ---------------------------------------------

wos.abstracts <- wos.abstracts %>%
  dplyr::mutate(id_unique = paste0("ID", 1:nrow(wos.abstracts)))

# 1.4 Create N preliminary sample for PRISMA flow ------------------------------
n_deduplicated <- nrow(wos.abstracts)

# Export -----------------------------------------------------------------------

out_dir <- PATHS$int
stamp   <- format(Sys.time(), "%Y%m%d_%H%M")
out_file <- file.path(out_dir, paste0("01_wos_abstracts_clean_", stamp, ".rds"))
saveRDS(wos.abstracts, out_file)

message("01 completed.")
message("- Cleaned WoS abstracts saved to: ", out_file)
message("- n_deduplicated = ", n_deduplicated)



