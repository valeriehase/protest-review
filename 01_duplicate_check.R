########################
#
# Duplicate check for NW literature review
# Author: Daniel Thiele
# Date: 2023-07-24
#
########################

# Load data ---------------------------------------------------------------

# Annett's data

# Get paths
paths_savedrecs <- dir("data/Annett/", pattern = "savedrecs", full.names = T) 

# Loop over all paths
savedrecs <- vector("list", length(paths_savedrecs)) # create an empty vector
for (i in seq_along(savedrecs)) {
  savedrecs[[i]] <- readxl::read_xls(paths_savedrecs[[i]], col_names = T, trim_ws = T, .name_repair = "minimal")
}
# Bind
savedrecs <- bind_rows(savedrecs)


## Other data
checkedrecs <- read.csv2("data/Annett/Gegencheck.csv", strip.white = T, na.strings = "")


# Inspect data ------------------------------------------------------------

glimpse(checkedrecs)
glimpse(savedrecs)


# Fix column names
names(savedrecs)<-make.names(names(savedrecs),unique = TRUE)


# Check if dfs are identical
all.equal(savedrecs, checkedrecs)
#...well almost

# Make sure Article Tiles are idnetical in both dfs:
comp_v <- checkedrecs$Article.Title == savedrecs$Article.Title
length(comp_v[comp_v==F])
# Yes, 0 cases differ.

# Inspect DOIs
sum(is.na(checkedrecs$DOI))
sum(is.na(savedrecs$DOI))
# 239 missing in both cases


# Is there a unique identifier?
sum(is.na(savedrecs$UT..Unique.WOS.ID.))
sum(is.na(checkedrecs$UT..Unique.WOS.ID.))
# looks good

# Is it unique?
savedrecs |> distinct(UT..Unique.WOS.ID.) |> nrow() == nrow(savedrecs)
checkedrecs |> distinct(UT..Unique.WOS.ID.) |> nrow() == nrow(checkedrecs)
# check!

## Rename WoS ID
savedrecs$wos_id <- savedrecs$UT..Unique.WOS.ID.
checkedrecs$wos_id <- checkedrecs$UT..Unique.WOS.ID.


# Check if there are DOI duplicats ----------------------------------------

# Keep entries without DOI
savedrecs_wo_doi <- savedrecs |> filter(is.na(DOI))
savedrecs_w_doi <- savedrecs |> filter(!is.na(DOI)) |> distinct(DOI, .keep_all = T)

# Bind
savedrecs_b <- bind_rows(savedrecs_wo_doi, savedrecs_w_doi)

# Compare N
nrow(savedrecs) - nrow(savedrecs_b)


# In 1 case a DOI duplicate was dropped.
# Inspect that duplicate:
doi_dup <- savedrecs |> filter(!wos_id %in% savedrecs_w_doi$wos_id,
                               !wos_id %in% savedrecs_wo_doi$wos_id) 

# Inspect both entries:
savedrecs |> 
  filter(DOI == doi_dup$DOI[[1]]) |>
  select(Authors, Article.Title, Source.Title, Document.Type) |> glimpse()

# Dropped entry is the "Early Access" version of the same article. That's fine!
# Rows: 2
# Columns: 4
# $ Authors       <chr> "Bhowmik, M; Ben Rogaly", "Bhowmik, M; Rogaly, B"
# $ Article.Title <chr> "In search of unbordered homelands: Exploring the role of music in building affective internationalist politics of solidarity", "In search o…
# $ Source.Title  <chr> "SOCIOLOGICAL REVIEW", "SOCIOLOGICAL REVIEW"
# $ Document.Type <chr> "Article", "Article; Early Access"


## Create flag in original df
doi_dups <- savedrecs |> 
            filter(DOI == doi_dup$DOI[[1]]) |>
            select(wos_id, DOI)
doi_dups_2 <- doi_dups |> select(DOI, DUPLICATE_wos_id = wos_id)


doi_dups <- left_join(doi_dups, doi_dups_2, relationship = "many-to-many") |> select(-DOI) |> filter(wos_id != DUPLICATE_wos_id)


# Check for duplicates by title similarity --------------------------------------------------

library(tidytext)

words_df  <- savedrecs_b |>
                mutate(title_proc = tolower(Article.Title)) |> 
                unnest_tokens(word, title_proc) |>
                count(wos_id, word) |>
                ungroup() 

similarity_df <- words_df|>
                  widyr::pairwise_similarity(wos_id, word, n) |>
                  arrange(desc(similarity)) |> 
                  filter(similarity >= .9)

m1 <- savedrecs_b %>% select(item1=wos_id, Article.Title_1=Article.Title, Authors_1 = Authors, Source.Title_1 =  Source.Title, DOI_1 = DOI, Document.Type_1 = Document.Type)
m2 <- savedrecs_b %>% select(item2=wos_id, Article.Title_2=Article.Title, Authors_2 = Authors, Source.Title_2 =  Source.Title, DOI_2 = DOI, Document.Type_2 = Document.Type)

similarity_df <- left_join(similarity_df, m1) %>% left_join(m2)

## Identified 5 unique papers that have a duplicate 
# (NOTE: Setting similarity threshold lower does only identify false positives)

# Inspect:
similarity_df |> select(Article.Title_1, Article.Title_2, Authors_1, Authors_2, Source.Title_1, Source.Title_2, Document.Type_1, Document.Type_2)

# One is the "Early access" duplicate of an Aricle and can be dropped:
# drop_early <- similarity_df |> filter(stringr::str_detect(Document.Type_2, "Early")) |> pull(item2)
# 
# savedrecs_sub <- savedrecs_b |> filter(!wos_id %in% drop_early)  

# Duplicates from Title similarity 
dup_df <- similarity_df |> 
  select(wos_id = item1, DUPLICATE_wos_id = item2)

# Add Duplicates identified by DOI
dup_df <- bind_rows(dup_df, doi_dups)

# Merge flag to original df
savedrecs_flagged <- left_join(savedrecs, dup_df) |> mutate(HAS_DUPLICATE = ifelse(is.na(DUPLICATE_wos_id), 0, 1))

# Save --------------------------------------------------------------------

writexl::write_xlsx(savedrecs_flagged, "data/savedrecs_SSCI_dup_flagged_DT20230725.xlsx")