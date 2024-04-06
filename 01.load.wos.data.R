########################
#
# Load raw Web of Science data & check for duplicates
# Author: Valerie Hase
# Date: 2024-04-05
#
########################

#### Step 1.1: Load WoS data from Excel sheet & do some cleaning ####
wos.abstracts <- read_excel("raw_data_wos/WoS_savedrecs_1-1000.xls") %>%
  rbind(read_excel("raw_data_wos/WoS_savedrecs_1001-2000.xls")) %>%
  rbind(read_excel("raw_data_wos/WoS_savedrecs_2001-2765.xls"))

# Inspect data
#glimpse(wos_abstracts)

table(wos.abstracts$Language)

# Clean data (e.g., rename variable, filter relevant variables, filter relevant cases)
wos.abstracts <- wos.abstracts %>%
  
  #reduce to relevant cases
  filter(Language == "English") %>%
  
  #rename variable in dplyr style
  rename(id = `UT (Unique WOS ID)`,
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
  
  #reduce to relevant variables
  select(id, authors, title, abstract, 
         source.title, source.type, source.conference,
         keywords, keywords.plus,
         author.addresses, author.affiliations,
         year, volume, issue, doi) 

#for articles with 2024 as year: were they originally published in 2023?
check <- wos.abstracts %>%
  filter(year == 2024) %>%
  select(title, id, doi) %>%
  mutate(link = paste0("https://doi.org/", doi))

#manually check their website
#browseURL(check$link[48])

#set years to online first year, set volume and issue to NA
wos.abstracts <- wos.abstracts %>%
  
  #all years to 2023
  mutate(year = replace(year,
                        id %in% check$id,
                        2023),
         
         #all volumes/issues to NA
         volume = replace(volume,
                          id %in% check$id,
                          NA),
         issue = replace(issue,
                          id %in% check$id,
                          NA),
         
         #expect for those already published in 2021
         year = replace(year,
                        id %in% c("WOS:000730401500001", "WOS:000621157300001", "WOS:000721544900001",
                                  "WOS:000715965000001"),
                        2021),
         #expect for those already publised in 2022
         year = replace(year,
                        id %in% c("WOS:000882767000001", "WOS:000795946700001", "WOS:000797400200001",
                                  "WOS:000773468100001", "WOS:000781753400001"),
                        2022))

rm(check)

#### Step 1.2: Check for duplicates ####

#by ID: looks good
length(unique(wos.abstracts$id))

#by DOI
wos.abstracts %>%
  filter(!is.na(doi)) %>%
  count(doi) %>%
  filter(n>1)

#we drop 1 article included both as early access and as regular article based on same doi
wos.abstracts <- wos.abstracts %>%
    filter(id != "WOS:000953595300001")

#by title similarity
titles <- wos.abstracts %>%
  mutate(title = tolower(title)) %>% 
  select(title, id) %>%
  unnest_tokens(word, title) %>%
  count(id, word) %>%
  ungroup() 

similarity <- titles %>%
                  pairwise_similarity(id, word, n) %>%
                  arrange(desc(similarity)) %>% 
                  filter(similarity >= .8)

duplicates <- wos.abstracts %>%
  filter(id %in% similarity$item1 | id %in% similarity$item2)

#we drop 3 more articles included both as early access and as regular article based on same title
wos.abstracts <- wos.abstracts %>%
  filter(id != "WOS:000209845700001" & id != "WOS:000209845600001" & id != "WOS:000777856400001")

#clean house
rm(duplicates, similarity, titles)

# save number of prisma flow
n_deduplicated <- nrow(wos.abstracts)