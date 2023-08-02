########################
#
# Intercoder samples & tests for NW literature review
# Author: Valerie Hase
# Date: 2023-07-28
#
########################

# Load & deduplicate data ---------------------------------------------------------------

#Remove duplicate articles

sample_prelim <- read_xlsx(here("data", paste0("savedrecs_SSCI_dup_flagged_DT20230725.xlsx"))) %>%
  filter(UT..Unique.WOS.ID. %notin% c("WOS:000953595300001", "WOS:000777856400001", 
                              "WOS:000921127300001", "WOS:000209845700001",
                              "WOS:000209845600001")) %>% #remove duplicates (prefer issued articles over early access, i.e., go by date)
  mutate(ID = paste0("ID", 1:nrow(.)))

# Add link from "Gegencheck" file ---------------------------------------------------------------
sample_prelim <- sample_prelim %>%
  select(-DOI.Link) %>% #delete because empty in original file
  right_join(read.csv2(here("data", paste0("Gegencheck.csv"))) %>%
               filter(UT..Unique.WOS.ID. %in% sample_prelim$UT..Unique.WOS.ID.) %>%
               select(UT..Unique.WOS.ID., DOI.Link))

# Intercodertest 1: 100 articles, stratified sample ---------------------------------------------------------------

intercoder_1 <- sample_prelim %>%
  group_by(Publication.Year) %>%
  slice_sample(n = 7) %>%
  select(c(ID, Author.Full.Names, Article.Title, Source.Title, Abstract,
           Publication.Year, Volume, Issue, 
           UT..Unique.WOS.ID., DOI, DOI.Link)) %>%
  sample_n(., 105)

#write.csv2(intercoder_1, "data/intercoder_1.csv", row.names = FALSE)


