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

# Intercodertest 1 Sample (Group 1 & 2): 105 articles, stratified sample ---------------------------------------------------------------

intercoder_1 <- sample_prelim %>%
  group_by(Publication.Year) %>%
  slice_sample(n = 7) %>%
  select(c(ID, Author.Full.Names, Article.Title, Source.Title, Abstract,
           Publication.Year, Volume, Issue, 
           UT..Unique.WOS.ID., DOI, DOI.Link)) %>%
  ungroup() %>%
  sample_n(., 105)

#write.csv2(intercoder_1, "data/intercoder_1.csv", row.names = FALSE)

# Intercodertest 2 Sample (Merja & Valerie, abstracts only: 105 articles, stratified sample, excluding articles from test 1---------------------------
intercoder_1 <- read.csv2("data/intercoder_1.csv")
intercoder_2 <- sample_prelim %>%
  filter(UT..Unique.WOS.ID. %notin% intercoder_1$UT..Unique.WOS.ID.) %>%
  group_by(Publication.Year) %>%
  slice_sample(n = 7) %>%
  select(c(ID, Author.Full.Names, Article.Title, Source.Title, Abstract,
           Publication.Year, Volume, Issue, 
           UT..Unique.WOS.ID., DOI, DOI.Link)) %>%
  ungroup() %>%
  sample_n(., 105) %>%
  
  #add Link to DOI where possible
  mutate(DOI.Link = replace(DOI.Link,
                            DOI.Link == 0,
                            paste0("http://dx.doi.org/", 
                                   intercoder_2 %>%
                                     filter(DOI.Link == 0) %>%
                                     pull(DOI))))

#write.csv2(intercoder_2, "data/intercoder_2.csv", row.names = FALSE)

# Intercodertest 2 Reli-Values
inclusion_1 <- read_xlsx(here("data", paste0("Inclusion_MerjaValerie_IR1.xlsx"))) %>%
  
  #only include those where we both coded 
  filter(!is.na(CSS_VH)) %>%
  
  #change CSS codes to pure 0/1 coding
  mutate(CSS_MM = substr(CSS_MM, 1, 1),
         CSS_VH = substr(CSS_VH, 1, 1),
         CSS_MM = replace(CSS_MM,
                          CSS_MM == 2,
                          1),
         CSS_VH = replace(CSS_VH,
                          CSS_VH == 2,
                          1)) %>%
  
  #to long format for reliability
  select(-c(Coder)) %>%
  pivot_longer(!ID, names_to = "coder") %>%
  mutate(coder = substr(coder, 5,6))

#Check intercoder reliability
reli_inclusion_1 <- inclusion_1 %>% 
  test_icr(unit_var = ID,
           coder_var = coder) %>%
  mutate(Krippendorffs_Alpha = round(Krippendorffs_Alpha, 2))
