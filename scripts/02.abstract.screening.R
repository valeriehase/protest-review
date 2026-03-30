#
# Sort into CSS/non-CSS sample & abstract screening
# Date: 2026-03-30
#
# Setup ------------------------------------------------------------------------

library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(ggpubr)
library(tidycomm)
library(caret)

if (!exists("PATHS")) source(here::here("helper functions/paths.R"))
if (!exists("IN")) source(here::here("helper functions/config.R"))
if (!exists("require_file")) source(here::here("helper functions/helpers.R"))

`%!in%` <- Negate(`%in%`)

# Input data -------------------------------------------------------------------

if (!exists("wos_abstracts", inherits = TRUE)) {
  
  # Directory
  dir_path <- PATHS$int
  
  # List matching files
  files <- list.files(
    dir_path,
    pattern = "^01_wos_abstracts_clean(_\\d{8}_\\d{4})?\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No matching WoS abstracts files found, please run script 01.load.wos.data first.")
  }
  
  # Extract timestamps (if present)
  timestamps <- sub(
    ".*_(\\d{8}_\\d{4})\\.rds$",
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
  
  message("Loading cleaned WoS abstracts from: ", timestamps[which.max(timestamps)])
  
  wos_abstracts <- readRDS(latest_file)
  
  #clean house
  rm(timestamps, dir_path, files, latest_file)
}

# 2.1 Sort sample into CSS/non-CSS samples -------------------------------------

#inspect further keywords; based on all keywords
keywords <- wos_abstracts %>%
  dplyr::mutate(keywords.css = paste0(keywords, ";", keywords.plus)) %>%
  dplyr::pull(keywords.css) %>%
  strsplit(., split = ";") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  table() %>%
  tibble::as_tibble()

#inspect further keywords; based on CSS keywords in abstract
keywords <- wos_abstracts %>%
  dplyr::filter(grepl("computational|CSS|big data|automated", abstract)) %>%
  dplyr::mutate(keywords.css = paste0(keywords, ";", keywords.plus)) %>%
  dplyr::pull(keywords.css) %>%
  strsplit(., split = ";") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  table() %>%
  tibble::as_tibble()

rm(keywords)

#create CSS search terms
search.terms.CSS <- "agent-based|agent based|algorith|API|argument mining|aspect-based mining|
aspect based mining|automated|bag-of-word|bag of word|big data|community detec|computational|computer-assisted|
computer assisted|computer vision|corpus-linguistic|corpus linguistic|crawl|CSS|data donation|data mining|deep learn|
digital method|digital trac|embedding|emotion detect|eye-track|eye track|hyperlink|image recogn|large language model|
llm|log data|machine learning|metered data|natural language processing|NLP|network analy|neural net|
opinion mining|part of speech|part-of-speech|scraping|sensor data|sentiment analy|simula|text-as-data|
text as data|text mining|topic model|track|transformer"

#create CSS sample
css.sample <- wos_abstracts %>%
  
  #text including title, abstracts, keywords
  dplyr::mutate(search.text = paste0(title, " ", abstract, " ", keywords),
         search.text = tolower(search.text)) %>%
  
  #filter for CSS keywords
  dplyr::filter(grepl(search.terms.CSS, search.text))

#create non-CSS sample
non.css.sample <- wos_abstracts %>%
  
  #text including title, abstracts, keywords
  dplyr::mutate(search.text = paste0(title, " ", abstract, " ", keywords),
         search.text = tolower(search.text)) %>%
  
  #filter for CSS keywords
  dplyr::filter(!grepl(search.terms.CSS, search.text))

# 2.2 Intercodertest 1 Sample --------------------------------------------------
# N = 20 articles, stratified by CSS vs. non-CSS (OLD ID)

#sample.abstracts.1 <- css.sample %>%
#  
#  #get 10 articles from CSS sample
#  slice_sample(n = 10) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#          slice_sample(n = 10)) %>%
#  
#  #create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA) %>%
#  
#  #randomly sort rows
#  slice_sample(n = 20)

# 2.3 Intercodertest 2 Sample --------------------------------------------------
# N = 20 articles, stratified by CSS vs. non-CSS (OLD ID)

#sample.abstracts.2 <- css.sample %>%

#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                            pull(id_unique))) %>%
#
#  #get 10 articles from CSS sample
#  slice_sample(n = 10) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#          
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          slice_sample(n = 10)) %>%
#  
#  #create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA) %>%
#  
#  #randomly sort rows
#  slice_sample(n = 20)

# 2.4 Intercodertest 3 Sample --------------------------------------------------
# N = 30 articles, stratified by CSS vs. non-CSS (OLD ID)

#sample.abstracts.3 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                            pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 2
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.2.csv")) %>%
#                            pull(id_unique))) %>%
#
#  #get 15 articles from CSS sample
#  slice_sample(n = 15) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#          
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.2.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          slice_sample(n = 15)) %>%
#  
#  #create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA) %>%
#  
#  #randomly sort rows
#  slice_sample(n = 30)

# 2.5 Intercodertest 4 Sample --------------------------------------------------
# N = 30 articles, stratified by CSS vs. non-CSS (OLD ID)

#sample.abstracts.4 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                            pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 2
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.2.csv")) %>%
#                            pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 3
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.3.csv")) %>%
#                            pull(id_unique))) %>%
#
#  #get 15 articles from CSS sample
#  slice_sample(n = 15) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#                     
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.2.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 3
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.3.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          slice_sample(n = 15)) %>%
#  
#  #create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  
#  select(id_unique, authors, title, abstract, keywords, link) %>%
  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA) %>%
#  
#  #randomly sort rows
#  slice_sample(n = 30)

# 2.6 Intercodertest 5 Sample --------------------------------------------------
# N = 30 articles, stratified by CSS vs. non-CSS 

#sample.abstracts.5 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                            pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 2
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.2.csv")) %>%
#                            pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 3
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.3.csv")) %>%
#                            pull(id_unique))) %>%
#
#  #filter by articles from intercoder test 4
#  filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.4.csv")) %>%
#                            pull(id_unique))) %>%
#
#  #get 15 articles from CSS sample
#  slice_sample(n = 15) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#                     
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.1.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.2.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 3
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.3.csv")) %>%
#                                      pull(id_unique))) %>%
#
#          #filter by articles from intercoder test 4
#          filter(id_unique %!in% (read.csv2(file.path(PATHS$IN, "intercoder.4.csv")) %>%
#                                      pull(id_unique))) %>%
#          
#          slice_sample(n = 15)) %>%
#  
#  #create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA) %>%
#  
#  #randomly sort rows
#  slice_sample(n = 30)

# 2.7 Test reliability with latest coding after finalizing codebook (reli codings 4 and 5) ----------------------------

intercoder_cases <- read.csv2(file.path(PATHS$IN, "comparison.4.csv")) %>%
  
  #reduce to necessary variables
  select(id_unique, starts_with("protest"), 
         starts_with("method"), starts_with("type")) %>%
  
  #reduce to non-empty cases
  filter(!is.na(id_unique) & id_unique != "") %>%
  
  #type to binary, as we could only use this "broader" coding reliably
  mutate(empirical_coder1 = 0,
         empirical_coder1 = replace(empirical_coder1,
                                type_coder1 == 1,
                                1),
         empirical_coder2 = 0,
         empirical_coder2 = replace(empirical_coder2,
                                type_coder2 == 1,
                                1),
         empirical_coder3 = 0,
         empirical_coder3 = replace(empirical_coder3,
                                type_coder3 == 1,
                                1)) %>%
  
  #create overarching inclusion criterion
  mutate(inclusion_coder1 = 0,
         inclusion_coder1 = replace(inclusion_coder1,
                                protest_coder1 == 1 & type_coder1 == 1,
                                1),
         inclusion_coder2 = 0,
         inclusion_coder2 = replace(inclusion_coder2,
                                protest_coder2 == 1 & type_coder2 == 1,
                                1),
         inclusion_coder3 = 0,
         inclusion_coder3 = replace(inclusion_coder3,
                                protest_coder3 == 1 & type_coder3 == 1,
                                1)) %>%
  
  #bind coding 4
  rbind(read.csv2(file.path(PATHS$IN, "comparison.5.csv")) %>%
          
          #reduce to necessary variables
          select(id_unique, starts_with("protest"), 
                 starts_with("method"), starts_with("type")) %>%
          
          #reduce to non-empty cases
          filter(!is.na(id_unique) & id_unique != "") %>%
          
          #type to binary, as we could only use this "broader" coding reliably
          mutate(empirical_coder1 = 0,
                 empirical_coder1 = replace(empirical_coder1,
                                            type_coder1 == 1,
                                            1),
                 empirical_coder2 = 0,
                 empirical_coder2 = replace(empirical_coder2,
                                            type_coder2 == 1,
                                            1),
                 empirical_coder3 = 0,
                 empirical_coder3 = replace(empirical_coder3,
                                            type_coder3 == 1,
                                            1)) %>%
          
          #create overarching inclusion criterion
          mutate(inclusion_coder1 = 0,
                 inclusion_coder1 = replace(inclusion_coder1,
                                            protest_coder1 == 1 & type_coder1 == 1,
                                            1),
                 inclusion_coder2 = 0,
                 inclusion_coder2 = replace(inclusion_coder2,
                                            protest_coder2 == 1 & type_coder2 == 1,
                                            1),
                 inclusion_coder3 = 0,
                 inclusion_coder3 = replace(inclusion_coder3,
                                            protest_coder3 == 1 & type_coder3 == 1,
                                            1))) %>%
  
  mutate(across(everything(), as.character)) %>%
  
  #from wide to long
  pivot_longer(!id_unique, names_to = "variable", values_to = "value") %>%
  mutate(coder = gsub("protest_|method_|type_|empirical_|inclusion_", "", variable),
         variable = gsub("_coder1|_coder2|_coder3", "", variable)) %>%
  
  #to wide again
  pivot_wider(names_from = variable, values_from = c(value))

#save relevant intercoder values
intercoder <- intercoder_cases %>% 
  test_icr(id_unique, coder, protest, method, type, empirical, inclusion) %>%
  filter(Variable != "empirical" & Variable != "inclusion")

# 2.8 Validate CSS keywords ----------------------------------------------------

#read in manual coding based on N = 60 cases from intercoder 4/5
manual_coding <- read.csv2(file.path(PATHS$IN, "validation.1.csv")) %>%
  rbind(read.csv2(file.path(PATHS$IN, "validation.2.csv")) %>%
          
          #reduce to relevant variables
          select(id_unique:type)) %>%
  select(id_unique, method) %>%
  rename(coding_manual = method)

#read in automated coding based on keyword search
automated_coding <- css.sample %>%
  
  #create automated CSS coding
  mutate(method = 1) %>%
  
  #bind non-css sample
  rbind(non.css.sample %>%
          mutate(method = 0)) %>%
  
  #reduce to necessary variables & cases
  select(id_unique, method) %>%
  filter(id_unique %in% manual_coding$id_unique) %>%
  rename(coding_automated = method)

#create validation set
validation <- full_join(automated_coding, manual_coding) %>%
  mutate(coding_automated = as.factor(coding_automated),
         coding_manual = as.factor(coding_manual))

#get result of validation
validation <- confusionMatrix(data = validation$coding_automated, 
                              reference = validation$coding_manual, 
                              mode = "prec_recall", 
                              positive = "1")

# 2.9 Draw samples for coding --------------------------------------------------
#random <- non.css.sample %>%
#  slice_sample(n = nrow(css.sample)) 

#create sample for coding
#coding_abstracts <- css.sample %>%
#  mutate(sample = "CSS") %>%
#  full_join(random %>%
#              mutate(sample = "non-CSS")) %>%
#  
#  #create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  #sort randomly
#  slice_sample(n = nrow(random) + nrow(css.sample)) %>%
#  
#  #assign coder
#  mutate(coder = NA,
#         coder = replace(coder,
#                         id_unique %in% intercoder_cases$id_unique,
#                         "already coded (reliability)"),
#         coder = replace(coder,
#                         is.na(coder),
#                         rep(c("coder1", "coder2", "coder3"), 226)))


# 2.10 Read in & clean abstract coding ----------------------------------

#read in files by three coders plus additional coding for similar css/non-css size, reduce to relevant variables and adapt data type
coding_abstracts <- read.csv2(file.path(PATHS$IN, "abstract_screening_coder1.csv")) %>%
  select(id_unique:type) %>%
  mutate(across(protest:type, as.numeric)) %>%
  rbind(read.csv2(file.path(PATHS$IN, "abstract_screening_coder2.csv")) %>%
          select(id_unique:type) %>%
          mutate(across(protest:type, as.numeric))) %>%
  rbind(read.csv2(file.path(PATHS$IN, "abstract_screening_coder3.csv")) %>%
          select(id_unique:type) %>%
          mutate(across(protest:type, as.numeric))) %>%
  rbind(read.csv2(file.path(PATHS$IN, "abstract_screening_coder2_added.csv")) %>%
          select(id_unique:type) %>%
          mutate(across(protest:type, as.numeric)))  %>%
  
  #remove empty rows
  filter(!is.na(id_unique) & id_unique != "" & !is.na(coder)) %>%
  
  #remove inaccessible studies (paper not available)
  mutate(protest = replace(protest,
                           id_unique %in% c("ID2626", "ID1295", "ID1854"),
                           NA),
         method = replace(method,
                          id_unique %in% c("ID2626", "ID1295", "ID1854"),
                          NA),
         type = replace(type,
                        id_unique %in% c("ID2626", "ID1295", "ID1854"),
                        NA)) %>%
  
  #correct coding error for method variable
  mutate(method = replace(method,
                          method == 2,
                          1)) %>%
  
  #correct coding error for type variable
  mutate(type = replace(type,
                        type == 0,
                        2)) %>%
  
  #create overarching inclusion criterion
  mutate(inclusion = 0,
         inclusion = replace(inclusion,
                             protest == 1 & type == 1,
                             1))

#identify relevant sample
coding_abstracts_relevant <- coding_abstracts %>%
  
  #identify relevance
  filter(inclusion == 1) %>%
  
  #remove duplicates: coded by different coders twice (always with exactly the same codes)
  filter(!(id_unique %in% c("ID1109", "ID1171", "ID1228", "ID1278", "ID1314",
                            "ID1449", "ID173", "ID1863", "ID19", "ID2217", "ID1139",
                            "ID2368", "ID41", "ID641", "ID873") & coder == 2)) %>%
  filter(!(id_unique %in% c("ID1658") & coder == 3)) %>%
  
  #remove duplicates: coded by the same coder twice
  group_by(id_unique, coder) %>%
  filter(
    !(id_unique == "ID1810" & coder == 2) | row_number() == 1
  ) %>%
  ungroup() %>%
  
  group_by(id_unique, coder) %>%
  filter(
    !(id_unique == "ID433" & coder == 2) | row_number() == 1
  ) %>%
  ungroup() %>%
  
  group_by(id_unique, coder) %>%
  filter(
    !(id_unique == "ID404" & coder == 3) | row_number() == 1
  ) %>%
  ungroup() %>%
  
  #add other meta data
  left_join(wos_abstracts %>%
              select(id_unique, source.title, source.type, source.conference,
                     author.addresses, author.affiliations, year))

# 2.11 Export -----------------------------------------------------------------------

out_dir <- PATHS$int
stamp   <- format(Sys.time(), "%Y%m%d_%H%M")
out_file <- file.path(out_dir, paste0("02_abstract_screening_clean_", stamp, ".rds"))

#save all relevant output as list object
saveRDS(
  list(
    intercoder = intercoder,
    validation  = validation,
    coding_abstracts = coding_abstracts, 
    coding_abstracts_relevant = coding_abstracts_relevant
  ),
  out_file
)

message("02 completed.")
message("- Results of abstract coding saved to: ", out_file)