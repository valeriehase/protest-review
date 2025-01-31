########################
#
# Sort into CSS/non-CSS sample & abstract screening
# Author: Valerie Hase
# Date: 2024-10-03
#
########################

`%!in%` <- Negate(`%in%`)

#### Step 2.1: Sort sample into CSS/non-CSS samples #### 

#inspect further keywords; based on all keywords
keywords <- wos.abstracts %>%
  mutate(keywords.css = paste0(keywords, ";", keywords.plus)) %>%
  pull(keywords.css) %>%
  strsplit(., split = ";") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  table() %>%
  as_tibble()

#inspect further keywords; based on CSS keywords in abstract
keywords <- wos.abstracts %>%
  filter(grepl("computational|CSS|big data|automated", abstract)) %>%
  mutate(keywords.css = paste0(keywords, ";", keywords.plus)) %>%
  pull(keywords.css) %>%
  strsplit(., split = ";") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  table() %>%
  as_tibble()

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
css.sample <- wos.abstracts %>%
  
  #text including title, abstracts, keywords
  mutate(search.text = paste0(title, " ", abstract, " ", keywords),
         search.text = tolower(search.text)) %>%
  
  #filter for CSS keywords
  filter(grepl(search.terms.CSS, search.text))

#create non-CSS sample
non.css.sample <- wos.abstracts %>%
  
  #text including title, abstracts, keywords
  mutate(search.text = paste0(title, " ", abstract, " ", keywords),
         search.text = tolower(search.text)) %>%
  
  #filter for CSS keywords
  filter(!grepl(search.terms.CSS, search.text))

#check out distribution of samples over time
ggarrange(css.sample %>%
            count(year) %>%
            ggplot(aes(x = year, y = n)) + geom_line() +
            ggtitle(paste0("CSS Sample (N = ", nrow(css.sample), ")")) + theme_bw(),
          non.css.sample %>%
            count(year) %>%
            ggplot(aes(x = year, y = n)) + geom_line() +
            ggtitle(paste0("Non CSS Sample (N = ", nrow(non.css.sample), ")")) + theme_bw())

####  Step 2.2 Intercodertest 1 Sample: N = 20 articles, stratified by CSS vs. non-CSS (OLD ID)#### 

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

#write out for manual coding
#write.csv2(sample.abstracts.1, "codings/abstract_screening_ah_mm_vh/intercoder.1.ah.csv", row.names = FALSE)
#write.csv2(sample.abstracts.1, "codings/abstract_screening_ah_mm_vh/intercoder.1.mm.csv", row.names = FALSE)
#write.csv2(sample.abstracts.1, "codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv", row.names = FALSE)

####  Step 2.3  Intercodertest 2 Sample: N = 20 articles, stratified by CSS vs. non-CSS (OLD ID) #### 

#sample.abstracts.2 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                              pull(id_unique))) %>%
#
#  #get 10 articles from CSS sample
#  slice_sample(n = 10) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#          
#          #filter by articles from intercoder test 1
#          filter(id_unique %notin% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
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

#write out for manual coding
#write.csv2(sample.abstracts.2, "codings/abstract_screening_ah_mm_vh/intercoder.2.ah.csv", row.names = FALSE)
#write.csv2(sample.abstracts.2, "codings/abstract_screening_ah_mm_vh/intercoder.2.mm.csv", row.names = FALSE)
#write.csv2(sample.abstracts.2, "codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv", row.names = FALSE)

####  Step 2.4 Intercodertest 3 Sample: N = 30 articles, stratified by CSS vs. non-CSS (OLD ID) #### 

#sample.abstracts.3 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                              pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 2
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv") %>%
#                              pull(id_unique))) %>%
#
#  #get 15 articles from CSS sample
#  slice_sample(n = 15) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#          
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv") %>%
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

#write out for manual coding
#write.csv2(sample.abstracts.3, "codings/abstract_screening_ah_mm_vh/intercoder.3.ah.csv", row.names = FALSE)
#write.csv2(sample.abstracts.3, "codings/abstract_screening_ah_mm_vh/intercoder.3.mm.csv", row.names = FALSE)
#write.csv2(sample.abstracts.3, "codings/abstract_screening_ah_mm_vh/intercoder.3.vh.csv", row.names = FALSE)

####  Step 2.5  Intercodertest 4 Sample: N = 30 articles, stratified by CSS vs. non-CSS (OLD ID) #### 

#sample.abstracts.4 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                              pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 2
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv") %>%
#                              pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 3
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.3.vh.csv") %>%
#                            pull(id_unique))) %>%
#
#  #get 15 articles from CSS sample
#  slice_sample(n = 15) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#                     
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv") %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.3.vh.csv") %>%
#                                    pull(id_unique))) %>%
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

#write out for manual coding
#write.csv2(sample.abstracts.4, "codings/abstract_screening_ah_mm_vh/intercoder.4.ah.csv", row.names = FALSE)
#write.csv2(sample.abstracts.4, "codings/abstract_screening_ah_mm_vh/intercoder.4.mm.csv", row.names = FALSE)
#write.csv2(sample.abstracts.4, "codings/abstract_screening_ah_mm_vh/intercoder.4.vh.csv", row.names = FALSE)

####  Step 2.6  Intercodertest 5 Sample: N = 30 articles, stratified by CSS vs. non-CSS #### 

#sample.abstracts.5 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                              pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 2
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv") %>%
#                              pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 3
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.3.vh.csv") %>%
#                            pull(id_unique))) %>%
#  
#  #filter by articles from intercoder test 4
#  filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.4.vh.csv") %>%
#                            pull(id_unique))) %>%
#
#  #get 15 articles from CSS sample
#  slice_sample(n = 15) %>%
#  
#  #add non-CSS articles
#  rbind(non.css.sample %>%
#                     
#          #filter by articles from intercoder test 1
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 2
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.2.vh.csv") %>%
#                                      pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 3
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.3.vh.csv") %>%
#                                    pull(id_unique))) %>%
#          
#          #filter by articles from intercoder test 4
#          filter(id_unique %!in% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.4.vh.csv") %>%
#                                    pull(id_unique))) %>%
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

#write out for manual coding
#write.csv2(sample.abstracts.5, "codings/abstract_screening_ah_mm_vh/intercoder.5.ah.csv", row.names = FALSE)
#write.csv2(sample.abstracts.5, "codings/abstract_screening_ah_mm_vh/intercoder.5.mm.csv", row.names = FALSE)
#write.csv2(sample.abstracts.5, "codings/abstract_screening_ah_mm_vh/intercoder.5.vh.csv", row.names = FALSE)

####  Step 2.7 Test reliability with latest coding (4 and 5) #### 
intercoder_cases <- read_xlsx("codings/abstract_screening_ah_mm_vh/vergleich5.xlsx") %>%
  
  #reduce to necessary variables
  select(id_unique, starts_with("protest"), 
         starts_with("method"), starts_with("type")) %>%
  
  #type to binary
  mutate(empirical_ah = 0,
         empirical_ah = replace(empirical_ah,
                                type_ah == 1,
                                1),
         empirical_mm = 0,
         empirical_mm = replace(empirical_mm,
                                type_mm == 1,
                                1),
         empirical_vh = 0,
         empirical_vh = replace(empirical_vh,
                                type_vh == 1,
                                1)) %>%
  
  #create overarching inclusion criterion
  mutate(inclusion_ah = 0,
         inclusion_ah = replace(inclusion_ah,
                                protest_ah == 1 & type_ah == 1,
                                1),
         inclusion_mm = 0,
         inclusion_mm = replace(inclusion_mm,
                                protest_mm == 1 & type_mm == 1,
                                1),
         inclusion_vh = 0,
         inclusion_vh = replace(inclusion_vh,
                                protest_vh == 1 & type_vh == 1,
                                1)) %>%
  
  #bind coding 4
  rbind(read_xlsx("codings/abstract_screening_ah_mm_vh/vergleich4.xlsx") %>%
          
          #reduce to necessary variables
          select(id_unique, starts_with("protest"), 
                 starts_with("method"), starts_with("type")) %>%
          
          #type to binary
          mutate(empirical_ah = 0,
                 empirical_ah = replace(empirical_ah,
                                        type_ah == 1,
                                        1),
                 empirical_mm = 0,
                 empirical_mm = replace(empirical_mm,
                                        type_mm == 1,
                                        1),
                 empirical_vh = 0,
                 empirical_vh = replace(empirical_vh,
                                        type_vh == 1,
                                        1)) %>%
          
          #create overarching inclusion criterion
          mutate(inclusion_ah = 0,
                 inclusion_ah = replace(inclusion_ah,
                                        protest_ah == 1 & type_ah == 1,
                                        1),
                 inclusion_mm = 0,
                 inclusion_mm = replace(inclusion_mm,
                                        protest_mm == 1 & type_mm == 1,
                                        1),
                 inclusion_vh = 0,
                 inclusion_vh = replace(inclusion_vh,
                                        protest_vh == 1 & type_vh == 1,
                                        1))) %>%
  
  mutate(across(everything(), as.character)) %>%
  
  #from wide to long
  pivot_longer(!id_unique, names_to = "variable", values_to = "value") %>%
  mutate(coder = gsub("protest_|method_|type_|empirical_|inclusion_", "", variable),
         variable = gsub("_ah|_mm|_vh", "", variable)) %>%
  
  #to wide again
  pivot_wider(names_from = variable, values_from = c(value))

#save relevant intercoder values
intercoder <- intercoder_cases %>% 
  test_icr(id_unique, coder, protest, method, type, empirical, inclusion) %>%
  filter(Variable != "empirical" & Variable != "inclusion")

####  Step 2.7 Validate CSS keywords #### 

#read in manual coding based on N = 60 cases from intercoder 4/5
manual_coding <- read_xlsx("codings/validation_CSS_sample/intercoder.4.vh.xlsx") %>%
  rbind(read_xlsx("codings/validation_CSS_sample/intercoder.5.vh.xlsx")) %>%
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

#clean house
rm(automated_coding, manual_coding)

####  Step 2.8 Draw samples for coding #### 
#random <- non.css.sample %>%
#  slice_sample(n = nrow(css.sample)) 

#check out distribution of samples over time
#ggarrange(css.sample %>%
#            count(year) %>%
#            ggplot(aes(x = year, y = n)) + geom_line() +
#            ggtitle(paste0("CSS Sample (N = ", nrow(css.sample), ")")) + theme_bw(),
#          random %>%
#            count(year) %>%
#            ggplot(aes(x = year, y = n)) + geom_line() +
#            ggtitle(paste0("Non CSS Sample (N = ", nrow(non.css.sample), ")")) + theme_bw())

#check spread over years: randomly enough
#css.sample %>%
#  mutate(sample = "CSS") %>%
#  full_join(random %>%
#              mutate(sample = "non-CSS")) %>%
#  group_by(sample) %>%
#  count(year) %>%
#  ungroup %>%

#  #to wide again
#  pivot_wider(names_from = c(sample), values_from = c(n))

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
#                         id_unique %in% intercoder$id_unique,
#                         "already coded (reliability)"),
#         coder = replace(coder,
#                         is.na(coder),
#                         rep(c("AH", "MM", "VH"), 226)))

#create codings sheets
# coding_abstracts_ah <- coding_abstracts %>%
#  filter(coder == "AH") %>%
#  
#  #select necessary variables
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA)

#coding_abstracts_mm <- coding_abstracts %>%
#  filter(coder == "MM") %>%
#  
#  #select necessary variables
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA)

#coding_abstracts_vh <- coding_abstracts %>%
#  filter(coder == "VH") %>%
#  
#  #select necessary variables
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #add empty coding variables
#  mutate(coder = NA,
#         protest = NA,
#         method = NA,
#         type = NA)

#write out for manual coding
#write.csv2(coding_abstracts_ah, "codings/abstract_screening_ah_mm_vh/coding.ah.csv", row.names = FALSE)
#write.csv2(coding_abstracts_mm, "codings/abstract_screening_ah_mm_vh/coding.mm.csv", row.names = FALSE)
#write.csv2(coding_abstracts_vh, "codings/abstract_screening_ah_mm_vh/coding.vh.csv", row.names = FALSE)

####  Step 2.9 Read in & clean initial abstract coding #### 

#read in files by three coders
coding_abstracts <- read_xlsx("codings/abstract_screening_ah_mm_vh/coding.ah.xlsx") %>%
  rbind(read_xlsx("codings/abstract_screening_ah_mm_vh/coding.mm.xlsx")) %>%
  rbind(read_xlsx("codings/abstract_screening_ah_mm_vh/coding.vh.xlsx"))

#clean
coding_abstracts <- coding_abstracts %>%
  
  #mark inaccessible studies (paper not available)
  mutate(protest = replace(protest,
                           id_unique %in% c("ID2626", "ID1295", "ID2376"),
                           NA),
         method = replace(method,
                          id_unique %in% c("ID2626", "ID1295", "ID2376"),
                          NA),
         type = replace(type,
                        id_unique %in% c("ID2626", "ID1295", "ID2376"),
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
  
  #add other meta data
  left_join(wos.abstracts %>%
              select(id_unique, source.title, source.type, source.conference,
                     author.addresses, author.affiliations, year))

####  Step 2.10 Add files for second coding round to get similar sample size for CSS and non-CSS sample (N = 207 each) #### 

#based on step 2.9 (initial coding of CSS sample, we have to add around N = 49 non-CSS pieces 
# as well as a missing CSS file to get equal sample sizes for both CSS and non-CSS sample

#coding_abstracts_added <- css.sample %>%
# 
#  #add missing CSS piece, likely coded as CSS, meaning that CSS sample: N = 207
#  filter(id_unique %!in% coding_abstracts$id_unique & id_unique %!in% intercoder_cases$id_unique) %>%
#  mutate(sample = "CSS") %>%
#  
#  #add around 250 non-CSS pieces (but only code until N = 207 "reached" so CSS size = non-CSS size)
#  #we oversample once so we do not have to do it again
#  full_join(non.css.sample %>%
#              filter(id_unique %!in% coding_abstracts) %>%
#              slice_sample(n = 250) %>%
#              mutate(sample = "non-CSS")) %>%
#  
#  # create link, replace with NA if doi missing
#  mutate(link = paste0("https://doi.org/", doi),
#         link = replace(link,
#                        is.na(doi),
#                        NA)) %>%
#  
#  #  select necessary variables
#  select(id_unique, authors, title, abstract, keywords, link) %>%
#  
#  #  add empty coding variables
#  mutate(coder = "MM",
#         protest = NA,
#         method = NA,
#         type = NA)

#write out for manual coding
#write.csv2(coding_abstracts_added, "codings/abstract_screening_ah_mm_vh/coding.mm.added.csv", row.names = FALSE)

coding_abstracts_2 <- read_xlsx("codings/abstract_screening_ah_mm_vh/coding.mm.added.xlsx") %>%
  
  #reduce to relevant variables & cases
  select(id_unique:type) %>%
  filter(coder != "NA") %>%
  
  #reformat
  mutate_at(c("coder", "protest", "method", "type"), as.numeric) %>%
  
  #create overarching inclusion criterion
  mutate(inclusion = 0,
         inclusion = replace(inclusion,
                             protest == 1 & type == 1,
                             1))

#identify relevant sample from additionally coded abstracts
coding_abstracts_2_relevant <- coding_abstracts_2 %>%
  
  #identify relevance
  filter(inclusion == 1) %>%
  
  #add other meta data
  left_join(wos.abstracts %>%
              select(id_unique, source.title, source.type, source.conference,
                     author.addresses, author.affiliations, year))

####  Step 2.11 Create final sample #### 
sample_relevant <- rbind(coding_abstracts_relevant,
                         coding_abstracts_2_relevant) %>%
  
  #remove by chance identified oversampled "0" (non-CSS observation)
  filter(id_unique != "ID2218")

#write out for manual coding
#write.csv2(sample_relevant, "codings/full_paper/full_paper_sample.csv", row.names = FALSE)

#identify inaccessible studies
n_inaccessible <- rbind(coding_abstracts, coding_abstracts_2) %>%
  filter(is.na(protest) & is.na(method) & is.na(type)) %>%
  nrow()