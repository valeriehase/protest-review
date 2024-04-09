########################
#
# Sort into CSS/non-CSS sample & abstract screening
# Author: Valerie Hase
# Date: 2024-04-09
#
########################

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


####  Intercodertest 1 Sample: N = 20 articles, stratified by CSS vs. non-CSS #### 

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

####  Intercodertest 2 Sample: N = 20 articles, stratified by CSS vs. non-CSS #### 

#sample.abstracts.2 <- css.sample %>%
#  
#  #filter by articles from intercoder test 1
#  filter(id_unique %notin% (read.csv2("codings/abstract_screening_ah_mm_vh/intercoder.1.vh.csv") %>%
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