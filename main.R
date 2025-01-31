########################
#
# Main Script for Lit Review Protest
# Author: Valerie Hase
# Date: 2025-01-27
#
########################

library("here")
library("readxl")
library("tidyverse")
library("tidytext")
library("widyr")
library("magrittr")
library("tidycomm")
library("ggpubr")
library("PRISMAstatement")
library("caret")
`%!in%` <- Negate(`%in%`)

#### Step 1: Load & deduplicate raw Web of Science data ####
# Loads data by Annett (Web of Science, 2019-2023, downloaded on 03.04.2024)
# Does some initial data cleaning (removing duplicates and other irrelevant abstracts)

#source("01.load.wos.data.R")
#save.image("working_spaces/01.load.data.RDATA")

#### Step 2: Screening ####

#samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
#draws sample for intercoder tests & coding of abstracts

load("working_spaces/01.load.data.RDATA")
source("02.abstract.screening.R")

##### 2.1 report details for method/appendix section #####

#initial amount of identified studies
n_deduplicated

#validation of search string
validation

#reliability
intercoder

#initial sample of CSS vs. non-CSS studies
nrow(css.sample)
nrow(non.css.sample)

#coded articles until we had identified equally large CSS and non-CSS samples
rbind(coding_abstracts, coding_abstracts_2) %>%
  filter(!is.na(protest) & !is.na(method) & !is.na(type)) %>%
  nrow()

#inaccessible studies per sample

#CSS
rbind(coding_abstracts, coding_abstracts_2) %>%
  filter(is.na(protest) & is.na(method) & is.na(type)) %>%
  filter(id_unique %in% css.sample$id_unique)

#non-CSS
rbind(coding_abstracts, coding_abstracts_2) %>%
  filter(is.na(protest) & is.na(method) & is.na(type)) %>%
  filter(id_unique %in% non.css.sample$id_unique)

#final sample of of CSS vs. non-CSS studies
sample_relevant %>%
  group_by(method) %>%
  count(protest)

#distribution of CSS vs. non-CSS over time
ggarrange(sample_relevant %>%
            filter(method == 1) %>%
            count(year) %>%
            ggplot(aes(x = year, y = n)) + geom_line() +
            ggtitle(paste0("CSS Sample (N = ", nrow(css.sample), ")")) + theme_bw(),
          sample_relevant %>%
            filter(method == 0) %>%
            count(year) %>%
            ggplot(aes(x = year, y = n)) + geom_line() +
            ggtitle(paste0("Non CSS Sample (N = ", nrow(non.css.sample), ")")) + theme_bw())

sample_relevant %>%
  group_by(method) %>%
  count(year) %>%
  ungroup %>%
  mutate(method = replace(method,
                          method == 0,
                          "non-CSS"),
         method = replace(method,
                          method == 1,
                          "CSS")) %>%
  pivot_wider(names_from = c(method), values_from = c(n)) %>%
  arrange(as.numeric(year))

##### 2.2 flow chart #####

flow.chart <- flow_exclusions(
  incl_counts = c(nrow(wos.abstracts), 
                  nrow(coding_abstracts) + nrow(coding_abstracts_2), 
                  nrow(coding_abstracts) + nrow(coding_abstracts_2) - n_inaccessible,
                  nrow(sample_relevant)),
  total_label = "Deduplicated articles from WoS",
  incl_labels = c("By-method stratified sample", 
                  "Accessible full papers",
                  "Relevant full papers"),
  excl_labels = c("Removal due by-method\nstratified sampling", 
                  "Removal due to\ninaccessible full paper", 
                  "Removal due to\nirrelevance (manual coding)"),
)

#has to be saved manually (500, 300 as size for export)
flow.chart

#save.image("working_spaces/02.abstract.screening.RDATA")

#### Step 3: Full-paper coding ####