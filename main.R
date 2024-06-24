########################
#
# Main Script for Lit Review Protest
# Author: Valerie Hase
# Date: 2024-06-24
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

#### Step 2: Abstract screening ####

#samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
#draws sample for intercoder tests & coding of abstracts

load("working_spaces/01.load.data.RDATA")
source("02.abstract.screening.R")

##### 2.1 report details for method/appendix section #####

#initial amount of identified studies
n_deduplicated

#validation of search string
validation

#initial sample of CSS vs. non-CSS studies
nrow(css.sample)
nrow(non.css.sample)

#coded sample of CSS vs. non-CSS studies for abstracts
nrow(coding_abstracts) #tbd

#reliability
intercoder

#number of relevant studies
coding_abstracts_relevant %>%
  filter(method == 0) %>%
  nrow()

#distribution of CSS vs. non-CSS over time
ggarrange(coding_abstracts_relevant %>%
            filter(method == 1) %>%
            count(year) %>%
            ggplot(aes(x = year, y = n)) + geom_line() +
            ggtitle(paste0("CSS Sample (N = ", nrow(css.sample), ")")) + theme_bw(),
          coding_abstracts_relevant %>%
            filter(method == 0) %>%
            count(year) %>%
            ggplot(aes(x = year, y = n)) + geom_line() +
            ggtitle(paste0("Non CSS Sample (N = ", nrow(non.css.sample), ")")) + theme_bw())

coding_abstracts_relevant %>%
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

##### 2.1 report details for method/appendix section #####

flow.chart <- flow_exclusions(
  incl_counts = c(nrow(wos.abstracts), 
                  nrow(coding_abstracts), 
                  nrow(coding_abstracts %>%
                         filter(!is.na(protest) & !is.na(method) & !is.na(type))),
                  nrow(coding_abstracts_relevant)),
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