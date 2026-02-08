#
# Main Script for Lit Review Protest
# Author: Miriam Milzner, Valerie Hase
# Date: 2026-01-10 (updated 2026-02-09)
#

library("here")
library("readxl")
library("tidyverse")
library("tidytext")
library("widyr")
library("magrittr")
library("tidycomm")
library("ggpubr")
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

#load("working_spaces/01.load.data.RDATA")
#source("02.abstract.screening.R")
#save.image("working_spaces/02.abstract.screening.data.RDATA")

##### 2.1 Details Method Section / Appendix A2.1 #####

#initial amount of identified studies
n_deduplicated

#reliability for inclusion/exclusion
intercoder

#validation of search string
validation

#initial sample of CSS vs. non-CSS studies
nrow(css.sample)
nrow(non.css.sample)

#coded articles until we had identified equally large CSS and non-CSS samples
rbind(coding_abstracts, coding_abstracts_2, coding_abstracts_3) %>%
  filter(!is.na(protest) & !is.na(method) & !is.na(type)) %>%
  nrow()

#CSS vs. non-CSS sample - relevant studies
sample_relevant %>%
  count(method)

#### Step 3: Full-paper coding ####

# we will integrate all following scripts here after the ICA deadline
# to be able to run them from main