########################
#
# Main Script for Lit Review Protest
# Author: Valerie Hase
# Date: 2024-04-06
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
`%notin%` <- Negate(`%in%`)

#### Step 1: Load & deduplicate raw Web of Science data ####
# Loads data by Annett (Web of Science, 2019-2023, downloaded on 03.04.2024)
# Does some initial data cleaning (removing duplicates and other irrelevant abstracts)

#source("01.load.wos.data.R")
#save.image("working_spaces/01.load.data.RDATA")

#### Step 2: Abstract screening ####

#samples sample 1 (CSS) and sample 2 (non-CSS) based on search terms
#draws sample for intercoder tests & coding of abstracts

load("working_spaces/01.load.data.RDATA")
#source("02.abstract.screening.R")
#save.image("working_spaces/02.abstract.screening.RDATA")