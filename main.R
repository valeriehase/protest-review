########################
#
# Main Script for Lit Review Protest
# Author: Valerie Hase
# Date: 2023-07-28
#
########################

# Prepare workspace -------------------------------------------------------

library(here)
library(readr)
library(readxl)
library(tidyverse)
library(tidytext)
library(magrittr)
`%notin%` <- Negate(`%in%`)

# Step 1: Load & deduplicate data ---------------------------------------------------------------
# Loads data by Annett
# Check data against data by Valerie
# Identifies duplicates
#source("01_duplicate_check.R") #only run by Daniel/Annett

# Step 2: Draw sample for intercoder test(s) ---------------------------------------------------------------
# Draws stratified sample for intercoder test 1

source("02_intercoder.R")

