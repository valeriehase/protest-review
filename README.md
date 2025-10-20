# Protest-Review

This repository contains code for the protest-review within the DFG Network on Protest & CSS Methods.  
Simply run *main.R* to execute all subscripts.  
Use the folder *data/* as your working directory to directly execute all analyses with the corresponding data.

## Scripts

**01.load.WoS.data**  
Code to load raw abstract data retrieved via search strings from *Web of Science* (2009–2023) and perform basic data cleaning.

**02.abstract.screening**  
Code to identify CSS and non-CSS research using search strings, intercoder tests, and abstract-level coding.

**03.reliability.testing**  
Code to generate coding masks for reliability testing of full-paper coding and to compute intercoder reliability metrics.

**04.final.coding.masks**  
Code to generate the final coding masks for full-paper coding in the main sample.

**05.data.cleaning**  
Code to clean the coded full-paper dataset and document the cleaning process (log).

**06.analysis**  
Code to analyze the cleaned data and produce APA-style summary tables and figures.
