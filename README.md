## Protest-Review

This repository contains all code for the protest literature review within the DFG Network on Protest & CSS Methods.

To run the full pipeline, execute:

```r
source("main.R")
```

### Reproducibility

This project uses renv to ensure reproducibility. Restore the environment with:

```r
install.packages("renv")
renv::restore()
```
All paths are project-relative; no working directory needs to be set manually.

The pipeline is deterministic except for manual coding steps. 
Manual coding steps are required at several stages of the workflow.
These files must be placed in the specified directories before the pipeline can continue.

### Scripts

**01.load.WoS.data**  
Code to load raw abstract data retrieved via search strings from *Web of Science* (2009–2023) and perform basic data cleaning.

**02.abstract.screening**  
Code to identify CSS and non-CSS research using search strings, intercoder tests, and abstract-level coding.

**03.reliability.testing**  
03a generates coding masks for intercoder reliability testing of full-paper coding.  
03b reads the coded masks and computes intercoder reliability metrics.

**04.final.coding.masks**  
Code to generate the final coding masks for full-paper coding of the main sample.

**05.deduplication**
There were duplicates in the coded final sample (n=19). This file exports duplicates as a table for manual inspection.
It then applies keep/remove decision to coded final sample.

**06.data.cleaning**  
Code to clean the coded full-paper dataset and document the cleaning process (log).

**07.analysis**  
Code to analyze the cleaned data and produce APA-style summary tables and figures.

### Required Input Data

Place the following files in `data/in/` before running `main.R`.

| File | Description |
|------|------------|
| `wos_abstracts_2009_2023.xlsx` | Web of Science abstract export |
| `full_paper_sample.xlsx` | Full-paper sample |
| `full_paper_sample_coded.xlsx` | Coded full-paper sample (pre-deduplication) |
| `dupes_checked.xlsx` | Manually curated list of IDs to retain during deduplication |
| `df_sample_coded_reli.xlsx` | Reliability coded cases used to exclude papers before final coding |

### Output Data

All generated files are written to `data/out/`:

- `data/out/intermediate/` – intermediate processing steps (e.g., deduplication tables)
- `data/out/final/` – cleaned analysis dataset and final tables/figures
- `logs/` – data-cleaning and pipeline logs

### Important Notes

Some scripts intentionally do not overwrite existing files (e.g., reliability samples, coding masks).
If regeneration is required, delete the existing files first.

## Project Structure

```text
.
├── data/
│   ├── in/              # required input files
│   ├── out/             # output files
│   │   ├── intermediate/
│   │   └── final/
│   │   └── logs/
│
├── R/                   # helper functions and configuration
├── scripts/             # analysis pipeline
└── main.R               # entry point
```


