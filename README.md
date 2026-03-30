## Protest-Review

This repository contains the complete analysis pipeline for the protest literature review conducted within the DFG Network on Protest & CSS Methods.

The workflow follows a linear, script-based pipeline with clearly defined inputs and outputs. To run the full pipeline, open the R project to execute the main script:

```r
source("main.R")
```

All analytical steps are reproducible given the required manually coded input files provided in `data/in/`. Please note that we cannot share the full data for all steps given terms of use by respective literature databases. 
As such, data can only be provided from step XX onwards. We recommending running the code in `main.R` from thereon (as indicated in the script).

## Project Structure

```text
.
├── data/
│   ├── in/              # manually provided input data (required)
│   ├── out/             # output files
│   │   ├── intermediate/
│   │   └── final/
│   │   └── logs/
│
├── helper functions/     # helper functions and configuration
├── scripts/             # analysis pipeline
└── main.R               # entry point
```

### Reproducibility

This project uses renv to manage the R package environment.
If required, restore the environment with:

```r
install.packages("renv")
renv::restore()
```
All paths are project-relative. The pipeline is deterministic given the input files. Make sure to open all scripts from within the R project.

### Pipeline Scripts

Scripts are executed sequentially via `main.R` and stored in `/scripts`.

- **01.load.WoS.data**: Imports raw abstract data retrieved from Web of Science (2009–2023) and performs basic cleaning.

- **02.abstract.screening**: Identifies CSS and non-CSS research using search strings, intercoder tests, and abstract-level coding.

- **03.reliability.testing**: Generates coding masks for intercoder reliability testing of full-paper coding (script a). Reads the coded masks and computes intercoder reliability metrics (script b).

- **04.final.coding.masks**: Generates final coding masks for full-paper coding of the main sample.

- **05.deduplication**: There were duplicates in the coded final sample. This file exports duplicates as a table for manual inspection. It then applies keep/remove decision to coded final sample.

- **06.data.cleaning**: Cleans the coded full-paper dataset and documents all cleaning steps in a log file.

- **07a.analysis.figures**: Produces descriptive statistics with figures from the cleaned dataset.

- **07b.analysis.tables**: Produces descriptive statistics with APA-style tables from the cleaned dataset.

### Helper functions

Scripts are loaded via `main.R` and stored in `/helper functions`.

- **paths**: Defines relative paths for the project and output messages to be printed when executing code

- **config**: Defines names of input data files

- **codebook**: Defines variables and codes coded manually, as defined in our codebook

- **logging**: Defines a function for logging all manual changes to data for error spotting after the manual coding (e.g., impossible codes)

- **helpers**: Various helper functions for cleaning data, printing outputs, etc. used in the main script

### Required Input Data (important: subject to update)

| File | Description | Used for |
|------|------------||------------|
| `01_wos_abstracts_2009_2023.xlsx` or, alternatively, `01_WoS_savedrecs_1-1000.xls`, `01_WoS_savedrecs_10001-2000.xls`, and `01_WoS_savedrecs_2000-2765.xls` | Web of Science abstract export | 01.load.wos.data |
| `02_intercoder_abstract_R1.csv`, `02_intercoder_abstract_R2.csv`, `02_intercoder_abstract_R3.csv`, `02_intercoder_abstract_R4.csv`, `02_intercoder_abstract_R5.csv` | Reliability sheets for inclusion via abstracts | 02.abstract.screening |
| `02_abstract_reli_R1.csv`, `02_abstract_reli_R2.csv` | Reliability codings for inclusion via abstracts | 02.abstract.screening |
| `02_validation_keywords_1.csv`, `02_validation_keywords_2.csv` | Manual codings for validation of CSS search strings | 02.abstract.screening |
| `02_abstract_screening_coder1.csv`, `02_abstract_screening_coder2.csv`, `02_abstract_screening_coder2_added.csv` , `02_abstract_screening_coder3.csv`| Full abstract inclusion codings | 02.abstract.screening |
| `03a_full_paper_sample.xlsx` | Full-paper sample | 03a.reliability.masks (and following) |
| `03b_reliability_mask_R1_coder1.xlsx`, `03b_reliability_mask_R1_coder2.xlsx`, `03b_reliability_mask_R1_coder3.xlsx`, `03b_reliability_mask_R2_coder1.xlsx`, `03b_reliability_mask_R2_coder2.xlsx`, `03b_reliability_mask_R2_coder3.xlsx` | Full-paper sample | 03a.reliability.masks (and following) |
| `04_df_sample_coded_reli.xlsx` | Reliability coded cases used to exclude papers before final coding | 04.final.coding.masks |
| `05_full_paper_sample_coded.xlsx` | Coded full-paper sample (pre-deduplication) | 05.deduplication |
| `05_dupes_checked.xlsx` | Manually curated list of IDs to retain during deduplication | 05.deduplication |

Several stages of the literature review require manual coding or researcher judgment (e.g., abstract screening, full-paper coding, deduplication decisions, reliability testing).
All results of these manual steps must be provided as input data in `data/in/` before running the pipeline.

### Output Paths

| Path | Description |
|------|------------|
| `data/out/intermediate/` | intermediate processing artifacts used by later pipeline steps | 
| `data/out/final/` | analysis-ready datasets and final tables/figures | 
| `data/out/logs/` | data-cleaning and processing logs | 
