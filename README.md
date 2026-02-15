## Protest-Review

This repository contains the complete analysis pipeline for the protest literature review conducted within the DFG Network on Protest & CSS Methods.

The workflow follows a linear, script-based pipeline with clearly defined inputs and outputs.
All analytical steps are reproducible given the required manually coded input files provided in `data/in/`.

To run the full pipeline:

```r
source("main.R")
```

### Reproducibility

This project uses renv to manage the R package environment.
If required, restore the environment with:

```r
install.packages("renv")
renv::restore()
```
All paths are project-relative. The pipeline is deterministic given the input files.

### Manual Coding & Researcher Decisions

Several stages of the literature review require manual coding or researcher judgment (e.g., abstract screening, full-paper coding, deduplication decisions, reliability testing).
All results of these manual steps must be provided as input data in `data/in/` before running the pipeline.

### Pipeline Scripts

Scripts are executed sequentially via `main.R`.

- **01.load.WoS.data**: Imports raw abstract data retrieved from Web of Science (2009–2023) and performs basic cleaning.

- **02.abstract.screening**: Identifies CSS and non-CSS research using search strings, intercoder tests, and abstract-level coding.

- **03.reliability.testing**: Generates coding masks for intercoder reliability testing of full-paper coding. Reads the coded masks and computes intercoder reliability metrics.

- **04.final.coding.masks**: Generates final coding masks for full-paper coding of the main sample.

- **05.deduplication**: There were duplicates in the coded final sample (n=19). This file exports duplicates as a table for manual inspection.
It then applies keep/remove decision to coded final sample.

- **06.data.cleaning**: Cleans the coded full-paper dataset and documents all cleaning steps in a log file.

- **07.analysis**: Produces descriptive statistics, APA-style tables, and figures from the cleaned dataset.

### Required Input Data

Before running `main.R`, all manually coded data from the literature review process must be placed in `data/in/`.
These files represent fixed research decisions.

| File | Description |
|------|------------|
| `wos_abstracts_2009_2023.xlsx` | Web of Science abstract export |
| `full_paper_sample.xlsx` | Full-paper sample |
| `full_paper_sample_coded.xlsx` | Coded full-paper sample (pre-deduplication) |
| `dupes_checked.xlsx` | Manually curated list of IDs to retain during deduplication |
| `df_sample_coded_reli.xlsx` | Reliability coded cases used to exclude papers before final coding |

### Output Data

All generated files are written to `data/out/`:

- `data/out/intermediate/` – intermediate processing artifacts used by later pipeline steps
- `data/out/final/` – analysis-ready datasets and final tables/figures
- `logs/` – data-cleaning and processing logs

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
├── R/                   # helper functions and configuration
├── scripts/             # analysis pipeline
└── main.R               # entry point
```


