# Motivating Example: IPTW for Count Exposures using BCS70 Data

This repository contains the analysis code for the BCS70 motivating example in the manuscript titled *Inverse Probability Weighting of Count Exposures in the Presence of Missing Data: A Simulation Study*. The manuscript is currently under development.

## Getting started

If you are primarily interested in checking how the IPTW methods are applied rather than reproducing the full analysis, see `scripts/3-iptw-illustrative.R`. This part is a small illustrative script that walks through weight estimation, balance checking, and outcome modelling step by step on a single dataset. The rest of this file assumes you wish to reproduce the entire analysis.

1. **Download** the project folder.
2. **Open** `iptw-sim-example.Rproj` in your IDE (RStudio, Positron, VS Code, etc.) to set the working directory and activate `renv`.
3. **Install packages** by running `renv::restore()` in the R console. This installs exact package versions from `renv.lock` into an isolated project library (one-time step).
4. **Run the scripts** in `scripts/` sequentially (restart R between scripts for a clean environment).

## Data access

Data from the 1970 British Cohort Study (BCS70) can be obtained from the [UK Data Service](https://ukdataservice.ac.uk/) under a standard End User Licence Agreement (Study Number 200001).

The project expects the raw `.dta` files to be placed in sweep-specific subfolders inside `data/` (e.g., `data/0y/`, `data/5y/`, `data/10y/`, etc.). The [CLS GitHub documentation](https://cls-data.github.io/docs/bcs70-sweep_folders.html) describes this folder structure and provides code to download and organise the files automatically. If your files are organised differently, you will need to adjust the paths in `scripts/1-bcs70-data-cleaning.R`.

## Project structure

```
iptw-sim-example/
├── scripts/          Main analysis scripts (run in order)
├── R/                Reusable functions sourced by scripts
├── data/             BCS70 sweep data (not distributed) and processed outputs
│   └── processed/    Intermediate and final .rds objects
├── results/
│   ├── tables/       Exported summary CSVs
│   └── figures/      Exported plots (PDFs)
├── renv.lock         Package versions for reproducibility
├── renv/             renv project library
└── iptw-sim-example.Rproj
```


## Scripts

| Script | Purpose |
|--------|---------|
| `1-bcs70-data-cleaning.R` | Import, clean, and merge BCS70 sweeps; apply eligibility criteria |
| `2-multiple-imputation.R` | Set up and run multiple imputation (`mice`); produce diagnostics |
| `3-iptw-illustrative.R` | Simple worked example of IPTW for a single and multiply imputed datasets |
| `4-iptw-main.R` | Full pipeline: weights, balance, outcome models, pooling across all imputations |
| `5-results-tables-figures.R` | Generate result summaries (CSVs) and figures (PDFs) |

Reusable functions for weighting, balance diagnostics, modelling, and plotting are in `R/`. Some of these files are shared with the simulation study and may contain functions not used in this motivating example.

## Computational notes

Some steps are computationally intensive, specifically multiple imputation (script 2) and weight estimation across all imputations (script 4) can take several hours. No intermediate files are included in this repository because they contain individual-level BCS70 data, so you will need to run the full pipeline from the beginning on first use.

The scripts save intermediate objects to `data/processed/` as they run. Once generated, you can comment out the expensive steps and import the saved `.rds` files instead if you only need to re-run later scripts.

## Parallel computation

Scripts 2-4 use the `future` and `furrr` packages for parallel processing. By default, the scripts are configured to use 6 parallel workers (`n_core <- 6`). This assumes a machine with at least 6 CPU cores and sufficient RAM.

If you have fewer cores or run into memory issues, reduce `n_core` in the relevant script (search for `n_core <-`). A reasonable default is `n_core <- parallel::detectCores() - 2`. You can also replace `plan(multisession, workers = n_core)` with `plan(sequential)` to disable parallel processing entirely.

Changing the number of workers does not affect the analysis logic, but may lead to small numerical differences in results due to how random seed streams are distributed across workers.

## Original setup

- **OS:** macOS on Apple Silicon (should also run on Windows or Linux)
- **R version:** 4.4.1 (should work on 4.2+; different versions may not reproduce exact numbers)
- **Hardware:** 8 CPU cores (6 performance + 2 efficiency), 32 GB RAM; scripts configured to use 6 workers
