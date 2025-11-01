# trialforgeR

[![R-CMD-check](https://github.com/codex-assistant/trialforgeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/codex-assistant/trialforgeR/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/codex-assistant/trialforgeR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/codex-assistant/trialforgeR/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/codex-assistant/trialforgeR/actions/workflows/lint.yaml/badge.svg)](https://github.com/codex-assistant/trialforgeR/actions/workflows/lint.yaml)

Spec-driven validation utilities, domain rules, and reporting for synthetic
clinical datasets.

## Installation

```r
# install.packages("remotes")
remotes::install_github("codex-assistant/trialforgeR")
```

## Quickstart

```r
library(trialforgeR)

data_files <- list(
  sdtm_dm = system.file("extdata/sample_data/sdtm_dm.csv", package = "trialforgeR"),
  sdtm_ae = system.file("extdata/sample_data/sdtm_ae.csv", package = "trialforgeR"),
  adam_adae = system.file("extdata/sample_data/adam_adae.csv", package = "trialforgeR")
)

results <- trialforge_validate(data_files)
results

report_path <- generate_qc_report(results)
report_path
```

## Shiny App

```r
run_trialforge_app()
```

Upload CSV files, optionally override the spec, and download the resulting QC
report directly from the interface.

## Spec-Driven CI

The package ships with GitHub Actions workflows for R CMD check, unit test
coverage, and linting to encourage disciplined validation practices.
