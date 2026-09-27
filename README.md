<!-- README.md is written by hand; keep it short and link to the pkgdown site. -->

# tflplanner

<!-- badges: start -->
[![R-CMD-check](https://github.com/ichirio/tflplanner/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ichirio/tflplanner/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/ichirio/tflplanner/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ichirio/tflplanner/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/ichirio/tflplanner/graph/badge.svg)](https://app.codecov.io/gh/ichirio/tflplanner)
[![pkgdown](https://github.com/ichirio/tflplanner/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/ichirio/tflplanner/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: Apache 2.0](https://img.shields.io/badge/license-Apache_2.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
<!-- badges: end -->

`tflplanner` is a 'shiny' study manager for clinical **Tables, Listings
and Figures (TFLs)** made with
[rtfreporter](https://github.com/ichirio/rtfreporter).  Each study is a
folder of its own -- input data, definitions, programs, results and the
records of its official runs -- and the app defines the study's analysis
results data (ARD) and reports, writes their R programs and runs them.

```
ARD      web GUI  =>  (Excel, optional)  =>  ARD programs  =>  the study ARD
Tables   the study ARD, by output_id  =>  normalize, rework  =>  RTF
Listings, Figures   SDTM / ADaM  =>  report programs  =>  RTF
Official runs   autoexec_*.R  =>  runs/<date>_<time>_<what>/  (logs, results, code)
```

- **The ARD and the reports are separate steps**, which different people
  can take at different times: a table is made from whatever of the study
  ARD is there.
- **Company standards**: every default and code list the app offers --
  dropdowns, presets, ARD methods and statistics, code templates, the rows
  a new study starts with -- comes from one workbook, set up once.
- **Previews and official runs**: a program run on its own is a preview;
  an official run keeps each program's log ([logrx](https://github.com/pharmaverse/logrx)),
  what it made and the code it ran in a dated batch folder.
- English (default) or Japanese.

> **Status: experimental.**  tflplanner depends on the development branch
> of rtfreporter, and its screens and functions may change.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ichirio/rtfreporter@feat/474-ard-experimental")
remotes::install_github("ichirio/tflplanner")
install.packages(c("cards", "cardx", "dplyr", "logrx"))
```

## Getting started

```r
library(tflplanner)
setup_tflplanner(studies_root = "C:/studies")          # once
standards_template("company_standards.xlsx")           # the draft to edit
setup_tflplanner(standards = "company_standards.xlsx") # your standards
run_app()
```

A guide in Japanese (provisional):
[tflplanner 利用ガイド（日本語）](https://ichirio.github.io/tflplanner/articles/ja-guide.html).

## License

Apache License 2.0.  See [LICENSE.md](LICENSE.md).
