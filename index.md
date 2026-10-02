# tflplanner

`tflplanner` is a ‘shiny’ study manager for clinical **Tables, Listings
and Figures (TFLs)** made with
[rtfreporter](https://github.com/ichirio/rtfreporter). Each study is a
folder of its own – input data, definitions, programs, results and the
records of its official runs – and the app defines the study’s analysis
results data (ARD) and reports, writes their R programs and runs them.

    ARD      web GUI  =>  (Excel, optional)  =>  ARD programs  =>  the study ARD
    Tables   the study ARD, by output_id  =>  normalize, rework  =>  RTF
    Listings, Figures   SDTM / ADaM  =>  report programs  =>  RTF
    Official runs   autoexec_*.R  =>  runs/<date>_<time>_<what>/  (logs, results, code)

- **The ARD and the reports are separate steps**, which different people
  can take at different times: a table is made from whatever of the
  study ARD is there.
- **Company standards**: every default and code list the app offers –
  dropdowns, presets, ARD methods and statistics, code templates, the
  rows a new study starts with – comes from one workbook, set up once.
- **Previews and official runs**: a program run on its own is a preview;
  an official run keeps each program’s log
  ([logrx](https://github.com/pharmaverse/logrx)), what it made and the
  code it ran in a dated batch folder.
- English (default) or Japanese.

> **Status: experimental.** tflplanner is the GUI over
> [tflspec](https://github.com/ichirio/tflspec) (the specifications and
> the code written from them) and rtfreporter (the ARD and table engine
> and the RTF renderer); its screens and functions may change.

## Installation

Once, at the R console (rtfreporter and tflspec come with it):

``` r

install.packages("remotes")
remotes::install_github("ichirio/tflplanner")
```

Without access to GitHub, install the three package files you were
given, in this order:

``` r

install.packages(c("rtfreporter_0.8.2.tar.gz", "tflspec_0.0.24.tar.gz",
                   "tflplanner_0.0.2.tar.gz"), repos = NULL, type = "source")
```

## Getting started

``` r

tflplanner::setup_tflplanner()
```

asks, step by step, for the three things done at the console – where
tflplanner keeps its studies (its home), the packages its programs use,
and a shortcut that starts it with a double click (Windows: desktop and
Start menu; macOS: `~/Applications`; Linux: the applications menu).
Everything else is done in the app. Without a shortcut, start it with
[`launch_app()`](https://ichirio.github.io/tflplanner/reference/launch_app.md)
(in its own R process; also the RStudio add-in *Launch tflplanner*) or
[`run_app()`](https://ichirio.github.io/tflplanner/reference/run_app.md).

To update rtfreporter, tflspec and tflplanner later, close the app and
start it from the shortcut **tflplanner (update and launch)**, or run

``` r

tflplanner::update_tflplanner()          # the released versions
tflplanner::update_tflplanner("dev")     # the development versions
tflplanner::update_tflplanner(from = "D:/packages")   # from package files
```

The app says when a newer version is out (it looks when it starts; this
can be turned off in its settings).

The same steps as code, for scripts:

``` r

library(tflplanner)
setup_tflplanner(studies_root = "C:/studies",          # once; with the
                 sample = TRUE)                        #   sample study
standards_template("company_standards.xlsx")           # the draft to edit
setup_tflplanner(standards = "company_standards.xlsx") # your standards
add_shortcut(ask = FALSE)
run_app()
```

`sample = TRUE` (or
[`create_sample_study()`](https://ichirio.github.io/tflplanner/reference/create_sample_study.md),
or *Add the sample study* in the app’s settings) copies the sample study
**SAMPLE-01** into the studies folder and makes it: the CDISC pilot ADaM
data of
[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/), one
study ARD, four tables, a listing and a figure – a study to try
everything on.

A guide in Japanese (provisional): [tflplanner
利用ガイド（日本語）](https://ichirio.github.io/tflplanner/articles/ja-guide.html).

## License

Apache License 2.0. See
[LICENSE.md](https://ichirio.github.io/tflplanner/LICENSE.md).
