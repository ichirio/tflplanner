# rtfplanner

A 'shiny' study manager for clinical TFLs built with
[rtfreporter](https://github.com/ichirio/rtfreporter).  Each study is a
folder that holds its input data, its definitions, its programs, its
deliverables and its logs; the app defines the reports and runs them.

## Where rtfplanner keeps things

rtfplanner has a **home** of its own, apart from the study folders:

```
<home>/                      tools::R_user_dir("rtfplanner", "data") by default
  config.yml                 studies_root (where new study folders go), last_study
  studies/<STUDY_ID>/
    state.json               the study as last saved -- the master copy
    history/<time>.json      every earlier saved state
```

A study opens **as it was last saved**, from its `state.json`: the study's
fields, every sheet of the definition, the report list and the data code.
The definition workbooks in the study folder's `spec/` are written from it
on every save (the programs read them); exporting them elsewhere and
importing workbooks are separate, explicit actions.

```r
rtfplanner::setup_rtfplanner(studies_root = "C:/studies")   # once
rtfplanner::setup_rtfplanner(home = "D:/rtfplanner")         # another home
```

## A study is a folder

```
ABC-101/
  study.yml            the study: id, title, compound, phase, description
  ABC-101.Rproj        open it and the study folder is the working directory
  data/adam/           input data: analysis datasets
  data/sdtm/                       tabulation datasets
  data/other/                      anything else
  spec/                table_spec.xlsx, report_spec.xlsx
  programs/            one program per report, autoexec_report.R
  output/ard/          deliverable data: each Table's ARD (.rds)
  output/tfl/          deliverable reports: the RTF files
  logs/                one log per program run
```

Every program runs **from the study folder** and names its files relative
to it, so a study can be moved, copied or zipped and still run.

## Reports: Tables, Listings, Figures

A study's reports (TFL) are listed in order; each has a type.

| Type | Content | Layout |
|---|---|---|
| Table | 1. ARD code makes `ard`; 2. normalize and rework makes `data` (default `data <- ard_normalize(ard)`), saved to `output/ard/` | `table_spec.xlsx` → `rtf_plan(spec = )` |
| Listing | the data code leaves `content`, `rtftable` pages | program |
| Figure | the data code leaves `content`, the figures | program |

The report around the content — page, header, footer, titles, footnotes —
comes from `report_spec.xlsx` for every type.

A generated program carries a checksum.  While nobody edits it, it follows
the definition (saving rewrites it when the definition or data code
changes); once edited by hand it is kept, until you regenerate it.

## Input assistance from the ARD

**Run and read the ARD** runs a report's data code (setup, ARD code,
normalize and rework) from the study folder and keeps what the result holds
(`fetch_ard()`, `ard_meta()`) -- read after the rework, since that is what
the table is built from:

- the **keys**: the column key (`TRT01A`) and the hierarchy
  (`AEBODSYS > AEDECOD`), with their levels;
- the **analysis variables**, their kind, levels and statistics;
- from the source data the code loaded (`adsl`, ...): each variable's
  **label**, and the **order** of its values -- a factor's levels, or a
  numeric companion `<name>N` (`TRT01A` by `TRT01AN`, `AGEGR1` by
  `AGEGR1N`).

With it the Table definition offers:

- **Fill variables and levels** -- a `variables` row per column key and
  analysis variable, with levels, label and order (blank cells only);
- **Fill table roles** -- `cols`, `rows` / `label` / `sort` of the
  hierarchy, or `group = variable`;
- **cell presets** (n / Mean (SD) / Median / Q1, Q3 / Min, Max; n (%);
  n/N (%) ...) for every variable of a kind or one variable, and **column
  header presets** (`Arm / (N=n)`, `Arm (N=n) n (%)`, SOC / PT);
- **dropdowns** in the grids with the ARD's variables, keys, contexts and
  the templates its statistics can fill -- anything else may still be
  typed.

A report's grid also shows, greyed, the **study default** rows it
inherits (rows with a blank `output_id`: the page header, the run line,
the usual cell template ...).

## Install and start

rtfplanner needs the rtfreporter branch that has the definition workbooks
(`feat/474-ard-experimental`, 0.8.0.9082 or later: its column header N is the analysis set).

```r
remotes::install_github("ichirio/rtfreporter@feat/474-ard-experimental")
remotes::install_local("C:/Yrepo/rtfplanner")

rtfplanner::run_app()             # sets the home up the first time
rtfplanner::run_app("ABC-101")    # open a study at start
```

## The app

- **試験** — the registered studies (the last one used is selected);
  open one as last saved, create one (empty, copied from another study,
  or from rtfreporter's five sample reports), register an existing study
  folder, unregister one (its folder stays); edit its title / compound /
  phase / rounding; import or export the definition workbooks; zip it.
- **帳票一覧** — add (with type), copy, rename, delete and order the
  reports; each report's data code and the setup code every program runs;
  the program as it will be written, and whether the one on disk was edited.
- **Table definition / Report layout** — one grid per sheet, filtered to
  what the sidebar shows: a report, the **Study defaults**, or **ALL**
  (every row, with `output_id`); input assistance above; paste from
  Excel; column help from rtfreporter's `_README`.
- The app is in English or Japanese: Studies > Settings, or
  `setup_rtfplanner(language = "ja")`.
- **データ** — the input data; upload into `data/adam|sdtm|other`, preview
  (`.rds`, `.csv`, `.xpt`, `.sas7bdat`, `.parquet`).
- **成果物** — per report: program state, ARD, RTF, status (未作成 / TODO /
  未実行 / エラー / 要再実行 / OK); run all or the selected ones in the
  background, read logs, download an RTF, check the definition.
- **保存** writes the workbooks, the programs and `study.yml`; nothing is
  written before.

## From R

```r
library(rtfplanner)
s <- create_study("ABC-101", title = "A phase 2 study")
s <- open_study("ABC-101")                  # as last saved
s$planner <- add_output(s$planner, "T_DM", type = "table")
s <- save_study(s)
run_study(s)            # -> study_status(s)
```

## Roadmap

- ARD generation: cards / cardx code generated from an Excel spec
  (`programs/ard/`, results in `output/ard/`), which Table programs read.
- Figures from templates, defined in an Excel spec and the GUI.
- Listings generated by listing type.
