# rtfplanner

A 'shiny' editor for the definition workbooks that
[rtfreporter](https://github.com/ichirio/rtfreporter) builds clinical
tables from — and the programs that use them.

rtfplanner writes, for one study:

| File | What it holds |
|---|---|
| `table_spec.xlsx` | the table: `tables`, `variables`, `cells`, `layout`, `columns`, `style`, `col_header` (+ `rounding`) |
| `report_spec.xlsx` | the report around it: `report`, `page`, `header`, `footer`, `titles`, `footnotes` (+ `output_path`, `program_dir`), and the `_rtfplanner` sheet (report list, data code) |
| `<program>.R` | one program per report: makes `data`, then `read_report_spec()` → `rtf_plan(spec = )` → `rtf_report()` → `generate_rtfreport()` |
| `autoexec_report.R` | runs every report program in list order, one `Rscript` process each, logs in `logs/` |

The workbooks are written by `rtfreporter::write_table_spec()` and checked
with `rtfreporter::read_report_spec()`, so what the app saves is exactly
what the programs read.

## Install and start

rtfplanner needs the rtfreporter branch that has the definition workbooks
(`feat/474-ard-experimental`, 0.8.0.9081 or later).

```r
# install.packages("remotes")
remotes::install_github("ichirio/rtfreporter@feat/474-ard-experimental")
remotes::install_local("C:/Yrepo/rtfplanner")

rtfplanner::run_app()
rtfplanner::run_app(c("spec/table_spec.xlsx", "spec/report_spec.xlsx"))
```

## Using the app

- **Sidebar — the chosen report.**  Every sheet shows only the rows of the
  report chosen here; `(既定 = 空欄)` shows the study-wide defaults
  (blank `output_id`), `(全行)` shows everything.
- **帳票一覧** — add, copy (every sheet's rows at once), rename, delete and
  order the reports; write each report's data code (it must leave `data`,
  the `ard_normalize()`d ARD — `content` for a figure) and the setup code
  every program runs first.  A report with no data code gets a TODO that
  stops with a clear message.  The generated program is previewed live.
- **表の定義 / 帳票の体裁** — one grid per sheet: right-click to add or
  delete rows, paste blocks from Excel, dropdowns for fixed values.  The
  column descriptions come from rtfreporter's own `_README`.
- **試験** — `rounding`, `output_path`, `program_dir`.
- **ファイル** — open workbooks, check them, write everything to a folder
  (an existing report program is kept unless you tick overwrite — it may
  have been edited by hand), or download a portable zip.

## From R

```r
library(rtfplanner)
p <- read_planner(c("table_spec.xlsx", "report_spec.xlsx"))
p <- copy_output(p, "DM", "DM_ITT")
check_planner(p)
export_planner(p, "study/tfl")
```
