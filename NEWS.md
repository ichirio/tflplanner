# tflplanner (development version)

- **ARDs made elsewhere** (tflspec #101): `import_ard()` takes an ARD (rds,
  the JSON / YAML of `tflspec::tfl_write_ard()`, XPT, CSV) into the study's
  `input/ard/` -- a folder nothing else writes to, so making the study ARD
  again never overwrites it -- read-only, checked
  (`tflspec::tfl_check_ard()`) and recorded in `input/ard/imports.csv`
  (`ard_imports()`: source, time, user, md5, rows, outputs, the check,
  in use / removed).  `use_imported_ard()` sets a report's `ard_source`;
  its program then reads that file, and `study_ard()` gives a report's ARD
  from wherever it comes.  `compare_imported_ard()` compares it with the
  report's own ARD (`cards::compare_ard()`), `remove_imported_ard()` takes
  one out of use but keeps it on the record.  The screen is a later PR.
- **The company's own ARD functions** (tflspec #102):
  `company_ard_functions()` lists the functions of the standards folder's
  `ard_functions/*.R`; `use_company_ard_function()` copies one into the
  study's `programs/ard/functions/` (a study file of that name wins) and
  adds it to the ARD definition's `source`; `check_company_ard_function()`
  tries it (`tflspec::tfl_check_ard_function()`).

- **The data of an analysis is one choice** (#74).  On the ARD tab's
  analysis form, Data and Analysis set are one choice of a dataset and an
  analysis set -- "ADSL × SAF (pop_saf)", "ADAE × SAF (adae_saf)",
  "ADSL, no analysis set (adsl)" -- named as the program names the data.
  The definition keeps its two columns (`dataset`, blank for the analysis
  set's own data, and `population_id`); "(the analysis set's data)" is gone.

- **Copyright of the sample data, and citation** (#72).  The sample study's
  ADaM data are pharmaverseadam's (Apache License 2.0): its copyright
  holders are listed in `Authors@R` and `inst/COPYRIGHTS`, with what was
  changed.  `citation("tflplanner")` has a CITATION file, and the README
  says how to cite tflplanner (and cards / cardx) and acknowledges the
  packages it builds on.

- The `style` sheet's `align` help (and its Japanese) no longer warns that
  any `border_*` or look column left-aligns every column: from rtfreporter
  0.8.2.9008 a blank `align` keeps each column's default.

- **The table sheets have the columns tflspec 0.0.24.9021 added** (#66): the
  `layout` sheet's `pages_page_by` (BY pages with a row budget inside), the
  `style` sheet's default look (`header_align`, `header_bold`,
  `header_italic`, `align`, `bold`, `italic`, `underline`), table width
  (`table_width_twips`, `table_width_pct`, `table_width_pct_of_writable`)
  and `col_header_align`, and the `cell_styles` sheet's `underline` and
  `indent_twips`.  They appear in the grids as every column does (read
  from tflspec), with their help in Japanese, and a new study's standards
  offer `TRUE` / `FALSE` and `left` / `center` / `right` for them.  Blank
  = as before.

- **A figure template fits the study's data** (#64).  Applied with its
  defaults (KM: parameter OS, flag FASFL, group TRT01P), a template no
  longer draws an error when the data has not got them: for each the user
  left blank, the dataset's own is used -- its first parameter, a
  population flag it has (else ADSL's), a treatment variable it has (else
  ADSL's) -- and a note says which.  The ADSL join takes only what the
  dataset lacks (a flag in both came back as SAFFL.x / SAFFL.y and was
  found by neither name).  SAMPLE-01's KM template now draws as applied.

- **A figure is made one way: the plot designer** (#62).  Its first step
  is "Start from a template" (a type, its data and a few settings, applied
  as the designer's layers at once) or "Start empty"; both say they become
  the designer's layers.  "Apply a template..." on a figure that already
  has a design asks before replacing it.  The designer names its result,
  "This figure's Spec (YAML): spec/figures/<ID>.yml", and its YAML tab is
  "Spec (YAML)".  The plot written by hand is apart, below, as "User code
  (write the ggplot yourself)": open for a figure drawn that way, folded
  for a new one.  No change to the Spec.

- **`update_tflplanner()` behind a firewall or a proxy** (#60).  The
  update's own R process now gets this session's package repositories
  (RStudio's, an internal mirror) and download method -- a fresh Rscript
  had only cloud.r-project.org -- and first checks it can reach them and
  GitHub, naming every address it cannot ("UNREACHABLE: ...").  When the
  update does not finish, it says what failed and the two ways round it:
  `remotes::install_github()` of the three in this session, or
  `update_tflplanner(from = <folder>)`.  When the newest versions cannot
  be looked up, the list shows "?" with a note, and the update goes on.

- **The tabs in the order of the work, and where a report is made** (#58):
  Study, Data, Reports, ARD, Runs (Reports before ARD).  A report chosen
  in the list shows the buttons for its kind -- a table "Go to ARD" and
  "Make the table", a figure "Make the figure", a listing "Make the
  listing".  A top tab the chosen report has nothing on (ARD for a figure
  or a listing) is faded but still opens, and then says why.

- **The study's code list** (#56): Details (sheets) has a `codelists`
  sheet (tflspec 0.0.24.9017: `variable`, `value`, `label`, `order`), and
  its tab reads a code list from an `.xlsx` or `.csv` file into the
  study's defaults -- every table prints the values' text in their order
  (`read_codelist()`, `set_codelist()`).  A report's own rows replace the
  defaults for it; a variable's `levels` on the `variables` sheet, when
  given, is its order instead.

- **The ARD tab says what each field is in the spec** (#54).  A method is
  offered as what it does and the function it calls ("Count of one level
  (ard_dichotomous())"); each field says its spec name and, when the
  method has one, its default ("Percentages of (denominator =
  population)"); the formats have a heading; the data left blank names
  the analysis set's data ("(the analysis set's: ADSL)"); the analysis
  edited is shown as its own code under the form.  A tick shows every
  report's analyses in the grid (an analysis several reports use, such as
  BIGN, at once), edited as the whole sheet; a double click on a report
  in the study ARD's list opens its analyses.  The sidebar's report is
  "Report (output_id)"; in Japanese the sheet tabs read "ヘッダー
  (header)" and the Content tab "内容 (content)".  Data files may be `.rda`
  / `.RData` (one dataset a file; tflspec 0.0.24.9016).

- **Several column variables in the table builder** (#52): the Content
  tab's "Column variables" takes more than one (a group, then a visit
  ...), as `tables$cols` writes them (`A | B`, outermost first).  With
  several, their order is dragged; each one's columns are ordered by
  dragging as before.  The column header presets are named after the
  outermost one.

- **The page takes no clicks while the app works** (#50): opening a
  study, switching a tab or a report, saving.  After 0.4 s of work a light
  veil covers the page (the cursor says it is busy) until it is done; the
  short updates (the builder's preview, the run status) do not show it.

- **Typing in the table builder no longer closes it** (#48).  Every edit
  writes the definition, and the report chosen, the page shown and the
  builder's case were read from the definition: each edit redrew the
  form, closing the variable's panel and losing the text being typed (or,
  in an emptied field, a further BackSpace).  They now pass on only a
  change.  **A double click on the report list opens the report's
  Content**, as the study list opens a study.  **Save says "Saving..."**
  at once and cannot be pressed again until it is done ("Saved").  The
  column header presets offered are those that fit the table (the SOC / PT
  one only for a nested table), named after its column variable instead
  of "Arm".  A figure's Content shows making it from a design first, the
  plot written by hand after, under headings that say so.  The Japanese
  app says Listing throughout (no 一覧表), as the report types.

- **The report list says what each report reads, and its title** (#46).
  The Data column names the datasets a report reads -- a table its ARD
  analyses' data and their populations', a listing its dataset, a figure
  the datasets its design (or its row) reads: `adsl`, `adae / adsl` --
  with "(reworked by its own code)" when the report has data code; the
  ARD's state shows on hover.  The Description column is now the Title:
  the report's titles (the titles sheet's own lines; without them, its
  description), cut short with the whole title on hover.  The report
  types stay Table / Listing / Figure in the Japanese app too.

- **The setup, shortcut and update questions say what they do** (#44).
  Before it asks, `add_shortcut()` lists each shortcut by name and by
  place (Windows: desktop and Start menu; macOS: Applications; Linux: the
  application menu), with its folder, marks one that is already there as
  replaced, says where the launcher files are kept and that
  `remove_shortcut()` removes them; the question names how many ("Make
  these 3 shortcuts?").  Afterwards it names the files and the next step
  (Windows: pin to the taskbar from the Start menu).  `remove_shortcut()`
  separates the shortcuts from the launcher files and says that studies
  and settings are not touched; `update_tflplanner()` says where it
  installs, in what order, the channels, and to restart R; the steps of
  `setup_tflplanner()` say which files and folders they write, where the
  packages go, and how to do a declined step later.  In English and
  Japanese.

- **tflspec's new listing and report columns** (ichirio/tflspec#64): the
  listing sheets' `blank_row`, `wrap`, `sep` and `align`, and the report
  sheet's `watermark`, `figure_width_in` and `figure_height_in` show in
  Details (sheets), with their help in the app's language.  Needs tflspec
  0.0.24.9013.

- **The cell_styles sheet in Details (sheets)** (ichirio/tflspec#64).
  tflspec's table spec has a `cell_styles` sheet (one
  `plan_cell_style()` a row); it is now a table sheet of the study's
  workbook, shown and edited under Details (sheets), a report's own rows
  replacing the defaults whole.
- `preview_html()` is exported as documented (`.page_lines()`, internal,
  was exported in its place: a roxygen block attached to the wrong
  function); `devtools::document()` leaves the Rd as it is.  Needs
  tflspec 0.0.24.9012.

- **Follows tflspec's column names and help** (ichirio/tflspec#64).  The
  table spec's `columns$width` is `rel_width`, and the report's
  `table_font_size` / `title_font_size` / `footnote_font_size` are
  `*_font_size_half_points` -- in the company standards' defaults, their
  draft workbook and the sample study (SAMPLE-01; its eight reports'
  RTFs unchanged).  tflspec's column help is English now; the app shows
  it in Japanese from its own translations.  An ARD built before a
  study's own analysis functions (its key `source`) changed shows as
  outdated.  Needs tflspec 0.0.24.9011.

- **The ARD form's strata and denominator** (ichirio/tflspec#64).  An
  analysis may be repeated within variables ("Repeated within (strata)":
  a subgroup, a parameter by visit) and say what its percentages are of
  ("Percentages of": the analysis set, within a row / column / the whole
  table, another population or a dataset) -- the ARD spec's new
  `strata` and `denominator` columns, also on the analyses grid.  The
  study sheet offers the key `source` (R files of the study's own
  analysis functions).  Needs tflspec 0.0.24.9010.

- **Set up, update and start tflplanner without the console** (#40).  The
  console is needed only for what the app cannot do for itself:
  `setup_tflplanner()` with no arguments asks, step by step, for the home,
  the packages the programs use, and a shortcut.  `add_shortcut()` /
  `remove_shortcut()` make "tflplanner" and "tflplanner (update and
  launch)": on Windows a desktop and Start menu `.lnk` (no console window;
  R is found in the registry at every start, so updating R does not break
  it), on macOS `~/Applications/*.app`, on Linux a `.desktop` menu entry.
  The shortcut opens the app if it already runs on its port
  (`setup_tflplanner(port = )`, default 7470), and closing the browser
  stops it (`run_app(stop_on_close = )`).  `update_tflplanner()` updates
  rtfreporter, tflspec and tflplanner in that order in a separate R
  process -- release (CRAN, else the GitHub release) or `"dev"`, or `from =`
  package files -- and the shortcut's "update and launch" does the same.
  The app never updates itself: when it starts it looks for a newer
  version and says so (off in its settings, or `check_updates = FALSE`).
  `tflplanner_packages()` lists the packages and their versions;
  `launch_app()` (also the RStudio add-in *Launch tflplanner*) starts the
  app in its own R process.

- **Getting started: one way into New study** (#38).  With no study, the
  getting-started card's buttons open the New study dialog: "Try the
  sample study (about 1 minute)..." with the sample chosen (its ID given
  there, no longer fixed to SAMPLE-01), "New study..." (was "Create an
  empty study", which opened the same dialog) with nothing copied.  New
  study... and Register a folder show under the list even with no study;
  Open / Unregister / Refresh wait for one.  The dialog offers "Copy
  another study" only when there is one.  The Japanese "Next steps" card
  names the tabs as the app does (中身, 実行).

- **The study's analyses as CDISC ARS.**  `export_ars()` writes the ARD
  definition, with the table and report definitions, as a CDISC Analysis
  Results Standard reporting event (`tflspec::tfl_ars()`): the ARS JSON,
  CDISC's Excel template of it, and `ars_check.csv` (what the check finds
  and what ARS does not say).  The Study tab has a button for it ("Export
  the analyses as CDISC ARS").  The ARD definition's analyses gain the
  `purpose` / `reason` columns (from tflspec).  Needs tflspec 0.0.24.9009.

- The two definition workbooks are written by tflspec
  (`tfl_write_table_spec()` / `tfl_write_report_spec()`): each holds only
  its own half's sheets, and what a column means is a comment on its
  header cell instead of a `_README` sheet.  The grid's column help reads
  `tflspec::tfl_spec_columns()`.  Needs tflspec 0.0.24.9004.

- Development reopens at 0.0.2.9000, after the 0.0.2 release.

# tflplanner 0.0.2

- **Tables on rtfreporter's plan, adopted.**  The plan engine (ARD
  functions, `table_plan()` and the `plan_*()` verbs) was adopted in
  rtfreporter's pre-CRAN API review and released in rtfreporter 0.8.2;
  tflplanner 0.0.2 needs rtfreporter 0.8.2 and tflspec 0.0.24.  The
  entries below (0.0.1.9000, 0.0.1.9001) are the changes since 0.0.1.

## tflplanner 0.0.1.9001

- **Tables follow rtfreporter's redesigned plan verbs** (rtfreporter
  0.8.1.9003, ichirio/rtfreporter#498) through tflspec 0.0.23.9001.  The
  table definition's `layout` columns are named after the verbs'
  arguments now: `stub_into` is `stub_name`, `group_show` is
  `group_keep`, `colpages_carry` is `colpages_keep`, and `pages_by` is
  gone (one page per value is `group_page = TRUE` with `group_col`).
  - **A study saved with the former names is not read**: opening it
    stops with the columns to rename (in the study's workbook) instead
    of dropping their values.  Company standards with the former
    `default_layout` columns are refused the same way.  The sample study
    shipped with the package (SAMPLE-01) is updated.
  - Code written by hand in a study (data code, normalize code, setup)
    is not rewritten: a call to a former plan verb (`plan_fmt()`,
    `plan_header_style()`, `table_plan(notes = )` ...) needs the change
    by hand -- see rtfreporter's NEWS.
  - Needs rtfreporter 0.8.1.9003 and tflspec 0.0.23.9001.  The sample
    study SAMPLE-01's reports are byte-identical to 0.0.1.9000's.

## tflplanner 0.0.1.9000

- **The table engine is rtfreporter's** (plan E, ichirio/tflspec
  Discussion #23): tflspec 0.0.23.9000 keeps the specifications only, and
  the ARD functions and the plan moved to rtfreporter (0.8.1.9001, as
  experimental) under new names, with no aliases: `tfl_ard_normalize()`
  is now `normalize_ard()`, `tfl_plan()` `table_plan()`, `tfl_plan_*()`
  `plan_*()`, `tfl_apply_plan()` `plan_apply()`.  Generated programs and
  the built-in table template write the new names.
  - **Saved code is rewritten as it is read**: `tfl_ard_normalize(`
    becomes `normalize_ard(` (and `tflspec::tfl_ard_normalize(`
    `rtfreporter::normalize_ard(`) in a study's data code, normalize code
    and setup code -- from the app's saved state and from a study
    workbook's `_tflplanner` sheet -- and in the company standards' code
    templates.  Save the study (and install the standards again) to keep
    the new names; programs the app wrote are written again on save,
    hand-edited ones need the rename by hand.
  - Needs rtfreporter 0.8.1.9001 and tflspec 0.0.23.9000.
  - tflplanner 0.0.1 (tag `v0.0.1`, with tflspec `v0.0.23` and
    rtfreporter `v0.8.1`) is the last version on the former engine.

# tflplanner 0.0.1

- **First release.**  The last version before plan E (the table engine
  moving from tflspec to rtfreporter), where to go back to if plan E is
  undone.  It runs on tflspec 0.0.23 (`Remotes:` pins that tag) and
  rtfreporter 0.8.1 or later.

- Fix: `run_app("SAMPLE-01")` (a study given at start) stopped the session
  on its first page ("Can't access reactive value 'ver' outside of
  reactive consumer"); opening the study from the Studies tab worked.

- **The Figures tab keeps up with editing**: the preview is drawn at
  screen resolution (the saved size; about 1 s instead of several), only
  while the tab shows, 0.6 s after the last change; *Redraw on change* can
  be turned off, and a notice then says the drawing is older than the
  design.  The code of the chosen piece still follows every change at
  once.  `preview_figure()` gains `max_px`.  (Code generation itself got
  about 30 times faster in tflspec's catalog cache, ichirio/tflspec#41.)

- **One tab a kind of report** (#26): the tabs are now *Tables* (the
  table definition and the table builder, as two sub-tabs), *Listings*
  and *Figures* (the Plot Designer), in place of *Table definition*,
  *Table builder (beta)*, *Listing / Figure* and *Plot Designer*.  A
  hand-written figure's datasets and data part moved from *Listing /
  Figure* to the top of *Figures*.  Choosing a report in the sidebar while
  one of these tabs is open opens its own kind's tab.

- The table builder's preview plans the table with
  `tflspec::tfl_table_plan()`: tflspec 0.0.20's `tfl_plan()` no longer
  reads a table definition (`spec =`).  Needs tflspec 0.0.20 (#23).

- **Plot Designer: every figure type, and copies per parameter** (#21).
  The template list (grouped by kind) now covers every type: the 27
  templates in parts (KM, waterfall, swimmer, spider, bar, mean over time,
  spaghetti, box, scatter, PK) with the fields each kind needs on the start
  screen (value, x / y, nominal time, category, responders, duration, the
  visit), and the whole-script templates (forest, AE dot, butterfly, eDISH,
  sankey, sunburst), whose design is one *Whole figure* piece edited on a
  form of the type's own arguments.  *Copy to other parameters* makes one
  new figure report per PARAMCD from a designed figure, its design the
  same but for the parameter (`copy_fig_to_params()`).  Needs tflspec
  0.0.19.

- **Plot Designer: advice and presets** (#19).  The preview now carries
  tflspec's advice on the design (what is usually wanted and is missing or
  unusual: a KM figure without the number at risk, a legend inside the
  panel with many groups, more groups than the palette has colours, text
  visits with no order, a waterfall without its marks ...) together with
  the checks, as an overlay on the figure; where one change would do it,
  *Apply* makes it.  A designed figure can be kept as a company *preset*
  (*Save as preset*: a `.yml` under the home's `standards/figure-presets`),
  and a new design can start from a preset instead of a template.
  `fig_presets()`, `save_fig_preset()`, `read_fig_preset()`,
  `remove_fig_preset()`; `preview_figure()` gains `advice`.  Needs tflspec
  0.0.18.

- **Plot Designer** (#15): a figure can be designed instead of written by
  hand, as tflspec's figure design: data steps from ADaM (read, join, keep a
  PARAMCD or an analysis set, derive, change a time's unit, order values,
  rank, or R code), statistics (a KM fit, summary statistics, or R code),
  the figure-wide settings and the layers in order (KM curves and marks,
  the number at risk, any layer of tflspec's geom catalog, any function by
  name, or R code).  The new *Plot Designer* tab starts a design from a
  template (KM with the number at risk, mean over time, waterfall ...)
  filled in for the study's data; the pieces are then a stack on the left
  (add, move, remove; a piece with a problem is marked), the chosen piece
  is a form on the right (defaults shown; the data's datasets, variables
  and PARAMCDs to pick from), and the middle shows the figure as its
  program saves it (the PNG, at its size), redrawn as it changes, with the
  checks.  The code and the design (YAML) are below.  Designs are saved
  with the study and written as `spec/figures/<output_id>.yml`; the
  figure's program takes its plot from the design.  `fig_design()`,
  `set_fig_design()` and `preview_figure()` do the same from R.  Needs
  tflspec 0.0.15.

- **Report programs carry the expanded code** (#17): instead of reading
  the workbooks at run time (`tfl_read_report_spec()`, `tfl_plan(data, spec
  = spec)`, `tfl_report(spec, ...)`), a report program now says what it
  makes -- the table's `tfl_plan() |> tfl_plan_*()` pipeline
  (`tflspec::tfl_table_code()`), the document's `rtf_document()`,
  `rtf_section()`, `rtf_tables()` / `rtf_figures()`, `rtf_titles()`,
  `rtf_footnotes()` (`tfl_report_code()`) and the output path.  Generate
  the programs again after changing the workbooks.  A definition that does
  not hold yet gives a program whose `stop()` says why.  The reports are
  unchanged (the sample study's RTF are identical).  Requires tflspec
  0.0.14.

- The listing sheets of `listing_figure_spec.xlsx` (`listings`,
  `listing_cols`) are tflspec's listing definition: their columns, reading
  and normalising come from `tflspec::tfl_listing_spec()` /
  `tfl_read_listing_spec()`, and the listing program from
  `tfl_listing_code()` with that definition (tflspec 0.0.11).  The workbook
  and the programs are unchanged; only the `figures` sheet stays
  tflplanner's own (#13).

- Follows tflspec's `tfl_` prefix (tflspec 0.0.9): the programs it writes
  say `tfl_ard_normalize()`, `tfl_read_report_spec()`, `tfl_plan()`,
  `tfl_report()` and `tfl_report_path()`, and it calls tflspec by the new
  names.  tflspec no longer has the former names, so rework code saved in
  a study that calls them (`data <- ard_normalize(ard)` ...) stops with
  "could not find function": rename the call (`tfl_ard_normalize()`).  The
  reports are unchanged; the programs' checksums change with the names
  (#11).

- *New study* now offers the sample study itself (copied under the new
  study's ID, its ARD and reports made at once) in place of tflspec's
  table-definition examples, which had no ARD definition or data.
  `create_sample_study()` gains `study_id` / `title` / `compound` /
  `phase` / `description`, and writes `spec/ard_spec.xlsx` (an export of
  the ARD definition, to read) so every definition is there as Excel.
- Figures in the company's style, and checked: the company standards gain
  `figure_settings`, `figure_colors` and `figure_markers` (built-in =
  tflspec's figure style, taken from the KM / waterfall / swimmer sample
  programs).  Saving writes `programs/tfl/fig_setup.R` (`theme_tfl()`,
  `scale_colour_tfl()`, `tfl_marker()`, `tfl_save()`, `tfl_km_risk()`,
  `tfl_check()`); every figure program sources it and ends with
  `tfl_check(plot)`, whose warnings the official run counts.
- SAMPLE-01 gains ADTTE (time to the first dermatologic event, derived from
  ADSL / ADAE), a Kaplan-Meier table (T-14-2-2) and a Kaplan-Meier figure
  (F-14-2-2) whose number at risk is the table's ARD.
- The sample study SAMPLE-01 (`create_sample_study()`,
  `setup_tflplanner(sample = TRUE)`, *Add the sample study* in the app):
  the CDISC pilot ADaM data of pharmaverseadam, one study ARD, four
  tables, a listing and a figure, made at once by an official run.
- The ARD definition is kept by the app (web GUI); `ard_spec.xlsx` is an
  optional export / import (`write_ard_spec()`, `export_spec()`,
  `import_spec()`).  Saving writes one ARD program per output
  (`programs/ard/`), `ard_setup.R` and the folder's copy of the definition.
- Official runs (`run_batch()`, `programs/*/autoexec_*.R`) make a dated
  batch folder with each program's logrx log, what the run made and the
  code it ran; a program run on its own is a preview and keeps no log.
- Company standards (`standards_template()`, `setup_tflplanner(standards =)`):
  dropdowns, presets, ARD methods and statistics (`ard_statistics()`), code
  templates (`code_templates()`), the rows a new study starts with.
- ARD statistics beyond cards' own (CV, SE, geometric mean / CV and CIs,
  percentiles, ...) and per-statistic formats giving `stat_fmt`.
- Listings in rows (rtfreporter's `multiline`) and figures that read their
  data, with the plot written by hand.

- Input assistance from the ARD: `fetch_ard()` runs a report's data code
  from the study folder and keeps `ard_meta()` (keys, hierarchy,
  variables, levels, statistics, with labels and value order from the
  source data); `fill_variables()`, `fill_tables()`, `cell_presets()`,
  `header_presets()`, `add_preset()`; dropdowns in the grids.
- A report's data code is two steps: the ARD code (`data_code`, makes
  `ard`) and normalize and rework (`process_code`, makes `data`;
  `data_lines()`).
- The sidebar shows a report, the Study defaults or ALL; a report's grid
  shows the default rows it inherits (`inherited_rows()`).
- The app is in English (default) or Japanese (`tr()`,
  `setup_tflplanner(language = )`).

- tflplanner keeps each study's state in a home of its own
  (`setup_tflplanner()`, `tflplanner_home()`, `tflplanner_config()`): a
  study opens as it was last saved, with a history of earlier saves.  The
  definition workbooks are written from it; `export_spec()` /
  `import_spec()` move them in and out.  `create_study()`,
  `open_study()`, `register_study()`, `unregister_study()` and
  `list_studies()` work on the registered studies; `run_app()` sets the
  home up on first use.
- The code editors no longer write one report's text into another when
  the study or report is switched while the browser is still reporting.

- Studies: each study is a folder (`study.yml`, `data/`, `spec/`, `programs/`,
  `output/ard/`, `output/tfl/`, `logs/`) created, opened, saved, run and
  tracked from the app (`create_study()`, `open_study()`, `save_study()`,
  `run_study()`, `study_status()`).
- Reports have a type (Table / Listing / Figure); programs run from the
  study folder, Tables save their ARD, generated programs carry a checksum
  and follow the definition until edited by hand.
- First version: a 'shiny' editor for rtfreporter's `table_spec.xlsx` and
  `report_spec.xlsx`, writing one R program per report and
  `autoexec_report.R`.
