# tflplanner 0.0.0.9000

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
