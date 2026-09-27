# tflplanner 0.0.0.9000

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
