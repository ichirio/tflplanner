# Package index

## The app and its home

- [`run_app()`](https://ichirio.github.io/tflplanner/reference/run_app.md)
  [`planner_app()`](https://ichirio.github.io/tflplanner/reference/run_app.md)
  : Start the tflplanner app
- [`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)
  : Set up tflplanner's home
- [`tflplanner_home()`](https://ichirio.github.io/tflplanner/reference/tflplanner_home.md)
  : Where tflplanner keeps its settings and the studies' saved state
- [`tflplanner_config()`](https://ichirio.github.io/tflplanner/reference/tflplanner_config.md)
  [`studies_root()`](https://ichirio.github.io/tflplanner/reference/tflplanner_config.md)
  : tflplanner's settings
- [`tr()`](https://ichirio.github.io/tflplanner/reference/tr.md)
  [`tflplanner_language()`](https://ichirio.github.io/tflplanner/reference/tr.md)
  [`app_languages()`](https://ichirio.github.io/tflplanner/reference/tr.md)
  : The app's languages

## Install, update and launch

What is done at the R console: the setup, the packages, the updates and
a shortcut that starts the app with a double click.

- [`add_shortcut()`](https://ichirio.github.io/tflplanner/reference/add_shortcut.md)
  [`remove_shortcut()`](https://ichirio.github.io/tflplanner/reference/add_shortcut.md)
  : Make a shortcut that starts tflplanner
- [`launch_app()`](https://ichirio.github.io/tflplanner/reference/launch_app.md)
  : Start tflplanner in its own R process
- [`update_tflplanner()`](https://ichirio.github.io/tflplanner/reference/update_tflplanner.md)
  : Update tflplanner, rtfreporter and tflspec
- [`tflplanner_packages()`](https://ichirio.github.io/tflplanner/reference/tflplanner_packages.md)
  : The packages tflplanner needs, and their versions

## Company standards

- [`standards_template()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)
  [`read_standards()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)
  [`company_standards()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)
  : Company standards
- [`code_templates()`](https://ichirio.github.io/tflplanner/reference/code_templates.md)
  : Code templates
- [`setup_code()`](https://ichirio.github.io/tflplanner/reference/setup_code.md)
  : The company's setup code
- [`add_standard_defaults()`](https://ichirio.github.io/tflplanner/reference/add_standard_defaults.md)
  : Add the company's study defaults a study lacks

## Studies

- [`create_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
  [`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
  [`register_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
  [`unregister_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
  [`list_studies()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
  : Create, open, register and list studies
- [`create_sample_study()`](https://ichirio.github.io/tflplanner/reference/create_sample_study.md)
  : Add the sample study
- [`save_study()`](https://ichirio.github.io/tflplanner/reference/save_study.md)
  : Save a study
- [`reload_from_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)
  [`write_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)
  [`spec_status()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)
  : The study's definition files: read them, compare them, write them
  back
- [`export_spec_files()`](https://ichirio.github.io/tflplanner/reference/export_spec_files.md)
  [`preview_spec_import()`](https://ichirio.github.io/tflplanner/reference/export_spec_files.md)
  [`import_spec_files()`](https://ichirio.github.io/tflplanner/reference/export_spec_files.md)
  : Copy the definition files out, and bring an edited copy back
- [`export_spec()`](https://ichirio.github.io/tflplanner/reference/export_spec.md)
  [`import_spec()`](https://ichirio.github.io/tflplanner/reference/export_spec.md)
  : Definition workbooks in and out
- [`export_ars()`](https://ichirio.github.io/tflplanner/reference/export_ars.md)
  : Export the study's analyses as CDISC ARS
- [`study_layout()`](https://ichirio.github.io/tflplanner/reference/study_layout.md)
  : The folder layout of a study
- [`study_setup_code()`](https://ichirio.github.io/tflplanner/reference/study_setup_code.md)
  : The study's setup program
- [`study_files()`](https://ichirio.github.io/tflplanner/reference/study_files.md)
  : Files in a study folder
- [`read_data_head()`](https://ichirio.github.io/tflplanner/reference/read_data_head.md)
  : The first rows of a data file
- [`study_status()`](https://ichirio.github.io/tflplanner/reference/study_status.md)
  : What each report of a study has produced

## The ARD definition and the study ARD

- [`study_ard_conditions()`](https://ichirio.github.io/tflplanner/reference/study_ard_conditions.md)
  : What went wrong inside the analyses while a study's ARD was made
- [`ard_rows()`](https://ichirio.github.io/tflplanner/reference/ard_rows.md)
  [`set_ard_rows()`](https://ichirio.github.io/tflplanner/reference/ard_rows.md)
  : A report's rows of the ARD definition
- [`remove_analysis()`](https://ichirio.github.io/tflplanner/reference/remove_analysis.md)
  : Delete one analysis of a report
- [`copy_analysis()`](https://ichirio.github.io/tflplanner/reference/copy_analysis.md)
  : Copy one analysis of a report
- [`set_analysis_data()`](https://ichirio.github.io/tflplanner/reference/set_analysis_data.md)
  [`remove_analysis_data()`](https://ichirio.github.io/tflplanner/reference/set_analysis_data.md)
  [`import_analysis_data()`](https://ichirio.github.io/tflplanner/reference/set_analysis_data.md)
  [`name_analysis_data()`](https://ichirio.github.io/tflplanner/reference/set_analysis_data.md)
  [`copy_analysis_data()`](https://ichirio.github.io/tflplanner/reference/set_analysis_data.md)
  : Add, change or remove an analysis data of the ARD definition
- [`set_report_population()`](https://ichirio.github.io/tflplanner/reference/set_report_population.md)
  : A report's analysis set
- [`ard_setup_code()`](https://ichirio.github.io/tflplanner/reference/ard_setup_code.md)
  [`ard_program_code()`](https://ichirio.github.io/tflplanner/reference/ard_setup_code.md)
  [`ard_autoexec_code()`](https://ichirio.github.io/tflplanner/reference/ard_setup_code.md)
  : The ARD programs of a study
- [`study_helpers_code()`](https://ichirio.github.io/tflplanner/reference/study_helpers_code.md)
  : The functions a study's programs call
- [`run_ard()`](https://ichirio.github.io/tflplanner/reference/run_ard.md)
  : Run a report's analyses, or the whole study's
- [`ard_view()`](https://ichirio.github.io/tflplanner/reference/ard_view.md)
  : An ARD as a plain table to read
- [`ard_status()`](https://ichirio.github.io/tflplanner/reference/ard_status.md)
  : Where each output's ARD stands
- [`update_study_ard()`](https://ichirio.github.io/tflplanner/reference/update_study_ard.md)
  : Preview one output's ARD
- [`study_ard()`](https://ichirio.github.io/tflplanner/reference/study_ard.md)
  : One report's ARD, wherever it comes from

## ARDs made elsewhere

- [`import_ard()`](https://ichirio.github.io/tflplanner/reference/import_ard.md)
  : Take an ARD made elsewhere into a study
- [`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md)
  : ARDs taken into a study
- [`use_imported_ard()`](https://ichirio.github.io/tflplanner/reference/use_imported_ard.md)
  : Use an ARD taken in for a report, or stop using it
- [`remove_imported_ard()`](https://ichirio.github.io/tflplanner/reference/remove_imported_ard.md)
  : Stop using an ARD taken in
- [`compare_imported_ard()`](https://ichirio.github.io/tflplanner/reference/compare_imported_ard.md)
  : Compare a report's own ARD with the one taken in

## ARD functions of one’s own

- [`company_ard_functions()`](https://ichirio.github.io/tflplanner/reference/company_ard_functions.md)
  : The company's own ARD functions
- [`use_company_ard_function()`](https://ichirio.github.io/tflplanner/reference/use_company_ard_function.md)
  : Use one of the company's ARD functions in a study
- [`check_company_ard_function()`](https://ichirio.github.io/tflplanner/reference/check_company_ard_function.md)
  : Try one of the company's ARD functions
- [`study_ard_functions()`](https://ichirio.github.io/tflplanner/reference/study_ard_functions.md)
  : A study's own ARD functions
- [`own_ard_functions()`](https://ichirio.github.io/tflplanner/reference/own_ard_functions.md)
  : The ARD functions of one's own a study can use
- [`own_function_checks()`](https://ichirio.github.io/tflplanner/reference/own_function_checks.md)
  : The last try of each of a study's own ARD functions
- [`try_ard_function()`](https://ichirio.github.io/tflplanner/reference/try_ard_function.md)
  : Try one of a study's (or the company's) ARD functions
- [`new_ard_function()`](https://ichirio.github.io/tflplanner/reference/new_ard_function.md)
  : Start an ARD function of one's own from a template
- [`take_company_ard_function()`](https://ichirio.github.io/tflplanner/reference/take_company_ard_function.md)
  : Replace a study's copy of an ARD function with the company's

## Tables, listings and figures

- [`new_planner()`](https://ichirio.github.io/tflplanner/reference/new_planner.md)
  : A new, empty study definition
- [`add_output()`](https://ichirio.github.io/tflplanner/reference/add_output.md)
  [`sort_outputs()`](https://ichirio.github.io/tflplanner/reference/add_output.md)
  [`copy_output()`](https://ichirio.github.io/tflplanner/reference/add_output.md)
  [`rename_output()`](https://ichirio.github.io/tflplanner/reference/add_output.md)
  [`remove_output()`](https://ichirio.github.io/tflplanner/reference/add_output.md)
  : Edit the report list
- [`output_ids()`](https://ichirio.github.io/tflplanner/reference/output_ids.md)
  : Report ids the definition names
- [`table_sheets()`](https://ichirio.github.io/tflplanner/reference/table_sheets.md)
  [`report_sheets()`](https://ichirio.github.io/tflplanner/reference/table_sheets.md)
  [`report_types()`](https://ichirio.github.io/tflplanner/reference/table_sheets.md)
  : Sheets of the two definition workbooks
- [`read_codelist()`](https://ichirio.github.io/tflplanner/reference/read_codelist.md)
  [`set_codelist()`](https://ichirio.github.io/tflplanner/reference/read_codelist.md)
  : Read a code list into a report
- [`import_codelist()`](https://ichirio.github.io/tflplanner/reference/import_codelist.md)
  [`standard_codelists()`](https://ichirio.github.io/tflplanner/reference/import_codelist.md)
  : A report's code lists: copy one in
- [`inherited_rows()`](https://ichirio.github.io/tflplanner/reference/inherited_rows.md)
  : The study default rows a report inherits
- [`read_planner()`](https://ichirio.github.io/tflplanner/reference/read_planner.md)
  : Read the definition workbooks
- [`write_planner()`](https://ichirio.github.io/tflplanner/reference/write_planner.md)
  : Write the two definition workbooks
- [`check_planner()`](https://ichirio.github.io/tflplanner/reference/check_planner.md)
  : Check the definition the way the report programs will read it
- [`report_info()`](https://ichirio.github.io/tflplanner/reference/report_info.md)
  : Where a report's program and RTF go
- [`lf_rows()`](https://ichirio.github.io/tflplanner/reference/lf_rows.md)
  [`set_lf_rows()`](https://ichirio.github.io/tflplanner/reference/lf_rows.md)
  [`listing_types()`](https://ichirio.github.io/tflplanner/reference/lf_rows.md)
  : A listing's or figure's definition
- [`preview_listing()`](https://ichirio.github.io/tflplanner/reference/preview_listing.md)
  : The rows of a listing, as they will print
- [`fig_design()`](https://ichirio.github.io/tflplanner/reference/fig_design.md)
  [`set_fig_design()`](https://ichirio.github.io/tflplanner/reference/fig_design.md)
  : A figure's design (the Plot Designer)
- [`preview_figure()`](https://ichirio.github.io/tflplanner/reference/preview_figure.md)
  : Draw a figure from its design
- [`set_fig_ard_source()`](https://ichirio.github.io/tflplanner/reference/set_fig_ard_source.md)
  : Where a figure's ARD comes from
- [`set_fig_own_analyses()`](https://ichirio.github.io/tflplanner/reference/set_fig_own_analyses.md)
  : A figure's own analyses, written from its design
- [`fig_presets()`](https://ichirio.github.io/tflplanner/reference/fig_presets.md)
  [`save_fig_preset()`](https://ichirio.github.io/tflplanner/reference/fig_presets.md)
  [`read_fig_preset()`](https://ichirio.github.io/tflplanner/reference/fig_presets.md)
  [`remove_fig_preset()`](https://ichirio.github.io/tflplanner/reference/fig_presets.md)
  : Figure presets
- [`copy_fig_to_params()`](https://ichirio.github.io/tflplanner/reference/copy_fig_to_params.md)
  : The same figure for other parameters
- [`make_user_report()`](https://ichirio.github.io/tflplanner/reference/make_user_report.md)
  : Make a figure written by hand a user-code report
- [`preview_user()`](https://ichirio.github.io/tflplanner/reference/preview_user.md)
  : Run a user-code report's code and see what it leaves

## Reviewing a study

- [`study_review()`](https://ichirio.github.io/tflplanner/reference/study_review.md)
  [`review_problems()`](https://ichirio.github.io/tflplanner/reference/study_review.md)
  [`review_facts()`](https://ichirio.github.io/tflplanner/reference/study_review.md)
  : Review a study's definition

## Input assistance and the table builder

- [`fetch_ard()`](https://ichirio.github.io/tflplanner/reference/fetch_ard.md)
  [`ard_info()`](https://ichirio.github.io/tflplanner/reference/fetch_ard.md)
  [`ard_data()`](https://ichirio.github.io/tflplanner/reference/fetch_ard.md)
  : Run a report's data part and keep what its ARD holds
- [`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md)
  : What an ARD holds
- [`fill_variables()`](https://ichirio.github.io/tflplanner/reference/fill_variables.md)
  [`fill_tables()`](https://ichirio.github.io/tflplanner/reference/fill_variables.md)
  : Fill a report's definition from its ARD
- [`cell_presets()`](https://ichirio.github.io/tflplanner/reference/cell_presets.md)
  [`header_presets()`](https://ichirio.github.io/tflplanner/reference/cell_presets.md)
  : Presets for cells and column headers
- [`add_preset()`](https://ichirio.github.io/tflplanner/reference/add_preset.md)
  : Add a preset's rows to a report
- [`builder_read()`](https://ichirio.github.io/tflplanner/reference/builder_read.md)
  [`builder_write()`](https://ichirio.github.io/tflplanner/reference/builder_read.md)
  : Read and write a table the builder way
- [`builder_stats()`](https://ichirio.github.io/tflplanner/reference/builder_stats.md)
  : The rows the builder offers for a continuous variable
- [`preview_pages()`](https://ichirio.github.io/tflplanner/reference/preview_pages.md)
  [`preview_html()`](https://ichirio.github.io/tflplanner/reference/preview_pages.md)
  : The table as it will print

## Starting from the data

- [`first_table()`](https://ichirio.github.io/tflplanner/reference/first_table.md)
  : Start a summary table from the data
- [`first_listing()`](https://ichirio.github.io/tflplanner/reference/first_listing.md)
  : Start a listing from the data
- [`catalog_add_files()`](https://ichirio.github.io/tflplanner/reference/catalog_add_files.md)
  : Put a study's data files into its data catalog
- [`add_group_n()`](https://ichirio.github.io/tflplanner/reference/add_group_n.md)
  : Count the subjects per group for a report's column headers
- [`set_total_column()`](https://ichirio.github.io/tflplanner/reference/set_total_column.md)
  : A Total column for a report

## A TOC taken in

- [`toc_changes()`](https://ichirio.github.io/tflplanner/reference/toc_changes.md)
  : What taking in a TOC would change
- [`toc_apply()`](https://ichirio.github.io/tflplanner/reference/toc_apply.md)
  : Put a TOC into a study's definition
- [`toc_populations()`](https://ichirio.github.io/tflplanner/reference/toc_populations.md)
  : The analysis set each report of a TOC names
- [`toc_snapshot()`](https://ichirio.github.io/tflplanner/reference/toc_snapshot.md)
  : What a TOC said, kept for the next time one is taken in
- [`toc_imports()`](https://ichirio.github.io/tflplanner/reference/toc_imports.md)
  [`toc_last()`](https://ichirio.github.io/tflplanner/reference/toc_imports.md)
  : TOCs taken into a study

## Programs and runs

- [`data_lines()`](https://ichirio.github.io/tflplanner/reference/data_lines.md)
  : The data part of a report's program
- [`program_code()`](https://ichirio.github.io/tflplanner/reference/program_code.md)
  : The R program for one report
- [`report_setup_code()`](https://ichirio.github.io/tflplanner/reference/report_setup_code.md)
  : The study's setup of its report programs
- [`autoexec_code()`](https://ichirio.github.io/tflplanner/reference/autoexec_code.md)
  : The program that runs every report program
- [`batch_code()`](https://ichirio.github.io/tflplanner/reference/batch_code.md)
  [`autoexec_all_code()`](https://ichirio.github.io/tflplanner/reference/batch_code.md)
  : The official-run programs of a study
- [`run_study()`](https://ichirio.github.io/tflplanner/reference/run_study.md)
  : Preview a study's reports
- [`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md)
  [`list_batches()`](https://ichirio.github.io/tflplanner/reference/run_batch.md)
  : Official runs of a study
- [`batch_sets()`](https://ichirio.github.io/tflplanner/reference/batch_sets.md)
  [`set_batch()`](https://ichirio.github.io/tflplanner/reference/batch_sets.md)
  [`rename_batch()`](https://ichirio.github.io/tflplanner/reference/batch_sets.md)
  [`remove_batch()`](https://ichirio.github.io/tflplanner/reference/batch_sets.md)
  [`batch_set_problems()`](https://ichirio.github.io/tflplanner/reference/batch_sets.md)
  : Named batches of a study's reports
