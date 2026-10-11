# Copy the definition files out, and bring an edited copy back

The usual way to edit a study's definition outside tflplanner (#274):
copy the files out with `export_spec_files()`, edit the copy (in Excel,
renamed as you like), and bring it back with `import_spec_files()`.
(Editing `spec/` directly works too:
[`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
takes it in.)

## Usage

``` r
export_spec_files(study, path, home = tflplanner_home())

preview_spec_import(study, path, home = tflplanner_home())

import_spec_files(study, path, parts = NULL, home = tflplanner_home())
```

## Arguments

- study:

  A registered study's id, or an `rtfstudy`.

- path:

  `export_spec_files()`: a folder, or a `.zip` file, to write.
  `preview_spec_import()` / `import_spec_files()`: what to bring back –
  a folder (a study folder, its `spec/`, or the files), a `.zip`, or one
  file. A file is known by what it holds, not its name: a workbook by
  its sheets, `.json` as the ARD definition, a `.yml` with a `study_id`
  as `study.yml`, another `.yml` as the figure design of the report its
  name starts with.

- home:

  tflplanner's home.

- parts:

  The parts to take in (`preview_spec_import()$parts` names them);
  `NULL`: every part that differs.

## Value

`export_spec_files()`: the paths written. `preview_spec_import()`: a
list – `parts` (what differs), `labels`, `outputs` (the reports
touched), `problems`, `ok` (no error). `import_spec_files()`: the study,
with `backup` (the folder of the copy made before).

## Details

`preview_spec_import()` says what an import would do, changing nothing:
the copy is read in a temporary folder – every reader's check, every
cell that holds R parsed, tflspec's checks of the ARD definition and the
figure designs, and the programs of the reports it touches written and
parsed – and compared with the study part by part.

`import_spec_files()` then takes in the parts chosen (all that differ,
unless `parts` says which). Before it changes anything it copies the
study's definition files as they are to `spec/.backup/<time>/`; after,
the study is saved (its files written from it). A copy with errors
changes nothing (class `tflplanner_spec_invalid`, its `problems` the
file, sheet, row, column and message of each); warnings do not stop it.
