# Files in a study folder

Files in a study folder

## Usage

``` r
study_files(study, folder = "data")
```

## Arguments

- study:

  An `rtfstudy`.

- folder:

  A folder of
  [`study_layout()`](https://ichirio.github.io/tflplanner/reference/study_layout.md)
  (`"adam"`, `"tfl"`, ...), or `"data"` for all three data folders.

## Value

A data frame: `folder`, `file`, `size_kb`, `modified`, `path`.
