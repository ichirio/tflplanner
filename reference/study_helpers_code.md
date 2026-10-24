# The functions a study's programs call

`programs/study_helpers.R`: the code of the functions the generated
programs call as they run – `set_levels()` (a report's code lists on its
data: each listed column a factor in the list's order, its values the
labels; a value not listed stops the program), `tag_ard()`, `fmt_ard()`,
`fmt_pvalue()`, `keep_stats()` and `save_ard()` – as tflspec gives them
([`tflspec::tfl_helpers_code()`](https://ichirio.github.io/tflspec/reference/tfl_helpers_code.html)).
The study's setup sources it, so its programs run without tflspec.
Saving the study writes it again when tflspec's have changed (one edited
by hand is copied to `programs/.edited/` first).

## Usage

``` r
study_helpers_code(date = Sys.Date())
```

## Arguments

- date:

  The date stamped in the banner.

## Value

The code, one element per line.
