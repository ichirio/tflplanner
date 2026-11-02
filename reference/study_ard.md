# One report's ARD, wherever it comes from

The rows a report's table reads: from the ARD taken in when the report
row says `ard_source = import:<file>`, else from the study ARD the ARD
definition makes (`output/ard/ard.rds`). The id columns are dropped, as
[`tflspec::tfl_ard_for()`](https://ichirio.github.io/tflspec/reference/tfl_ard_for.html)
does.

## Usage

``` r
study_ard(study, output_id)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report.

## Value

The ARD, or `NULL` when there is none yet.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba50a5dc7e is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba50a5dc7e
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba50a5dc7e/studies
# \donttest{
# (a few seconds: it writes and runs the ARD program)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba50a5dc7e/studies/SAMPLE-01
s <- save_study(s)
update_study_ard(s, "T-14-1-2")$ok  # makes the report's rows of the study ARD
#> [1] TRUE
head(study_ard(s, "T-14-1-2"), 3)
#> # An ARD data frame: 3 × 12
#>   variable variable_level context stat_name    stat stat_fmt group1 group1_level
#>   <chr>    <list>         <chr>   <chr>      <list> <list>   <chr>  <list>      
#> 1 TRT01A   Drug X         tabula… n          80     80       NA     <NULL>      
#> 2 TRT01A   Drug X         tabula… N         158     158      NA     <NULL>      
#> 3 TRT01A   Drug X         tabula… p           0.506 50.6     NA     <NULL>      
#> # ℹ 4 more variables: stat_label <chr>, fmt_fun <list>, warning <list>,
#> #   error <list>
# }
options(old)
```
