# Add, change or remove an analysis data of the ARD definition

The analysis data are named data the analyses read (tflspec's sheet
`analysis_data`): made `from` a dataset or an analysis data above, kept
to a population's subjects and the records `where` keeps, with columns
of the population's data added (`add`), columns derived (`derive`) and
one row per set of values (`distinct`) – or, when the columns cannot say
it, made by R of its own (`code`, the other columns but `from` blank).
An analysis names one in `data` (instead of `dataset` /
`population_id`), or as its `denominator`.

## Usage

``` r
set_analysis_data(
  x,
  output_id,
  data_id,
  from,
  population_id = NA,
  where = NA,
  add = NA,
  derive = NA,
  distinct = NA,
  label = NA,
  old = NULL,
  subjects = NA,
  keep = NA,
  code = NA
)

remove_analysis_data(x, output_id, data_id)

import_analysis_data(x, from_output, output_id, data_ids = NULL)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- data_id:

  The name (lower case; also the object's name in the ARD program).

- from:

  A dataset, or an analysis data above.

- population_id, subjects, where, add, derive, keep, distinct, label:

  The other columns (`subjects`: an analysis data above whose subjects
  it keeps, instead of `population_id`; `add`, `keep`, `distinct`:
  several columns with `" | "` between them).

- old:

  The name of the one to replace; `NULL`: a new one.

- code:

  R that makes the data itself (its value is the data); with it the
  other columns but `from` stay blank.

- from_output:

  The report whose analysis data are copied.

- data_ids:

  Which of them (`NULL`: all), with what they are made from and kept to,
  when those are analysis data as well.

## Value

The `tflplanner`.

## Details

`set_analysis_data()` adds one, or replaces the one named `old` (a new
name is followed in the analyses and the analysis data made from it).
`remove_analysis_data()` takes one out, refused while it is used.

`import_analysis_data()` copies a report's analysis data into another
report, under the same names (a name the report has already gets `_1`,
`_2` ...); attribute `copied` names the rows added.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba399f3822 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba399f3822
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba399f3822/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba399f3822/studies/SAMPLE-01
# the subjects aged 50 or more: a new analysis data made from ADSL
x <- set_analysis_data(s$planner, "T-14-1-1", "adsl_old", from = "adsl", where = "AGE >= 50")
ard_rows(x, "analysis_data", "T-14-1-1")[, c("data_id", "from", "where")]
#>             data_id  from                                          where
#> 1          adsl_saf  ADSL                                           <NA>
#> 2          adsl_old  adsl                                      AGE >= 50
#> 3          adsl_saf  ADSL                                           <NA>
#> 4          adsl_saf  ADSL                                           <NA>
#> 5          adsl_saf  ADSL                                           <NA>
#> 6         adsl_scrf  ADSL                                           <NA>
#> 7          adsl_enr  ADSL                                           <NA>
#> 8        adsl_enr_1  ADSL                                           <NA>
#> 9          adsl_enr  ADSL                                           <NA>
#> 10       adsl_enr_1  ADSL                                           <NA>
#> 11         adsl_saf  ADSL                                           <NA>
#> 12   advs_saf_sysbp  ADVS PARAMCD == "SYSBP" & AVISIT == "CYCLE 9 DAY 1"
#> 13         adsl_saf  ADSL                                           <NA>
#> 14 adtte_saf_ttskin ADTTE                            PARAMCD == "TTSKIN"
#> 15         adsl_saf  ADSL                                           <NA>
#> 16   advs_saf_sysbp  ADVS PARAMCD == "SYSBP" & AVISIT == "CYCLE 9 DAY 1"
#> 17 adtte_saf_ttskin ADTTE                            PARAMCD == "TTSKIN"
#> 18         adsl_saf  ADSL                                           <NA>
#> 19         adsl_saf  ADSL                                           <NA>
#> 20         adae_saf  ADAE                                 TRTEMFL == "Y"
# }
options(old)
```
