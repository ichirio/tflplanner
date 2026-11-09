# The ARD functions of one's own a study can use

The study's
([`study_ard_functions()`](https://ichirio.github.io/tflplanner/reference/study_ard_functions.md))
and the company's
([`company_ard_functions()`](https://ichirio.github.io/tflplanner/reference/company_ard_functions.md))
side by side: one row a function name. A study's copy wins; when both
have one, whether their files differ and which is newer. Where the
study's analyses use it, and the last time it was tried
([`try_ard_function()`](https://ichirio.github.io/tflplanner/reference/try_ard_function.md)).

## Usage

``` r
own_ard_functions(study, home = tflplanner_home())
```

## Arguments

- study:

  An `rtfstudy`.

- home:

  The tflplanner home.

## Value

A data frame: `name`, `where` (`"study"`, `"company"`, `"both"`),
`study_file`, `company_file`, `loaded`, `differs`, `newer` (`"study"`,
`"company"` or `NA`), `used_by` (`"T1 / RD | ..."`), `title`,
`description`, `stat_names`, `keywords`, `tried` (when), `problems`
(errors and warnings found), `stale` (the file changed since).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b186659c028 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b186659c028
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b186659c028/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b186659c028/studies/SAMPLE-01
own_ard_functions(s)[, c("name", "where", "used_by")]
#>             name where used_by
#> 1 ard_cv_geomean study    <NA>
# }
options(old)
```
