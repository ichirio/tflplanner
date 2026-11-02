# The company's setup code

The code that opens every study's `programs/study_setup.R` (its part 1):
the company standards' sheet `setup_code`, one line of R a row
([`company_standards()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)),
with `{STUDY_ID}` filled in. A new study gets a copy; a later change to
the standards does not change a study made before.

## Usage

``` r
setup_code(study_id = NA, standards = company_standards())
```

## Arguments

- study_id:

  The study's id (`{STUDY_ID}`); `NA` leaves it.

- standards:

  The standards
  ([`company_standards()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)).

## Value

The code, one element per line.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba6fdbbab3 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba6fdbbab3
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba6fdbbab3/studies
setup_code("ABC-1")  # the company's lines, with the study's id in
#> [1] "library(cards)"            "library(dplyr)"           
#> [3] "library(rtfreporter)"      "options(warn = 1)"        
#> [5] "days_per_week <- 7"        "days_per_month <- 30.4375"
#> [7] "days_per_year <- 365.25"  
options(old)
```
