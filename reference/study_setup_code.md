# The study's setup program

`study_setup_code()` is a new study's `programs/study_setup.R`, which
`programs/ard/ard_setup.R`, `programs/tfl/report_setup.R` and
`programs/tfl/fig_setup.R` each source first, so every ARD, report and
figure program runs it. It is one file of three marked parts, run in
this order:

## Usage

``` r
study_setup_code(meta, date = Sys.Date(), standards = company_standards())
```

## Arguments

- meta:

  The study's fields (`study_id`, `title`, ...), as an `rtfstudy`'s
  `meta`.

- date:

  The date the marker of part 1 says it was copied.

- standards:

  The standards to copy part 1 from.

## Value

The program, one element per line.

## Details

1.  the company standard:
    [`setup_code()`](https://ichirio.github.io/tflplanner/reference/setup_code.md),
    copied when the study is made (its marker line says when, and which
    standards); tflplanner does not change it after.

2.  the study: its folders
    ([`study_layout()`](https://ichirio.github.io/tflplanner/reference/study_layout.md):
    `path_adam`, `path_sdtm` ...) and what it is (`study_id`,
    `study_title`, `study_short_title`, `study_compound`, `study_phase`,
    `study_description` from `study.yml`). tflplanner writes it again on
    every save.

3.  the study's own code: tflplanner never touches it.

Saving a study rewrites part 2 only, and parts 1 and 3 stay byte for
byte. A part 2 edited by hand (its checksum no longer matches) is
written again too, the edited file first put in `programs/.edited/`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1856128538 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1856128538
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1856128538/studies
meta <- list(study_id = "ABC-1", title = "A study of Drug X")
head(study_setup_code(meta), 8)
#> [1] "# ==== tflplanner: company standard (copied 2026-11-09, standards tflplanner draft standards 0.1 of 2026-09-27) ===="
#> [2] "library(cards)"                                                                                                      
#> [3] "library(dplyr)"                                                                                                      
#> [4] "library(rtfreporter)"                                                                                                
#> [5] "options(warn = 1)"                                                                                                   
#> [6] "days_per_week <- 7"                                                                                                  
#> [7] "days_per_month <- 30.4375"                                                                                           
#> [8] "days_per_year <- 365.25"                                                                                             
options(old)
```
