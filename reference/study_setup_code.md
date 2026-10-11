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
    `study_title`, `study_compound`, `study_phase`, `study_description`
    from `study.yml`). tflplanner writes it again on every save.

3.  the study's own code: tflplanner never touches it.

Saving a study rewrites part 2 only, and parts 1 and 3 stay byte for
byte. A part 2 edited by hand (its checksum no longer matches) is
written again too, the edited file first put in `programs/.edited/`.
