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
