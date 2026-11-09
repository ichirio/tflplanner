# Review a study's definition

`study_review()` lists, for the whole study or some reports, what is
wrong with the definition (`error`), what is valid but probably wrong
(`check`) and what is missing and has to be set by hand (`hand`): the
rules of
[`tflspec::tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.html),
those of the report list and the study folder among them.
`data = "cached"` checks against the facts of the data kept from the
last read and reads no file; `"read"` reads the datasets whose file or
definition changed since; `"none"` reviews the definition alone.
`review_problems()` reviews a definition with no study (the planner
alone: what an import's draft and the tests call). `review_facts()`
gives the facts the review checks against.

## Usage

``` r
study_review(
  study,
  output_id = NULL,
  data = c("cached", "read", "none"),
  ard = TRUE,
  deep = FALSE,
  home = tflplanner_home(),
  lang = tflplanner_language(),
  progress = NULL
)
```

## Arguments

- study:

  An `rtfstudy`
  ([`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)).

- output_id:

  Only these reports; `NULL` reviews them all and the study-wide rows.

- data:

  `"cached"`, `"read"` or `"none"`: see above.

- ard:

  `TRUE` also checks each report's table against what the study ARD
  holds, and lists what went wrong while it was made.

- deep:

  `TRUE` also reads the definition back as the report programs will
  ([`check_planner()`](https://ichirio.github.io/tflplanner/reference/check_planner.md)):
  slow, one read a report.

- home:

  The tflplanner home.

- lang:

  The language of the messages (the app's by default).

- progress:

  A function called with each dataset's name before its file is read
  (`data = "read"`), or `NULL`.

## Value

A `tflspec::tfl_review`, its `message` in the app's language and
`message_en` in English; `attr(, "facts_made")`: when the facts of the
data were made (`NA`: none were used); `attr(, "off")`: the rules the
company standards leave out (their settings' `review_off`, `A12 | C02`),
whose rows are not there.

## See also

[`tflspec::tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.html),
[`tflspec::tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.html)

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1831d4bbb2 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1831d4bbb2
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1831d4bbb2/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b1831d4bbb2/studies/SAMPLE-01
# the definition alone, no data read
study_review(s, data = "none")
#> <tfl_review> 0 errors, 4 to check, 0 to set by hand
#>   check F02  F-14-2-3     design | layers: A KM figure usually shows the number at risk below the curves.
#>   check F02  F-14-2-3     design | plot: Set X max and X step, so the breaks (and the number at risk's columns) fall on round times.
#>   check F07  F-14-2-3     report |  | ard_source: the ARD of T-14-2-2 is not made yet: make it first (its step 2)
#>   check F07  F-14-2-4     report |  | ard_source: the ARD of F-14-2-4 is not made yet: make it first (its step 2)
# }
options(old)
```
