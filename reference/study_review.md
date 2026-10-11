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

review_problems(
  x,
  output_id = NULL,
  facts = NULL,
  lang = tflplanner_language()
)

review_facts(
  study,
  refresh = FALSE,
  read = TRUE,
  home = tflplanner_home(),
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

- x:

  A planner
  ([`new_planner()`](https://ichirio.github.io/tflplanner/reference/new_planner.md),
  [`read_planner()`](https://ichirio.github.io/tflplanner/reference/read_planner.md)).

- facts:

  The facts of the data (`review_facts()`), or `NULL`.

- refresh:

  `TRUE` makes the facts again whatever is kept.

- read:

  `FALSE` reads no file: the facts kept, the datasets with none listed
  in `attr(, "not_read")`.

## Value

A `tflspec::tfl_review`, its `message` in the app's language and
`message_en` in English; `attr(, "facts_made")`: when the facts of the
data were made (`NA`: none were used); `attr(, "off")`: the rules the
company standards leave out (their settings' `review_off`, `A12 | C02`),
whose rows are not there.

## See also

[`tflspec::tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.html),
[`tflspec::tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.html)
