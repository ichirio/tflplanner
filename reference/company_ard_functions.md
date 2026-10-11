# The company's own ARD functions

The functions defined in the R files of the standards folder's
`ard_functions/` (`<home>/standards/ard_functions/*.R`), read – not run
– with
[`tflspec::tfl_ard_function_info()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_info.html):
a plain `name <- function` and one written as
[`cards::as_cards_fn()`](https://pharmaverse.github.io/cards/latest-tag/reference/as_cards_fn.html)
(the templates of
[`tflspec::tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.html))
alike; test files (`test-*.R`) are not read. An analysis names one as
its `method` once the study uses it
([`use_company_ard_function()`](https://ichirio.github.io/tflplanner/reference/use_company_ard_function.md)).

## Usage

``` r
company_ard_functions(home = tflplanner_home())
```

## Arguments

- home:

  The tflplanner home.

## Value

A data frame, one row a function: `name`, `file`, `title`, `description`
(from the roxygen block above it), `stat_names` (the statistics it
declares, `|` between them), `keywords` (the words the ARD form's search
finds it by: the comment lines
`# tflplanner-keywords: odds ratio, PROC LOGISTIC` right above it;
`", "` between them).
