# Named batches of a study's reports

A named batch is a set of reports an official run can take by name
([`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md)`(batch = )`).
A report's batches are the report list's `batches` column
(`"Topline | Final"`).

## Usage

``` r
batch_sets(x)

set_batch(x, name, output_ids)

rename_batch(x, from, to)

remove_batch(x, name)

batch_set_problems(x)
```

## Arguments

- x:

  A `tflplanner`.

- name, from, to:

  A batch's name.

- output_ids:

  The reports of the batch.

## Value

`batch_sets()`: a named list of output ids. `set_batch()`,
`rename_batch()`, `remove_batch()`: the `tflplanner`.
`batch_set_problems()`: a data frame `batch`, `problem`.

## Details

- `batch_sets()`: every name and its reports, in the report list's
  order.

- `set_batch()`: the batch `name` is these reports (and no other): a new
  name, or one overwritten.

- `rename_batch()`, `remove_batch()`: the name changed, or taken off
  every report (the reports stay).

- `batch_set_problems()`: what an official run would trip on: a name a
  batch folder cannot carry, a batch of no report.

## Examples

``` r
p <- add_output(add_output(new_planner(), "DM"), "AE")
p <- set_batch(p, "Topline", "DM")
p <- set_batch(p, "Final", c("DM", "AE"))
batch_sets(p)
#> $Final
#> [1] "AE" "DM"
#> 
#> $Topline
#> [1] "DM"
#> 
```
