# Check the definition the way the report programs will read it

Writes the workbooks to a temporary folder and reads each report back
with
[`tflspec::tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.html)
narrowed to it, the call every report program makes. (What needs the
data – a column the ARD lacks – shows only when the program runs.)

## Usage

``` r
check_planner(x)
```

## Arguments

- x:

  An `tflplanner`.

## Value

A data frame: `output_id`, `ok`, `message`.
