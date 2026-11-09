# Where a report's program and RTF go

Where a report's program and RTF go

## Usage

``` r
report_info(x, output_id)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  A report id.

## Value

A list: `type`, `program` (file name, with `.R`), `file` (the RTF,
joined to `output_path`).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b186632a513 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b186632a513
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b186632a513/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b186632a513/studies/SAMPLE-01
report_info(s$planner, "T-14-1-1")  # the program and the RTF of a report
#> $type
#> [1] "table"
#> 
#> $program
#> [1] "T-14-1-1.R"
#> 
#> $file
#> [1] "output/tfl/T-14-1-1.rtf"
#> 
# }
options(old)
```
