# A report's rows of the ARD definition

`ard_rows()` gives the rows of an ARD definition sheet the app shows for
a report (`analyses`: that report's; the other sheets are the study's);
`set_ard_rows()` puts them back, edited.

## Usage

``` r
ard_rows(x, sheet, output_id = "")

set_ard_rows(x, sheet, output_id = "", rows)
```

## Arguments

- x:

  A `tflplanner`.

- sheet:

  `analyses`, `datasets`, `populations` or `study`.

- output_id:

  A report, or `""` / `NA` for every row.

- rows:

  The rows, edited.

## Value

`ard_rows()`: a data frame; `set_ard_rows()`: the `tflplanner`.

## Examples

``` r
x <- new_planner()
ard_rows(x, "datasets")  # the datasets of the ARD definition (none yet)
#> [1] dataset level   path    derive 
#> <0 rows> (or 0-length row.names)
ard_rows(x, "analyses", "T-1")  # a report's analyses
#>  [1] output_id     analysis_id   parent        label         method       
#>  [6] data          dataset       records       population_id where        
#> [11] by            overall       strata        groups        variables    
#> [16] optional      statistics    denominator   formats       args         
#> [21] post          code          purpose       reason       
#> <0 rows> (or 0-length row.names)
```
