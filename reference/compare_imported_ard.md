# Compare a report's own ARD with the one taken in

Double programming: the report's rows of the study ARD (from its ARD
definition) against the ARD taken in for it, by
[`cards::compare_ard()`](https://pharmaverse.github.io/cards/latest-tag/reference/compare_ard.html).

## Usage

``` r
compare_imported_ard(study, output_id, file = NULL)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report.

- file:

  The ARD taken in (a `file` of
  [`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md));
  `NULL`: the one the report row names.

## Value

The comparison
([`cards::compare_ard()`](https://pharmaverse.github.io/cards/latest-tag/reference/compare_ard.html));
[`cards::is_ard_equal()`](https://pharmaverse.github.io/cards/latest-tag/reference/compare_ard.html)
says whether they agree.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1abac9929b0 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1abac9929b0
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1abac9929b0/studies
# \donttest{
# (a few seconds: it writes and runs the ARD program)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1abac9929b0/studies/SAMPLE-01
s <- save_study(s)  # the report's ARD program is written
r <- run_ard(s, "T-14-1-2")  # the report's ARD, made in its own R process
f <- tempfile(fileext = ".rds")
saveRDS(r$ard, f)  # (here the same ARD stands for one made elsewhere)
imp <- import_ard(s, f, output_id = "T-14-1-2", source = "CRO")
update_study_ard(s, "T-14-1-2")  # the report's own ARD
#> $ok
#> [1] TRUE
#> 
#> $rows
#> [1] 14
#> 
#> $error
#> NULL
#> 
#> $output
#>  [1] ""                                                      
#>  [2] "Attaching package: ‘dplyr’"                            
#>  [3] ""                                                      
#>  [4] "The following objects are masked from ‘package:stats’:"
#>  [5] ""                                                      
#>  [6] "    filter, lag"                                       
#>  [7] ""                                                      
#>  [8] "The following objects are masked from ‘package:base’:" 
#>  [9] ""                                                      
#> [10] "    intersect, setdiff, setequal, union"               
#> [11] ""                                                      
#> [12] "T-14-1-2: 14 rows into output/ard/ard.rds"             
#> 
s$planner <- use_imported_ard(s$planner, "T-14-1-2", imp$file)
if (requireNamespace("cards", quietly = TRUE)) {
  cards::is_ard_equal(compare_imported_ard(s, "T-14-1-2"))  # double programming
}
#> [1] TRUE
# }
options(old)
```
