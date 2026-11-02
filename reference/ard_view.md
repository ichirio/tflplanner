# An ARD as a plain table to read

An ARD as a plain table to read

## Usage

``` r
ard_view(ard)
```

## Arguments

- ard:

  An ARD.

## Value

A data frame of text: the ids, the groups, the variable and its level,
the context, the statistic's name, label and value.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba3e7f7e34 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba3e7f7e34
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba3e7f7e34/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba3e7f7e34/studies/SAMPLE-01
s <- save_study(s)
r <- run_ard(s, "T-14-1-2")
head(ard_view(r$ard), 3)  # the ARD as a plain table of text
#>   output_id analysis_id group1 group1_level variable variable_level  context
#> 1  T-14-1-2      GROUPN                       TRT01A         Drug X tabulate
#> 2  T-14-1-2      GROUPN                       TRT01A         Drug X tabulate
#> 3  T-14-1-2      GROUPN                       TRT01A         Drug X tabulate
#>   stat_name stat_label     stat stat_fmt
#> 1         n          n       80       80
#> 2         N          N      158      158
#> 3         p          % 0.506329     50.6
# }
options(old)
```
