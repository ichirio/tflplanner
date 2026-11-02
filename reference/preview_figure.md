# Draw a figure from its design

Runs a design's script on the study's data, as the figure's program will
(in a temporary folder, so nothing of the study changes), and gives the
PNG it saves – at its size on the page (step 3) – with the design's
problems and the figure checks. The Plot Designer's preview.

## Usage

``` r
preview_figure(
  study,
  output_id,
  design = fig_design(study$planner, output_id),
  max_px = NULL
)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The figure.

- design:

  The design (default: the figure's).

- max_px:

  For the screen: at most this many pixels wide (the PNG is drawn at a
  lower dpi, same size); `NULL` draws it as saved.

## Value

A list: `png` (the file, `NULL` when it failed), `size` (its `width`,
`height`, `units` and `dpi`), `advice` (what is usually wanted and
missing, see
[`tflspec::tfl_fig_advice()`](https://ichirio.github.io/tflspec/reference/tfl_fig_advice.html)),
`problems` (the design against the schema and the data, see
[`tflspec::tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.html)),
`warnings` (the figure checks and the plot's own warnings), `error`
(`NULL` or the message), `code`, `objects` (what the data steps made,
each in short: see below).

`objects` is a list by name (`df`, then each object a step makes: a KM
fit, summary statistics, the ARD's statistics ...), each a list:
`class`; for a data frame `rows`, `columns` and `head` (its first 6
rows), for anything else `text` (the first lines it prints). Those made
before an error are there.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba37f9ab0 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba37f9ab0
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba37f9ab0/studies
# \donttest{
# (about 10 seconds: it makes the sample study, its ARD and the figure)
s <- save_study(create_sample_study(run = FALSE))
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba37f9ab0/studies/SAMPLE-01
update_study_ard(s, "T-14-2-2")  # the ARD the figure reads
#> $ok
#> [1] TRUE
#> 
#> $rows
#> [1] 117
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
#> [12] "T-14-2-2: 117 rows into output/ard/ard.rds"            
#> 
p <- preview_figure(s, "F-14-2-3")
p$png      # the picture, in a temporary folder
#> [1] "/tmp/RtmphQvIXi/tflplanner-fig-1aba5fabaea5/output/F-14-2-3.png"
p$size     # its size on the page
#> $width
#> [1] 8.33
#> 
#> $height
#> [1] 4.79
#> 
#> $units
#> [1] "in"
#> 
#> $dpi
#> [1] 300
#> 
# }
options(old)
```
