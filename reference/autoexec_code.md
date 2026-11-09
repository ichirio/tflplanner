# The program that runs every report program

`programs/tfl/autoexec_report.R` runs from the study folder. It runs the
programs in the order of the report list – or only the ones named on its
command line (`Rscript programs/tfl/autoexec_report.R DM.R AE.R`) – each
in its own `Rscript` process with the study folder as its working
directory and its log in `logs/`, and ends with a table of what passed
(`logs/autoexec_report.csv`).

## Usage

``` r
autoexec_code(x, date = Sys.Date())
```

## Arguments

- x:

  An `tflplanner`.

- date:

  The date stamped in the banner.

## Value

The program, one element per line.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1838d5ebda is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1838d5ebda
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1838d5ebda/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b1838d5ebda/studies/SAMPLE-01
head(autoexec_code(s$planner), 6)
#> [1] "# ============================================================================"                
#> [2] "#  Program    : programs/tfl/autoexec_report.R"                                                
#> [3] "#  Official run of the report programs, in the report list's order."                           
#> [4] "#    Rscript programs/tfl/autoexec_report.R                  every program"                    
#> [5] "#    Rscript programs/tfl/autoexec_report.R T-14-1-1.R       only these"                       
#> [6] "#    Rscript programs/tfl/autoexec_report.R --no-code        the batch folder without the code"
# }
options(old)
```
