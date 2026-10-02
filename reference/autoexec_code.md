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
