# Replace a study's copy of an ARD function with the company's

The study's file of that function is overwritten with the company's
(when the company's is newer, say). The study's results change only when
its ARD is made again.

## Usage

``` r
take_company_ard_function(study, name, home = tflplanner_home())
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function.

- home:

  The tflplanner home.

## Value

The study file's path, invisibly.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18f61bf94 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18f61bf94
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18f61bf94/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18f61bf94/studies/SAMPLE-01
# a function in the company standards, and a copy of it in the study
s <- new_ard_function(s, "ard_my_mean", where = "company", test = FALSE)
own <- own_ard_functions(s)
file.copy(own$company_file, file.path(s$path, "programs/ard/functions"))
#> [1] FALSE  TRUE
own_ard_functions(s)[, c("name", "where")]
#>             name where
#> 1 ard_cv_geomean study
#> 2    ard_my_mean  both
# the study's copy is overwritten with the company's
take_company_ard_function(s, "ard_my_mean")
# }
options(old)
```
