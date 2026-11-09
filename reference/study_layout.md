# The folder layout of a study

The folder layout of a study

## Usage

``` r
study_layout()
```

## Value

A named character vector of folders, relative to the study folder.

## Examples

``` r
study_layout()["adam"]
#>        adam 
#> "data/adam" 
names(study_layout())
#>  [1] "adam"         "sdtm"         "other"        "spec"         "programs_ard"
#>  [6] "programs_tfl" "ard"          "tfl"          "ard_import"   "toc_import"  
#> [11] "runs"         "logs_preview" "docs"        
```
