# Figure presets

A preset is a figure design kept with the company standards, to start a
figure from as a template is: a KM figure as the company draws it, the
mean-over-time figure of a study type ... `fig_presets()` lists them;
`save_fig_preset()` keeps a design as one (a `.yml` in the home's
`standards/figure-presets`); `read_fig_preset()` gives it back;
`remove_fig_preset()` drops it.

## Usage

``` r
fig_presets(home = tflplanner_home())

save_fig_preset(design, name, description = "", home = tflplanner_home())

read_fig_preset(name, home = tflplanner_home())

remove_fig_preset(name, home = tflplanner_home())
```

## Arguments

- home:

  tflplanner's home.

- design:

  A `tfl_fig_design`.

- name:

  The preset's name (its file's).

- description:

  A line on what it is.

## Value

`fig_presets()`: a data frame (`name`, `description`, `template`,
`file`); `read_fig_preset()`: a `tfl_fig_design`; the others: the file,
invisibly.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba6d697f4f is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba6d697f4f
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba6d697f4f/studies
# \donttest{
# (a few seconds: it makes the sample study)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba6d697f4f/studies/SAMPLE-01
fig_presets()  # none yet
#> [1] name        description template    file       
#> <0 rows> (or 0-length row.names)
save_fig_preset(fig_design(s$planner, "F-14-2-3"), "km-company",
                "KM figure as the company draws it")
fig_presets()[c("name", "description")]
#>         name                       description
#> 1 km-company KM figure as the company draws it
# }
options(old)
```
