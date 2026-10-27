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
