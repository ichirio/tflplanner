# Draw a figure from its design

Runs a design's script on the study's data, as the figure's program will
(in a temporary folder, so nothing of the study changes), and gives the
PNG it saves – at the figure's own size – with the design's problems and
the figure checks. The Plot Designer's preview.

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
