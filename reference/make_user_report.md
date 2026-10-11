# Make a figure written by hand a user-code report

A figure with no design whose plot is its data code becomes a report of
the type `user`: the same datasets, the same code, with
`content <- plot` added when the code leaves `plot` (and no `content`).
Its program is then written again (it is "to be made again").

## Usage

``` r
make_user_report(x, output_id)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The figure.

## Value

The `tflplanner`.
