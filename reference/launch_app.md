# Start tflplanner in its own R process

Starts the app the way its shortcut does
([`add_shortcut()`](https://ichirio.github.io/tflplanner/reference/add_shortcut.md))
– in a separate R process – and opens it in the browser, so the R
console (or RStudio) stays free. If the app already runs on the port, it
opens that one. Closing the browser stops it. This is the RStudio add-in
"Launch tflplanner".

## Usage

``` r
launch_app(port = NULL)
```

## Arguments

- port:

  The port; `NULL` uses the setting (default 7470).

## Value

The app's address, invisibly.

## See also

[`run_app()`](https://ichirio.github.io/tflplanner/reference/run_app.md)
to run the app in this R session.

## Examples

``` r
if (interactive()) {
  launch_app()
}
```
