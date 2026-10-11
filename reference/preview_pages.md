# The table as it will print

Plans the report's table from the definition as it stands (saved or not)
and the report's normalized data, and gives its pages – what
[`tflspec::tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.html)
lays out – or an HTML rendering of them.

## Usage

``` r
preview_pages(x, output_id, data)

preview_html(pages, max_pages = 3L, align = "center")
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report.

- data:

  Its normalized data
  ([`ard_data()`](https://ichirio.github.io/tflplanner/reference/fetch_ard.md)).

- pages:

  `rtftable` pages.

- max_pages:

  How many pages to render.

- align:

  How value cells align: `"center"` (a table) or `"left"` (a listing).

## Value

`preview_pages()`: a list of `rtftable`; `preview_html()`: HTML.
