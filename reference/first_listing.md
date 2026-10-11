# Start a listing from the data

Writes what a listing needs from the answers of the app's "first
listing" form: the dataset in the data catalog (added when missing), the
report (a Listing, added when missing), its `listings` row (the
company's listing type; the order: the group, the subject, the start
date when the data have one) and one `listing_cols` row per column, in
order, headed by the data's label (a repeated group or subject printed
once).

## Usage

``` r
first_listing(
  x,
  output_id,
  path,
  data,
  columns,
  group = NULL,
  description = NA_character_
)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- path:

  The data file, relative to the study folder.

- data:

  Its data (the columns' labels).

- columns:

  The columns, in order.

- group:

  A group column the listing is ordered by first, or `NULL`.

- description:

  The report's description.

## Value

The `tflplanner`.
