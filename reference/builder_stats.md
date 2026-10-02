# The statistics the builder offers for a continuous variable

Each has its row label, its template and its digits as a function of the
decimals the data are collected with (`d`): the usual convention is the
mean and median one more, the SD two more, the extremes as collected.

## Usage

``` r
builder_stats()
```

## Value

A data frame: `key`, `row`, `template`, `digits` (a function of `d`).
