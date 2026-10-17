# The first rows of a data file

Reads `.rds`, `.rda` / `.RData` (one dataset a file), `.csv`, `.xpt`,
`.sas7bdat` (haven) and `.parquet` (arrow).

## Usage

``` r
read_data_head(path, n = 50L)
```

## Arguments

- path:

  A file.

- n:

  Rows to return.

## Value

A data frame, with the full dimensions in attribute `dim_full`.
