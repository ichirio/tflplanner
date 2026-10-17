# The last try of each of a study's own ARD functions

The last try of each of a study's own ARD functions

## Usage

``` r
own_function_checks(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A list by function name: `when`, `user`, `md5` (of the file tried – not
of other files it may use: a change there is not seen), `data`, `args`,
`problems` (errors and warnings found), `errors`, `warnings`, `notes`,
`rows` (of the whole ARD it gave), `versions` (tflspec, cards, cardx). A
company's function tried before the study uses it is kept here too: once
it is copied, the record holds (the files are the same).
