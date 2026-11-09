# Make, rename or remove a page pattern

A new pattern starts as Standard (no difference: one empty row of the
report sheet, `output_id` `@<name>`). Renaming moves its rows and the
reports' choice of it; a pattern a report uses is not removed.

## Usage

``` r
add_page_pattern(x, name)

rename_page_pattern(x, name, new)

remove_page_pattern(x, name)
```

## Arguments

- x:

  An `tflplanner`.

- name, new:

  A pattern's name: a letter, then letters, digits or `_`.

## Value

`x`.

## Examples

``` r
x <- add_page_pattern(new_planner(), "landscape")
x <- rename_page_pattern(x, "landscape", "wide")
page_patterns(x)
#> [1] "wide"
x <- remove_page_pattern(x, "wide")
page_patterns(x)
#> character(0)
```
