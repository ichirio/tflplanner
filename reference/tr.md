# The app's languages

`tr()` translates an English text of the app; `tflplanner_language()` is
the language set with
[`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)
(English by default).

## Usage

``` r
tr(x, lang = tflplanner_language())

tflplanner_language()

app_languages()
```

## Arguments

- x:

  English text.

- lang:

  `"en"` or `"ja"`.

## Value

The text in `lang`.
