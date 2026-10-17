# Stop using an ARD taken in

Marks it `"removed"` in the record
([`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md)):
the file stays in `input/ard/` and on the record (what a report was made
from is not forgotten), and a report still pointing at it is named in an
error when its program runs.

## Usage

``` r
remove_imported_ard(study, import_id)
```

## Arguments

- study:

  An `rtfstudy`.

- import_id:

  The import.

## Value

The record, invisibly.
