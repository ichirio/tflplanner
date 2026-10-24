# Copy how a study reads its TOC to another study

Copies the study's TOC profile (`input/toc/profile.yml`: the rule set,
the way the report IDs are made, the sheets and header rows, the phase
settings) to another study, as copying a study in the app does. Not the
TOCs it took in nor what they said (`last.json`): the other study's TOC
is its own, and its first one is compared with its own lines.

## Usage

``` r
toc_copy_profile(from, to)
```

## Arguments

- from, to:

  `rtfstudy` objects
  ([`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)).

## Value

`TRUE` when there was a profile to copy, invisibly.
