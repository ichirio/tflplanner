# Set up tflplanner's home

Creates the folder where tflplanner keeps its settings and the saved
state of every study, and says where new study folders go. Run it once;
[`run_app()`](https://ichirio.github.io/tflplanner/reference/run_app.md)
runs it with the defaults when it finds no home. Running it again
changes the settings given and keeps the rest.

## Usage

``` r
setup_tflplanner(
  home = NULL,
  studies_root = NULL,
  language = NULL,
  standards = NULL,
  sample = FALSE,
  port = NULL,
  check_updates = NULL
)
```

## Arguments

- home:

  The home folder. `NULL` keeps the current one (see
  [`tflplanner_home()`](https://ichirio.github.io/tflplanner/reference/tflplanner_home.md));
  a folder given here is remembered for later sessions – one in the
  temporary folder only for this session.

- studies_root:

  Where new study folders are created. `NULL` keeps the current setting,
  or on a first setup uses `<home>/workspace`.

- language:

  The app's language, `"en"` (the default) or `"ja"`; `NULL` keeps the
  current setting.

- standards:

  A company standards workbook
  ([`standards_template()`](https://ichirio.github.io/tflplanner/reference/standards_template.md))
  to install, `"builtin"` to go back to the built-in draft, or `NULL` to
  keep what is installed.

- sample:

  `TRUE` adds the sample study (SAMPLE-01) to the studies folder and
  makes its ARD and reports
  ([`create_sample_study()`](https://ichirio.github.io/tflplanner/reference/create_sample_study.md));
  a sample already there is left as it is.

- port:

  The port the app runs on when started from its shortcut or
  [`launch_app()`](https://ichirio.github.io/tflplanner/reference/launch_app.md)
  (default 7470); `NULL` keeps the setting.

- check_updates:

  Whether the app looks for a newer version when it starts (default
  `FALSE`; it only says so, it installs nothing); `NULL` keeps the
  setting. tflplanner is updated only when you ask:
  [`update_tflplanner()`](https://ichirio.github.io/tflplanner/reference/update_tflplanner.md),
  or the "update and launch" shortcut (`add_shortcut(update = TRUE)`).

## Value

The settings, invisibly.

## Details

**Called with no arguments in an interactive session**, it walks you
through the setup step by step, asking before it changes anything:

1.  the home folder and where new study folders go;

2.  the packages tflplanner's programs use (cards, cardx, dplyr, ...)
    that are not installed yet
    ([`tflplanner_packages()`](https://ichirio.github.io/tflplanner/reference/tflplanner_packages.md));

3.  a shortcut that starts tflplanner with a double click
    ([`add_shortcut()`](https://ichirio.github.io/tflplanner/reference/add_shortcut.md)).

Everything else – the study folders, the language, the company
standards, the sample study – can be changed in the app's settings. To
update tflplanner later, use
[`update_tflplanner()`](https://ichirio.github.io/tflplanner/reference/update_tflplanner.md)
or the "update and launch" shortcut.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmppeoQLT/tflplanner-home1a495108a4b7 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmppeoQLT/tflplanner-home1a495108a4b7
#> new studies go to: /tmp/RtmppeoQLT/tflplanner-home1a495108a4b7/workspace
tflplanner_config()
#> $studies_root
#> [1] "/tmp/RtmppeoQLT/tflplanner-home1a495108a4b7/workspace"
#> 
#> $last_study
#> NULL
#> 
#> $language
#> NULL
#> 
#> $update_channel
#> NULL
#> 
options(old)

if (FALSE) { # \dontrun{
# not run: these remember the home and the study folder for every later
# session (and the first one asks, step by step)
setup_tflplanner()
setup_tflplanner(studies_root = "C:/studies", sample = TRUE)
} # }
```
