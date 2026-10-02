# Update tflplanner, rtfreporter and tflspec

Updates rtfreporter, then tflspec, then tflplanner – in that order, so
each finds the version of the one before it that it needs. The work is
done in a separate R process, since this session cannot replace a
package it has loaded; afterwards, restart R (in RStudio: *Session \>
Restart R*) before using tflplanner again. The running app does not
update itself: it says when a newer version is out.

## Usage

``` r
update_tflplanner(channel = NULL, from = NULL, ask = interactive())
```

## Arguments

- channel:

  `"release"`: CRAN when the package is there, else its latest GitHub
  release. `"dev"`: the development version on GitHub (main). `NULL`
  uses the channel last used (`"release"` at first). The channel is
  remembered, and the "update and launch" shortcut uses it.

- from:

  A folder with package files (`rtfreporter_*.tar.gz` / `.zip` / `.tgz`,
  and the same for tflspec and tflplanner) to install from instead, when
  this computer cannot reach GitHub. The newest file of each package is
  installed.

- ask:

  Show the plan and ask before installing. Outside an interactive
  session nothing is installed unless `ask = FALSE`.

## Value

`TRUE` when the update finished, invisibly.

## See also

[`tflplanner_packages()`](https://ichirio.github.io/tflplanner/reference/tflplanner_packages.md),
[`add_shortcut()`](https://ichirio.github.io/tflplanner/reference/add_shortcut.md).

## Examples

``` r
if (FALSE) { # \dontrun{
update_tflplanner()                    # the released versions
update_tflplanner("dev")               # the development versions
update_tflplanner(from = "D:/tflplanner-packages")
} # }
```
