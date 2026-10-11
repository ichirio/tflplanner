# Make a shortcut that starts tflplanner

Makes the shortcuts that start tflplanner in the browser with a double
click, without R or RStudio open:

## Usage

``` r
add_shortcut(
  port = NULL,
  desktop = TRUE,
  start_menu = TRUE,
  update = FALSE,
  ask = interactive()
)

remove_shortcut(launcher = TRUE, ask = interactive())
```

## Arguments

- port:

  The port the app runs on. `NULL` keeps the setting
  (`setup_tflplanner(port = )`, default 7470); a number is saved as the
  setting.

- desktop:

  Make the desktop shortcut (Windows).

- start_menu:

  Make the Start menu entries (Windows).

- update:

  Also make "update and launch" (`FALSE` by default: tflplanner is never
  updated unless you ask – with this shortcut, made when
  `update = TRUE`, or
  [`update_tflplanner()`](https://ichirio.github.io/tflplanner/reference/update_tflplanner.md)).

- ask:

  Ask before making them. `FALSE` makes them without asking (scripts);
  outside an interactive session nothing is made unless `ask = FALSE`.

- launcher:

  Also remove the launcher files tflplanner keeps.

## Value

The shortcuts made, invisibly.

## Details

- **Windows**: "tflplanner" on the desktop and in the Start menu, and
  "tflplanner (update and launch)" in the Start menu. To pin tflplanner
  to the taskbar, right-click it in the Start menu and choose *Pin to
  taskbar* (Windows does not let a program do it).

- **macOS**: `tflplanner.app` and `tflplanner (update).app` in
  `~/Applications`.

- **Linux**: a `tflplanner.desktop` menu entry in
  `~/.local/share/applications`, with an *Update and launch* action.

A shortcut runs a small launcher that tflplanner keeps in
`tools::R_user_dir("tflplanner", "config")`. The launcher finds R each
time it starts (on Windows from the registry entry the R installer
writes), so updating R does not break the shortcut, and it shows no
console window. If tflplanner already runs on the port, the shortcut
opens it; otherwise it starts it, and closing the browser stops it.

"Update and launch" first updates rtfreporter, tflspec and tflplanner
([`update_tflplanner()`](https://ichirio.github.io/tflplanner/reference/update_tflplanner.md),
on the channel last used), then starts the app.

On Windows the shortcuts are written by a VBScript, and one it could not
write again by PowerShell: security software may stop a script from
writing to the desktop without a message. If neither can, the error says
how to allow it or to make the shortcut by hand.

## See also

`remove_shortcut()`,
[`launch_app()`](https://ichirio.github.io/tflplanner/reference/launch_app.md),
[`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# not run: writes a shortcut on your desktop and in the start menu
add_shortcut()
add_shortcut(port = 7480)
} # }
```
