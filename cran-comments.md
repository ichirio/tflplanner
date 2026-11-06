## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

<Confirm before submitting: the result of the final `R CMD check --as-cran`
on the submitted tarball.  Expected: the one note is "New submission".>

## Test environments

* local: <OS>, R <version>
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1 and 4.2),
  macos-latest (release), windows-latest (release)
* win-builder: <R-devel and R-release, and the result>

## Notes for the reviewers

* tflplanner is the third of three related packages.  rtfreporter and
  tflspec, which it imports, reached CRAN on <date> and <date>.
* The package is a shiny application.  The examples of run_app() and
  launch_app() run only inside `if (interactive())`.
* User files: the app keeps its settings and each study's saved state under
  `tools::R_user_dir("tflplanner", "config")` and `"data"`, as the CRAN
  policy allows for R >= 4.0; the user chooses where study folders go.
  Examples and tests write nothing there: the tests set R_USER_CONFIG_DIR,
  R_USER_DATA_DIR and R_USER_CACHE_DIR to a temporary folder
  (tests/testthat/setup-user-dirs.R) and use a home in the temporary folder
  (tests/testthat/helper.R, helper-app.R).
* add_shortcut() writes a desktop and start menu shortcut only when the
  user calls it, and asks first in an interactive session (outside one it
  stops unless `ask = FALSE`).  update_tflplanner() installs rtfreporter,
  tflspec and tflplanner (from CRAN, or from GitHub on the "dev" channel)
  only when the user calls it, in a separate R process, after showing the
  plan and asking (outside an interactive session nothing is installed
  unless `ask = FALSE`).  The examples of add_shortcut(), setup_tflplanner()
  and update_tflplanner() are in `\dontrun`, each with a comment saying why:
  they write a shortcut, remember the home for every later session, or
  install packages.  setup_tflplanner() also has a runnable example that
  uses a home in the temporary folder.
* No test or example needs the network.  The optional check for a newer
  version is off unless the user turns it on, and it is off in the tests.
* inst/launcher holds the shortcut scripts and icons that add_shortcut()
  copies for the user (a Windows .vbs, a Unix .sh, and the R scripts
  launch.R and update.R).  Nothing runs them when the package is loaded.
  launch_app() and update_tflplanner() run the R scripts in a separate R
  process, and only when the user calls them.
* inst/sample holds the data of the sample study.  They are the simulated
  oncology study XYZ-ONC-01 of the tflspec package, not real patient data,
  under the same copyright holder and licence (Apache License 2.0); see
  inst/COPYRIGHTS.
