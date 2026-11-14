withr_tempdir <- function(env = parent.frame()) {
  d <- tempfile("tflplanner-test")
  dir.create(d)
  withr_defer <- function() unlink(d, recursive = TRUE)
  do.call(on.exit, list(substitute(withr_defer()), add = TRUE), envir = env)
  d
}

# The app looks for a newer version on the network when it starts; not in
# the tests.
options(tflplanner.check_updates = FALSE)

# a figure design of the sample study (its YAML spec, as the app reads it)
sample_fig_design <- function(id) {
  .planner_from_yaml(
    system.file("sample", "SAMPLE-01", package = "tflplanner"))$fig_designs[[id]]
}

# The slowest test files (the whole app's server driven through
# shiny::testServer(), whole-study builds) start with skip_if_quick(): the
# quick check on a pull request into next sets TFLPLANNER_QUICK=true and leaves
# them out.  They run everywhere else -- locally, on each push to next, on pull
# requests into main and nightly (R-CMD-check.yaml).
skip_if_quick <- function() {
  if (identical(tolower(Sys.getenv("TFLPLANNER_QUICK")), "true")) {
    testthat::skip("slow file: left out of the quick check (TFLPLANNER_QUICK=true)")
  }
  invisible(TRUE)
}
