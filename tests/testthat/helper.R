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
