# The test-only series tflplanner_test (fixtures/series/tflplanner_test):
# the template param forms no shipped template has since the purpose
# templates left tflspec_v1 (canon 2026-11-24).  tflspec reads it while the
# option tflspec.series_dir points at it, for the calling test only (not a
# setup file: a test that lists the series would see it); the arg defs'
# cache is emptied before and after.
local_test_series <- function(env = parent.frame()) {
  dir <- normalizePath(testthat::test_path("fixtures", "series"), winslash = "/", mustWork = TRUE)
  withr::local_options(tflspec.series_dir = dir, .local_envir = env)
  .arg_defs_reset()
  withr::defer(.arg_defs_reset(), envir = env)
  invisible("tflplanner_test")
}
