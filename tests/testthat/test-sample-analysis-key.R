# A report's analysis step is written `analysis:` in YAML and `ard:` stops
# in tflspec (canon-gap #3).  tflplanner's own reads of a report file are
# quiet (tryCatch(..., NULL)): a report that no longer reads would only
# look empty there.  So this test calls tflspec's readers directly.

test_that("every SAMPLE-01 report is read by tflspec, its analysis step under $ard", {
  skip_on_cran()
  dir <- system.file("sample", "SAMPLE-01", "spec", "outputs", package = "tflplanner")
  skip_if(!nzchar(dir), "the sample study is not installed")
  files <- list.files(dir, "\\.ya?ml$", full.names = TRUE)
  expect_gt(length(files), 15L)
  for (f in files) {
    o <- tryCatch(tflspec::tfl_read_output(f), error = function(e) e)
    expect_false(inherits(o, "error"), info = paste(basename(f), if (inherits(o, "error")) conditionMessage(o)))
    # (the old key is gone from the files: tflspec stops on it)
    expect_false(any(grepl("^ard:", readLines(f, warn = FALSE))), info = basename(f))
  }
  # (the whole folder in one read, as the study is read)
  expect_no_error(tflspec::tfl_read_outputs(dirname(dirname(dir))))
  # (one report: its analyses are in the object's `$ard` field)
  o <- tflspec::tfl_read_output(file.path(dir, "T-14-1-1.yml"))
  expect_true(length(o$ard$analyses) > 0L)
  expect_true(any(grepl("^analysis:", readLines(file.path(dir, "T-14-1-1.yml"), warn = FALSE))))
})

test_that("a template report's built-in part is read under `analysis:`, the old `ard:` is not an argument part", {
  t <- list(name = "demog", analysis = list(continuous = "AGE"), ard = list(by = "X"))
  expect_identical(.tpl_args_of(t), list(continuous = "AGE"))
  expect_identical(.tpl_builtin_parts, c("analysis", "content"))
})
