# The Template series card has no Save of its own (#753): its edits are the
# planner's `series_edits`, and the study's save writes them into the study's
# copy of the series (spec/series/<series>/series.yml) -- with the rest of
# the save, all or nothing.

series_study <- function(env = parent.frame()) {
  ok <- tryCatch(.series_available() && is.data.frame(tflspec::tfl_series_list()),
                 error = function(e) FALSE)
  testthat::skip_if(!isTRUE(ok), "this tflspec has no template series")
  home <- local_home(env)
  s <- suppressMessages(sample_study_copy(home))
  invisible(tflspec::tfl_copy_templates(s$path, "tflspec_v1/tflspec_summary"))
  s
}
copy_value <- function(s, key) {
  tflspec::tfl_series_read("tflspec_v1", dir = .series_copy_root(s$path))$values[[key]]
}

test_that("the study's save writes the series card's edits into the copy, and keeps none in the planner", {
  skip_on_cran()
  s <- series_study()
  src <- tflspec::tfl_series_read("tflspec_v1")$values
  expect_false(identical(copy_value(s, "max_rows"), 12L))
  s$planner$series_edits <- list(tflspec_v1 = list(max_rows = "12"))
  out <- suppressMessages(save_study(s))
  expect_identical(copy_value(s, "max_rows"), 12L)
  expect_null(out$planner$series_edits)
  expect_true(any(endsWith(out$files$file, file.path("spec", "series", "tflspec_v1", "series.yml"))))
  # (the other values stay the source's)
  expect_identical(copy_value(s, "population_safety"), src$population_safety)
  # the state saved has none either: the study opens with no unsaved values
  expect_null(open_study(s$meta$study_id)$planner$series_edits)
  # back to the series' value: the source's again
  o <- open_study(s$meta$study_id)
  o$planner$series_edits <- list(tflspec_v1 = stats::setNames(list(), character()))
  suppressMessages(save_study(o))
  expect_identical(copy_value(s, "max_rows"), src$max_rows)
})

test_that("a save that stops writes no series value either (#751 item 4)", {
  skip_on_cran()
  s <- series_study()
  f <- file.path(.series_copy_root(s$path), "tflspec_v1", "series.yml")
  before <- tools::md5sum(f)
  s$planner$series_edits <- list(tflspec_v1 = list(max_rows = "12"))
  testthat::with_mocked_bindings(
    expect_error(suppressMessages(save_study(s)), "no program"),
    .save_ard = function(...) stop("no program", call. = FALSE))
  expect_identical(tools::md5sum(f), before)
  # (a value that is not YAML stops the save: nothing written)
  s$planner$series_edits <- list(tflspec_v1 = list(max_rows = "[12"))
  expect_error(suppressMessages(save_study(s)), "is not YAML")
  expect_identical(tools::md5sum(f), before)
})
