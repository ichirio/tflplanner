# tflspec's second stage in a study tflplanner opens and saves (#475):
# data.yml's records: and objects:, an analysis's records: and by: [] are
# kept as they are written

skip_if_not("records" %in% names(tflspec::tfl_ard_spec_template()), "tflspec before #460 (records)")
skip_on_cran()

test_that("records, objects and by: [] survive opening and saving the study", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  x <- suppressMessages(create_sample_study(run = FALSE, home = home))
  d <- x$path %||% file.path(home, "studies", "SAMPLE-01")
  dy <- file.path(d, "spec", "data.yml")
  cat("records:", "  - id: TEAE", "    dataset: ADAE", "    where: !r TRTEMFL == \"Y\"",
      "    label: Treatment-emergent AEs", "    any_label: Any TEAE",
      "objects:", "  - id: n_saf", "    code: !r 254L", "    label: Subjects in the safety set",
      sep = "\n", file = dy, append = TRUE)
  of <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  y <- readLines(of, encoding = "UTF-8")
  at <- grep("^  analyses:", y)
  y <- append(y, c("    - id: NALL", "      label: All subjects", "      method: total_n",
                   "      data: adsl_saf", "      by: []"), at)
  writeLines(y, of, useBytes = TRUE)
  suppressMessages(reload_from_spec("SAMPLE-01", home = home))
  st <- suppressMessages(open_study("SAMPLE-01", home = home))
  suppressMessages(save_study(st, home = home))
  # (the files written again from what tflplanner holds)
  suppressMessages(write_spec("SAMPLE-01", home = home))
  back <- tflspec::tfl_read_study_spec(d)
  expect_identical(vapply(back$data$records, `[[`, "", "id"), "TEAE")
  expect_identical(back$data$records[[1L]]$any_label, "Any TEAE")
  expect_identical(vapply(back$data$objects, `[[`, "", "id"), "n_saf")
  o <- tflspec::tfl_read_outputs(d)[["T-14-1-1"]]
  nall <- Filter(function(a) identical(a$id, "NALL"), o$ard$analyses)
  expect_length(nall, 1L)
  # (by: [], "no columns": tflspec's by required, #461)
  skip_if_not(exists(".by_needed_problems", asNamespace("tflspec")), "tflspec before #461 (by: [])")
  expect_true("by" %in% names(nall[[1L]]))
  expect_length(nall[[1L]]$by, 0L)
})
