# The texts' Japanese, a file per R file (inst/i18n/strings/*.csv): each
# file `en`, `ja`; a text in one file only

test_that("the translation files hold each text once", {
  fs <- list.files(system.file("i18n", "strings", package = "tflplanner"), "[.]csv$", full.names = TRUE)
  expect_gt(length(fs), 1L)
  for (f in fs) {
    d <- utils::read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character())
    expect_identical(names(d), c("en", "ja"), info = basename(f))
  }
  s <- .strings()
  expect_identical(s$en[duplicated(s$en)], character())
  expect_identical(tr("Apply", "ja"), "\u9069\u7528")
})
