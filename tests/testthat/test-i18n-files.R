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

test_that("only the words that stay English have no Japanese, and no row is a fragment", {
  s <- .strings()
  stay <- c("output_id", "KB", "TODO", "ARD", "error", "todo", "outdated", "unsaved",
            "ok", "OK", "RTF")
  expect_identical(setdiff(s$en[!nzchar(s$ja)], stay), character())
  # the long rows that were cut at a comma (their tail was a row of its own) hold their end
  k <- "it does not say which statistics it gives: cards::as_cards_fn(<the function>, stat_names = c(...)) lets a check see them"
  expect_match(tr(k, "ja"), "stat_names = c(...))", fixed = TRUE)
  k <- grep("^With group_page = TRUE: each page's name, the heading", s$en, value = TRUE)
  expect_match(tr(k, "ja"), "variables = TRT01A", fixed = TRUE)
})

test_that("the file inputs and the grid grip speak the app's language", {
  html <- as.character(app_ui("ja"))
  expect_match(html, "\u53c2\u7167\u2026", fixed = TRUE)
  expect_match(html, "\u672a\u9078\u629e", fixed = TRUE)
  expect_false(grepl("No file selected", html, fixed = TRUE))
  expect_false(grepl("drag to resize", html, fixed = TRUE))
  expect_match(as.character(app_ui("en")), "drag to resize", fixed = TRUE)
})
