# The preview engine (R/preview_engine.R): SAMPLE-01's DM report.

# the sample study in a folder of its own, made by the package's function
.pv_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  withr_old <- function() options(old)
  do.call(on.exit, list(substitute(withr_old()), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  s$path
}

test_that("a table preview returns ok and the table, the second call is a cache hit", {
  skip_on_cran()
  root <- .pv_sample()
  text <- paste(readLines(file.path(root, "spec", "outputs", "T-14-1-1.yml"), warn = FALSE),
                collapse = "\n")
  cache <- new.env()
  r1 <- .preview_report(text, root, "table", cache = cache)
  expect_true(r1$ok, info = paste(r1$messages, collapse = " | "))
  expect_match(r1$html, "<table", fixed = TRUE)
  expect_match(r1$html, "Age", fixed = TRUE)
  expect_true(is.character(r1$code) && nzchar(r1$code))
  expect_null(r1$rtf)
  expect_true(nzchar(r1$key))
  expect_false(isTRUE(r1$cached))
  r2 <- .preview_report(text, root, "table", cache = cache)
  expect_true(r2$ok)
  expect_true(isTRUE(r2$cached))
  expect_identical(r2$key, r1$key)
  expect_identical(r2$html, r1$html)
  message(sprintf("preview timing: cold table %.2f s, cache hit %.3f s", r1$seconds, r2$seconds))
  # a change of the text is another key: not a hit
  r3 <- .preview_key(paste(text, "# changed"), root, "table")
  expect_false(identical(r3, r1$key))
  # the stage is part of the key
  expect_false(identical(.preview_key(text, root, "full"), r1$key))
  # a data file's mtime is part of the key
  adsl <- file.path(root, "data", "adam", "adsl.rds")
  old <- file.mtime(adsl)
  Sys.setFileTime(adsl, old + 60)
  expect_false(identical(.preview_key(text, root, "table"), r1$key))
  expect_identical(.preview_key(text, root, "table"), .preview_key(text, root, "table"))
})

test_that("the full stage also makes the RTF", {
  skip_on_cran()
  root <- .pv_sample()
  text <- paste(readLines(file.path(root, "spec", "outputs", "T-14-1-1.yml"), warn = FALSE),
                collapse = "\n")
  r <- .preview_report(text, root, "full", cache = new.env())
  expect_true(r$ok, info = paste(r$messages, collapse = " | "))
  expect_true(file.exists(r$rtf))
  expect_match(readLines(r$rtf, n = 1L, warn = FALSE), "^\\{\\\\rtf")
  message(sprintf("preview timing: cold full %.2f s", r$seconds))
})

test_that("a broken report gives ok = FALSE and a message, never an error", {
  skip_on_cran()
  root <- .pv_sample()
  r <- expect_no_error(.preview_report("id: [unclosed\n  - : :", root, cache = new.env()))
  expect_false(r$ok)
  expect_true(length(r$messages) >= 1L && nzchar(r$messages[length(r$messages)]))
  expect_null(r$html)
  # a report that reads but does not run (a content step that does not exist)
  text <- paste(readLines(file.path(root, "spec", "outputs", "T-14-1-1.yml"), warn = FALSE),
                collapse = "\n")
  bad <- sub("- table_plan:", "- no_such_step:", text, fixed = TRUE)
  r2 <- expect_no_error(.preview_report(bad, root, cache = new.env()))
  expect_false(r2$ok)
  # a failure is not cached
  cache <- new.env()
  .preview_report(bad, root, cache = cache)
  expect_length(grep("^study:", ls(cache), value = TRUE, invert = TRUE), 0L)
  # no study folder
  r3 <- expect_no_error(.preview_report(text, file.path(root, "nope"), cache = new.env()))
  expect_false(r3$ok)
})
