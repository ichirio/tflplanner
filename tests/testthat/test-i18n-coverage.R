# The screen texts' rows (inst/i18n/strings/*.csv) against what the app can
# show: every tflspec text the app shows has its Japanese, and every row's
# English is a text the app can show (helper-i18n-coverage.R).

test_that("every tflspec text the app shows has a row with its Japanese", {
  skip_if_not_installed("tflspec")
  tx <- .i18n_tflspec_texts()
  # (the kinds the app shows: the schema's, the definitions', the templates')
  expect_true(all(c("schema", "defs", "template") %in% tx$kind))
  s <- .strings()
  miss <- tx[!tx$text %in% s$en[nzchar(s$ja)], , drop = FALSE]
  expect_identical(paste(miss$where, miss$text, sep = ": "), character())
})

# Rows kept although no code shows their text as a whole: each with why
.i18n_kept_rows <- c(
  # the start of a longer text tflspec has now (args.csv, the reports'
  # tokens): `git grep -F` finds it, so it is not removed by that rule
  "arg-hint:The subject key, e.g. USUBJID; subjects are counted once." = "part of a tflspec text",
  "arg-hint:What a percentage divides by: the population, or cards' row / column / cell." =
    "part of a tflspec text",
  "arg-hint:Which of the variables to report (default: all of them)." = "part of a tflspec text",
  "arg-hint:TRUE: also the missing counts of each variable." = "part of a tflspec text",
  "arg-hint:The levels of the hierarchy, outermost first: AEBODSYS then AEDECOD." =
    "part of a tflspec text",
  "arg-hint:The levels of the hierarchy to report (default: all)." = "part of a tflspec text",
  "arg-hint:Columns to analyse within, like by, but only the combinations the data have (e.g. a parameter by visit)." =
    "part of a tflspec text",
  "(none here)" = "part of a tflspec comment",
  # words a comment or a longer text of tflplanner / rtfreporter has
  "Modified" = "part of an rtfreporter comment",
  "as generated" = "part of a tflplanner comment",
  "The whole study" = "part of a tflplanner comment",
  "no section" = "part of a tflplanner comment",
  "Check the definition" = "part of a tflplanner function's title")

test_that("every row's English is a text the app can show", {
  skip_if_not_installed("tflspec")
  skip_if_not_installed("rtfreporter")
  s <- .strings()
  used <- .i18n_keys_used(s$en)
  # (a kept row is one no code has: once code has it again, it leaves the list)
  kept <- names(.i18n_kept_rows)
  expect_identical(setdiff(kept, s$en[!used]), character())
  expect_identical(setdiff(s$en[!used], kept), character())
})
