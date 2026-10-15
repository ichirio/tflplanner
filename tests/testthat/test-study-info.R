# The study tab: Title, Short title, the read-only Study ID (#386); no veil
# while a field is edited (#51).

test_that("a study keeps a short title in study.yml and says it in the setup", {
  skip_on_cran()
  withr::local_options(list(tflplanner.home = withr::local_tempdir()))
  root <- withr::local_tempdir()
  s <- create_study("SI-1", title = "A long, formal title of the study",
                    short_title = "Short one", root = root)
  expect_identical(s$meta$short_title, "Short one")
  expect_identical(open_study("SI-1")$meta$short_title, "Short one")
  expect_identical(list_studies()$short_title, "Short one")
  txt <- readLines(file.path(s$path, "programs", "study_setup.R"))
  expect_true(any(grepl("^study_short_title +<- \"Short one\"$", txt)))
  # none: NA, as the other fields
  t2 <- create_study("SI-2", root = root)
  expect_true(is.na(t2$meta$short_title))
})

test_that("a STUDY_ID token that differs from the Study ID is kept and said", {
  t <- function(x) x
  x <- new_planner()
  expect_null(.study_id_token_note(x, "ABC-1", t))
  x <- set_study_token(x, "STUDY_ID", "ABC-1")
  expect_null(.study_id_token_note(x, "ABC-1", t))
  x <- set_study_token(x, "STUDY_ID", "OLD-7")
  note <- as.character(.study_id_token_note(x, "ABC-1", t))
  expect_match(note, "OLD-7", fixed = TRUE)
  expect_match(note, "tok_study_id_use", fixed = TRUE)
  expect_identical(study_token(x, "STUDY_ID"), "OLD-7")
})

test_that("editing a field or Save does not put up the busy veil", {
  js <- .updating_js
  expect_match(js, "textarea, select", fixed = TRUE)
  expect_match(js, "#save", fixed = TRUE)
  expect_match(js, "if (e.target && e.target.closest && e.target.closest(quiet)) return;",
               fixed = TRUE)
})

test_that("the study's fields are tokens a page may say, set only when one does", {
  meta <- list(title = "A long, formal title", short_title = NA, compound = "ABC-1",
               phase = "3", description = "")
  x <- new_planner()
  # no line says one: nothing is added (a study that does not use them is as it was)
  expect_identical(.meta_tokens(x, meta)$sheets$tokens, x$sheets$tokens)
  x <- set_sheet_rows(x, "header", NA, data.frame(line = "1", left = "{STUDY_TITLE}",
                                                  right = "{STUDY_COMPOUND} / {STUDY_SHORT_TITLE}"))
  y <- .meta_tokens(x, meta)
  expect_identical(study_token(y, "STUDY_TITLE"), "A long, formal title")
  expect_identical(study_token(y, "STUDY_COMPOUND"), "ABC-1")
  # a blank field has no row; one no line says is not added
  expect_identical(study_token(y, "STUDY_SHORT_TITLE"), NA_character_)
  expect_identical(study_token(y, "STUDY_PHASE"), NA_character_)
  # it follows the field
  meta$title <- "Another title"
  expect_identical(study_token(.meta_tokens(y, meta), "STUDY_TITLE"), "Another title")
  # named as study_setup.R's variables (study_title ...)
  expect_identical(unname(.meta_token_fields),
                   c("STUDY_TITLE", "STUDY_SHORT_TITLE", "STUDY_COMPOUND", "STUDY_PHASE",
                     "STUDY_DESCRIPTION"))
})
