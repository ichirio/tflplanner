# The common errors get a lead in the app's language; the message stays.

test_that("a common error gets a Japanese lead, the message kept after it", {
  ja <- function(x) tr(x, "ja")
  m <- "There is an ARD function ard_cv already."
  out <- .error_view(m, ja)
  expect_true(endsWith(out, m))
  expect_false(identical(out, m))
  expect_identical(out, paste0(ja("An ARD function of that name is already there"), "\uff1a", m))
  m2 <- "Report 'T-1' is already on the list."
  expect_true(endsWith(.error_view(m2, ja), m2))
})

test_that("an unknown error, and the English app, show the message alone", {
  ja <- function(x) tr(x, "ja")
  expect_identical(.error_view("something odd", ja), "something odd")
  m <- "Study 'S1' is already registered."
  expect_identical(.error_view(m, identity), m)
  expect_identical(.error_view(m, function(x) tr(x, "en")), m)
})

test_that("every lead has its Japanese", {
  ja <- function(x) tr(x, "ja")
  leads <- vapply(.error_leads, `[[`, "", 2L)
  expect_true(all(vapply(leads, function(l) !identical(ja(l), l), TRUE)))
})

test_that("an error notification has its text and a copy button", {
  ja <- function(x) tr(x, "ja")
  m <- "Study 'S1' is already registered.\nSecond line"
  h <- as.character(.error_note_ui(m, ja))
  expect_match(h, "tp-error-text", fixed = TRUE)
  expect_match(h, "Study 'S1' is already registered.", fixed = TRUE)
  expect_match(h, "tp-copy-error", fixed = TRUE)
  expect_match(h, ja("Copy the message"), fixed = TRUE)
  expect_match(h, paste0('data-copied="', ja("Copied"), '"'), fixed = TRUE)
  expect_false(identical(ja("Copied"), "Copied"))
  expect_match(.error_note_js, "tp-copy-error", fixed = TRUE)
})
