# The AI tab's step 1: the numbers in the shell sheets, boxes changed and
# given to tfl_ai_prepare(numbers = ) (R/ai_numbers_ui.R)

test_that("a box changed is a row given; set back to the rule's choice, dropped", {
  e <- .ai_numbers_edit(NULL, "S1", "15.2.4.2.b", TRUE, rule = FALSE)
  expect_identical(e, data.frame(sheet = "S1", number = "15.2.4.2.b", make = TRUE, stringsAsFactors = FALSE))
  e <- .ai_numbers_edit(e, "S2", "14.1.1", FALSE, rule = TRUE)
  expect_identical(nrow(e), 2L)
  # (the first set back: the rule's again, not given)
  e <- .ai_numbers_edit(e, "S1", "15.2.4.2.b", FALSE, rule = FALSE)
  expect_identical(e$sheet, "S2")
  # (no rule known: kept as given)
  expect_identical(nrow(.ai_numbers_edit(NULL, "S3", "15.9", TRUE)), 1L)
})

test_that("the boxes' table says each number's choice and where it comes from", {
  n <- data.frame(sheet = c("S1", "S1"), number = c("14.1.1", "14.1.2"), title = c("Demog", NA),
                  toc_id = c("14.1.1", NA), make = c(TRUE, FALSE), source = c("rule", "guess"),
                  stringsAsFactors = FALSE)
  v <- .ai_numbers_view(n, shiny::NS("ai"), identity)
  expect_identical(nrow(v), 2L)
  expect_match(v[[1L]][1L], "checked", fixed = TRUE)
  expect_false(grepl("checked", v[[1L]][2L], fixed = TRUE))
  expect_identical(v[[5L]], c("14.1.1", "\u2014"))
  expect_identical(v[[6L]], c("rule", "guess"))
})

test_that("a guess says why, when tflspec gives the reason", {
  n <- data.frame(sheet = c("S1", "S2"), number = c("15.2.4.2.b", "14.1.1"), title = NA_character_,
                  toc_id = c("15.2.4.2", "14.1.1"), make = c(TRUE, TRUE), source = c("guess", "rule"),
                  reason = c("part letter differs", NA), stringsAsFactors = FALSE)
  v <- .ai_numbers_view(n, shiny::NS("ai"), identity)
  expect_identical(v[[6L]], c("guess (part letter differs)", "rule"))
})
