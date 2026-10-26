# The AI tab's step 4: the list of the AI's reports and their adopting
# (R/ai_list_ui.R); the screen itself in test-ai-draft.R

test_that("a report's file is the study's own (spec/outputs) or a draft in the AI's area", {
  expect_identical(.ai_in_study_spec(c("spec/outputs/T-1.yml", "ai/spec/outputs/T-1.yml", NA,
                                       "spec\\outputs\\T-2.yml")),
                   c(TRUE, FALSE, FALSE, TRUE))
})

test_that("no study, or one tflspec cannot list: no list", {
  expect_null(.ai_list(NULL))
  expect_null(.ai_list(file.path(tempdir(), "no-such-study")))
  expect_null(.ai_adopt_diff(tempdir(), character()))
})

test_that("the list ticks at first the drafts not adopted yet", {
  d <- data.frame(output_id = c("A", "B", "C", "D", "E"),
                  file = c("ai/spec/outputs/A.yml", "ai/spec/outputs/B.yml", "ai/spec/outputs/C.yml", NA,
                           "ai/spec/outputs/E.yml"),
                  status = c("ok", "warning", "ok", "no answer", "error"),
                  adopted = c("no", "yes", "yes, draft newer", "no", "no"))
  expect_identical(.ai_adoptable(d), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(.ai_ticked_first(d), "A")
})

test_that("the person's list: a row changed (what it was said), dropped with a reason, set back, added", {
  e <- data.frame(sheet = c("S1", "S2"), toc_id = c("14.1.1", "14.2.1"), output_id = c("T-1", "T-2"),
                  title = c("Demographics", "AEs"), population = c("SAF", "SAF"), split = NA_character_,
                  action = NA_character_, was = NA_character_, reason = NA_character_, note = NA_character_,
                  stringsAsFactors = FALSE)
  x <- .ai_expected_apply(e, "change", "T-1", list(title = "Demographic Characteristics", population = "SAF",
                                                   note = "by age group"))
  expect_identical(x$action, c("change", NA))
  expect_identical(x$was[1L], "title: Demographics")
  expect_identical(x$title[1L], "Demographic Characteristics")
  expect_identical(x$note[1L], "by age group")
  # (changed again: what the AI said kept)
  x <- .ai_expected_apply(x, "change", "T-1", list(title = "Demographics 2"))
  expect_identical(x$was[1L], "title: Demographics")
  # (a note alone: no action)
  y <- .ai_expected_apply(e, "change", "T-2", list(title = "AEs", note = "SOC / PT"))
  expect_true(is.na(y$action[2L]))
  expect_identical(y$note[2L], "SOC / PT")
  expect_error(.ai_expected_apply(e, "drop", "T-2", list(reason = " ")), "Say why")
  z <- .ai_expected_apply(e, "drop", "T-2", list(reason = "in the CSR only"))
  expect_identical(z$action[2L], "drop")
  z <- .ai_expected_apply(z, "keep", "T-2")
  expect_true(is.na(z$action[2L]) && is.na(z$reason[2L]))
  a <- .ai_expected_apply(e, "add", fields = list(sheet = "S3", title = "Vital signs"))
  expect_identical(nrow(a), 3L)
  expect_identical(a$action[3L], "add")
  expect_true(is.na(a$output_id[3L]))
})
