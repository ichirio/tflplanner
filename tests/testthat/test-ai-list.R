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
