# The AI drafts tab's card for the look of past reports (R/ai_style_ui.R)

test_that("the survey's files are the home's, not a study's", {
  local_home()
  expect_identical(.ai_style_dir(), file.path(tflplanner_home(), "style"))
})
