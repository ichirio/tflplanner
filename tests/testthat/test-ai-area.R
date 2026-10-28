# The AI's drafts always in their own folder, ai/spec/outputs (R/ai_draft.R):
# a study answered before has them put apart before a check or run.R

test_that("a study's AI reports in spec/outputs are put apart, its own files kept", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  dir.create(file.path(d, "ai"))
  o <- tflspec::tfl_output("T-1", "table")
  tflspec::tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  utils::write.csv(data.frame(output_id = "T-1", request = "r1"), file.path(d, "ai", "manifest.csv"),
                   row.names = FALSE)
  expect_true(.ai_ensure_area(d))
  expect_true(file.exists(file.path(d, "ai", "spec", "outputs", "T-1.yml")))
  expect_true(file.exists(file.path(d, "spec", "outputs", "T-1.yml")))
  # once apart: left as it is
  expect_false(.ai_ensure_area(d))
  # no AI folder (a study the AI never touched), no study: nothing
  e <- withr_tempdir()
  expect_false(.ai_ensure_area(e))
  expect_false(dir.exists(file.path(e, "ai", "spec", "outputs")))
  expect_false(.ai_ensure_area(NULL))
})

test_that("requests written ask for the drafts' own folder; a dry run does not say", {
  seen <- list()
  local_mocked_bindings(tfl_ai_prepare = function(...) {
    seen[[length(seen) + 1L]] <<- list(...)
    structure(list(), class = "tfl_ai_prepared")
  }, .package = "tflspec")
  adam <- withr_tempdir()
  d <- file.path(withr_tempdir(), "S")
  .ai_prepare(d, "toc.xlsx", adam, dry_run = FALSE)
  .ai_prepare(d, "toc.xlsx", adam, dry_run = TRUE)
  .ai_prepare(d, "toc.xlsx", adam, args = list(area = FALSE), dry_run = FALSE)
  expect_true(seen[[1]]$area)
  expect_null(seen[[2]]$area)
  expect_false(seen[[3]]$area)
})
