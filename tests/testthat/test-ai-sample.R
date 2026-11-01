test_that("the AI sample: its materials written, and where each is for step 1", {
  skip_if_not("tfl_ai_sample_study" %in% getNamespaceExports("tflspec"))
  d <- tempfile("aisample")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  k <- .ai_sample_kit(d)
  expect_named(k, c("toc", "shells", "adam", "adam_spec", "docs"))
  expect_true(all(file.exists(unlist(k))))
  expect_length(list.files(k$adam), 5L)
  # written again over what is there
  expect_identical(.ai_sample_kit(d), k)
  expect_true(.ai_sample_ok() || isFALSE(getOption("tflplanner.ai_drafts", TRUE)))
})

test_that("without a tflspec that writes it, the AI sample says what is needed", {
  skip_if("tfl_ai_sample_study" %in% getNamespaceExports("tflspec"))
  expect_false(.ai_sample_ok())
  expect_error(.ai_sample_kit(tempfile()), "tfl_ai_sample_study")
})

test_that("the AI sample as a study: made at once, its materials where the study reads them", {
  skip_if_not("tfl_ai_sample_study" %in% getNamespaceExports("tflspec"))
  local_home()
  d <- tempfile("aisample")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  k <- .ai_sample_kit(d)
  m <- .ai_sample_study("aitest-02", k)
  dir <- normalizePath(file.path(studies_root(), "aitest-02"), "/")
  expect_identical(normalizePath(m$study$path, "/"), dir)
  expect_identical(list_studies()$study_id, "aitest-02")
  expect_true(all(file.exists(file.path(dir, c("study.yml", "spec/data.yml", "spec/defaults.yml")))))
  expect_identical(m$kit$toc, file.path(dir, "docs", "toc.xlsx"))
  expect_identical(m$kit$shells, file.path(dir, "docs", "shells.xlsx"))
  expect_identical(m$kit$adam_spec, file.path(dir, "docs", "adam_spec.xlsx"))
  expect_identical(m$kit$docs, file.path(dir, "ai", "docs", "sap.docx"))
  expect_identical(m$kit$adam, file.path(dir, "data", "adam"))
  expect_setequal(list.files(m$kit$adam), list.files(k$adam))
  expect_true(all(file.exists(unlist(m$kit))))
  # the ID taken: refused, and what was there kept
  expect_error(.ai_sample_study("aitest-02", k), "already there")
  expect_true(file.exists(file.path(dir, "study.yml")))
  # a failure leaves nothing behind
  bad <- k
  bad$adam <- file.path(d, "no-adam")
  expect_error(.ai_sample_study("aitest-03", bad))
  expect_false(dir.exists(file.path(studies_root(), "aitest-03")))
})
