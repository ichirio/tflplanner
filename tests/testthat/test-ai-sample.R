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
