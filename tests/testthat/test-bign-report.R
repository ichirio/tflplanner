# BIGN in a report program (A10, the user's decision 2026-11-24): the report
# splits BIGN's rows off its ARD with the study's own helpers
# (study_helpers.R: bign_values(), bign_drop()), so it runs without tflspec,
# and the header's N are BIGN's.  SAMPLE-01's T-14-1-1, as an official run
# makes it.

bign_chr <- function(x) vapply(x, function(v) as.character(unlist(v))[1L], "")

test_that("SAMPLE-01's T-14-1-1: the header's N are BIGN's, and BIGN is not a table row", {
  skip_on_cran()
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("cards")
  local_home()
  suppressMessages(sample_study_copy())
  o <- open_study("SAMPLE-01")
  b <- suppressMessages(run_batch(o, c("ard", "tfl"), only = "T-14-1-1"))
  bt <- file.path(b$batch, "batch.txt")
  expect_true(isTRUE(b$ok), info = if (file.exists(bt)) paste(readLines(bt), collapse = "\n"))
  # BIGN's numbers, as the ARD program made them
  ard <- readRDS(file.path(b$batch, "output", "ard", "ard.rds"))
  hit <- bign_chr(ard$output_id) == "T-14-1-1" & bign_chr(ard$analysis_id) == "BIGN" &
    bign_chr(ard$stat_name) == "n"
  n <- as.integer(bign_chr(ard$stat[hit]))
  expect_gt(length(n), 1L)
  f <- list.files(file.path(b$batch, "output", "tfl"), "[.]rtf$", full.names = TRUE)
  expect_length(f, 1L)
  rtf <- paste(readLines(f[1L], warn = FALSE), collapse = "\n")
  heads <- unique(regmatches(rtf, gregexpr("[(]N=[^)]*[)]", rtf))[[1L]])
  for (k in n) {
    expect_true(grepl(sprintf("(N=%d)", k), rtf, fixed = TRUE),
                info = paste("BIGN n", k, "; the header has:", paste(heads, collapse = " ")))
  }
  # BIGN's rows (variable TRT01A) are not a group of the table's body
  expect_false(grepl("TRT01A", rtf, fixed = TRUE))
  # the report program calls the study's helpers, not tflspec
  prog <- readLines(file.path(b$batch, "programs", "tfl", "T-14-1-1.R"), warn = FALSE)
  expect_false(any(grepl("tflspec::", prog, fixed = TRUE)))
})
