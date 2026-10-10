# A figure printing a table's numbers turns stale with that table's ARD
# (#293 phase 4): its program records the definition of the ARD it read
# (record_report(ard =)), study_status() compares it with the definition
# now and says why (`ard:<id>`), and the official run and the preview make
# the table's ARD first when it is not made from its definition.

stale_study <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  withr::local_options(tflplanner.home = home, .local_envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE))
}

fig_state <- function(s, id = "F-14-2-3") {
  st <- study_status(s)
  as.list(st[st$output_id == id, c("status", "why")])
}

test_that("a figure from an ARD records the definition it read; batch.R knows its need", {
  s <- stale_study()
  p <- s$planner
  expect_identical(.fig_ard_need(p, "F-14-2-3"), "T-14-2-2")
  expect_identical(.fig_ard_need(p, "T-14-2-2"), NA_character_)
  expect_identical(.fig_ard_need(p, "F-14-2-1"), NA_character_)
  code <- program_code(p, "F-14-2-3")
  expect_true("ard_built <- ard_fingerprint(\"T-14-2-2\")" %in% code)
  expect_true("record_report(report_id, ard = ard_built)" %in% code)
  # the others record no ARD
  expect_true("record_report(report_id)" %in% program_code(p, "T-14-2-2"))
  # record_report() has the column
  rs <- report_setup_code(p)
  expect_true(any(grepl("record_report <- function(output_id, ard = \"\")", rs, fixed = TRUE)))
  # (one element a statement: a line each)
  lines_of <- function(x) unlist(strsplit(x, "\n", fixed = TRUE))
  b <- lines_of(batch_code(p, root = s$path))
  i <- grep("^[.]batch_needs <- ", b)
  expect_identical(b[i + 1L], "  `programs/tfl/F-14-2-3.R` = \"T-14-2-2\",")
  # the forest plot reads its own ARD (#293 P6): made first too
  expect_identical(b[i + 2L], "  `programs/tfl/F-14-2-4.R` = \"F-14-2-4\")")
  h <- grep("^[.]batch_needs_hash <- ", b)
  expect_match(b[h + 1L], "^  `T-14-2-2` = \"[0-9a-f]{32}\",$")
  expect_match(b[h + 2L], "^  `F-14-2-4` = \"[0-9a-f]{32}\"[)]$")
  # no figure from an ARD: empty
  p0 <- set_fig_ard_source(p, "F-14-2-3", NULL)
  b0 <- lines_of(batch_code(p0, root = s$path))
  expect_true(".batch_needs <- character()" %in% b0)
})

test_that("the figure turns outdated with the table's ARD, and the preview and the batch make it first", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggsurvfit")
  s <- stale_study()
  # the preview: T-14-2-2's ARD made first (it is not made yet)
  run_study(s, "F-14-2-3")
  expect_identical(ard_status(s)$state[ard_status(s)$output_id == "T-14-2-2"], "built")
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # T-14-2-2's definition changes: its ARD is not made from it, the figure
  # is to be made again because of it
  an <- s$planner$ard$analyses
  k <- an$output_id %in% "T-14-2-2" & an$analysis_id == "KM"
  an$code[k] <- sub("150, 180)", "150)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  expect_identical(fig_state(s), list(status = "outdated", why = "ard:T-14-2-2"))
  # the ARD made again: the figure was made from the old one
  suppressMessages(update_study_ard(s, "T-14-2-2"))
  expect_identical(fig_state(s), list(status = "outdated", why = "ard:T-14-2-2"))
  run_study(s, "F-14-2-3")
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # changed again; the official run of the reports alone makes the ARD first
  an$code[k] <- sub("150)", "150, 180)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  b <- run_batch(s, "tfl", only = "F-14-2-3", code = FALSE)
  expect_identical(b$result$part, c("ard", "tfl"))
  expect_identical(b$result$program, c("programs/ard/T-14-2-2.R", "programs/tfl/F-14-2-3.R"))
  expect_true(any(grepl("made first", b$output, fixed = TRUE)))
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # nothing changed: the ARD is not made again
  b2 <- run_batch(s, "tfl", only = "F-14-2-3", code = FALSE)
  expect_identical(b2$result$part, "tfl")
})

test_that("the runs table says why", {
  expect_identical(.why_text(c("", "program", "ard:T-14-2-2 setup")),
                   c("", ": its program changed", ": T-14-2-2's ARD changed; the study setup changed"))
})
