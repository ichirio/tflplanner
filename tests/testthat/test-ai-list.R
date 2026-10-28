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
  # (set back: the AI's values again, no action)
  w <- .ai_expected_apply(.ai_expected_apply(e, "change", "T-1", list(title = "D", population = "FAS")), "keep", "T-1")
  expect_identical(w$title[1L], "Demographics")
  expect_identical(w$population[1L], "SAF")
  expect_true(is.na(w$action[1L]) && is.na(w$was[1L]))
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

test_that("run.R is not started beside an official run, nor with changes unsaved", {
  d <- withr_tempdir()
  writeLines("stop('not to run')", file.path(d, "run.R"))
  said <- character()
  unsaved <- FALSE
  shiny::testServer(function(input, output, session) {
    session$userData$rv <- shiny::reactiveValues(job = "a run")
    ai_list_server(input, output, session, function() d, shiny::reactiveVal(0L), function(...) TRUE,
                   notify = function(x, ...) said <<- c(said, x), guarded = function(x) x,
                   t = function(x) x, dirty = function() unsaved)
  }, {
    session$setInputs(run_all = 1)
    expect_identical(said[length(said)], "A run is going on")
    session$userData$rv$job <- NULL
    unsaved <<- TRUE
    session$setInputs(run_all = 2)
    expect_match(said[length(said)], "Save first", fixed = TRUE)
    expect_false(file.exists(file.path(d, "ai", "run-from-app.log")))
  })
})

test_that("every request's reports are built by run.R, or by tfl_ai_run() in chat mode", {
  d <- withr_tempdir()
  expect_identical(.ai_run_args(d), c("-e", "invisible(tflspec::tfl_ai_run('.'))"))
  writeLines("1", file.path(d, "run.R"))
  expect_identical(.ai_run_args(d), "run.R")
  env <- .ai_run_env()
  expect_identical(env[[1]], "current")
  expect_identical(unname(env["R_LIBS"]), paste(.libPaths(), collapse = .Platform$path.sep))
})

test_that("the reports adopted go into their TOC batches, as spec/toc.yml says them", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  expect_identical(nrow(.ai_toc_batches(d)), 0L)
  writeLines(c("outputs:", "  - id: T-1", "    batches: Topline", "  - id: T-2",
               "    batches: [Topline, Safety]", "  - id: T-3",
               "batches:", "  DSUR: [T-3, T-1]"), file.path(d, "spec", "toc.yml"))
  b <- .ai_toc_batches(d)
  expect_identical(b$batch, c("Topline", "Topline", "Safety", "DSUR", "DSUR"))
  expect_identical(b$output_id, c("T-1", "T-2", "T-2", "T-3", "T-1"))
  x <- new_planner()
  for (id in c("T-1", "T-2", "T-3", "T-4")) x <- add_output(x, id, process_code = "1")
  x <- .set_report_batches(x, "T-2", "Mine")
  y <- .ai_apply_toc_batches(x, d, c("T-1", "T-2", "T-3", "T-4"))
  expect_identical(y$outputs$batches, c("Topline | DSUR", "Mine", "DSUR", NA))
  # only the reports adopted
  expect_identical(.ai_apply_toc_batches(x, d, "T-3")$outputs$batches, c(NA, "Mine", "DSUR", NA))
})

test_that("an official run says the AI's reports whose draft changed or that wait for corrections", {
  d <- withr_tempdir()
  expect_identical(.ai_run_warnings(NULL, "T-1"), list(newer = character(), fix = character()))
  expect_identical(.ai_run_warnings(d, "T-1"), list(newer = character(), fix = character()))
  dir.create(file.path(d, "ai", "spec", "outputs"), recursive = TRUE)
  f <- file.path(d, "ai", "spec", "outputs", c("T-1.yml", "T-2.yml"))
  writeLines("id: T-1", f[1])
  writeLines("id: T-2", f[2])
  utils::write.csv(data.frame(output_id = c("T-1", "T-2"), adopted = "2026-10-29 03:00:00",
                              md5 = unname(tools::md5sum(f))),
                   file.path(d, "ai", "adopted.csv"), row.names = FALSE)
  writeLines("id: T-2\nlabel: redrafted", f[2])
  w <- .ai_run_warnings(d, c("T-1", "T-2", "T-3"))
  expect_identical(w$newer, "T-2")
  expect_identical(w$fix, character())
})
