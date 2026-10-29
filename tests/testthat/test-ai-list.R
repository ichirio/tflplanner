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

test_that("the reports adopted go into their TOC batches, as spec/toc.yml says them", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  expect_identical(nrow(.ai_toc_batches(d)), 0L)
  writeLines(c("outputs:", "  - id: T-1", "    sets: [Topline]", "  - id: T-2",
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


test_that("an official run says the AI's reports whose draft changed or that leave places to confirm", {
  d <- withr_tempdir()
  expect_identical(.ai_run_warnings(NULL, "T-1"), list(newer = character(), confirm = character()))
  expect_identical(.ai_run_warnings(d, "T-1"), list(newer = character(), confirm = character()))
  dir.create(file.path(d, "ai", "spec", "outputs"), recursive = TRUE)
  f <- file.path(d, "ai", "spec", "outputs", c("T-1.yml", "T-2.yml"))
  writeLines("id: T-1", f[1])
  writeLines("id: T-2", f[2])
  utils::write.csv(data.frame(output_id = c("T-1", "T-2"), adopted = "2026-10-29 03:00:00",
                              md5 = unname(tools::md5sum(f))),
                   file.path(d, "ai", "adopted.csv"), row.names = FALSE)
  writeLines("id: T-2\nlabel: redrafted", f[2])
  w <- .ai_run_warnings(d, c("T-1", "T-2", "T-3"))
  expect_named(w, c("newer", "confirm"))
  expect_identical(w$newer, "T-2")
  expect_identical(w$confirm, character())
  # a report in the study whose ai: block leaves a place to confirm
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  o <- tflspec::tfl_output("T-3", "table")
  o$ai <- list(confirm = list(list(where = "content", item = "plan_cells", why = "the denominator")))
  tflspec::tfl_write_output(o, file.path(d, "spec", "outputs", "T-3.yml"))
  w <- .ai_run_warnings(d, c("T-1", "T-2", "T-3"))
  expect_identical(w$confirm, "T-3")
  # (only the reports run)
  expect_identical(.ai_run_warnings(d, "T-1")$confirm, character())
})

test_that("step 4: Tick only those not in the study yet", {
  d <- data.frame(output_id = c("A", "B", "C", "D"),
                  file = c("ai/spec/outputs/A.yml", "ai/spec/outputs/B.yml", NA, "ai/spec/outputs/D.yml"),
                  status = c("ok", "warning", "no answer", "error"),
                  adopted = c("no", "yes", "no", "no"), title = "t", population = "SAF",
                  split = NA_character_, rtf = NA_character_, problem = NA_character_,
                  stringsAsFactors = FALSE)
  local_mocked_bindings(.ai_list = function(dir) d)
  shiny::testServer(function(input, output, session) {
    session$userData$rv <- shiny::reactiveValues(job = NULL)
    ai_list_server(input, output, session, function() tempdir(), shiny::reactiveVal(0L), function(...) TRUE,
                   notify = function(...) NULL, guarded = function(x) x, t = function(x) x)
  }, {
    expect_match(output$list_ticked$html, "1 ticked", fixed = TRUE)
    session$setInputs(list_all = 1)
    expect_match(output$list_ticked$html, "2 ticked", fixed = TRUE)
    session$setInputs(list_new = 1)
    expect_match(output$list_ticked$html, "1 ticked", fixed = TRUE)
    session$setInputs(list_none = 1)
    expect_match(output$list_ticked$html, "0 ticked", fixed = TRUE)
  })
})
