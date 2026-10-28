# Step 4's card of the AI's assumptions (R/ai_assumptions_ui.R)

asm_study <- function() {
  d <- tempfile("asm-")
  dir.create(file.path(d, "spec"), recursive = TRUE)
  dir.create(file.path(d, "ai"))
  rows <- list(
    list(output_id = "T-1", where = "ard.analyses", item = "arm", assumption = "TRT01A",
         question = "Actual or planned?"),
    list(output_id = "T-2", where = "ard.analyses", item = "arm", assumption = "TRT01A",
         question = "actual or planned?"),
    list(output_id = "T-3", where = "page", item = "titles", assumption = "two lines"))
  writeLines(yaml::as.yaml(rows), file.path(d, "spec", "_assumptions.yml"))
  d
}

test_that("the assumptions read with each id's own answer and the kinds", {
  expect_null(.ai_asm_now(NULL))
  expect_null(.ai_asm_now(tempfile()))
  d <- asm_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .ai_asm_now(d)
  expect_identical(a$id, c("A1", "A2", "A3"))
  expect_identical(attr(a, "kinds")$kind, c("K1", "K2"))
  expect_true(all(is.na(a$own_verdict)))
  tflspec::tfl_ai_assumption_answers_write(d, data.frame(id = c("K1", "A3"), verdict = c("wrong", "ok"),
                                                         correct = c("TRT01P", NA)))
  a <- .ai_asm_now(d)
  expect_identical(attr(a, "kinds")$verdict, c("wrong", NA))
  expect_identical(attr(a, "kinds")$correct, c("TRT01P", NA))
  expect_identical(a$own_verdict, c(NA, NA, "ok"))
  expect_identical(a$status, c("answered", "answered", "confirmed"))
})

test_that("only the answers changed are written; emptied ones removed", {
  ch <- .ai_asm_changes(c("K1", "A1", "A2", "A3"),
                        verdict = c("wrong", "", "", "ok"), correct = c("TRT01P", "", "x", "y"),
                        old_verdict = c("wrong", NA, NA, "wrong"), old_correct = c("TRT01P", NA, NA, "z"))
  expect_identical(ch$id, c("A2", "A3"))
  expect_identical(ch$verdict, c("wrong", "ok"))
  expect_identical(ch$correct, c("x", NA))
  ch <- .ai_asm_changes("A1", "", "", "ok", NA)
  expect_identical(ch$id, "A1")
  expect_true(is.na(ch$verdict) && is.na(ch$correct))
})

test_that("a kind's content, and the answers wrong with no right content", {
  r <- data.frame(item = c("arm", "Arm"), assumption = c("TRT01A", "TRT01A"))
  expect_identical(.ai_asm_kind_said(r[1, ]), "arm : TRT01A")
  expect_true(is.na(.ai_asm_kind_said(r)))
  ch <- .ai_asm_changes(c("K1", "A1"), c("wrong", "wrong"), c("", "x"), c(NA, NA), c(NA, NA))
  expect_identical(.ai_asm_missing_correct(ch), "K1")
})

test_that("the card saves a kind's answer and one report's own", {
  d <- asm_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  said <- character()
  shiny::testServer(function(input, output, session) {
    ver <- shiny::reactiveVal(0L)
    saved <- ai_asm_server(input, output, session, function() d, ver,
                           notify = function(x, ...) said <<- c(said, x),
                           guarded = function(x) x, t = function(x) x)
  }, {
    session$setInputs(asm_show = "open")
    expect_match(paste(as.character(output$asm$html), collapse = ""), "Actual or planned?", fixed = TRUE)
    session$setInputs(asm_v_K1 = "wrong", asm_c_K1 = "TRT01P", asm_v_K2 = "", asm_c_K2 = "",
                      asm_v_A1 = "", asm_c_A1 = "", asm_v_A2 = "ok", asm_c_A2 = "",
                      asm_v_A3 = "", asm_c_A3 = "")
    session$setInputs(asm_save = 1)
    expect_identical(saved(), 1L)
    x <- tflspec::tfl_ai_assumption_answers_read(d)
    expect_identical(x$id, c("K1", "A2"))
    a <- tflspec::tfl_ai_assumptions(d)
    expect_identical(a$status, c("answered", "confirmed", "open"))
    session$setInputs(asm_save = 2)
    expect_match(said[length(said)], "No answer changed.", fixed = TRUE)
    # the kind's answer shown in its reports' empty choice and field
    html <- paste(as.character(output$asm$html), collapse = "")
    expect_match(html, "(K1's answer: Wrong (wrong))", fixed = TRUE)
    expect_match(html, "K1: TRT01P", fixed = TRUE)
    # wrong with no right content: named, nothing written
    session$setInputs(asm_v_K2 = "wrong", asm_c_K2 = "")
    session$setInputs(asm_save2 = 1)
    expect_match(said[length(said)], "K2: when wrong", fixed = TRUE)
    expect_identical(saved(), 1L)
    # read again while an answer is typed: kept, a note instead
    ver(1L)
    session$flushReact()
    expect_match(paste(as.character(output$asm_stale$html), collapse = ""), "not saved", fixed = TRUE)
    session$setInputs(asm_reread = 1)
    expect_null(output$asm_stale)
  })
})
