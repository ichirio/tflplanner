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
    # (a report's own answer read only once its kind's reports are drawn)
    session$setInputs(asm_open = list(kind = "K1", n = 1))
    session$setInputs(asm_save = 1)
    expect_identical(saved(), 1L)
    x <- tflspec::tfl_ai_assumption_answers_read(d)
    expect_identical(x$id, c("K1", "A2"))
    a <- tflspec::tfl_ai_assumptions(d)
    expect_identical(a$status, c("answered", "confirmed", "open"))
    session$setInputs(asm_save = 2)
    expect_match(said[length(said)], "No answer changed.", fixed = TRUE)
    # the kind opened before: drawn open with its reports, its answer shown
    # in their empty choice and field
    html <- paste(as.character(output$asm$html), collapse = "")
    expect_match(html, "(K1's answer: Wrong (wrong))", fixed = TRUE)
    expect_match(html, "K1: TRT01P", fixed = TRUE)
    expect_false(grepl("asm_v_A3", html, fixed = TRUE))
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

test_that("an assumption's value: given, else followed in its report's YAML", {
  y <- list(page = list(titles = list(`1` = list(center = "Table 14.1.1"), `2` = list(center = "Demographics"))),
            codelists = list(SEX = list(list(value = "M", label = "Male"), list(value = "F", label = "Female"))),
            ard = list(analyses = list(list(id = "AGE", method = "continuous", variables = "AGE"))))
  expect_identical(.ai_asm_value_text(.ai_asm_follow(y, "page", "titles")),
                   "1: center: Table 14.1.1
2: center: Demographics")
  expect_identical(.ai_asm_value_text(.ai_asm_follow(y, "codelists", "sex")),
                   "value: M, label: Male
value: F, label: Female")
  expect_identical(.ai_asm_value_text(.ai_asm_follow(y, "ard.analyses", "AGE")),
                   "id: AGE
method: continuous
variables: AGE")
  expect_null(.ai_asm_follow(y, "ard.analyses", "arm"))
  expect_null(.ai_asm_follow(y, "spec/data.yml populations", "SAF"))
  expect_identical(.ai_asm_value_text(as.list(letters[1:5])), "a
b
c
…")
  d <- asm_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "ai", "spec", "outputs"), recursive = TRUE)
  yaml::write_yaml(y, file.path(d, "ai", "spec", "outputs", "T-3.yml"))
  a <- .ai_asm_now(d)
  expect_identical(a$value, c(NA, NA, "1: center: Table 14.1.1
2: center: Demographics"))
  expect_identical(attr(a, "kinds")$value, c(NA, "1: center: Table 14.1.1
2: center: Demographics"))
  # the AI's own value first
  a$value <- c("TRT01A", NA, "")
  expect_identical(.ai_asm_values(d, a), c("TRT01A", NA, "1: center: Table 14.1.1
2: center: Demographics"))
})

test_that("the AI's value in spec/_assumptions.yml comes first", {
  d <- asm_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # (the ids written first: tflspec rewrites the file without `value` when
  # it gives them)
  tflspec::tfl_ai_assumptions(d)
  rows <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  rows[[3]]$value <- "Table 14.3.1 / Summary of Adverse Events"
  writeLines(yaml::as.yaml(rows), file.path(d, "spec", "_assumptions.yml"))
  a <- .ai_asm_now(d)
  expect_identical(a$value, c(NA, NA, "Table 14.3.1 / Summary of Adverse Events"))
  # (kept when the ids are written into the file)
  expect_identical(.ai_asm_now(d)$value[3], "Table 14.3.1 / Summary of Adverse Events")
})

test_that("the reports waiting for corrections: an assumption answered wrong, not yet applied", {
  d <- asm_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_identical(.ai_to_correct(d, c("T-1", "T-2", "T-3")), character())
  tflspec::tfl_ai_assumption_answers_write(d, data.frame(id = c("K1", "A3"), verdict = c("wrong", "ok"),
                                                         correct = c("TRT01P", NA)))
  expect_identical(.ai_to_correct(d, c("T-1", "T-2", "T-3")), c("T-1", "T-2"))
  expect_identical(.ai_to_correct(d, "T-3"), character())
  expect_identical(.ai_to_correct(NULL, "T-1"), character())
})

test_that("the Report list marks a report adopted from the AI, and a draft changed since", {
  d <- withr_tempdir()
  dir.create(file.path(d, "ai", "spec", "outputs"), recursive = TRUE)
  expect_identical(unname(.ai_adopted_marks(d, c("T-1", "T-2"))), c("", ""))
  f <- file.path(d, "ai", "spec", "outputs", c("T-1.yml", "T-2.yml"))
  writeLines("id: T-1", f[1])
  writeLines("id: T-2", f[2])
  utils::write.csv(data.frame(output_id = c("T-1", "T-2"), adopted = "2026-10-29 03:00:00",
                              md5 = unname(tools::md5sum(f))),
                   file.path(d, "ai", "adopted.csv"), row.names = FALSE)
  writeLines("id: T-2\nlabel: redrafted", f[2])
  m <- .ai_adopted_marks(d, c("T-1", "T-2", "T-3"))
  expect_identical(unname(m), c("adopted", "newer", ""))
  h <- .ai_mark_html(m, function(x) x)
  expect_match(h[1], "text-bg-light", fixed = TRUE)
  expect_match(h[2], "the draft changed since", fixed = TRUE)
  expect_identical(h[3], "")
  expect_identical(unname(.ai_adopted_marks(NULL, "T-1")), "")
})

test_that("the review says each AI assumption not answered (I01), and goes to step 4", {
  d <- asm_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  r <- .rule_i01(list(path = d))
  expect_identical(r$rule, rep("I01", 3L))
  expect_setequal(r$output_id, c("T-1", "T-2", "T-3"))
  expect_setequal(r$row, c("A1", "A2", "A3"))
  expect_match(r$message[r$row == "A1"], "AI assumption A1 is unanswered", fixed = TRUE)
  # answered: gone (the files changed, read again)
  Sys.sleep(1.1)
  tflspec::tfl_ai_assumption_answers_write(d, data.frame(id = "K1", verdict = "ok"))
  r <- .rule_i01(list(path = d))
  expect_identical(r$row, "A3")
  expect_identical(.review_target(r[1, ], new_planner())$nav, list(nav = "ai", `ai-step` = "list"))
  expect_null(.rule_i01(list(path = tempfile())))
})
