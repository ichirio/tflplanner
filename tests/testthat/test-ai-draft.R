# AI drafts (#431): the adapters over tflspec's AI requests and answers,
# and the tab's steps 2 and 3

# (slow: a study is prepared with tflspec -- run on CI and locally with
# NOT_CRAN=true, not in CRAN's check)
skip_on_cran()
skip_if_not_installed("haven")

# A study prepared once for the file (tfl_ai_prepare() on tflspec's own
# example workbook and data, 4 requests of 1 report each); each test copies
# it into its own home.  tflspec's example answers beside it.
ai_example <- system.file("ai", "example", package = "tflspec")
ai_prepared <- local({
  d <- file.path(tempfile("ai-prep-"), "XYZ-1")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  utils::capture.output(suppressMessages(tflspec::tfl_ai_prepare(
    d, file.path(ai_example, "onco_toc_shell.xlsx"), study_id = "XYZ-1", size = 3)))
  d
})

# the prepared study in this test's home, registered: its folder
ai_study <- function(register = TRUE) {
  root <- studies_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.copy(ai_prepared, root, recursive = TRUE)
  path <- file.path(root, "XYZ-1")
  if (register) suppressMessages(register_study(path))
  path
}
ai_answer <- function(path, request, from = request) {
  file.copy(file.path(ai_example, "answers", paste0(from, ".md")),
            file.path(path, "ai", "answers", paste0(request, ".md")), overwrite = TRUE)
}
utf8_bytes <- function(f) readBin(f, "raw", file.info(f)$size)

test_that("the requests: one row each, in the order written, the reports from tflspec's record", {
  local_home()
  path <- ai_study(register = FALSE)
  r <- .ai_requests(path)
  expect_identical(r$request, c("demographics-1", "ae-1", "labs-1", "efficacy-1"))
  expect_identical(r$family, c("demographics", "ae", "labs", "efficacy"))
  expect_identical(r$outputs, c("T-14-1-1", "T-14-3-1", "T-14-3-5", "T-14-2-1"))
  expect_identical(r$n, rep(1L, 4))
  expect_identical(r$file, paste0("ai/requests/", r$request, ".md"))
  txt <- .ai_request_text(path, "ae-1")
  expect_identical(r$chars[2], nchar(sub("\n$", "", txt)))
  expect_match(txt, "T-14-3-1", fixed = TRUE)
  expect_true(all(is.na(r$answer)))
  expect_true(is.na(attr(r, "instructions")))
  # the instructions once: their file named; no requests: no rows
  writeLines("x", file.path(path, "ai", "requests", "00-instructions.md"))
  expect_identical(attr(.ai_requests(path), "instructions"), "ai/requests/00-instructions.md")
  expect_identical(nrow(.ai_requests(tempfile())), 0L)
})

test_that("an answer's file: <request>.md first, then the names Notepad saves", {
  local_home()
  path <- ai_study(register = FALSE)
  a <- file.path(path, "ai", "answers")
  writeLines("t", file.path(a, "ae-1.txt"))
  expect_identical(.ai_answer_file(path, "ae-1"), "ai/answers/ae-1.txt")
  writeLines("m", file.path(a, "ae-1.md.txt"))
  expect_identical(.ai_answer_file(path, "ae-1"), "ai/answers/ae-1.md.txt")
  writeLines("m", file.path(a, "ae-1.md"))
  expect_identical(.ai_answer_file(path, "ae-1"), "ai/answers/ae-1.md")
  expect_true(is.na(.ai_answer_file(path, "labs-1")))
  expect_identical(.ai_requests(path)$answer[2], "ai/answers/ae-1.md")
  expect_s3_class(.ai_requests(path)$answer_time, "POSIXct")
})

test_that("an answer saved: ai/answers/<request>.md, UTF-8 with no mark, LF; a name not a request's is refused", {
  local_home()
  path <- ai_study(register = FALSE)
  ja <- "\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3"
  f <- .ai_save_answer(path, "ae-1", paste0("```yaml\r\nlabel: ", ja, "\r\n```\r\n"))
  expect_identical(f, "ai/answers/ae-1.md")
  b <- utf8_bytes(file.path(path, f))
  expect_identical(b, charToRaw(enc2utf8(paste0("```yaml\nlabel: ", ja, "\n```\n"))))
  expect_false(identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
  expect_identical(.ai_answer_text(path, "ae-1"), paste0("```yaml\nlabel: ", ja, "\n```\n"))
  # saved again: over the old
  .ai_save_answer(path, "ae-1", "second")
  expect_identical(.ai_answer_text(path, "ae-1"), "second\n")
  expect_error(.ai_save_answer(path, "ae-1", "third", overwrite = FALSE), "already there")
  expect_error(.ai_save_answer(path, "ae-01", "x"), "one of the study's requests")
  expect_error(.ai_save_answer(path, "../ae-1", "x"), "A request's name")
  expect_error(.ai_save_answer(path, "ae-1", "  \n "), "not empty")
  expect_false(file.exists(file.path(path, "ai", "answers", "ae-01.md")))
})

test_that("an answer not in UTF-8 (Shift-JIS) is an error that says how to save it", {
  local_home()
  path <- ai_study(register = FALSE)
  f <- file.path(path, "ai", "answers", "ae-1.md")
  sjis <- iconv("\u89e3\u6790\u5bfe\u8c61", "UTF-8", "CP932", toRaw = TRUE)[[1L]]
  writeBin(c(charToRaw("label: "), sjis, charToRaw("\n")), f)
  expect_error(.ai_read_text(f), class = "tflplanner_ai_encoding")
  expect_error(.ai_read_text(f), "not UTF-8")
  x <- .ai_answer_text(path, "ae-1")
  expect_identical(as.vector(x), "")
  expect_match(attr(x, "problem"), "Shift-JIS")
  # a mark at the start (Notepad's UTF-8) is read without it
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a\r\nb")), f)
  expect_identical(.ai_answer_text(path, "ae-1"), "a\nb")
  # tflspec's check says it too
  writeBin(c(charToRaw("label: "), sjis, charToRaw("\n")), f)
  co <- .ai_collect(path)
  expect_identical(co$reports$status[co$reports$request == "ae-1"], "error")
})

test_that("the answers checked: tflspec's list (reports, next_steps, assumptions, stray)", {
  local_home()
  path <- ai_study(register = FALSE)
  ai_answer(path, "demographics-1")
  # an answer with a problem: its fix request written
  a <- readLines(file.path(ai_example, "answers", "ae-1.md"), encoding = "UTF-8")
  .ai_save_answer(path, "ae-1", sub("table_plan:", "table_plann:", a))
  # an answer of a name no request has
  ai_answer(path, "efficacy-01", from = "efficacy-1")
  co <- .ai_collect(path)
  expect_s3_class(co, "tfl_ai_collected")
  expect_true(all(c("reports", "next_steps", "assumptions", "stray", "time") %in% names(co)))
  r <- co$reports
  expect_true(all(c("request", "output_id", "status", "problems", "first", "file",
                    "repair", "problems_text") %in% names(r)))
  expect_identical(r$status, c("ok", "error", "no answer", "no answer"))
  expect_identical(r$file, c("spec/outputs/T-14-1-1.yml", NA, NA, NA))
  expect_identical(r$repair, c(NA, "ai/repairs/ae-1.md", NA, NA))
  expect_match(.ai_request_text(path, "ae-1", "repair"), "table_plan", fixed = TRUE)
  expect_identical(names(co$next_steps), r$request)
  expect_match(co$next_steps[["ae-1"]], "SAME chat")
  expect_identical(co$stray, "ai/answers/efficacy-01.md")
  expect_identical(names(co$assumptions), c("output_id", "where", "item", "assumption", "question"))
  expect_true("T-14-1-1" %in% co$assumptions$output_id)
  expect_true(file.exists(file.path(path, "spec", "outputs", "T-14-1-1.yml")))
  expect_false(file.exists(file.path(path, "spec", "outputs", "T-14-3-1.yml")))
  # a request's status: its worst report's
  expect_identical(.ai_request_status(c("ae-1", "labs-1", "x"), r), c("error", "no answer", NA))
  expect_identical(.ai_status_label(c("demographics-1", "ae-1", "labs-1"), r),
                   c("ok 1", "error 1", "no answer"))
  n <- .ai_counts(.ai_requests(path), co)
  expect_identical(n$answers, 2L)
  expect_identical(unname(n$status), c(1L, 0L, 2L, 1L))
})

test_that("after a check the study is read again from its files; never over unsaved changes", {
  local_home()
  path <- ai_study()
  for (q in c("demographics-1", "ae-1")) ai_answer(path, q)
  .ai_collect(path)
  expect_error(.ai_reload_study("XYZ-1", unsaved = TRUE), class = "tflplanner_ai_unsaved")
  s <- .ai_reload_study("XYZ-1", unsaved = FALSE)
  expect_true(all(c("T-14-1-1", "T-14-3-1") %in% output_ids(s$planner)))
  # the new files' fingerprints recorded: no save sees them as made outside
  expect_true(all(spec_status("XYZ-1")$status == "same"))
})

# -- the tab (shiny::testServer on the whole app) ------------------------

test_that("the tab is off unless asked for", {
  local_home()
  old <- options(tflplanner.ai_drafts = NULL)
  on.exit(options(old))
  expect_false(grepl("ai-requests", as.character(app_ui("en")), fixed = TRUE))
  options(tflplanner.ai_drafts = TRUE)
  expect_true(grepl("ai-requests", as.character(app_ui("en")), fixed = TRUE))
})

test_that("step 2 and 3: the list, View, save an answer, check all, the reports taken in", {
  local_home()
  path <- ai_study()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    rv <- session$userData$rv
    ai <- session$userData$ai
    expect_identical(ai$requests()$request, c("demographics-1", "ae-1", "labs-1", "efficacy-1"))
    expect_match(output$`ai-status_line`$html, "4 requests", fixed = TRUE)
    # the request's text, as the copy and View give it
    expect_identical(ai$text("ae-1"), .ai_request_text(path, "ae-1"))
    # an answer pasted and saved
    a <- paste(readLines(file.path(ai_example, "answers", "ae-1.md"), encoding = "UTF-8"),
               collapse = "\n")
    session$setInputs(`ai-pick` = "ae-1")
    session$setInputs(`ai-answer_text` = a, `ai-save` = 1)
    expect_identical(.ai_answer_text(path, "ae-1"), paste0(a, "\n"))
    expect_identical(ai$requests()$answer[2], "ai/answers/ae-1.md")
    # unsaved changes: no check
    rv$meta$title <- "mine"
    expect_true(isTRUE(session$userData$dirty()))
    session$setInputs(`ai-check` = 1)
    expect_null(ai$collected())
    expect_identical(rv$meta$title, "mine")
    expect_false(file.exists(file.path(path, "spec", "outputs", "T-14-3-1.yml")))
    # saved: the check writes the report, the study read again
    rv$meta$title <- rv$saved_meta$title
    session$setInputs(`ai-check` = 2)
    co <- ai$collected()
    expect_identical(co$reports$status[co$reports$request == "ae-1"], "ok")
    expect_true("T-14-3-1" %in% output_ids(rv$p))
    expect_false(isTRUE(session$userData$dirty()))
    expect_true(all(spec_status("XYZ-1")$status == "same"))
    expect_match(output$`ai-result`$html, "T-14-3-1", fixed = TRUE)
  })
})
