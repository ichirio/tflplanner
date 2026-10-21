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
  # the example's KM figure reads ADTTE: the sample study's
  file.copy(system.file("sample", "SAMPLE-01", "data", "adam", "adtte.rds", package = "tflplanner"),
            file.path(d, "adam"))
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
  expect_identical(r$request, c("demographics-1", "ae-1", "labs-1", "efficacy-1", "figure-1"))
  expect_identical(r$family, c("demographics", "ae", "labs", "efficacy", "figure"))
  expect_identical(r$outputs, c("T-14-1-1", "T-14-3-1", "T-14-3-5", "T-14-2-1", "F-14-2-2"))
  expect_identical(r$n, rep(1L, 5))
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
  expect_identical(r$status, c("ok", "error", "no answer", "no answer", "no answer"))
  expect_identical(r$file, c("spec/outputs/T-14-1-1.yml", NA, NA, NA, NA))
  expect_identical(r$repair, c(NA, "ai/repairs/ae-1.md", NA, NA, NA))
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
  expect_identical(unname(n$status), c(1L, 0L, 3L, 1L))
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

test_that("the tab is there unless turned off", {
  local_home()
  old <- options(tflplanner.ai_drafts = NULL)
  on.exit(options(old))
  expect_true(grepl("ai-requests", as.character(app_ui("en")), fixed = TRUE))
  options(tflplanner.ai_drafts = FALSE)
  expect_false(grepl("ai-requests", as.character(app_ui("en")), fixed = TRUE))
})

test_that("step 2 and 3: the list, View, save an answer, check all, the reports taken in", {
  local_home()
  path <- ai_study()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    rv <- session$userData$rv
    ai <- session$userData$ai
    expect_identical(ai$requests()$request,
                     c("demographics-1", "ae-1", "labs-1", "efficacy-1", "figure-1"))
    expect_match(output$`ai-status_line`$html, "5 requests", fixed = TRUE)
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
    # the answers compared, under the checks (tflspec >= 0.0.24.9191)
    if (exists("tfl_ai_compare", envir = asNamespace("tflspec"))) {
      expect_match(output$`ai-compare_card`$html, "compare.csv", fixed = TRUE)
      expect_true(file.exists(file.path(path, "ai", "compare.csv")))
    }
  })
})

# -- step 1: the requests prepared --------------------------------------------

ai_toc <- file.path(ai_example, "onco_toc_shell.xlsx")
ai_adam <- normalizePath(file.path(ai_prepared, "adam"), "/")

test_that("a dry run reads the TOC and the ADaM summary, and writes nothing", {
  local_home()
  dir <- file.path(studies_root(), "DRY-1")
  p <- .ai_prepare(dir, ai_toc, ai_adam, list(size = 3L), dry_run = TRUE)
  expect_s3_class(p, "tfl_ai_prepared")
  expect_true(p$dry_run)
  expect_identical(sort(p$toc$output_id), c("F-14-2-2", "T-14-1-1", "T-14-2-1", "T-14-3-1", "T-14-3-5"))
  expect_true(all(p$toc$shell_found))
  expect_identical(sort(p$adam$datasets$dataset), c("ADAE", "ADLB", "ADRS", "ADSL", "ADTTE"))
  expect_identical(nrow(p$requests), 5L)
  expect_false(dir.exists(dir))
  # the TOC sheet named wrong: the TOC of that sheet, one report
  p2 <- .ai_prepare(dir, ai_toc, ai_adam, list(toc_sheet = "T14.1.1"), dry_run = TRUE)
  expect_identical(nrow(p2$toc), 1L)
  expect_error(.ai_prepare(dir, ai_toc, file.path(tempdir(), "no-such-adam")), "No ADaM folder")
  expect_identical(.ai_adam_files(ai_adam), c("adae.xpt", "adlb.xpt", "adrs.xpt", "adsl.xpt", "adtte.rds"))
  expect_null(.ai_adam_files(file.path(tempdir(), "no-such-adam")))
})

test_that("a new study: written by tfl_ai_prepare(), then registered; its ID checked first", {
  local_home()
  expect_match(.ai_new_study_problem(""), "Give the study an ID")
  expect_match(.ai_new_study_problem("a b"), "letters, digits")
  up <- file.path(tempdir(), "0.xlsx")
  file.copy(ai_toc, up, overwrite = TRUE)
  out <- .ai_new_study("NEW-1", up, "onco_toc_shell.xlsx", ai_adam,
                       list(size = 3L, datasets = c("ADSL", "ADAE")), title = "A new study")
  s <- out$study
  expect_s3_class(s, "rtfstudy")
  expect_identical(s$meta$study_id, "NEW-1")
  expect_identical(s$meta$title, "A new study")
  expect_true("NEW-1" %in% list_studies()$study_id)
  # the workbook kept beside the requests, under its own name
  expect_true(file.exists(file.path(s$path, "ai", "onco_toc_shell.xlsx")))
  expect_identical(.ai_requests(s$path)$request, out$prepared$requests$request)
  # only the datasets ticked are sent
  sent <- paste(readLines(file.path(s$path, "ai", "adam_summary.md"), encoding = "UTF-8"),
                collapse = "\n")
  expect_match(sent, "ADAE", fixed = TRUE)
  expect_no_match(sent, "ADLB", fixed = TRUE)
  expect_true(file.exists(file.path(s$path, "spec", "data.yml")))
  expect_match(.ai_new_study_problem("NEW-1"), "already registered")
  # a failure leaves no folder behind
  expect_error(.ai_new_study("NEW-2", up, "x.xlsx", file.path(tempdir(), "no-such-adam")))
  expect_false(dir.exists(file.path(studies_root(), "NEW-2")))
})

test_that("the requests written into a study open: its study.yml and spec kept", {
  local_home()
  path <- ai_study()
  before <- tools::md5sum(file.path(path, c("study.yml", "spec/data.yml")))
  unlink(file.path(path, "ai", "requests"), recursive = TRUE)
  p <- .ai_write_requests(path, ai_toc, "onco_toc_shell.xlsx", ai_adam,
                          list(size = 2L, instructions = "once"))
  expect_false(p$dry_run)
  expect_identical(unname(tools::md5sum(file.path(path, c("study.yml", "spec/data.yml")))),
                   unname(before))
  r <- .ai_requests(path)
  expect_identical(nrow(r), nrow(p$requests))
  expect_identical(attr(r, "instructions"), "ai/requests/00-instructions.md")
  expect_true(all(spec_status("XYZ-1")$status == "same"))
  # the templates listed (the default), or not (templates = FALSE)
  first <- function() readLines(file.path(path, "ai", "requests", paste0(r$request[1], ".md")), 1L)
  expect_identical(first(), "<!-- templates: TRUE -->")
  .ai_write_requests(path, ai_toc, "onco_toc_shell.xlsx", ai_adam,
                     list(size = 2L, instructions = "once", templates = FALSE))
  expect_false(identical(first(), "<!-- templates: TRUE -->"))
  # the study's ADaM folder, from its data catalog
  s <- open_study("XYZ-1")
  expect_identical(.ai_adam_default(s), normalizePath(file.path(path, "adam"), "/"))
})

test_that("step 1 in the app: read, then a new study written, registered and open; step 2 lists its requests", {
  local_home()
  ai_study()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    rv <- session$userData$rv
    ai <- session$userData$ai
    session$setInputs(`ai-study_mode` = "new", `ai-new_id` = "NEW-9", `ai-new_title` = "Nine",
                      `ai-toc_path` = ai_toc, `ai-adam_dir` = ai_adam, `ai-size` = 3,
                      `ai-language` = "English", `ai-instructions` = "each",
                      `ai-workbook` = "text", `ai-max_levels` = 15)
    expect_null(ai$prepared())
    session$setInputs(`ai-read` = 1)
    p <- ai$prepared()
    expect_true(p$dry_run)
    expect_identical(nrow(p$toc), 5L)
    expect_false(dir.exists(file.path(studies_root(), "NEW-9")))
    expect_match(output$`ai-toc_head`$html, "5 reports, 5 with a shell", fixed = TRUE)
    session$setInputs(`ai-datasets` = c("ADSL", "ADAE"), `ai-write` = 1)
    expect_identical(rv$study$meta$study_id, "NEW-9")
    expect_false(ai$prepared()$dry_run)
    expect_identical(nrow(ai$requests()), 5L)
    # the open study again: Write asks first (its requests are there)
    session$setInputs(`ai-study_mode` = "open", `ai-read` = 2)
    expect_true(ai$prepared()$dry_run)
  })
})

test_that("the workbook by its path: checked, then copied into the study's ai/ like an upload", {
  local_home()
  expect_null(.ai_toc_problem(ai_toc))
  expect_match(.ai_toc_problem(file.path(tempdir(), "none.xlsx")), "No file")
  expect_match(.ai_toc_problem(""), "Choose the TOC")
  txt <- tempfile(fileext = ".txt")
  writeLines("x", txt)
  expect_match(.ai_toc_problem(txt), ".xlsx or .xlsm")
  # a workbook in a folder of its own, outside the studies
  d <- file.path(tempdir(), "toc-elsewhere")
  dir.create(d, showWarnings = FALSE)
  file.copy(ai_toc, file.path(d, "TOC_SHELL.xlsx"), overwrite = TRUE)
  out <- .ai_new_study("PATH-1", file.path(d, "TOC_SHELL.xlsx"), "TOC_SHELL.xlsx", ai_adam,
                       list(size = 3L))
  kept <- file.path(out$study$path, "ai", "TOC_SHELL.xlsx")
  expect_true(file.exists(kept))
  expect_identical(unname(tools::md5sum(kept)), unname(tools::md5sum(ai_toc)))
  expect_error(.ai_new_study("PATH-2", file.path(d, "gone.xlsx"), "gone.xlsx", ai_adam),
               "No file")
  expect_false(dir.exists(file.path(studies_root(), "PATH-2")))
})

test_that("step 1 in the app: the path given after an upload is the one read", {
  local_home()
  ai_study()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    session$setInputs(`ai-toc_path` = file.path(tempdir(), "missing.xlsx"))
    expect_match(output$`ai-toc_check`$html, "No file", fixed = TRUE)
    session$setInputs(`ai-toc_path` = ai_toc)
    expect_match(output$`ai-toc_check`$html, "onco_toc_shell.xlsx", fixed = TRUE)
  })
})

test_that("Browse... fills the path from the computer's dialog; cancelled, nothing changes", {
  local_home()
  ai_study()
  old <- options(tflplanner.ai_drafts = TRUE,
                 tflplanner.choose_dir = function(default) ai_adam,
                 tflplanner.choose_file = function(default) character())
  on.exit(options(old))
  expect_identical(.ai_choose_dir(), ai_adam)
  expect_true(is.na(.ai_choose_file()))
  shiny::testServer(server_for("XYZ-1"), {
    session$setInputs(`ai-adam_dir` = "", `ai-toc_path` = "")
    session$setInputs(`ai-adam_browse` = 1)
    # (the field is set on the page; what it sends back is the typed value)
    session$setInputs(`ai-toc_browse` = 1)
    expect_identical(input$`ai-toc_path`, "")
  })
  # no dialog on a server
  options(tflplanner.file_dialog = FALSE)
  expect_false(.ai_dialog_ok())
  expect_false(grepl("ai-adam_browse", as.character(app_ui("en")), fixed = TRUE))
  options(tflplanner.file_dialog = NULL)
})

# -- a coding agent (mode = "agent") -------------------------------------------

ai_agent_questions <- function(path) {
  f <- file.path(path, "ai", "questions.md")
  cat(c("", "## Q1. Is TRTEMFL the treatment-emergent flag?", "",
        "- Reports: T-14-3-1, T-14-3-5", "- Where: ard.analyses",
        "- The agent took, for now: TRTEMFL == \"Y\"", "- Status: decided", "- Answer: Yes", "",
        "## Q2. Which visit is the baseline?", "",
        "- Reports: T-14-3-5", "- Where: ard.analyses[1].where",
        "- The agent took, for now: ABLFL == \"Y\"", "- Status: open", "- Answer:", ""),
      file = f, sep = "\n", append = TRUE)
  f
}

test_that("agent mode: AGENTS.md written, the agent's first message, the questions read and answered", {
  local_home()
  out <- .ai_new_study("AG-1", ai_toc, "onco_toc_shell.xlsx", ai_adam,
                       list(size = 3L, mode = "agent"))
  path <- out$study$path
  expect_identical(.ai_mode(path), "agent")
  expect_identical(.ai_mode(ai_prepared), "chat")
  expect_identical(.ai_agent_prompt(path), out$prepared$agent_prompt)
  expect_identical(nrow(.ai_questions(path)), 0L)
  ai_agent_questions(path)
  q <- .ai_questions(path)
  expect_identical(q$id, c("Q2", "Q1"))
  expect_identical(q$status, c("open", "decided"))
  expect_identical(q$output_id[2], "T-14-3-1, T-14-3-5")
  .ai_answer_questions(path, c(Q2 = "The last visit\nbefore the first dose"))
  q <- .ai_questions(path)
  expect_identical(q$answer[1], "The last visit\nbefore the first dose")
  expect_identical(q$answer[2], "Yes")
})

test_that("agent mode in the app: the first message on step 2, the questions on step 3 written back", {
  local_home()
  out <- .ai_new_study("AG-2", ai_toc, "onco_toc_shell.xlsx", ai_adam,
                       list(size = 3L, mode = "agent"))
  path <- out$study$path
  ai_agent_questions(path)
  writeLines(c("# Status", "", "- T-14-1-1: ok"), file.path(path, "ai", "status.md"))
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("AG-2"), {
    ai <- session$userData$ai
    expect_identical(ai$mode(), "agent")
    expect_identical(output$`ai-mode_now`, "agent")
    expect_match(output$`ai-agent_card`$html, "AGENTS.md", fixed = TRUE)
    expect_identical(ai$text("agent", "agent"), .ai_agent_prompt(path))
    expect_match(output$`ai-status_md`$html, "T-14-1-1: ok", fixed = TRUE)
    expect_match(output$`ai-questions`$html, "Which visit is the baseline?", fixed = TRUE)
    # nothing typed: nothing written
    session$setInputs(`ai-qa_Q2` = "", `ai-qa_Q1` = "Yes", `ai-questions_write` = 1)
    expect_true(is.na(.ai_questions(path)$answer[1]))
    session$setInputs(`ai-qa_Q2` = "ABLFL", `ai-questions_write` = 2)
    expect_identical(.ai_questions(path)$answer[1], "ABLFL")
    expect_identical(.ai_questions(path)$answer[2], "Yes")
    expect_match(output$`ai-tell_agent`$html, "data-what=\"tell\"", fixed = TRUE)
    expect_match(ai$text("tell", "tell"), "ai/questions.md", fixed = TRUE)
  })
})

# -- several workbooks, and requests added (tflspec >= 0.0.24.9178) ----------

# the example split in two (its TOC alone, its shells alone), and a second
# TOC of one more table with its shell (a sheet named as one of the first's)
ai_split_books <- function(dir) {
  wb <- openxlsx::loadWorkbook(ai_toc)
  for (s in setdiff(names(wb), "TOC")) openxlsx::removeWorksheet(wb, s)
  openxlsx::saveWorkbook(wb, file.path(dir, "toc.xlsx"), overwrite = TRUE)
  wb <- openxlsx::loadWorkbook(ai_toc)
  openxlsx::removeWorksheet(wb, "TOC")
  openxlsx::saveWorkbook(wb, file.path(dir, "shells.xlsx"), overwrite = TRUE)
  file.path(dir, c("toc.xlsx", "shells.xlsx"))
}
ai_more_book <- function(dir) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = "T-14-3-2", Type = "Table", Title = "Serious Adverse Events by System Organ Class",
    Population = "Safety Population"))
  openxlsx::addWorksheet(wb, "Safety")
  openxlsx::writeData(wb, "Safety", c("Table 14.3.2", "Serious Adverse Events by System Organ Class",
                                      "(Safety Population)", "", "System Organ Class | Drug X | Control"))
  f <- file.path(dir, "more.xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

test_that("the shells in a workbook of their own: read, kept in ai/, each request's workbook", {
  local_home()
  d <- file.path(tempdir(), "ai-books")
  dir.create(d, showWarnings = FALSE)
  f <- ai_split_books(d)
  p <- .ai_prepare(file.path(studies_root(), "BK-0"), f[1L], ai_adam, list(), dry_run = TRUE,
                   shells = f[2L])
  expect_identical(unique(p$toc$book), "shells.xlsx")
  expect_true(all(p$toc$shell_found))
  out <- .ai_new_study("BK-1", f[1L], "toc.xlsx", ai_adam, list(size = 3L),
                       shells = list(list(path = f[2L], name = "shells.xlsx")))
  path <- out$study$path
  expect_true(all(file.exists(file.path(path, "ai", c("toc.xlsx", "shells.xlsx")))))
  r <- .ai_requests(path)
  expect_true("book" %in% names(r))
  expect_identical(unique(r$book), "shells.xlsx")
})

test_that("requests added: the next names, the answers kept; a report asked again only with overwrite", {
  local_home()
  path <- ai_study()
  d <- file.path(tempdir(), "ai-more")
  dir.create(d, showWarnings = FALSE)
  m <- ai_more_book(d)
  ai_answer(path, "ae-1")
  own <- file.path(path, c("study.yml", "spec/data.yml", "spec/defaults.yml"))
  before <- tools::md5sum(own)
  n0 <- nrow(.ai_requests(path))
  # the dry run says nothing is asked for twice; then added
  p <- .ai_prepare(path, m, ai_adam, list(add = TRUE), dry_run = TRUE)
  expect_identical(nrow(.ai_already_asked(path, p$toc$output_id)), 0L)
  w <- .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE))
  r <- .ai_requests(path)
  expect_identical(nrow(r), n0 + 1L)
  expect_identical(r$request[nrow(r)], "ae-2")
  expect_identical(r$book[nrow(r)], "more.xlsx")
  expect_true(file.exists(file.path(path, "ai", "answers", "ae-1.md")))
  expect_true(file.exists(file.path(path, "ai", "more.xlsx")))
  # the same workbook again: asked for already
  again <- .ai_already_asked(path, "T-14-3-2")
  expect_identical(again$request, "ae-2")
  e <- tryCatch(.ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE)),
                error = function(e) conditionMessage(e))
  expect_true(.ai_is_asked_again(e))
  expect_match(e, "T-14-3-2 already asked for (ae-2)", fixed = TRUE)
  # written again (overwrite): one request for it, the study's own files kept
  .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE, overwrite = TRUE))
  r2 <- .ai_requests(path)
  expect_identical(sum(r2$outputs == "T-14-3-2"), 1L)
  expect_identical(nrow(r2), nrow(r))
  expect_identical(unname(tools::md5sum(own)), unname(before))
})

test_that("an agent's study: a request asked again keeps the study's files and the agent's", {
  local_home()
  out <- .ai_new_study("AG-4", ai_toc, "onco_toc_shell.xlsx", ai_adam,
                       list(size = 3L, mode = "agent"))
  path <- out$study$path
  d <- file.path(tempdir(), "ai-more-agent")
  dir.create(d, showWarnings = FALSE)
  m <- ai_more_book(d)
  .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE, mode = "agent"))
  own <- file.path(path, c("study.yml", "spec/data.yml", "spec/defaults.yml", "AGENTS.md", "run.R"))
  before <- tools::md5sum(own)
  .ai_write_requests(path, m, "more.xlsx", ai_adam,
                     list(add = TRUE, overwrite = TRUE, mode = "agent"))
  expect_identical(unname(tools::md5sum(own)), unname(before))
  expect_identical(sum(.ai_requests(path)$outputs == "T-14-3-2"), 1L)
})

test_that("the agent's first message: ai/agent_prompt.md when the study has it", {
  local_home()
  out <- .ai_new_study("AG-3", ai_toc, "onco_toc_shell.xlsx", ai_adam,
                       list(size = 3L, mode = "agent"))
  path <- out$study$path
  expect_true(file.exists(file.path(path, "ai", "agent_prompt.md")))
  expect_identical(.ai_agent_prompt(path), out$prepared$agent_prompt)
  writeLines("Follow the steps.", file.path(path, "ai", "agent_prompt.md"))
  expect_identical(.ai_agent_prompt(path), "Follow the steps.")
})

test_that("step 1 in the app: a workbook added to the requests; asked again, written with overwrite", {
  local_home()
  path <- ai_study()
  d <- file.path(tempdir(), "ai-more-app")
  dir.create(d, showWarnings = FALSE)
  m <- ai_more_book(d)
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    ai <- session$userData$ai
    n0 <- nrow(ai$requests())
    session$setInputs(`ai-study_mode` = "open", `ai-toc_path` = m, `ai-adam_dir` = ai_adam,
                      `ai-add` = TRUE, `ai-size` = 3, `ai-language` = "English",
                      `ai-instructions` = "each", `ai-workbook` = "text", `ai-max_levels` = 15,
                      `ai-mode` = "chat")
    session$setInputs(`ai-read` = 1)
    expect_match(output$`ai-prep_status`$html, "Added to the", fixed = TRUE)
    session$setInputs(`ai-write` = 1)
    r <- ai$requests()
    expect_identical(nrow(r), n0 + 1L)
    expect_match(output$`ai-requests`, "more.xlsx", fixed = TRUE)
    # read again: asked for already, said; written again on overwrite
    session$setInputs(`ai-read` = 2)
    expect_match(output$`ai-prep_status`$html, "T-14-3-2 (ae-2)", fixed = TRUE)
    session$setInputs(`ai-write` = 2)
    expect_identical(nrow(ai$requests()), n0 + 1L)
    session$setInputs(`ai-write_overwrite` = 1)
    expect_identical(sum(ai$requests()$outputs == "T-14-3-2"), 1L)
    expect_identical(nrow(ai$requests()), n0 + 1L)
  })
})

test_that("a study made with the ADaM outside its folder: its data found, no D01 for a report taken in", {
  local_home()
  out <- .ai_new_study("ABS-1", ai_toc, "onco_toc_shell.xlsx", ai_adam, list(size = 3L))
  path <- out$study$path
  # the catalog's paths are the ADaM folder's own (absolute)
  expect_true(all(startsWith(out$study$planner$ard$datasets$path, ai_adam)))
  ai_answer(path, "ae-1")
  .ai_collect(path)
  s <- .ai_reload_study("ABS-1", unsaved = FALSE)
  expect_true("T-14-3-1" %in% output_ids(s$planner))
  r <- study_review(s, data = "read", lang = "en")
  expect_false(any(r$rule %in% c("D01", "D02")),
               info = paste(r$rule, r$output_id, r$message, collapse = "\n"))
  expect_false(any(r$output_id %in% "T-14-3-1" & r$level == "error"),
               info = paste(r$rule, r$output_id, r$message, collapse = "\n"))
})

test_that("an ADaM folder inside the study is passed relative; a folder with only its data can become the study", {
  d <- file.path(tempfile("adamrel"), "ABC-1")
  dir.create(file.path(d, "data", "adam"), recursive = TRUE)
  on.exit(unlink(dirname(d), recursive = TRUE), add = TRUE)
  inside <- normalizePath(file.path(d, "data", "adam"), "/")
  expect_identical(.ai_adam_in_study(inside, d), "data/adam")
  expect_identical(.ai_adam_in_study(paste0(inside, "/"), d), "data/adam")
  outside <- normalizePath(tempdir(), "/")
  expect_identical(.ai_adam_in_study(outside, d), outside)
  # a sibling folder that only starts with the study's name stays absolute
  sib <- paste0(normalizePath(d, "/"), "-other/adam")
  expect_identical(.ai_adam_in_study(sib, d), sib)
  # the new study's ID: a folder holding only data is fine, a study is not
  home <- file.path(dirname(d), "home")
  expect_null(.ai_new_study_problem("ABC-1", root = dirname(d), home = home))
  writeLines("study_id: ABC-1", file.path(d, "study.yml"))
  expect_match(.ai_new_study_problem("ABC-1", root = dirname(d), home = home), "already there")
})
