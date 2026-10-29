# AI drafts (#431): the adapters over tflspec's AI requests and the
# agent's SPEC drafts (ai/spec/outputs), and the tab's steps 2 to 4

# (slow: a study is prepared with tflspec -- run on CI and locally with
# NOT_CRAN=true, not in CRAN's check)
skip_on_cran()
skip_if_not_installed("haven")

# A study prepared once for the file (tfl_ai_prepare() on tflspec's own
# example workbook and data; a request a shell
# sheet -- demographics-1, efficacy-1, ae-1 (the Safety sheet: T-14-3-1 and
# T-14-3-5), figure-1); each test copies it into its own home.  tflspec's
# example drafts beside it (drafts/<id>.yml).
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
  # (as the AI tab prepares it, .ai_prepare(): a new study's drafts in the
  # AI's area, ai/spec/outputs, with a tflspec that has it)
  utils::capture.output(suppressMessages(tflspec::tfl_ai_prepare(
    d, file.path(ai_example, "onco_toc_shell.xlsx"), study_id = "XYZ-1", size = 3)))
  d
})

# where a study's AI drafts are (from its folder): the AI's area when it
# has one (tflspec >= #488), else the study's spec/outputs
ai_drafts <- function(path) {
  if (dir.exists(file.path(path, "ai", "spec", "outputs"))) "ai/spec/outputs" else "spec/outputs"
}

# the drafts adopted into the study (step 4), when they are apart
ai_adopt_all <- function(path) {
  if (identical(ai_drafts(path), "ai/spec/outputs")) {
    utils::capture.output(suppressMessages(tflspec::tfl_ai_adopt(path)))
  }
  invisible(path)
}

# the prepared study in this test's home, registered: its folder
ai_study <- function(register = TRUE) {
  root <- studies_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.copy(ai_prepared, root, recursive = TRUE)
  path <- file.path(root, "XYZ-1")
  if (register) suppressMessages(register_study(path))
  path
}
# the agent's draft of a report, as it writes it (ai/spec/outputs/<id>.yml):
# tflspec's example draft
ai_draft <- function(path, id, from = id) {
  d <- file.path(path, "ai", "spec", "outputs")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(ai_example, "drafts", paste0(from, ".yml")), file.path(d, paste0(id, ".yml")),
            overwrite = TRUE)
}
# the drafts of a request's reports
ai_drafts_of <- function(path, request) {
  r <- .ai_requests(path)
  for (id in trimws(strsplit(r$outputs[r$request == request], ",", fixed = TRUE)[[1L]])) ai_draft(path, id)
}
# a draft whose ai: block leaves a place to confirm (as tflspec's own test
# of the agent's route writes it)
ai_draft_confirm <- function(path, id) {
  f <- file.path(path, "ai", "spec", "outputs", paste0(id, ".yml"))
  if (!file.exists(f)) ai_draft(path, id)
  o <- tflspec::tfl_read_output(f)
  o$ai$confirm <- list(list(where = "content", item = "plan_cells",
                            why = "the shell shows n (%) but not its denominator"))
  tflspec::tfl_write_output(o, f)
  invisible(f)
}

test_that("the requests: one row each, in the order written, the reports from tflspec's record", {
  local_home()
  path <- ai_study(register = FALSE)
  r <- .ai_requests(path)
  expect_identical(r$request, c("demographics-1", "efficacy-1", "ae-1", "figure-1"))
  expect_identical(r$family, c("demographics", "efficacy", "ae", "figure"))
  expect_identical(r$outputs, c("T-14-1-1", "T-14-2-1", "T-14-3-1, T-14-3-5", "F-14-2-2"))
  expect_identical(r$n, c(1L, 1L, 2L, 1L))
  expect_identical(r$file, paste0("ai/requests/", r$request, ".md"))
  txt <- .ai_request_text(path, "ae-1")
  expect_identical(r$chars[r$request == "ae-1"], nchar(sub("\n$", "", txt)))
  expect_match(txt, "T-14-3-1", fixed = TRUE)
  # (no draft yet; no answers file, no instructions file)
  expect_identical(r$drafts, c(0L, 0L, 0L, 0L))
  expect_true(all(is.na(r$draft_time)))
  expect_false(any(c("answer", "answer_time") %in% names(r)))
  expect_false(file.exists(file.path(path, "ai", "requests", "00-instructions.md")))
  # no requests: no rows
  expect_identical(nrow(.ai_requests(tempfile())), 0L)
})

test_that("a request's drafts: how many of its reports have one, and the newest's time", {
  local_home()
  path <- ai_study(register = FALSE)
  none <- .ai_request_drafts(path, "T-14-3-1, T-14-3-5")
  expect_identical(none$n, 0L)
  expect_identical(none$of, 2L)
  expect_true(is.na(none$time))
  ai_draft(path, "T-14-3-1")
  one <- .ai_request_drafts(path, "T-14-3-1, T-14-3-5")
  expect_identical(one$n, 1L)
  expect_s3_class(one$time, "POSIXct")
  expect_false(is.na(one$time))
  ai_draft(path, "T-14-3-5")
  expect_identical(.ai_request_drafts(path, "T-14-3-1, T-14-3-5")$n, 2L)
  expect_identical(.ai_request_drafts(path, "")$of, 0L)
  r <- .ai_requests(path)
  expect_identical(r$drafts, c(0L, 0L, 2L, 0L))
  expect_s3_class(r$draft_time, "POSIXct")
  expect_false(is.na(r$draft_time[r$request == "ae-1"]))
  expect_true(is.na(r$draft_time[r$request == "efficacy-1"]))
})

test_that("a text file not in UTF-8 (Shift-JIS) is an error that says how to save it", {
  local_home()
  path <- ai_study(register = FALSE)
  f <- file.path(path, "ai", "note.md")
  sjis <- iconv("解析対象", "UTF-8", "CP932", toRaw = TRUE)[[1L]]
  writeBin(c(charToRaw("label: "), sjis, charToRaw("\n")), f)
  expect_error(.ai_read_text(f), class = "tflplanner_ai_encoding")
  expect_error(.ai_read_text(f), "not UTF-8")
  # a mark at the start (Notepad's UTF-8) is read without it, CR LF as LF
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a\r\nb")), f)
  expect_identical(.ai_read_text(f), "a\nb")
})

test_that("the check's levels: OK, WARNING, ERROR, no draft", {
  expect_identical(.ai_check_level(c("ok", "warning", "error", "no answer")),
                   c("OK", "WARNING", "ERROR", "no draft"))
  expect_true(is.na(.ai_check_level("other")))
  expect_identical(.ai_check_level(character()), character())
})

test_that("the drafts checked: tflspec's list (reports, next_steps, assumptions), what is to confirm", {
  local_home()
  path <- ai_study(register = FALSE)
  ai_draft(path, "T-14-1-1")
  # a draft with a problem: its fix request written
  d <- file.path(path, "ai", "spec", "outputs")
  a <- readLines(file.path(ai_example, "drafts", "T-14-3-1.yml"), encoding = "UTF-8")
  writeLines(sub("table_plan:", "table_plann:", a), file.path(d, "T-14-3-1.yml"))
  co <- .ai_collect(path)
  expect_s3_class(co, "tfl_ai_collected")
  expect_true(all(c("reports", "next_steps", "assumptions", "time", "confirm") %in% names(co)))
  expect_true(is.data.frame(co$confirm))
  expect_true(all(c("output_id", "where", "item", "why", "file") %in% names(co$confirm)))
  r <- co$reports
  expect_true(all(c("request", "output_id", "status", "problems", "first", "file",
                    "repair", "problems_text") %in% names(r)))
  # (a row a report: ae-1's two)
  expect_identical(r$request, c("demographics-1", "efficacy-1", "ae-1", "ae-1", "figure-1"))
  expect_identical(r$status, c("ok", "no answer", "error", "error", "no answer"))
  expect_identical(.ai_check_level(r$status), c("OK", "no draft", "ERROR", "ERROR", "no draft"))
  expect_identical(r$file, c("ai/spec/outputs/T-14-1-1.yml", NA, NA, NA, NA))
  expect_identical(r$repair, c(NA, NA, "ai/repairs/ae-1.md", "ai/repairs/ae-1.md", NA))
  expect_match(.ai_request_text(path, "ae-1", "repair"), "table_plan", fixed = TRUE)
  expect_identical(names(co$next_steps), unique(r$request))
  expect_match(co$next_steps[["ae-1"]], "agent's chat", fixed = TRUE)
  expect_true(all(c("output_id", "where", "item", "assumption") %in% names(co$assumptions)))
  expect_true("T-14-1-1" %in% co$assumptions$output_id)
  # (no answers folder)
  expect_false(dir.exists(file.path(path, "ai", "answers")))
  # a request's status: its worst report's
  expect_identical(.ai_request_status(c("ae-1", "efficacy-1", "x"), r), c("error", "no answer", NA))
  expect_identical(.ai_status_label(c("demographics-1", "ae-1", "efficacy-1"), r),
                   c("ok 1", "error 2", "no answer"))
  n <- .ai_counts(.ai_requests(path), co)
  expect_identical(n$drafts, 2L)
  expect_null(n$answers)
  # (the reports': ok 1, no answer 2, error 2)
  expect_identical(unname(n$status), c(1L, 0L, 2L, 2L))
})

test_that("what a draft leaves to confirm: listed by .ai_confirm(), the drafts' and the study's", {
  local_home()
  path <- ai_study(register = FALSE)
  expect_identical(nrow(.ai_confirm(path)), 0L)
  ai_draft_confirm(path, "T-14-1-1")
  cf <- .ai_confirm(path)
  expect_identical(cf$output_id, "T-14-1-1")
  expect_identical(cf$item, "plan_cells")
  expect_match(cf$file, "ai/spec/outputs/T-14-1-1.yml", fixed = TRUE)
  # (no such study: none)
  expect_identical(nrow(.ai_confirm(file.path(tempdir(), "no-such-study"))), 0L)
})

test_that("after a check the study is read again from its files; never over unsaved changes", {
  local_home()
  path <- ai_study()
  for (q in c("demographics-1", "ae-1")) ai_drafts_of(path, q)
  .ai_collect(path)
  # (the drafts adopted: step 4)
  ai_adopt_all(path)
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

test_that("steps 2 to 4: the list and its drafts, Check, the levels and what is to confirm, the copy", {
  local_home()
  path <- ai_study()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    rv <- session$userData$rv
    ai <- session$userData$ai
    expect_identical(ai$requests()$request,
                     c("demographics-1", "efficacy-1", "ae-1", "figure-1"))
    expect_match(output$`ai-status_line`$html, "5 reports, 4 requests", fixed = TRUE)
    expect_match(output$`ai-status_line`$html, "drafts 0 / 5", fixed = TRUE)
    # the request's text, as the copy and View give it
    expect_identical(ai$text("ae-1"), .ai_request_text(path, "ae-1"))
    # before a check: what Check does
    expect_match(output$`ai-result_all`$html, "Check reads the agent's SPEC drafts", fixed = TRUE)
    # the agent writes the drafts (ae-1's, one of them with a place to confirm)
    ai_drafts_of(path, "ae-1")
    ai_draft_confirm(path, "T-14-3-1")
    session$setInputs(`ai-reread` = 1)
    expect_identical(ai$requests()$drafts[ai$requests()$request == "ae-1"], 2L)
    expect_match(output$`ai-requests`, "2 / 2", fixed = TRUE)
    # unsaved changes: no check
    rv$meta$title <- "mine"
    expect_true(isTRUE(session$userData$dirty()))
    session$setInputs(`ai-status_reread` = 1)
    expect_null(ai$collected())
    expect_identical(rv$meta$title, "mine")
    # saved: the check reads the drafts
    rv$meta$title <- rv$saved_meta$title
    session$setInputs(`ai-status_reread` = 2)
    co <- ai$collected()
    expect_identical(co$reports$status[co$reports$request == "ae-1"], rep("ok", 2L))
    expect_identical(co$confirm$output_id, "T-14-3-1")
    # step 3's table: the level, the count to confirm, the draft's ai: block
    html <- output$`ai-result_all`$html
    expect_match(html, "Checked ", fixed = TRUE)
    expect_match(html, "<td>T-14-3-1</td>", fixed = TRUE)
    expect_match(html, "<td class=\"text-success\">OK</td>", fixed = TRUE)
    expect_match(html, "<td class=\"text-muted\">no draft</td>", fixed = TRUE)
    expect_match(html, "1 to confirm", fixed = TRUE)
    expect_match(html, "<details>", fixed = TRUE)
    expect_match(html, "the shell shows n (%) but not its denominator", fixed = TRUE)
    expect_match(html, "treatment-emergent is TRTEMFL", fixed = TRUE)
    # (a draft not copied yet: copied first)
    expect_match(html, "Copy it into the study first (step 4)", fixed = TRUE)
    # (no ERROR or WARNING: no fix text)
    expect_false(grepl("Copy the fix text", html, fixed = TRUE))
    # step 4: the drafts not adopted yet are ticked at first, copied into the study
    expect_false("T-14-3-1" %in% output_ids(rv$p))
    expect_match(output$`ai-taken_note`$html, "step 4", fixed = TRUE)
    # (not while the study has unsaved changes: nothing written)
    rv$meta$title <- "mine"
    session$setInputs(`ai-adopt` = 1)
    session$setInputs(`ai-adopt_fresh` = 1)
    expect_false(file.exists(file.path(path, "spec", "outputs", "T-14-3-1.yml")))
    rv$meta$title <- rv$saved_meta$title
    session$setInputs(`ai-adopt` = 2)
    session$setInputs(`ai-adopt_fresh` = 2)
    # (saved at once: the adopted report's program written, for a run)
    expect_length(list.files(file.path(path, "programs", "tfl"), "14.3.1"), 1L)
    expect_true("T-14-3-1" %in% output_ids(rv$p))
    expect_false(isTRUE(session$userData$dirty()))
    expect_true(all(spec_status("XYZ-1")$status == "same"))
    # copied: checked again, a place to confirm opens the report in the
    # builder, its card in view
    session$setInputs(`ai-status_reread` = 3)
    html <- output$`ai-result_all`$html
    expect_match(html, "Open in Make a report", fixed = TRUE)
    expect_match(html, "data-item=\"", fixed = TRUE)
  })
})

test_that("step 3: an ERROR row says its first problem and gives the fix text to copy", {
  local_home()
  path <- ai_study()
  d <- file.path(path, "ai", "spec", "outputs")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  a <- readLines(file.path(ai_example, "drafts", "T-14-3-1.yml"), encoding = "UTF-8")
  writeLines(sub("table_plan:", "table_plann:", a), file.path(d, "T-14-3-1.yml"))
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    ai <- session$userData$ai
    session$setInputs(`ai-status_reread` = 1)
    html <- output$`ai-result_all`$html
    expect_match(html, "<td class=\"text-danger\">ERROR</td>", fixed = TRUE)
    expect_match(html, "has no table_plan item", fixed = TRUE)
    expect_match(html, "data-what=\"repair\" data-id=\"ae-1\"", fixed = TRUE)
    expect_match(html, "Copy the fix text", fixed = TRUE)
    expect_match(ai$text("ae-1", "repair"), "table_plan", fixed = TRUE)
    expect_match(output$`ai-taken_note`$html, "No report could be taken in", fixed = TRUE)
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
  expect_identical(sort(p$toc$toc_id), c("F-14-2-2", "T-14-1-1", "T-14-2-1", "T-14-3-1", "T-14-3-5"))
  expect_true(all(p$toc$shell_found))
  expect_identical(sort(p$adam$datasets$dataset), c("ADAE", "ADLB", "ADRS", "ADSL", "ADTTE"))
  # (a request a shell sheet: the Safety sheet's two TOC items in one)
  expect_identical(nrow(p$requests), 4L)
  expect_identical(p$counts[["shells"]], 4L)
  expect_identical(p$counts[["toc_items"]], 5L)
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
  expect_true(file.exists(file.path(s$path, "docs", "onco_toc_shell.xlsx")))
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
  p <- .ai_write_requests(path, ai_toc, "onco_toc_shell.xlsx", ai_adam, list(size = 2L))
  expect_false(p$dry_run)
  expect_identical(unname(tools::md5sum(file.path(path, c("study.yml", "spec/data.yml")))),
                   unname(before))
  r <- .ai_requests(path)
  expect_identical(nrow(r), nrow(p$requests))
  expect_false(file.exists(file.path(path, "ai", "requests", "00-instructions.md")))
  expect_true(all(spec_status("XYZ-1")$status == "same"))
  # the templates listed (the default), or not (templates = FALSE)
  # (in the request's head, under the note for the agent)
  first <- function() readLines(file.path(path, "ai", "requests", paste0(r$request[1], ".md")), 10L)
  expect_true("<!-- templates: TRUE -->" %in% first())
  .ai_write_requests(path, ai_toc, "onco_toc_shell.xlsx", ai_adam,
                     list(size = 2L, templates = FALSE))
  expect_false("<!-- templates: TRUE -->" %in% first())
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
                      `ai-toc_path` = ai_toc, `ai-adam_dir` = ai_adam,
                      `ai-language` = "English", `ai-max_levels` = 15)
    expect_null(ai$prepared())
    session$setInputs(`ai-read` = 1)
    p <- ai$prepared()
    expect_true(p$dry_run)
    expect_identical(nrow(p$toc), 5L)
    expect_false(dir.exists(file.path(studies_root(), "NEW-9")))
    expect_match(output$`ai-toc_head`$html, "4 shells, 5 TOC items, 0 of them with no shell",
                 fixed = TRUE)
    session$setInputs(`ai-datasets` = c("ADSL", "ADAE"), `ai-write` = 1)
    expect_identical(rv$study$meta$study_id, "NEW-9")
    expect_false(ai$prepared()$dry_run)
    expect_identical(nrow(ai$requests()), 4L)
    # the open study again: Write asks first (its requests are there)
    session$setInputs(`ai-study_mode` = "open", `ai-read` = 2)
    expect_true(ai$prepared()$dry_run)
  })
})

test_that("the workbook by its path: checked, then copied into the study's docs/ like Browse...", {
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
  kept <- file.path(out$study$path, "docs", "TOC_SHELL.xlsx")
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

# the updates the server sends to the fields (shiny's update*Input() mocked):
# the last of each field, by its id in the tab
ai_catch_updates <- function(env = parent.frame()) {
  sent <- new.env()
  keep <- function(id, ...) assign(id, list(...), envir = sent)
  testthat::local_mocked_bindings(
    updateTextInput = function(session, inputId, label = NULL, value = NULL, placeholder = NULL) {
      keep(inputId, value = value)
    },
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL,
                                 selected = NULL) {
      keep(inputId, choices = choices, selected = selected)
    },
    updateSelectizeInput = function(session, inputId, label = NULL, choices = NULL,
                                    selected = NULL, options = list(), server = FALSE) {
      keep(inputId, choices = choices, selected = selected)
    },
    .package = "shiny", .env = env)
  sent
}

test_that("Browse... opens in the study's folder: the path given, the study's, studies_root", {
  local_home()
  root <- studies_root()
  open <- file.path(root, "OPEN-1")
  dir.create(file.path(open, "data", "adam"), recursive = TRUE)
  dir.create(file.path(root, "NEW-1", "docs"), recursive = TRUE)
  norm <- function(d) normalizePath(d, "/")
  # the open study: its data/adam; its folder for the workbooks (no docs/)
  expect_identical(.ai_browse_start("adam", "", study_dir = open, root = root),
                   norm(file.path(open, "data", "adam")))
  expect_identical(.ai_browse_start("toc", "", study_dir = open, root = root), norm(open))
  # a new study: studies_root/<ID>/docs when there; its folder; else studies_root
  expect_identical(.ai_browse_start("toc", "", root = root, new_id = "NEW-1"),
                   norm(file.path(root, "NEW-1", "docs")))
  expect_identical(.ai_browse_start("shells", "", root = root, new_id = "NEW-1"),
                   norm(file.path(root, "NEW-1", "docs")))
  expect_identical(.ai_browse_start("adam", "", root = root, new_id = "NEW-1"),
                   norm(file.path(root, "NEW-1")))
  expect_identical(.ai_browse_start("adam", "", root = root, new_id = "NONE-9"), norm(root))
  expect_identical(.ai_browse_start("adam", "", root = root, new_id = "../NEW-1"), norm(root))
  expect_identical(.ai_browse_start("adam", ""), norm(root))
  # the path given first: a file's folder, a folder; a path not there passed over
  wb <- file.path(tempdir(), "browse-start.xlsx")
  file.copy(ai_toc, wb, overwrite = TRUE)
  expect_identical(.ai_browse_start("toc", wb, study_dir = open, root = root), norm(tempdir()))
  expect_identical(.ai_browse_start("shells", paste0("C:/none/x.xlsx\n\"", wb, "\""), root = root),
                   norm(tempdir()))
  expect_identical(.ai_browse_start("adam", ai_adam, study_dir = open, root = root), ai_adam)
  expect_identical(.ai_browse_start("adam", file.path(tempdir(), "no-such"), study_dir = open,
                                    root = root), norm(file.path(open, "data", "adam")))
  expect_identical(.ai_browse_start("adam", "", root = file.path(tempdir(), "no-root")), "")
  # what the Windows dialog is given: a folder's mask, a file itself
  expect_identical(.ai_file_default(root, windows = TRUE),
                   paste0(normalizePath(root, "\\"), "\\*.*"))
  expect_identical(.ai_file_default(wb, windows = TRUE), normalizePath(wb, "\\"))
  expect_identical(.ai_file_default(root, windows = FALSE), file.path(root, "*.xlsx"))
  expect_identical(.ai_file_default(""), "")
})

test_that("Browse... in the app: the dialog is given the study's folder", {
  local_home()
  path <- ai_study()
  dir.create(file.path(studies_root(), "NEW-2", "docs"), recursive = TRUE)
  got <- new.env()
  old <- options(tflplanner.ai_drafts = TRUE,
                 tflplanner.choose_dir = function(default) {
                   got$dir <- default
                   NA_character_
                 },
                 tflplanner.choose_file = function(default) {
                   got$file <- default
                   NA_character_
                 },
                 tflplanner.choose_files = function(default) {
                   got$files <- default
                   character()
                 })
  on.exit(options(old))
  shiny::testServer(server_for("XYZ-1"), {
    session$setInputs(`ai-study_mode` = "open", `ai-adam_dir` = "", `ai-toc_path` = "",
                      `ai-shell_paths` = "")
    session$setInputs(`ai-adam_browse` = 1)
    # (XYZ-1 has no data/adam: the study's folder)
    expect_identical(got$dir, normalizePath(path, "/"))
    session$setInputs(`ai-study_mode` = "new", `ai-new_id` = "NEW-2")
    session$setInputs(`ai-toc_browse` = 1, `ai-shells_browse` = 1)
    expect_identical(got$file, normalizePath(file.path(studies_root(), "NEW-2", "docs"), "/"))
    expect_identical(got$files, got$file)
    session$setInputs(`ai-adam_dir` = ai_adam, `ai-adam_browse` = 2)
    expect_identical(got$dir, ai_adam)
  })
})

test_that("the ADaM folder put in: the chosen study's, following the ID; a folder given kept", {
  local_home()
  path <- ai_study()
  root <- studies_root()
  dir.create(file.path(root, "NEW-3", "data", "adam"), recursive = TRUE)
  dir.create(file.path(root, "NEW-4"), recursive = TRUE)
  new3 <- normalizePath(file.path(root, "NEW-3", "data", "adam"), "/")
  # the pure part
  expect_identical(.ai_adam_auto(NULL, "NEW-3", root), new3)
  expect_identical(.ai_adam_auto(NULL, "NEW-4", root), "")
  expect_identical(.ai_adam_auto(NULL, "", root), "")
  expect_identical(.ai_adam_auto(NULL, "..", root), "")
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  sent <- ai_catch_updates()
  shiny::testServer(server_for("XYZ-1"), {
    adam <- function() sent$adam_dir$value
    # a new study: its data/adam when there, else empty (not the open study's)
    session$setInputs(`ai-study_mode` = "new", `ai-new_id` = "NEW-3")
    expect_identical(adam(), new3)
    session$setInputs(`ai-adam_dir` = new3, `ai-new_id` = "NEW-4")
    expect_identical(adam(), "")
    session$setInputs(`ai-adam_dir` = "", `ai-new_id` = "NEW-3")
    expect_identical(adam(), new3)
    # the open study's (.ai_adam_default())
    session$setInputs(`ai-adam_dir` = new3, `ai-study_mode` = "open")
    expect_identical(adam(), .ai_adam_default(open_study("XYZ-1")))
    expect_true(nzchar(adam()))
    # a folder given: kept when the study or the ID changes
    rm(list = ls(sent), envir = sent)
    session$setInputs(`ai-adam_dir` = file.path(tempdir(), "mine"), `ai-study_mode` = "new",
                      `ai-new_id` = "NEW-4")
    session$setInputs(`ai-new_id` = "NEW-3")
    expect_null(sent$adam_dir)
  })
})

test_that("the sheets to choose from as soon as the workbook is given; the one chosen kept", {
  local_home()
  ai_study()
  sh <- readxl::excel_sheets(ai_toc)
  more <- file.path(tempdir(), "more_shells.xlsx")
  file.copy(ai_toc, more, overwrite = TRUE)
  s <- .ai_sheet_names(ai_toc, more)
  expect_identical(s$sheet, c(sh, sprintf("%s [more_shells.xlsx]", sh)))
  expect_identical(s$toc, rep(c(TRUE, FALSE), each = length(sh)))
  expect_identical(.ai_sheet_labels(s$sheet, s$book)[1L],
                   sprintf("%s [%s]", sh[1L], basename(ai_toc)))
  expect_identical(nrow(.ai_sheet_names(file.path(tempdir(), "none.xlsx"))), 0L)
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  sent <- ai_catch_updates()
  shiny::testServer(server_for("XYZ-1"), {
    session$setInputs(`ai-toc_path` = ai_toc)
    toc <- sent$toc_sheet
    expect_identical(unname(toc$choices), c("", sh))
    expect_identical(names(toc$choices)[1L], "guess")
    expect_identical(toc$selected, "")
    # the sheet chosen kept when the workbook is given again, with more shells
    session$setInputs(`ai-toc_sheet` = sh[1L], `ai-sheets` = sh[2L], `ai-toc_path` = "",
                      `ai-shell_paths` = more)
    session$setInputs(`ai-toc_path` = ai_toc)
    expect_identical(sent$toc_sheet$selected, sh[1L])
    expect_identical(unname(sent$toc_sheet$choices), c("", sh))
    expect_identical(unname(sent$sheets$choices), s$sheet)
    expect_identical(names(sent$sheets$choices), .ai_sheet_labels(s$sheet, s$book))
    expect_identical(sent$sheets$selected, sh[2L])
  })
})

test_that("step 1's labels: the language of the AI's questions and notes; no chat's fields", {
  local_home()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  ui <- as.character(app_ui("en"))
  expect_match(ui, "Language of the AI's questions and notes (language)", fixed = TRUE)
  expect_match(ui, "1 Write the prompt", fixed = TRUE)
  expect_match(ui, "2 The agent writes the SPEC", fixed = TRUE)
  expect_match(ui, "3 Check the SPEC", fixed = TRUE)
  expect_match(ui, "4 Copy into the study", fixed = TRUE)
  # (one way: no mode to choose, no chat's size, instructions or limits)
  for (id in c("ai-mode", "ai-size", "ai-instructions_each", "ai-workbook", "ai-max_chars",
               "ai-answer_text", "ai-questions", "ai-expected_outputs", "ai-run_all")) {
    expect_false(grepl(sprintf("id=\"%s\"", id), ui, fixed = TRUE), info = id)
  }
  expect_match(ui, "id=\"ai-status_reread\"", fixed = TRUE)
  expect_match(ui, "id=\"ai-list_new\"", fixed = TRUE)
  expect_match(as.character(app_ui("ja")), "AI の質問と注記の言語",
               fixed = TRUE)
})

test_that("the reports a request holds: one, the agent's; no chat's arguments given", {
  local_home()
  ai_study()
  expect_identical(.ai_size_default("agent"), 1)
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  got <- new.env()
  local_mocked_bindings(.ai_prepare = function(dir, toc, adam, args = list(), ...) {
    got$args <- args
    stop("read no further")
  })
  shiny::testServer(server_for("XYZ-1"), {
    session$setInputs(`ai-toc_path` = ai_toc, `ai-adam_dir` = ai_adam)
    session$setInputs(`ai-read` = 1)
    expect_identical(got$args$size, 1L)
    expect_false(any(c("mode", "route", "instructions", "agent_run", "workbook") %in% names(got$args)))
  })
})

test_that("the requests a dry run would write at another size", {
  toc <- data.frame(output_id = c("T-1", "T-2", "T-3", "T-4", "F-1"),
                    title = c("Adverse Events by SOC", "Adverse Events by PT", "Serious Adverse Events",
                              "Demographics", "KM plot"),
                    type = c("table", "table", "table", "table", "figure"), stringsAsFactors = FALSE)
  three <- .ai_dry_requests(toc, 3L)
  one <- .ai_dry_requests(toc, 1L)
  expect_identical(sum(three$n), 5L)
  expect_identical(sum(one$n), 5L)
  expect_gt(nrow(one), nrow(three))
  expect_identical(nrow(one), 5L)
  expect_true("figure-1" %in% three$request)
})

test_that("the requests to write: one report each; the workbook changed, read again", {
  local_home()
  ai_study()
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  got <- new.env()
  local_mocked_bindings(.ai_write_requests = function(path, toc, toc_name, adam, args = list(),
                                                      shells = list()) {
    got$size <- args$size
    stop("write no further")
  })
  shiny::testServer(server_for("XYZ-1"), {
    ai <- session$userData$ai
    session$setInputs(`ai-study_mode` = "open", `ai-toc_path` = ai_toc, `ai-adam_dir` = ai_adam,
                      `ai-add` = FALSE)
    session$setInputs(`ai-read` = 1)
    p1 <- ai$prepared()
    expect_true(p1$dry_run)
    expect_identical(p1$requests$request, c("T14.1.1", "T14.2.1", "Safety", "F14.2.2"))
    # (as tflspec's dry run counts them: its shells and TOC items)
    count <- function(p) sprintf("%d requests to write (%d shells, %d TOC items).", nrow(p$requests),
                                 p$counts[["shells"]], p$counts[["toc_items"]])
    expect_match(output$`ai-prep_status`$html, count(p1), fixed = TRUE)
    # (the study has requests: Write asks first, then writes at one a request)
    session$setInputs(`ai-write_ok` = 1)
    expect_identical(got$size, 1L)
    # another TOC sheet: read again first
    session$setInputs(`ai-toc_sheet` = "Safety")
    expect_match(output$`ai-prep_status`$html, "press \"Read\" again", fixed = TRUE)
    got$size <- NULL
    session$setInputs(`ai-write` = 1)
    expect_null(got$size)
    session$setInputs(`ai-toc_sheet` = "")
    expect_match(output$`ai-prep_status`$html, count(p1), fixed = TRUE)
  })
})

# -- the coding agent ------------------------------------------------------------

test_that("the agent in the app: one prompt on step 2, to copy", {
  local_home()
  out <- .ai_new_study("AG-2", ai_toc, "onco_toc_shell.xlsx", ai_adam, list(size = 3L))
  path <- out$study$path
  expect_true(file.exists(file.path(path, "AGENTS.md")))
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("AG-2"), {
    ai <- session$userData$ai
    expect_identical(ai$mode(), "agent")
    expect_identical(output$`ai-mode_now`, "agent")
    h <- output$`ai-agent_card`$html
    expect_match(h, "AGENTS.md", fixed = TRUE)
    expect_match(h, "Copy the prompt", fixed = TRUE)
    expect_identical(ai$text("agent", "agent"), .ai_agent_prompt(path))
    # (one prompt: no message for the next requests, no other file to show)
    expect_no_match(h, "data-what=\"continue\"", fixed = TRUE)
    expect_no_match(h, "data-what=\"file\"", fixed = TRUE)
  })
})

# what tfl_ai_run() writes for a report that
# passed its checks but could not be built
ai_unbuilt_repair <- function(path, request, id, why) {
  dir.create(file.path(path, "ai", "repairs"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(sprintf("# Repair: %s", request), "",
               "The answer passed its checks, but a report could not be built.", "",
               "## Could not be built", "", sprintf("- %s: %s", id, why)),
             file.path(path, "ai", "repairs", paste0(request, ".md")))
}

test_that("the reports run.R could not build: read from ai/repairs, none when there is no file", {
  local_home()
  path <- ai_study(register = FALSE)
  u <- .ai_unbuilt(path)
  expect_identical(nrow(u), 0L)
  ai_unbuilt_repair(path, "ae-1", "T-14-3-1", "object 'AEDECOD' not found: in the ARD")
  # a fix request of the checks alone (no such section): not one
  writeLines(c("# Repair", "", "- table_plan: unknown"), file.path(path, "ai", "repairs", "labs-1.md"))
  u <- .ai_unbuilt(path)
  expect_identical(u$request, "ae-1")
  expect_identical(u$output_id, "T-14-3-1")
  expect_identical(u$reason, "object 'AEDECOD' not found: in the ARD")
  expect_named(attr(u, "texts"), "ae-1")
})

# its own file, ai/repairs/<request>-build.md
ai_build_repair <- function(path, request, id, why) {
  dir.create(file.path(path, "ai", "repairs"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(sprintf("# Repair: %s -- could not be built", request), "",
               sprintf("Fix the answer and run `Rscript run.R %s`.", request), "",
               "## Could not be built", "", sprintf("- %s: %s", id, why)),
             file.path(path, "ai", "repairs", paste0(request, "-build.md")))
}

test_that("a report not built, from <request>-build.md: read first; its texts after a check", {
  local_home()
  path <- ai_study(register = FALSE)
  # both files: the separate one's reason
  ai_unbuilt_repair(path, "ae-1", "T-14-3-1", "the old section")
  ai_build_repair(path, "ae-1", "T-14-3-1", "the build file")
  u <- .ai_unbuilt(path)
  expect_identical(u$reason, "the build file")
  expect_identical(u$from, "build")
  expect_identical(attr(u, "build"), "ae-1")
  expect_length(attr(u, "texts"), 0L)

  out <- .ai_new_study("AG-4", ai_toc, "onco_toc_shell.xlsx", ai_adam, list(size = 3L))
  path <- out$study$path
  ai_drafts_of(path, "demographics-1")
  ai_build_repair(path, "demographics-1", "T-14-1-1", "object 'ard' not found")
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("AG-4"), {
    ai <- session$userData$ai
    session$setInputs(`ai-status_reread` = 1)
    expect_true(file.exists(file.path(path, "ai", "repairs", "demographics-1-build.md")))
    expect_match(ai$text("demographics-1", "build"), "could not be built", fixed = TRUE)
    expect_match(ai$text("demographics-1", "unbuilt"), "object 'ard' not found", fixed = TRUE)
    expect_identical(ai$text("demographics-1", "rerun"), "Rscript run.R demographics-1")
  })
})

# -- several workbooks, and requests added ----------------

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
# (tflspec's sheet-first requests do not stop a TOC item
# asked for again with add = TRUE, nor drop its old request on overwrite --
# the tests of it skipped until tflspec does; tried once)
ai_asks_again_stops <- local({
  ans <- NULL
  function() {
    if (is.null(ans)) {
      d <- tempfile("ai-again-")
      dir.create(d)
      on.exit(unlink(d, recursive = TRUE))
      file.copy(ai_prepared, d, recursive = TRUE)
      path <- file.path(d, "XYZ-1")
      m <- ai_more_book(d)
      .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE))
      e <- tryCatch({
        .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE))
        ""
      }, error = function(e) conditionMessage(e))
      ans <<- .ai_is_asked_again(e)
    }
    ans
  }
})
skip_unless_asked_again_stops <- function() {
  if (!ai_asks_again_stops()) {
    skip("tflspec's sheet-first requests do not stop a TOC item asked for again (add = TRUE)")
  }
}

test_that("the shells in a workbook of their own: read, kept in docs/, each request's workbook", {
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
  expect_true(all(file.exists(file.path(path, "docs", c("toc.xlsx", "shells.xlsx")))))
  r <- .ai_requests(path)
  expect_true("book" %in% names(r))
  expect_identical(unique(r$book), "shells.xlsx")
})

test_that("requests added: the next names, the drafts kept; a report asked again only with overwrite", {
  local_home()
  path <- ai_study()
  d <- file.path(tempdir(), "ai-more")
  dir.create(d, showWarnings = FALSE)
  m <- ai_more_book(d)
  ai_drafts_of(path, "ae-1")
  own <- file.path(path, c("study.yml", "spec/data.yml", "spec/defaults.yml"))
  before <- tools::md5sum(own)
  n0 <- nrow(.ai_requests(path))
  # the dry run says nothing is asked for twice; then added
  p <- .ai_prepare(path, m, ai_adam, list(add = TRUE), dry_run = TRUE)
  expect_identical(nrow(.ai_already_asked(path, p$toc$toc_id)), 0L)
  w <- .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE))
  r <- .ai_requests(path)
  expect_identical(nrow(r), n0 + 1L)
  expect_identical(r$request[nrow(r)], "Safety")
  expect_identical(r$book[nrow(r)], "more.xlsx")
  expect_true(file.exists(file.path(path, "ai", "spec", "outputs", "T-14-3-1.yml")))
  expect_true(file.exists(file.path(path, "docs", "more.xlsx")))
  # the same workbook again: asked for already
  again <- .ai_already_asked(path, "T-14-3-2")
  expect_identical(again$request, "Safety")
  skip_unless_asked_again_stops()
  e <- tryCatch(.ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE)),
                error = function(e) conditionMessage(e))
  expect_true(.ai_is_asked_again(e))
  expect_match(e, "T-14-3-2 already asked for (Safety)", fixed = TRUE)
  # written again (overwrite): one request for it, the study's own files kept
  .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE, overwrite = TRUE))
  r2 <- .ai_requests(path)
  expect_identical(sum(r2$outputs == "T-14-3-2"), 1L)
  expect_identical(nrow(r2), nrow(r))
  expect_identical(unname(tools::md5sum(own)), unname(before))
})

test_that("an agent's study: a request asked again keeps the study's files and the agent's", {
  local_home()
  skip_unless_asked_again_stops()
  out <- .ai_new_study("AG-4", ai_toc, "onco_toc_shell.xlsx", ai_adam, list(size = 3L))
  path <- out$study$path
  d <- file.path(tempdir(), "ai-more-agent")
  dir.create(d, showWarnings = FALSE)
  m <- ai_more_book(d)
  .ai_write_requests(path, m, "more.xlsx", ai_adam, list(add = TRUE))
  own <- file.path(path, c("study.yml", "spec/data.yml", "spec/defaults.yml", "AGENTS.md"))
  before <- tools::md5sum(own)
  .ai_write_requests(path, m, "more.xlsx", ai_adam,
                     list(add = TRUE, overwrite = TRUE))
  expect_identical(unname(tools::md5sum(own)), unname(before))
  expect_identical(sum(.ai_requests(path)$outputs == "T-14-3-2"), 1L)
})

test_that("the agent's first message: ai/agent_prompt.md when the study has it", {
  local_home()
  out <- .ai_new_study("AG-3", ai_toc, "onco_toc_shell.xlsx", ai_adam, list(size = 3L))
  path <- out$study$path
  expect_true(file.exists(file.path(path, "ai", "agent_prompt.md")))
  expect_identical(.ai_agent_prompt(path), out$prepared$agent_prompt)
  writeLines("Follow the steps.", file.path(path, "ai", "agent_prompt.md"))
  expect_identical(.ai_agent_prompt(path), "Follow the steps.")
  # none (a study prepared before it was written): a plain one, its paths
  # from the study folder (no absolute path)
  unlink(file.path(path, "ai", "agent_prompt.md"))
  m <- .ai_agent_prompt(path)
  expect_match(m, "ai/AGENTS.md", fixed = TRUE)
  expect_match(m, "continue", fixed = TRUE)
  expect_false(grepl(normalizePath(path, "/"), m, fixed = TRUE))
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
                      `ai-add` = TRUE, `ai-language` = "English", `ai-max_levels` = 15)
    session$setInputs(`ai-read` = 1)
    expect_match(output$`ai-prep_status`$html, "Added to the", fixed = TRUE)
    session$setInputs(`ai-write` = 1)
    r <- ai$requests()
    expect_identical(nrow(r), n0 + 1L)
    expect_match(output$`ai-requests`, "more.xlsx", fixed = TRUE)
    # read again: asked for already, said; written again on overwrite
    session$setInputs(`ai-read` = 2)
    expect_match(output$`ai-prep_status`$html, "T-14-3-2 (Safety)", fixed = TRUE)
    skip_unless_asked_again_stops()
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
  ai_drafts_of(path, "ae-1")
  .ai_collect(path)
  ai_adopt_all(path)
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

test_that("a new study that fails keeps what its folder held before (its data/adam)", {
  root <- tempfile("keep")
  d <- file.path(root, "KEEP-1")
  dir.create(file.path(d, "data", "adam"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  saveRDS(data.frame(USUBJID = "1"), file.path(d, "data", "adam", "adsl.rds"))
  home <- file.path(root, "home")
  bad <- file.path(root, "no-such-toc.xlsx")
  expect_error(.ai_new_study("KEEP-1", bad, "no-such-toc.xlsx", file.path(d, "data", "adam"),
                             root = root, home = home))
  expect_true(file.exists(file.path(d, "data", "adam", "adsl.rds")))
  expect_identical(sort(list.files(d, all.files = TRUE, no.. = TRUE)), "data")
  # a folder that was not there is taken away whole
  expect_error(.ai_new_study("KEEP-2", bad, "no-such-toc.xlsx", file.path(d, "data", "adam"),
                             root = root, home = home))
  expect_false(dir.exists(file.path(root, "KEEP-2")))
})

test_that("step 1: a number unticked and applied, then back to the rule, before and after writing", {
  local_home()
  ai_study()
  shiny::testServer(server_for("XYZ-1"), {
    ai <- session$userData$ai
    session$setInputs(`ai-study_mode` = "new", `ai-new_id` = "NUM-8", `ai-new_title` = "Eight",
                      `ai-toc_path` = ai_toc, `ai-adam_dir` = ai_adam,
                      `ai-language` = "English", `ai-max_levels` = 15)
    session$setInputs(`ai-read` = 1)
    n <- ai$prepared()$numbers
    skip_if(is.null(n) || !nrow(n))
    k <- which(n$make %in% TRUE)[1L]
    row_of <- function() {
      m <- ai$prepared()$numbers
      m[m$sheet == n$sheet[k] & m$number == n$number[k], , drop = FALSE]
    }
    session$setInputs(`ai-num_toggle` = list(sheet = n$sheet[k], number = n$number[k], on = FALSE, n = 1))
    session$setInputs(`ai-numbers_apply` = 1)
    expect_false(row_of()$make)
    expect_identical(row_of()$source, "given")
    # (the TOC view follows: the item made by no sheet now has no shell,
    # unless another sheet makes it)
    tc <- ai$prepared()$toc
    id <- n$toc_id[k]
    if (!is.na(id) && sum(ai$prepared()$numbers$toc_id %in% id & ai$prepared()$numbers$make %in% TRUE) == 0L) {
      expect_false(tc$shell_found[(tc$toc_id %||% tc$output_id) == id])
      expect_match(output$`ai-toc_head`$html, "1 of them with no shell", fixed = TRUE)
    }
    session$setInputs(`ai-numbers_rule` = 1)
    expect_true(row_of()$make)
    expect_identical(row_of()$source, "rule")
    # (written with the change, then set back on the open study)
    session$setInputs(`ai-num_toggle` = list(sheet = n$sheet[k], number = n$number[k], on = FALSE, n = 2))
    session$setInputs(`ai-numbers_apply` = 2)
    session$setInputs(`ai-write` = 1)
    session$setInputs(`ai-study_mode` = "open", `ai-read` = 2)
    expect_false(row_of()$make)
    session$setInputs(`ai-numbers_rule` = 2)
    expect_true(row_of()$make)
  })
})

test_that("the drafts' check: a level and what is to confirm a report, the fix text of the ERROR one", {
  path <- withr_tempdir()
  area <- file.path(path, "ai", "spec", "outputs")
  dir.create(area, recursive = TRUE)
  o <- tflspec::tfl_output("T-1", "table", label = "One",
                           content = list(tflspec::tfl_item("table_plan", list(cols = "TRT01A"))))
  o$ai <- list(confirm = list(list(where = "content", item = "plan_cells", why = "the decimals")))
  tflspec::tfl_write_output(o, file.path(area, "T-1.yml"))
  writeLines(c("id: T-2", "kind: table", "content: [unclosed"), file.path(area, "T-2.yml"))
  ck <- .ai_check_drafts(path)
  r <- ck$reports
  expect_identical(r$level[match(c("T-1", "T-2"), r$output_id)], c("OK", "ERROR"))
  expect_identical(r$confirms[r$output_id == "T-1"], 1L)
  expect_identical(names(ck$fix), "T-2")
  # the fix text's language: the study's (study.yml's ai: language)
  expect_identical(.ai_fix_language(path), "en")
  writeLines(c("study_id: X", "ai:", "  language: Japanese"), file.path(path, "study.yml"))
  expect_identical(.ai_fix_language(path), "ja")
})
