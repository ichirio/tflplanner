# The branches of R/ai_draft.R no other test reaches: text that is not
# UTF-8, a request's name and file, the reports that could not be built,
# tflspec's checks that stop, the workbooks kept in docs/, the file
# dialogs, the review depth and the run setting left as they are, and
# step 1's settings as tflspec keeps them.

cov_ai_dir <- function(env = parent.frame()) {
  d <- withr_tempdir(env)
  dir.create(file.path(d, .ai_repairs_dir), recursive = TRUE)
  d
}

test_that("a text with a NUL byte (UTF-16, say) is an encoding error that names the file", {
  f <- tempfile(fileext = ".md")
  on.exit(unlink(f), add = TRUE)
  writeBin(c(charToRaw("a"), as.raw(0L), charToRaw("b")), f)
  err <- tryCatch(.ai_read_text(f), error = function(e) e)
  expect_s3_class(err, "tflplanner_ai_encoding")
  expect_match(conditionMessage(err), basename(f), fixed = TRUE)
  expect_identical(err$file, f)
})

test_that("a request's name with a space or a slash is refused", {
  expect_error(.ai_check_request_name("ae 1"), "letters, digits")
  expect_error(.ai_check_request_name("../ae-1"), "letters, digits")
  expect_error(.ai_check_request_name(c("a", "b")), "letters, digits")
  expect_error(.ai_check_request_name(NA_character_), "letters, digits")
  expect_identical(.ai_check_request_name("t.15.1-2"), "t.15.1-2")
})

test_that("a request with no file is an error naming the file from the study's folder", {
  d <- cov_ai_dir()
  expect_error(.ai_request_text(d, "ae-9"), "No file ai/requests/ae-9.md", fixed = TRUE)
  expect_error(.ai_request_text(d, "ae-9", "repair"), "No file ai/repairs/ae-9.md", fixed = TRUE)
})

test_that("the reports not built: the section ends at the next heading, a file not UTF-8 or with no list is passed over", {
  d <- cov_ai_dir()
  rp <- file.path(d, .ai_repairs_dir)
  writeLines(c("# Fix request", "## Could not be built", "- T-1: no data",
               "## Next", "- T-2: not a reason"), file.path(rp, "ae-1.md"))
  writeLines(c("## Could not be built", "nothing in a list"), file.path(rp, "ae-2.md"))
  writeBin(c(charToRaw("## Could not be built\n- T-3: "), as.raw(c(0x82, 0xa0))),
           file.path(rp, "ae-3.md"))
  u <- .ai_unbuilt(d)
  expect_identical(u$output_id, "T-1")
  expect_identical(u$reason, "no data")
  # (each older fix request with the section is kept whole, rows or not)
  expect_setequal(names(attr(u, "texts")), c("ae-1", "ae-2"))
})

test_that("the reports not built: a request's -build.md is read, its older fix request passed over", {
  d <- cov_ai_dir()
  rp <- file.path(d, .ai_repairs_dir)
  writeLines(c("## Could not be built", "- T-1: old reason"), file.path(rp, "ae-1.md"))
  writeLines(c("## Could not be built", "- T-1: new reason"), file.path(rp, "ae-1-build.md"))
  u <- .ai_unbuilt(d)
  expect_identical(u$reason, "new reason")
  expect_identical(u$from, "build")
  expect_identical(attr(u, "build"), "ae-1")
})

test_that("the requests of a tflspec that gives no family or book: the family from the name, no book", {
  d <- cov_ai_dir()
  local_mocked_bindings(tfl_ai_requests = function(path) data.frame(
    request = c("ae-1", "ae-1a"), outputs = c("T-1", "T-2"), n = 1L, chars = 10L,
    file = c("ai/requests/ae-1.md", "ai/requests/ae-1a.md"), stringsAsFactors = FALSE),
    .package = "tflspec")
  r <- .ai_requests(d)
  expect_identical(r$family, c("ae", "ae"))
  expect_true(all(is.na(r$book)))
  expect_identical(r$drafts, c(0L, 0L))
})

test_that("the drafts' check is NULL when tflspec's check stops", {
  d <- cov_ai_dir()
  local_mocked_bindings(tfl_check_spec = function(...) stop("cannot read"), .package = "tflspec")
  expect_null(.ai_check_drafts(d))
})

test_that("what is to confirm: NULL when tflspec says no table, the missing columns added as NA", {
  d <- cov_ai_dir()
  local_mocked_bindings(tfl_ai_confirm = function(...) NULL, .package = "tflspec")
  expect_null(.ai_confirm(d))
  local_mocked_bindings(tfl_ai_confirm = function(...) data.frame(output_id = "T-1"),
                        .package = "tflspec")
  cf <- .ai_confirm(d)
  expect_named(cf, c("output_id", "where", "item", "why", "file"), ignore.order = TRUE)
  expect_true(is.na(cf$why))
})

test_that("the ADaM folder of a study with no datasets and no data/adam is none", {
  s <- list(path = withr_tempdir(), planner = list(ard = list(datasets = NULL)))
  expect_identical(.ai_adam_default(s), "")
})

test_that("a dry run's reports already asked for: none with no study folder, those of the requests otherwise", {
  expect_identical(nrow(.ai_already_asked(NULL, "T-1")), 0L)
  local_mocked_bindings(tfl_ai_requests = function(path) data.frame(
    request = c("ae-1", "ae-2"), outputs = c("T-1, T-2", "T-3"), stringsAsFactors = FALSE),
    .package = "tflspec")
  a <- .ai_already_asked(withr_tempdir(), c("T-2", "T-3", "T-9"))
  expect_identical(a$output_id, c("T-2", "T-3"))
  expect_identical(a$request, c("ae-1", "ae-2"))
})

test_that("the TOC kept in docs/: the same file already there is taken, another of its name stops", {
  d <- withr_tempdir()
  src <- file.path(withr_tempdir(), "toc.xlsx")
  writeBin(charToRaw("PK\003\004 one"), src)
  to <- .ai_keep_toc(d, src)
  expect_identical(to, file.path(d, "docs", "toc.xlsx"))
  expect_identical(.ai_keep_toc(d, src), to)
  other <- file.path(withr_tempdir(), "toc.xlsx")
  writeBin(charToRaw("PK\003\004 two"), other)
  expect_error(.ai_keep_toc(d, other), "has another toc.xlsx already", fixed = TRUE)
})

test_that("a workbook chosen outside the study is copied into docs/, the same one taken, another stops", {
  d <- withr_tempdir()
  src <- file.path(withr_tempdir(), "shell.xlsx")
  writeBin(charToRaw("PK\003\004 one"), src)
  to <- .ai_book_into_docs(src, d)
  expect_identical(to, normalizePath(file.path(d, "docs", "shell.xlsx"), "/"))
  expect_identical(.ai_book_into_docs(src, d), to)
  other <- file.path(withr_tempdir(), "shell.xlsx")
  writeBin(charToRaw("PK\003\004 two"), other)
  expect_error(.ai_book_into_docs(other, d), "has another shell.xlsx already", fixed = TRUE)
  # no study folder yet: the path as it is
  expect_identical(.ai_book_into_docs(src, NULL), src)
})

test_that("a file dialog's start: a file itself (Windows separators on Windows), nothing for a path not there", {
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  writeLines("x", f)
  expect_identical(.ai_file_default(f, windows = FALSE), f)
  expect_identical(.ai_file_default(f, windows = TRUE), normalizePath(f, "\\", mustWork = FALSE))
  expect_identical(.ai_file_default(file.path(tempdir(), "no-such-file.xlsx"), windows = FALSE), "")
  expect_identical(.ai_file_default(NA_character_), "")
})

test_that("the dialogs the tests choose with: a folder, a file, an ADaM spec; NA when cancelled", {
  withr::local_options(tflplanner.choose_dir = function(default) "C:\\study\\adam",
                       tflplanner.choose_file = function(default) character())
  expect_identical(.ai_choose_dir(), normalizePath("C:\\study\\adam", "/", mustWork = FALSE))
  expect_identical(.ai_choose_file(), NA_character_)
  expect_identical(.ai_choose_adam_spec(xls = TRUE), NA_character_)
})

test_that("shell workbooks and references chosen: the empty and NA ones left out, none when cancelled", {
  withr::local_options(tflplanner.choose_files = function(default) c("a.xlsx", NA, ""),
                       tflplanner.choose_refs = function(default) NULL)
  expect_identical(.ai_choose_files(), normalizePath("a.xlsx", "/", mustWork = FALSE))
  expect_identical(.ai_choose_refs(), character())
})

test_that("drafts that cannot be put apart are said in a message, not an error", {
  d <- cov_ai_dir()
  local_mocked_bindings(tfl_ai_migrate = function(path) stop("locked"), .package = "tflspec")
  expect_message(out <- .ai_ensure_area(d), "not put apart: locked")
  expect_true(out)
})

test_that("a review level that is not one is NA; `none`, `off` and FALSE are none", {
  expect_identical(.ai_review_level(NULL), NA_character_)
  expect_identical(.ai_review_level(NA), NA_character_)
  expect_identical(.ai_review_level("Off"), "none")
  expect_identical(.ai_review_level(FALSE), "none")
  expect_identical(.ai_review_level(7), NA_character_)
})

test_that("one review level for every kind: a yml's ai: review that is not a map", {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("ai:", "  review: 0"), f)
  expect_identical(unname(.ai_review_file(f)), c("0", "0", "0"))
  writeLines("not: [a, map", f)
  expect_true(all(is.na(.ai_review_file(f))))
})

test_that("the review levels and the run setting already in study.yml are not written again", {
  d <- withr_tempdir()
  writeLines(c("study_id: X1", "# kept"), file.path(d, "study.yml"))
  before <- readLines(file.path(d, "study.yml"))
  expect_false(.ai_review_set(d, as.list(.ai_review_read(d))))
  expect_false(.ai_run_set(d, "one"))
  expect_false(.ai_run_set(d, "sometimes"))
  expect_identical(readLines(file.path(d, "study.yml")), before)
  # a study with no AGENTS.md: nothing to make again
  expect_false(.ai_agent_refresh(d))
})

test_that("a stored path: a relative one kept as it is, one outside the study absolute", {
  d <- withr_tempdir()
  expect_identical(.ai_path_store(c("docs/toc.xlsx", "", NA), d), "docs/toc.xlsx")
  out <- file.path(withr_tempdir(), "toc.xlsx")
  expect_identical(.ai_path_store(out, d), gsub("\\\\", "/", out))
  expect_identical(.ai_path_store(file.path(d, "docs", "toc.xlsx"), d), "docs/toc.xlsx")
})

test_that("step 1's settings from tflspec: none, an error said, a time from the file when `written` does not read", {
  d <- withr_tempdir()
  local_mocked_bindings(tfl_ai_prepare_settings = function(dir) NULL, .package = "tflspec")
  expect_identical(.ai_step1_prepared(d), list(x = NULL, error = NULL))
  local_mocked_bindings(tfl_ai_prepare_settings = function(dir) stop("a newer format"),
                        .package = "tflspec")
  expect_identical(.ai_step1_prepared(d)$error, "a newer format")
  f <- file.path(d, "settings.yml")
  writeLines("toc: x", f)
  local_mocked_bindings(tfl_ai_prepare_settings = function(dir) structure(
    list(toc = "docs/toc.xlsx", define = "define.xml", templates = "yes"),
    written = "not a time", file = f), .package = "tflspec")
  s <- .ai_step1_prepared(d)
  expect_identical(s$x$toc, "docs/toc.xlsx")
  expect_identical(s$x$adam_spec, "define.xml")
  expect_false(s$x$templates)
  expect_equal(s$time, file.mtime(f))
})

test_that("the agent mode asks for one report a request, the chat three", {
  expect_identical(.ai_size_default("agent"), 1)
  expect_identical(.ai_size_default(), 3)
})
