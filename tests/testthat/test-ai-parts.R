# AI tab and tflspec >= 0.0.24.9211 (#470): a report's parts on its shell
# sheet, a number the TOC names twice, requests named after their report
# (size = 1), and a workbook already in the study folder read where it is

skip_on_cran()
skip_if_not_installed("haven")
skip_if_not_installed("openxlsx")

# A workbook of the company's shape: a TOC; a report whose sheet holds it in
# two parts (.a whole, .b below it with what differs); a second TOC sheet
# naming one number twice
parts_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = c("t.14.1.1", "t.15.1.5.1"), Title = c("Disposition", "Demographic Characteristics")))
  openxlsx::addWorksheet(wb, "TOC2")
  openxlsx::writeData(wb, "TOC2", data.frame(
    No = c("t.14.1.1", "T.14.1.1", "t.15.1.5.1"), Title = c("Disposition", "Disposition again",
                                                          "Demographic Characteristics")))
  openxlsx::addWorksheet(wb, "t.14.1.1")
  openxlsx::writeData(wb, "t.14.1.1", "Table 14.1.1  Disposition", startRow = 1)
  s <- "t.15.1.5.1"
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, "Table 15.1.5.1.a", startRow = 5)
  openxlsx::writeData(wb, s, "Demographic Characteristics - Phase 1 Part", startRow = 6)
  openxlsx::writeData(wb, s, "SAFFL = \"Y\" and APHASE = \"P1\"", startRow = 7)
  openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 9, colNames = FALSE)
  openxlsx::writeData(wb, s, t(c("Age (years)", "xx.x", "xx.x")), startRow = 10, colNames = FALSE)
  openxlsx::writeData(wb, s, "Table 15.1.5.1.b", startRow = 85)
  openxlsx::writeData(wb, s, "Demographic Characteristics - Phase 2 Part", startRow = 86)
  openxlsx::writeData(wb, s, "APHASE = \"P2\"; columns TRT01AN = 2", startRow = 87)
  openxlsx::saveWorkbook(wb, f)
  f
}

parts_adam <- function(d) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  df <- tflspec::adsl
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adsl.xpt"))
  normalizePath(d, "/")
}

test_that("a dry run: a report's parts, a number named twice, requests named after their report", {
  root <- tempfile("parts-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(root)
  book <- parts_book(file.path(root, "toc_shell.xlsx"))
  adam <- parts_adam(file.path(root, "adam"))
  dir <- file.path(root, "P-1")
  p <- .ai_prepare(dir, book, adam, list(size = 1L, mode = "agent", templates = FALSE),
                   dry_run = TRUE)
  expect_identical(p$toc$output_id, c("t.14.1.1", "t.15.1.5.1"))
  expect_identical(p$toc$parts, c(NA, "t.15.1.5.1.a | t.15.1.5.1.b"))
  expect_identical(p$toc$same_number, c(FALSE, FALSE))
  expect_identical(.ai_parts_label(p$toc$output_id, p$toc$parts), c("", "2 parts (.a, .b)"))
  # size = 1: named after the report, as written
  expect_identical(p$requests$request, c("t.14.1.1", "t.15.1.5.1"))
  expect_identical(p$toc$request, c("t.14.1.1", "t.15.1.5.1"))
  expect_identical(.ai_dry_requests(p$toc, 1L)$request, c("t.14.1.1", "t.15.1.5.1"))
  expect_false(any(grepl("^t[.]", .ai_dry_requests(p$toc, 3L)$request)))
  # the second TOC sheet names t.14.1.1 twice: its first row kept, marked
  p2 <- .ai_prepare(dir, book, adam, list(size = 3L, toc_sheet = "TOC2", templates = FALSE),
                    dry_run = TRUE)
  expect_identical(p2$toc$same_number, c(TRUE, FALSE))
  expect_false(dir.exists(dir))
  # no parts column (an older reading): nothing said
  expect_identical(.ai_parts_label(c("a", "b"), NULL), c("", ""))
  expect_identical(.ai_one_names(c("t 15.1", "T15.1.5.1", "t 15.1")), c("t-15.1", "T15.1.5.1", "t-15.1-1"))
})

test_that("a workbook in the study folder is read where it is; one outside is copied into ai/", {
  local_home()
  root <- studies_root()
  dir <- file.path(root, "P-2")
  dir.create(file.path(dir, "docs"), recursive = TRUE)
  inside <- parts_book(file.path(dir, "docs", "toc_shell.xlsx"))
  outside <- parts_book(file.path(tempdir(), "outside_toc.xlsx"))
  on.exit(unlink(outside), add = TRUE)
  expect_true(.ai_in_study(inside, dir))
  expect_false(.ai_in_study(outside, dir))
  # a sibling folder that only starts with the study's name is outside
  sib <- file.path(paste0(dir, "-x"), "toc.xlsx")
  dir.create(dirname(sib))
  file.copy(outside, sib)
  expect_false(.ai_in_study(sib, dir))
  expect_identical(.ai_keep_toc(dir, inside), normalizePath(inside, "/"))
  expect_false(file.exists(file.path(dir, "ai", "toc_shell.xlsx")))
  kept <- .ai_keep_toc(dir, outside, "renamed.xlsx")
  expect_true(file.exists(file.path(dir, "ai", "renamed.xlsx")))
  expect_identical(normalizePath(kept, "/"), normalizePath(file.path(dir, "ai", "renamed.xlsx"), "/"))
  expect_identical(.ai_keep_shells(dir, list(list(path = inside, name = "x.xlsx"))),
                   normalizePath(inside, "/"))
  # a new study made from the workbook in its docs/: nothing copied, the
  # requests named after their reports, their parts said
  adam <- parts_adam(file.path(dir, "data", "adam"))
  out <- .ai_new_study("P-2", inside, "toc_shell.xlsx", adam,
                       list(size = 1L, mode = "agent", workbook = "file", templates = FALSE))
  expect_false(file.exists(file.path(dir, "ai", "toc_shell.xlsx")))
  r <- .ai_requests(dir)
  expect_setequal(r$request, c("t.14.1.1", "t.15.1.5.1"))
  expect_identical(r$file[r$request == "t.15.1.5.1"], "ai/requests/t.15.1.5.1.md")
  txt <- .ai_request_text(dir, "t.15.1.5.1", "request")
  expect_match(txt, "docs/toc_shell.xlsx", fixed = TRUE)
  expect_match(txt, "2 parts", fixed = TRUE)
  # an answer and a fix request by a name with dots
  dir.create(file.path(dir, "ai", "answers"), showWarnings = FALSE)
  writeLines("x", file.path(dir, "ai", "answers", "t.15.1.5.1.md"))
  expect_identical(.ai_answer_file(dir, "t.15.1.5.1"), "ai/answers/t.15.1.5.1.md")
  dir.create(file.path(dir, "ai", "repairs"), showWarnings = FALSE)
  writeLines(c("## Could not be built", "", "- t.15.1.5.1.b: no data"),
             file.path(dir, "ai", "repairs", "t.15.1.5.1-build.md"))
  u <- .ai_unbuilt(dir)
  expect_identical(u$request, "t.15.1.5.1")
  expect_identical(u$output_id, "t.15.1.5.1.b")
  expect_identical(attr(u, "build"), "t.15.1.5.1")
})

test_that("step 1's TOC table says a report's parts; a number named twice is warned about", {
  local_home()
  create_study("P-3")
  book <- parts_book(file.path(tempdir(), "p3_toc.xlsx"))
  adam <- parts_adam(file.path(tempdir(), "p3-adam"))
  on.exit(unlink(c(book, adam), recursive = TRUE), add = TRUE)
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old), add = TRUE)
  shiny::testServer(server_for("P-3"), {
    session$setInputs(`ai-study_mode` = "open", `ai-toc_path` = book, `ai-adam_dir` = adam,
                      `ai-size` = 1, `ai-mode` = "agent", `ai-add` = FALSE)
    session$setInputs(`ai-read` = 1)
    expect_match(output$`ai-toc_table`, "2 parts (.a, .b)", fixed = TRUE)
    expect_no_match(output$`ai-toc_head`$html, "more than once", fixed = TRUE)
    session$setInputs(`ai-toc_sheet` = "TOC2")
    session$setInputs(`ai-read` = 2)
    expect_match(output$`ai-toc_head`$html, "names t.14.1.1 more than once", fixed = TRUE)
  })
})
