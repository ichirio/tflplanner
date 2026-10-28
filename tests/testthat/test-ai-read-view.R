# AI tab (#473): what was read, as tflspec's
# sheet-first dry run gives it -- the TOC items (their shell, a TOC item
# with no shell in red), the shells (a request a sheet), the counts, the
# sheets' roles changed and read again -- and a workbook already in the
# study folder read where it is

skip_on_cran()
skip_if_not_installed("haven")
skip_if_not_installed("openxlsx")

# A workbook: a TOC of three items, two with a shell sheet of their own,
# one with none; a sheet of notes (not a shell)
view_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = c("t.14.1.1", "t.15.1.5.1", "t.16.1"),
    Title = c("Disposition", "Demographic Characteristics", "Listing of Deaths")))
  openxlsx::addWorksheet(wb, "t.14.1.1")
  openxlsx::writeData(wb, "t.14.1.1", "Table 14.1.1  Disposition", startRow = 1)
  s <- "t.15.1.5.1"
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, "Table 15.1.5.1", startRow = 5)
  openxlsx::writeData(wb, s, "Demographic Characteristics", startRow = 6)
  openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 9, colNames = FALSE)
  openxlsx::addWorksheet(wb, "Notes")
  openxlsx::writeData(wb, "Notes", "General notes", startRow = 1)
  openxlsx::saveWorkbook(wb, f)
  f
}

view_adam <- function(d) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  df <- tflspec::adsl
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adsl.xpt"))
  normalizePath(d, "/")
}

test_that("a dry run: a row a TOC item, a row a shell sheet, the counts; requests named after their sheet", {
  root <- tempfile("view-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(root)
  book <- view_book(file.path(root, "toc_shell.xlsx"))
  adam <- view_adam(file.path(root, "adam"))
  dir <- file.path(root, "P-1")
  p <- .ai_prepare(dir, book, adam, list(size = 1L, mode = "agent", templates = FALSE),
                   dry_run = TRUE)
  # (tflspec's shape, as it gives it: nothing added by tflplanner)
  expect_identical(p$toc$toc_id, c("t.14.1.1", "t.15.1.5.1", "t.16.1"))
  expect_null(p$toc$parts)
  expect_identical(p$toc$shell_found, c(TRUE, TRUE, FALSE))
  expect_identical(p$toc$request, c("t.14.1.1", "t.15.1.5.1", "no-shell"))
  expect_identical(p$shells$sheet, c("t.14.1.1", "t.15.1.5.1"))
  expect_identical(p$shells$request, p$shells$sheet)
  expect_identical(as.integer(p$counts[c("shells", "toc_items", "no_shell")]), c(2L, 3L, 1L))
  expect_identical(p$requests$request, c("t.14.1.1", "t.15.1.5.1", "no-shell"))
  expect_identical(p$sheets$role, c("toc", "shell", "shell", "other"))
  # a sheet's role given: read as it says
  p2 <- .ai_prepare(dir, book, adam, list(size = 1L, mode = "agent", templates = FALSE,
                                          sheet_roles = c(TOC = "toc", t.14.1.1 = "shell",
                                                          t.15.1.5.1 = "other", Notes = "other")),
                    dry_run = TRUE)
  expect_identical(as.integer(p2$counts[c("shells", "toc_items", "no_shell")]), c(1L, 3L, 2L))
  expect_false(dir.exists(dir))
  # (the parts said, the names made from the report's number)
  expect_identical(.ai_parts_label(c("a", "b"), NULL), c("", ""))
  expect_identical(.ai_parts_label(c("t.1", "t.2"), c(NA, "t.2.a | t.2.b")), c("", "2 parts (.a, .b)"))
  expect_identical(.ai_one_names(c("t 15.1", "T15.1.5.1", "t 15.1")), c("t-15.1", "T15.1.5.1", "t-15.1-1"))
})

test_that("a workbook in the study folder is read where it is; one outside is copied into docs/", {
  local_home()
  root <- studies_root()
  dir <- file.path(root, "P-2")
  dir.create(file.path(dir, "docs"), recursive = TRUE)
  inside <- view_book(file.path(dir, "docs", "toc_shell.xlsx"))
  outside <- view_book(file.path(tempdir(), "outside_toc.xlsx"))
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
  expect_true(file.exists(file.path(dir, "docs", "renamed.xlsx")))
  expect_identical(normalizePath(kept, "/"), normalizePath(file.path(dir, "docs", "renamed.xlsx"), "/"))
  expect_identical(.ai_keep_shells(dir, list(list(path = inside, name = "x.xlsx"))),
                   normalizePath(inside, "/"))
  # a new study made from the workbook in its docs/: nothing copied, the
  # requests named after their sheets
  adam <- view_adam(file.path(dir, "data", "adam"))
  out <- .ai_new_study("P-2", inside, "toc_shell.xlsx", adam,
                       list(size = 1L, mode = "agent", workbook = "file", templates = FALSE))
  expect_false(file.exists(file.path(dir, "ai", "toc_shell.xlsx")))
  r <- .ai_requests(dir)
  expect_setequal(r$request, c("t.14.1.1", "t.15.1.5.1", "no-shell"))
  expect_identical(r$file[r$request == "t.15.1.5.1"], "ai/requests/t.15.1.5.1.md")
  txt <- .ai_request_text(dir, "t.15.1.5.1", "request")
  expect_match(txt, "docs/toc_shell.xlsx", fixed = TRUE)
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

test_that("step 1: the TOC items (no shell in red), the shells, the counts; the sheets' roles applied and read again", {
  local_home()
  create_study("P-3")
  book <- view_book(file.path(tempdir(), "p3_toc.xlsx"))
  adam <- view_adam(file.path(tempdir(), "p3-adam"))
  on.exit(unlink(c(book, adam), recursive = TRUE), add = TRUE)
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old), add = TRUE)
  # each read's arguments kept, then read as it would be
  got <- new.env()
  got$roles <- list()
  real <- .ai_prepare
  local_mocked_bindings(.ai_prepare = function(dir, toc, adam, args = list(), ...) {
    got$roles <- c(got$roles, list(args$sheet_roles))
    real(dir, toc, adam, args, ...)
  })
  shiny::testServer(server_for("P-3"), {
    ai <- session$userData$ai
    session$setInputs(`ai-study_mode` = "open", `ai-toc_path` = book, `ai-adam_dir` = adam,
                      `ai-size` = 1, `ai-mode` = "agent", `ai-add` = FALSE)
    session$setInputs(`ai-read` = 1)
    expect_true(ai$prepared()$dry_run)
    # (no roles changed: none passed)
    expect_null(got$roles[[length(got$roles)]])
    # the TOC items: a row each, its shell or a red dash
    tt <- output$`ai-toc_table`
    expect_match(tt, "TOC item", fixed = TRUE)
    expect_match(tt, "t.16.1", fixed = TRUE)
    expect_match(tt, "no-shell", fixed = TRUE)
    expect_match(tt, "#f8d7da", fixed = TRUE)
    # the counts, the TOC item with no shell named
    h <- output$`ai-toc_head`$html
    expect_match(h, "2 shells, 3 TOC items, 1 of them with no shell", fixed = TRUE)
    expect_match(h, "1 TOC items with no shell (t.16.1)", fixed = TRUE)
    # the shells: a row a sheet
    st <- output$`ai-shells_table`
    expect_match(st, "Numbers in the sheet", fixed = TRUE)
    expect_match(st, "Table 15.1.5.1", fixed = TRUE)
    expect_no_match(st, "Notes", fixed = TRUE)
    expect_match(output$`ai-prep_status`$html, "3 requests to write (2 shells, 3 TOC items).",
                 fixed = TRUE)
    # the sheets' roles: a shell read as other, applied -- read again with them
    expect_match(output$`ai-roles_ui`$html, "Apply and read again", fixed = TRUE)
    session$setInputs(`ai-role_toc` = "TOC", `ai-role_other` = c("Notes", "t.15.1.5.1"))
    session$setInputs(`ai-roles_apply` = 1)
    expect_identical(got$roles[[length(got$roles)]],
                     c(TOC = "toc", t.14.1.1 = "shell", t.15.1.5.1 = "other", Notes = "other"))
    expect_match(output$`ai-toc_head`$html, "1 shells, 3 TOC items, 2 of them with no shell",
                 fixed = TRUE)
    # (and kept for the next read)
    session$setInputs(`ai-read` = 2)
    expect_identical(got$roles[[length(got$roles)]][["t.15.1.5.1"]], "other")
  })
})
