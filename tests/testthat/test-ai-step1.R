# The AI tab's step 1 kept with the study (#512): ai/step1.yml, its helpers
# (R/ai_draft.R) and the tab filling step 1 from it

test_that("paths inside the study are stored relative to it, others absolute", {
  d <- normalizePath(withr_tempdir(), "/")
  study <- file.path(d, "S-1")
  dir.create(study)
  inside <- file.path(study, "docs", "TOC.xlsx")
  outside <- file.path(d, "elsewhere", "SAP.pdf")
  expect_identical(.ai_path_store(c(inside, outside), study), c("docs/TOC.xlsx", outside))
  # (back slashes, quotes and blanks; case of the drive and folders ignored)
  expect_identical(.ai_path_store(paste0('"', gsub("/", "\\\\", inside), '" '), study), "docs/TOC.xlsx")
  expect_identical(.ai_path_store(c("", NA), study), character())
  # read back: a relative path from the study folder, an absolute one as it is
  back <- .ai_path_resolve(c("docs/TOC.xlsx", outside), study)
  expect_identical(back, c(normalizePath(inside, "/", mustWork = FALSE), outside))
  # (a folder with the study's name as a prefix is not inside)
  expect_identical(.ai_path_store(paste0(study, "-2/x.xlsx"), study), paste0(study, "-2/x.xlsx"))
})

test_that("step1.yml: every field round trips, paths relative inside the study", {
  skip_if_not_installed("yaml")
  d <- normalizePath(withr_tempdir(), "/")
  study <- file.path(d, "S-1")
  dir.create(study)
  x <- list(toc = file.path(study, "docs", "TOC_SHELL.xlsx"), toc_sheet = "TOC",
            shells = c(file.path(study, "docs", "SHELLS.xlsx"), file.path(d, "out", "S2.xlsx")),
            adam = file.path(study, "data", "adam"), adam_spec = file.path(d, "define.xml"),
            docs = file.path(study, "ai", "docs", "SAP.pdf"), language = "Japanese",
            templates = FALSE, agent_reads = c("workbook", "adam"), agent_max_reads = 5L,
            sheets = c("Notes", "T-14-1"),
            sheet_roles = c(TOC = "toc", Safety = "other", `T-14-1` = "shell", yes = "other"))
  f <- .ai_step1_write(study, x)
  expect_identical(f, file.path(study, "ai", "step1.yml"))
  txt <- paste(readLines(f), collapse = "\n")
  expect_match(txt, "toc: docs/TOC_SHELL.xlsx", fixed = TRUE)
  expect_match(txt, "adam: data/adam", fixed = TRUE)
  expect_match(txt, "- docs/SHELLS.xlsx", fixed = TRUE)
  expect_false(grepl(study, txt, fixed = TRUE))
  y <- .ai_step1_read(study)
  abs <- function(p) normalizePath(p, "/", mustWork = FALSE)
  expect_identical(y$toc, abs(x$toc))
  expect_identical(y$shells, c(abs(x$shells[1L]), x$shells[2L]))
  expect_identical(y$adam, abs(x$adam))
  expect_identical(y$adam_spec, x$adam_spec)
  expect_identical(y$docs, abs(x$docs))
  expect_identical(y[c("toc_sheet", "language", "templates", "agent_reads", "agent_max_reads", "sheets")],
                   x[c("toc_sheet", "language", "templates", "agent_reads", "agent_max_reads", "sheets")])
  expect_identical(y$sheet_roles, x$sheet_roles)
  # the study copied elsewhere: the relative paths follow it
  copy <- file.path(d, "S-1-copy")
  dir.create(file.path(copy, "ai"), recursive = TRUE)
  file.copy(f, file.path(copy, "ai", "step1.yml"))
  expect_identical(.ai_step1_read(copy)$toc, abs(file.path(copy, "docs", "TOC_SHELL.xlsx")))
})

test_that("step1.yml: blanks, the chat's language and no roles; none kept reads as NULL", {
  skip_if_not_installed("yaml")
  d <- normalizePath(withr_tempdir(), "/")
  expect_null(.ai_step1_read(d))
  .ai_step1_write(d, list(toc = NULL, shells = character(), adam = "", language = "",
                          templates = TRUE, agent_reads = character(), sheets = character()))
  y <- .ai_step1_read(d)
  expect_null(y$toc)
  expect_identical(y$shells, character())
  expect_null(y$adam)
  expect_identical(y$language, "")
  expect_true(y$templates)
  expect_identical(y$agent_reads, character())
  expect_null(y$sheet_roles)
  # a file that does not read: none (said in the console)
  writeLines("toc: [unclosed", file.path(d, "ai", "step1.yml"))
  expect_null(suppressMessages(.ai_step1_read(d)))
})

test_that("a kept path that is gone is flagged, one that is there is not", {
  skip_if_not_installed("yaml")
  d <- normalizePath(withr_tempdir(), "/")
  study <- file.path(d, "S-1")
  dir.create(file.path(study, "docs"), recursive = TRUE)
  dir.create(file.path(study, "data", "adam"), recursive = TRUE)
  writeLines("x", file.path(study, "docs", "TOC.xlsx"))
  .ai_step1_write(study, list(
    toc = file.path(study, "docs", "TOC.xlsx"), shells = file.path(study, "docs", "GONE.xlsx"),
    adam = file.path(study, "data", "adam"), adam_spec = file.path(study, "docs", "define.xml"),
    docs = c(file.path(d, "SAP.pdf"), file.path(study, "data", "adam"))))
  m <- .ai_step1_missing(.ai_step1_read(study))
  expect_identical(m$field, c("shells", "adam_spec", "docs", "docs"))
  expect_identical(basename(m$path), c("GONE.xlsx", "define.xml", "SAP.pdf", "adam"))
  # (the gone folder of adam)
  unlink(file.path(study, "data", "adam"), recursive = TRUE)
  expect_true("adam" %in% .ai_step1_missing(.ai_step1_read(study))$field)
  expect_identical(nrow(.ai_step1_missing(list())), 0L)
})

test_that("sheet roles are kept by sheet name; those of sheets that are gone are listed", {
  saved <- c(TOC = "toc", Safety = "other", Old = "other", `T-14-1` = "shell")
  r <- .ai_step1_roles(saved, c("T-14-1", "TOC", "Safety", "New"))
  expect_identical(r$roles, c(TOC = "toc", Safety = "other", `T-14-1` = "shell"))
  expect_identical(r$gone, "Old")
  # (a workbook with none of them)
  r <- .ai_step1_roles(saved, "Other")
  expect_null(r$roles)
  expect_identical(r$gone, names(saved))
  expect_identical(.ai_step1_roles(NULL, "A")$gone, NULL)
})

test_that("where a workbook is kept: inside the study as it is, else docs/, an upload always docs/", {
  d <- normalizePath(withr_tempdir(), "/")
  study <- file.path(d, "S-1")
  dir.create(file.path(study, "docs"), recursive = TRUE)
  inside <- file.path(study, "docs", "in.xlsx")
  out <- file.path(d, "out.xlsx")
  writeLines("x", inside)
  writeLines("x", out)
  expect_identical(.ai_kept_path(inside, "in.xlsx", study), normalizePath(inside, "/"))
  # not copied yet: the path as typed
  expect_identical(.ai_kept_path(out, "out.xlsx", study), out)
  # copied: the copy
  file.copy(out, file.path(study, "docs", "out.xlsx"))
  expect_identical(.ai_kept_path(out, "out.xlsx", study), file.path(study, "docs", "out.xlsx"))
  # an upload: its temporary path is of no use; the copy it will be
  expect_identical(.ai_kept_path(tempfile(fileext = ".xlsx"), "up.xlsx", study, upload = TRUE),
                   file.path(study, "docs", "up.xlsx"))
})

# the updates the server sends to the fields (shiny's update*Input() mocked)
step1_catch <- function(env = parent.frame()) {
  sent <- new.env()
  keep <- function(id, ...) assign(id, list(...), envir = sent)
  testthat::local_mocked_bindings(
    updateTextInput = function(session, inputId, label = NULL, value = NULL, placeholder = NULL) {
      keep(inputId, value = value)
    },
    updateTextAreaInput = function(session, inputId, label = NULL, value = NULL, placeholder = NULL) {
      keep(inputId, value = value)
    },
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL) {
      keep(inputId, choices = choices, selected = selected)
    },
    updateSelectizeInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL,
                                    options = list(), server = FALSE) {
      keep(inputId, choices = choices, selected = selected)
    },
    updateCheckboxInput = function(session, inputId, label = NULL, value = NULL) {
      keep(inputId, value = value)
    },
    updateCheckboxGroupInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL,
                                        inline = FALSE, choiceNames = NULL, choiceValues = NULL) {
      keep(inputId, selected = selected)
    },
    updateNumericInput = function(session, inputId, label = NULL, value = NULL, min = NULL,
                                  max = NULL, step = NULL) {
      keep(inputId, value = value)
    },
    .package = "shiny", .env = env)
  sent
}

test_that("a study opened in the tab: step 1 filled from ai/step1.yml, a path gone still shown", {
  skip_if_not_installed("yaml")
  local_home()
  s <- create_study("KEP-1")
  .ai_step1_write(s$path, list(
    toc = file.path(s$path, "docs", "TOC.xlsx"), toc_sheet = "TOC",
    shells = file.path(s$path, "docs", "SHELLS.xlsx"), adam = file.path(s$path, "data", "adam"),
    adam_spec = file.path(s$path, "docs", "define.xml"), docs = file.path(s$path, "ai", "docs", "SAP.pdf"),
    language = "English", templates = FALSE, agent_reads = "adam", agent_max_reads = 7L,
    sheets = "Notes", sheet_roles = c(TOC = "toc", Safety = "other")))
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  sent <- step1_catch()
  shiny::testServer(server_for("KEP-1"), {
    session$flushReact()
    abs <- function(...) normalizePath(file.path(s$path, ...), "/", mustWork = FALSE)
    expect_identical(sent$toc_path$value, abs("docs", "TOC.xlsx"))
    expect_identical(sent$shell_paths$value, abs("docs", "SHELLS.xlsx"))
    expect_identical(sent$adam_dir$value, abs("data", "adam"))
    expect_identical(sent$adam_spec$value, abs("docs", "define.xml"))
    expect_identical(sent$language$selected, "English")
    expect_false(sent$templates$value)
    expect_identical(sent$agent_reads$selected, "adam")
    expect_identical(sent$agent_max_reads$value, 7L)
    expect_identical(sent$toc_sheet$selected, "TOC")
    expect_identical(sent$sheets$selected, "Notes")
    # (the docs list shows the file that is not there, flagged)
    html <- output[["ai-ref_list"]]$html
    expect_match(html, "SAP.pdf", fixed = TRUE)
    expect_match(html, "not found", fixed = TRUE)
    expect_match(output[["ai-step1_note"]]$html, "ai/step1.yml", fixed = TRUE)
  })
})

test_that("the dry run keeps step 1 in ai/step1.yml first; a study with no ai/ waits for the write", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("haven")
  local_home()
  s <- create_study("KEP-2")
  adam <- file.path(s$path, "data", "adam")
  dir.create(adam, recursive = TRUE)
  df <- utils::head(tflspec::adsl, 5L)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(adam, "adsl.xpt"))
  toc <- file.path(s$path, "docs", "toc.xlsx")
  dir.create(dirname(toc), recursive = TRUE, showWarnings = FALSE)
  file.copy(system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"), toc)
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("KEP-2"), {
    adam <- normalizePath(adam, "/")
    session$setInputs(`ai-study_mode` = "open", `ai-toc_path` = toc, `ai-shell_paths` = "",
                      `ai-adam_dir` = adam, `ai-adam_spec` = "", `ai-toc_sheet` = "",
                      `ai-language` = "Japanese", `ai-templates` = FALSE,
                      `ai-agent_reads` = "workbook", `ai-agent_max_reads` = 4L)
    # (no ai/ folder yet: the dry run writes nothing into the study)
    session$setInputs(`ai-read` = 1L)
    expect_false(file.exists(file.path(s$path, "ai", "step1.yml")))
    # with the folder there (a study asked before): kept at the dry run
    dir.create(file.path(s$path, "ai"))
    session$setInputs(`ai-read` = 2L)
    y <- .ai_step1_read(s$path)
    expect_identical(y$toc, normalizePath(toc, "/"))
    expect_identical(y$adam, adam)
    expect_identical(y$language, "Japanese")
    expect_false(y$templates)
    expect_identical(y$agent_reads, "workbook")
    expect_identical(y$agent_max_reads, 4L)
    txt <- readLines(file.path(s$path, "ai", "step1.yml"))
    expect_true("toc: docs/toc.xlsx" %in% txt)
    expect_true("adam: data/adam" %in% txt)
  })
})

test_that("a new study: step 1 held until the requests are written, then kept in its ai/step1.yml", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("haven")
  local_home()
  create_study("KEP-3")
  adam <- normalizePath(file.path(withr_tempdir(), "adam"), "/", mustWork = FALSE)
  dir.create(adam)
  df <- utils::head(tflspec::adsl, 5L)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(adam, "adsl.xpt"))
  toc <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  shiny::testServer(server_for("KEP-3"), {
    session$setInputs(`ai-study_mode` = "new", `ai-new_id` = "NEW-3", `ai-new_title` = "Three",
                      `ai-toc_path` = toc, `ai-shell_paths` = "", `ai-adam_dir` = adam,
                      `ai-adam_spec` = "", `ai-language` = "English", `ai-templates` = TRUE,
                      `ai-agent_reads` = "adam", `ai-agent_max_reads` = 2L, `ai-max_levels` = 15)
    session$setInputs(`ai-read` = 1)
    dir <- file.path(studies_root(), "NEW-3")
    # (read only: the study's folder is not there)
    expect_false(dir.exists(dir))
    session$setInputs(`ai-write` = 1)
    expect_true(dir.exists(dir))
    y <- .ai_step1_read(dir)
    # the workbook as kept in the study (the copy), the ADaM folder where it is
    expect_identical(y$toc, normalizePath(file.path(dir, "docs", basename(toc)), "/"))
    expect_true(file.exists(y$toc))
    expect_identical(y$adam, adam)
    expect_identical(y$agent_reads, "adam")
    expect_identical(y$agent_max_reads, 2L)
    expect_true(any(readLines(file.path(dir, "ai", "step1.yml")) == paste0("toc: docs/", basename(toc))))
    expect_identical(nrow(.ai_step1_missing(y)), 0L)
  })
})
