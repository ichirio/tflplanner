# The AI tab after the user's second test (#473): a study's docs/ folder,
# the agent the default, the workbook's copy said, the reports in parts marked

test_that("a new study has a docs/ folder for its reference documents", {
  local_home()
  expect_identical(study_layout()[["docs"]], "docs")
  s <- create_study("DOC-1")
  expect_true(dir.exists(file.path(s$path, "docs")))
})

test_that("the drafting AI is the agent by default, one report a request", {
  h <- as.character(ai_ui("ai"))
  agent <- regmatches(h, regexpr("<input[^>]*value=\"agent\"[^>]*>", h))
  expect_match(agent, "checked", fixed = TRUE)
  size <- regmatches(h, regexpr("<input[^>]*id=\"ai-size\"[^>]*>", h))
  expect_match(size, "value=\"1\"", fixed = TRUE)
})

test_that("the inputs: one chooser for the workbook, short labels, a chat's size and instructions", {
  op <- options(tflplanner.file_dialog = TRUE)
  on.exit(options(op))
  h <- as.character(ai_ui("ai"))
  # (with the computer's dialog: no upload, the path and Browse...; with
  # none -- a Mac with no Tcl/Tk, a server -- the upload and its help)
  if (.ai_dialog_ok()) {
    expect_false(grepl("ai-toc_file", h, fixed = TRUE))
    expect_match(h, "copied into the study's docs/ when it is chosen with Browse...", fixed = TRUE)
  } else {
    expect_match(h, "ai-toc_file", fixed = TRUE)
    expect_match(h, "dialog is the browser", fixed = TRUE)
  }
  expect_match(h, "Sheets for every request", fixed = TRUE)
  expect_match(h, "Several can be chosen.", fixed = TRUE)
  expect_match(h, "Read and check", fixed = TRUE)
  # size and the common instructions: shown for a chat only
  expect_match(h, "data-display-if=\"input.mode == &#39;chat&#39;\"", fixed = TRUE)
  expect_match(h, "ai-instructions_each", fixed = TRUE)
  options(tflplanner.file_dialog = FALSE)
  h <- as.character(ai_ui("ai"))
  expect_match(h, "ai-toc_file", fixed = TRUE)
})

test_that("Browse...: a workbook outside the study copied into its docs/, one inside as it is", {
  d <- withr_tempdir()
  study <- file.path(d, "S-1")
  dir.create(file.path(study, "docs"), recursive = TRUE)
  out <- file.path(d, "outside.xlsx")
  writeLines("x", out)
  got <- .ai_book_into_docs(out, study)
  expect_identical(got, normalizePath(file.path(study, "docs", "outside.xlsx"), "/"))
  expect_true(file.exists(got))
  # the same again: the copy, nothing new
  expect_identical(.ai_book_into_docs(out, study), got)
  # another file of that name: stops
  writeLines("y", out)
  expect_error(.ai_book_into_docs(out, study), "another outside.xlsx")
  # inside the study: its own path
  inside <- file.path(study, "docs", "in.xlsx")
  writeLines("z", inside)
  expect_identical(.ai_book_into_docs(inside, study), normalizePath(inside, "/"))
  # no study folder yet: as it is
  expect_identical(.ai_book_into_docs(out, file.path(d, "none")), out)
})

test_that("every new text has its Japanese", {
  s <- .strings()
  for (x in c("Read and check", "Sheets for every request", "none (the shells found)",
              "Put the common instructions in each request",
              "Documents for reference: SAP, TOC / SHELL workbooks")) {
    expect_true(x %in% s[[1L]], info = x)
  }
})
