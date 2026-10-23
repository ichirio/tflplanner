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

test_that("the inputs say when the workbook is copied, what the sheets add, what Read checks", {
  h <- as.character(ai_ui("ai"))
  expect_match(h, "a path inside the study folder (its docs/, say), no", fixed = TRUE)
  expect_match(h, "Sheets to add to every request", fixed = TRUE)
  expect_match(h, "Several can be chosen.", fixed = TRUE)
  expect_match(h, "Read and check", fixed = TRUE)
  expect_match(h, "or their file paths (several: one a line)", fixed = TRUE)
})

test_that("every new text has its Japanese", {
  s <- utils::read.csv(system.file("i18n", "strings.csv", package = "tflplanner"),
                       encoding = "UTF-8", stringsAsFactors = FALSE)
  for (x in c("Read and check", "Sheets to add to every request", "none (the shells found)",
              "or their file paths (several: one a line)",
              "Documents for reference: SAP, TOC / SHELL workbooks")) {
    expect_true(x %in% s[[1L]], info = x)
  }
})
