# The AI tab's docs field (#510): a path typed or pasted is added to the
# list on Enter or when the field is left (the page says so, in
# `ref_commit`); no Add button

test_that("the docs field has no Add button; the field commits on Enter and on leaving", {
  h <- as.character(ai_ui("ai"))
  expect_false(grepl("ai-ref_add", h, fixed = TRUE))
  expect_match(h, "ai-ref_path", fixed = TRUE)
  expect_match(h, 'data-commit="ai-ref_commit"', fixed = TRUE)
  expect_match(h, "press Enter", fixed = TRUE)
  expect_match(.ai_draft_js, "'focusout', '.ai-commit'", fixed = TRUE)
  expect_match(.ai_draft_js, "priority: 'event'", fixed = TRUE)
})

test_that("a path committed is added once to the list, quotes taken off; a missing file said", {
  local_home()
  create_study("REF-1")
  old <- options(tflplanner.ai_drafts = TRUE)
  on.exit(options(old))
  d <- withr_tempdir()
  f <- file.path(d, "SAP.pdf")
  writeLines("x", f)
  shiny::testServer(server_for("REF-1"), {
    session$setInputs(`ai-ref_commit` = list(value = paste0('"', f, '"'), n = 1))
    # (the same again: still one)
    session$setInputs(`ai-ref_commit` = list(value = f, n = 2))
    html <- output[["ai-ref_list"]]$html
    expect_identical(lengths(regmatches(html, gregexpr("ai-ref-remove", html, fixed = TRUE))), 1L)
    expect_match(html, "SAP.pdf", fixed = TRUE)
    # (a file that is not there: not listed)
    session$setInputs(`ai-ref_commit` = list(value = file.path(d, "none.pdf"), n = 3))
    html <- output[["ai-ref_list"]]$html
    expect_false(grepl("none.pdf", html, fixed = TRUE))
  })
})
