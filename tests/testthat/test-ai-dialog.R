# The computer's dialogs on Windows in front of the browser (R/ai_dialog_win.R)

test_that("the dialog's script: its owner top-most, the folder it opens in, the paths written", {
  s <- paste(.ps_dialog_script("dir", tempdir(), "ADaM folder"), collapse = "\n")
  expect_match(s, "$owner.TopMost = $true", fixed = TRUE)
  expect_match(s, "FolderBrowserDialog", fixed = TRUE)
  expect_match(s, "$d.ShowDialog($owner)", fixed = TRUE)
  expect_match(s, "$d.SelectedPath = '", fixed = TRUE)
  f <- paste(.ps_dialog_script("file", "", "Shell workbooks", multi = TRUE), collapse = "\n")
  expect_match(f, "OpenFileDialog", fixed = TRUE)
  expect_match(f, "$d.Multiselect = $true", fixed = TRUE)
  expect_false(grepl("InitialDirectory", f, fixed = TRUE))
  # (a quote in a path doubled, as PowerShell reads it)
  expect_identical(.ps_lit("C:/it's"), "'C:/it''s'")
})

test_that("a dialog that cannot open says to paste the path", {
  expect_error(.ai_dialog_try(stop("no GUI")), "paste the path", fixed = TRUE)
  expect_identical(.ai_dialog_try("C:/x"), "C:/x")
})
