test_that("a double click on a filtered list's row is that row's, not the data's at that place", {
  # the k-th row shown is the k-th of the rows the table shows now
  expect_identical(.dt_row_shown(3, c(14L, 15L, 16L, 17L)), 16L)
  # no rows shown yet: the k-th of the data
  expect_identical(.dt_row_shown(3, NULL), 3L)
  expect_identical(.dt_row_shown(NULL, 1:3), NA_integer_)
})
