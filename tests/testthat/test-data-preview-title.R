test_that("the Data tab's preview header names the dataset chosen", {
  expect_identical(.data_preview_title(), t("Preview"))
  expect_identical(.data_preview_title(NULL), t("Preview"))
  expect_identical(.data_preview_title(NA_character_), t("Preview"))
  expect_identical(.data_preview_title(""), t("Preview"))
  expect_identical(.data_preview_title("ADSL"), sprintf(t("Preview: %s"), "ADSL"))
  expect_match(.data_preview_title("ADSL"), "ADSL", fixed = TRUE)
})
