# A figure's or a listing's program starts with the report's analysis data
# it reads (tflspec's tfl_ard_code(part = "data")): a figure's read step
# names one (`data:`), a listing's row its `data` or an analysis set's
# subjects (`population_id`)

dp_planner <- function() {
  p <- add_output(new_planner(), "L1", type = "listing")
  p <- add_output(p, "L2", type = "listing")
  p <- add_output(p, "F1", type = "figure")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADAE", "ADSL"), level = "ADaM",
    path = c("data/adam/adae.rds", "data/adam/adsl.rds")), "datasets")
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""), "populations")
  p <- set_analysis_data(p, "L1", "adae_ser", from = "ADAE", where = "AESER == \"Y\"")
  p <- set_analysis_data(p, "F1", "adae_ser", from = "ADAE", where = "AESER == \"Y\"")
  p <- set_lf_rows(p, "listings", "L1", data.frame(data = "adae_ser", sort = "USUBJID"))
  p <- set_lf_rows(p, "listing_cols", "L1", data.frame(vars = "USUBJID"))
  p <- set_lf_rows(p, "listings", "L2", data.frame(dataset = "ADAE", population_id = "SAF"))
  p <- set_lf_rows(p, "listing_cols", "L2", data.frame(vars = "USUBJID"))
  p
}

test_that("a listing of an analysis data, or of an analysis set's subjects: its data part first", {
  p <- dp_planner()
  code <- unlist(strsplit(data_lines(p, "L1"), "\n", fixed = TRUE))
  expect_true(any(startsWith(code, "adae_ser <- ")))
  expect_true("data <- adae_ser |>" %in% code)
  expect_lt(grep("^adae_ser <- ", code)[1L], match("data <- adae_ser |>", code))
  expect_silent(parse(text = program_code(p, "L1")))
  code2 <- unlist(strsplit(data_lines(p, "L2"), "\n", fixed = TRUE))
  expect_true(any(startsWith(code2, "pop_saf <- ")))
  expect_true(any(grepl("semi_join(dplyr::select(pop_saf, USUBJID)", code2, fixed = TRUE)))
})

test_that("a figure whose read step names an analysis data: made before the plot", {
  p <- dp_planner()
  d <- list(data = list(list(step = "read", data = "adae_ser")),
            layers = list(list(layer = "point", x = "AESTDY", y = "AESTDY")))
  p <- set_fig_design(p, "F1", d)
  code <- unlist(strsplit(data_lines(p, "F1"), "\n", fixed = TRUE))
  made <- grep("^adae_ser <- ", code)
  expect_length(made, 1L)
  expect_lt(made, grep("adae_ser", code)[length(grep("adae_ser", code))])
  expect_true(any(grepl("readRDS(\"data/adam/adae.rds\")", code, fixed = TRUE)))
  expect_silent(parse(text = program_code(p, "F1")))
})
