lf_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

lf_planner <- function() {
  p <- add_output(new_planner(), "L1", type = "listing")
  p <- add_output(p, "F1", type = "figure")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADAE", "ADSL", "AE"), level = c("ADaM", "ADaM", "SDTM"),
    path = c("data/adam/adae.rds", "data/adam/adsl.rds", "data/sdtm/ae.rds")),
    "datasets")
  p <- set_lf_rows(p, "listings", "L1", data.frame(
    type = "multiline", dataset = "ADAE", where = "AESEV == \"SEVERE\"",
    sort = "TRTA | -ASTDT", max_rows = "20"))
  p <- set_lf_rows(p, "listing_cols", "L1", data.frame(
    vars = c("TRTA", "USUBJID", "AEBODSYS | AEDECOD"),
    label = c("Treatment", "Subject", "SOC/\\nPT"),
    width = c("20", "14", "40"), collapse_repeats = c(NA, "TRUE", NA)))
  p <- set_lf_rows(p, "figures", "F1", data.frame(datasets = "ADSL"))
  p
}

test_that("a listing in rows becomes its program", {
  p <- lf_planner()
  code <- data_lines(p, "L1")
  expect_true("adae <- readRDS(\"data/adam/adae.rds\")" %in% code)
  expect_true("data <- subset(adae, AESEV == \"SEVERE\")" %in% code)
  expect_true(any(grepl("order(data$TRTA, -xtfrm(data$ASTDT))", code,
                        fixed = TRUE)))
  expect_true(any(grepl("collapse_repeats = TRUE", code, fixed = TRUE)))
  expect_true(any(grepl("\"SOC/\\nPT\"", code, fixed = TRUE)))
  expect_true(any(grepl("max_rows = 20", code, fixed = TRUE)))
  expect_silent(parse(text = program_code(p, "L1")))
  # its data code reworks `data` after the condition
  p$outputs$data_code[p$outputs$output_id == "L1"] <- "data$X <- 1"
  code <- data_lines(p, "L1")
  expect_lt(match("data$X <- 1", code),
            grep("order(", code, fixed = TRUE))
})

test_that("a figure reads its data and leaves the plot to its code", {
  p <- lf_planner()
  code <- data_lines(p, "F1")
  expect_true("adsl <- readRDS(\"data/adam/adsl.rds\")" %in% code)
  expect_true(any(grepl("still to be written", code)))
  # the figure style of the company standards, and the checks
  expect_true('source("programs/tfl/fig_setup.R")' %in% code)
  p$outputs$data_code[p$outputs$output_id == "F1"] <- "plot <- 1"
  code <- data_lines(p, "F1")
  expect_true("content <- list(plot)" %in% code)
  expect_true("tfl_check(plot)" %in% code)
  p$outputs$data_code[p$outputs$output_id == "F1"] <- "content <- list(1)"
  expect_false("content <- list(plot)" %in% data_lines(p, "F1"))
})

test_that("listing and figure rows follow the report and are saved", {
  skip_if_not_installed("cards")
  lf_home()
  p <- lf_planner()
  q <- copy_output(p, "L1", "L2")
  expect_equal(nrow(lf_rows(q, "listing_cols", "L2")), 3)
  q <- rename_output(q, "L2", "L3")
  expect_equal(nrow(lf_rows(q, "listings", "L3")), 1)
  expect_identical(remove_output(q, "L3")$lf, p$lf)

  s <- create_study("LF1", planner = p)
  expect_true(file.exists(file.path(s$path, "spec",
                                    "listing_figure_spec.xlsx")))
  expect_identical(open_study("LF1")$planner$lf, s$planner$lf)
  unregister_study("LF1")
  expect_identical(register_study(s$path)$planner$lf, s$planner$lf)

  saveRDS(cards::ADAE, file.path(s$path, "data", "adam", "adae.rds"))
  pages <- preview_listing(open_study("LF1"), "L1")
  expect_s3_class(pages[[1]], "rtftable")
  expect_match(as.character(preview_html(pages, align = "left")),
               "rp-pv-left")
})
