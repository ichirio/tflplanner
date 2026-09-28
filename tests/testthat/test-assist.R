dm_ard <- function() {
  adsl <- cards::ADSL
  cards::ard_stack(
    adsl, .by = TRT01A,
    cards::ard_continuous(variables = AGE),
    cards::ard_categorical(variables = c(AGEGR1, SEX)),
    .total_n = TRUE)
}

ae_data <- function() {
  adsl <- cards::ADSL
  adsl$TRTA <- adsl$TRT01A
  adae <- cards::ADAE[cards::ADAE$TRTEMFL == "Y", ]
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AEBODSYS, AEDECOD), by = TRTA,
    denominator = adsl, id = USUBJID, over_variables = TRUE)
  tflspec::tfl_ard_normalize(ard, hierarchy = c("AEBODSYS", "AEDECOD"),
                             overall = "Any TEAE")
}

test_that("ard_meta() reads keys, variables, levels and statistics", {
  skip_if_not_installed("cards")
  m <- ard_meta(dm_ard())
  expect_equal(names(m$keys), "TRT01A")
  expect_setequal(m$keys$TRT01A,
                  c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  expect_equal(m$by, "TRT01A")
  expect_equal(m$variables$variable, c("AGE", "AGEGR1", "SEX"))
  expect_equal(m$variables$kind, c("continuous", "categorical",
                                   "categorical"))
  expect_equal(m$variables$levels[3], "F | M")
  expect_true(all(c("N", "mean", "sd", "n", "p") %in% m$stats))

  # the raw ARD alone gives the same keys and variables
  r <- ard_meta(dm_ard(), data = NULL)
  expect_equal(r$variables$variable, m$variables$variable)
})

test_that("ard_meta() reads the rework, and a hierarchy", {
  skip_if_not_installed("cards")
  d <- tflspec::tfl_ard_normalize(dm_ard())
  d$variable_level[d$variable == "SEX"] <-
    ifelse(d$variable_level[d$variable == "SEX"] == "F", "Female", "Male")
  m <- ard_meta(data = d)
  expect_equal(m$variables$levels[m$variables$variable == "SEX"],
               "Female | Male")

  a <- ard_meta(data = ae_data())
  expect_equal(a$by, "TRTA")
  expect_equal(a$hierarchy, c("AEBODSYS", "AEDECOD"))
  expect_false("Any TEAE" %in% a$keys$AEBODSYS)
  expect_equal(nrow(a$variables), 0)
})

test_that("fill_variables() and fill_tables() fill from the metadata", {
  skip_if_not_installed("cards")
  m <- ard_meta(dm_ard())
  p <- add_output(new_planner(), "DM")
  p <- fill_variables(p, "DM", m)
  expect_equal(attr(p, "changed"), 4L)
  v <- sheet_rows(p, "variables", "DM")
  expect_equal(v$variable, c("TRT01A", "AGE", "AGEGR1", "SEX"))
  expect_equal(v$order, c(NA, "1", "2", "3"))
  expect_equal(v$levels[4], "F | M")

  # rows already there keep what they say; only blanks are filled
  p$sheets$variables$levels[4] <- "M | F"
  p$sheets$variables$label[2] <- NA
  p <- fill_variables(p, "DM", m)
  expect_equal(attr(p, "changed"), 0L)
  expect_equal(sheet_rows(p, "variables", "DM")$levels[4], "M | F")

  p <- fill_tables(p, "DM", m)
  t <- sheet_rows(p, "tables", "DM")
  expect_equal(c(t$cols, t$rows), c("TRT01A", "group = variable"))

  a <- ard_meta(data = ae_data())
  q <- fill_tables(add_output(new_planner(), "AE"), "AE", a)
  t <- sheet_rows(q, "tables", "AE")
  expect_equal(c(t$cols, t$rows, t$label),
               c("TRTA", "group1 = AEBODSYS", "label = AEDECOD"))
  q <- fill_variables(q, "AE", a)
  expect_equal(sheet_rows(q, "variables", "AE")$variable, "TRTA")
})

test_that("presets add cells and replace the column header", {
  p <- add_output(new_planner(), "DM")
  p <- add_preset(p, "DM", "Continuous: n / Mean (SD) / Median / Min, Max")
  p <- add_preset(p, "DM", "Categorical: n (%)", variable = "SEX")
  c <- sheet_rows(p, "cells", "DM")
  expect_equal(c$row[1:4], c("n", "Mean (SD)", "Median", "Min, Max"))
  expect_equal(c$variable[5], "SEX")
  # the same preset twice does not duplicate its rows
  p <- add_preset(p, "DM", "Continuous: n / Mean (SD) / Median / Min, Max")
  expect_equal(nrow(sheet_rows(p, "cells", "DM")), 5)

  p <- add_preset(p, "DM", "Arm / (N=n)")
  p <- add_preset(p, "DM", "Arm (N=n) n (%)")
  h <- sheet_rows(p, "col_header", "DM")
  expect_equal(nrow(h), 2)
  expect_error(add_preset(p, "DM", "nope"), "No preset")

  expect_equal(.missing_stats("{median} ({p25}, {p75})", c("median")),
               c("p25", "p75"))
})

test_that("a report inherits the defaults it has no row for", {
  p <- add_output(new_planner(), "DM")
  p$sheets$header <- .normalize_sheet(data.frame(
    output_id = c(NA, NA, "DM"), line = c("1", "2", "2"),
    left = c("Sponsor", "Protocol", "DM protocol")), "header")
  i <- inherited_rows(p, "header", "DM")
  expect_equal(i$left, "Sponsor")
  p$sheets$col_header <- .normalize_sheet(data.frame(
    output_id = c(NA, "DM"), line = "1", cols = ".values",
    text = c("{col}", "x")), "col_header")
  expect_equal(nrow(inherited_rows(p, "col_header", "DM")), 0)
})

test_that("fetch_ard() runs the data part from the study folder", {
  skip_if_not_installed("cards")
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  p <- add_output(new_planner(), "DM", data_code = paste(
    "adsl <- readRDS(\"data/adam/adsl.rds\")",
    "ard <- cards::ard_stack(adsl, .by = TRT01A,",
    "  cards::ard_categorical(variables = SEX))", sep = "\n"),
    process_code = paste(
      "data <- ard_normalize(ard)",
      "data$variable_level[data$variable == \"SEX\"] <- \"Any\"",
      sep = "\n"))
  s <- create_study("S1", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  m <- fetch_ard(s, "DM")
  expect_equal(m$source, "data")
  expect_equal(m$variables$levels, "Any")          # after the rework
  expect_identical(ard_info(open_study("S1"), "DM")$variables,
                   m$variables)

  # the source data order the levels and label the variables
  adsl <- cards::ADSL
  adsl$SEX <- factor(adsl$SEX, levels = c("M", "F"))
  attr(adsl$SEX, "label") <- "Sex at birth"
  saveRDS(adsl, file.path(s$path, "data", "adam", "adsl.rds"))
  s$planner$outputs$process_code <- NA
  s$planner$outputs$data_code <- paste(
    "adsl <- readRDS(\"data/adam/adsl.rds\")",
    "ard <- cards::ard_stack(adsl, .by = TRT01A,",
    "  cards::ard_categorical(variables = SEX))", sep = "
")
  m <- fetch_ard(s, "DM")
  expect_equal(m$keys$TRT01A, c("Placebo", "Xanomeline Low Dose",
                                "Xanomeline High Dose"))   # by TRT01AN
  expect_equal(m$variables$levels, "M | F")                # the factor
  expect_equal(m$variables$label, "Sex at birth")

  s$planner$outputs$process_code <- "stop('rework failed')"
  m <- fetch_ard(s, "DM")
  expect_equal(m$source, "ard")
  expect_match(m$error, "rework failed")
  expect_equal(m$variables$variable, "SEX")

  s$planner$outputs$data_code <- "stop('no data')"
  expect_error(fetch_ard(s, "DM"), "no data")
})
