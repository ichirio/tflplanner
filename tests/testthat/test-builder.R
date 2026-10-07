dm_study_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$sheets$tables <- .normalize_sheet(data.frame(
    output_id = "DM", cols = "TRT01A", rows = "group = variable"), "tables")
  p$sheets$cells <- .normalize_sheet(data.frame(
    output_id = c(NA, "DM", "DM", "DM", "DM"),
    variable = c(NA, rep("continuous", 4)),
    row = c(NA, "n", "Mean (SD)", "Median", "Min, Max"),
    template = c("{n:.0f} ({p:.1f%})", "{N}", "{mean} ({sd})", "{median}",
                 "{min}, {max}"),
    digits = c(NA, "0", "1,2", "1", "0")), "cells")
  p
}

dm_ard <- function() {
  cards::ard_stack(
    cards::ADSL, .by = TRT01A,
    cards::ard_continuous(variables = AGE),
    cards::ard_categorical(variables = c(AGEGR1, SEX)),
    .total_n = TRUE)
}

test_that("reading and writing back unchanged changes nothing", {
  skip_if_not_installed("cards")
  p <- dm_study_planner()
  m <- ard_meta(dm_ard())
  st <- builder_read(p, "DM", m)
  expect_equal(st$key, "TRT01A")
  expect_equal(st$rows, c("n", "Mean (SD)", "Median", "Min, Max"))
  expect_equal(st$value, "stat")
  # the decimals the rows print now (their own digits)
  expect_equal(st$digits, c(N = 0L, mean = 1L, sd = 2L, median = 1L, min = 0L, max = 0L))
  expect_equal(st$cat_format, "npct")
  expect_equal(st$variables$variable, c("AGE", "AGEGR1", "SEX"))
  q <- builder_write(p, "DM", st)
  # the order it now states is the only thing written
  expect_equal(sheet_rows(q, "variables", "DM")$order, c("1", "2", "3"))
  expect_true(all(is.na(sheet_rows(q, "variables", "DM")$levels)))
  expect_identical(q$sheets$cells, p$sheets$cells)
  expect_identical(nrow(sheet_rows(q, "digits", "DM")), 0L)
  expect_identical(builder_write(q, "DM", builder_read(q, "DM", m))$sheets,
                   q$sheets)
})

test_that("the builder's choices land in the sheets", {
  skip_if_not_installed("cards")
  p <- dm_study_planner()
  m <- ard_meta(dm_ard())
  st <- builder_read(p, "DM", m)
  st$arms$TRT01A <- rev(st$arms$TRT01A)
  st$variables <- st$variables[c(3, 1, 2), ]
  st$variables$label[1] <- "Sex"
  st$levels$SEX <- c("M", "F")
  st$rows <- c("n", "Mean (SD)", "Q1, Q3", "Min, Max")
  st$templates[["Q1, Q3"]] <- "{p25}, {p75}"
  st$digits <- c(N = 0L, mean = 2L, sd = 3L, p25 = 2L, p75 = 2L, min = 1L, max = 1L)
  st$exceptions <- data.frame(variable = "AGE", statistic = "mean", digits = 3L)
  st$cat_format <- "nNpct"
  st$pct_decimals <- 0
  st$header <- "Arm / (N=n)"
  q <- builder_write(p, "DM", st)
  v <- sheet_rows(q, "variables", "DM")
  expect_equal(v$levels[v$variable == "TRT01A"], paste(rev(
    ard_meta(dm_ard())$keys$TRT01A), collapse = " | "))
  expect_equal(v$order[match(c("SEX", "AGE", "AGEGR1"), v$variable)],
               c("1", "2", "3"))
  expect_equal(v$label[v$variable == "SEX"], "Sex")
  expect_equal(v$levels[v$variable == "SEX"], "M | F")
  c <- sheet_rows(q, "cells", "DM")
  expect_equal(c$row[c$variable == "continuous"],
               c("n", "Mean (SD)", "Q1, Q3", "Min, Max"))
  # the rows say no digits of their own: the statistics' are on the digits sheet
  expect_true(all(is.na(c$digits[c$variable == "continuous"])))
  expect_equal(c$template[c$variable == "continuous"],
               c("{N}", "{mean} ({sd})", "{p25}, {p75}", "{min}, {max}"))
  dg <- sheet_rows(q, "digits", "DM")
  expect_equal(dg$digits[is.na(dg$variable) & dg$statistic == "mean"], "2")
  expect_equal(dg$digits[dg$variable %in% "AGE" & dg$statistic == "mean"], "3")
  expect_equal(c$template[c$variable %in% "categorical"],
               "{n:.0f}/{N:.0f} ({p:.0f%})")
  expect_equal(nrow(sheet_rows(q, "col_header", "DM")), 4)
  # and reads back as written
  back <- builder_read(q, "DM", m)
  expect_equal(back$rows, st$rows)
  expect_equal(back$digits[c("mean", "sd")], c(mean = 2L, sd = 3L))
  expect_equal(back$exceptions$digits, 3L)
  expect_equal(back$cat_format, "nNpct")
  expect_equal(back$variables$variable, c("SEX", "AGE", "AGEGR1"))
})

test_that("the preview is the table rtfreporter lays out", {
  skip_if_not_installed("cards")
  p <- dm_study_planner()
  d <- rtfreporter::normalize_ard(dm_ard())
  pages <- preview_pages(p, "DM", d)
  expect_s3_class(pages[[1]], "rtftable")
  expect_true(any(vapply(pages[[1]]$data, function(v) "Mean (SD)" %in% v,
                         NA)))
  h <- as.character(preview_html(pages))
  expect_match(h, "<table class=\"rp-pv\">")
  expect_match(h, "Mean (SD)", fixed = TRUE)
})

test_that("several column variables: written as tables$cols A | B, read back in order", {
  skip_if_not_installed("cards")
  p <- dm_study_planner()
  m <- ard_meta(dm_ard())
  st <- builder_read(p, "DM", m)
  expect_type(st$arms, "list")
  expect_identical(names(st$arms), "TRT01A")
  # SEX across the columns (not a row variable any more)
  st$variables <- st$variables[st$variables$variable != "SEX", ]
  st$key <- c("TRT01A", "SEX")
  st$arms <- list(TRT01A = rev(m$keys$TRT01A), SEX = c("M", "F"))
  q <- builder_write(p, "DM", st)
  expect_identical(sheet_rows(q, "tables", "DM")$cols, "TRT01A | SEX")
  v <- sheet_rows(q, "variables", "DM")
  expect_identical(v$levels[v$variable == "SEX"], "M | F")
  back <- builder_read(q, "DM", m)
  expect_identical(back$key, c("TRT01A", "SEX"))
  expect_identical(back$arms$SEX, c("M", "F"))
  expect_identical(back$arms$TRT01A, rev(m$keys$TRT01A))
})
