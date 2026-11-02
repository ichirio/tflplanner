# plan_cells() by variable: a variable's own template and its conditions,
# the decimals in them, the company standard copied into the study's (#454)

cells_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))
}

files_changed <- function(a, b) {
  k <- union(names(a), names(b))
  k[!vapply(k, function(x) identical(a[x], b[x]), NA)]
}

test_that("a condition reads as its three fields and is written back as !r", {
  c1 <- .cells_cond_parse(tflspec::tfl_r("estimate < 0.5 ~ \"<0.5\""))
  expect_true(c1$ok)
  expect_identical(c1[c("col", "op", "value", "tpl")],
                   list(col = "estimate", op = "<", value = "0.5", tpl = "<0.5"))
  c2 <- .cells_cond_parse(tflspec::tfl_r("TRT01A == \"Drug X\" ~ \"{n}\""))
  expect_identical(c2$value, "Drug X")
  expect_identical(.cells_cond_parse(tflspec::tfl_r("x >= -1 ~ \"a\""))$value, "-1")
  # what the fields cannot say: R, read only (its template still known)
  c3 <- .cells_cond_parse(tflspec::tfl_r("is.na(estimate) ~ \"NE\""))
  expect_false(c3$ok)
  expect_identical(c3$tpl, "NE")
  expect_false(.cells_cond_parse(tflspec::tfl_r("a + "))$ok)
  # numbers as numbers, text quoted
  r <- .cells_cond_r("estimate", "<", "0.5", "<0.5")
  expect_s3_class(r, "tfl_r")
  expect_identical(as.character(unclass(r)), "estimate < 0.5 ~ \"<0.5\"")
  expect_identical(as.character(unclass(.cells_cond_r("TRT01A", "!=", "Drug X", "{n}"))),
                   "TRT01A != \"Drug X\" ~ \"{n}\"")
  back <- .cells_cond_parse(.cells_cond_r("conf.high", ">=", "10", "{estimate:.1f}"))
  expect_identical(back[c("col", "op", "value", "tpl")],
                   list(col = "conf.high", op = ">=", value = "10", tpl = "{estimate:.1f}"))
  expect_error(.cells_cond_r("a", "%in%", "1", "x"), "operator")
})

test_that("a variable's entry reads and builds back unchanged (T-14-2-2's prob, time)", {
  skip_on_cran()
  s <- cells_sample()
  pc <- .builder_args(builder_items(s$planner, "T-14-2-2"), "plan_cells")
  e <- .cells_entry_parse(pc$prob)
  expect_identical(e$type, "single")
  expect_length(e$conds, 2L)
  expect_false(e$conds[[1L]]$ok)
  expect_identical(e$fallback, "{estimate:.0f} ({conf.low:.0f}, {conf.high:.0f})")
  expect_identical(.cells_entry_build(e), pc$prob)
  expect_identical(.cells_entry_build(.cells_entry_parse(pc$time)), pc$time)
  rows <- list(n = "{N}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})")
  expect_identical(.cells_entry_parse(rows)$type, "rows")
  expect_identical(.cells_entry_build(.cells_entry_parse(rows)), rows)
  expect_identical(.cells_entry_parse(list(n = list(1)))$type, "r")
  # a condition not said yet (no value): not written
  e <- .cells_entry_parse(pc$time)
  e$conds <- list(list(col = "estimate", op = "==", value = "", tpl = "x", raw = NULL, ok = TRUE))
  expect_identical(.cells_entry_build(e), pc$time)
  e$conds[[1L]]$value <- "1"
  expect_identical(as.character(unclass(.cells_entry_build(e)[[1L]])), "estimate == 1 ~ \"x\"")
  # a continuous variable's rows with a condition: cell_rows(), read back
  r <- .cells_entry_parse(rows)
  u <- .cells_entry_part(r, "r:Mean (SD)")
  u$conds <- list(list(col = "N", op = "<", value = "3", tpl = "NE", raw = NULL, ok = TRUE))
  r <- .cells_entry_part_set(r, "r:Mean (SD)", u)
  b <- .cells_entry_build(r)
  expect_s3_class(b, "tfl_call")
  expect_identical(b$fn, "cell_rows")
  expect_identical(b$args$n, "{N}")
  expect_length(b$args[["Mean (SD)"]], 2L)
  r2 <- .cells_entry_parse(b)
  expect_identical(r2$type, "rows")
  expect_identical(.cells_entry_build(r2), b)
  expect_identical(r2$rows[["Mean (SD)"]]$conds[[1L]]$value, "3")
  expect_identical(.cells_var_names(pc), c("prob", "time"))
  expect_identical(.cells_vars_summary(pc), "Per variable: prob (3), time")
  expect_identical(.cells_vars_summary(list(default = "{n}")), "Per variable: none")
  # the table's rows: the ARD's variables, then those only the plan names
  meta <- list(variables = data.frame(variable = c("prob", "AVAL"),
                                      kind = c("continuous", "categorical"),
                                      stats = c("estimate | conf.low | conf.high", "n | N | p"),
                                      stringsAsFactors = FALSE))
  r <- .cells_vars_rows(pc, meta)
  expect_identical(r$variable, c("prob", "AVAL", "time"))
  expect_identical(r$in_ard, c(TRUE, TRUE, FALSE))
  expect_identical(r$own, c(TRUE, FALSE, TRUE))
  expect_identical(r$kind, c("continuous", "categorical", "continuous"))
  expect_identical(.cells_chip_text("mean", c(mean = 1L)), "{mean:.1f}")
  expect_identical(.cells_chip_text("p", pct = 1), "{p:.1f%}")
  expect_identical(.cells_chip_text("estimate"), "{estimate}")
})

test_that("a variable's own entry is written into plan_cells() only; the same writes nothing", {
  skip_on_cran()
  s <- cells_sample()
  p <- s$planner
  id <- "T-14-2-2"
  pc <- .builder_args(builder_items(p, id), "plan_cells")
  expect_identical(.cells_var_put(p, id, "time", pc$time), p)
  e <- .cells_entry_parse(pc$prob)
  e$conds <- c(e$conds, list(list(col = "estimate", op = "<", value = "1", tpl = "<1")))
  q <- .cells_var_put(p, id, "prob", .cells_entry_build(e))
  expect_identical(files_changed(p$spec$text, q$spec$text), "spec/outputs/T-14-2-2.yml")
  got <- .builder_args(builder_items(q, id), "plan_cells")$prob
  expect_length(got, 4L)
  expect_identical(as.character(unclass(got[[3L]])), "estimate < 1 ~ \"<1\"")
  # read back: the new condition in its fields
  e2 <- .cells_entry_parse(got)
  expect_true(e2$conds[[3L]]$ok)
  # taken away: the common template again
  q2 <- .cells_var_put(q, id, "time", NULL)
  expect_null(.builder_args(builder_items(q2, id), "plan_cells")$time)
})

test_that("changing the decimals rewrites a variable's own rows and conditions, dropping none", {
  skip_on_cran()
  # (the decimals once took a variable's conditions away)
  x <- list(tflspec::tfl_r("is.na(sd) ~ \"{mean:.1f} (NE)\""), "{mean:.1f} ({sd:.2f})")
  y <- .cells_redigit(x, c(mean = 2L))
  expect_length(y, 2L)
  expect_s3_class(y[[1L]], "tfl_r")
  expect_identical(as.character(unclass(y[[1L]])), "is.na(sd) ~ \"{mean:.2f} (NE)\"")
  expect_identical(y[[2L]], "{mean:.2f} ({sd:.2f})")
  expect_identical(.cells_redigit(x, integer()), x)
  expect_identical(.cells_digits_changed(c(mean = 2L, sd = 2L), c(mean = 1L, sd = 2L)),
                   c(mean = 2L))

  s <- cells_sample()
  p <- s$planner
  id <- "T-14-1-1"
  cells <- function(q) .builder_args(builder_items(q, id), "plan_cells")
  own <- list(tflspec::tfl_r("is.na(sd) ~ \"{mean:.1f}\""), "{mean:.1f} ({sd:.2f})")
  p <- .cells_var_put(p, id, "AGE", own)
  # a continuous row of conditions too
  pc <- cells(p)
  pc$continuous[["Mean (SD)"]] <- list(tflspec::tfl_r("N < 3 ~ \"{mean:.1f}\""),
                                       pc$continuous[["Mean (SD)"]])
  p <- .builder_item_write(p, id, "plan_cells", pc)
  st <- builder_read(p, id)
  st1 <- st
  st1$digits[["mean"]] <- 2L
  q <- builder_write(p, id, st1, was = st)
  got <- cells(q)
  expect_length(got$AGE, 2L)
  expect_identical(as.character(unclass(got$AGE[[1L]])), "is.na(sd) ~ \"{mean:.2f}\"")
  expect_identical(got$AGE[[2L]], "{mean:.2f} ({sd:.2f})")
  ms <- got$continuous[["Mean (SD)"]]
  expect_length(ms, 2L)
  expect_identical(as.character(unclass(ms[[1L]])), "N < 3 ~ \"{mean:.2f}\"")
  expect_identical(ms[[2L]], "{mean:.2f} ({sd:.2f})")
  # a continuous variable's own rows with a condition: cell_rows() in the code
  e <- .cells_entry_parse(.cells_common_entry(cells(p), st, "continuous"))
  u <- .cells_entry_part(e, "r:n")
  u$conds <- list(list(col = "N", op = "<", value = "3", tpl = "few", raw = NULL, ok = TRUE))
  q2 <- .cells_var_put(p, id, "AGE", .cells_entry_build(.cells_entry_part_set(e, "r:n", u)))
  code <- paste(program_code(q2, id), collapse = " ")
  expect_match(code, "cell_rows(", fixed = TRUE)
  expect_match(code, "N < 3 ~ \"few\"", fixed = TRUE)
  # with no ARD to edit against: read only
  h <- as.character(.cells_vars_ro(cells(q2)))
  expect_match(h, "AGE", fixed = TRUE)
  # the sd unchanged: its decimals stay
  expect_identical(got$continuous[["n"]], cells(p)$continuous[["n"]])
})

test_that("the company standard is copied into the study's, the chosen items only", {
  skip_on_cran()
  s <- cells_sample()
  p <- s$planner
  co <- list(default = "{n:.0f} ({p:.1f%})",
             continuous = list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})"))
  d <- .cells_company_diff(p, co)
  # the sample's study standard has the same default, no continuous rows:
  # the company's rows are one item
  expect_identical(d$key, "continuous")
  expect_true(is.na(d$study))
  expect_false(d$differs)
  q <- .spec_set_study_cells(p, .cells_company_apply(.cells_study(p), "continuous", co))
  expect_identical(files_changed(p$spec$text, q$spec$text), "spec/defaults.yml")
  expect_identical(.cells_study(q)$continuous, co$continuous)
  expect_identical(nrow(.cells_company_diff(q, co)), 0L)
  # rows of its own: a row an item, the ticked one only copied
  sc <- .cells_study(p)
  sc$continuous <- list(n = "{N}")
  p1 <- .spec_set_study_cells(p, sc)
  d <- .cells_company_diff(p1, co)
  expect_identical(d$key, c("continuous:n", "continuous:Mean (SD)"))
  expect_identical(d$differs, c(TRUE, FALSE))
  q <- .spec_set_study_cells(p1, .cells_company_apply(.cells_study(p1), "continuous:Mean (SD)", co))
  got <- .cells_study(q)
  expect_identical(got$default, "{n:.0f} ({p:.1f%})")
  expect_identical(got$continuous, list(n = "{N}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})"))
  expect_identical(.cells_company_diff(q, co)$key, "continuous:n")
  # a value of its own: differs
  sc2 <- .cells_study(q)
  sc2$default <- "{n}"
  q2 <- .spec_set_study_cells(q, sc2)
  d2 <- .cells_company_diff(q2, co)
  expect_true(d2$differs[d2$key == "default"])
  expect_identical(.cells_company_apply(.cells_study(q2), "default", co)$default, co$default)
  # the company standards' own
  cc <- .cells_company()
  expect_type(cc$continuous, "list")
  expect_true(length(cc$continuous) > 0L)
})
