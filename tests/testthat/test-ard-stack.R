stack_planner <- function() {
  x <- new_planner()
  a <- data.frame(
    analysis_id = c("BIGN", "CONT", "CAT", "PVAL", "AE"),
    label = NA_character_,
    method = c("categorical", "continuous", "categorical", "ttest", "categorical"),
    dataset = c("ADSL", "ADSL", "ADSL", "ADSL", "ADSL"),
    population_id = "SAF",
    where = c(NA, NA, NA, NA, "SEX == \"F\""),
    by = c(NA, "TRT01A", "TRT01A", "TRT01A", "TRT01A"),
    variables = c("TRT01A", "AGE | BMIBL", "SEX | AGEGR1", "AGE", "RACE"),
    stringsAsFactors = FALSE)
  x <- set_ard_rows(x, "datasets", "", data.frame(dataset = "ADSL", level = "adam",
                                                   path = "data/adam/adsl.rds"))
  x <- set_ard_rows(x, "populations", "", data.frame(population_id = "SAF", dataset = "ADSL",
                                                      where = "SAFFL == \"Y\""))
  set_ard_rows(x, "analyses", "T1", a)
}

stack_remove_bign <- function(x) {
  a <- ard_rows(x, "analyses", "T1")
  a$output_id <- NULL
  set_ard_rows(x, "analyses", "T1", a[a$analysis_id != "BIGN", ])
}

test_that("the outline puts the analyses inside a stack under it", {
  a <- data.frame(analysis_id = c("CAT", "S", "CONT", "X"),
                  parent = c("S", NA, "S", NA), stringsAsFactors = FALSE)
  o <- .an_outline(a)
  expect_identical(o$analysis_id, c("S", "CAT", "CONT", "X"))
  expect_identical(o$depth, c(0L, 1L, 1L, 0L))
  # a parent the report has not got: at the top
  a$parent[4] <- "NOPE"
  expect_identical(.an_outline(a)$depth[4], 0L)
})

test_that("analyses are grouped into a stack, and the ones that cannot say why", {
  x <- stack_planner()
  cand <- stack_candidates(x, "T1", "CONT")
  r <- stats::setNames(cand$reason, cand$analysis_id)
  expect_true(is.na(r[["CONT"]]))
  expect_true(is.na(r[["CAT"]]))
  # BIGN counts the subjects per group: the stack can do it itself
  expect_identical(r[["BIGN"]], "group_n")
  expect_identical(r[["PVAL"]], "variable")
  expect_identical(r[["AE"]], "where")
  y <- stack_group(x, "T1", c("CONT", "CAT"), label = "Demographics")
  a <- ard_rows(y, "analyses", "T1")
  expect_identical(a$analysis_id, c("BIGN", "STACK", "CONT", "CAT", "PVAL", "AE"))
  s <- a[a$analysis_id == "STACK", ]
  expect_identical(s$method, "cards::ard_stack")
  expect_identical(c(s$dataset, s$population_id, s$by), c("ADSL", "SAF", "TRT01A"))
  # BIGN counts the subjects per group already: the stack does not again
  expect_identical(s$args, ".by_stats = FALSE")
  inside <- a[a$analysis_id %in% c("CONT", "CAT"), ]
  expect_identical(inside$parent, c("STACK", "STACK"))
  expect_true(all(is.na(inside$dataset)) && all(is.na(inside$by)))
  # tflspec takes it, and writes one call
  spec <- structure(y$ard, class = "tfl_ard_spec")
  code <- paste(tflspec::tfl_ard_code(spec, part = "body"), collapse = "\n")
  expect_match(code, "cards::ard_stack(", fixed = TRUE)
  expect_error(stack_group(x, "T1", c("CONT", "AE")), "cannot run with")
})

test_that("ungrouping gives the data back and keeps the column headers' N", {
  x <- stack_planner()
  y <- stack_group(x, "T1", c("CONT", "CAT"))
  a <- ard_rows(y, "analyses", "T1")
  a$args[a$analysis_id == "STACK"] <- ".total_n = TRUE"
  a$output_id <- NULL
  y <- set_ard_rows(y, "analyses", "T1", a)
  z <- stack_ungroup(y, "T1", "STACK")
  b <- ard_rows(z, "analyses", "T1")
  expect_false("STACK" %in% b$analysis_id)
  expect_identical(b$by[b$analysis_id == "CONT"], "TRT01A")
  expect_identical(b$dataset[b$analysis_id == "CAT"], "ADSL")
  # BIGN was there already: not made again; the total N is TOTAL
  expect_false("BIGN2" %in% b$analysis_id)
  expect_true("TOTAL" %in% b$analysis_id)
  expect_identical(b$method[b$analysis_id == "TOTAL"], "cards::ard_total_n")
  # a report with no BIGN gets one
  w <- stack_ungroup(stack_group(stack_remove_bign(stack_planner()), "T1", c("CONT", "CAT")),
                     "T1", "STACK")
  wb <- ard_rows(w, "analyses", "T1")
  expect_identical(wb$variables[wb$analysis_id == "BIGN"], "TRT01A")
  # without them
  z2 <- stack_ungroup(y, "T1", "STACK", keep_n = FALSE)
  expect_false(any(c("BIGN2", "TOTAL") %in% ard_rows(z2, "analyses", "T1")$analysis_id))
})

test_that("one analysis taken out; the last one out takes the stack away", {
  x <- stack_group(stack_planner(), "T1", c("CONT", "CAT"))
  y <- stack_take_out(x, "T1", "CAT")
  a <- ard_rows(y, "analyses", "T1")
  expect_true(is.na(a$parent[a$analysis_id == "CAT"]))
  expect_identical(a$by[a$analysis_id == "CAT"], "TRT01A")
  expect_true("STACK" %in% a$analysis_id)
  z <- stack_take_out(y, "T1", "CONT", keep_n = FALSE)
  expect_false("STACK" %in% ard_rows(z, "analyses", "T1")$analysis_id)
})

test_that("inside a stack: an analysis added, moved; a parent renamed or removed", {
  x <- stack_group(stack_planner(), "T1", c("CONT", "CAT"))
  y <- stack_add_inside(x, "T1", "STACK", id = "MISS", method = "missing",
                        variables = "HEIGHTBL")
  a <- ard_rows(y, "analyses", "T1")
  expect_identical(.an_outline(a)$analysis_id[2:5], c("STACK", "CONT", "CAT", "MISS"))
  y <- stack_move(y, "T1", "MISS", -1L)
  expect_identical(.an_outline(ard_rows(y, "analyses", "T1"))$analysis_id[3:5],
                   c("CONT", "MISS", "CAT"))
  # the first one cannot go up
  expect_identical(stack_move(y, "T1", "CONT", -1L), y)
  r <- stack_rename(y, "T1", "STACK", "DEMO")
  expect_identical(unique(stats::na.omit(ard_rows(r, "analyses", "T1")$parent)), "DEMO")
  gone <- stack_remove(y, "T1", "STACK", how = "all")
  expect_false(any(c("STACK", "CONT", "CAT", "MISS") %in% ard_rows(gone, "analyses", "T1")$analysis_id))
})

test_that("ard_stack()'s switches: read from args, written only when not cards' default", {
  f <- .stack_flags_of(".total_n = TRUE, .shuffle = TRUE, .missing = FALSE, foo = 1")
  expect_true(f$flags[[".total_n"]])
  expect_true(f$flags[[".by_stats"]])
  expect_identical(f$other, "foo = 1")
  expect_identical(.stack_args(f$flags, f$other), ".total_n = TRUE, foo = 1")
  expect_true(is.na(.stack_args(c(.by_stats = TRUE))))
  expect_identical(.stack_args(c(.by_stats = FALSE)), ".by_stats = FALSE")
})

test_that("the subjects per group counted twice are found; ungrouping does not count them again", {
  x <- stack_group(stack_planner(), "T1", c("CONT", "CAT"))
  a <- ard_rows(x, "analyses", "T1")
  expect_identical(nrow(stack_n_twice(a)), 0L)
  # the stack made to count them too (BIGN does already)
  a$args[a$analysis_id == "STACK"] <- NA
  tw <- stack_n_twice(a)
  expect_setequal(tw$analysis_id, c("BIGN", "STACK"))
  expect_identical(tw$with[tw$analysis_id == "STACK"], "BIGN")
  a$output_id <- NULL
  y <- set_ard_rows(x, "analyses", "T1", a)
  # ungrouping: BIGN is there, no second one
  z <- stack_ungroup(y, "T1", "STACK")
  expect_false("BIGN2" %in% ard_rows(z, "analyses", "T1")$analysis_id)
  expect_identical(nrow(stack_n_twice(ard_rows(z, "analyses", "T1"))), 0L)
})
