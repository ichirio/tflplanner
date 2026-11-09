branch_planner <- function() {
  x <- new_planner()
  a <- data.frame(
    analysis_id = c("CONT", "CAT", "TOT", "SUBJ", "OTHERDS", "BYX", "STRX", "DENX", "NEWP"),
    label = NA_character_,
    method = c("continuous", "categorical", "total_n", "custom", "continuous",
               "continuous", "continuous", "continuous", "ttest"),
    dataset = c(rep("ADSL", 4), "ADAE", rep("ADSL", 4)),
    population_id = "SAF",
    where = NA_character_,
    by = c("TRT01A", "TRT01A", NA, "TRT01A", "TRT01A", "SEX", "TRT01A", "TRT01A", "TRT01A"),
    strata = c(rep(NA, 6), "AGEGR1", NA, NA),
    denominator = c(rep(NA, 7), "col", NA),
    variables = c("AGE", "SEX", NA, "X", "AGE2", "AGE3", "AGE4", "AGE5", "AGE6"),
    stringsAsFactors = FALSE)
  x <- set_ard_rows(x, "datasets", "", data.frame(dataset = c("ADSL", "ADAE"), level = "adam",
                                                   path = c("a.rds", "b.rds")))
  x <- set_ard_rows(x, "populations", "", data.frame(population_id = "SAF", dataset = "ADSL",
                                                      where = "SAFFL == \"Y\""))
  set_ard_rows(x, "analyses", "T1", a)
}

ids_of <- function(x) ard_rows(x, "analyses", "T1")$analysis_id

test_that(".an_outline of no analyses is an empty table with the three columns", {
  for (a in list(NULL, data.frame(analysis_id = character()))) {
    o <- .an_outline(a)
    expect_identical(nrow(o), 0L)
    expect_identical(names(o), c("analysis_id", "depth", "parent"))
  }
})

test_that(".stack_free_id counts up from the base to the first free id", {
  a <- data.frame(analysis_id = c("STACK", "STACK2"))
  expect_identical(.stack_free_id(a), "STACK3")
  expect_identical(.stack_free_id(a, "AN"), "AN")
})

test_that(".stack_flags_of keeps the text of args R cannot parse as the other args, the cards defaults as flags", {
  r <- .stack_flags_of("a = (")
  expect_identical(r$other, "a = (")
  expect_identical(r$flags, .stack_flags)
})

test_that(".stack_reason names why an analysis cannot go in a stack", {
  a <- ard_rows(branch_planner(), "analyses", "T1")
  a$parent <- NA_character_
  a$parent[a$analysis_id == "NEWP"] <- "SOMEWHERE"
  r <- function(id) .stack_reason(a, match(id, a$analysis_id), 1L)
  expect_identical(r("NEWP"), "inside")
  expect_identical(r("TOT"), "total_n")
  expect_identical(r("SUBJ"), "method")
  expect_identical(r("OTHERDS"), "data")
  expect_identical(r("BYX"), "by")
  expect_identical(r("STRX"), "strata")
  expect_identical(r("DENX"), "denominator")
  expect_true(is.na(r("CAT")))
  # a different where
  a$where[a$analysis_id == "CAT"] <- "AGE > 1"
  expect_identical(r("CAT"), "where")
})

test_that(".stack_group_n is empty for an analysis with no by variable", {
  a <- ard_rows(branch_planner(), "analyses", "T1")
  a$parent <- NA_character_
  expect_identical(.stack_group_n(a, a[3L, ]), integer())
})

test_that("stack_candidates refuses an analysis the report has not got", {
  expect_error(stack_candidates(branch_planner(), "T1", "NOPE"),
               "'NOPE' is not an analysis of T1.", fixed = TRUE)
})

test_that("stack_group refuses ids the report has not got, and two analyses of one variable", {
  x <- branch_planner()
  expect_error(stack_group(x, "T1", c("CONT", "NOPE")), "Choose analyses of T1.", fixed = TRUE)
  expect_error(stack_group(x, "T1", character()), "Choose analyses of T1.", fixed = TRUE)
  expect_error(stack_group(x, "T1", c("CONT", "BYX")), "'BYX' cannot run with 'CONT' (by).",
               fixed = TRUE)
  a <- ard_rows(x, "analyses", "T1")
  a$output_id <- NULL
  a$variables[a$analysis_id == "CAT"] <- "AGE"
  x <- set_ard_rows(x, "analyses", "T1", a)
  expect_error(stack_group(x, "T1", c("CONT", "CAT")), "A variable is in two of them: AGE.",
               fixed = TRUE)
})

test_that("stack_group takes a given parent id for the stack", {
  x <- branch_planner()
  y <- stack_group(x, "T1", c("CONT", "CAT"), parent = "MYSTACK")
  a <- ard_rows(y, "analyses", "T1")
  expect_identical(a$analysis_id[1:3], c("MYSTACK", "CONT", "CAT"))
  expect_identical(a$parent[a$analysis_id %in% c("CONT", "CAT")], c("MYSTACK", "MYSTACK"))
})

test_that("stack_n_twice of no analyses is an empty table", {
  for (a in list(NULL, data.frame(analysis_id = character()))) {
    o <- stack_n_twice(a)
    expect_identical(nrow(o), 0L)
    expect_identical(names(o), c("analysis_id", "with"))
  }
})

test_that("stack_ungroup, stack_take_out and stack_move: an unknown id is an error or no change", {
  x <- stack_group(branch_planner(), "T1", c("CONT", "CAT"))
  expect_error(stack_ungroup(x, "T1", "NOPE"), "'NOPE' is not an analysis of T1.", fixed = TRUE)
  expect_identical(ids_of(stack_take_out(x, "T1", "NOPE")), ids_of(x))
  expect_identical(ids_of(stack_move(x, "T1", "NOPE")), ids_of(x))
  # an analysis outside any stack stays put
  expect_identical(ids_of(stack_move(x, "T1", "TOT")), ids_of(x))
})

test_that("copy_analysis puts a copy right after the original under the next free id", {
  x <- branch_planner()
  y <- copy_analysis(x, "T1", "CONT")
  expect_identical(attr(y, "copied"), "CONT_2")
  expect_identical(ids_of(y)[1:3], c("CONT", "CONT_2", "CAT"))
  z <- copy_analysis(y, "T1", "CONT")
  expect_identical(attr(z, "copied"), "CONT_3")
  expect_identical(ids_of(z)[1:4], c("CONT", "CONT_3", "CONT_2", "CAT"))
  expect_error(copy_analysis(x, "T1", "NOPE"), "'NOPE' is not an analysis of T1.", fixed = TRUE)
})

test_that("copy_analysis copies a stack with the analyses inside it, those pointing at the new parent", {
  x <- stack_group(branch_planner(), "T1", c("CONT", "CAT"))
  y <- copy_analysis(x, "T1", "STACK")
  expect_identical(attr(y, "copied"), "STACK_2")
  expect_identical(ids_of(y)[1:6], c("STACK", "CONT", "CAT", "STACK_2", "CONT_2", "CAT_2"))
  a <- ard_rows(y, "analyses", "T1")
  expect_identical(a$parent[a$analysis_id %in% c("CONT_2", "CAT_2")], c("STACK_2", "STACK_2"))
  expect_identical(a$parent[a$analysis_id %in% c("CONT", "CAT")], c("STACK", "STACK"))
})

test_that("stack_add_inside refuses an unknown parent, and a parent that runs one analysis and has one", {
  x <- stack_group(branch_planner(), "T1", c("CONT", "CAT"))
  expect_error(stack_add_inside(x, "T1", "NOPE"), "'NOPE' is not an analysis of T1.", fixed = TRUE)
  # make the parent a method that runs one analysis
  a <- ard_rows(x, "analyses", "T1")
  a$output_id <- NULL
  a$method[a$analysis_id == "STACK"] <- "cards::ard_strata"
  x2 <- set_ard_rows(x, "analyses", "T1", a)
  expect_error(stack_add_inside(x2, "T1", "STACK"), "cards::ard_strata() runs one analysis.",
               fixed = TRUE)
})

test_that("stack_add_inside takes the next free AN id when none is given, after the last one inside", {
  x <- stack_group(branch_planner(), "T1", c("CONT", "CAT"))
  y <- stack_add_inside(x, "T1", "STACK")
  a <- ard_rows(y, "analyses", "T1")
  expect_identical(a$analysis_id[1:4], c("STACK", "CONT", "CAT", "AN"))
  expect_identical(a$parent[4L], "STACK")
  expect_identical(a$method[4L], "cards::ard_summary")
})

test_that("stack_n_twice takes a table with no parent column as one with no stacks", {
  a <- data.frame(analysis_id = c("G1", "G2"), method = "categorical", dataset = "ADSL",
                  population_id = "SAF", by = NA_character_, variables = "TRT01A",
                  args = NA_character_, stringsAsFactors = FALSE)
  o <- stack_n_twice(a)
  expect_identical(o$analysis_id, c("G1", "G2"))
  expect_identical(o$with, c("G2", "G1"))
})
