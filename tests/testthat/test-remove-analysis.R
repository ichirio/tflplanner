# Deleting an analysis: one of its own, or one inside a stack; a stack
# itself goes through stack_remove().

ra_planner <- function() {
  p <- add_output(new_planner(), "T1")
  a <- data.frame(
    analysis_id = c("A1", "S1", "A2", "A3"),
    parent = c(NA, NA, "S1", "S1"),
    method = c("cards::ard_tabulate", "cards::ard_stack", "cards::ard_summary",
               "cards::ard_tabulate"),
    dataset = c("ADSL", "ADSL", NA, NA), population_id = "SAF",
    variables = c("SEX", NA, "AGE", "RACE"), stringsAsFactors = FALSE)
  set_ard_rows(p, "analyses", "T1", a)
}

test_that("an analysis of its own is deleted", {
  p <- remove_analysis(ra_planner(), "T1", "A1")
  expect_identical(ard_rows(p, "analyses", "T1")$analysis_id, c("S1", "A2", "A3"))
})

test_that("an analysis inside a stack is deleted; the stack keeps the others", {
  p <- remove_analysis(ra_planner(), "T1", "A2")
  a <- ard_rows(p, "analyses", "T1")
  expect_identical(a$analysis_id, c("A1", "S1", "A3"))
  expect_identical(a$parent[a$analysis_id == "A3"], "S1")
})

test_that("a stack is not deleted here, and an unknown id is an error", {
  expect_error(remove_analysis(ra_planner(), "T1", "S1"), "stack_remove")
  expect_error(remove_analysis(ra_planner(), "T1", "NOPE"), "not an analysis")
})
