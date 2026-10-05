# A report's program: its own file name, unless the report sheet says one.

test_that("tflplanner's old default program is read as blank; a choice is kept", {
  p <- new_planner()
  p$sheets$report <- .normalize_sheet(data.frame(
    output_id = c(NA, "T1", NA),
    type = c("table", NA, NA),
    program = c("{output_id}.R", "{output_id}.R", NA)), "report")
  q <- .old_program_default(p)
  expect_true(is.na(q$sheets$report$program[1L]))
  # a report's own row is a choice
  expect_identical(q$sheets$report$program[2L], "{output_id}.R")
  # another default is a choice too
  p$sheets$report$program[1L] <- "t_{output_id}.R"
  expect_identical(.old_program_default(p)$sheets$report$program[1L], "t_{output_id}.R")
})

test_that("a new study's default report row names no program", {
  local_home()
  s <- create_study("PD1")
  r <- s$planner$sheets$report
  expect_true(all(is.na(r$program[is.na(r$output_id)])))
  # the program file is still <output_id>.R
  s$planner <- add_output(s$planner, "T-1")
  expect_identical(report_info(s$planner, "T-1")$program, "T-1.R")
})
