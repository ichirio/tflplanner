# A report saved from the app says its column widths (R/save_widths.R):
# tflspec::tfl_fill_widths() on the reports new or changed in the save, a
# table whose columns changed given new ones

widths_of <- function(o) {
  hit <- Filter(function(it) identical(as.character(it$fn), "plan_columns"), o$content)
  if (length(hit)) hit[[1L]]$args$widths else NULL
}

test_that("a table's columns' signature, and its widths taken away", {
  o <- tflspec::tfl_output("T-1", "table")
  o$content <- list(tflspec::tfl_item("table_plan", list(cols = "TRT01A")),
                    tflspec::tfl_item("plan_columns", list(widths = list(row_label = 3, .values = 1),
                                                            align = "decimal")),
                    tflspec::tfl_item("plan_levels", list(TRT01A = list("A", "B"), SEX = list("M", "F"))))
  sig <- .cols_signature(o)
  expect_identical(sig$cols, "TRT01A")
  expect_named(sig$levels, "TRT01A")
  expect_false(sig$total)
  # the rows' levels are not the columns
  o2 <- o
  o2$content[[3]]$args$SEX <- list("F", "M")
  expect_identical(.cols_signature(o2), sig)
  # a total column is
  o3 <- o
  o3$content[[4]] <- tflspec::tfl_item("plan_total", list(label = "Total"))
  expect_false(identical(.cols_signature(o3), sig))
  expect_true(.has_widths(o))
  d <- .drop_widths(o)
  expect_false(.has_widths(d))
  expect_identical(d$content[[2]]$args, list(align = "decimal"))
  # an item that said only the widths: gone
  o$content[[2]]$args$align <- NULL
  expect_identical(vapply(.drop_widths(o)$content, function(it) as.character(it$fn), ""),
                   c("table_plan", "plan_levels"))
})

# tflspec::tfl_fill_widths() as a stand-in: the widths it would write
# (`asked` the reports it was asked for)
fake_fill <- function(env) {
  function(study_dir, output_id = NULL, assumption = TRUE) {
    env$asked <- c(env$asked, list(output_id))
    env$assumption <- assumption
    for (id in output_id) {
      f <- file.path(study_dir, "spec", "outputs", paste0(id, ".yml"))
      o <- tflspec::tfl_read_output(f)
      if (.has_widths(o)) next
      o$content[[length(o$content) + 1L]] <- tflspec::tfl_item("plan_columns",
                                                              list(widths = list(row_label = 3, .values = 1)))
      tflspec::tfl_write_output(o, f)
    }
    invisible(output_id)
  }
}

test_that("the app's save gives the reports changed there widths; a table whose columns changed new ones", {
  skip_on_cran()
  home <- local_home()
  suppressMessages(create_sample_study(run = FALSE, home = home))
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  env <- new.env()
  local_mocked_bindings(tfl_fill_widths = fake_fill(env), .package = "tflspec")
  id <- "T-14-1-1"
  f <- file.path(s$path, "spec", "outputs", paste0(id, ".yml"))
  # a save that changes nothing asks for nothing
  s <- suppressMessages(save_study(s, home = home, fill_widths = TRUE))
  expect_null(env$asked)
  # the default save (not the app's) leaves the widths alone
  o <- s$planner$spec$outputs[[id]]
  o$label <- paste(o$label, "(1)")
  s$planner <- .spec_set_output(s$planner, id, o)
  s <- suppressMessages(save_study(s, home = home))
  expect_null(env$asked)
  expect_false(.has_widths(tflspec::tfl_read_output(f)))
  # the app's save: only the report changed, no assumption; the planner as the file
  o <- s$planner$spec$outputs[[id]]
  o$label <- paste(o$label, "(2)")
  s$planner <- .spec_set_output(s$planner, id, o)
  s <- suppressMessages(save_study(s, home = home, fill_widths = TRUE))
  expect_identical(env$asked, list(id))
  expect_false(env$assumption)
  expect_identical(widths_of(tflspec::tfl_read_output(f)), list(row_label = 3, .values = 1))
  expect_identical(widths_of(s$planner$spec$outputs[[id]]), list(row_label = 3, .values = 1))
  expect_false(is.null(s$planner_saved))
  # the files as the save recorded them: the next save is not stopped
  s2 <- suppressMessages(open_study("SAMPLE-01", home = home))
  expect_identical(widths_of(s2$planner$spec$outputs[[id]]), list(row_label = 3, .values = 1))
  # its widths changed by hand, then its columns: the old widths go, new ones come
  o <- s2$planner$spec$outputs[[id]]
  at <- which(vapply(o$content, function(it) it$fn, "") == "plan_columns")
  o$content[[at]]$args$widths <- list(row_label = 5, .values = 2)
  s2$planner <- .spec_set_output(s2$planner, id, o)
  s2 <- suppressMessages(save_study(s2, home = home, fill_widths = TRUE))
  expect_identical(widths_of(tflspec::tfl_read_output(f)), list(row_label = 5, .values = 2))
  o <- s2$planner$spec$outputs[[id]]
  o$content[[length(o$content) + 1L]] <- tflspec::tfl_item("plan_total", list(label = "Total"))
  s2$planner <- .spec_set_output(s2$planner, id, o)
  s2 <- suppressMessages(save_study(s2, home = home, fill_widths = TRUE))
  expect_identical(widths_of(tflspec::tfl_read_output(f)), list(row_label = 3, .values = 1))
})
