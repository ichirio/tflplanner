# A template report's sheets edited (the cells sheet): only the item the
# edit changes is written, as the report's own after the template -- not
# the template's items expanded next to it (the user's DM report, 2026-11-09)

test_that("a cells-sheet edit of a template report writes one item after the template", {
  skip_on_cran()
  p <- .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
  o <- tflspec::tfl_output("T-99", "table", population = "SAF",
                           template = list(name = "demog",
                                         args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  p <- .spec_add_output(p, o, population = "SAF")
  ce <- sheet_rows(p, "cells", "T-99")
  i <- which(ce$row == "Median")[1L]
  expect_identical(ce$template[i], "{median}")
  ce$template[i] <- "{median:.2f}"
  q <- .spec_sync(set_sheet_rows(p, "cells", "T-99", ce), p$spec$meta)
  out <- q$spec$outputs[["T-99"]]
  # the template stays, and its items are not written again
  expect_identical(out$template$name, "demog")
  fns <- vapply(out$content %||% list(), function(x) x$fn, "")
  expect_identical(fns, "plan_cells")
  txt <- paste(q$spec$text[[q$spec$where[["T-99"]]]], collapse = "\n")
  expect_match(txt, "{median:.2f}", fixed = TRUE)
  expect_false(grepl("table_plan", txt, fixed = TRUE))
  expect_false(grepl("plan_labels", txt, fixed = TRUE))
  # read back: the view says the edit, and the program's cells say it too
  expect_identical(sheet_rows(q, "cells", "T-99")$template[i], "{median:.2f}")
  r <- tflspec::tfl_resolve_output(out, q$spec$study)
  cells <- Filter(function(x) identical(x$fn, "plan_cells"), r$content)
  expect_match(paste(unlist(cells), collapse = " "), "{median:.2f}", fixed = TRUE)
  # the template's other arguments of the item stay in effect (its
  # categorical format), and nothing is unset
  r0 <- tflspec::tfl_resolve_output(p$spec$outputs[["T-99"]], p$spec$study)
  before <- Filter(function(x) identical(x$fn, "plan_cells"), r0$content)[[1L]]$args
  expect_identical(cells[[1L]]$args$default, before$default)
  expect_false(grepl("!unset", txt, fixed = TRUE))
  expect_false(grepl("default:", txt, fixed = TRUE))
  # a second edit changes the same own item, not a second one
  ce2 <- sheet_rows(q, "cells", "T-99")
  ce2$template[i] <- "{median:.3f}"
  q2 <- .spec_sync(set_sheet_rows(q, "cells", "T-99", ce2), q$spec$meta)
  expect_identical(vapply(q2$spec$outputs[["T-99"]]$content, function(x) x$fn, ""), "plan_cells")
  expect_match(paste(q2$spec$text[[q2$spec$where[["T-99"]]]], collapse = "\n"), "{median:.3f}", fixed = TRUE)
})

test_that("a template report is expanded before tflspec makes its sheets", {
  # a tflspec that writes a template report to its `templates` sheet leaves
  # the expanding to tflplanner: the sheets still hold the report's items
  skip_on_cran()
  p <- .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
  o <- tflspec::tfl_output("T-99", "table", population = "SAF",
                           template = list(name = "demog", args = list(population = "SAF")))
  p <- .spec_add_output(p, o, population = "SAF")
  seen <- NULL
  real <- tflspec::tfl_sheets_from_spec
  local_mocked_bindings(tfl_sheets_from_spec = function(study, outputs, ...) {
    seen <<- outputs
    real(study, outputs)
  }, .package = "tflspec")
  back <- .sheets_from_spec(p$spec$study, p$spec$outputs["T-99"])
  expect_false(is.list(seen[["T-99"]]$template))
  expect_true(length(seen[["T-99"]]$content) > 0L)
  ce <- back$table$cells
  expect_true(any(!is.na(ce$output_id) & ce$output_id == "T-99"))
  # a report with no template is passed as it is
  plain <- tflspec::tfl_output("T-98", "table", content = list())
  .sheets_from_spec(p$spec$study, list(`T-98` = plain))
  expect_identical(seen[["T-98"]], plain)
})
