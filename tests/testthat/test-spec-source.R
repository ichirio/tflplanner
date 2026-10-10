# The SPEC is the source (#274): the definition files are read on open when
# they changed outside tflplanner, a save never writes over such a change.

# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

spec_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

spec_study <- function(id = "SP-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  p <- set_sheet_rows(p, "titles", "T-2", data.frame(line = "1", center = "Other"))
  suppressMessages(create_study(id, planner = p))
}

# a person's edit in Excel: one cell of a sheet of a workbook
edit_cell <- function(path, sheet, col, value, row = 1L) {
  wb <- openxlsx::loadWorkbook(path)
  d <- openxlsx::read.xlsx(wb, sheet)
  d[[col]][row] <- value
  openxlsx::writeData(wb, sheet, d)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

report_book <- function(s) file.path(s$path, "spec", "report_spec.xlsx")
history_n <- function(home, id) {
  length(list.files(file.path(.store_dir(id, home), "history")))
}

test_that("a save records each definition file's fingerprint", {
  home <- spec_home()
  s <- spec_study()
  st <- .read_state("SP-1", home)
  expect_identical(st$format, 2L)
  expect_setequal(names(st$files), c("study.yml", "spec/table_spec.xlsx",
                                     "spec/report_spec.xlsx"))
  expect_identical(st$files[["spec/report_spec.xlsx"]]$md5,
                   unname(tools::md5sum(report_book(s))))
  expect_true(all(spec_status("SP-1")$status == "same"))
})

test_that("an untouched study opens from its state, without reading a workbook", {
  spec_home()
  spec_study()
  local_mocked_bindings(read_planner = function(...) stop("read"))
  o <- open_study("SP-1")
  expect_identical(o$spec$status, "same")
})

test_that("a direct edit that reads is taken in on open", {
  home <- spec_home()
  s <- spec_study()
  n <- history_n(home, "SP-1")
  edit_cell(report_book(s), "titles", "center", "New title")
  expect_identical(spec_status("SP-1")$status[
    spec_status("SP-1")$file == "spec/report_spec.xlsx"], "changed")
  expect_message(o <- open_study("SP-1"), "changed outside tflplanner")
  expect_identical(o$spec$status, "adopted")
  expect_identical(o$spec$files$file, "spec/report_spec.xlsx")
  expect_true("sheet:titles" %in% o$spec$parts)
  expect_identical(o$spec$outputs, "T-1")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "New title")
  # the copy follows: the next open is the same, the old state in history
  expect_identical(open_study("SP-1")$spec$status, "same")
  expect_identical(history_n(home, "SP-1"), n + 1L)
  # and a save keeps the edit
  o2 <- save_study(o)
  expect_identical(sheet_rows(open_study("SP-1")$planner, "titles", "T-1")$center,
                   "New title")
})

test_that("a direct edit that does not read: open as saved, no save, write back or reload", {
  home <- spec_home()
  s <- spec_study()
  good <- tempfile(fileext = ".xlsx")
  edit_cell(report_book(s), "titles", "center", "Fixed title")
  file.copy(report_book(s), good)
  writeLines("not a workbook", report_book(s))
  o <- suppressMessages(open_study("SP-1"))
  expect_identical(o$spec$status, "invalid")
  expect_identical(o$spec$problems$file, "spec/report_spec.xlsx")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "Old title")
  # a save is stopped, whatever it changes
  o$planner$outputs$description[1] <- "changed in the app"
  expect_error(save_study(o), class = "tflplanner_spec_changed")
  expect_error(reload_from_spec("SP-1"), class = "tflplanner_spec_invalid")
  # fixed in Excel, then reloaded: taken in
  file.copy(good, report_book(s), overwrite = TRUE)
  r <- reload_from_spec("SP-1")
  expect_identical(r$spec$status, "adopted")
  expect_identical(sheet_rows(r$planner, "titles", "T-1")$center, "Fixed title")
  # broken again, then written back from the last save: the rejected file
  # is kept in spec/.rejected/
  writeLines("not a workbook", report_book(s))
  w <- write_spec("SP-1")
  rej <- list.files(file.path(s$path, "spec", ".rejected"))
  expect_length(rej, 1L)
  expect_match(rej, "^report_spec_[0-9-]+\\.xlsx$")
  o <- open_study("SP-1")
  expect_identical(o$spec$status, "same")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "Fixed title")
})

test_that("a missing workbook does not read; a figure design added or removed is taken in", {
  spec_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  s <- suppressMessages(create_study("SP-2", planner = p))
  d <- tflspec::tfl_fig_template("km_simple", data = "ADTTE")
  f <- file.path(s$path, "spec", "figures", "F-1.yml")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  tflspec::tfl_write_fig_design(d, f)
  o <- suppressMessages(open_study("SP-2"))
  expect_identical(o$spec$status, "adopted")
  expect_identical(fig_design(o$planner, "F-1")$template, "km_simple")
  unlink(f)
  o <- suppressMessages(open_study("SP-2"))
  expect_identical(o$spec$status, "adopted")
  expect_null(fig_design(o$planner, "F-1"))
  unlink(file.path(s$path, "spec", "table_spec.xlsx"))
  o <- suppressMessages(open_study("SP-2"))
  expect_identical(o$spec$status, "invalid")
  expect_match(o$spec$problems$message, "missing")
})

test_that("a workbook saved again unchanged is touched: no notice, no history", {
  home <- spec_home()
  s <- spec_study()
  n <- history_n(home, "SP-1")
  wb <- openxlsx::loadWorkbook(report_book(s))
  openxlsx::saveWorkbook(wb, report_book(s), overwrite = TRUE)
  skip_if(identical(spec_status("SP-1")$status[2:3], c("same", "same")),
          "openxlsx wrote the same bytes")
  expect_no_message(o <- open_study("SP-1"))
  expect_identical(o$spec$status, "touched")
  expect_identical(history_n(home, "SP-1"), n)
  expect_identical(open_study("SP-1")$spec$status, "same")
})

test_that("a state of an earlier version reads the files once, an earlier edit kept", {
  home <- spec_home()
  s <- spec_study()
  f <- .state_file("SP-1", home)
  st <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  st$files <- NULL
  st$format <- 1L
  writeLines(.json(st), f)
  # unchanged: read once, recorded, nothing to say
  expect_no_message(o <- open_study("SP-1"))
  expect_identical(o$spec$status, "same")
  expect_false(is.null(.read_state("SP-1", home)$files))
  # an edit made before the upgrade is taken in, not written over
  st$files <- NULL
  writeLines(.json(st), f)
  edit_cell(report_book(s), "titles", "center", "Edited before")
  o <- suppressMessages(open_study("SP-1"))
  expect_identical(o$spec$status, "adopted")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "Edited before")
})

test_that("a moved study folder matches its record", {
  home <- spec_home()
  s <- spec_study()
  to <- file.path(home, "moved")
  dir.create(to)
  file.rename(s$path, file.path(to, "SP-1"))
  o <- open_study(file.path(to, "SP-1"))
  expect_identical(o$spec$status, "same")
  expect_identical(.read_state("SP-1", home)$path,
                   normalizePath(file.path(to, "SP-1"), "/"))
})

test_that("an edit made while the study was off the list is taken in on register", {
  home <- spec_home()
  s <- spec_study()
  unregister_study("SP-1")
  edit_cell(report_book(s), "titles", "center", "While away")
  r <- suppressMessages(register_study(s$path))
  expect_identical(r$spec$status, "adopted")
  expect_identical(sheet_rows(r$planner, "titles", "T-1")$center, "While away")
  # one that does not read: the kept state comes back, marked
  unregister_study("SP-1")
  writeLines("not a workbook", report_book(s))
  r <- suppressMessages(register_study(s$path))
  expect_identical(r$spec$status, "invalid")
  expect_identical(sheet_rows(r$planner, "titles", "T-1")$center, "While away")
})

test_that("two sessions: another's save passes the guard, an outside edit does not", {
  spec_home()
  s <- spec_study()
  a <- open_study("SP-1")
  b <- open_study("SP-1")
  b$planner <- set_sheet_rows(b$planner, "titles", "T-2",
                              data.frame(line = "1", center = "B's"))
  save_study(b, base = open_study("SP-1"))
  a2 <- a
  a2$planner$outputs$description[1] <- "A's"
  saved <- save_study(a2, base = a)
  expect_identical(saved$planner$outputs$description[1], "A's")
  expect_identical(sheet_rows(saved$planner, "titles", "T-2")$center, "B's")
  # an Excel edit after A opened: A's save stops
  a <- open_study("SP-1")
  edit_cell(report_book(s), "titles", "center", "Outside")
  a$planner$outputs$description[1] <- "A again"
  expect_error(save_study(a, base = a), class = "tflplanner_spec_changed")
})

test_that("a save with nothing changed in the workbooks does not read them", {
  spec_home()
  s <- spec_study()
  local_mocked_bindings(read_planner = function(...) stop("read"))
  s$planner$outputs$description[1] <- "report list only"
  # the report list is in the report workbook: written, not read
  out <- save_study(s)
  st <- out$files$status[basename(out$files$file) == "report_spec.xlsx"]
  expect_identical(st, "written")
  # a study field only: neither workbook read nor written
  out$meta$title <- "a title"
  out <- save_study(out)
  st <- out$files$status[basename(out$files$file) == "table_spec.xlsx"]
  expect_identical(st, "unchanged")
})

test_that("a figure design is a part: merged, or a conflict", {
  spec_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- add_output(p, "T-1", type = "table")
  s <- suppressMessages(create_study("SP-3", planner = p))
  base <- open_study("SP-3")
  other <- base
  other$planner <- set_fig_design(other$planner, "F-1",
                                  tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  save_study(other, base = base)
  mine <- base
  mine$planner$outputs$description[mine$planner$outputs$output_id == "T-1"] <- "mine"
  m <- save_study(mine, base = base)
  expect_identical(fig_design(m$planner, "F-1")$template, "km_simple")
  # the same design changed by both: a conflict
  b2 <- open_study("SP-3")
  x <- b2
  x$planner <- set_fig_design(x$planner, "F-1", NULL)
  save_study(x, base = b2)
  y <- b2
  y$planner <- set_fig_design(y$planner, "F-1",
                              tflspec::tfl_fig_template("km_simple", data = "ADTTE2"))
  expect_error(save_study(y, base = b2), class = "tflplanner_conflict")
})

test_that("a design file tflplanner did not write is kept when its figure has none", {
  spec_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  s <- suppressMessages(create_study("SP-4", planner = p))
  # a file for a report the study does not have: kept, reported
  f <- file.path(s$path, "spec", "figures", "X-9.yml")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  tflspec::tfl_write_fig_design(tflspec::tfl_fig_template("km_simple", data = "ADTTE"), f)
  o <- suppressMessages(open_study("SP-4"))
  out <- save_study(o)
  expect_true(file.exists(f))
  # a design dropped in the app: tflplanner's own file goes
  o <- open_study("SP-4")
  o$planner <- set_fig_design(o$planner, "F-1",
                              tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  o <- save_study(o)
  g <- file.path(s$path, "spec", "figures", "F-1.yml")
  expect_true(file.exists(g))
  o$planner <- set_fig_design(o$planner, "F-1", NULL)
  o <- save_study(o)
  expect_false(file.exists(g))
  expect_true(file.exists(f))
})
