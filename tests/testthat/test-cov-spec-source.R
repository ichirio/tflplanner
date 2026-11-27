# The branches of R/spec_source.R no other test reaches: a study folder
# that is not there, problems that name no sheet, a study not registered,
# where each kind of file of a copy goes, a copy in a zip, an unsaved spec
# that cannot be kept, and an adoption taken in part.

cov_src_study <- function(id = "CS-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "A title"))
  suppressMessages(create_study(id, planner = p, title = "Coverage study"))
}

test_that("a study folder that is not there has no fingerprints", {
  expect_identical(.spec_fingerprints(NULL), list())
  expect_identical(.spec_fingerprints(NA_character_), list())
  expect_identical(.spec_fingerprints(file.path(tempdir(), "no-such-study")), list())
})

test_that("a problem whose message names no sheet has no sheet and no row", {
  p <- .spec_problems("spec/toc.yml", "The file does not read.")
  expect_true(is.na(p$sheet))
  expect_true(is.na(p$row))
  expect_identical(p$severity, "error")
  p <- .spec_problems("spec/x.yml", "`cells` row 12: no value", "warning")
  expect_identical(c(p$sheet, p$row), c("cells", "12"))
  expect_false(.has_errors(p))
  expect_false(.has_errors(NULL))
})

test_that("a changed part with no output_id column touches no report", {
  a <- list(meta = list(), planner = new_planner())
  b <- a
  a$planner$ard$datasets <- data.frame(dataset = "ADSL", path = "a.rds")
  b$planner$ard$datasets <- data.frame(dataset = "ADSL", path = "b.rds")
  expect_identical(.changed_outputs(a, b, "ard:datasets"), character())
  # a figure's design names its report
  expect_identical(.changed_outputs(a, b, "fig:F-1"), "F-1")
})

test_that("a study that is not registered: each function that reads one says so", {
  local_home()
  msg <- "No study 'NOPE-1' is registered."
  expect_error(reload_from_spec("NOPE-1"), msg, fixed = TRUE)
  expect_error(write_spec("NOPE-1"), msg, fixed = TRUE)
  expect_error(spec_status("NOPE-1"), msg, fixed = TRUE)
  expect_error(export_spec_files("NOPE-1", tempfile()), msg, fixed = TRUE)
  expect_error(preview_spec_import("NOPE-1", tempfile()), msg, fixed = TRUE)
  expect_error(list_spec_backups("NOPE-1"), msg, fixed = TRUE)
  expect_error(preview_spec_backup("NOPE-1", "20260101-000000"), msg, fixed = TRUE)
})

test_that("a backup that is not there is an error naming it", {
  local_home()
  cov_src_study()
  expect_error(.backup_copy("CS-1", "20990101-000000", tflplanner_home()),
               "No backup '20990101-000000'.", fixed = TRUE)
  expect_error(.backup_copy("CS-1", "", tflplanner_home()), "No backup", fixed = TRUE)
  expect_identical(nrow(list_spec_backups("CS-1")), 0L)
})

test_that("where a copy's workbooks go: the flat one, an earlier tflplanner's, another workbook not at all", {
  d <- withr_tempdir()
  book <- function(name, sheets) {
    f <- file.path(d, name)
    writexl::write_xlsx(stats::setNames(rep(list(data.frame(a = 1)), length(sheets)), sheets), f)
    f
  }
  expect_identical(.import_place(book("flat.xlsx", c("toc", "cells")), "T-1"), "(flat)")
  expect_identical(.import_place(book("old.xlsx", c("titles", "page")), "T-1"), "(old)")
  expect_true(is.na(.import_place(book("other.xlsx", c("Budget", "Notes")), "T-1")))
  json <- file.path(d, "ard.json")
  writeLines("{}", json)
  expect_identical(.import_place(json, "T-1"), "(old)")
  txt <- file.path(d, "notes.txt")
  writeLines("x", txt)
  expect_true(is.na(.import_place(txt, "T-1")))
})

test_that("where a copy's YAML files go: by their place, Windows separators too, else by what they hold", {
  d <- withr_tempdir()
  y <- function(name, lines) {
    f <- file.path(d, name)
    writeLines(lines, f)
    f
  }
  f <- y("T-1.yml", "id: T-1")
  expect_identical(.import_place(f, "T-1", "copy\\spec\\outputs\\T-1.yml"), "spec/outputs/T-1.yml")
  expect_true(is.na(.import_place(f, "T-1", "spec/codelists.yml")))
  # an old figure design in spec/figures/: by what it holds (its layers)
  expect_identical(.import_place(y("F-1.yml", "layers: []"), "T-1", "spec/figures/F-1.yml"), "(old)")
  expect_identical(.import_place(y("s.yml", "study_id: CS-1"), "T-1"), .study_file)
  expect_identical(.import_place(y("l.yml", "layers: []"), "T-1"), "(old)")
  expect_identical(.import_place(y("d.yml", "styles: {}"), "T-1"), "spec/defaults.yml")
  expect_identical(.import_place(y("data.yml", "datasets: []"), "T-1"), "spec/data.yml")
  expect_identical(.import_place(y("toc.yml", "outputs: []"), "T-1"), "spec/toc.yml")
  # a file that is not a map: a report's, by its name
  expect_identical(.import_place(y("T-9 mine.yml", "just text"), "T-1"),
                   "spec/outputs/T-9 mine.yml")
  expect_true(is.na(.import_place(y("other.yml", "other: 1"), "T-1")))
})

test_that("an earlier tflplanner's workbook goes where its sheets say", {
  d <- withr_tempdir()
  book <- function(name, sheets) {
    f <- file.path(d, name)
    writexl::write_xlsx(stats::setNames(rep(list(data.frame(a = 1)), length(sheets)), sheets), f)
    f
  }
  expect_identical(.old_book_name(file.path(d, "x.json")), .ard_json)
  expect_identical(.old_book_name(book("a.xlsx", "analyses")), .ard_file)
  expect_identical(.old_book_name(book("l.xlsx", "listings")), .lf_file)
  expect_identical(.old_book_name(book("r.xlsx", "titles")), .report_file)
  expect_identical(.old_book_name(book("t.xlsx", "cells")), .table_file)
})

test_that("a part in a few words: none, a long text cut, a design, a vector", {
  cur <- list(meta = list(title = NULL, compound = paste(rep("x", 50), collapse = "")),
              planner = new_planner())
  cur$planner$fig_designs <- list(F1 = list(a = 1))
  inc <- list(meta = list(title = "New", compound = "short"), planner = new_planner())
  inc$planner$fig_designs <- list(F1 = list(a = 2))
  s <- .part_summary(cur, inc, c("meta:title", "meta:compound", "fig:F1"))
  expect_identical(s[1], "none \u2192 New")
  expect_identical(s[2], paste0(strrep("x", 37), "... \u2192 short"))
  expect_identical(s[3], "a design \u2192 a design")
})

test_that("a copy in a zip is read like a folder", {
  skip_on_cran()
  home <- local_home()
  cov_src_study()
  z <- file.path(home, "copy.zip")
  export_spec_files("CS-1", z)
  pv <- preview_spec_import("CS-1", z)
  expect_true(pv$ok)
  expect_identical(pv$parts, character())
  expect_true(any(startsWith(pv$files, "spec/outputs/")))
})

test_that("a report's unsaved spec that cannot be set is said in a warning, the merge going on", {
  base <- list(spec = list(outputs = list(A = list(x = 1))))
  mine <- list(spec = list(outputs = list(A = list(x = 2))))
  now <- list(spec = list(outputs = list(A = list(x = 1))),
              outputs = data.frame(output_id = "A"))
  local_mocked_bindings(.spec_set_output = function(...) stop("bad template"))
  expect_warning(out <- .merge_spec_outputs(base, mine, now),
                 "The report A's unsaved spec could not be kept: bad template", fixed = TRUE)
  expect_identical(out, now)
  # a side with no spec: `now` as it is
  expect_identical(.merge_spec_outputs(base, list(), now), now)
})

test_that("keeping my changes: nothing to do without a spec; a spec that cannot be set is said", {
  keep <- list(planner = list(spec = NULL))
  expect_identical(.keep_mine_outputs(keep, list(planner = list()), list(), "fig:A"), keep)
  keep <- list(planner = list(spec = list(outputs = list(A = list(x = 1)))))
  mine <- list(planner = list(spec = list(outputs = list(A = list(x = 2)))))
  now <- list(planner = list(spec = list(outputs = list(A = list(x = 1)))))
  local_mocked_bindings(.spec_set_output = function(...) stop("bad template"),
                        .tpl_of = function(...) "tpl")
  expect_warning(out <- .keep_mine_outputs(keep, mine, now, "fig:A"),
                 "could not be kept: bad template", fixed = TRUE)
  expect_identical(out, keep)
})

test_that("a change taken in part: the parts taken stay, the others go back, the files kept first", {
  skip_on_cran()
  local_home()
  s <- open_study(cov_src_study()$meta$study_id)
  was <- list(planner = s$planner, meta = s$meta)
  s$meta$title <- "Title from the files"
  s$meta$compound <- "CPD-9"
  s$spec <- list(status = "adopted", was = was, parts = c("meta:title", "meta:compound"),
                 outputs = character())
  out <- suppressMessages(.apply_adoption(s, take = "meta:title"))
  expect_identical(out$spec$status, "imported")
  expect_identical(out$spec$parts, "meta:title")
  expect_identical(out$meta$title, "Title from the files")
  expect_identical(out$meta$compound, was$meta$compound)
  expect_true(dir.exists(out$backup))
  # every part taken: nothing to do
  expect_identical(.apply_adoption(s, take = s$spec$parts), s)
})
