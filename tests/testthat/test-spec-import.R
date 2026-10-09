# Export the definition files, edit the copy, import it back (#274)

imp_study <- function(id = "IM-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  p <- set_sheet_rows(p, "titles", "T-2", data.frame(line = "1", center = "Other"))
  suppressMessages(create_study(id, planner = p))
}

edit_book <- function(f, sheet, fn) {
  wb <- openxlsx::loadWorkbook(f)
  d <- openxlsx::read.xlsx(wb, sheet)
  d2 <- fn(d)
  openxlsx::deleteData(wb, sheet, cols = seq_len(ncol(d)), rows = seq_len(nrow(d) + 1L),
                       gridExpand = TRUE)
  openxlsx::writeData(wb, sheet, d2)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
}

test_that("the files go out as a folder or a zip", {
  home <- local_home()
  s <- imp_study()
  out <- file.path(home, "copy")
  export_spec_files("IM-1", out)
  expect_setequal(list.files(out, recursive = TRUE),
                  c("study.yml", "spec/table_spec.xlsx", "spec/report_spec.xlsx"))
  z <- file.path(home, "copy.zip")
  export_spec_files("IM-1", z)
  expect_setequal(utils::unzip(z, list = TRUE)$Name,
                  c("study.yml", "spec/table_spec.xlsx", "spec/report_spec.xlsx"))
})

test_that("an edited, renamed copy comes back part by part, the old files kept", {
  home <- local_home()
  s <- imp_study()
  out <- file.path(home, "copy")
  export_spec_files("IM-1", out)
  g <- file.path(home, "report_spec edited by me.xlsx")
  file.copy(file.path(out, "spec", "report_spec.xlsx"), g)
  edit_book(g, "titles", function(d) { d$center[d$output_id == "T-1"] <- "New title"; d })
  edit_book(g, "_tflplanner", function(d) { d$description[d$output_id %in% "T-2"] <- "renamed"; d })
  pv <- preview_spec_import("IM-1", g)
  expect_true(pv$ok)
  expect_setequal(pv$parts, c("sheet:titles", "report list"))
  expect_true("T-1" %in% pv$outputs)
  # nothing changed by a preview
  expect_identical(sheet_rows(open_study("IM-1")$planner, "titles", "T-1")$center, "Old title")
  before <- unname(tools::md5sum(file.path(s$path, "spec", "report_spec.xlsx")))
  # only the titles taken in
  r <- import_spec_files("IM-1", g, parts = "sheet:titles")
  o <- open_study("IM-1")
  expect_identical(o$spec$status, "same")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "New title")
  expect_identical(o$planner$outputs$description[2], "two")
  # the files as they were, kept before
  expect_true(dir.exists(r$backup))
  expect_identical(unname(tools::md5sum(file.path(r$backup, "spec", "report_spec.xlsx"))),
                   before)
  expect_match(r$backup, "spec/[.]backup/[0-9]{8}-[0-9]{6}$")
})

test_that("a copy that does not read changes nothing", {
  home <- local_home()
  s <- imp_study()
  g <- file.path(home, "report_spec.xlsx")
  writeLines("not a workbook", g)
  rec <- .read_state("IM-1")$files
  pv <- preview_spec_import("IM-1", g)
  expect_false(pv$ok)
  expect_identical(pv$problems$file[1], "spec/report_spec.xlsx")
  expect_error(import_spec_files("IM-1", g), class = "tflplanner_spec_invalid")
  expect_identical(.read_state("IM-1")$files, rec)
  expect_false(dir.exists(file.path(s$path, "spec", ".backup")))
})

test_that("a missing output_id column stops the import, in its sheet and workbook", {
  home <- local_home()
  s <- imp_study()
  g <- file.path(home, "titles.xlsx")
  file.copy(file.path(s$path, "spec", "report_spec.xlsx"), g)
  edit_book(g, "titles", function(d) d[setdiff(names(d), "output_id")])
  pv <- preview_spec_import("IM-1", g)
  expect_false(pv$ok)
  p <- pv$problems[pv$problems$severity == "error", ]
  expect_identical(p$file[1], "spec/report_spec.xlsx")
  expect_identical(p$sheet[1], "titles")
})

test_that("a filter that is not R stops the import at its cell", {
  skip_if_not_installed("cards")
  home <- local_home()
  suppressMessages(create_sample_study(run = FALSE))
  out <- file.path(home, "copy")
  export_spec_files("SAMPLE-01", out)
  # the ARD definition goes out as a workbook, to edit in Excel
  x <- file.path(out, "spec", "ard_spec.xlsx")
  expect_true(file.exists(x))
  expect_false(file.exists(file.path(out, "spec", "ard_definition.json")))
  edit_book(x, "analyses", function(d) { d$where[2] <- "AGE >"; d })
  pv <- preview_spec_import("SAMPLE-01", x)
  expect_false(pv$ok)
  p <- pv$problems[pv$problems$column %in% "where", ]
  expect_identical(p$file, "spec/ard_spec.xlsx")
  expect_identical(p$sheet, "analyses")
  expect_identical(p$row, "3")
  expect_match(p$message, "R cannot read it")
  expect_error(import_spec_files("SAMPLE-01", x), class = "tflplanner_spec_invalid")
  # and the same put in spec/ directly (its JSON): not taken in on open
  j <- file.path(studies_root(), "SAMPLE-01", "spec", "ard_definition.json")
  a <- jsonlite::fromJSON(j)
  a$analyses$where[2] <- "AGE >"
  writeLines(jsonlite::toJSON(a, dataframe = "columns", na = "null", pretty = TRUE), j)
  o <- suppressMessages(open_study("SAMPLE-01"))
  expect_identical(o$spec$status, "invalid")
  expect_true("where" %in% o$spec$problems$column)
})

test_that("a figure design's R that does not read stops the import at its field", {
  home <- local_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_fig_design(p, "F-1", tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  s <- suppressMessages(create_study("IM-2", planner = p))
  d <- tflspec::tfl_read_fig_design(file.path(s$path, "spec", "figures", "F-1.yml"))
  d$layers <- c(d$layers, list(list(layer = "layer_code", code = "geom_point(")))
  g <- file.path(home, "F-1 edited.yml")
  tflspec::tfl_write_fig_design(d, g)
  pv <- preview_spec_import("IM-2", g)
  expect_false(pv$ok)
  p <- pv$problems[pv$problems$file == "spec/figures/F-1.yml", ]
  expect_true(any(grepl("code", p$column)))
  expect_true(any(grepl("R cannot read it", p$message)))
})
