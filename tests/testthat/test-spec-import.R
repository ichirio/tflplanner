# Export the definition files, edit the copy, import it back (#274)

imp_study <- function(id = "IM-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  p <- set_sheet_rows(p, "titles", "T-2", data.frame(line = "1", center = "Other"))
  suppressMessages(create_study(id, planner = p))
}

# a report's file of the YAML spec, edited (fn: the tfl_output to the edited one)
edit_output <- function(f, fn) {
  tflspec::tfl_write_output(fn(tflspec::tfl_read_output(f)), f)
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

test_that("the files go out as a folder or a zip, with a flat workbook", {
  home <- local_home()
  s <- imp_study()
  out <- file.path(home, "copy")
  export_spec_files("IM-1", out)
  want <- c(.spec_files_yaml(s$path), .flat_xlsx)
  expect_setequal(list.files(out, recursive = TRUE), want)
  z <- file.path(home, "copy.zip")
  export_spec_files("IM-1", z)
  expect_setequal(utils::unzip(z, list = TRUE)$Name, want)
})

test_that("an edited, renamed copy comes back part by part, the old files kept", {
  home <- local_home()
  s <- imp_study()
  out <- file.path(home, "copy")
  export_spec_files("IM-1", out)
  g <- file.path(home, "mine")
  dir.create(g)
  t1 <- file.path(g, "T-1 edited by me.yml")
  file.copy(file.path(out, "spec", "outputs", "T-1.yml"), t1)
  edit_output(t1, function(o) { o$page$titles[["1"]]$center <- "New title"; o })
  toc <- file.path(g, "my toc.yml")
  y <- yaml::read_yaml(file.path(out, "spec", "toc.yml"))
  y$outputs[[2L]]$label <- "renamed"
  writeLines(yaml::as.yaml(y), toc)
  pv <- preview_spec_import("IM-1", g)
  expect_true(pv$ok)
  expect_true(all(c("sheet:titles", "report list") %in% pv$parts))
  expect_true("T-1" %in% pv$outputs)
  # nothing changed by a preview
  expect_identical(sheet_rows(open_study("IM-1")$planner, "titles", "T-1")$center, "Old title")
  before <- unname(tools::md5sum(file.path(s$path, "spec", "outputs", "T-1.yml")))
  # only the titles taken in
  r <- import_spec_files("IM-1", g, parts = "sheet:titles")
  o <- open_study("IM-1")
  expect_identical(o$spec$status, "same")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "New title")
  expect_identical(o$planner$outputs$description[2], "two")
  # the files as they were, kept before
  expect_true(dir.exists(r$backup))
  expect_identical(unname(tools::md5sum(file.path(r$backup, "spec", "outputs", "T-1.yml"))),
                   before)
  expect_match(r$backup, "spec/[.]backup/[0-9]{8}-[0-9]{6}$")
})

test_that("a copy that does not read changes nothing", {
  home <- local_home()
  s <- imp_study()
  g <- file.path(home, "T-1.yml")
  writeLines(c("id: [", "kind"), g)
  rec <- .read_state("IM-1")$files
  pv <- preview_spec_import("IM-1", g)
  expect_false(pv$ok)
  expect_true(any(grepl("T-1.yml", pv$problems$file, fixed = TRUE)))
  expect_error(import_spec_files("IM-1", g), class = "tflplanner_spec_invalid")
  expect_identical(.read_state("IM-1")$files, rec)
  expect_false(dir.exists(file.path(s$path, "spec", ".backup")))
})

test_that("the flat workbook comes back: its rows into the YAML files", {
  skip_if_not_installed("openxlsx")
  home <- local_home()
  s <- imp_study()
  out <- file.path(home, "copy")
  export_spec_files("IM-1", out)
  x <- file.path(out, .flat_xlsx)
  edit_book(x, "titles", function(d) { d$center[d$output_id %in% "T-2"] <- "From Excel"; d })
  pv <- preview_spec_import("IM-1", x)
  expect_true(pv$ok)
  expect_true("sheet:titles" %in% pv$parts)
  import_spec_files("IM-1", x)
  expect_identical(sheet_rows(open_study("IM-1")$planner, "titles", "T-2")$center, "From Excel")
})

test_that("the workbooks of an earlier tflplanner are converted on import", {
  skip_if_not_installed("openxlsx")
  home <- local_home()
  s <- imp_study()
  # the study's definition as the workbooks of before the YAML spec
  old <- file.path(home, "old")
  write_planner(s$planner, old)
  r <- write_planner(set_sheet_rows(s$planner, "titles", "T-1",
                                    data.frame(line = "1", center = "From the old workbook")),
                     old)
  pv <- preview_spec_import("IM-1", old)
  expect_true(pv$ok)
  expect_true("sheet:titles" %in% pv$parts)
  import_spec_files("IM-1", old)
  o <- open_study("IM-1")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "From the old workbook")
  # the study is still the YAML spec only
  expect_length(list.files(file.path(s$path, "spec"), "\\.(xlsx|json)$"), 0L)
})

test_that("a filter that is not R stops the import at its report", {
  skip_if_not_installed("cards")
  home <- local_home()
  suppressMessages(create_sample_study(run = FALSE))
  out <- file.path(home, "copy")
  export_spec_files("SAMPLE-01", out)
  x <- file.path(out, "spec", "outputs", "T-14-1-1.yml")
  edit_output(x, function(o) { o$ard$analyses[[2L]]$where <- tflspec::tfl_r("AGE >"); o })
  pv <- preview_spec_import("SAMPLE-01", x)
  expect_false(pv$ok)
  p <- pv$problems[pv$problems$column %in% "where", ]
  expect_identical(p$file, "spec/outputs/T-14-1-1.yml")
  expect_identical(p$sheet, "analyses")
  expect_match(p$message, "R cannot read it")
  expect_error(import_spec_files("SAMPLE-01", x), class = "tflplanner_spec_invalid")
  # and the same put in spec/ directly: not taken in on open
  file.copy(x, file.path(studies_root(), "SAMPLE-01", "spec", "outputs", "T-14-1-1.yml"),
            overwrite = TRUE)
  o <- suppressMessages(open_study("SAMPLE-01"))
  expect_identical(o$spec$status, "invalid")
  expect_true("where" %in% o$spec$problems$column)
})

test_that("a figure design's R that does not read stops the import at its field", {
  home <- local_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_fig_design(p, "F-1", tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  s <- suppressMessages(create_study("IM-2", planner = p))
  g <- file.path(home, "F-1 edited.yml")
  file.copy(file.path(s$path, "spec", "outputs", "F-1.yml"), g)
  edit_output(g, function(o) {
    o$content <- c(o$content, list(tflspec::tfl_item("layer_code", list(code = "geom_point("))))
    o
  })
  pv <- preview_spec_import("IM-2", g)
  expect_false(pv$ok)
  p <- pv$problems[pv$problems$file == "spec/outputs/F-1.yml", ]
  expect_true(any(grepl("code", p$column)))
  expect_true(any(grepl("R cannot read it", p$message)))
})
