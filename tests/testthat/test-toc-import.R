toc_file <- function(rows) {
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title,Population,Footnotes", rows), f)
  f
}
toc_map <- c(output_id = "No.", type = "Kind", title = "Title",
             population = "Population", footnote = "Footnotes")

test_that("a TOC taken in the first time adds its reports, after the study's title lines", {
  x <- new_planner()
  x <- set_sheet_rows(x, "titles", NA, data.frame(line = c("1", "2"),
                                                  center = c("Sponsor", "Protocol")))
  expect_identical(toc_title_offset(x), 2L)
  sp <- tflspec::tfl_read_toc(toc_file(c(
    "T-14-1-1,Table,Demographics,Safety Population,N: subjects",
    "F-14-2-1,,Mean SBP by visit,Safety Population,")), map = toc_map)
  ch <- toc_changes(x, sp)
  expect_identical(ch$reports$status, c("new", "new"))
  expect_identical(ch$reports$guessed, c(FALSE, TRUE))
  y <- toc_apply(x, sp, ch)
  expect_identical(y$outputs$output_id, c("T-14-1-1", "F-14-2-1"))
  expect_identical(report_info(y, "F-14-2-1")$type, "figure")
  t1 <- sheet_rows(y, "titles", "T-14-1-1")
  expect_identical(t1$line, c("3", "4"))
  expect_identical(t1$center, c("Demographics", "Safety Population"))
  expect_identical(sheet_rows(y, "footnotes", "T-14-1-1")$left, "N: subjects")
  # a guessed type corrected before taking it in
  y2 <- toc_apply(x, sp, ch, types = c("F-14-2-1" = "user"))
  expect_identical(report_info(y2, "F-14-2-1")$type, "user")
})

test_that("taken in again: the TOC's lines updated, lines edited here asked about, a dropped report marked", {
  x <- new_planner()
  sp1 <- tflspec::tfl_read_toc(toc_file(c(
    "T-1,Table,Demographics,Safety Population,Note A | Note B",
    "T-2,Table,Disposition,All Subjects,",
    "T-3,Table,Vital signs,Safety Population,")), map = toc_map)
  x <- toc_apply(x, sp1, toc_changes(x, sp1))
  last <- toc_snapshot(sp1, toc_title_offset(x))
  # here: T-2's first title edited by hand, a table built for T-1 (a row of
  # its own the TOC does not hold)
  t2 <- sheet_rows(x, "titles", "T-2")
  t2$center[1] <- "Subject disposition (edited)"
  t2$output_id <- NULL
  x <- set_sheet_rows(x, "titles", "T-2", t2)
  x <- set_sheet_rows(x, "tables", "T-1", data.frame(cols = "TRT01A"))
  # the new TOC: T-1's title changed and a footnote dropped, T-2's title
  # changed, T-3 gone, T-4 new
  sp2 <- tflspec::tfl_read_toc(toc_file(c(
    "T-1,Table,Demographic characteristics,Safety Population,Note A",
    "T-2,Table,Disposition of subjects,All Subjects,",
    "T-4,Listing,Adverse events,Safety Population,")), map = toc_map)
  ch <- toc_changes(x, sp2, last)
  st <- stats::setNames(ch$reports$status, ch$reports$output_id)
  expect_identical(st[["T-1"]], "changed")
  expect_identical(st[["T-2"]], "changed")
  expect_identical(st[["T-3"]], "missing")
  expect_identical(st[["T-4"]], "new")
  l <- ch$lines
  expect_identical(l$action[l$output_id == "T-1" & l$sheet == "titles"], "update")
  expect_identical(l$action[l$output_id == "T-1" & l$sheet == "footnotes"], "remove")
  expect_identical(l$action[l$output_id == "T-2"], "ask")
  # by default the edited line is kept
  y <- toc_apply(x, sp2, ch)
  expect_identical(sheet_rows(y, "titles", "T-1")$center[1], "Demographic characteristics")
  expect_identical(sheet_rows(y, "footnotes", "T-1")$left, "Note A")
  expect_identical(sheet_rows(y, "titles", "T-2")$center[1], "Subject disposition (edited)")
  # what tflplanner added stays; the dropped report stays
  expect_identical(sheet_rows(y, "tables", "T-1")$cols, "TRT01A")
  expect_true("T-3" %in% y$outputs$output_id)
  expect_identical(report_info(y, "T-4")$type, "listing")
  # taken from the TOC when asked
  y2 <- toc_apply(x, sp2, ch, use_toc = "T-2|titles|1")
  expect_identical(sheet_rows(y2, "titles", "T-2")$center[1], "Disposition of subjects")
  # the same TOC again: nothing to do
  last2 <- toc_snapshot(sp2, toc_title_offset(y2))
  ch2 <- toc_changes(y2, sp2, last2)
  expect_identical(ch2$reports$status[ch2$reports$output_id != "T-3"],
                   c("same", "same", "same"))
  expect_identical(nrow(ch2$lines), 0L)
})

test_that("a report's type is not changed by a TOC taken in again", {
  x <- add_output(new_planner(), "F-1", type = "user")
  sp <- tflspec::tfl_read_toc(toc_file("F-1,Figure,KM plot,Safety Population,"),
                              map = toc_map)
  ch <- toc_changes(x, sp)
  expect_identical(ch$reports$status, "changed")
  expect_identical(ch$reports$type_toc, "figure")
  y <- toc_apply(x, sp, ch)
  expect_identical(report_info(y, "F-1")$type, "user")
})

test_that("a TOC's columns are mapped from the company's toc_map, and a mapping can be remembered", {
  std <- .builtin_standards()$toc_map
  m <- toc_map_for(c("No.", "Kind", "Title 1", "Title 2", "Population",
                     "Footnote 1", "Footnote 2", "Remark"), std)
  expect_identical(m$output_id, "No.")
  expect_identical(m$type, "Kind")
  expect_identical(m$title, c("Title 1", "Title 2"))
  expect_identical(m$footnote, c("Footnote 1", "Footnote 2"))
  expect_null(m$note)
  # what tfl_read_toc() takes
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title 1,Title 2,Population,Footnote 1,Footnote 2,Remark",
               "T-1,Table,Demog,Age,Safety Population,N,,x"), f)
  sp <- tflspec::tfl_read_toc(f, map = m)
  expect_identical(sp$report$output_id, "T-1")
  # remembered: "Remark" becomes a note's candidate
  l <- .toc_map_learn(std, list(note = "Remark", output_id = "No."))
  expect_identical(toc_map_for("Remark", l)$note, "Remark")
  expect_identical(sum(grepl("No.", strsplit(l$columns[l$item == "output_id"], " | ",
                                               fixed = TRUE)[[1]], fixed = TRUE)), 2L)
  home <- tempfile(); dir.create(home)
  remember_toc_map(list(note = "Remark"), home = home)
  expect_identical(toc_map_for("Remark", company_standards(home)$toc_map)$note, "Remark")
  expect_identical(toc_headers(f), c("No.", "Kind", "Title 1", "Title 2", "Population",
                                    "Footnote 1", "Footnote 2", "Remark"))
})

test_that("a TOC taken in is kept in input/toc with its record and what it said", {
  study <- list(path = tempfile())
  dir.create(study$path)
  expect_identical(nrow(toc_imports(study)), 0L)
  expect_null(toc_last(study))
  f <- toc_file(c("T-1,Table,Demographics,Safety Population,Note A | Note B"))
  x <- new_planner()
  sp <- tflspec::tfl_read_toc(f, map = toc_map)
  ch <- toc_changes(x, sp)
  .toc_record(study, f, "My TOC.csv", ch, toc_snapshot(sp, 0L))
  log <- toc_imports(study)
  expect_identical(log$import_id, "TOC001")
  expect_identical(log$new, "1")
  expect_true(file.exists(file.path(study$path, "input", "toc", log$file)))
  last <- toc_last(study)
  expect_identical(last, toc_snapshot(sp, 0L))
  # the next time, nothing changed
  y <- toc_apply(x, sp, ch)
  expect_identical(nrow(toc_changes(y, sp, last)$lines), 0L)
})

test_that("the app takes a TOC in: mapped, previewed, taken in, saved at once; again with an edited line", {
  local_home()
  create_study("TC", planner = new_planner())
  f1 <- file.path(tempdir(), "My TOC.csv")
  writeLines(c("No.,Kind,Title,Population,Footnotes",
               "T-1,Table,Demographics,Safety Population,N: subjects",
               "F-1,,Mean SBP,Safety Population,"), f1)
  f2 <- file.path(tempdir(), "My TOC v2.csv")
  writeLines(c("No.,Kind,Title,Population,Footnotes",
               "T-1,Table,Demographic characteristics,Safety Population,N: subjects"), f2)
  up <- function(f) data.frame(name = basename(f), datapath = f, stringsAsFactors = FALSE)
  shiny::testServer(server_for("TC"), {
    rv <- session$userData$rv
    session$setInputs(toc_new = 1)
    session$setInputs(toc1_file = up(f1), toc1_skip = 0)
    # the company's toc_map put on the header
    expect_match(output$toc1_map$html, "Which column is what", fixed = TRUE)
    session$setInputs(toc1_map_output_id = "No.", toc1_map_type = "Kind",
                      toc1_map_title = "Title", toc1_map_population = "Population",
                      toc1_map_footnote = "Footnotes", toc1_map_program = "",
                      toc1_map_file = "", toc1_map_note = "")
    h <- output$toc1_changes$html
    expect_match(h, "New 2, changed 0", fixed = TRUE)
    expect_match(h, "toc1_type_2", fixed = TRUE)
    # the guessed figure taken in as user code
    session$setInputs(toc1_type_1 = "table", toc1_type_2 = "user", toc1_remember = TRUE)
    session$setInputs(toc1_do = 1)
    expect_identical(rv$p$outputs$output_id, c("T-1", "F-1"))
    expect_identical(report_info(rv$p, "F-1")$type, "user")
    expect_match(output$toc1_result$html, "TOC001", fixed = TRUE)
    expect_null(output$toc1_do_btn$html)
    expect_null(output$toc1_changes$html)
    # saved at once, with the record
    expect_identical(open_study(rv$study$path)$planner$outputs$output_id, c("T-1", "F-1"))
    expect_identical(toc_imports(rv$study)$original, "My TOC.csv")
    # the mapping remembered in the home's company standards
    expect_true(file.exists(.standards_file()))
    # T-1's title edited here, then the TOC changed it too: asked
    t1 <- sheet_rows(rv$p, "titles", "T-1")
    t1$center[1] <- "Demography (edited)"
    t1$output_id <- NULL
    rv$p <- set_sheet_rows(rv$p, "titles", "T-1", t1)
    session$setInputs(toc_new = 2)
    session$setInputs(toc2_file = up(f2), toc2_skip = 0)
    session$setInputs(toc2_map_output_id = "No.", toc2_map_type = "Kind",
                      toc2_map_title = "Title", toc2_map_population = "Population",
                      toc2_map_footnote = "Footnotes")
    h <- output$toc2_changes$html
    expect_match(h, "Not in the TOC", fixed = TRUE)
    expect_match(h, "edited here", fixed = TRUE)
    k <- regmatches(h, regexpr("toc2_ask_[0-9]+", h))
    do.call(session$setInputs, stats::setNames(list(TRUE), k))
    session$setInputs(toc2_do = 1)
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center[1], "Demographic characteristics")
    # F-1 not in the TOC: kept
    expect_true("F-1" %in% rv$p$outputs$output_id)
    expect_identical(nrow(toc_imports(rv$study)), 2L)
  })
})
