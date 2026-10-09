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

test_that("lines added here stay, after the TOC's lines, when the TOC gets more lines (S1's review of #104)", {
  x <- new_planner()
  sp1 <- tflspec::tfl_read_toc(toc_file(
    "T-1,Table,Demographics,Safety Population,Note A"), map = toc_map)
  ch1 <- toc_changes(x, sp1)
  x <- toc_apply(x, sp1, ch1)
  last <- toc_snapshot(sp1, toc_title_offset(x))
  # here: title 1 edited, a third title line and a second footnote added
  ti <- sheet_rows(x, "titles", "T-1")
  ti$output_id <- NULL
  ti$center[1] <- "Demographics (edited)"
  ti[3, ] <- NA
  ti$line[3] <- "3"
  ti$center[3] <- "Our own third line"
  x <- set_sheet_rows(x, "titles", "T-1", ti)
  fo <- sheet_rows(x, "footnotes", "T-1")
  fo$output_id <- NULL
  fo[2, ] <- NA
  fo$line[2] <- "2"
  fo$left[2] <- "Our own footnote"
  x <- set_sheet_rows(x, "footnotes", "T-1", fo)
  # the TOC adds a second title and a second footnote
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title 1,Title 2,Population,Footnote 1,Footnote 2",
               "T-1,Table,Demographics,By Treatment Group,Safety Population,Note A,Percentages use N"), f)
  m <- list(output_id = "No.", type = "Kind", title = c("Title 1", "Title 2"),
            population = "Population", footnote = c("Footnote 1", "Footnote 2"))
  sp2 <- tflspec::tfl_read_toc(f, map = m)
  ch <- toc_changes(x, sp2, last)
  # nothing to ask: title 1 was edited here, but the TOC did not change it
  expect_false(any(ch$lines$action == "ask"))
  expect_identical(ch$kept$line, "1")
  expect_true(all(c("add", "update", "move") %in% ch$lines$action))
  y <- toc_apply(x, sp2, ch)
  expect_identical(sheet_rows(y, "titles", "T-1")$center,
                   c("Demographics (edited)", "By Treatment Group", "Safety Population",
                     "Our own third line"))
  expect_identical(sheet_rows(y, "footnotes", "T-1")$left,
                   c("Note A", "Percentages use N", "Our own footnote"))
  # again with the same TOC: nothing changes, nothing asked
  last2 <- toc_snapshot(sp2, toc_title_offset(y), last)
  ch2 <- toc_changes(y, sp2, last2)
  expect_identical(nrow(ch2$lines), 0L)
  expect_identical(ch2$reports$status, "same")
  # the TOC changes title 1 too: now it is asked about
  writeLines(c("No.,Kind,Title 1,Title 2,Population,Footnote 1,Footnote 2",
               "T-1,Table,Demography,By Treatment Group,Safety Population,Note A,Percentages use N"), f)
  sp3 <- tflspec::tfl_read_toc(f, map = m)
  ch3 <- toc_changes(y, sp3, last2)
  expect_identical(ch3$lines$action, "ask")
  expect_identical(sheet_rows(toc_apply(y, sp3, ch3, use_toc = "T-1|titles|1"),
                              "titles", "T-1")$center[c(1, 4)],
                   c("Demography", "Our own third line"))
})

test_that("a report once taken in from a TOC is said to be missing every time", {
  x <- new_planner()
  sp1 <- tflspec::tfl_read_toc(toc_file(c("T-1,Table,A,Safety Population,",
                                          "T-2,Table,B,Safety Population,")), map = toc_map)
  x <- toc_apply(x, sp1, toc_changes(x, sp1))
  last <- toc_snapshot(sp1, 0L)
  sp2 <- tflspec::tfl_read_toc(toc_file("T-1,Table,A,Safety Population,"), map = toc_map)
  ch2 <- toc_changes(x, sp2, last)
  expect_identical(ch2$reports$status[ch2$reports$output_id == "T-2"], "missing")
  last2 <- toc_snapshot(sp2, 0L, last)
  expect_false(last2[["T-2"]]$in_toc)
  ch3 <- toc_changes(x, sp2, last2)
  expect_identical(ch3$reports$status[ch3$reports$output_id == "T-2"], "missing")
})

test_that("a report ID on two rows is said with its rows", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Title", "T-1,A", "T-2,B", "T-1,C"), f)
  d <- .toc_dups(f, NULL, 0L, "No.")
  expect_identical(d$output_id, "T-1")
  expect_identical(d$rows, "2, 4")
  expect_null(.toc_dups(f, NULL, 0L, "Title"))
})

test_that("the dialog: the last TOC said, the same file noticed, two rows of one ID refused, an xlsx read", {
  local_home()
  create_study("TD", planner = new_planner())
  f1 <- file.path(tempdir(), "toc_a.csv")
  writeLines(c("No.,Kind,Title,Population,Footnotes",
               "T-1,Table,Demographics,Safety Population,"), f1)
  f2 <- file.path(tempdir(), "toc_dup.csv")
  writeLines(c("No.,Kind,Title,Population,Footnotes",
               "T-1,Table,Demographics,Safety Population,",
               "T-1,Table,Again,Safety Population,"), f2)
  f3 <- file.path(tempdir(), "toc_b.xlsx")
  writexl::write_xlsx(list(
    Cover = data.frame(x = "cover"),
    TOC = data.frame(`No.` = c("Study ABC", "No.", "T-2"),
                     Kind = c(NA, "Kind", "Table"),
                     Title = c(NA, "Title", "Vital signs"),
                     check.names = FALSE)), f3, col_names = FALSE)
  up <- function(f) data.frame(name = basename(f), datapath = f, stringsAsFactors = FALSE)
  map <- function(n) {
    # the first three items mapped, the rest (however many) not
    l <- c(list("No.", "Kind", "Title"), as.list(rep("", length(.toc_items) - 3L)))
    names(l) <- paste0("toc", n, "_map_", .toc_items)
    l
  }
  shiny::testServer(server_for("TD"), {
    rv <- session$userData$rv
    session$setInputs(toc_new = 1)
    expect_null(output$toc1_prev$html)
    session$setInputs(toc1_file = up(f1), toc1_skip = 0)
    do.call(session$setInputs, map(1))
    expect_match(output$toc1_do_btn$html, "Taking it in", fixed = TRUE)
    session$setInputs(toc1_do = 1)
    expect_identical(toc_imports(rv$study)$import_id, "TOC001")
    # again: the last one said; the same file noticed
    session$setInputs(toc_new = 2)
    expect_match(output$toc2_prev$html, "TOC001 (toc_a.csv", fixed = TRUE)
    session$setInputs(toc2_file = up(f1), toc2_skip = 0)
    expect_match(output$toc2_same_file$html, "same file as TOC001", fixed = TRUE)
    # two rows of one ID: said with the rows, Take it in off
    session$setInputs(toc_new = 3)
    session$setInputs(toc3_file = up(f2), toc3_skip = 0)
    do.call(session$setInputs, map(3))
    expect_match(output$toc3_changes$html, "T-1 is on more than one row (rows 2, 3)", fixed = TRUE)
    expect_match(output$toc3_do_btn$html, "disabled", fixed = TRUE)
    # an xlsx: its sheet and the rows above its header
    session$setInputs(toc_new = 4)
    session$setInputs(toc4_file = up(f3))
    expect_match(output$toc4_where$html, "Cover", fixed = TRUE)
    session$setInputs(toc4_sheet = "TOC", toc4_skip = 1)
    do.call(session$setInputs, map(4))
    expect_match(output$toc4_changes$html, "New 1", fixed = TRUE)
    # T-1 is not in this TOC: said, and the reports with no change folded
    expect_match(output$toc4_changes$html, "Not in the TOC", fixed = TRUE)
    session$setInputs(toc4_type_1 = "listing", toc4_do = 1)
    expect_identical(report_info(rv$p, "T-2")$type, "listing")
    expect_identical(sheet_rows(rv$p, "titles", "T-2")$center, "Vital signs")
  })
})

test_that("the TOC's headings become the reports' sections; one given here is kept", {
  x <- new_planner()
  sp <- tflspec::tfl_read_toc(toc_file(c(
    ",14.1 Demographics,,,",
    "T-14-1-1,Table,Demographics,Safety Population,",
    "T-14-1-2,Table,Disposition,All Subjects,",
    ",14.3 Safety,,,",
    "T-14-3-1,Table,Overview of TEAEs,Safety Population,")), map = toc_map)
  y <- toc_apply(x, sp, toc_changes(x, sp))
  expect_identical(y$outputs$section,
                   c("14.1 Demographics", "14.1 Demographics", "14.3 Safety"))
  # the picker's sections are the headings, in the TOC's order
  r <- .report_rows(y)
  expect_identical(.section_order(unique(r$section)), c("14.1 Demographics", "14.3 Safety"))
  # a section written here stays when the TOC is taken in again
  y$outputs$section[1L] <- "Mine"
  z <- toc_apply(y, sp, toc_changes(y, sp, last = toc_snapshot(sp, toc_title_offset(y))))
  expect_identical(z$outputs$section[1L], "Mine")
  # saved and read back
  d <- withr_tempdir()
  write_planner(z, d)
  back <- read_planner(file.path(d, c("table_spec.xlsx", "report_spec.xlsx")))
  expect_identical(back$outputs$section, z$outputs$section)
})
