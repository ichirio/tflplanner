# A TOC taken in by the company's rules (#299): rule sets, the sheet and
# header found, the rule set picked, the output id made, phases, shells,
# the Topline batch, the problems.  Every workbook here is made up.

# A workbook of sheets given as rows (character vectors; NA a blank cell),
# written without column names so the rows are the sheet's rows
toc_workbook <- function(sheets, path = tempfile(fileext = ".xlsx")) {
  d <- lapply(sheets, function(rows) {
    w <- max(lengths(rows))
    m <- do.call(rbind, lapply(rows, function(r) c(r, rep(NA, w - length(r)))))
    as.data.frame(m, stringsAsFactors = FALSE)
  })
  writexl::write_xlsx(d, path, col_names = FALSE)
  path
}

# A shell sheet: a few cells
shell <- function(id) list(c(id, "Shell"), c("Column", "N (%)"))

# The two-phase layout: header on row 2 (a heading above), no topline sheet
# unless asked, phase-specific shells for t.14.1.1
two_phase_book <- function(topline = TRUE, path = tempfile("tfl", fileext = ".xlsx")) {
  hdr <- c("Shell Number", "g-", "Topline", "SAP NO.", "Title1", "Title2", "Title3")
  toc <- list(
    c("Study XYZ-001 TFL table of contents"),
    hdr,
    c("t.14.1.1", "both", "Y", "10.1", "Table 14.1.1", "Demographic Characteristics", "Safety Analysis Set"),
    c("t.14.1.2", "ph1", NA, "10.2", "Table 14.1.2", "Disposition", "All Subjects"),
    c("t.14.2.1", "ph2", "Y", NA, "Table 14.2.1", "Adverse Events Overview", "Safety Analysis Set"),
    c("f.14.3.1", NA, NA, "10.1", "Figure 14.3.1", "Mean SBP by Visit", "Safety Analysis Set"),
    c("t.14.4.1", "ph3", NA, "10.4", "Table 14.4.1", "Vital Signs", "Safety Analysis Set"),
    c(NA, "both", NA, NA, NA, "A row with no number", "Safety Analysis Set"))
  sheets <- list(Cover = list(c("Study XYZ-001", "TFL shells"), c("Version", "1.0")),
                 `TOC_table and figure` = toc)
  if (topline) {
    sheets$TOC_Topline <- list(hdr,
      c("t.14.1.1", "both", "Y", "10.1", "Table 14.1.1", "Demographic Characteristics", "Safety Analysis Set"),
      c("t.14.1.2", "ph1", "Y", "10.2", "Table 14.1.2", "Disposition", "All Subjects"))
  }
  for (s in c("t.14.1.1-1", "t.14.1.1-2", "t.14.1.2", "f.14.3.1", "t.14.9.9")) {
    sheets[[s]] <- shell(s)
  }
  toc_workbook(sheets, path)
}

builtin_rules <- function() toc_rules(standards = .builtin_standards())

test_that("the built-in rules: two sets, the defaults filled", {
  r <- builtin_rules()
  expect_identical(names(r), c("standard", "two-phase"))
  s <- toc_rule_set(r, "two-phase")
  expect_s3_class(s, "toc_rule_set")
  expect_identical(s$settings$header_rows, 10L)
  expect_identical(.toc_list_keep(s$settings$title_suffix),
                   c(" - Phase 1 Part", " - Phase 2 Part"))
  expect_identical(.toc_phase_values(s$settings$phase_values),
                   list(both = 1:2, ph1 = 1L, ph2 = 2L))
  expect_true(all(c("phase", "topline", "sap_no", "shell") %in% s$map$item))
  expect_false("phase" %in% r$standard$map$item)
  # a study's override wins
  o <- toc_rule_set(r, "two-phase", overrides = list(number_suffix = "_1 | _2", title_line = 1))
  expect_identical(o$settings$number_suffix, "_1 | _2")
  expect_identical(o$settings$title_line, "1")
  expect_error(toc_rule_set(r, "nope"), "No TOC rule set")
})

test_that("a standards workbook written before the rule sets reads as one set, standard (backwards compatible)", {
  home <- tempfile()
  dir.create(file.path(home, "standards"), recursive = TRUE)
  b <- .builtin_standards()
  old <- b
  old$toc_map <- b$toc_map[b$toc_map$rule_set == "standard", c("item", "columns")]
  old$toc_rules <- NULL
  f <- file.path(home, "standards", "company_standards.xlsx")
  writexl::write_xlsx(old, f)
  s <- read_standards(f)
  expect_true(all(is.na(s$toc_map$rule_set)))
  r <- toc_rules(standards = s)
  expect_identical(names(r), "standard")
  expect_identical(r$standard$settings$sheet_names, .toc_rule_defaults$sheet_names)
  # today's calls keep their results
  m <- toc_map_for(c("No.", "Kind", "Title 1", "Title 2"), s$toc_map)
  expect_identical(m$output_id, "No.")
  expect_identical(m$title, c("Title 1", "Title 2"))
  expect_identical(toc_map_for(c("No.", "Title"), b$toc_map)$output_id, "No.")
  # a toc_rules row with blanks takes the defaults; its suffixes keep their blanks
  s2 <- b
  s2$toc_rules$number_suffix[2] <- NA
  s2$toc_rules$title_suffix[2] <- " (part 1) |  (part 2)"
  f2 <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(s2, f2)
  r2 <- toc_rules(standards = read_standards(f2))
  expect_identical(r2$`two-phase`$settings$number_suffix, ".a | .b")
  expect_identical(.toc_list_keep(r2$`two-phase`$settings$title_suffix),
                   c(" (part 1)", " (part 2)"))
})

test_that("the rule set picked: the layout's own first, a plain layout standard, none when no title", {
  r <- builtin_rules()
  p <- toc_pick_rules(c("Shell Number", "g-", "Topline", "SAP NO.", "Title1", "Title2"), r)
  expect_identical(p$rule_set[1], "two-phase")
  expect_true(p$matched[1] > p$matched[2])
  expect_identical(toc_pick_rules(c("No.", "Kind", "Title", "Population"), r)$rule_set[1],
                   "standard")
  # only a number and a title: both sets equal, the first kept
  p2 <- toc_pick_rules(c("No.", "Title"), r)
  expect_identical(p2$matched[1], p2$matched[2])
  expect_identical(p2$rule_set[1], "standard")
  p3 <- toc_pick_rules(c("Code", "Text"), r)
  expect_false(any(p3$required_ok))
})

test_that("header on row 2, a sheet of any name found by its columns, two TOC-like sheets", {
  f <- two_phase_book()
  src <- toc_source(f)
  expect_identical(src$sheets[1:3], c("Cover", "TOC_table and figure", "TOC_Topline"))
  r <- builtin_rules()
  h <- toc_find_header(src$head[["TOC_table and figure"]], r)
  expect_identical(h$row, 2L)
  expect_true(h$toc)
  fs <- toc_find_sheet(src, r)
  expect_identical(fs$sheet[1], "TOC_table and figure")
  expect_identical(fs$role[1], "toc")
  expect_identical(fs$why[1], "score")
  expect_identical(fs$rows[1], 6L)
  expect_identical(fs$role[fs$sheet == "TOC_Topline"], "topline")
  expect_identical(fs$role[fs$sheet == "t.14.9.9"], "shell")
  expect_true(fs$shell_like[1] >= 3L)
  expect_identical(attr(fs, "kind"), "toc")
  # a cover sheet is not a TOC
  expect_identical(fs$role[fs$sheet == "Cover"], "")
})

test_that("two phases: both gives two reports, suffixes on id, label and title 2; the population line untouched", {
  f <- two_phase_book()
  x <- toc_import_read(f, builtin_rules())
  expect_s3_class(x, "toc_import")
  expect_identical(x$rule_set$name, "two-phase")
  expect_identical(x$detection$sheet, "TOC_table and figure")
  expect_identical(x$detection$header_row, 2L)
  expect_identical(x$detection$topline_sheet, "TOC_Topline")
  expect_identical(x$id_rule, "column")
  r <- x$reports
  expect_identical(r$output_id, c("t.14.1.1.a", "t.14.1.1.b", "t.14.1.2.a", "t.14.2.1.b",
                                  "f.14.3.1"))
  expect_identical(r$phase, c(1L, 2L, 1L, 2L, NA))
  expect_identical(r$label, c("Table 14.1.1.a", "Table 14.1.1.b", "Table 14.1.2.a",
                              "Table 14.2.1.b", "Figure 14.3.1"))
  expect_identical(r$type, c("table", "table", "table", "table", "figure"))
  t1 <- r$titles[[1]]
  expect_identical(t1[!is.na(t1)], c("Demographic Characteristics - Phase 1 Part",
                                     "Safety Analysis Set"))
  expect_identical(r$titles[[2]][!is.na(r$titles[[2]])],
                   c("Demographic Characteristics - Phase 2 Part", "Safety Analysis Set"))
  # a blank phase (phase_blank = none): one report, no suffix
  expect_identical(r$titles[[5]][!is.na(r$titles[[5]])], c("Mean SBP by Visit", "Safety Analysis Set"))
  # ph3 is not in the rules, the row with no number not read: set by hand
  p <- x$problems
  expect_identical(sort(p$rule[p$level == "hand"]), c("TOC04", "TOC05"))
  expect_match(p$message[p$rule == "TOC05"], "phase value ph3 is not in the rules", fixed = TRUE)
  expect_match(p$message[p$rule == "TOC04"], "Row 8 has no report ID", fixed = TRUE)
  expect_false(any(p$level == "error"))
  # an override from the study wins
  y <- toc_import_read(f, builtin_rules(), choices = list(
    overrides = list(number_suffix = "-P1 | -P2", title_suffix = " (Part 1) |  (Part 2)",
                     title_line = "last")))
  expect_identical(y$reports$output_id[1:2], c("t.14.1.1-P1", "t.14.1.1-P2"))
  expect_identical(y$reports$titles[[1]][3], "Safety Analysis Set (Part 1)")
  expect_identical(y$profile$overrides$number_suffix, "-P1 | -P2")
  # phase_blank = all: the blank row gives every phase
  z <- toc_import_read(f, builtin_rules(), choices = list(overrides = list(phase_blank = "all")))
  expect_true(all(c("f.14.3.1.a", "f.14.3.1.b") %in% z$reports$output_id))
})

test_that("shell sheets: the phase's first, else the plain one; none and unlinked said", {
  x <- toc_import_read(two_phase_book(), builtin_rules())
  r <- x$reports
  sh <- stats::setNames(r$shell_sheet, r$output_id)
  expect_identical(sh[["t.14.1.1.a"]], "t.14.1.1-1")
  expect_identical(sh[["t.14.1.1.b"]], "t.14.1.1-2")
  expect_identical(sh[["t.14.1.2.a"]], "t.14.1.2")
  expect_identical(sh[["f.14.3.1"]], "f.14.3.1")
  expect_true(is.na(sh[["t.14.2.1.b"]]))
  p <- x$problems
  expect_identical(p$output_id[p$rule == "TOC08"], "t.14.2.1.b")
  expect_match(p$message[p$rule == "TOC09"], "t.14.9.9", fixed = TRUE)
  expect_identical(unique(r$shell_file[!is.na(r$shell_file)]), basename(x$sources[[1]]$path))
})

test_that("the Topline batch from the flag column; the Topline sheet a cross-check only", {
  x <- toc_import_read(two_phase_book(), builtin_rules())
  b <- x$batches
  expect_identical(unique(b$batch), "Topline")
  expect_identical(b$output_id, c("t.14.1.1.a", "t.14.1.1.b", "t.14.2.1.b"))
  p <- x$problems
  expect_identical(sort(p$output_id[p$rule == "TOC10"]), c("t.14.1.2", "t.14.2.1"))
  expect_true(all(p$level[p$rule == "TOC10"] == "check"))
  # two TOC-like sheets: said once
  expect_identical(sum(p$rule == "TOC11"), 1L)
  # the sheet chosen by the user wins: the Topline sheet as the TOC
  y <- toc_import_read(two_phase_book(), builtin_rules(),
                       choices = list(sheet = c(placeholder = "x")))
  expect_identical(y$detection$sheet, "TOC_table and figure")
  f <- two_phase_book()
  z <- toc_import_read(f, builtin_rules(),
                       choices = list(sheet = stats::setNames("TOC_Topline", basename(f)),
                                      header_row = stats::setNames(1L, basename(f))))
  expect_identical(z$detection$sheet, "TOC_Topline")
  expect_identical(z$detection$how, "chosen")
  expect_false(any(z$problems$rule == "TOC11"))
})

test_that("SAP numbers: blank, and one on two rows (the phases of one row are one)", {
  x <- toc_import_read(two_phase_book(), builtin_rules())
  p <- x$problems
  expect_identical(p$output_id[p$rule == "TOC06"], "t.14.2.1.b")
  expect_identical(sum(p$rule == "TOC07"), 1L)
  expect_match(p$message[p$rule == "TOC07"], "10.1 is on more than one report: t.14.1.1.a, t.14.1.1.b, f.14.3.1",
               fixed = TRUE)
})

test_that("no phase column, no topline column: one report a row, no batch, the skipped items said", {
  f <- toc_workbook(list(TOC = list(
    c("Shell Number", "SAP NO.", "Title1", "Title2"),
    c("t.14.1.1", "10.1", "Table 14.1.1", "Demographics"),
    c("t.14.2.1", "10.2", "Table 14.2.1", "Adverse Events"))))
  x <- toc_import_read(f, builtin_rules())
  expect_identical(x$rule_set$name, "two-phase")
  expect_identical(x$detection$why %||% x$detection$how, "name")
  expect_identical(x$reports$output_id, c("t.14.1.1", "t.14.2.1"))
  expect_true(all(is.na(x$reports$phase)))
  expect_identical(nrow(x$batches), 0L)
  p <- x$problems
  expect_false(any(p$rule == "TOC05"))
  expect_setequal(p$field[p$rule == "TOC12"], c("phase", "topline"))
  # no shells in the workbook: no shell checks
  expect_false(any(p$rule %in% c("TOC08", "TOC09")))
  # the study's planner: no batch written
  x0 <- add_output(new_planner(), "t.14.1.1")
  expect_identical(toc_apply_extras(x0, x), x0)
})

test_that("a listing workbook: a second source, its own header row and shells; an id in both an error", {
  tfl <- two_phase_book(topline = FALSE)
  lst <- toc_workbook(list(
    TOC_listing = list(c(NA), c("Shell Number", "g-", "Title1", "Title2"),
                       c("l.16.2.1", "both", "Listing 16.2.1", "Subject Disposition"),
                       c("l.16.2.2", "ph2", "Listing 16.2.2", "Deaths")),
    `l.16.2.1` = shell("l.16.2.1"), `l.16.2.2-2` = shell("l.16.2.2-2")),
    tempfile("lst", fileext = ".xlsx"))
  x <- toc_import_read(c(tfl, lst), builtin_rules())
  expect_identical(x$detection$sheet, c("TOC_table and figure", "TOC_listing"))
  expect_identical(x$detection$header_row, c(2L, 2L))
  r <- x$reports
  expect_true(all(c("l.16.2.1.a", "l.16.2.1.b", "l.16.2.2.b") %in% r$output_id))
  expect_identical(r$type[r$output_id == "l.16.2.2.b"], "listing")
  expect_identical(r$shell_file[r$output_id == "l.16.2.2.b"], basename(lst))
  expect_identical(r$shell_sheet[r$output_id == "l.16.2.2.b"], "l.16.2.2-2")
  expect_identical(r$shell_sheet[r$output_id == "l.16.2.1.b"], "l.16.2.1")
  expect_false(any(x$problems$level == "error"))
  # an id given in both workbooks
  lst2 <- toc_workbook(list(TOC_listing = list(
    c("Shell Number", "g-", "Title1", "Title2"),
    c("t.14.1.2", "ph1", "Table 14.1.2", "Disposition again"))), tempfile("l2", fileext = ".xlsx"))
  y <- toc_import_read(c(tfl, lst2), builtin_rules())
  p <- y$problems
  expect_identical(p$output_id[p$rule == "TOC02"], "t.14.1.2.a")
  expect_identical(p$level[p$rule == "TOC02"], "error")
  expect_match(p$message[p$rule == "TOC02"], basename(lst2), fixed = TRUE)
})

test_that("a titles-only TOC: the id and the label from Title 1", {
  f <- toc_workbook(list(`TFL List` = list(
    c("Title 1", "Title 2", "Population"),
    c(NA, "14.1 Demographics", NA),
    c("Table 14.1.1", "Demographic Characteristics", "Safety Population"),
    c("Figure 14.2.1: Mean SBP by visit", NA, "Safety Population"),
    c("Listing 16.2.1", "Deaths", "All Subjects"))))
  x <- toc_import_read(f, builtin_rules())
  expect_identical(x$id_rule, "title")
  expect_identical(x$id_candidates$rule, "title")
  r <- x$reports
  expect_identical(r$output_id, c("t.14.1.1", "f.14.2.1", "l.16.2.1"))
  expect_identical(r$label, c("Table 14.1.1", "Figure 14.2.1", "Listing 16.2.1"))
  expect_identical(r$type, c("table", "figure", "listing"))
  # the label is not a title line; what follows it in the cell is
  expect_identical(r$titles[[1]][!is.na(r$titles[[1]])], "Demographic Characteristics")
  expect_identical(r$titles[[2]][!is.na(r$titles[[2]])], "Mean SBP by visit")
  # the heading row is the reports' section, and marked (not a row lost)
  expect_identical(r$section, rep("14.1 Demographics", 3))
  expect_identical(sum(x$rows$heading), 1L)
  sp <- toc_import_spec(x)
  expect_identical(sp$report$output_id, r$output_id)
  expect_identical(sp$titles$center[sp$titles$output_id == "t.14.1.1"],
                   c("Demographic Characteristics", "Safety Population"))
  expect_identical(unname(attr(sp, "labels")), r$label)
  expect_identical(unname(attr(sp, "sections")), rep("14.1 Demographics", 3))
  # the type came from the title, not a type column: marked to check
  expect_identical(attr(sp, "guessed"), r$output_id)
  # another pattern: T-14-1-1
  y <- toc_import_read(f, builtin_rules(),
                       choices = list(overrides = list(id_pattern = "{TYPE}-{number-}")))
  expect_identical(y$reports$output_id, c("T-14-1-1", "F-14-2-1", "L-16-2-1"))
})

test_that("ids without a prefix and a separate type column: normalised proposed, the column on request", {
  f <- toc_workbook(list(Contents = list(
    c("No.", "Type", "Title"),
    c("14.1.1", "Table", "Demographics"),
    c("14.1.1", "Figure", "Age histogram"),
    c("16.2.1", "Listing", "Deaths"))))
  x <- toc_import_read(f, builtin_rules())
  expect_identical(x$detection$how, "score")
  ca <- x$id_candidates
  expect_identical(ca$rule[1], "normalised")
  expect_identical(ca$ok[ca$rule == "column"], 1L)
  expect_identical(ca$ok[1], 3L)
  expect_identical(x$reports$output_id, c("t.14.1.1", "f.14.1.1", "l.16.2.1"))
  expect_identical(x$type_from, "column")
  expect_identical(attr(toc_import_spec(x), "guessed"), character())
  # the id column as it is: 14.1.1 twice, an error
  y <- toc_import_read(f, builtin_rules(), choices = list(id_rule = "column"))
  expect_identical(y$problems$rule[y$problems$level == "error"], "TOC02")
  # type + number with a pattern
  z <- toc_import_read(f, builtin_rules(), choices = list(
    id_rule = "type_number", overrides = list(id_pattern = "{TYPE}-{number-}")))
  expect_identical(z$reports$output_id, c("T-14-1-1", "F-14-1-1", "L-16-2-1"))
  # without a type column the ids have no prefix to add: the column as it is
  g <- toc_workbook(list(TOC = list(c("No.", "Title"), c("14.1.1", "A"), c("14.2.1", "B"))))
  expect_identical(toc_import_read(g, builtin_rules())$reports$output_id, c("14.1.1", "14.2.1"))
})

test_that("a sheet name remembered becomes a name hit; the id rule remembered is used", {
  home <- tempfile()
  dir.create(home)
  f <- toc_workbook(list(Notes = list(c("Some notes")),
                         `TFL List` = list(c("Output No.", "Title"), c("T-14-1-1", "Demographics"))))
  x <- toc_import_read(f, toc_rules(home))
  expect_identical(x$detection$how, "score")
  remember_toc_map(list(note = "Remark"), home = home, rule_set = "standard",
                   sheet = "TFL List", header_row = 12L, id_rule = "normalised")
  r <- toc_rules(home)
  expect_match(r$standard$settings$sheet_names, "TFL List", fixed = TRUE)
  expect_identical(r$standard$settings$header_rows, 12L)
  expect_identical(r$standard$settings$id_rule, "normalised")
  expect_match(r$standard$map$columns[r$standard$map$item == "note"], "Remark", fixed = TRUE)
  # the two-phase set is not touched
  expect_false(grepl("Remark", r$`two-phase`$map$columns[r$`two-phase`$map$item == "note"]))
  y <- toc_import_read(f, r)
  expect_identical(y$detection$how, "name")
  expect_identical(y$id_rule, "normalised")
  expect_identical(y$reports$output_id, "t.14.1.1")
})

test_that("not a TOC: shells only, nothing readable, a document", {
  r <- builtin_rules()
  a <- toc_source(toc_workbook(list(`t.14.1.1` = shell("t.14.1.1"), `t.14.1.2` = shell("x"))))
  expect_identical(attr(toc_find_sheet(a, r), "kind"), "shells")
  b <- toc_source(toc_workbook(list(Notes = list(c("Nothing"), c("here")))))
  expect_identical(attr(toc_find_sheet(b, r), "kind"), "none")
  d <- tempfile(fileext = ".docx")
  writeLines("not a workbook", d)
  x <- toc_import_read(d, r)
  expect_identical(x$detection$kind, "other")
  expect_identical(nrow(x$reports), 0L)
  y <- toc_import_read(a$path, r)
  expect_identical(y$detection$kind, "shells")
  sp <- toc_import_spec(y)
  expect_identical(nrow(sp$report), 0L)
})

test_that("a csv TOC is one sheet; heading rows are sections, rows with no title not read", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title,Population",
               ",14.1 Demographics,,",
               "T-14-1-1,Table,Demographics,Safety Population",
               "T-14-1-2,Table,,Safety Population",
               "F-14-2-1,,Mean SBP | Mean (SE),Safety Population"), f)
  x <- toc_import_read(f, builtin_rules())
  expect_identical(x$detection$how, "name")
  expect_identical(x$rule_set$name, "standard")
  expect_identical(x$reports$output_id, c("T-14-1-1", "F-14-2-1"))
  expect_identical(x$problems$rule[x$problems$level == "hand"], "TOC04")
  expect_match(x$problems$message[x$problems$level == "hand"], "Row 4 (T-14-1-2) has no title",
               fixed = TRUE)
  sp <- toc_import_spec(x)
  expect_identical(sp$titles$center[sp$titles$output_id == "F-14-2-1"],
                   c("Mean SBP", "Mean (SE)", "Safety Population"))
  expect_identical(unname(attr(sp, "sections")), rep("14.1 Demographics", 2))
  expect_identical(attr(sp, "guessed"), "F-14-2-1")
  # in Japanese
  p <- toc_problems(x, "ja")
  expect_false(identical(p$message, x$problems$message))
})

test_that("taken in by the rules: the reports, the Topline batch, the record, the profile, shells.csv", {
  local_home()
  st <- create_study("TR", planner = new_planner())
  f <- two_phase_book()
  res <- toc_import_read(f, toc_rules())
  sp <- toc_import_spec(res)
  ch <- toc_changes(st$planner, sp)
  expect_identical(ch$reports$status, rep("new", 5))
  x <- toc_apply(st$planner, sp, ch)
  x <- toc_apply_extras(x, res)
  expect_identical(batch_sets(x)$Topline, c("t.14.1.1.a", "t.14.1.1.b", "t.14.2.1.b"))
  snap <- .toc_snapshot_rules(sp, toc_title_offset(x), NULL, res)
  rec <- .toc_record(st, f, "XYZ_TFL_shells.xlsx", ch, snap, res)
  log <- toc_imports(st)
  expect_identical(log$rule_set, "two-phase")
  expect_identical(log$sources, "1")
  expect_identical(log$unread, "2")
  expect_true(is.na(log$standards_md5))
  links <- toc_shell_links(st)
  expect_identical(links$sheet[links$output_id == "t.14.1.1.b"], "t.14.1.1-2")
  expect_identical(links$file[1], "TOC001_XYZ_TFL_shells.xlsx")
  expect_identical(links$original[1], "XYZ_TFL_shells.xlsx")
  expect_true(file.exists(file.path(st$path, "input", "toc", "problems.csv")))
  ex <- utils::read.csv(file.path(st$path, "input", "toc", "reports.csv"),
                        colClasses = "character", na.strings = "")
  expect_identical(ex$sap_no[ex$output_id == "t.14.1.2.a"], "10.2")
  prof <- toc_profile(st)
  expect_identical(prof$rule_set, "two-phase")
  expect_identical(prof$sources[[1]]$toc_sheet, "TOC_table and figure")
  expect_identical(prof$sources[[1]]$header_row, 2L)
  # the next time: the profile pre-fills; the flag dropped takes the batch off
  last <- toc_last(st)
  expect_identical(unlist(last[["t.14.1.1.a"]]$batches), "Topline")
  x <- set_batch(x, "Final", x$outputs$output_id)
  # the profile names the sheet the file still has (the file's own name)
  prof$sources[[1]]$file <- basename(f)
  res2 <- toc_import_read(f, toc_rules(), profile = prof)
  expect_identical(res2$detection$how, "profile")
  res2$batches <- res2$batches[res2$batches$output_id != "t.14.2.1.b", , drop = FALSE]
  y <- toc_apply_extras(x, res2, last)
  expect_identical(batch_sets(y)$Topline, c("t.14.1.1.a", "t.14.1.1.b"))
  expect_identical(length(batch_sets(y)$Final), 5L)
  # a sheet renamed: found again, said
  prof$sources[[1]]$toc_sheet <- "Gone"
  res3 <- toc_import_read(f, toc_rules(), profile = prof)
  expect_identical(res3$detection$sheet, "TOC_table and figure")
  expect_match(res3$notes$message, "not in the file any more", fixed = TRUE)
  # two sources: each copied
  lst <- toc_workbook(list(TOC = list(c("Shell Number", "Title1", "Title2"),
                                      c("l.16.1", "Listing 16.1", "Deaths"))))
  res4 <- toc_import_read(c(f, lst), toc_rules())
  rec4 <- .toc_record(st, c(f, lst), c("a.xlsx", "b.xlsx"), toc_changes(y, toc_import_spec(res4)),
                      snap, res4)
  expect_identical(rec4$file, "TOC002_a.xlsx | TOC002_b.xlsx")
  expect_identical(rec4$sources, "2")
  expect_true(all(file.exists(file.path(st$path, "input", "toc", c("TOC002_a.xlsx", "TOC002_b.xlsx")))))
})

test_that("the dialog: two files found, a sheet switched, the id rule, Take it in with the Topline batch, again as the last time", {
  local_home()
  create_study("TQ", planner = new_planner())
  tfl <- two_phase_book(path = file.path(tempdir(), "XYZ_TFL_shells.xlsx"))
  lst <- toc_workbook(list(
    TOC_listing = list(c(NA), c("Shell Number", "g-", "Title1", "Title2"),
                       c("l.16.2.1", "both", "Listing 16.2.1", "Subject Disposition")),
    `l.16.2.1` = shell("l.16.2.1")), file.path(tempdir(), "XYZ_Listing_shells.xlsx"))
  up <- function(f) data.frame(name = basename(f), datapath = f, stringsAsFactors = FALSE)
  shiny::testServer(server_for("TQ"), {
    rv <- session$userData$rv
    session$setInputs(toc_new = 1)
    session$setInputs(toc1_file = rbind(up(tfl), up(lst)))
    h <- output$toc1_where$html
    expect_match(h, "TOC_table and figure", fixed = TRUE)
    expect_match(h, "header on row 2", fixed = TRUE)
    expect_match(h, "most column names matched", fixed = TRUE)
    expect_match(h, "Topline sheet", fixed = TRUE)
    expect_match(h, "toc1_sheet_2", fixed = TRUE)
    expect_match(h, "Two phases", fixed = TRUE)
    expect_match(h, "the ID column as it is", fixed = TRUE)
    # the preview: the phase reports, the shells, the batch, rows not read
    p <- output$toc1_preview$html
    expect_match(p, "t.14.1.1.a", fixed = TRUE)
    expect_match(p, "t.14.1.1-1", fixed = TRUE)
    expect_match(p, "Topline batch: 3 reports", fixed = TRUE)
    expect_match(p, "Rows not read (2)", fixed = TRUE)
    expect_match(output$toc1_map$html, "Phase suffixes for this study", fixed = TRUE)
    expect_match(output$toc1_changes$html, "New 7", fixed = TRUE)
    # the Topline sheet chosen as the TOC: fewer reports; back again
    session$setInputs(toc1_sheet = "TOC_Topline", toc1_skip = 0)
    expect_match(output$toc1_preview$html, "5 reports from 3 rows", fixed = TRUE)
    session$setInputs(toc1_sheet = "TOC_table and figure", toc1_skip = 1)
    expect_match(output$toc1_preview$html, "7 reports from 7 rows", fixed = TRUE)
    # the second file's inputs, as the browser sends them
    session$setInputs(toc1_sheet_2 = "TOC_listing", toc1_skip_2 = 1)
    expect_match(output$toc1_preview$html, "7 reports from 7 rows", fixed = TRUE)
    # the study's own suffixes
    session$setInputs(toc1_ov_number_suffix = "_p1 | _p2")
    expect_match(output$toc1_preview$html, "t.14.1.1_p1", fixed = TRUE)
    session$setInputs(toc1_ov_number_suffix = ".a | .b")
    # the id column as it is vs normalised: an error disables Take it in
    session$setInputs(toc1_id_rule = "title")
    expect_match(output$toc1_preview$html, "t.14.1.1.a", fixed = TRUE)
    session$setInputs(toc1_id_rule = "column", toc1_remember = TRUE)
    session$setInputs(toc1_type_1 = "table")
    session$setInputs(toc1_do = 1)
    expect_identical(length(rv$p$outputs$output_id), 7L)
    expect_identical(batch_sets(rv$p)$Topline, c("t.14.1.1.a", "t.14.1.1.b", "t.14.2.1.b"))
    res <- output$toc1_result$html
    expect_match(res, "TOC001", fixed = TRUE)
    expect_match(res, "Topline batch: 3 reports", fixed = TRUE)
    expect_match(res, "shells linked", fixed = TRUE)
    expect_identical(toc_imports(rv$study)$sources, "2")
    expect_identical(nrow(toc_shell_links(rv$study)), 6L)
    expect_identical(toc_profile(rv$study)$rule_set, "two-phase")
    expect_identical(open_study(rv$study$path)$planner$outputs$batches[1], "Topline")
    # remembered: the sheet's name is a candidate of the two-phase set now
    expect_match(toc_rules()$`two-phase`$settings$sheet_names, "TOC_table and figure", fixed = TRUE)
    # again: found by name, nothing new
    session$setInputs(toc_new = 2)
    session$setInputs(toc2_file = rbind(up(tfl), up(lst)))
    expect_match(output$toc2_where$html, "as the last time", fixed = TRUE)
    expect_match(output$toc2_changes$html, "New 0", fixed = TRUE)
    # an id twice: Take it in off
    dup <- toc_workbook(list(TOC = list(c("Shell Number", "Title1", "Title2"),
                                        c("t.1", "Table 1", "A"), c("t.1", "Table 1", "B"))),
                        file.path(tempdir(), "dup.xlsx"))
    session$setInputs(toc_new = 3)
    session$setInputs(toc3_file = up(dup))
    expect_match(output$toc3_changes$html, "t.1 is on more than one row (rows 2, 3)", fixed = TRUE)
    expect_match(output$toc3_do_btn$html, "disabled", fixed = TRUE)
  })
})

test_that("a second source with no SAP column says nothing of SAP numbers", {
  tfl <- two_phase_book(topline = FALSE)
  lis <- toc_workbook(list(TOC_listing = list(
    c("ABC listings"),
    c("Output", "Title 1", "Title 2", "Population"),
    c("l.16.2.1", "Listing 16.2.1", "Subject Disposition", "All Subjects"))))
  x <- toc_import_read(c(tfl, lis), builtin_rules())
  p <- x$problems
  expect_false(any(p$rule == "TOC06" & p$output_id %in% "l.16.2.1"))
  # the TFL TOC's own blank SAP number is still said
  expect_true(any(p$rule == "TOC06" & grepl("^t[.]14[.]2[.]1", p$output_id)))
})

test_that("a blank in the dialog's map leaves out the first source's column, not another's", {
  tfl <- two_phase_book(topline = FALSE)
  lis <- toc_workbook(list(TOC_listing = list(
    c("ABC listings"),
    c("Output", "Title 1", "Title 2", "Population"),
    c("l.16.2.1", "Listing 16.2.1", "Subject Disposition", "All Subjects"))))
  # the TFL TOC has no population column: the dialog sends it blank
  x <- toc_import_read(c(tfl, lis), builtin_rules(), choices = list(map = list(population = "")))
  r <- x$reports
  expect_identical(r$population[r$output_id == "l.16.2.1"], "All Subjects")
  # a blank for a column the first source has does leave it out there
  y <- toc_import_read(c(tfl, lis), builtin_rules(), choices = list(map = list(sap_no = "")))
  expect_true(all(is.na(y$reports$sap_no[startsWith(y$reports$output_id, "t.")])))
})

test_that("an item only a second source has is still there for the dialog", {
  tfl <- two_phase_book(topline = FALSE)
  lis <- toc_workbook(list(TOC_listing = list(
    c("ABC listings"),
    c("Output", "Title 1", "Title 2", "Population"),
    c("l.16.2.1", "Listing 16.2.1", "Subject Disposition", "All Subjects"))))
  x <- toc_import_read(c(tfl, lis), builtin_rules())
  # the first source's map has no population: the listing's has
  expect_null(x$map$population)
  expect_true(.toc_has_item(x, "population"))
  expect_false(.toc_has_item(x, "datasets"))
  expect_true(.toc_has_item(toc_import_read(tfl, builtin_rules()), "phase"))
})

test_that("the last time's ID rule gives way when it makes fewer unique IDs here", {
  f <- toc_workbook(list(Contents = list(
    c("No.", "Type", "Title", "Analysis Set"),
    c("14.1.1", "Table", "Demographic Characteristics", "Safety Analysis Set"),
    c("14.2.1", "Table", "Primary Endpoint", "Full Analysis Set"),
    c("14.2.1", "Figure", "Kaplan-Meier Plot", "Full Analysis Set"))))
  x <- toc_import_read(f, builtin_rules(), profile = list(id_rule = "column"))
  expect_false(identical(x$id_rule, "column"))
  expect_false(anyDuplicated(x$reports$output_id) > 0L)
  expect_match(x$notes$message, "as the last time (column)", fixed = TRUE)
  # chosen here, it is kept, duplicates and all
  y <- toc_import_read(f, builtin_rules(), profile = list(id_rule = "column"),
                       choices = list(id_rule = "column"))
  expect_identical(y$id_rule, "column")
  # as good as the best: kept, nothing said
  g <- two_phase_book(topline = FALSE)
  z <- toc_import_read(g, builtin_rules(), profile = list(id_rule = "column"))
  expect_identical(z$id_rule, "column")
  expect_false(any(grepl("as the last time", z$notes$message, fixed = TRUE)))
})
