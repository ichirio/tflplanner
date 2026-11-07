test_that("a part's items read back from the YAML the card shows", {
  items <- .part_quickstart("ATPT")
  txt <- .part_items_yaml(items)
  expect_match(txt, "table_plan")
  back <- .part_items_parse(txt)
  expect_identical(vapply(back, `[[`, "", "fn"), c("normalize_ard", "table_plan", "plan_cells"))
  expect_identical(back[[2L]]$args$cols, "ATPT")
  expect_identical(.part_items_parse("  "), list())
})

test_that("a new part starts as the ARD's summary statistics by the table's columns", {
  a <- .plan_starter_args("plan_append", list(key = "ATPT"))
  expect_identical(a$name, "summary")
  expect_identical(a$data, list(source = "ard"))
  expect_identical(a$content[[2L]]$args$cols, "ATPT")
})

test_that("the card's fields make the item's arguments", {
  it <- list(path = "plan_append", args = list())
  input <- list(c__name = "summary", c__absent = "skip", c__source = "dataset", c__dataset = "",
                c__content = .part_items_yaml(.part_quickstart()))
  expect_error(.part_args_read(input, "c", it), "dataset")
  input$c__dataset <- "ADPC"
  a <- .part_args_read(input, "c", it)
  expect_identical(a$absent, "skip")
  expect_identical(a$data, list(source = "dataset", dataset = "ADPC"))
  expect_length(a$content, 3L)
})

test_that("a part's builder id names its report and its part", {
  expect_identical(.part_of(.part_id("T-PK-1", "summary")), list(id = "T-PK-1", name = "summary"))
  expect_null(.part_of("T-PK-1"))
  expect_null(.part_of(NA_character_))
  expect_identical(.part_parent("T-PK-1::summary"), "T-PK-1")
  expect_identical(.part_parent("T-PK-1"), "T-PK-1")
  content <- list(tflspec::tfl_item("table_plan", list(cols = "ATPT")),
                  tflspec::tfl_item("plan_append", list(name = "summary", content = .part_quickstart())),
                  tflspec::tfl_item("plan_append", list(content = .part_quickstart())))
  expect_identical(.part_names(content), c("summary", "part3"))
  expect_identical(.part_at(content, "part3"), 3L)
  expect_true(is.na(.part_at(content, "none")))
})

# ---- the cards on a part (#506) ---------------------------------------------
# The sample study's T-14-1-1 with a part under it ("summary": the ARD's
# summary statistics by ARM, the table's own columns being TRT01A), made once.
parts_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      home <- tempfile("tflplanner-parts-home")
      dir.create(home)
      old <- options(tflplanner.home = home)
      on.exit(options(old), add = TRUE)
      suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
      suppressMessages(create_sample_study(run = FALSE, home = home))
      s <- suppressMessages(open_study("SAMPLE-01", home = home))
      p <- .builder_item_write(s$planner, "T-14-1-1", "plan_append",
                               .plan_starter_args("plan_append", list(key = "ARM")), 1L)
      cache <<- list(p = p, id = "T-14-1-1", pid = "T-14-1-1::summary", home = home)
    }
    cache
  }
})

# the table's own items (all but the part) and the part's, from the spec
own_items <- function(p, id) {
  own <- .spec_items(p, id)$own
  fns <- vapply(own, function(it) it$fn, "")
  list(table = own[fns != "plan_append"], part = own[fns == "plan_append"][[1L]])
}

test_that("a part's cards read the part's own items, not the table's sheets", {
  skip_on_cran()
  f <- parts_fixture()
  p <- f$p
  expect_identical(.part_names(.spec_items(p, f$id)$own), "summary")
  it <- builder_items(p, f$pid)
  expect_identical(vapply(it, function(i) i$fn, ""), c("normalize_ard", "table_plan", "plan_cells"))
  # the rows and the cells are the part's: ARM, not the table's TRT01A
  expect_identical(sheet_rows(p, "tables", f$id)$cols[1L], "TRT01A")
  expect_identical(sheet_rows(p, "tables", f$pid)$cols, "ARM")
  expect_identical(sheet_rows(p, "cells", f$pid)$row, c("n", "Mean", "SD", "Median", "Min, Max"))
  expect_identical(sheet_rows(p, "cells", f$pid)$output_id, rep(f$pid, 5L))
  expect_identical(nrow(sheet_rows(p, "variables", f$pid)), 0L)
  # no layer under it: nothing inherited, nothing written to the sheets
  expect_identical(nrow(inherited_rows(p, "tables", f$pid)), 0L)
  expect_identical(nrow(.rows_for(p, "cells", f$pid)), 5L)
  expect_error(set_sheet_rows(p, "tables", f$pid, data.frame(cols = "A")), "no rows in the sheets")
  st <- builder_read(p, f$pid)
  expect_identical(st$key, "ARM")
  expect_identical(st$rows, c("n", "Mean", "SD", "Median", "Min, Max"))
  expect_identical(unname(st$templates[["n"]]), "{N:.0f}")
  # the table's cards are unchanged by the part's being there
  expect_identical(builder_read(p, f$id)$key, "TRT01A")
})

test_that("an edit of a part's cards changes the part's items only", {
  skip_on_cran()
  f <- parts_fixture()
  p <- f$p
  before <- own_items(p, f$id)
  sheets0 <- p$sheets
  st <- builder_read(p, f$pid)
  st2 <- st
  st2$rows <- c("n", "Mean")
  st2$templates <- st2$templates[c("n", "Mean")]
  p2 <- builder_write(p, f$pid, st2, was = st)
  after <- own_items(p2, f$id)
  # the table's items, byte for byte, and the part's other arguments
  expect_identical(after$table, before$table)
  expect_identical(after$part$args[c("name", "data")], before$part$args[c("name", "data")])
  # ... the part's own rows are what changed
  cells <- Filter(function(i) identical(i$fn, "plan_cells"), after$part$args$content)[[1L]]
  expect_identical(names(cells$args$continuous), c("n", "Mean"))
  # a card of another verb: written into the part, not the table
  p3 <- .builder_item_write(p2, f$pid, "plan_labels", list(AGE = "Age (years)"))
  a3 <- own_items(p3, f$id)
  expect_identical(a3$table, before$table)
  pl <- Filter(function(i) identical(i$fn, "plan_labels"), a3$part$args$content)
  expect_length(pl, 1L)
  expect_identical(pl[[1L]]$args$AGE, "Age (years)")
  # ... and taken away again
  p4 <- .builder_item_revert(p3, f$pid, "plan_labels")
  expect_identical(own_items(p4, f$id)$part$args$content, a3$part$args$content[vapply(
    a3$part$args$content, function(i) !identical(i$fn, "plan_labels"), NA)])
  # the sheets: only the table row's mirror of the spec (its content_more,
  # where the part is written) changes; nothing else of the table's rows
  for (s in names(sheets0)) {
    a <- sheets0[[s]]
    b <- p3$sheets[[s]]
    if (identical(s, "tables")) a$content_more <- b$content_more <- NULL
    expect_identical(b, a, info = s)
  }
  expect_false(identical(p3$sheets$tables$content_more, sheets0$tables$content_more))
})

test_that("a part is edited in cards and in its YAML field, each showing in the other", {
  skip_on_cran()
  f <- parts_fixture()
  p <- f$p
  part_args <- function(p) own_items(p, f$id)$part$args
  # a card's edit shows in the YAML field
  st <- builder_read(p, f$pid)
  st2 <- st
  st2$rows <- c("n", "Mean")
  st2$templates <- st2$templates[c("n", "Mean")]
  p2 <- builder_write(p, f$pid, st2, was = st)
  yaml <- .part_items_yaml(part_args(p2)$content)
  expect_match(yaml, "Mean:", fixed = TRUE)
  expect_no_match(yaml, "Median", fixed = TRUE)
  # the YAML field's Apply (its text changed) shows in the cards
  yaml2 <- sub("Mean:", "SD:", yaml, fixed = TRUE)
  input <- list(c__name = "summary", c__absent = "error", c__source = "ard", c__dataset = "",
                c__content = yaml2)
  args <- .part_args_read(input, "c", list(path = "plan_append"))
  p3 <- .builder_item_write(p2, f$id, "plan_append", args, 1L)
  expect_identical(builder_read(p3, f$pid)$rows, c("n", "SD"))
  expect_identical(.part_items_yaml(part_args(p3)$content), yaml2)
  # saved and read again, the part is the same
  home <- f$home
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  s$planner <- p3
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  s <- suppressMessages(save_study(s, home = home))
  s2 <- suppressMessages(open_study("SAMPLE-01", home = home))
  expect_identical(builder_read(s2$planner, f$pid)$rows, c("n", "SD"))
  expect_identical(.part_items_yaml(own_items(s2$planner, f$id)$part$args$content), yaml2)
})

test_that("only a part of the report's own, from its ARD, gets the cards", {
  skip_on_cran()
  mk <- function(from = "own", source = "ard", name = "summary") list(
    fn = "plan_append", from = from,
    args = list(name = name, data = if (identical(source, "dataset")) list(source = "dataset", dataset = "ADPC") else
                  list(source = "ard"), content = .part_quickstart()))
  expect_identical(.part_modes(list(mk()))[[1L]], list(name = "summary", mode = "cards"))
  expect_identical(.part_modes(list(mk(source = "dataset")))[[1L]]$mode, "dataset")
  expect_identical(.part_modes(list(mk(from = "defaults")))[[1L]]$mode, "defaults")
  expect_identical(vapply(.part_modes(list(mk(), mk(source = "dataset", name = "b"))), `[[`, "", "mode"),
                   c("cards", "dataset"))
  # the scope bar: a button for each part the cards can edit, none else
  bar <- as.character(.part_scope_bar(.part_modes(list(mk(), mk(source = "dataset", name = "listing"))), "", "en"))
  expect_match(bar, "Editing:", fixed = TRUE)
  expect_match(bar, ">This table<", fixed = TRUE)
  expect_match(bar, "Part: summary (plan_append)", fixed = TRUE)
  expect_no_match(bar, "listing", fixed = TRUE)
  expect_null(.part_scope_bar(.part_modes(list(mk(source = "dataset"))), "", "en"))
  expect_null(.part_scope_bar(.part_modes(list(mk(from = "defaults"))), "", "en"))
  expect_null(.part_scope_bar(list(), "", "en"))
  # the part's card: the button for the one the cards can edit; a line, no
  # button, for the others -- under the YAML field and the buttons
  body <- function(mode) as.character(.part_body("c", mk(), lang = "en", mode = mode))
  expect_match(body(list(name = "summary", mode = "cards")), ">Open in cards<", fixed = TRUE)
  expect_match(body(list(name = "summary", mode = "cards")), "data-rp-scope-jump", fixed = TRUE)
  ds <- body(list(name = "summary", mode = "dataset"))
  expect_no_match(ds, "Open in cards", fixed = TRUE)
  expect_match(ds, "No cards for a dataset part (the cards read the ARD): edit its items in the YAML above.",
               fixed = TRUE)
  expect_lt(regexpr("c__content", ds, fixed = TRUE), regexpr("No cards for a dataset part", ds, fixed = TRUE))
  df <- body(list(name = "summary", mode = "defaults"))
  expect_no_match(df, "Open in cards", fixed = TRUE)
  expect_match(df, "From the study defaults: no cards.", fixed = TRUE)
  expect_match(df, "spec/defaults.yml", fixed = TRUE)
  expect_no_match(body(NULL), "Open in cards", fixed = TRUE)
  # all these words are in Japanese too
  for (z in c("Editing:", "Part: %s (plan_append)", "Open in cards", "This part",
              "The part is on page %s.", "The part's rows print under the table: see the later pages.",
              "The table's sheets; the part is in its plan_append content.",
              "These cards edit the part \"%s\" (its rows print under the table).",
              "The part's name, where its rows come from and its YAML are in its card under this table.",
              "No cards for a dataset part (the cards read the ARD): edit its items in the YAML above.")) {
    expect_false(identical(tr(z, "ja"), z), info = z)
  }
  expect_identical(tr("Open in cards", "ja"), "カードで編集")
  expect_identical(tr("This part", "ja"), "この部分")
})

test_that("in part scope the bar says what the cards edit; the table scope says nothing more", {
  modes <- list(list(name = "summary", mode = "cards"))
  tb <- as.character(.part_scope_bar(modes, "", "en"))
  expect_no_match(tb, "These cards edit", fixed = TRUE)
  pt <- as.character(.part_scope_bar(modes, "summary", "en"))
  expect_match(pt, "These cards edit the part \"summary\" (its rows print under the table).",
               fixed = TRUE)
  expect_match(pt, "its card under this table", fixed = TRUE)
  expect_match(pt, 'aria-pressed="true"', fixed = TRUE)
  ja <- as.character(.part_scope_bar(modes, "summary", "ja"))
  expect_match(ja, "「summary」", fixed = TRUE)
  expect_match(ja, "「Table の下に足す部分」", fixed = TRUE)
})

test_that("a part's cards say This part, and only table_plan is required", {
  mk <- function(fn) list(fn = fn, args = list(), path = fn, present = TRUE, from = "own",
                          own_args = list(a = 1), required = fn %in% .plan_required)
  its <- .part_items_mark(list(mk("normalize_ard"), mk("table_plan"), mk("plan_cells"), mk("plan_labels")))
  expect_identical(vapply(its, function(it) it$required, NA), c(FALSE, TRUE, FALSE, FALSE))
  expect_true(all(vapply(its, function(it) identical(it$own_word, "This part"), NA)))
  h <- function(items, lang = "en") paste(vapply(.table_cards(items, lang = lang), as.character, ""),
                                          collapse = "\n")
  part <- h(its)
  expect_match(part, ">This part<", fixed = TRUE)
  expect_no_match(part, ">This table<", fixed = TRUE)
  expect_equal(lengths(regmatches(part, gregexpr(">Required<", part, fixed = TRUE))), 1L)
  expect_match(h(its, "ja"), "この部分", fixed = TRUE)
  # the table's own cards are as they were
  tbl <- h(lapply(its, function(it) {
    it$own_word <- NULL
    it$required <- it$fn %in% .plan_required
    it
  }))
  expect_match(tbl, ">This table<", fixed = TRUE)
  expect_no_match(tbl, ">This part<", fixed = TRUE)
  expect_equal(lengths(regmatches(tbl, gregexpr(">Required<", tbl, fixed = TRUE))), 3L)
  # the mark sent after a write, and a field's mark
  expect_match(as.character(.card_mark(its[[2L]], "c", "en")), ">This part<", fixed = TRUE)
  it <- list(fn = "plan_sort", args = list(keys = c(".overall", "label"), keep = ".overall"),
             own_args = list(keep = ".overall"), path = "plan_sort", present = TRUE, from = "own",
             own_word = "This part")
  fo <- as.character(.special_more("c", "plan_sort", it))
  expect_match(fo, ">This part<", fixed = TRUE)
})

test_that("a part has no cards for what rtfreporter lets only the table say", {
  skip_if_not_installed("rtfreporter")
  # ?plan_append: the pages, column header, widths and style are the first plan's
  gone <- c("plan_paginate_rows", "plan_paginate_group", "plan_paginate_cols", "plan_col_header",
            "plan_columns", "plan_style", "plan_append", "plan_total", "plan_digits")
  expect_true(all(gone %in% .part_skip))
  # ... and these a part may carry
  expect_false(any(c("plan_cells", "plan_labels", "plan_levels", "plan_hide", "plan_cell_style",
                     "plan_blanks", "plan_stub", "plan_sort", "plan_filter") %in% .part_skip))
  d <- data.frame(g = rep(c("A", "B"), each = 2), v = c("x", "y", "x", "y"), s = 1:4)
  parent <- rtfreporter::table_plan(d, cols = "g", rows = c(v = "v"), value = "s")
  ok <- function(f) tryCatch({
    rtfreporter::plan_append(parent, f(parent), name = "p")
    TRUE
  }, error = function(e) FALSE)
  expect_false(ok(function(q) rtfreporter::plan_paginate_rows(q, max_rows = 10)))
  expect_false(ok(function(q) rtfreporter::plan_columns(q, widths = c(v = 20))))
  expect_false(ok(function(q) rtfreporter::plan_style(q, align_count_pct = TRUE)))
  expect_false(ok(function(q) rtfreporter::plan_col_header(q, header = list(list(line = "1", cols = "v")))))
  expect_true(ok(function(q) rtfreporter::plan_hide(q, cols = "g")))
  expect_true(ok(function(q) rtfreporter::plan_cell_style(q, where = ~ s > 1, bold = TRUE)))
})

test_that("the part's page in the preview is found by its first row", {
  full <- data.frame(row_label = c("a", "b", "Summary", "n", "Mean"), X = c("1", "2", "", "80", "60"),
                     .part = c("part1", "part1", "summary", "summary", "summary"),
                     check.names = FALSE, stringsAsFactors = FALSE)
  first <- .part_first_rows(full)
  expect_identical(names(first), "summary")
  expect_identical(first$summary, c("Summary", ""))
  expect_null(.part_first_rows(full[, c("row_label", "X")]))
  pg <- function(label, x) structure(list(data = data.frame(row_label = label, X = x, check.names = FALSE,
                                                           stringsAsFactors = FALSE)), class = "rtftable")
  pages <- structure(list(pg(c("a", "b"), c("1", "2")), pg(c("c", "  Summary", "n"), c("9", "", "80"))),
                     part_first = first)
  expect_identical(.part_start_page(pages, "summary"), 2L)
  expect_true(is.na(.part_start_page(pages, "other")))
  expect_true(is.na(.part_start_page(structure(list(pg("a", "1")), part_first = first), "summary")))
  note <- function(pages, lang = "en") as.character(.part_page_note(pages, "summary", lang))
  expect_match(note(pages), "The part is on page 2.", fixed = TRUE)
  expect_match(note(pages), 'data-part-page="2"', fixed = TRUE)
  expect_match(note(pages, "ja"),
               "部分は 2 ページ目にあります。", fixed = TRUE)
  # not found: the line that says where to look, and nothing to scroll to
  none <- note(structure(list(pg("a", "1")), part_first = first))
  expect_match(none, "see the later pages", fixed = TRUE)
  expect_no_match(none, "data-part-page", fixed = TRUE)
})

test_that("a dataset part and a study-defaults part are not written by the cards", {
  skip_on_cran()
  f <- parts_fixture()
  p <- f$p
  # a part of the study's defaults: not the report's own, so none to write
  q <- p
  std <- tflspec::tfl_item("plan_append", list(name = "std", content = .part_quickstart()))
  std$from <- "defaults"
  q$spec$study$defaults$content <- c(q$spec$study$defaults$content %||% list(), list(std))
  other <- "T-14-1-2"
  items <- .spec_items(q, other)$items
  expect_identical(vapply(.part_modes(items), `[[`, "", "mode"), "defaults")
  expect_null(.spec_items(q, .part_id(other, "std")))
  expect_error(.spec_set_part_items(q, .part_id(other, "part2"), list(list(fn = "plan_cells", args = list()))),
               "no part of its own")
  # a part whose rows come from a dataset: written as before, by its YAML card
  a <- .plan_starter_args("plan_append", list(key = "ARM"))
  a$data <- list(source = "dataset", dataset = "ADSL")
  a$name <- "records"
  d <- .builder_item_write(p, "T-14-1-2", "plan_append", a, 1L)
  modes <- .part_modes(.spec_items(d, "T-14-1-2")$items)
  expect_identical(vapply(modes, `[[`, "", "mode"), "dataset")
  expect_null(.part_scope_bar(modes, "", "en"))
})

test_that("a part's id never reaches a program, a file or the table's sheets", {
  skip_on_cran()
  f <- parts_fixture()
  p <- f$p
  expect_error(report_info(p, f$pid), "no program or file")
  expect_error(program_code(p, f$pid), "no program or file")
  expect_error(set_total_column(p, f$pid, "Total"), "no program or file")
  expect_error(.meta_file(list(meta = list(study_id = "S")), f$pid), "no program or file")
  # the preview is the table's, parts and all
  expect_true(.preview_by_content(p, f$id))
  expect_false(.preview_by_content(p, f$pid))
  # a part that is not there has no items: not the sheets' bridge
  expect_identical(builder_items(p, .part_id(f$id, "nothing")), .builder_items_ard_first(list()))
})

# a sample study whose T-14-1-2 has a part read from ADSL (its data code
# calls order_arms(), which programs/study_helpers.R defines)
.part_study <- function(env = parent.frame()) {
  home <- local_home(env)
  s <- suppressMessages(create_sample_study(run = FALSE))
  args <- list(name = "demog", absent = "skip",
               data = list(source = "dataset", dataset = "ADSL"),
               content = .part_items_parse("- fn: table_plan\n  args:\n    cols: SEX\n"))
  s$planner <- .builder_item_write(s$planner, "T-14-1-2", "plan_append", args, 1L)
  s
}

test_that("the preview of a part from a dataset runs the data code with the study's helpers", {
  skip_on_cran()
  skip_if_not_installed("cards")
  s <- .part_study()
  expect_true(any(grepl("order_arms()", .table_dataset_lines(s$planner, "T-14-1-2"), fixed = TRUE)))
  wd <- getwd()
  r <- .preview_records(s$planner, "T-14-1-2", s$path)
  expect_gt(length(r), 0L)
  expect_true(all(vapply(r, is.data.frame, NA)))
  expect_gt(nrow(r[[1L]]), 0L)
  # the app's own session is as it was
  expect_identical(getwd(), wd)
  expect_false(exists("order_arms", envir = globalenv(), inherits = FALSE))
  expect_false(exists("adsl", envir = globalenv(), inherits = FALSE))
})

test_that("the preview reads the study's own study_helpers.R, and says when it stops", {
  skip_on_cran()
  skip_if_not_installed("cards")
  s <- .part_study()
  f <- file.path(s$path, "programs", "study_helpers.R")
  writeLines("order_arms <- function(data) { data$MARKED <- 1L; data }", f)
  r <- .preview_records(s$planner, "T-14-1-2", s$path)
  expect_true(all(vapply(r, function(d) "MARKED" %in% names(d), NA)))
  writeLines("stop('boom')", f)
  expect_error(.preview_records(s$planner, "T-14-1-2", s$path), "study_helpers.R stopped: boom")
  # no helpers file: tflspec's functions stand
  unlink(f)
  r <- .preview_records(s$planner, "T-14-1-2", s$path)
  expect_gt(nrow(r[[1L]]), 0L)
})

test_that("a dataset part with items for an ARD is found, and the message names it", {
  part <- function(name, source, fns) {
    list(fn = "plan_append", args = list(
      name = name, data = list(source = source),
      content = lapply(fns, function(f) list(fn = f, args = list()))))
  }
  content <- list(list(fn = "table_plan", args = list()),
                  part("summary", "dataset", c("normalize_ard", "table_plan")),
                  part("by_ard", "ard", c("normalize_ard", "table_plan")),
                  part("plain", "dataset", "table_plan"))
  expect_identical(.dataset_parts_with_ard_items(content), 2L)
  expect_identical(.dataset_parts_with_ard_items(NULL), integer())
  msg <- .dataset_part_ard_items_note("summary")
  expect_match(msg, "The part summary reads a dataset, but its items are for an ARD", fixed = TRUE)
  expect_match(msg, "normalize_ard", fixed = TRUE)
  expect_match(msg, "ARD", fixed = TRUE)
  expect_match(.dataset_part_ard_items_note(c("a", "b")), "part a, b ", fixed = TRUE)
  expect_match(.dataset_part_ard_items_note(NA_character_), "(no name)", fixed = TRUE)
})
