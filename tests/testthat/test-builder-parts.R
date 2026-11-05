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
  expect_match(bar, "Part: summary", fixed = TRUE)
  expect_no_match(bar, "listing", fixed = TRUE)
  expect_null(.part_scope_bar(.part_modes(list(mk(source = "dataset"))), "", "en"))
  expect_null(.part_scope_bar(.part_modes(list(mk(from = "defaults"))), "", "en"))
  expect_null(.part_scope_bar(list(), "", "en"))
  # the part's card: the button for the one the cards can edit; a line, no
  # button, for the others
  body <- function(mode) as.character(.part_body("c", mk(), lang = "en", mode = mode))
  expect_match(body(list(name = "summary", mode = "cards")), "Edit its items in cards", fixed = TRUE)
  ds <- body(list(name = "summary", mode = "dataset"))
  expect_no_match(ds, "Edit its items in cards", fixed = TRUE)
  expect_match(ds, "come from a dataset", fixed = TRUE)
  df <- body(list(name = "summary", mode = "defaults"))
  expect_no_match(df, "Edit its items in cards", fixed = TRUE)
  expect_match(df, "set in the study defaults", fixed = TRUE)
  expect_no_match(body(NULL), "Edit its items in cards", fixed = TRUE)
  # both lines are in Japanese too
  for (z in c("Its rows come from a dataset: the cards work on the ARD, so edit its items here.",
              "This part is set in the study defaults: the cards cannot edit it.",
              "Cards of", "Part", "Edit its items in cards")) {
    expect_false(identical(tr(z, "ja"), z), info = z)
  }
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
