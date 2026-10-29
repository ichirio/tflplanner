# The table builder's content as cards (R/builder_content.R): one card an
# item of the plan, in its order, labelled "meaning (verb())"; each marked
# where it came from; its edits written back to the same sheets; the
# program's lines traced to the cards.

html <- function(...) as.character(htmltools::tagList(...))
# (the spec in step with its view, as a study opened: the items read from it)
sample_planner <- function() {
  .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
}
fns_of <- function(items) vapply(items, function(it) it$fn, "")
# a report's own order of values (the sample's are the ARD's, which tflspec
# does not write, #355): SEX's values the other way round, before the labels
own_levels <- function(p, id, args = list(SEX = c("Male", "Female"))) {
  o <- p$spec$outputs[[id]]
  k <- match("plan_labels", fns_of(o$content))
  o$content <- append(o$content, list(list(fn = "plan_levels", args = args)), after = k - 1L)
  .spec_set_output(p, id, o)
}

test_that("the cards are the plan's items, in its order", {
  skip_on_cran()
  p <- sample_planner()
  sp <- .spec_object_last(p, table_sheets(), .study_keys$table)
  for (id in c("T-14-1-1", "T-14-3-1")) {
    items <- builder_items(p, id)
    # (the bridge given the ARD's definition, as the builder gives it: no
    # plan_levels() the ARD already says, #358)
    plan <- .tflspec_fn(".plan_content_from_sheets")(sp, id, ard_spec = .ard_spec(p$ard))
    # (the ARD's normalization first, then the plan's)
    expect_identical(fns_of(items)[-1L], fns_of(plan), info = id)
    expect_identical(fns_of(items)[1:2], c("normalize_ard", "table_plan"))
    h <- html(.table_cards(items))
    shown <- sub('data-fn="([^"]+)"', "\\1", regmatches(h, gregexpr('data-fn="[^"]+"', h))[[1L]])
    expect_identical(shown, fns_of(items), info = id)
  }
  # a card shown though the plan has none (the Total column): placed by the
  # schema's order, switched off, its switch working
  items <- .builder_items_shown(builder_items(p, "T-14-1-1"), "plan_total")
  expect_true("plan_total" %in% fns_of(items))
  expect_false(items[[match("plan_total", fns_of(items))]]$present)
  h <- html(.table_cards(items[match("plan_total", fns_of(items))], switches = "plan_total"))
  expect_match(h, "rp-card-off", fixed = TRUE)
  expect_false(grepl("disabled", h, fixed = TRUE))
  # another optional card, whose on / off does not work yet: no switch at
  # all (not a disabled one that looks on), and no "off writes nothing"
  h <- html(.table_cards(items[match("plan_stub", fns_of(items))]))
  expect_false(grepl("form-switch", h, fixed = TRUE))
  expect_false(grepl("disabled", h, fixed = TRUE))
  expect_false(grepl("off writes nothing", h, fixed = TRUE))
})

test_that("a card comes only from the content: one it lacks is not shown on", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-3-1")
  # T-14-3-1's code has no plan_labels(): no item, and the card the builder
  # shows for its form says it is not in the code yet -- no switch, no mark
  expect_false("plan_labels" %in% fns_of(items))
  shown <- .builder_items_shown(items, "plan_labels")
  h <- html(.table_cards(shown[match("plan_labels", fns_of(shown))], switches = "plan_total"))
  expect_match(h, "rp-card-absent", fixed = TRUE)
  expect_match(h, "Not in the code yet", fixed = TRUE)
  expect_false(grepl("form-switch", h, fixed = TRUE))
  expect_false(grepl("rp-badge-own", h, fixed = TRUE))
})

test_that("the report's own item has the mark This table, with Revert over a default", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  own <- Filter(function(it) identical(it$from, "own"), items)
  expect_true(length(own) > 0L)
  for (it in own) {
    h <- html(.table_cards(list(it)))
    expect_match(h, "rp-badge-own", fixed = TRUE, info = it$fn)
    expect_match(h, "This table", fixed = TRUE, info = it$fn)
    expect_identical(grepl("rp-revert", h, fixed = TRUE), isTRUE(it$revert), info = it$fn)
  }
  dfl <- Filter(function(it) identical(it$from, "defaults") && length(it$args), items)
  if (length(dfl)) {
    h <- html(.table_cards(dfl[1L]))
    expect_false(grepl("rp-badge-own", h, fixed = TRUE))
    expect_match(h, "Study standard", fixed = TRUE)
  }
})

test_that("normalize_ard() is the first card: the hierarchy read only, the overall row", {
  skip_on_cran()
  p <- sample_planner()
  # (the report's YAML says it: hierarchy and the any-event row)
  items <- builder_items(p, "T-14-3-1")
  expect_identical(fns_of(items)[1:2], c("normalize_ard", "table_plan"))
  expect_identical(items[[1L]]$args, list(hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Any TEAE"))
  h <- html(.table_cards(items[1L]))
  expect_match(h, "Normalize the ARD", fixed = TRUE)
  expect_match(h, "rp-card-required", fixed = TRUE)
  # no R editing of it here: the hierarchy is step 1's
  expect_false(grepl("rp-tool-r", h, fixed = TRUE))
  # its line of the program leads to it
  lines <- c('data <- normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Any TEAE")',
             "", "plan <- table_plan(data, cols = TRT01A)", "")
  m <- builder_code_map(lines, items)
  expect_identical(m$path[m$line == 1L], "normalize_ard")
})

test_that("a card's label is the meaning and the verb; an argument's the meaning and its name", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  h <- html(.table_cards(items))
  expect_match(h, "rp-meaning\">Table structure<", fixed = TRUE)
  expect_match(h, "(table_plan())", fixed = TRUE)
  expect_match(h, "rp-meaning\">Statistics in the cells<", fixed = TRUE)
  expect_match(h, "(plan_cells())", fixed = TRUE)
  # every verb shown has its meaning
  expect_true(all(fns_of(items) %in% names(.plan_meaning)))
  # an argument: "Columns (cols)"
  a <- html(.item_args_view(items[[2L]]))
  expect_match(a, "rp-meaning\">Columns<", fixed = TRUE)
  expect_match(a, "(cols)", fixed = TRUE)
  # in Japanese, every meaning translated
  m <- unique(c(unname(.plan_meaning), unlist(.plan_arg_meaning, use.names = FALSE)))
  ja <- vapply(m, tr, "", lang = "ja")
  expect_true(all(ja != m), info = paste(m[ja == m], collapse = ", "))
  expect_match(html(.table_cards(items[2L], lang = "ja")), tr("Table structure", "ja"), fixed = TRUE)
})

test_that("read and written back unchanged, the sheets stay as they are", {
  skip_on_cran()
  skip_if_not_installed("cards")
  p <- add_output(new_planner(), "DM")
  p$sheets$tables <- .normalize_sheet(data.frame(
    output_id = "DM", cols = "TRT01A", rows = "group = variable"), "tables")
  m <- ard_meta(cards::ard_stack(
    cards::ADSL, .by = TRT01A,
    cards::ard_continuous(variables = AGE),
    cards::ard_categorical(variables = c(AGEGR1, SEX)), .total_n = TRUE))
  st <- builder_read(p, "DM", m)
  # the rows as layers: the block of the variables
  expect_identical(st$layers, ".variable")
  expect_identical(builder_write(p, "DM", st, was = st), p)
  # a key over the block: written as the rows
  m$keys$REGION <- c("EU", "US")
  st <- builder_read(p, "DM", m)
  expect_true("REGION" %in% st$layer_choices)
  # (an analysis variable is no key of the rows)
  expect_false("SEX" %in% st$layer_choices)
  sx <- st
  sx$layers <- c("REGION", ".variable")
  q <- builder_write(p, "DM", sx, was = st)
  expect_identical(sheet_rows(q, "tables", "DM")$rows, "group1 = REGION | group = variable")
  expect_identical(builder_read(q, "DM", m)$layers, c("REGION", ".variable"))
  # each item of the sample's tables, written back as the report says it
  s <- sample_planner()
  for (id in c("T-14-1-1", "T-14-3-1")) {
    seen <- character()
    for (it in builder_items(s, id)) {
      seen <- c(seen, it$fn)
      if (identical(it$fn, "normalize_ard")) next
      q <- .builder_item_write(s, id, it$fn, it$args, sum(seen == it$fn))
      expect_identical(q$sheets, s$sheets, info = paste(id, it$fn))
      expect_identical(q$spec$text, s$spec$text, info = paste(id, it$fn))
    }
  }
})

test_that("an item of the defaults is marked; the report's own over it can be reverted", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  at <- function(fn) items[[match(fn, fns_of(items))]]
  # the study's default: marked, nothing to revert, closed
  expect_identical(at("plan_stub")$from, "defaults")
  expect_false(at("plan_stub")$revert)
  h <- html(.table_cards(list(at("plan_stub")), id = function(x) paste0("c_", x)))
  expect_match(h, 'data-from="defaults"', fixed = TRUE)
  expect_match(h, ">Study standard<", fixed = TRUE)
  expect_false(grepl("__revert", h, fixed = TRUE))
  expect_false(grepl("rp-card-open", h, fixed = TRUE))
  # the report's own, alone: no mark, nothing to revert
  expect_identical(at("plan_paginate_rows")$from, "own")
  expect_false(at("plan_paginate_rows")$revert)
  # the report's own over the defaults': Revert
  expect_identical(at("plan_cells")$from, "own")
  expect_true(at("plan_cells")$revert)
  h <- html(.table_cards(list(at("plan_cells")), id = function(x) paste0("c_", x)))
  expect_match(h, 'id="c_plan_cells__revert"', fixed = TRUE)
  # reverted: the defaults' again, the report's own rows gone
  q <- .builder_item_revert(p, "T-14-1-1", "plan_cells")
  expect_identical(nrow(sheet_rows(q, "cells", "T-14-1-1")), 0L)
  it <- builder_items(q, "T-14-1-1")
  expect_identical(it[[match("plan_cells", fns_of(it))]]$from, "defaults")
  # an edit as R lands in its sheet
  r <- .builder_item_write(p, "T-14-1-1", "plan_paginate_rows",
                           args_from_r("plan_paginate_rows", 'max_rows = 25, split = "group_safe"'))
  expect_identical(sheet_rows(r, "layout", "T-14-1-1")$pages_max_rows, "25")
})

test_that("a line of the program leads to the card that wrote it", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-1"
  items <- builder_items(p, id)
  code <- unlist(strsplit(paste(program_code(p, id), collapse = "\n"), "\n", fixed = TRUE))
  m <- builder_code_map(code, items)
  expect_true(all(m$path %in% vapply(items, function(it) it$path, "")))
  path_of <- function(pattern) m$path[match(grep(pattern, code)[1L], m$line)]
  # (the plan is one pipe from the ARD: plan <- ard |> normalize_ard() |> table_plan())
  expect_identical(path_of("^(plan <- |  )table_plan\\("), "table_plan")
  # with the normalization's card among the items: its line in the pipe
  m2 <- builder_code_map(code, items)
  at <- grep("^  normalize_ard\\(", code)[1L]
  expect_false(is.na(at))
  expect_identical(m2$path[match(at, m2$line)], "normalize_ard")
  expect_identical(path_of("^  plan_cells\\("), "plan_cells")
  expect_identical(path_of("^  plan_col_header\\("), "plan_col_header")
  # a call over several lines: each of them
  cells <- m$line[m$path == "plan_cells"]
  expect_gt(length(cells), 1L)
  expect_identical(cells, seq(min(cells), max(cells)))
  # the code lists at the head: plan_levels()'s.  T-14-1-1's orders are the
  # ARD's own, which tflspec does not write (#355): an order of the
  # report's own, SEX's values the other way round
  p3 <- own_levels(p, id)
  code3 <- unlist(strsplit(paste(program_code(p3, id), collapse = "\n"), "\n", fixed = TRUE))
  m3 <- builder_code_map(code3, builder_items(p3, id))
  at <- grep("^cl_sex <- ", code3)[1L]
  expect_false(is.na(at))
  expect_identical(m3$path[match(at, m3$line)], "plan_levels")
  # nothing outside the plan
  expect_true(is.na(path_of("^generate_rtfreport")))
  # the map on the page
  h <- html(code_jump(m, "program_table"))
  expect_match(h, 'data-for="program_table"', fixed = TRUE)
  expect_match(h, "\"plan_cells\"", fixed = TRUE)
})

test_that("a thin line before each group of cards; the content's order kept", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  h <- html(.table_cards(items))
  # the cards in the content's order, as before
  shown <- sub('data-fn="([^"]+)"', "\\1", regmatches(h, gregexpr('data-fn="[^"]+"', h))[[1L]])
  expect_identical(shown, fns_of(items))
  # one line a group, before its first card
  seps <- regmatches(h, gregexpr('rp-card-sep" data-section="[a-z]+"', h))[[1L]]
  expect_identical(sub('.*"([a-z]+)"$', "\\1", seps), c("mapping", "finish"))
  at <- function(x) regexpr(x, h, fixed = TRUE)[[1L]]
  first_finish <- fns_of(items)[vapply(fns_of(items), .plan_section, "") == "finish"][1L]
  expect_lt(at('data-section="finish">'), at(sprintf('data-fn="%s"', first_finish)))
  expect_lt(at('data-section="mapping">'), at('data-fn="table_plan"'))
  expect_match(h, ">Table skeleton<", fixed = TRUE)
  expect_match(h, ">Finishing<", fixed = TRUE)
  # the groups by verb
  expect_identical(.plan_section("plan_sort"), "finish")
  expect_identical(.plan_section("plan_col_header"), "finish")
  expect_identical(.plan_section("plan_paginate_cols"), "finish")
  expect_match(html(.table_cards(items, lang = "ja")), tr("Finishing", "ja"), fixed = TRUE)
})

test_that("a required card says Required; the arrow is the open state's; the seldom changed start closed", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  h <- html(.table_cards(items[match("table_plan", fns_of(items))]))
  expect_match(h, 'class="rp-card-req">Required<', fixed = TRUE)
  expect_match(h, "rp-card-open", fixed = TRUE)
  # (the arrow is drawn by the CSS: right closed, down open)
  expect_false(grepl("\u25b8", h, fixed = TRUE))
  expect_match(.builder_cards_css, "rp-card-open > .rp-card-head .rp-card-toggle::before", fixed = TRUE)
  h <- html(.table_cards(items[match("plan_stub", fns_of(items))]))
  expect_false(grepl("rp-card-req", h, fixed = TRUE))
  expect_match(h, "rp-card-rare", fixed = TRUE)
  expect_false(grepl("rp-card-open", h, fixed = TRUE))
  # a seldom changed card with the report's own form still starts closed
  pg <- items[match("plan_paginate_rows", fns_of(items))]
  h <- html(.table_cards(pg, bodies = list(plan_paginate_rows = shiny::div("form"))))
  expect_false(grepl("rp-card-open", h, fixed = TRUE))
})

test_that("the decimals' card: its heading, where they go, and plan_cells' lines lit with it", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  h <- html(.table_cards(items[match("plan_digits", fns_of(items))]))
  expect_match(h, "rp-meaning\">Decimals and rounding<", fixed = TRUE)
  expect_match(h, "The decimals are written into plan_cells", fixed = TRUE)
  expect_match(h, 'data-also="plan_cells"', fixed = TRUE)
  expect_match(.builder_cards_js, "data-also", fixed = TRUE)
  expect_identical(tr("Decimals and rounding", "ja"), "\u6841\u3068\u4e38\u3081")
})

test_that("the order of the values says where it is edited; the row order explains its keys", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(own_levels(p, "T-14-1-1"), "T-14-1-1")
  h <- html(.table_cards(items[match("plan_levels", fns_of(items))]))
  # (a form now: no "edit them as R" note; Edit as R in the card's menu)
  expect_false(grepl("edited in stage 2", h, fixed = TRUE))
  expect_match(h, "rp-tool-r", fixed = TRUE)
  items <- builder_items(p, "T-14-3-1")
  h <- html(.table_cards(items[match("plan_sort", fns_of(items))]))
  expect_match(h, ".overall the any-event row first", fixed = TRUE)
  expect_match(h, ".depth each group's own row", fixed = TRUE)
  expect_match(h, "rp-tip", fixed = TRUE)
})

test_that("the cards' menu lists the cards in their order and brings one into view", {
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
  h <- html(.table_cards_bar(items, id = function(x) paste0("c_", x)))
  expect_match(h, ">Cards (plan)<", fixed = TRUE)
  tg <- sub('data-target="c_([^"]+)"', "\\1", regmatches(h, gregexpr('data-target="c_[^"]+"', h))[[1L]])
  expect_identical(tg, vapply(items, function(it) it$path, ""))
  expect_match(h, "dropdown-header", fixed = TRUE)
  expect_match(h, "rp-layer-go rp-layer-rare", fixed = TRUE)
  expect_match(.builder_cards_js, "rp-layer-go", fixed = TRUE)
  expect_identical(tr("Cards (plan)", "ja"), "\u30ab\u30fc\u30c9\u4e00\u89a7 (plan)")
})

test_that("the chips of an order wrap when narrow, never cut", {
  expect_match(.builder_content_css, "horizontal .rank-list { flex-wrap: wrap;", fixed = TRUE)
  expect_match(.builder_content_css, "overflow: visible; white-space: nowrap;", fixed = TRUE)
})

test_that("the screen review's fixes: per-variable templates said, every argument on a form of its own, an off card with no body (#417)", {
  # plan_cells()' entries of a variable of its own: said on the card
  pc <- list(default = "{n}", continuous = list(n = "{N}"),
             prob = list(tflspec::tfl_r("a ~ 1"), tflspec::tfl_r("b ~ 2"), "{x}"), time = "{t}")
  expect_identical(.cells_own_vars(pc), c("prob (3)", "time"))
  expect_null(.cells_own_vars(list(default = "{n}", AGE = list(n = "{N}")), skip = "AGE"))
  # (the card's table says them now: #454)
  h <- html(.cells_vars_ro(pc))
  expect_match(h, "Per variable: prob (3), time", fixed = TRUE)
  # a form of its own: the function's other arguments, below it
  expect_true("keep" %in% .special_args("plan_sort"))
  expect_false("keys" %in% .special_args("plan_sort"))
  expect_true(all(c("stat", "show_keys") %in% .special_args("table_plan")))
  expect_false(any(c("cols", "rows", "label") %in% .special_args("table_plan")))
  it <- list(fn = "plan_sort", args = list(keys = c(".overall", "label")), path = "plan_sort",
             present = TRUE, from = "own")
  h <- html(.special_more("c", "plan_sort", it))
  expect_match(h, "Not in the code", fixed = TRUE)
  expect_match(h, 'data-arg="keep"', fixed = TRUE)
  # ... read back onto the item's arguments as they are now
  # (keep's default TRUE: unticked is written, ticked again is none)
  r <- .special_read(list(c__keep = FALSE), "c", "plan_sort")
  expect_identical(.special_args_put(it$args, r)$keep, FALSE)
  expect_identical(.special_args_put(c(it$args, list(keep = FALSE)),
                                     .special_read(list(c__keep = TRUE), "c", "plan_sort")),
                   it$args)
  expect_null(.special_read(list(), "c", "plan_sort"))
  # an off card: no body, the summary says what turning it on does
  off <- list(fn = "plan_filter", args = list(), path = "plan_filter", present = FALSE,
              switch = TRUE, on = FALSE)
  h <- html(.table_cards(list(off)))
  expect_false(grepl("No arguments: the defaults of the function.", h, fixed = TRUE))
  expect_match(h, "On: its usual values are written.", fixed = TRUE)
})

test_that("the rounding's choices say SAS, R, IEC 60559; the YAML keeps sas, r, iec60559 (#417)", {
  h <- html(.rounding_field("n", "sas"))
  expect_match(h, '<option value="sas" selected>SAS</option>', fixed = TRUE)
  expect_match(h, '<option value="r">R</option>', fixed = TRUE)
  expect_match(h, '<option value="iec60559">IEC 60559</option>', fixed = TRUE)
})

test_that("an ordered field of several values is dragged to reorder (#417)", {
  h <- html(arg_field("c", "hierarchy", "list<column>", value = c("AEBODSYS", "AEDECOD"),
                      choices = c("AEBODSYS", "AEDECOD")))
  expect_match(h, "drag_drop", fixed = TRUE)
  expect_match(h, "remove_button", fixed = TRUE)
})
