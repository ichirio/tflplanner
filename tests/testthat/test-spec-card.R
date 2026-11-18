# The card generator (R/spec_card.R): a card's fields are its function's
# definitions -- tflspec's tfl_arg_defs() table -- and nothing else; what a
# card reads back is what the SPEC's YAML keeps.

html <- function(...) as.character(htmltools::tagList(...))
data_args_of <- function(h) {
  gsub('data-arg="|"', "", regmatches(h, gregexpr('data-arg="[^"]+"', h))[[1L]])
}
# a card's fields filled as its item says (as the page would hold them)
inputs_of <- function(fn, args, id = "c") {
  d <- spec_card_defs(fn)
  inp <- list()
  for (a in setdiff(d$formals, .data_args)) {
    v <- args[[a]]
    kind <- .arg_kind(d$args[[a]]$type)
    if (.is_unset(v)) {
      inp[[paste0(id, "__", a, "__none")]] <- TRUE
      next
    }
    if (isTRUE(d$args[[a]]$dots)) v <- .spec_dots_text(fn, a, v)
    if (identical(kind, "logical")) v <- isTRUE(v)
    if (is.null(v)) next
    inp[[paste0(id, "__", a)]] <- if (is.logical(v)) v else
      if (kind %in% c("r", "string") && !is.character(v)) .arg_to_r(v) else
        if (inherits(v, "tfl_r")) as.character(unclass(v)) else v
  }
  inp
}

test_that("each generator card's fields are its definitions table", {
  skip_on_cran()
  expect_true(all(c("plan_blanks", "plan_style", "plan_empty", "plan_titles", "plan_footnotes",
                    "plan_after") %in% .plan_form_fns))
  for (fn in .plan_form_fns) {
    defs <- tflspec::tfl_arg_defs(fn = fn)
    sch <- tflspec::tfl_schema(fn)
    fl <- names(formals(utils::getFromNamespace(fn, "rtfreporter")))
    typed <- names(Filter(function(x) !is.null(x$type), sch$args))
    key <- if (identical(sch$dots$style, "positional") && !sch$dots$key %in% names(sch$args)) sch$dots$key
    f <- spec_card_fields(fn)
    # the table's rows with a type, the function's own arguments and the
    # key of its `...`: each a field, once, and no other
    want <- setdiff(unique(c(fl, intersect(defs$name, c(typed, fl)), key)), .data_args)
    expect_setequal(f$name, want)
    expect_false(any(duplicated(f$name)), info = fn)
    expect_true(all(setdiff(f$name, c(defs$name, key)) %in% fl), info = fn)
    # in the function's order
    in_fl <- f$name[f$name %in% fl]
    expect_identical(in_fl, fl[fl %in% in_fl], info = fn)
    # a field's type and tier are the table's (the fallback's types excepted)
    own <- setdiff(intersect(f$name, typed), names(.spec_fallback$type[[fn]]))
    expect_identical(f$type[match(own, f$name)], defs$type[match(own, defs$name)], info = fn)
    both <- intersect(f$name, defs$name)
    expect_identical(f$tier[match(both, f$name)], defs$tier[match(both, defs$name)], info = fn)
    # the body draws exactly these
    h <- html(spec_card("c", fn, list(fn = fn, args = list()),
                        ctx = list(cols = c("row_label", ".values"))))
    expect_setequal(data_args_of(h), f$name)
    expect_false(any(duplicated(data_args_of(h))), info = fn)
  }
})

test_that("the fallback names only arguments the cards have, and tflspec's own values win", {
  skip_on_cran()
  for (fn in names(.spec_fallback$type)) {
    expect_true(all(names(.spec_fallback$type[[fn]]) %in% spec_card_fields(fn)$name), info = fn)
  }
  for (fn in names(.spec_fallback$default)) {
    d <- spec_card_defs(fn)
    expect_true(all(names(.spec_fallback$default[[fn]]) %in% d$formals), info = fn)
  }
  # a label the schema gives is the card's, not the fallback's
  expect_identical(.verb_meaning("plan_blanks"),
                   tflspec::tfl_schema("plan_blanks")$label %||% .plan_meaning[["plan_blanks"]])
  # a default of the fallback: plan_empty()'s enum says which entry is the default
  expect_identical(spec_card_defs("plan_empty")$args$when$default, "auto")
  expect_match(html(spec_card("c", "plan_empty", list(fn = "plan_empty", args = list()))),
               "auto (default)", fixed = TRUE)
})

test_that("a field is above the fold when the call writes it or its tier is key; the rest folded", {
  skip_on_cran()
  fn <- "plan_blanks"
  it <- list(fn = fn, args = list(where = "between_groups"))
  f <- spec_card_fields(fn, it)
  expect_identical(f$name[f$shown], "where")
  # usual: folded at its default
  expect_true(all(f$tier[!f$shown] %in% c("usual", "other")))
  # key: shown though it has no value
  d <- spec_card_defs(fn)
  d$args$counted$tier <- "key"
  f2 <- spec_card_fields(fn, it, defs = d)
  expect_setequal(f2$name[f2$shown], c("where", "counted"))
  h <- html(spec_card("c", fn, it, defs = d))
  up <- regmatches(h, regexpr("(?s)rp-args-up.*?(?=rp-more)", h, perl = TRUE))
  expect_setequal(data_args_of(up), c("where", "counted"))
  expect_setequal(data_args_of(sub(up, "", h, fixed = TRUE)), setdiff(f2$name, c("where", "counted")))
})

test_that("group headings where the definitions give groups, none where they do not", {
  skip_on_cran()
  fn <- "plan_blanks"
  it <- list(fn = fn, args = list(where = "between_groups", first = TRUE, last = TRUE, counted = TRUE))
  d0 <- spec_card_defs(fn)
  d0$groups <- NULL
  for (a in names(d0$args)) d0$args[[a]]$group <- NULL
  h0 <- html(spec_card("c", fn, it, defs = d0))
  expect_false(grepl("rp-group-label", h0, fixed = TRUE))
  # groups: in the function's `groups:` order, each its heading (en / ja); a
  # field of no group first, a group only an argument names last
  d <- d0
  d$args$first$group <- "edges"
  d$args$last$group <- "edges"
  d$args$where$group <- "place"
  d$groups <- list(list(id = "edges", label = list(en = "Page edges", ja = "ページの端")),
                   list(id = "unused", label = "Unused"))
  h <- html(spec_card("c", fn, it, defs = d))
  heads <- regmatches(h, gregexpr('rp-group-label">[^<]*<', h))[[1L]]
  expect_identical(gsub('rp-group-label">|<', "", heads), c("Page edges", "place"))
  expect_identical(regmatches(h, gregexpr('data-group="[^"]+"', h))[[1L]],
                   c('data-group="edges"', 'data-group="place"'))
  # (all four are written: none folded; the field of no group first)
  expect_identical(data_args_of(h), c("counted", "first", "last", "where"))
  expect_match(html(spec_card("c", fn, it, defs = d, lang = "ja")), "ページの端", fixed = TRUE)
  # the folded fields: one fold at the card's end, "More settings (n)", the
  # same headings inside it (canon 04a5cbe) -- no fold a group
  it1 <- list(fn = fn, args = list(where = "between_groups"))
  h1 <- html(spec_card("c", fn, it1, defs = d))
  expect_identical(lengths(regmatches(h1, gregexpr("rp-more-toggle", h1, fixed = TRUE))), 1L)
  expect_match(h1, "More settings (3)", fixed = TRUE)
  expect_match(html(spec_card("c", fn, it1, defs = d, lang = "ja")), "詳細設定 (3)", fixed = TRUE)
  fold <- sub("(?s)^.*rp-more-body", "", h1, perl = TRUE)
  expect_identical(data_args_of(fold), c("counted", "first", "last"))
  expect_match(fold, '(?s)data-arg="counted".*rp-more-group" data-group="edges".*rp-group-label">Page edges<.*data-arg="first"',
               perl = TRUE)
  # (a group with no field on a side is there for the page to move a field into)
  expect_match(h1, '(?s)rp-args-up.*data-group="edges".*data-group="place".*data-arg="where".*rp-more', perl = TRUE)
  # no groups: the fold as it was, its fields in the function's order
  h01 <- html(spec_card("c", fn, it1, defs = d0))
  expect_match(h01, "More settings (3)", fixed = TRUE)
  expect_false(grepl("rp-more-group", h01, fixed = TRUE))
  # the same fields read back, grouped or not
  inp <- inputs_of(fn, it$args)
  expect_identical(spec_card_read(inp, "c", fn, defs = d), spec_card_read(inp, "c", fn, defs = d0))
})

test_that("the table's group and label columns reach the card when tflspec has them", {
  skip_on_cran()
  sch <- list(args = list(where = list(type = "string", label = "Where", help = "Help."),
                          first = list(type = "logical")))
  defs <- data.frame(name = c("where", "first"), group = c("place", NA),
                     label_en = c("Place", NA), label_ja = c("場所", NA),
                     help_ja = c("説明", NA), stringsAsFactors = FALSE)
  attr(defs, "groups") <- list(list(id = "place", label = list(en = "Placing", ja = "配置")))
  out <- .spec_defs_columns(sch, defs)
  expect_identical(out$args$where$group, "place")
  expect_identical(out$args$where$label, list(en = "Place", ja = "場所"))
  expect_identical(out$args$where$help, list(en = "Help.", ja = "説明"))
  expect_null(out$args$first$group)
  expect_identical(out$groups[[1L]]$id, "place")
  # today's table (no such columns): the schema as it was
  expect_identical(.spec_defs_columns(sch, defs[, "name", drop = FALSE]), sch)
})

test_that("the cards read back what they show: no edit, no change (all the generator's verbs)", {
  skip_on_cran()
  U <- tflspec::tfl_unset()
  R <- tflspec::tfl_r
  cases <- list(
    normalize_ard = list(overall = "Any TEAE", hierarchy = c("AEBODSYS", "AEDECOD")),
    plan_filter = list(any = FALSE, parents = "drop"),
    plan_blanks = list(where = "between_groups", first = TRUE, last = U),
    plan_paginate_rows = list(max_rows = 40L, split = "group_safe", page_by = "PARAM"),
    plan_paginate_cols = list(every = 3L, keep = "row_label"),
    plan_paginate_group = list(col = "PARAM", keep = FALSE, heading = "Parameter: {value}"),
    plan_row_group = list(mode = "indent", collapse = TRUE),
    plan_stub = list(name = "row_label", indent = 2L, heading = ""),
    plan_columns = list(decimal = ".values", auto_width = TRUE, cell_format = R("toupper")),
    plan_style = list(border = "none", align_count_pct = TRUE, font_size_half_points = 16L),
    plan_hide = list(cols = c("TRT01AN", "AVISITN")),
    plan_cell_style = list(cols = "row_label", bold = TRUE, color = "#CC0000", where = R("p > 0.5")),
    plan_empty = list(text = "No events.", when = "no_rows", footnote = "None reported."),
    plan_titles = list(titles = c("Table 14.1.1", "Demographics")),
    plan_footnotes = list(footnotes = c("Note 1", "Note 2")),
    plan_after = list(steps = list(R("function(x) x"))))
  expect_setequal(names(cases), .plan_form_fns)
  for (fn in names(cases)) {
    it <- list(fn = fn, args = cases[[fn]], own_args = cases[[fn]])
    r <- spec_card_read(inputs_of(fn, it$args), "c", fn)
    expect_false(is.null(r), info = fn)
    own <- spec_card_args(r, it, fn)
    expect_identical(args_to_r(fn, own), args_to_r(fn, it$args), info = fn)
    expect_identical(names(Filter(.is_unset, own)), names(Filter(.is_unset, it$args)), info = fn)
  }
})

test_that("a card's edit is written to the SPEC's YAML and read back the same", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  id <- "T-14-1-1"
  item_of <- function(p, fn) Filter(function(i) i$fn == fn, builder_items(p, id))[[1L]]
  # the fields as the page holds them after an edit: `set` over the item's
  edit <- function(p, fn, set, none = character()) {
    present <- fn %in% vapply(builder_items(p, id), function(i) i$fn, "")
    it <- if (present) item_of(p, fn) else list(fn = fn, args = list())
    inp <- inputs_of(fn, utils::modifyList(it$args %||% list(), set))
    for (a in none) inp[[paste0("c__", a, "__none")]] <- TRUE
    args <- spec_card_args(spec_card_read(inp, "c", fn), it, fn)
    .builder_item_write(p, id, fn, args, 1L)
  }
  p <- s$planner
  p <- edit(p, "plan_blanks", list(where = "between_groups", counted = TRUE), none = "first")
  p <- edit(p, "plan_paginate_rows", list(max_rows = 33L))
  p <- edit(p, "plan_stub", list(indent = 2L))
  p <- edit(p, "plan_empty", list(text = "No events.", when = "no_rows"))
  p <- edit(p, "plan_titles", list(titles = tflspec::tfl_r('"Table 14.1.1", "Demographics"')))
  p <- edit(p, "plan_footnotes", list(footnotes = tflspec::tfl_r('"Note 1"')))
  want <- lapply(stats::setNames(nm = c("plan_blanks", "plan_paginate_rows", "plan_stub", "plan_empty",
                                        "plan_titles", "plan_footnotes")),
                 function(fn) item_of(p, fn)$args)
  expect_s3_class(want$plan_blanks$first, "tfl_unset")
  expect_true(want$plan_blanks$counted)
  expect_identical(as.integer(want$plan_paginate_rows$max_rows), 33L)
  expect_identical(want$plan_empty$when, "no_rows")
  # (the `...` of a verb: its entries, not R)
  expect_identical(as.character(unlist(want$plan_titles$titles)), c("Table 14.1.1", "Demographics"))
  s$planner <- p
  s <- suppressWarnings(suppressMessages(save_study(s, home = home)))
  y <- paste(readLines(file.path(s$path, "spec", "outputs", paste0(id, ".yml")), warn = FALSE),
             collapse = "\n")
  expect_match(y, "first: !unset", fixed = TRUE)
  expect_match(y, "max_rows: 33", fixed = TRUE)
  expect_match(y, "plan_empty", fixed = TRUE)
  expect_match(y, "Table 14.1.1", fixed = TRUE)
  expect_false(grepl("titles: !r", y, fixed = TRUE))
  s2 <- suppressWarnings(suppressMessages(open_study(basename(s$path), home = home)))
  for (fn in names(want)) {
    it <- item_of(s2$planner, fn)
    expect_identical(args_to_r(fn, it$args), args_to_r(fn, want[[fn]]), info = fn)
    # drawn from the file and read back: nothing to write
    own <- spec_card_args(spec_card_read(inputs_of(fn, it$args), "c", fn), it, fn)
    expect_true(.args_same_r(fn, own, it$args), info = fn)
  }
  # the program has them
  code <- paste(program_code(s2$planner, id), collapse = "\n")
  expect_match(code, 'plan_titles(c("Table 14.1.1", "Demographics"))', fixed = TRUE)
  expect_match(code, 'plan_footnotes("Note 1")', fixed = TRUE)
  expect_match(code, "plan_empty(", fixed = TRUE)
})

test_that("the four verbs that had no form have fields on the builder's cards", {
  skip_on_cran()
  it <- list(fn = "plan_titles", args = list(titles = c("A title", "Another")), path = "plan_titles",
             from = "own", present = TRUE)
  h <- html(spec_card("c", "plan_titles", it))
  # the entries of its `...` as the call writes them, in one R field
  expect_match(h, 'id="c__titles"', fixed = TRUE)
  expect_match(h, "&quot;A title&quot;, &quot;Another&quot;", fixed = TRUE)
  expect_identical(spec_card_fields("plan_titles")$name, c("titles", "pages", "border"))
  expect_identical(spec_card_fields("plan_after")$name, "steps")
  expect_setequal(spec_card_fields("plan_empty")$name, c("text", "when", "position", "footnote"))
  # emptied: the argument goes
  r <- spec_card_read(list(c__titles = "", c__pages = "", c__border = ""), "c", "plan_titles")
  expect_null(spec_card_args(r, it, "plan_titles")$titles)
  # R that is not entries stays as typed (the write says what is wrong)
  r2 <- spec_card_read(list(c__titles = "pages = 1"), "c", "plan_titles")
  expect_s3_class(r2$titles, "tfl_r")
})

test_that("the R switch is the definition's: only on a field whose type takes an R expression", {
  skip_on_cran()
  expect_true(.type_takes_r("enum[a|b]|list<integer>|r"))
  expect_true(.type_takes_r("formula"))
  expect_true(.type_takes_r(NULL))
  expect_false(.type_takes_r("enum[sas|r|iec60559]"))
  expect_false(.type_takes_r("map<column, r>"))
  expect_false(.type_takes_r("list<column>"))
  has_switch <- function(h, a) grepl(sprintf('id="c__%s__isr"', a), h, fixed = TRUE)
  it <- list(fn = "plan_blanks", args = list())
  h <- html(spec_card("c", "plan_blanks", it))
  # where: enum[...]|list<integer>|r takes R; first: logical does not
  expect_true(has_switch(h, "where"))
  expect_false(has_switch(h, "first"))
  # every generator card: a switch exactly where the schema's type takes R
  for (fn in .plan_form_fns) {
    sch <- tflspec::tfl_schema(fn)
    d <- spec_card_defs(fn)
    h <- html(spec_card("c", fn, list(fn = fn, args = list()), ctx = list(cols = c("row_label", ".values"))))
    for (a in spec_card_fields(fn)$name) {
      only_r <- .arg_kind(d$args[[a]]$type) == "r"
      expect_identical(has_switch(h, a), !only_r && .type_takes_r(sch$args[[a]]$type),
                       info = paste(fn, a))
    }
  }
  # the type the field is drawn by is not what decides: plan_cell_style()'s
  # colour is a picker, and takes R (colour|r|map<column, r>)
  expect_true(has_switch(html(spec_card("c", "plan_cell_style", list(args = list()))), "color"))
  # a value that is R already (written in the YAML by hand) keeps its switch
  h2 <- html(spec_card("c", "plan_blanks", list(fn = "plan_blanks", args = list(first = tflspec::tfl_r("x > 1")))))
  expect_true(has_switch(h2, "first"))
  # plan_columns(widths = ): no R of its own, kept for a value that is R
  hw <- html(spec_card("c", "plan_columns", list(args = list()), ctx = list(cols = c("a", "b"))))
  expect_false(has_switch(hw, "widths"))
  hw2 <- html(spec_card("c", "plan_columns", list(args = list(widths = tflspec::tfl_r("c(1, 2)"))),
                        ctx = list(cols = c("a", "b"))))
  expect_true(has_switch(hw2, "widths"))
})
