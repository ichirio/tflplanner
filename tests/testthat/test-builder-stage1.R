# The table builder after the first screen check (2026-10-16): the cards'
# two groups and their lines, the switches only where on / off writes,
# "Add", normalize_ard()'s card with its "From the ARD" mark, the layout
# cards' forms and the rows' variables shown.

html <- function(...) as.character(htmltools::tagList(...))
sample_planner <- function() {
  .planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner"))
}
fns_of <- function(items) vapply(items, function(it) it$fn, "")
item <- function(items, fn) items[[match(fn, fns_of(items))]]
cards_of <- function(p, id, hier = FALSE, ...) {
  .builder_items_cards(.builder_items_normalize(builder_items(p, id),
                                                if (hier) c("AEBODSYS", "AEDECOD")),
                       hier, ...)
}

test_that("the groups by verb: the table's skeleton, the finishing; normalize_ard() neither", {
  for (fn in c("table_plan", "plan_cells", "plan_stat_groups", "plan_total", "plan_compare",
               "plan_filter")) {
    expect_identical(.plan_section(fn), "mapping", info = fn)
  }
  for (fn in c("plan_labels", "plan_levels", "plan_nest", "plan_digits", "plan_sort",
               "plan_col_header", "plan_stub", "plan_blanks", "plan_paginate_rows",
               "plan_style", "plan_columns", "plan_cell_style", "plan_hide")) {
    expect_identical(.plan_section(fn), "finish", info = fn)
  }
  expect_identical(.plan_section("normalize_ard"), "ard")
  # the schema's stage, when tflspec says one, wins
  local_mocked_bindings(.schema_of = function(fn) list(stage = "mapping"))
  expect_identical(.plan_section("plan_sort"), "mapping")
})

test_that("a line before each group's first card, with its meaning; every card names its group", {
  p <- sample_planner()
  items <- .builder_items_normalize(builder_items(p, "T-14-1-1"))
  # the content's order mixes the groups (digits before cells): still one
  # line a group, and the order kept
  h <- html(.table_cards(items))
  seps <- regmatches(h, gregexpr('rp-card-sep" data-section="[a-z]+"', h))[[1L]]
  expect_identical(sub('.*"([a-z]+)"$', "\\1", seps), c("mapping", "finish"))
  shown <- sub('data-fn="([^"]+)"', "\\1", regmatches(h, gregexpr('data-fn="[^"]+"', h))[[1L]])
  expect_identical(shown, fns_of(items))
  expect_match(h, "Which ARD values go to which rows, columns and cells", fixed = TRUE)
  expect_match(h, "Labels, order, decimals, pages: the look", fixed = TRUE)
  # no line for normalize_ard(): it comes first, its head says ARD
  expect_lt(regexpr('data-fn="normalize_ard"', h), regexpr("rp-card-sep", h))
  tags <- regmatches(h, gregexpr('class="rp-card-stage">[^<]+<', h))[[1L]]
  expect_length(tags, length(items))
  expect_identical(sub('.*">([^<]+)<$', "\\1", tags[1:2]), c("ARD", "Table skeleton"))
  # the words are the strings', in Japanese too
  hj <- html(.table_cards(items, lang = "ja"))
  expect_match(hj, tr("Table skeleton", "ja"), fixed = TRUE)
  expect_match(hj, tr("Which ARD values go to which rows, columns and cells", "ja"), fixed = TRUE)
  expect_match(html(.table_cards_bar(items)), ">Cards (plan)<", fixed = TRUE)
})

test_that("required cards have no switch; a switch only where on / off writes; none disabled", {
  p <- sample_planner()
  items <- cards_of(p, "T-14-1-1")
  h <- html(.table_cards(items))
  expect_false(grepl("disabled", h, fixed = TRUE))
  for (fn in c("normalize_ard", "table_plan", "plan_cells")) {
    expect_false(item(items, fn)$switch, info = fn)
  }
  # the report's own, no study default under it: on, off takes it away
  expect_true(item(items, "plan_paginate_rows")$switch)
  expect_true(item(items, "plan_paginate_rows")$on)
  # the study's defaults: no switch (off could not take them away)
  expect_false(item(items, "plan_stub")$switch)
  # the usual cards of a summary table are all there
  expect_true(all(.plan_usual(FALSE) %in% fns_of(items)))
  expect_true(all(.plan_study_cards %in% fns_of(items)))
})

test_that("an AE table: plan_levels not shown on, offered by Add; an empty usual card is off", {
  p <- sample_planner()
  raw <- Filter(function(it) it$fn != "plan_levels", builder_items(p, "T-14-3-1"))
  items <- .builder_items_cards(.builder_items_normalize(raw, c("AEBODSYS", "AEDECOD")), TRUE)
  expect_false("plan_levels" %in% fns_of(items))
  expect_true("plan_levels" %in% .plan_addable(items))
  expect_false("plan_sort" %in% .plan_addable(items))
  expect_false(any(c("normalize_ard", "table_plan", "plan_cells") %in% .plan_addable(items)))
  # plan_filter: a usual card of an AE table, empty: off, its switch works
  f <- item(items, "plan_filter")
  expect_false(f$present)
  expect_false(f$on)
  expect_true(f$switch)
  h <- html(.table_cards(list(f)))
  expect_match(h, "rp-card-off", fixed = TRUE)
  expect_match(h, "On: its usual values are written.", fixed = TRUE)
  expect_false(grepl("Not in the code yet", h, fixed = TRUE))
  # Add offers the verbs not shown, meaning (fn()), in the schema's order
  sch <- .plan_add_schema(items)
  skip_if(is.null(sch))
  ch <- .item_choices(sch, "plan")
  expect_true("plan_levels" %in% ch)
  expect_false("plan_sort" %in% ch)
  expect_false(any(c("plan_titles", "plan_footnotes", "plan_listing") %in% ch))
})

test_that("added: a verb with usual values is written; one without waits, on, for a value", {
  p <- sample_planner()
  st <- list(variables = data.frame(variable = c("AGE", "SEX"), label = c("Age", NA),
                                    stringsAsFactors = FALSE),
             levels = list(AGE = character(), SEX = c("F", "M")),
             row_keys = c("AEBODSYS", "AEDECOD"), hierarchy = c("AEBODSYS", "AEDECOD"))
  expect_identical(.plan_starter_args("plan_labels", st), list(AGE = "Age"))
  expect_identical(.plan_starter_args("plan_levels", st), list(SEX = c("F", "M")))
  expect_identical(.plan_starter_args("plan_sort", st, hier = TRUE)$keys,
                   c(".overall", "group1", ".depth", "-n", "label"))
  expect_null(.plan_starter_args("plan_compare", st))
  # written, the card is in the content, in its place
  q <- .builder_item_write(p, "T-14-3-1", "plan_blanks", .plan_starter_args("plan_blanks", st))
  expect_true("plan_blanks" %in% fns_of(builder_items(q, "T-14-3-1")))
  # a verb added with no value: on, waiting ("not in the code yet"), its R
  items <- cards_of(p, "T-14-3-1", hier = TRUE, added = "plan_compare")
  cp <- item(items, "plan_compare")
  expect_true(cp$added)
  expect_true(cp$on)
  # placed by the schema's order among the content's
  at <- match("plan_compare", fns_of(items))
  expect_true(at > match("table_plan", fns_of(items)))
  h <- html(.table_cards(list(cp), new = "plan_compare"))
  expect_match(h, "Not in the code yet", fixed = TRUE)
  expect_match(h, "rp-card-new", fixed = TRUE)
  expect_match(h, "rp-tool-r", fixed = TRUE)
  expect_match(.builder_cards_js, "rp-card-new", fixed = TRUE)
})

test_that("the Total card: on by default with overall rows; off and no switch without", {
  skip_if_not_installed("cards")
  with_ovr <- cards::ard_stack(cards::ADSL, .by = TRT01A, cards::ard_categorical(variables = SEX),
                               .overall = TRUE)
  without <- cards::ard_stack(cards::ADSL, .by = TRT01A, cards::ard_categorical(variables = SEX))
  m1 <- ard_meta(with_ovr, rtfreporter::normalize_ard(with_ovr))
  m0 <- ard_meta(without, rtfreporter::normalize_ard(without))
  expect_true(m1$overall)
  expect_false(m0$overall)
  expect_identical(.ard_total_kind(m1), "overall")
  expect_identical(.ard_total_kind(m0), "none")
  expect_true("plan_total" %in% .ard_on_cards(m1))
  expect_false("plan_total" %in% .ard_on_cards(m0))
  p <- sample_planner()
  t0 <- item(cards_of(p, "T-14-1-1", ard_total = "none"), "plan_total")
  expect_false(t0$switch)
  h <- html(.table_cards(list(t0)))
  expect_false(grepl("form-switch", h, fixed = TRUE))
  expect_false(grepl("disabled", h, fixed = TRUE))
  expect_match(h, "The ARD has no overall rows.", fixed = TRUE)
  expect_identical(tr("The ARD has no overall rows.", "ja"),
                   "ARD に全体の行がありません")
  t1 <- item(cards_of(p, "T-14-1-1", ard_total = "overall"), "plan_total")
  expect_true(t1$switch)
})

test_that("the ARD tells the order and the labels: a factor's levels, its labels", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$SEX <- factor(adsl$SEX, levels = c("M", "F"))
  ard <- cards::ard_stack(adsl, .by = TRT01A, cards::ard_categorical(variables = c(SEX, RACE)))
  m <- ard_meta(ard, rtfreporter::normalize_ard(ard))
  expect_identical(m$factor_vars, "SEX")
  expect_true("plan_levels" %in% .ard_on_cards(m))
  st <- list(levels = list(SEX = c("M", "F"), RACE = c("A", "B")))
  expect_identical(names(.plan_starter_args("plan_levels", st, meta = m)), "SEX")
  m$factor_vars <- character()
  expect_false("plan_levels" %in% .ard_on_cards(m))
})

test_that("normalize_ard(): the hierarchy fixed and From the ARD, overall editable, the rest folded", {
  h <- html(.normalize_body("n", c("AEBODSYS", "AEDECOD"),
                            tb = data.frame(overall_label = "Any TEAE", stack = NA),
                            overall_id = "ov", revert_id = "ovr"))
  expect_match(h, 'data-from="ard"', fixed = TRUE)
  expect_match(h, ">From the ARD<", fixed = TRUE)
  expect_match(h, 'class="rp-b-level">AEBODSYS<', fixed = TRUE)
  expect_false(grepl("remove", h, fixed = TRUE))
  expect_match(h, 'id="ov"', fixed = TRUE)
  expect_match(h, 'value="Any TEAE"', fixed = TRUE)
  expect_match(h, ">This table<", fixed = TRUE)
  expect_match(h, 'id="ovr"', fixed = TRUE)
  expect_match(h, "Other arguments (6)", fixed = TRUE)
  # a table with nothing set (DM): one line, the arguments folded
  h <- html(.normalize_body("n", character(), tb = NULL))
  expect_match(h, "No settings: the function's defaults.", fixed = TRUE)
  expect_match(h, "Other arguments (6)", fixed = TRUE)
  expect_false(grepl('data-arg="overall"', h, fixed = TRUE))
  # a stack the table says: shown, its fields read back as the columns
  h <- html(.normalize_body("n", character(), tb = data.frame(stack = "subgroup = SEX")))
  expect_match(h, 'id="n__stack"', fixed = TRUE)
  expect_match(h, "Other arguments (3)", fixed = TRUE)
  r <- .normalize_read(list(n__stack = " subgroup = SEX ", n__stack_sep = "", n__stack_nest = TRUE), "n")
  expect_identical(r, list(stack = "subgroup = SEX", stack_sep = NA_character_, stack_nest = "TRUE"))
  expect_null(.normalize_read(list(), "n"))
  # the mark: one of the badges
  expect_match(html(from_badge("ard", "ja")), tr("From the ARD", "ja"), fixed = TRUE)
  expect_identical(tr("From the ARD", "ja"), "ARD から")
})

test_that("the layout cards' forms: their fields, the study's defaults marked, edits written back", {
  p <- sample_planner()
  id <- "T-14-1-1"
  items <- builder_items(p, id)
  want <- list(plan_blanks = c("where", "first", "last"),
               plan_paginate_rows = c("max_rows", "split", "page_by"),
               plan_stub = c("name", "indent", "before"),
               plan_style = c("border", "align_count_pct", "font_size_half_points"))
  for (fn in names(want)) {
    it <- item(items, fn)
    h <- html(.plan_form_body("c", fn, it, choices = c("TRT01A", "SEX")))
    for (a in want[[fn]]) expect_match(h, sprintf('id="c__%s"', a), fixed = TRUE, info = paste(fn, a))
    expect_match(h, "Other arguments", fixed = TRUE, info = fn)
  }
  # the kinds of field: a choice, a number, a check
  h <- html(.plan_form_body("c", "plan_blanks", item(items, "plan_blanks")))
  expect_match(h, '<option value="between_groups" selected>', fixed = TRUE)
  expect_match(h, ">Study default<", fixed = TRUE)
  h <- html(.plan_form_body("c", "plan_paginate_rows", item(items, "plan_paginate_rows")))
  expect_match(h, 'type="number"', fixed = TRUE)
  expect_match(h, ">This table<", fixed = TRUE)
  h <- html(.plan_form_body("c", "plan_style", item(items, "plan_style")))
  expect_match(h, 'id="c__align_count_pct" checked', fixed = TRUE)
  # read back unchanged: nothing to write
  it <- item(items, "plan_blanks")
  inp <- list(c__where = "between_groups", c__first = TRUE, c__last = TRUE, c__counted = TRUE)
  r <- .plan_form_read(inp, "c", "plan_blanks")
  expect_identical(.plan_form_own(r, it, "plan_blanks"), it$own_args %||% list())
  # a check unticked over the study's TRUE: the report's own FALSE, written
  inp$c__first <- FALSE
  own <- .plan_form_own(.plan_form_read(inp, "c", "plan_blanks"), it, "plan_blanks")
  expect_identical(own, list(first = FALSE))
  q <- .builder_item_write(p, id, "plan_blanks", own)
  expect_identical(sheet_rows(q, "layout", id)$blank_first, "FALSE")
  expect_identical(item(builder_items(q, id), "plan_blanks")$args$where, "between_groups")
  # a number and a choice
  it <- item(items, "plan_paginate_rows")
  inp <- list(c__max_rows = 25, c__split = "group_force", c__page_by = NULL)
  own <- .plan_form_own(.plan_form_read(inp, "c", "plan_paginate_rows"), it, "plan_paginate_rows")
  q <- .builder_item_write(p, id, "plan_paginate_rows", own)
  expect_identical(sheet_rows(q, "layout", id)$pages_max_rows, "25")
  expect_identical(sheet_rows(q, "layout", id)$pages_split, "group_force")
  # the style's and the row label column's
  it <- item(items, "plan_style")
  own <- .plan_form_own(.plan_form_read(list(c__border = "tfl", c__align_count_pct = TRUE,
                                             c__font_size_half_points = 18), "c", "plan_style"),
                        it, "plan_style")
  expect_identical(own$border, "tfl")
  q <- .builder_item_write(p, id, "plan_style", own)
  expect_identical(item(builder_items(q, id), "plan_style")$args$font_size_half_points, 18L)
  it <- item(items, "plan_stub")
  own <- .plan_form_own(.plan_form_read(list(c__name = "row_label", c__indent = 3, c__before = TRUE),
                                        "c", "plan_stub"), it, "plan_stub")
  expect_identical(own, list(indent = 3L))
  q <- .builder_item_write(p, id, "plan_stub", own)
  expect_identical(sheet_rows(q, "layout", id)$stub_indent, "3")
  # the field parts the figures use too
  expect_match(html(arg_field("f", "n", "integer", value = 2)), 'step="1" min="0"', fixed = TRUE)
  h <- html(arg_field("f", "e", "enum[a|b]", value = "z", default = "a"))
  expect_match(h, '<option value="z" selected>', fixed = TRUE)
  expect_match(h, "a (default)", fixed = TRUE)
  expect_false(grepl("checked", html(arg_field("f", "l", "logical", value = FALSE, default = TRUE))))
})

test_that("the variables shown: written as the row filter, read back; the user's own kept", {
  expect_identical(.hidden_vars_read(NA), character())
  w <- .hidden_vars_write(c("AGE", "SEX"))
  expect_identical(w, '!variable %in% c("AGE", "SEX")')
  expect_identical(.hidden_vars_read(w), c("AGE", "SEX"))
  expect_identical(.hidden_vars_read(.hidden_vars_write("AGE")), "AGE")
  expect_true(is.na(.hidden_vars_write(character())))
  # a filter of the user's: not the builder's to rewrite
  expect_null(.hidden_vars_read("p >= 0.05"))
  # the program: the ARD as it is, the variable's rows filtered out
  p <- sample_planner()
  tb <- sheet_rows(p, "tables", "T-14-1-1")
  tb$output_id <- NULL
  tb$keep_where <- .hidden_vars_write("AGE")
  q <- set_sheet_rows(p, "tables", "T-14-1-1", tb)
  code <- paste(program_code(q, "T-14-1-1"), collapse = "\n")
  expect_match(code, 'plan_filter(!variable %in% "AGE")', fixed = TRUE)
  expect_true("plan_filter" %in% fns_of(builder_items(q, "T-14-1-1")))
})

test_that("nothing changed, nothing written: the sample's items and program as they were", {
  p <- sample_planner()
  for (id in c("T-14-1-1", "T-14-3-1")) {
    for (it in builder_items(p, id)) {
      if (!it$fn %in% names(.plan_form_main)) next
      sch <- .plan_form_schema(it$fn)
      inp <- list()
      for (a in sch$formals) {
        v <- it$args[[a]]
        if (identical(.arg_kind(sch$args[[a]]$type), "logical")) v <- isTRUE(v)
        if (!is.null(v)) inp[[paste0("c__", a)]] <- if (is.logical(v)) v else
          if (.arg_kind(sch$args[[a]]$type) %in% c("r", "string") && !is.character(v)) .arg_to_r(v) else v
      }
      own <- .plan_form_own(.plan_form_read(inp, "c", it$fn), it, it$fn)
      expect_identical(args_to_r(it$fn, own), args_to_r(it$fn, it$own_args %||% list()),
                       info = paste(id, it$fn))
    }
  }
})

# a study with one table and its ARD read (as the builder sees it)
stage1_study <- function(id, ard) {
  p <- add_standard_defaults(add_output(new_planner(), id, type = "table"), "BS")
  s <- create_study("BS", planner = p)
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  s
}
stage1_open <- function(session, id) {
  session$setInputs(target = id, nav = "make", step = "content", content_nav = "content",
                    table_nav = "builder")
  bf <- session$userData$bform
  b <- function(x) paste0("b", bf$n, "_", x)
  v <- list()
  v[[b("key")]] <- "TRT01A"
  v[[b("vars")]] <- bf$st$variables$variable
  v[[b("value")]] <- "stat"
  v[[b("cat")]] <- bf$st$cat_format
  do.call(session$setInputs, v)
  session$elapse(1000)
}
set_in <- function(session, name, value) {
  v <- list(value)
  names(v) <- name
  do.call(session$setInputs, v)
  session$elapse(1000)
}

test_that("in the app: a switch writes and takes away; Add; the variables shown; Total on", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX), .overall = TRUE)
  stage1_study("T-S1", ard)
  shiny::testServer(server_for("BS"), {
    stage1_open(session, "T-S1")
    session$flushReact()
    rv <- session$userData$rv
    fns <- function() vapply(builder_items(rv$p, "T-S1"), function(it) it$fn, "")
    # the ARD has rows over all subjects: a new table's Total column is on
    expect_identical(sheet_rows(rv$p, "tables", "T-S1")$total, "Total")
    bf <- session$userData$bform
    cid <- function(path) paste0("b", bf$n, "_card_", path)
    # a usual card off (no levels written): on writes the ARD's levels
    it <- .builder_item_at(bf$items, "plan_levels")
    skip_if(is.null(it) || isTRUE(it$present))
    expect_false(it$on)
    set_in(session, paste0(cid("plan_levels"), "__on"), TRUE)
    expect_true("plan_levels" %in% fns())
    # ... and off takes it away again
    bf <- session$userData$bform
    it <- .builder_item_at(bf$items, "plan_levels")
    expect_true(it$present)
    expect_true(it$switch)
    set_in(session, paste0(cid("plan_levels"), "__on"), FALSE)
    expect_false("plan_levels" %in% fns())
    # Add: a verb with no usual value -- its card, on, waiting
    bf <- session$userData$bform
    h <- output$builder_form$html
    expect_match(h, sprintf('id="b%d_add"', bf$n), fixed = TRUE)
    set_in(session, paste0("b", bf$n, "_add"), "plan_compare")
    set_in(session, paste0("b", bf$n, "_add__add"), 1L)
    bf <- session$userData$bform
    cp <- .builder_item_at(bf$items, "plan_compare")
    expect_false(is.null(cp))
    expect_true(cp$added)
    expect_match(output$builder_form$html, "rp-card-new", fixed = TRUE)
    # the variables shown: SEX alone, AGE's rows filtered (the ARD as it is)
    set_in(session, paste0("b", bf$n, "_shown"), "SEX")
    expect_identical(sheet_rows(rv$p, "tables", "T-S1")$keep_where, '!variable %in% "AGE"')
    expect_true("plan_filter" %in% fns())
  })
})
