# The table builder after the first screen check (2026-10-16): the cards'
# two groups and their lines, the switches only where on / off writes,
# "Add", normalize_ard()'s card with its "From the ARD" mark, the layout
# cards' forms and the rows' variables shown.

html <- function(...) as.character(htmltools::tagList(...))
# (the spec in step with its view, as a study opened: the items read from it)
sample_planner <- function() {
  .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
}
fns_of <- function(items) vapply(items, function(it) it$fn, "")
item <- function(items, fn) items[[match(fn, fns_of(items))]]
cards_of <- function(p, id, hier = FALSE, ...) {
  .builder_items_cards(builder_items(p, id), hier, ...)
}

test_that("the groups by verb: the table's skeleton, the finishing; normalize_ard() neither", {
  for (fn in c("table_plan", "plan_cells", "plan_stat_groups", "plan_total", "plan_compare",
               "plan_filter")) {
    expect_identical(.plan_section(fn), "mapping", info = fn)
  }
  for (fn in c("plan_levels", "plan_labels", "plan_nest", "plan_digits", "plan_sort",
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
  skip_on_cran()
  p <- sample_planner()
  items <- builder_items(p, "T-14-1-1")
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

test_that("the cards: required + the items in effect + added; no switch (#440)", {
  skip_on_cran()
  p <- sample_planner()
  items <- cards_of(p, "T-14-1-1")
  h <- html(.table_cards(items))
  expect_false(grepl("disabled", h, fixed = TRUE))
  expect_false(grepl("form-switch", h, fixed = TRUE))
  for (fn in c("normalize_ard", "table_plan", "plan_cells", "plan_paginate_rows", "plan_stub")) {
    expect_false(item(items, fn)$switch, info = fn)
    expect_true(item(items, fn)$on, info = fn)
  }
  # the study standard's cards in effect are shown, with their mark
  expect_true(all(.plan_study_cards %in% fns_of(items)))
  # a saved report: no empty usual card; a new one (not saved): all of them
  expect_false(any(!vapply(items, function(it) isTRUE(it$present), NA) &
                     !fns_of(items) %in% .plan_tier_fns("required")))
  fresh <- cards_of(p, "T-14-1-1", fresh = TRUE)
  expect_true(all(.plan_tier_fns("usual") %in% fns_of(fresh)))
})

test_that("an AE table: the same usual cards as any (plan_levels off); an empty usual card is off", {
  skip_on_cran()
  p <- sample_planner()
  raw <- Filter(function(it) it$fn != "plan_levels", builder_items(p, "T-14-3-1"))
  # (a saved report: only what it has -- one tier list for every table)
  items <- .builder_items_cards(raw, TRUE)
  expect_false("plan_levels" %in% fns_of(items))
  expect_true("plan_levels" %in% .plan_addable(items))
  expect_false("plan_sort" %in% .plan_addable(items))
  expect_false(any(c("normalize_ard", "table_plan", "plan_cells") %in% .plan_addable(items)))
  # a new report: the usual card empty -- on, nothing written yet
  items <- .builder_items_cards(raw, TRUE, fresh = TRUE)
  lv <- item(items, "plan_levels")
  expect_false(lv$present)
  expect_true(lv$on)
  expect_false(lv$switch)
  expect_true(lv$removable)
  h <- html(.table_cards(list(lv)))
  expect_false(grepl("rp-card-off", h, fixed = TRUE))
  expect_false(grepl("rp-card-absent", h, fixed = TRUE))
  expect_match(h, "Not set (not in the code)", fixed = TRUE)
  expect_false(grepl(">This table<", h, fixed = TRUE))
  # + Add a card: every verb in the pipe's order, those shown greyed
  menu <- .plan_add_menu(items)
  fns <- vapply(menu, function(m) m$fn, "")
  expect_false(any(c("normalize_ard", "table_plan", "plan_cells") %in% fns))
  expect_false(menu[[match("plan_sort", fns)]]$ok)
  expect_true(menu[[match("plan_compare", fns)]]$ok)
  h <- html(.plan_add_menu_ui("b_add_pick", menu))
  expect_match(h, "dropdown-item disabled", fixed = TRUE)
  expect_match(h, "+ Add a card", fixed = TRUE)
})

test_that("an added plan_stat_groups goes after plan_cells, before plan_total and plan_labels", {
  skip_on_cran()
  # (the schema's rank of tflspec >= 0.0.24.9131: cells 30, stat_groups 40,
  # total 50 ... labels 80)
  skip_if(packageVersion("tflspec") < "0.0.24.9131")
  p <- sample_planner()
  for (id in c("T-14-1-1", "T-14-3-1")) {
    fns <- fns_of(cards_of(p, id, hier = id == "T-14-3-1", added = "plan_stat_groups"))
    at <- match("plan_stat_groups", fns)
    expect_gt(at, match("plan_cells", fns))
    for (later in intersect(c("plan_total", "plan_labels"), fns)) {
      expect_lt(at, match(later, fns), label = paste(id, later))
    }
  }
})

test_that("added: a verb with usual values is written; one without waits, on, for a value", {
  skip_on_cran()
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
  # (#440: drawn as the table's own card, nothing written yet)
  expect_match(h, "Not set (not in the code)", fixed = TRUE)
  expect_match(h, "rp-card-new", fixed = TRUE)
  expect_match(h, "rp-tool-r", fixed = TRUE)
  expect_match(.builder_cards_js, "rp-card-new", fixed = TRUE)
})

test_that("the Total card: on by default with overall rows; off and no switch without", {
  skip_on_cran()
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
  t0 <- item(cards_of(p, "T-14-1-1", ard_total = "none", added = "plan_total"), "plan_total")
  expect_false(t0$switch)
  h <- html(.table_cards(list(t0)))
  expect_false(grepl("form-switch", h, fixed = TRUE))
  expect_false(grepl("disabled", h, fixed = TRUE))
  expect_match(h, "The ARD has no overall rows.", fixed = TRUE)
  expect_identical(tr("The ARD has no overall rows.", "ja"),
                   "ARD に全体の行がありません")
  t1 <- item(cards_of(p, "T-14-1-1", ard_total = "overall", added = "plan_total"), "plan_total")
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

test_that("normalize_ard(): a form card -- the hierarchy a field marked From the ARD, the rest its fields (#417)", {
  skip_on_cran()
  p <- sample_planner()
  hier <- c("AEBODSYS", "AEDECOD")
  it <- item(builder_items(p, "T-14-3-1"), "normalize_ard")
  h <- html(.plan_form_body("n", "normalize_ard", it, ard = list(hierarchy = hier),
                            fixed_link = shiny::tags$a(id = "lk", "step 1")))
  expect_match(h, 'data-from="ard"', fixed = TRUE)
  expect_match(h, ">From the ARD<", fixed = TRUE)
  expect_match(h, 'id="lk"', fixed = TRUE)
  # (a field of its own, the ARD's value on it; its warning hidden while
  # it says the ARD's)
  expect_match(h, 'id="n__hierarchy"', fixed = TRUE)
  expect_match(h, 'data-ard="AEBODSYS|AEDECOD"', fixed = TRUE)
  expect_match(h, 'rp-ard-warn" style="display: none;"', fixed = TRUE)
  expect_match(h, 'id="n__overall"', fixed = TRUE)
  expect_match(h, 'value="Any TEAE"', fixed = TRUE)
  expect_match(h, ">This table<", fixed = TRUE)
  # the arguments the code writes above, the rest below, each in the
  # function's order -- the same fields as any table's
  args <- gsub('data-arg="|"', "", regmatches(h, gregexpr('data-arg="[^"]+"', h))[[1L]])
  expect_identical(args, c("hierarchy", "overall", "keys", "drop_contexts", "drop_key_variables",
                           "stack", "stack_sep", "stack_nest"))
  expect_match(h, "Not in the code (6)", fixed = TRUE)
  expect_false(grepl("rp-badge-unsaved", h, fixed = TRUE))
  # a summary table's (the study's defaults: none): every argument below
  i0 <- item(builder_items(p, "T-14-1-1"), "normalize_ard")
  h0 <- html(.plan_form_body("n", "normalize_ard", i0, ard = list(hierarchy = character())))
  expect_match(h0, "Not in the code (8)", fixed = TRUE)
  # its hierarchy an ordinary field below (the code does not write it)
  expect_false(grepl("data-ard=", h0, fixed = TRUE))
  expect_match(h0, 'id="n__hierarchy"', fixed = TRUE)
  # another hierarchy than the ARD's: warned, not stopped
  it2 <- it
  it2$args$hierarchy <- rev(hier)
  h2 <- html(.plan_form_body("n", "normalize_ard", it2, ard = list(hierarchy = hier)))
  expect_false(grepl('rp-ard-warn" style="display: none;"', h2, fixed = TRUE))
  expect_match(h2, "Not the ARD analysis", fixed = TRUE)
  expect_match(h0, 'id="n__stack"', fixed = TRUE)
  expect_match(html(.plan_form_body("n", "normalize_ard", i0, lang = "ja")),
               tr(.schema_of("normalize_ard")$args$keys$help, "ja"), fixed = TRUE)
  # read back: the hierarchy kept as it is, the overall row the field's
  inp <- list(n__hierarchy = hier, n__overall = "Any TEAE", n__stack = "", n__stack_sep = "",
              n__stack_nest = FALSE, n__drop_key_variables = FALSE)
  r <- .plan_form_read(inp, "n", "normalize_ard")
  expect_identical(r$hierarchy, hier)
  expect_identical(.plan_form_args(r, it, "normalize_ard"), it$args)
  inp$n__overall <- "Any AE"
  a <- .plan_form_args(.plan_form_read(inp, "n", "normalize_ard"), it, "normalize_ard")
  expect_identical(a, list(hierarchy = hier, overall = "Any AE"))
  # written into the report's YAML: its normalize_ard() only
  q <- .builder_item_write(p, "T-14-3-1", "normalize_ard", a)
  o0 <- .spec_output(p, "T-14-3-1")
  o1 <- .spec_output(q, "T-14-3-1")
  expect_identical(o1$content[[1L]]$args, list(hierarchy = hier, overall = "Any AE"))
  expect_identical(o1$content[-1L], o0$content[-1L])
  expect_identical(sheet_rows(q, "tables", "T-14-3-1")$overall_label, "Any AE")
  # its card's note: one line, what it does with a row before and after under the (i)
  h <- html(.plan_card_note("normalize_ard"))
  expect_match(h, "Shapes the ARD for making the table.", fixed = TRUE)
  expect_match(h, "group1 = TRT01A, group1_level = Drug X", fixed = TRUE)
  expect_match(h, ".label = F, stat_name = n", fixed = TRUE)
  hj <- html(.plan_card_note("normalize_ard", lang = "ja"))
  expect_match(hj, "ARD を表を作りやすい形に整えます。", fixed = TRUE)
  expect_match(hj, "カテゴリの値は .label（行の文字）になります。", fixed = TRUE)
  # every argument's help under Other arguments is said in Japanese too
  helps <- unlist(lapply(names(.plan_meaning), function(fn)
    lapply(.schema_of(fn)$args, `[[`, "help")), use.names = FALSE)
  miss <- unique(helps[vapply(helps, tr, "", lang = "ja") == helps])
  expect_length(miss, 0L)
  expect_match(html(.plan_card_note("plan_levels")), "Sets the order of the levels of the column and row keys", fixed = TRUE)
  # the mark: one of the badges
  expect_match(html(from_badge("ard", "ja")), tr("From the ARD", "ja"), fixed = TRUE)
  expect_identical(tr("From the ARD", "ja"), "ARD から")
})

test_that("the layout cards' forms: their fields, the study's defaults marked, edits written back", {
  skip_on_cran()
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
    # (the arguments the code does not write: below, when there are any)
    if (length(setdiff(.plan_form_schema(fn)$formals, c(names(it$args), .data_args)))) {
      expect_match(h, "Not in the code", fixed = TRUE, info = fn)
    }
  }
  # the kinds of field: a choice, a number, a check
  h <- html(.plan_form_body("c", "plan_blanks", item(items, "plan_blanks")))
  expect_match(h, '<option value="between_groups" selected>', fixed = TRUE)
  expect_match(h, ">Study standard<", fixed = TRUE)
  h <- html(.plan_form_body("c", "plan_paginate_rows", item(items, "plan_paginate_rows")))
  expect_match(h, 'type="number"', fixed = TRUE)
  expect_match(h, ">This table<", fixed = TRUE)
  h <- html(.plan_form_body("c", "plan_style", item(items, "plan_style")))
  expect_match(h, 'id="c__align_count_pct" checked', fixed = TRUE)
  # read back unchanged: nothing to write
  it <- item(items, "plan_blanks")
  inp <- list(c__where = "between_groups", c__first = TRUE, c__last = TRUE, c__counted = TRUE)
  r <- .plan_form_read(inp, "c", "plan_blanks")
  expect_identical(.plan_form_args(r, it, "plan_blanks"), it$args %||% list())
  # a check unticked over the study's TRUE: the report's own FALSE, written
  inp$c__first <- FALSE
  own <- .plan_form_args(.plan_form_read(inp, "c", "plan_blanks"), it, "plan_blanks")
  # (the item whole, the study's values with it: the report's own from now on)
  expect_identical(own, utils::modifyList(it$args, list(first = FALSE)))
  q <- .builder_item_write(p, id, "plan_blanks", own)
  expect_identical(sheet_rows(q, "layout", id)$blank_first, "FALSE")
  expect_identical(item(builder_items(q, id), "plan_blanks")$args$where, "between_groups")
  expect_identical(item(builder_items(q, id), "plan_blanks")$from, "own")
  # a number and a choice
  it <- item(items, "plan_paginate_rows")
  inp <- list(c__max_rows = 25, c__split = "group_force", c__page_by = NULL)
  own <- .plan_form_args(.plan_form_read(inp, "c", "plan_paginate_rows"), it, "plan_paginate_rows")
  q <- .builder_item_write(p, id, "plan_paginate_rows", own)
  expect_identical(sheet_rows(q, "layout", id)$pages_max_rows, "25")
  expect_identical(sheet_rows(q, "layout", id)$pages_split, "group_force")
  # the style's and the row label column's
  it <- item(items, "plan_style")
  own <- .plan_form_args(.plan_form_read(list(c__border = "tfl", c__align_count_pct = TRUE,
                                             c__font_size_half_points = 18), "c", "plan_style"),
                        it, "plan_style")
  expect_identical(own$border, "tfl")
  q <- .builder_item_write(p, id, "plan_style", own)
  expect_identical(item(builder_items(q, id), "plan_style")$args$font_size_half_points, 18L)
  it <- item(items, "plan_stub")
  own <- .plan_form_args(.plan_form_read(list(c__name = "row_label", c__indent = 3, c__before = TRUE),
                                        "c", "plan_stub"), it, "plan_stub")
  expect_identical(own, utils::modifyList(it$args, list(indent = 3L)))
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
  skip_on_cran()
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
  skip_on_cran()
  p <- sample_planner()
  for (id in c("T-14-1-1", "T-14-3-1")) {
    for (it in builder_items(p, id)) {
      if (!it$fn %in% .plan_form_fns) next
      sch <- .plan_form_schema(it$fn)
      inp <- list()
      for (a in sch$formals) {
        v <- it$args[[a]]
        if (identical(.arg_kind(sch$args[[a]]$type), "logical")) v <- isTRUE(v)
        if (!is.null(v)) inp[[paste0("c__", a)]] <- if (is.logical(v)) v else
          if (.arg_kind(sch$args[[a]]$type) %in% c("r", "string") && !is.character(v)) .arg_to_r(v) else v
      }
      own <- .plan_form_args(.plan_form_read(inp, "c", it$fn), it, it$fn)
      expect_identical(args_to_r(it$fn, own), args_to_r(it$fn, it$args %||% list()),
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
    # the columns' order, where it comes from: no factor, no code list
    expect_match(output[[paste0("b", bf$n, "_arms_ui")]]$html,
                 "rp-levels-state text-warning", fixed = TRUE)
    cid <- function(path) paste0("b", bf$n, "_card_", path)
    # table_plan()'s card: its own arguments only (cols dragged and removed
    # as the rows); the block's order and the columns' levels are
    # plan_levels()' card's, the variables shown plan_filter()'s
    h <- output$builder_form$html
    tp <- regmatches(h, regexpr('(?s)data-fn="table_plan".*?data-fn="plan_', h, perl = TRUE))
    expect_length(tp, 1L)
    expect_match(tp, sprintf('data-for="b%d_key">{"plugins":["remove_button","drag_drop"', bf$n),
                 fixed = TRUE)
    for (x in c("_vars", "_shown", "_arms_ui", "_keyorder")) {
      expect_no_match(tp, paste0('id="b', bf$n, x, '"'), fixed = TRUE)
    }
    expect_no_match(h, "Order of the column variables", fixed = TRUE)
    expect_match(tp, "A variable not here is added in the ARD analysis.", fixed = TRUE)
    # "+ Add a card" below the cards: a verb with no usual value -- its
    # card, on, waiting, lit
    h <- output$builder_form$html
    expect_match(h, "rp-add-menu", fixed = TRUE)
    # (no switch but the Total column's own)
    bf <- session$userData$bform
    sw <- vapply(bf$items, function(it) isTRUE(it$switch), NA)
    expect_true(all(vapply(bf$items[sw], function(it) it$fn, "") == "plan_total"))
    session$setInputs(b_add_pick = "plan_compare")
    session$elapse(1000)
    bf <- session$userData$bform
    cp <- .builder_item_at(bf$items, "plan_compare")
    expect_false(is.null(cp))
    expect_true(cp$added)
    expect_false(cp$switch)
    expect_match(output$builder_form$html, "rp-card-new", fixed = TRUE)
    # the menu's Remove: the added card gone, Add offers it again
    session$setInputs(b_card_remove = list(path = "plan_compare", t = 1))
    session$elapse(1000)
    bf <- session$userData$bform
    expect_null(.builder_item_at(bf$items, "plan_compare"))
    expect_true("plan_compare" %in% .plan_addable(bf$items))
    # a card of the report's own, removed: its item taken away; Undo puts
    # the report's YAML back
    if (!"plan_levels" %in% fns()) {
      session$setInputs(b_add_pick = "plan_levels")
      session$elapse(1000)
    }
    expect_true("plan_levels" %in% fns())
    session$setInputs(b_card_remove = list(path = "plan_levels", t = 2))
    session$elapse(1000)
    expect_false("plan_levels" %in% fns())
    bf <- session$userData$bform
    expect_null(.builder_item_at(bf$items, "plan_levels"))
    if (!is.null(bf$undo)) {
      session$setInputs(b_card_undo = 1)
      session$elapse(1000)
      expect_true("plan_levels" %in% fns())
    }
    # a required card cannot be removed
    session$setInputs(b_card_remove = list(path = "plan_cells", t = 3))
    session$elapse(1000)
    expect_true("plan_cells" %in% fns())
    # the variables shown: SEX alone, AGE's rows filtered (the ARD as it is)
    set_in(session, paste0("b", bf$n, "_shown"), "SEX")
    expect_identical(sheet_rows(rv$p, "tables", "T-S1")$keep_where, '!variable %in% "AGE"')
    expect_true("plan_filter" %in% fns())
  })
})

test_that("Remove: only a card of the report's own, or one shown empty (#440)", {
  skip_on_cran()
  p <- sample_planner()
  # an added verb taken away again (off): no card
  expect_null(cards_of(p, "T-14-1-1", added = "plan_compare",
                       off = "plan_compare")[["plan_compare"]])
  expect_false("plan_compare" %in% fns_of(cards_of(p, "T-14-1-1", added = "plan_compare",
                                                   off = "plan_compare")))
  items <- cards_of(p, "T-14-1-1", added = "plan_compare")
  cp <- item(items, "plan_compare")
  expect_true(cp$on)
  expect_true(cp$removable)
  # the study standard's: no Remove (Revert, or this table's own value)
  expect_false(item(items, "plan_stub")$removable)
  for (fn in c("normalize_ard", "table_plan", "plan_cells")) {
    expect_false(isTRUE(item(items, fn)$removable), info = fn)
  }
  # the menu: Remove for the others, none for the required
  h <- html(.table_cards(items))
  card_html <- function(fn) {
    at <- regexpr(sprintf('data-fn="%s"', fn), h, fixed = TRUE)
    nxt <- regexpr('class="rp-card[ "]', substring(h, at + 1L))
    substr(h, at, if (nxt > 0) at + nxt else nchar(h))
  }
  expect_match(card_html("plan_compare"), "rp-tool-remove", fixed = TRUE)
  expect_match(card_html("plan_labels"), "rp-tool-remove", fixed = TRUE)
  for (fn in c("table_plan", "plan_cells")) {
    expect_false(grepl("rp-tool-remove", card_html(fn), fixed = TRUE), info = fn)
  }
  expect_match(.builder_content_js, "b_card_remove", fixed = TRUE)
})

test_that("a closed card's summary: meaning (name) = value, short, not R", {
  it <- list(fn = "plan_paginate_rows", args = list(max_rows = 30L, split = "group_safe"))
  expect_identical(.item_summary(it),
                   paste0(.summary_label("plan_paginate_rows", "max_rows"), " = 30; ",
                          .summary_label("plan_paginate_rows", "split"), " = group_safe"))
  expect_match(.summary_label("plan_paginate_rows", "max_rows"), " (max_rows)", fixed = TRUE)
  # a name with no meaning is the name alone; a map is name: value; R stays R
  expect_identical(.summary_label("f", "x"), "x")
  expect_identical(.summary_value(list(group1 = "SEX", group = "variable")),
                   "group1: SEX, group: variable")
  expect_identical(.summary_value(c("F", "M")), "F, M")
  expect_identical(.summary_value(tflspec::tfl_r("SAFFL == \"Y\"")), "SAFFL == \"Y\"")
  # cut short: each value, and the whole
  long <- list(fn = "plan_levels", args = list(A = strrep("x", 60), B = "y", C = strrep("z", 60)))
  s <- .item_summary(long, width = 50L)
  expect_identical(nchar(s), 50L)
  expect_true(endsWith(s, "\u2026"))
  expect_false(grepl("c(", .item_summary(list(fn = "plan_levels", args = list(SEX = c("F", "M")))),
                     fixed = TRUE))
  expect_identical(.item_summary(list(fn = "plan_total", args = list())), "")
})

test_that("the values' order, data first: this table's, the code list's, the ARD's factor, as seen", {
  o <- .level_order(c("B", "A"), c("A", "B"), c("B", "A"), TRUE)
  expect_identical(o[c("levels", "source")], list(levels = c("B", "A"), source = "table"))
  o <- .level_order(character(), c("A", "B"), c("B", "A", "C"), TRUE)
  expect_identical(o$source, "codelist")
  # (the ARD's values the list does not name: after its own)
  expect_identical(o$levels, c("A", "B", "C"))
  expect_true(o$differs)
  expect_false(.level_order(character(), c("A", "B"), c("A", "B"), TRUE)$differs)
  expect_false(.level_order(character(), c("A", "B"), c("B", "A"), FALSE)$differs)
  o <- .level_order(character(), character(), c("B", "A"), TRUE)
  expect_identical(o[c("levels", "source")], list(levels = c("B", "A"), source = "ard"))
  expect_identical(.level_order(character(), character(), c("B", "A"), FALSE)$source, "seen")
  # a code list: its labels (else values) in its order
  cl <- data.frame(variable = c("SEX", "SEX", "RACE"), value = c("M", "F", "W"),
                   label = c(NA, "Female", "White"), order = c(2, 1, NA),
                   stringsAsFactors = FALSE)
  expect_identical(.codelist_order(cl, "SEX"), c("Female", "M"))
  expect_identical(.codelist_order(cl, "AGE"), character())
  expect_identical(.codelist_order(NULL, "SEX"), character())
  # the words, the same in both places, by source
  expect_null(.level_order_text(list(source = "table")))
  expect_identical(.level_order_text(list(source = "ard"))$text, "The ARD's order (its factor levels).")
  expect_identical(tr("The ARD's order (its factor levels).", "ja"),
                   "ARD \u306e\u9806\uff08\u56e0\u5b50\u306e\u6c34\u6e96\u9806\uff09\u3067\u3059\u3002")
  w <- .level_order_text(list(source = "codelist", differs = TRUE), "ja")
  expect_true(w$warn)
  expect_match(w$text, tr("The code list's order (ARD step 1-1).", "ja"), fixed = TRUE)
  expect_match(w$text, tr("The ARD's order differs from the code list's; the table uses the code list's.", "ja"),
               fixed = TRUE)
  expect_true(.level_order_text(list(source = "seen"))$warn)
  # on plan_levels()' card, a variable's line (#428): the code list's order
  # the ARD does not keep is written; as first seen, a warning
  h <- html(.levels_state(c("F", "M"), from = list(source = "codelist")))
  expect_match(h, "Written in the code: The code list's order", fixed = TRUE)
  expect_match(html(.levels_state("A", from = list(source = "seen"))), "text-warning", fixed = TRUE)
  expect_null(.levels_state("A", from = list(source = "plan")))
})

test_that("plan_levels()' variables: a line until opened, the ARD's order not written (#428)", {
  lv <- c("White", "Black or African American", "Asian", "American Indian or Alaska Native",
          "Native Hawaiian or Other Pacific Islander", "Other")
  expect_identical(.levels_summary(lv), "White, Black or African American, \u2026")
  expect_identical(.levels_summary(c("F", "M")), "F, M")
  expect_identical(.levels_summary("A very long value that is longer than forty characters"),
                   "A very long value that is longer than forty characters")
  # the chips stay drawn (cut to no height): no <details>, no display: none
  h <- html(.levels_var("b1_lv4", "RACE", lv,
                        sortable::rank_list(text = NULL, labels = lv, input_id = "b1_lv4"),
                        ard = lv))
  expect_match(h, "RACE</code>[[:space:]]*:[[:space:]]*<span class=\"rp-levels-sum\">White, Black or African American")
  expect_match(h, "(6)", fixed = TRUE)
  expect_match(h, 'data-input="b1_lv4"', fixed = TRUE)
  expect_match(h, 'class="rp-levels-body"', fixed = TRUE)
  expect_false(grepl("<details", h, fixed = TRUE))
  expect_match(h, "Reorder", fixed = TRUE)
  expect_match(h, "Asian", fixed = TRUE)
  # the ARD's order: not written; another: written, with the way back
  expect_match(h, "rp-st-same\">[[:space:]]*The ARD's order [(]not written in the code[)]")
  expect_match(h, "rp-st-own\" style=\"display: none;\"", fixed = TRUE)
  h2 <- html(.levels_state(rev(lv), lv, nm = "RACE"))
  expect_match(h2, "rp-st-same\" style=\"display: none;\"", fixed = TRUE)
  expect_match(h2, "This table's order (written in the code)", fixed = TRUE)
  expect_match(h2, "data-ard=", fixed = TRUE)
  expect_match(h2, 'class="rp-levels-reset" data-variable="RACE"', fixed = TRUE)
  expect_match(html(.levels_state(rev(lv), lv, nm = "RACE", lang = "ja")),
               tr("Back to the ARD's order", "ja"), fixed = TRUE)
  # the block of variables: nested ones left out of its order
  st <- list(variables = data.frame(variable = c("RACE", "ASIANSUB", "ETHNIC"),
                                    under = c(NA, "RACE: Asian", NA)))
  expect_identical(.levels_block_order(st), c("RACE", "ETHNIC"))
  # a sample table: the ARD keeps SEX as a factor in its code list's order
  skip_if_not_installed("cards")
  p <- sample_planner()
  m <- list(keys = list(TRT01A = c("Drug X", "Control")), by = "TRT01A",
            variables = data.frame(variable = c("AGE", "SEX"), kind = c("continuous", "categorical"),
                                   levels = c(NA, "Female | Male"), n_levels = c(0L, 2L),
                                   stats = "n", label = NA_character_),
            factor_vars = "SEX", factor_keys = "TRT01A")
  st <- builder_read(p, "T-14-1-1", m)
  expect_identical(st$level_ard$SEX, c("Female", "Male"))
  expect_identical(st$variable_ard, c("AGE", "SEX"))
})

test_that("builder_read() tells where each order comes from; a key column's factor counts", {
  skip_on_cran()
  skip_if_not_installed("cards")
  p <- sample_planner()
  st <- builder_read(p, "T-14-1-1")
  # the sample's levels: its code list's order (#417: the report's
  # plan_levels() as tflspec writes it from the code list)
  expect_identical(st$level_from$SEX$source, "codelist")
  expect_null(st$level_from[["AGE"]])
  # no own levels: the report's code list's, as tflspec writes it
  vr <- sheet_rows(p, "variables", "T-14-1-1")
  vr$levels <- NA_character_
  vr$output_id <- NULL
  q <- set_sheet_rows(p, "variables", "T-14-1-1", vr)
  # (the arms of the sample's code list, the ARD's factor the other way round)
  cl <- .codelist_order(sheet_rows(p, "codelists", "T-14-1-1"), "TRT01A")
  expect_length(cl, 2L)
  adsl <- data.frame(TRT01A = factor(rep(cl, each = 3), levels = rev(cl)),
                     SEX = rep(c("F", "M", "F"), 2))
  ard <- cards::ard_stack(adsl, .by = TRT01A, cards::ard_categorical(variables = SEX))
  m <- ard_meta(ard, rtfreporter::normalize_ard(ard))
  expect_true("TRT01A" %in% m$factor_keys)
  st <- builder_read(q, "T-14-1-1", m)
  expect_identical(st$level_from$TRT01A, list(source = "codelist", differs = TRUE))
  expect_identical(st$arms$TRT01A, cl)
  expect_identical(st$auto_levels$TRT01A, st$arms$TRT01A)
  # the key column's factor: plan_levels on, its arms written
  m$factor_vars <- character()
  expect_true("plan_levels" %in% .ard_on_cards(m))
  expect_identical(.plan_starter_args("plan_levels", st, meta = m),
                   list(TRT01A = st$arms$TRT01A))
})
