# The table builder's first bundle of forms (the 7497 review, 2026-10-16):
# plan_columns() (a width a column, the decimal alignment, the row title,
# the automatic width), plan_digits()' rounding, plan_filter() (its
# condition by the condition builder, any, parents), plan_levels() (each
# variable's values dragged, its empty values hidden) -- and the rows of
# table_plan() as layers in the order dragged (the block of variables
# anywhere or nowhere, the stack's keys, a constant heading, the rows'
# text).

html <- function(...) as.character(htmltools::tagList(...))
# (the spec in step with its view, as a study opened: the items read from it)
sample_planner <- function() {
  .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
}
fns_of <- function(items) vapply(items, function(it) it$fn, "")
item <- function(items, fn) items[[match(fn, fns_of(items))]]
own_tables <- function(p, id) {
  tb <- sheet_rows(p, "tables", id)
  tb$output_id <- NULL
  tb
}

test_that("the row layers are written in their order, the block where it is or not at all", {
  expect_identical(.row_layers_read(NA), ".variable")
  expect_identical(.row_layers_read("group = variable"), ".variable")
  expect_identical(.row_layers_read("group1 = SEX | group = variable"), c("SEX", ".variable"))
  # (no block added where the rows have none: a table of one variable)
  expect_identical(.row_layers_read("grp = subgroup | lvl = subgroup_level"),
                   c("subgroup", "subgroup_level"))
  # a constant heading is a layer, as written
  expect_identical(.row_layers_read('group1 = "Overall" | group = variable'),
                   c('"Overall"', ".variable"))
  # written back as they were read
  for (r in c("group = variable", "group1 = SEX | group = variable",
              'group1 = "Overall" | group = variable')) {
    expect_identical(.row_layers_write(.row_layers_read(r)), r)
  }
  # the order dragged: the block outside a key
  expect_identical(.row_layers_write(c(".variable", "SEX")), "group = variable | group1 = SEX")
  expect_identical(.row_layers_write(c("subgroup", "subgroup_level")),
                   "group1 = subgroup | group2 = subgroup_level")
  expect_true(is.na(.row_layers_write(character())))
  expect_true(.is_heading('"Overall"'))
  expect_false(.is_heading("SEX"))
  # the chips' names
  expect_identical(.layer_names(c(".variable", "SEX", '"Overall"')),
                   c("Analysis variables (variable)", "SEX (key)", '"Overall"'))
  expect_identical(.layer_names(".variable", lang = "ja"), "解析変数 (variable)")
  expect_identical(.layer_names(c("subgroup", "subgroup_level", "TRT01A"), lang = "ja",
                                stack = c("subgroup", "subgroup_level")),
                   c("subgroup（積み上げ）", "subgroup_level（積み上げ）", "TRT01A（キー）"))
  expect_identical(.layer_names(c("AEBODSYS", "AEDECOD"), c("AEBODSYS", "AEDECOD")),
                   c("AEBODSYS (hierarchy)", "AEDECOD (label)"))
  expect_identical(.col_key_names("TRT01A", "ja"), c("TRT01A（列キー）" = "TRT01A"))
  # the rows' candidates: a column is in cols or in rows, not both
  expect_identical(.layer_choices(c("TRT01A", "SEX", "AGEGR1"), "TRT01A", "SEX"),
                   c("AGEGR1", ".variable"))
  expect_identical(.layer_choices(c("TRT01A", "SEX"), character(), "SEX", stack = "subgroup = ALL, SEX"),
                   c("TRT01A", "subgroup", "subgroup_level", ".variable"))
  expect_identical(.layer_choices(c("TRT01A", "AEBODSYS"), "TRT01A", character(), hier = TRUE),
                   "AEBODSYS")
  expect_match(.heading_create_js, "return {value: v, text: v}", fixed = TRUE)
})

test_that("the stack's keys, and the rows' text read and written", {
  expect_identical(.stack_keys(NA), character())
  expect_identical(.stack_keys("subgroup = ALL, SEX, AGEGR1"), c("subgroup", "subgroup_level"))
  expect_identical(.stack_keys("subgroup = ALL, AGEGR1 * SEX", "TRUE"),
                   c("subgroup", "subgroup_outer", "subgroup_level"))
  expect_identical(.stack_keys("a = SEX | b = AGEGR1"), c("a", "a_level", "b", "b_level"))
  expect_identical(.row_label_read(NA), ".label")
  expect_identical(.row_label_read("label = AEDECOD"), "AEDECOD")
  expect_true(is.na(.row_label_read("NA")))
  expect_identical(.row_label_read("label = ~paste(a, b)"), "own")
  expect_true(is.na(.row_label_write(".label")))
  expect_identical(.row_label_write(NA_character_), "NA")
  expect_identical(.row_label_write("SEX"), "label = SEX")
})

test_that("an Overall Response table: the stack's subgroups as the rows, no block of variables", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-1"
  tb <- own_tables(p, id)
  tb$stack <- "subgroup = ALL, SEX, AGEGR1"
  p <- set_sheet_rows(p, "tables", id, tb)
  st <- builder_read(p, id)
  # the stack's keys are offered to the rows
  expect_true(all(c("subgroup", "subgroup_level") %in% st$layer_choices))
  expect_identical(st$row_label, ".label")
  sx <- st
  sx$layers <- c("subgroup", "subgroup_level")
  q <- builder_write(p, id, sx, was = st)
  expect_identical(own_tables(q, id)$rows, "group1 = subgroup | group2 = subgroup_level")
  expect_identical(builder_read(q, id)$layers, c("subgroup", "subgroup_level"))
  code <- paste(program_code(q, id), collapse = "\n")
  expect_match(code, 'subgroup = c("ALL", "SEX", "AGEGR1")', fixed = TRUE)
  expect_match(code, 'rows = c(group1 = "subgroup", group2 = "subgroup_level")', fixed = TRUE)
  expect_false(grepl('group = "variable"', code, fixed = TRUE))
  # the rows' text: none (NA)
  s2 <- builder_read(q, id)
  s3 <- s2
  s3$row_label <- NA_character_
  q2 <- builder_write(q, id, s3, was = s2)
  expect_identical(own_tables(q2, id)$label, "NA")
  expect_true(is.na(builder_read(q2, id)$row_label))
  # a key as the rows' text: no row layer then
  s4 <- builder_read(q2, id)
  s5 <- s4
  s5$row_label <- "subgroup_level"
  q3 <- builder_write(q2, id, s5, was = s4)
  expect_identical(own_tables(q3, id)$label, "label = subgroup_level")
  expect_identical(own_tables(q3, id)$rows, "group1 = subgroup")
  # nothing changed, nothing written
  s6 <- builder_read(q3, id)
  expect_identical(builder_write(q3, id, s6, was = s6), q3)
})

test_that("a column the plan makes keeps the plan's order, not its code list's", {
  o <- .level_order(codelist = c("SEX", "ALL"), ard = c("ALL", "SEX"), plan_made = TRUE)
  expect_identical(o$levels, c("ALL", "SEX"))
  expect_identical(o$source, "plan")
  # nothing said of it on the cards
  expect_null(.level_order_text(o))
  # its own order (the table's) still wins
  expect_identical(.level_order(own = "SEX", ard = c("ALL", "SEX"), plan_made = TRUE)$source,
                   "table")
  # a data variable: the code list's, as before
  expect_identical(.level_order(codelist = c("SEX", "ALL"), ard = c("ALL", "SEX"))$source,
                   "codelist")
})

test_that("plan_columns(): a width a column, the decimal alignment, the row title, written by column", {
  skip_on_cran()
  sch <- .plan_form_schema("plan_columns")
  expect_identical(sch$visible, c("widths", "decimal", "row_title", "auto_width"))
  # (given as R: under Other arguments)
  expect_identical(sch$args$cell_format$type, "r")
  expect_identical(sch$args$column_widths_twips$type, "r")
  p <- sample_planner()
  it <- item(builder_items(p, "T-14-1-1"), "plan_columns")
  cols <- .widths_cols(it$args$widths, c("row_label", ".values", "Placebo"))
  expect_identical(cols, c("row_label", ".values", "Placebo"))
  h <- html(.plan_form_body("c", "plan_columns", it, choices = cols, cols = cols))
  expect_match(h, 'id="c__widths__1"', fixed = TRUE)
  expect_match(h, 'value="40"', fixed = TRUE)
  expect_match(h, 'id="c__widths__3"', fixed = TRUE)
  expect_match(h, "rp-widths", fixed = TRUE)
  # read back as the sheets give them: unchanged, no own arguments
  inp <- list(c__widths__1 = "40", c__widths__2 = "18", c__widths__3 = "", c__auto_width = FALSE)
  r <- .plan_form_read(inp, "c", "plan_columns", cols = cols)
  expect_identical(r$widths, list(row_label = 40, .values = 18))
  own <- .plan_form_args(r, it, "plan_columns")
  expect_identical(args_to_r("plan_columns", own), args_to_r("plan_columns", it$args %||% list()))
  # a width, the alignment and the automatic width of the table's own
  inp$c__widths__3 <- "25"
  inp$c__decimal <- ".values"
  inp$c__auto_width <- TRUE
  own <- .plan_form_args(.plan_form_read(inp, "c", "plan_columns", cols = cols), it, "plan_columns")
  q <- .builder_item_write(p, "T-14-1-1", "plan_columns", own)
  co <- sheet_rows(q, "columns", "T-14-1-1")
  expect_setequal(co$column, c("row_label", ".values", "Placebo"))
  expect_identical(co$rel_width[co$column == "Placebo"], "25")
  expect_identical(co$decimal_split[co$column == ".values"], "TRUE")
  expect_identical(sheet_rows(q, "style", "T-14-1-1")$auto_width, "TRUE")
  it2 <- item(builder_items(q, "T-14-1-1"), "plan_columns")
  expect_identical(it2$args$widths$Placebo, 25)
  expect_true(isTRUE(it2$args$auto_width))
  # taken away: the study's widths again, no empty rows left
  r0 <- .builder_item_revert(q, "T-14-1-1", "plan_columns")
  expect_identical(nrow(sheet_rows(r0, "columns", "T-14-1-1")), 0L)
})

test_that("plan_digits()' rounding: a choice, the study's (written there)", {
  skip_on_cran()
  h <- html(.rounding_field("d", "sas"))
  expect_match(h, 'id="d__rounding"', fixed = TRUE)
  for (v in c("sas", "r", "iec60559")) expect_match(h, sprintf('value="%s"', v), fixed = TRUE)
  expect_match(h, "Study standard", fixed = TRUE)
  p <- sample_planner()
  it <- item(builder_items(p, "T-14-1-1"), "plan_digits")
  args <- it$own_args %||% it$args
  args$rounding <- "r"
  q <- .builder_item_write(p, "T-14-1-1", "plan_digits", args)
  expect_identical(unname(q$study[["rounding"]]), "r")
  expect_identical(item(builder_items(q, "T-14-1-1"), "plan_digits")$args$rounding, "r")
  expect_identical(tr("Rounding", "ja"), "丸め方 (rounding)")
})

test_that("plan_filter(): its condition for the condition builder, any and parents as fields", {
  skip_on_cran()
  expect_identical(.plan_form_main("plan_filter"), c("any", "parents"))
  sch <- .plan_form_schema("plan_filter")
  expect_identical(sch$args$by$type, "r")
  expect_null(.filter_where_text(NULL))
  expect_identical(.filter_where_text(tflspec::tfl_r("p >= 0.05")), "p >= 0.05")
  expect_identical(.filter_where_text(list(tflspec::tfl_r("p >= 0.05"), tflspec::tfl_r("n > 2"))),
                   "(p >= 0.05) & (n > 2)")
  m <- list(keys = list(TRT01A = c("A", "B", "C")),
            variables = data.frame(variable = c("AGE", "SEX"), levels = c("", "F | M")),
            stats = c("n", "p"))
  d <- .filter_data(m)
  expect_identical(names(d), c("TRT01A", "variable", "variable_level", "n", "p"))
  expect_identical(.cond_type(d$p), "num")
  expect_null(.filter_data(NULL))
  # the condition and the switches written to the sheets, read back
  p <- sample_planner()
  q <- .builder_item_write(p, "T-14-1-1", "plan_filter",
                           list(where = tflspec::tfl_r("p >= 0.05"), any = FALSE, parents = "drop"))
  tb <- own_tables(q, "T-14-1-1")
  expect_identical(tb$keep_where, "p >= 0.05")
  expect_identical(tb$keep_any, "FALSE")
  expect_identical(tb$keep_parents, "drop")
  it <- item(builder_items(q, "T-14-1-1"), "plan_filter")
  inp <- list(c__any = FALSE, c__parents = "drop")
  own <- .plan_form_args(.plan_form_read(inp, "c", "plan_filter"), it, "plan_filter")
  expect_identical(args_to_r("plan_filter", own), args_to_r("plan_filter", it$args))
  h <- html(.plan_form_body("c", "plan_filter", it))
  expect_match(h, 'id="c__parents"', fixed = TRUE)
  expect_match(h, 'id="c__any"', fixed = TRUE)
})

test_that("plan_levels(): each variable's values dragged, its empty values hidden", {
  skip_on_cran()
  p <- sample_planner()
  st <- builder_read(p, "T-14-1-1")
  vr <- data.frame(variable = c("SEX", "RACE"), empty_levels = c("hide", NA))
  h <- html(.levels_body(function(x) paste0("b_", x), st, sheet_rows(p, "codelists", "T-14-1-1"),
                         empty = vr))
  i <- match("SEX", st$variables$variable)
  expect_match(h, sprintf('id="b_lv%d"', i), fixed = TRUE)
  expect_match(h, sprintf('id="b_lvdrop%d" class="form-check-input me-1" checked', i), fixed = TRUE)
  expect_match(h, "(.drop_empty)", fixed = TRUE)
  # the column variables' levels and the block's order are this card's
  # (table_plan()'s card has only its own arguments)
  expect_no_match(h, "ordered on the columns", fixed = TRUE)
  expect_no_match(h, 'id="b_vars"', fixed = TRUE)
  h2 <- html(.levels_body(function(x) paste0("b_", x), st, vars = list(SEX = "SEX", AGE = "AGE"),
                          cols = shiny::div(id = "the-cols")))
  expect_match(h2, 'id="b_vars"', fixed = TRUE)
  expect_match(h2, "(variable)", fixed = TRUE)
  expect_match(h2, 'id="the-cols"', fixed = TRUE)
  expect_identical(.empty_levels_of(vr), list(SEX = TRUE, RACE = FALSE))
  expect_identical(.empty_levels_of(NULL), list())
  # the labels' card has no order of the levels any more
  expect_false(grepl("Levels (drag to order)", paste(deparse(body(app_server)), collapse = ""),
                     fixed = TRUE))
})

test_that("the new words have their Japanese", {
  w <- c("Analysis variables (variable) are no rows (a table of one variable: ORR ...).",
         "None (NA)", "The ARD's row text (.label)", "(the last level)",
         "Blank: rtfreporter's width. .values: every value column.",
         "No categorical variable to order.")
  ja <- vapply(w, tr, "", lang = "ja")
  expect_true(all(ja != w), info = paste(w[ja == w], collapse = " / "))
  expect_identical(tr(w[[1L]], "ja"),
                   "解析変数 (variable) は行になりません（1 変数の表、ORR など）")
})

test_that("plan_labels(): the labels of the columns the plan makes are the item's, the data's code lists kept", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-1"
  before <- sheet_rows(p, "codelists", id)
  it <- item(builder_items(p, id), "plan_labels")
  q <- .builder_item_write(p, id, "plan_labels",
                           c(it$args, list(subgroup = c(ALL = "All Subjects", SEX = "Sex"))))
  got <- item(builder_items(q, id), "plan_labels")$args$subgroup
  expect_identical(got, c(ALL = "All Subjects", SEX = "Sex"))
  # (the report's YAML says it; the data's code lists as they were)
  o <- .spec_output(q, id)
  pl <- Filter(function(x) identical(x$fn, "plan_labels"), o$content)[[1L]]
  expect_identical(pl$args$subgroup, c(ALL = "All Subjects", SEX = "Sex"))
  cl <- sheet_rows(q, "codelists", id)
  keep <- cl[cl$variable %in% before$variable, names(before)]
  expect_identical(keep$label, before$label)
})

# ---- the second bundle: the cards "Add" brings (2026-10-16) -----------------

test_that("the ARD's comparison records: the analyses with no column key, their numeric statistics", {
  d <- data.frame(TRT01A = c("A", "B", NA, NA, NA, NA, NA),
                  variable = c("AGE", "AGE", "AGE", "AGE", "AGE", "SEX", "AGE"),
                  context = c("summary", "summary", "summary", "stats_t_test", "stats_t_test",
                              "stats_chisq_test", "attributes"),
                  stat_name = c("mean", "mean", "mean", "p.value", "method", "p.value", "label"),
                  stat = c(1, 2, 3, 0.04, NA, 0.5, NA))
  expect_identical(.ard_comparisons(d, "TRT01A"),
                   list(stats_t_test = "p.value", stats_chisq_test = "p.value"))
  expect_identical(.ard_comparisons(d[1:3, ], "TRT01A"), list())
  expect_identical(.ard_comparisons(d, character()), list())
  ch <- .compare_choices("p", list(stats_t_test = c("p.value", "estimate"), stats_chisq_test = "p.value"))
  expect_identical(unname(ch), c("p", "p.value", "estimate"))
  expect_identical(names(ch)[2L], "p.value (stats_t_test, stats_chisq_test)")
})

test_that("plan_compare(): its columns chosen, each its template, written with its cells rows", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-1"
  cells0 <- sheet_rows(p, "cells", id)
  q <- .builder_item_write(p, id, "plan_compare", list(p = "{p.value:.3f}", by = "TRT01A"))
  expect_identical(own_tables(q, id)$compare, "p")
  cl <- sheet_rows(q, "cells", id)
  expect_identical(cl$template[cl$variable == "p"], "{p.value:.3f}")
  it <- item(builder_items(q, id), "plan_compare")
  expect_identical(it$args$p, "{p.value:.3f}")
  h <- html(.compare_body("c", it, list(stats_t_test = c("p.value", "estimate")), by_choices = "TRT01A"))
  expect_match(h, 'id="c__cols"', fixed = TRUE)
  expect_match(h, 'id="c__tpl__1"', fixed = TRUE)
  expect_match(h, 'id="c__by"', fixed = TRUE)
  expect_match(h, 'id="c__position"', fixed = TRUE)
  expect_match(h, "(...)", fixed = TRUE)
  # drawn and read back unchanged: nothing to write
  drawn <- .dots_args("plan_compare", it$args)
  inp <- list(c__cols = "p", c__tpl__1 = "{p.value:.3f}", c__by = "TRT01A", c__position = "")
  r <- .compare_read(inp, "c", drawn)
  expect_identical(.dots_form_args(r, it, "plan_compare"), it$args)
  expect_null(.compare_read(list(), "c", drawn))
  # a column more (a statistic chosen: its template {name}), the place first
  inp$c__cols <- c("estimate", "p")
  inp$c__position <- "first"
  own <- .dots_form_args(.compare_read(inp, "c", drawn), it, "plan_compare")
  expect_identical(names(own), c("estimate", "p", "by", "position"))
  expect_identical(own$estimate, "{estimate}")
  q2 <- .builder_item_write(q, id, "plan_compare", own)
  expect_identical(own_tables(q2, id)$compare, "estimate | p")
  expect_identical(own_tables(q2, id)$compare_position, "first")
  cl2 <- sheet_rows(q2, "cells", id)
  expect_setequal(cl2$variable[cl2$variable %in% c("p", "estimate")], c("p", "estimate"))
  # a column taken away: its cells row too; the rest of the cells as they were
  inp$c__cols <- "estimate"
  it2 <- item(builder_items(q2, id), "plan_compare")
  q3 <- .builder_item_write(q2, id, "plan_compare",
                            .dots_form_args(.compare_read(inp, "c", .dots_args("plan_compare", it2$args)),
                                           it2, "plan_compare"))
  expect_false("p" %in% sheet_rows(q3, "cells", id)$variable)
  # taken away: the report's cells as before
  r0 <- .builder_item_revert(q3, id, "plan_compare")
  expect_identical(sheet_rows(r0, "cells", id)$template, cells0$template)
  expect_true(is.na(own_tables(r0, id)$compare))
  # a template of R (a map by test): shown as R, kept
  it3 <- list(fn = "plan_compare", args = list(p = c(stats_t_test = "{p.value:.3f}")))
  h3 <- html(.compare_body("c", it3))
  expect_false(grepl('id="c__tpl__1"', h3, fixed = TRUE))
  expect_match(h3, "Edit as R", fixed = TRUE)
  expect_match(h3, "no comparison records", fixed = TRUE)
  r3 <- .compare_read(list(c__cols = "p", c__position = ""), "c", .dots_args("plan_compare", it3$args))
  expect_identical(r3$p, c(stats_t_test = "{p.value:.3f}"))
})

test_that("plan_stat_groups(): a group a row (its name, its statistics), .variables and .others", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-1"
  q <- .builder_item_write(p, id, "plan_stat_groups",
                           list(n = "N", orr = c("n", "estimate"), .variables = "AGE", .others = "n"))
  expect_identical(own_tables(q, id)$stat_groups, "n = N | orr = n, estimate | .variables = AGE | .others = n")
  it <- item(builder_items(q, id), "plan_stat_groups")
  h <- html(.stat_groups_body("c", it, stats = c("N", "n", "estimate"), variables = c("AGE", "SEX")))
  expect_match(h, 'id="c__g__1__name"', fixed = TRUE)
  expect_match(h, 'id="c__g__3__name"', fixed = TRUE)   # the new group's row
  expect_match(h, 'id="c__g__2__stats"', fixed = TRUE)
  expect_match(h, 'id="c__.variables"', fixed = TRUE)
  expect_match(h, 'id="c__.others"', fixed = TRUE)
  inp <- list(c__g__1__name = "n", c__g__1__stats = "N",
              c__g__2__name = "orr", c__g__2__stats = c("n", "estimate"),
              c__g__3__name = "", c__g__3__stats = NULL,
              c__.variables = "AGE", c__.others = "n")
  r <- .stat_groups_read(inp, "c", 2L)
  expect_identical(.dots_form_args(r, it, "plan_stat_groups"), it$args)
  expect_null(.stat_groups_read(list(), "c", 2L))
  # a new group in the spare row; a group with no statistic is none
  inp$c__g__3__name <- "ci"
  inp$c__g__3__stats <- c("conf.low", "conf.high")
  inp$c__g__1__stats <- NULL
  own <- .dots_form_args(.stat_groups_read(inp, "c", 2L), it, "plan_stat_groups")
  expect_identical(names(own), c("orr", "ci", ".variables", ".others"))
  q2 <- .builder_item_write(q, id, "plan_stat_groups", own)
  expect_identical(own_tables(q2, id)$stat_groups,
                   "orr = n, estimate | ci = conf.low, conf.high | .variables = AGE | .others = n")
})

test_that("plan_nest(): a variable under a level of another, on its own card", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-1"
  st <- builder_read(p, id)
  vars <- .nest_vars(st)
  expect_true(all(c("SEX", "RACE") %in% vars))
  expect_identical(.nest_value("RACE: Asian"), list(RACE = "Asian"))
  expect_null(.nest_value(""))
  expect_identical(.nest_text(list(RACE = "Asian")), "RACE: Asian")
  q <- .builder_item_write(p, id, "plan_nest", list(ETHNIC = list(RACE = "Asian")))
  vr <- sheet_rows(q, "variables", id)
  expect_identical(vr$under[vr$variable == "ETHNIC"], "RACE: Asian")
  it <- item(builder_items(q, id), "plan_nest")
  h <- html(.nest_body("c", it, st))
  i <- match("ETHNIC", vars)
  expect_match(h, sprintf('id="c__nest__%d"', i), fixed = TRUE)
  expect_match(h, '<option value="RACE: Asian" selected>', fixed = TRUE)
  inp <- stats::setNames(as.list(rep("", length(vars))), paste0("c__nest__", seq_along(vars)))
  inp[[paste0("c__nest__", i)]] <- "RACE: Asian"
  expect_identical(.dots_form_args(.nest_read(inp, "c", vars, it), it, "plan_nest"), it$args)
  # taken away: its own block again
  inp[[paste0("c__nest__", i)]] <- ""
  own <- .dots_form_args(.nest_read(inp, "c", vars, it), it, "plan_nest")
  expect_length(own, 0L)
  q2 <- .builder_item_write(q, id, "plan_nest", own)
  vr2 <- sheet_rows(q2, "variables", id)
  expect_true(is.na(vr2$under[vr2$variable == "ETHNIC"]))
  expect_match(html(.nest_body("c", list(fn = "plan_nest", args = list()),
                               list(variables = data.frame(variable = "AGE", kind = "continuous")))),
               "No categorical variable", fixed = TRUE)
  # the labels' card has no field of its own for it any more
  expect_false(grepl('paste0("under", i)', paste(deparse(body(app_server)), collapse = ""),
                     fixed = TRUE))
})

test_that("a nest taken away keeps the variable where it was shown (#428)", {
  skip_on_cran()
  p <- sample_planner()
  id <- "T-14-1-5"
  order_of <- function(q) as.character(unlist(.builder_args(builder_items(q, id), "plan_levels")$variable))
  # ASIANSUB is shown after RACE, its parent; the order of the variables
  # leaves the nested one out
  st <- builder_read(p, id)
  expect_identical(st$variables$variable[match(c("RACE", "ASIANSUB", "ETHNIC"), st$variables$variable)],
                   c("RACE", "ASIANSUB", "ETHNIC"))
  expect_false("ASIANSUB" %in% order_of(p))
  want <- c("AGE", "AGEGR1", "SEX", "RACE", "ASIANSUB", "ETHNIC")
  # the card removed (or off), or its entry emptied: after RACE, not last
  q <- .builder_item_revert(p, id, "plan_nest")
  expect_identical(order_of(q), want)
  expect_identical(builder_read(q, id)$variables$variable[1:6], want)
  q2 <- .builder_item_write(p, id, "plan_nest", list())
  expect_identical(order_of(q2), want)
  # (nothing freed: the order as it was)
  q3 <- .builder_item_write(p, id, "plan_nest", list(ASIANSUB = list(RACE = "Asian")))
  expect_identical(order_of(q3), order_of(p))
})

test_that("plan_paginate_group(), plan_paginate_cols(), plan_row_group(): a form line each", {
  skip_on_cran()
  expect_identical(.plan_form_main("plan_paginate_group"), c("col", "keep", "heading"))
  expect_identical(.plan_form_main("plan_paginate_cols"),
                   c("every", "fit", "at", "keep", "cut_by", "order", "allow_span_break"))
  expect_identical(.plan_form_main("plan_row_group"), c("mode", "collapse"))
  # given as R: under Other arguments, saved
  expect_identical(.plan_form_schema("plan_row_group")$args$group_col$type, "r")
  expect_identical(.plan_form_schema("plan_paginate_cols")$args$col_header$type, "r")
  # the fields in the function's order (its formals)
  expect_identical(.plan_form_schema("plan_paginate_cols")$visible,
                   c("at", "cut_by", "every", "keep", "fit", "allow_span_break", "order"))
  h <- html(.plan_form_body("c", "plan_paginate_cols", list(fn = "plan_paginate_cols", args = list())))
  expect_lt(regexpr('data-arg="at"', h, fixed = TRUE), regexpr('data-arg="every"', h, fixed = TRUE))
  expect_no_match(h, "rp-badge-unsaved", fixed = TRUE)
  # keep = TRUE by the function: ticked, and no change while it stays so
  sch <- .plan_form_schema("plan_paginate_group")
  expect_true(isTRUE(sch$args$keep$default))
  it0 <- list(fn = "plan_paginate_group", args = list(col = "PARAM"), own_args = list(col = "PARAM"))
  h <- html(.plan_form_body("c", "plan_paginate_group", it0, choices = c("PARAM", "AVISIT")))
  expect_match(h, 'id="c__keep" checked', fixed = TRUE)
  r <- .plan_form_read(list(c__col = "PARAM", c__keep = TRUE, c__heading = ""), "c", "plan_paginate_group")
  expect_identical(.plan_form_args(r, it0, "plan_paginate_group"), it0$args)
  r$keep <- FALSE
  expect_false(.plan_form_args(r, it0, "plan_paginate_group")$keep)
  # written and read back through the sheets
  p <- sample_planner()
  id <- "T-14-1-1"
  q <- .builder_item_write(p, id, "plan_paginate_group", list(col = "SEX", keep = FALSE, heading = "Sex: {value}"))
  la <- sheet_rows(q, "layout", id)
  expect_identical(c(la$group_col, la$group_keep, la$group_heading), c("SEX", "FALSE", "Sex: {value}"))
  # column positions as whole numbers, a lone TRUE a logical
  expect_identical(.form_list_value(c("2", "4"), "list<integer>|list<column>"), c(2L, 4L))
  expect_identical(.form_list_value("row_label", "list<integer>|list<column>"), "row_label")
  expect_true(.form_list_value("TRUE", "list<integer>|list<column>|logical"))
  r2 <- .plan_form_read(list(c__every = "3", c__fit = TRUE, c__at = c("2", "4"), c__keep = "row_label",
                             c__cut_by = "", c__order = "down", c__allow_span_break = FALSE),
                        "c", "plan_paginate_cols")
  expect_identical(r2$at, c(2L, 4L))
  expect_identical(r2$order, "down")
  q2 <- .builder_item_write(p, id, "plan_paginate_cols", r2)
  it2 <- item(builder_items(q2, id), "plan_paginate_cols")
  expect_identical(it2$args$at, c(2L, 4L))
  expect_identical(it2$args$every, 3L)
  h2 <- html(.plan_form_body("c", "plan_paginate_cols", it2, choices = c("row_label", ".values")))
  expect_match(h2, 'value="down" selected', fixed = TRUE)
  r3 <- .plan_form_read(list(c__mode = "indent", c__collapse = "TRUE"), "c", "plan_row_group")
  expect_identical(r3, list(mode = "indent", collapse = TRUE))
  q3 <- .builder_item_write(p, id, "plan_row_group", r3)
  expect_identical(item(builder_items(q3, id), "plan_row_group")$args, list(mode = "indent", collapse = TRUE))
})

test_that("the second bundle's words have their Japanese", {
  w <- c("Variables split", "Group of the other variables", "Group boundary",
         "A repeat printed once", "Keep the column", "Group and its statistics", "Group name",
         "As R: Edit as R changes it.", "Choose or type a column's name", "A new group",
         "No categorical variable to nest.", "Every row key (TRUE)", "Rows under a level",
         "Group column",
         "The ARD has no comparison records (a test, a difference): add one in step 1, then read the ARD again.")
  ja <- vapply(w, tr, "", lang = "ja")
  expect_true(all(ja != w), info = paste(w[ja == w], collapse = " / "))
})

test_that("a comparison's rows (no arm row in their context) are not overall rows", {
  d <- data.frame(
    variable = c("AGE", "AGE", "AGE", "AGE"),
    TRT01A = c("A", "B", NA, NA),
    context = c("continuous", "continuous", "ttest", "ttest"),
    stringsAsFactors = FALSE
  )
  # a test only: no Total
  expect_false(.ard_has_overall(d, "TRT01A"))
  # the same analysis without by: a Total
  d2 <- rbind(d, data.frame(variable = "AGE", TRT01A = NA, context = "continuous"))
  expect_true(.ard_has_overall(d2, "TRT01A"))
})

test_that("plan_hide(): the table's columns hidden, a multi-select, written by column", {
  skip_on_cran()
  expect_identical(.plan_form_main("plan_hide"), "cols")
  sch <- .plan_form_schema("plan_hide")
  expect_identical(sch$formals, "cols")
  it0 <- list(fn = "plan_hide", args = list(), own_args = list())
  h <- html(.plan_form_body("c", "plan_hide", it0, choices = c("row_label", ".values", "Placebo")))
  expect_match(h, 'id="c__cols"', fixed = TRUE)
  expect_match(h, "multiple", fixed = TRUE)
  expect_match(h, "(cols)", fixed = TRUE)
  # nothing chosen: no change
  expect_null(.plan_form_read(list(c__cols = NULL), "c", "plan_hide"))
  r <- .plan_form_read(list(c__cols = c("Placebo", "row_label")), "c", "plan_hide")
  expect_identical(r, list(cols = c("Placebo", "row_label")))
  own <- .plan_form_args(r, it0, "plan_hide")
  p <- sample_planner()
  id <- "T-14-1-1"
  q <- .builder_item_write(p, id, "plan_hide", own)
  co <- sheet_rows(q, "columns", id)
  expect_setequal(co$column[co$hide %in% "TRUE"], c("Placebo", "row_label"))
  it <- item(builder_items(q, id), "plan_hide")
  expect_identical(args_to_r("plan_hide", it$args), 'c("Placebo", "row_label")')
  # drawn again: chosen as the sheets say; read back the same is no change
  h2 <- html(.plan_form_body("c", "plan_hide", it, choices = c("row_label", ".values", "Placebo")))
  expect_match(h2, '<option value="Placebo" selected>', fixed = TRUE)
  r2 <- .plan_form_read(list(c__cols = it$args$cols), "c", "plan_hide")
  expect_identical(args_to_r("plan_hide", .plan_form_args(r2, it, "plan_hide")),
                   args_to_r("plan_hide", it$args))
})

test_that("plan_cell_style(): cols, where, the looks as fields, each call its own card", {
  skip_on_cran()
  expect_identical(.plan_form_main("plan_cell_style"),
                   c("cols", "header", "where", "bold", "italic", "align", "color", "background"))
  sch <- .plan_form_schema("plan_cell_style")
  # a look left blank is not written (not FALSE); border: under Other arguments, as R
  expect_identical(.arg_kind(sch$args$bold$type), "enum")
  expect_identical(.arg_kind(sch$args$color$type), "colour")
  expect_identical(sch$args$border$type, "r")
  expect_identical(sch$args$border$type, "r")
  expect_true(isFALSE(sch$args$header$default))
  it0 <- list(fn = "plan_cell_style", args = list(), own_args = list())
  h <- html(.plan_form_body("c", "plan_cell_style", it0, choices = c("row_label", "Placebo")))
  for (a in c("cols", "header", "where", "bold", "italic", "align", "color", "background")) {
    expect_match(h, sprintf('id="c__%s"', a), fixed = TRUE)
  }
  expect_match(h, "Not in the code", fixed = TRUE)
  # blank fields: nothing (the header's check unticked is its default)
  inp <- list(c__cols = NULL, c__header = FALSE, c__where = "", c__bold = "", c__italic = "",
              c__align = "", c__color = "", c__background = "")
  r0 <- .plan_form_read(inp, "c", "plan_cell_style")
  expect_identical(.plan_form_args(r0, it0, "plan_cell_style"), list())
  # the looks as values; the condition without its ~ (the code writes it)
  inp$c__cols <- "Placebo"
  inp$c__where <- "~ is.na(label)"
  inp$c__bold <- "TRUE"
  inp$c__align <- "left"
  inp$c__color <- "#CC0000"
  r <- .plan_form_read(inp, "c", "plan_cell_style")
  expect_true(isTRUE(r$bold))
  expect_identical(as.character(unclass(r$where)), "is.na(label)")
  expect_identical(.formula_text(tflspec::tfl_r("  ~  ")), NULL)
  own <- .plan_form_args(r, it0, "plan_cell_style")
  expect_null(own$italic)
  expect_null(own$header)
  p <- sample_planner()
  id <- "T-14-1-1"
  q <- .builder_item_write(p, id, "plan_cell_style", own)
  # a second call, its own row
  q <- .builder_item_write(q, id, "plan_cell_style", list(cols = "row_label", italic = FALSE), k = 2L)
  cs <- sheet_rows(q, "cell_styles", id)
  expect_identical(nrow(cs), 2L)
  expect_identical(cs$where[1L], "is.na(label)")
  expect_identical(cs$italic[2L], "FALSE")
  its <- Filter(function(x) identical(x$fn, "plan_cell_style"), builder_items(q, id))
  expect_identical(vapply(its, `[[`, "", "path"), c("plan_cell_style", "plan_cell_style#2"))
  expect_identical(args_to_r("plan_cell_style", its[[1L]]$args),
                   'cols = "Placebo", where = ~ is.na(label), bold = TRUE, align = "left", color = "#CC0000"')
  # drawn again as they are: read back the same is no change
  h2 <- html(.plan_form_body("c", "plan_cell_style", its[[2L]], choices = c("row_label", "Placebo")))
  expect_match(h2, '<option value="FALSE" selected>', fixed = TRUE)
  r2 <- .plan_form_read(list(c__cols = "row_label", c__header = FALSE, c__where = "", c__bold = "",
                             c__italic = "FALSE", c__align = "", c__color = "", c__background = ""),
                        "c", "plan_cell_style")
  expect_identical(args_to_r("plan_cell_style", .plan_form_args(r2, its[[2L]], "plan_cell_style")),
                   args_to_r("plan_cell_style", its[[2L]]$args))
  # a colour typed is kept (the select takes one)
  expect_match(html(.field_colour("x", NULL, "#CC0000", NULL)), '"create":true', fixed = TRUE)
})

test_that("plan_col_header(values = ): what {n} counts, header_n kept when it is none of the choices", {
  skip_on_cran()
  ch <- header_n_choices(NULL, "TRT01A", "n = table")
  expect_true("n = table" %in% ch$choices)
  expect_identical(ch$selected, "n = table")
  expect_match(names(ch$choices)[ch$choices == "n = table"], "As the sheets say", fixed = TRUE)
  # one of the choices: not twice
  expect_identical(sum(header_n_choices(NULL, "TRT01A", "table")$choices == "table"), 1L)
  pg <- data.frame(scope = c("page", "page", "table", "table"), page = c("ALT", "ALT", NA, NA),
                   column = c("A", "B", "A", "B"), value = c(10, 12, 50, 52),
                   stringsAsFactors = FALSE)
  attr(pg, "differ") <- TRUE
  ch2 <- header_n_choices(pg, "TRT01A", "N = table")
  expect_identical(ch2$selected, "N = table")
  expect_null(ch2$warn)
  # its alignment (Edit as R) kept: the style sheet's col_header_align
  p <- sample_planner()
  id <- "T-14-1-1"
  it <- item(builder_items(p, id), "plan_col_header")
  q <- .builder_item_write(p, id, "plan_col_header", c(it$args, list(col_header_align = "left")))
  expect_identical(sheet_rows(q, "style", id)$col_header_align, "left")
  expect_identical(item(builder_items(q, id), "plan_col_header")$args$col_header_align, "left")
})

test_that("the third bundle's words have their Japanese", {
  w <- c("Columns styled", "The column header", "Rows styled", "Bold", "Italic", "Text colour",
         "Background colour", "Underline", "Indent (twips)", "As the sheets say: %s",
         "Hidden columns", "Alignment", "Borders")
  ja <- vapply(w, tr, "", lang = "ja")
  expect_true(all(ja != w), info = paste(w[ja == w], collapse = " / "))
})

test_that("every form card shows all its function's arguments: those written above, the rest below (#417)", {
  skip_on_cran()
  p <- sample_planner()
  args_of <- function(h, part) {
    x <- regmatches(h, regexpr(part, h, perl = TRUE))
    if (!length(x)) return(character())
    gsub('data-arg="|"', "", regmatches(x, gregexpr('data-arg="[^"]+"', x))[[1L]])
  }
  seen <- list()
  for (id in c("T-14-1-1", "T-14-3-1")) {
    items <- builder_items(p, id)
    for (fn in .plan_form_fns) {
      it <- Filter(function(x) identical(x$fn, fn), items)
      it <- if (length(it)) it[[1L]] else list(fn = fn, args = list())
      h <- html(.plan_form_body("c", fn, it, cols = c("row_label", ".values")))
      up <- args_of(h, '(?s)rp-args-up.*?(?=rp-more|$)')
      low <- args_of(h, '(?s)rp-more-body.*$')
      fo <- setdiff(.plan_form_schema(fn)$formals, .data_args)
      # all of them, once, each group in the function's order
      expect_setequal(c(up, low), fo)
      expect_false(any(duplicated(c(up, low))), info = fn)
      expect_identical(up, fo[fo %in% up], info = fn)
      expect_identical(low, fo[fo %in% low], info = fn)
      # above: what the call writes
      expect_true(all(intersect(names(it$args), fo) %in% up), info = paste(id, fn))
      seen[[id]][[fn]] <- sort(c(up, low))
    }
  }
  # the same fields whatever the table's kind
  expect_identical(seen[["T-14-1-1"]], seen[["T-14-3-1"]])
  expect_identical(tr("Not in the code", "ja"), "コードに書かれない引数")
})
