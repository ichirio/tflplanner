# The table builder's first bundle of forms (the 7497 review, 2026-10-16):
# plan_columns() (a width a column, the decimal alignment, the row title,
# the automatic width), plan_digits()' rounding, plan_filter() (its
# condition by the condition builder, any, parents), plan_levels() (each
# variable's values dragged, its empty values hidden) -- and the rows of
# table_plan() as layers in the order dragged (the block of variables
# anywhere or nowhere, the stack's keys, a constant heading, the rows'
# text).

html <- function(...) as.character(htmltools::tagList(...))
sample_planner <- function() {
  .planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner"))
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
  expect_identical(.layer_names(c(".variable", "SEX")), c("Analysis variables (the block)", "SEX"))
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
  sch <- .plan_form_schema("plan_columns")
  expect_identical(sch$visible, c("widths", "decimal", "row_title", "auto_width"))
  # (what the sheets cannot keep: under Other arguments, said so)
  expect_identical(sch$args$cell_format$type, "r")
  expect_match(sch$args$column_widths_twips$description, "Not kept in the sheets", fixed = TRUE)
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
  own <- .plan_form_own(r, it, "plan_columns")
  expect_identical(args_to_r("plan_columns", own), args_to_r("plan_columns", it$own_args %||% list()))
  # a width, the alignment and the automatic width of the table's own
  inp$c__widths__3 <- "25"
  inp$c__decimal <- ".values"
  inp$c__auto_width <- TRUE
  own <- .plan_form_own(.plan_form_read(inp, "c", "plan_columns", cols = cols), it, "plan_columns")
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
  h <- html(.rounding_field("d", "sas"))
  expect_match(h, 'id="d__rounding"', fixed = TRUE)
  for (v in c("sas", "r", "iec60559")) expect_match(h, sprintf('value="%s"', v), fixed = TRUE)
  expect_match(h, "Study default", fixed = TRUE)
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
  expect_identical(.plan_form_main$plan_filter, c("any", "parents"))
  sch <- .plan_form_schema("plan_filter")
  expect_match(sch$args$by$description, "Not kept in the sheets", fixed = TRUE)
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
  own <- .plan_form_own(.plan_form_read(inp, "c", "plan_filter"), it, "plan_filter")
  expect_identical(args_to_r("plan_filter", own), args_to_r("plan_filter", it$own_args))
  h <- html(.plan_form_body("c", "plan_filter", it))
  expect_match(h, 'id="c__parents"', fixed = TRUE)
  expect_match(h, 'id="c__any"', fixed = TRUE)
})

test_that("plan_levels(): each variable's values dragged, its empty values hidden", {
  p <- sample_planner()
  st <- builder_read(p, "T-14-1-1")
  vr <- data.frame(variable = c("SEX", "RACE"), empty_levels = c("hide", NA))
  h <- html(.levels_body(function(x) paste0("b_", x), st, sheet_rows(p, "codelists", "T-14-1-1"),
                         empty = vr))
  i <- match("SEX", st$variables$variable)
  expect_match(h, sprintf('id="b_lv%d"', i), fixed = TRUE)
  expect_match(h, sprintf('id="b_lvdrop%d" class="form-check-input me-1" checked', i), fixed = TRUE)
  expect_match(h, "(.drop_empty)", fixed = TRUE)
  expect_match(h, "ordered on the columns", fixed = TRUE)
  expect_identical(.empty_levels_of(vr), list(SEX = TRUE, RACE = FALSE))
  expect_identical(.empty_levels_of(NULL), list())
  # the labels' card has no order of the levels any more
  expect_false(grepl("Levels (drag to order)", paste(deparse(body(app_server)), collapse = ""),
                     fixed = TRUE))
})

test_that("the new words have their Japanese", {
  w <- c("The analysis variables are no rows (a table of one variable: ORR ...).",
         "None (NA)", "The ARD's row text (.label)", "(the last level)",
         "Blank: rtfreporter's width. .values: every value column.",
         "No categorical variable to order.",
         "The columns' values (%s): ordered on the columns (table_plan()).")
  ja <- vapply(w, tr, "", lang = "ja")
  expect_true(all(ja != w), info = paste(w[ja == w], collapse = " / "))
  expect_identical(tr(w[[1L]], "ja"),
                   "解析変数は行になりません（1 変数の表、ORR など）")
})

test_that("plan_labels(): the labels of the columns the plan makes go to their code list rows only", {
  p <- sample_planner()
  id <- "T-14-1-1"
  tb <- own_tables(p, id)
  tb$stack <- "subgroup = ALL, SEX"
  p <- set_sheet_rows(p, "tables", id, tb)
  expect_identical(.plan_made_cols(p, id), c("subgroup", "subgroup_level", ".stat_group"))
  before <- sheet_rows(p, "codelists", id)
  new <- data.frame(output_id = id, variable = c("subgroup", "subgroup", "SEX"),
                    value = c("ALL", "SEX", "F"), label = c("All Subjects", "Sex", "Nope"))
  q <- .put_plan_made_codelists(p, id, new)
  cl <- sheet_rows(q, "codelists", id)
  # the data's code lists as they were; the subgroup's rows added
  expect_identical(cl[cl$variable != "subgroup", names(before)], before[, names(before)],
                   ignore_attr = TRUE)
  expect_identical(cl$label[cl$variable == "subgroup"], c("All Subjects", "Sex"))
  # written again: replaced, not added twice; none: taken away
  q2 <- .put_plan_made_codelists(q, id, new[1L, ])
  expect_identical(sum(sheet_rows(q2, "codelists", id)$variable == "subgroup"), 1L)
  q3 <- .put_plan_made_codelists(q2, id, NULL)
  expect_identical(sum(sheet_rows(q3, "codelists", id)$variable == "subgroup"), 0L)
  # nothing of the plan's, nothing to write: the planner as it was
  expect_identical(.put_plan_made_codelists(p, id, NULL), p)
})
