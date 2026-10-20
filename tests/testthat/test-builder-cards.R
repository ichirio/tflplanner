# The builder's common card parts (R/builder_cards.R, R/builder_args.R):
# the HTML of each part, the arguments' round trip through R text, the
# card read back from its inputs.

html <- function(...) as.character(htmltools::tagList(...))

test_that("a label is the meaning and the name; the name alone without a meaning", {
  h <- html(builder_label("Structure", "table_plan", "fn"))
  expect_match(h, "rp-meaning\">Structure<", fixed = TRUE)
  expect_match(h, "(table_plan())", fixed = TRUE)
  h <- html(builder_label("Columns", "cols", "arg"))
  expect_match(h, ">Columns<", fixed = TRUE)
  expect_match(h, "(cols)", fixed = TRUE)
  expect_false(grepl("cols()", h, fixed = TRUE))
  # a package is kept
  expect_match(html(builder_label(NULL, "ggrepel::geom_text_repel", "fn")),
               "ggrepel::geom_text_repel()", fixed = TRUE)
  # no meaning: the name alone, no empty parentheses
  h <- html(builder_label(NULL, "geom_line", "fn"))
  expect_match(h, "rp-name-only", fixed = TRUE)
  expect_false(grepl("rp-meaning", h, fixed = TRUE))
  expect_false(grepl("(geom_line())", h, fixed = TRUE))
  # the schema's label by language, the English translated by tr()
  expect_match(html(builder_label(list(en = "Lines", ja = "\u7dda"), "geom_line", "fn", lang = "ja")),
               "\u7dda", fixed = TRUE)
  expect_match(html(builder_label(list(en = "Lines"), "geom_line", "fn", lang = "ja")),
               ">Lines<", fixed = TRUE)
  expect_match(html(builder_label("Column", "cols", "arg", lang = "ja")), "\u5217", fixed = TRUE)
  # a meaning that says the name already (a translation "... (rounding)"):
  # the name once
  for (lang in c("en", "ja")) {
    h <- html(builder_label("Rounding", "rounding", "arg", lang = lang))
    expect_identical(lengths(regmatches(h, gregexpr("(rounding)", h, fixed = TRUE))), 1L, info = lang)
  }
  h <- html(builder_label("Rows (table_plan())", "table_plan", "fn"))
  expect_match(h, "rp-meaning\">Rows<", fixed = TRUE)
  expect_identical(lengths(regmatches(h, gregexpr("table_plan()", h, fixed = TRUE))), 1L)
})

test_that("a card has its head, its switch or bar, its badge, its menu and its body", {
  h <- html(builder_card("c1", "table_plan", "Structure", required = TRUE,
                         body = shiny::div(class = "x-body", "fields"), summary = "cols TRT"))
  expect_match(h, 'class="rp-card rp-card-required rp-card-open"', fixed = TRUE)
  expect_match(h, 'data-card="c1"', fixed = TRUE)
  expect_match(h, 'data-fn="table_plan"', fixed = TRUE)
  expect_match(h, 'data-path="c1"', fixed = TRUE)
  expect_match(h, "rp-card-bar", fixed = TRUE)
  expect_false(grepl("c1__on", h, fixed = TRUE))
  expect_match(h, "rp-card-summary\">cols TRT<", fixed = TRUE)
  expect_match(h, "x-body", fixed = TRUE)
  # the menu, one button an input
  for (k in c("up", "down", "copy", "remove", "r")) {
    expect_match(h, paste0('id="c1__', k, '"'), fixed = TRUE, info = k)
  }
  expect_match(h, ">Edit as R<", fixed = TRUE)
  # not required: a switch, off when asked; from a pattern: the badge and Revert
  h <- html(builder_card("c2", "plan_sort", "Row order", from = "pattern:ae", required = FALSE,
                         on = FALSE, tools = c("remove"), nav = "Sort the rows here."))
  expect_match(h, 'id="c2__on"', fixed = TRUE)
  expect_match(h, "rp-card-off", fixed = TRUE)
  expect_false(grepl("checked", h, fixed = TRUE))
  expect_match(h, 'data-from="pattern:ae"', fixed = TRUE)
  expect_match(h, ">Pattern ae<", fixed = TRUE)
  expect_match(h, 'id="c2__revert"', fixed = TRUE)
  expect_match(h, 'id="c2__remove"', fixed = TRUE)
  expect_false(grepl('id="c2__up"', h, fixed = TRUE))
  expect_match(h, "rp-nav-hint rp-nav-group\">Sort the rows here.<", fixed = TRUE)
  # an error is marked
  h <- html(builder_card("c3", "plan_cells", "Cells", error = "no such stat", open = FALSE))
  expect_match(h, "rp-card-error", fixed = TRUE)
  expect_match(h, "no such stat", fixed = TRUE)
  expect_false(grepl("rp-card-open", h, fixed = TRUE))
  # in Japanese
  h <- html(builder_card("c4", "plan_sort", "Row order", from = "defaults", lang = "ja"))
  expect_match(h, "\u8a66\u9a13\u6a19\u6e96", fixed = TRUE)
  expect_match(h, "\u623b\u3059", fixed = TRUE)
})

test_that("a field is chosen by its type, with its R switch and its (i)", {
  cols <- c("TRT" = "TRT", "AGE \u2014 Age" = "AGE")
  h <- html(arg_field("c1", "cols", "list<column>", label = "Columns", choices = cols,
                      required = TRUE, help = "The keys across."))
  expect_match(h, 'class="rp-field rp-field-list rp-field-required"', fixed = TRUE)
  expect_match(h, 'id="c1__cols"', fixed = TRUE)
  expect_match(h, 'id="c1__cols__r"', fixed = TRUE)
  expect_match(h, 'id="c1__cols__isr"', fixed = TRUE)
  expect_match(h, 'aria-label="The keys across."', fixed = TRUE)
  expect_match(h, "rp-req", fixed = TRUE)
  expect_match(h, "(cols)", fixed = TRUE)
  expect_match(h, "multiple", fixed = TRUE)
  h <- html(arg_field("c1", "x", "column", choices = cols, value = "AGE"))
  expect_match(h, 'rp-field rp-field-column', fixed = TRUE)
  expect_match(h, 'selected', fixed = TRUE)
  h <- html(arg_field("c1", "position", "enum[last|first]", default = "last", value = "first"))
  expect_match(h, "<select", fixed = TRUE)
  expect_match(h, "last (default)", fixed = TRUE)
  h <- html(arg_field("c1", "max_rows", "integer", default = 26L))
  expect_match(h, 'type="number"', fixed = TRUE)
  expect_match(h, 'placeholder="26"', fixed = TRUE)
  expect_match(h, 'step="1"', fixed = TRUE)
  h <- html(arg_field("c1", "width", "number", value = 0.4))
  expect_match(h, 'value="0.4"', fixed = TRUE)
  expect_match(h, 'step="any"', fixed = TRUE)
  h <- html(arg_field("c1", "keep", "logical", value = TRUE))
  expect_match(h, 'type="checkbox" class="form-check-input" id="c1__keep" checked', fixed = TRUE)
  h <- html(arg_field("c1", "label", "string", default = "Total", choices = c("Total", "All")))
  expect_match(h, 'type="text" id="c1__label"', fixed = TRUE)
  expect_match(h, 'placeholder="Total"', fixed = TRUE)
  expect_match(h, '<datalist id="c1__label__dl"', fixed = TRUE)
  expect_match(h, '<option value="All"', fixed = TRUE)
  h <- html(arg_field("c1", "colour", "colour", value = "grey50"))
  expect_match(h, "rp-swatch", fixed = TRUE)
  expect_match(h, "grey50", fixed = TRUE)
  h <- html(arg_field("c1", "shape", "shape", value = 4))
  expect_match(h, 'rp-field-shape', fixed = TRUE)
  expect_match(h, "cross (4)", fixed = TRUE)
  expect_match(h, '<option value="4" selected', fixed = TRUE)
  # an R value: the switch on, the R field filled
  h <- html(arg_field("c1", "breaks", "list<number>|r", value = tflspec::tfl_r("seq(0, 24, 4)")))
  expect_match(h, "rp-field-r", fixed = TRUE)
  expect_match(h, 'id="c1__breaks__isr" class="rp-isr" checked', fixed = TRUE)
  expect_match(h, 'value="seq(0, 24, 4)"', fixed = TRUE)
  # r: a monospace field
  h <- html(arg_field("c1", "where", "r", value = tflspec::tfl_r("SAFFL == \"Y\"")))
  expect_match(h, "rp-mono", fixed = TRUE)
  # a call<fn> the schema does not know: R
  h <- html(arg_field("c1", "position", "call<position_dodge>|string", default = "identity"))
  expect_match(h, 'id="c1__position"', fixed = TRUE)
  expect_match(h, "rp-mono", fixed = TRUE)
  # from a template: the badge on the field
  h <- html(arg_field("c1", "y", "column", from = "template:km_simple"))
  expect_match(h, 'data-from="template:km_simple"', fixed = TRUE)
  expect_match(h, ">Template<", fixed = TRUE)
  # a hint under the field, in nav mode only
  h <- html(arg_field("c1", "y", "column", nav = "The value to plot."))
  expect_match(h, "rp-nav-hint rp-nav-field", fixed = TRUE)
})

test_that("a mapping is a row an aes, each with its column / value switch", {
  v <- list(x = "AVISITN", colour = structure("grey50", rp_value = TRUE))
  h <- html(arg_field("c1", "mapping", "map<aes, column|r>", value = v, choices = c("AVISITN", "TRTA"),
                      aes = c("x", "y", "colour")))
  expect_match(h, "rp-field-map", fixed = TRUE)
  for (k in c("x", "y", "colour")) {
    expect_match(h, paste0('id="c1__mapping__', k, '"'), fixed = TRUE, info = k)
    expect_match(h, paste0('id="c1__mapping__', k, '__isval"'), fixed = TRUE, info = k)
    expect_match(h, paste0('id="c1__mapping__', k, '__val"'), fixed = TRUE, info = k)
  }
  expect_match(h, 'data-aes="colour"', fixed = TRUE)
  expect_match(h, "rp-map-row rp-map-val", fixed = TRUE)
  expect_match(h, 'value="grey50"', fixed = TRUE)
  expect_match(h, ">Column<", fixed = TRUE)
  expect_match(h, ">Value<", fixed = TRUE)
})

test_that("a group puts its fields on one row; the folded arguments are the rest", {
  f <- list(arg_field("c1", "limits[1]", "number", label = "Lower"),
            arg_field("c1", "limits[2]", "number", label = "Upper"),
            arg_field("c1", "breaks", "number", label = "By"))
  h <- html(arg_group("Range and ticks", f, as = "min max by"))
  expect_match(h, 'class="rp-group" data-as="min max by"', fixed = TRUE)
  expect_match(h, "rp-group-label\">Range and ticks<", fixed = TRUE)
  expect_match(h, 'rp-group-cell" data-as="min"', fixed = TRUE)
  expect_match(h, 'id="c1__limits[1]"', fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("rp-group-cell", h, fixed = TRUE))), 3L)
  schema <- list(package = "ggplot2", visible = c("width", "position"),
                 formals = c("mapping", "data", "stat", "position", "na.rm", "show.legend",
                             "inherit.aes", "width", "orientation"),
                 args = list(na.rm = list(type = "logical", description = "Drop missing values?")))
  h <- html(arg_more("c1", "geom_errorbar", used = "mapping", schema = schema))
  expect_match(h, "Other arguments", fixed = TRUE)
  expect_match(h, "Other arguments (6)", fixed = TRUE)
  expect_match(h, 'id="c1__more_q"', fixed = TRUE)
  for (a in c("data", "inherit.aes", "na.rm", "orientation", "show.legend", "stat")) {
    expect_match(h, paste0('data-arg="', a, '"'), fixed = TRUE, info = a)
  }
  for (a in c("width", "position", "mapping")) {
    expect_false(grepl(paste0('data-arg="', a, '"'), h, fixed = TRUE), info = a)
  }
  # the names in the function's order (its formals), not alphabetical
  pos <- function(a) regexpr(paste0('data-arg="', a, '"'), h, fixed = TRUE)
  expect_lt(pos("data"), pos("stat"))
  expect_lt(pos("stat"), pos("na.rm"))
  expect_lt(pos("inherit.aes"), pos("orientation"))
  expect_lt(pos("na.rm"), pos("inherit.aes"))
  expect_identical(.arg_order(c("b", "z", "a"), c("a", "b")), c("a", "b", "z"))
  # the (i) is the argument's help
  schema$args$stat <- list(type = "r", description = "The statistic.")
  hu <- html(arg_more("c1", "geom_errorbar", used = "mapping", schema = schema, lang = "ja"))
  expect_match(hu, 'aria-label="The statistic."', fixed = TRUE)
  expect_false(grepl("まだシートに保存できません", hu, fixed = TRUE))
  # the schema's help, else a link to the package's
  expect_match(h, 'aria-label="Drop missing values?"', fixed = TRUE)
  expect_match(h, 'class="rp-help-link" href="https://rdrr.io/cran/ggplot2/man/geom_errorbar.html"', fixed = TRUE)
  expect_false(grepl("aria-label=\"&lt;a", h, fixed = TRUE))
  # a value already there keeps its row shown
  h <- html(arg_more("c1", "geom_errorbar", used = "mapping", schema = schema, values = list(na.rm = TRUE)))
  expect_match(h, "rp-field rp-field-logical rp-has-value", fixed = TRUE)
  # nothing left: nothing
  expect_null(arg_more("c1", "f", used = c("a", "b"), schema = list(formals = c("a", "b"))))
  # without `formals` in the schema: the installed function's
  skip_if_not_installed("ggplot2")
  h <- html(arg_more("c1", "geom_errorbar", schema = list(package = "ggplot2", visible = "width")))
  expect_match(h, 'data-arg="na.rm"', fixed = TRUE)
  expect_false(grepl('data-arg="..."', h, fixed = TRUE))
})

test_that("a card whose on / off cannot work has no switch", {
  h <- html(builder_card("c5", "plan_stub", "Row label column", switchable = FALSE))
  expect_false(grepl("c5__on", h, fixed = TRUE))
  expect_false(grepl("form-switch", h, fixed = TRUE))
  expect_false(grepl("off writes nothing", h, fixed = TRUE))
  # an item the code has not yet: said so, no mark of its own
  h <- html(builder_card("c6", "plan_labels", "Labels", switchable = FALSE, absent = TRUE,
                         own_label = "This table"))
  expect_match(h, "rp-card-absent", fixed = TRUE)
  expect_match(h, ">Not in the code yet<", fixed = TRUE)
  expect_false(grepl("rp-badge-own", h, fixed = TRUE))
})

test_that("the badge says where a value came from; own has its mark when named", {
  expect_match(html(from_badge("own", own = "This table")), ">This table<", fixed = TRUE)
  expect_match(html(from_badge("own", "ja", own = "This table")), "この表", fixed = TRUE)
  expect_null(from_badge("own"))
  expect_null(from_badge(NULL))
  h <- html(from_badge("defaults"))
  expect_match(h, 'class="rp-badge rp-badge-defaults" data-from="defaults"', fixed = TRUE)
  expect_match(h, ">Study standard<", fixed = TRUE)
  expect_match(html(from_badge("pattern:ae")), ">Pattern ae<", fixed = TRUE)
  expect_match(html(from_badge("standards")), ">Company standard<", fixed = TRUE)
  h <- html(from_badge("template:km_simple"))
  expect_match(h, ">Template<", fixed = TRUE)
  expect_match(h, 'title="km_simple"', fixed = TRUE)
  expect_match(html(from_badge("defaults", "ja")), tr("Study standard", "ja"), fixed = TRUE)
  expect_match(html(from_badge("standards", "ja")), tr("Company standard", "ja"), fixed = TRUE)
})

test_that("the Code lines' map is on the page for the code view; a hint has the nav class", {
  m <- data.frame(line = c(3L, 4L, 7L), path = c("c1", "c2", "km/1"))
  h <- html(code_jump(m, "code_T1"))
  expect_match(h, 'class="rp-code-jump" data-for="code_T1"', fixed = TRUE)
  expect_match(h, '"3":"c1"', fixed = TRUE)
  expect_match(h, '"7":"km/1"', fixed = TRUE)
  h <- html(code_jump(c("3" = "c1", "5" = "c3"), "code_T1"))
  expect_match(h, '"5":"c3"', fixed = TRUE)
  h <- html(nav_hint("First choose the function that makes the plot.", "group"))
  expect_match(h, 'class="rp-nav-hint rp-nav-group"', fixed = TRUE)
  expect_match(html(nav_hint("A one-liner.", "field")), "rp-nav-field", fixed = TRUE)
  expect_null(nav_hint(NULL))
  # the style hides a hint unless <html> has the nav class, and marks lines
  expect_match(.builder_cards_css, "html:not(.rp-nav-on):not(.rp-nav) .rp-nav-hint { display: none; }", fixed = TRUE)
  expect_match(.builder_cards_js, "rp-card-selected", fixed = TRUE)
  expect_match(.builder_cards_js, "rp-ln-hl", fixed = TRUE)
  # the page carries the style and the script
  ui <- as.character(app_ui())
  expect_match(ui, ".rp-card-head {", fixed = TRUE)
  expect_match(ui, "rp-card-open:", fixed = TRUE)
})

test_that("the add field lists the schema's functions as meaning (fn), narrowed by kind", {
  schema <- list(
    ggplot = list(figure = "plot", rank = 1, label = list(en = "Standard plot", ja = "Plot \u4f5c\u6210")),
    geom_line = list(figure = "layer", rank = 200, label = list(en = "Lines")),
    geom_point = list(figure = "layer", rank = 210),
    labs = list(figure = "labs", rank = 400),
    table_plan = list(rank = 20, label = "Structure"),
    plan_sort = list(rank = 30))
  ch <- .item_choices(schema, "layer")
  expect_identical(unname(ch), c("geom_line", "geom_point"))
  expect_identical(names(ch), c("Lines (geom_line())", "geom_point()"))
  expect_identical(unname(.item_choices(schema, c("plot", "layer"))), c("ggplot", "geom_line", "geom_point"))
  expect_identical(unname(.item_choices(schema, "plan")), c("table_plan", "plan_sort"))
  expect_identical(names(.item_choices(schema, "plot", lang = "ja")), "Plot \u4f5c\u6210 (ggplot())")
  h <- html(add_item_input("add_layer", "layer", schema))
  expect_match(h, 'id="add_layer"', fixed = TRUE)
  expect_match(h, 'id="add_layer__add"', fixed = TRUE)
  expect_match(h, "Lines (geom_line())", fixed = TRUE)
  expect_match(h, '"create":true', fixed = TRUE)
  expect_false(grepl("labs()", h, fixed = TRUE))
})

# ---- the arguments <-> R text -------------------------------------------------------

# (the check's problems, tflspec's, ride on the arguments read: left off)
rt <- function(fn, args) {
  txt <- args_to_r(fn, args)
  expect_identical(.args_plain(args_from_r(fn, txt)), args, info = paste(fn, txt))
  txt
}

# ... and the text read again gives the same text: what tflspec's reading
# makes of a value (a column for a name, a call of constants for its R)
# writes back as it was read, in the order written
rt_text <- function(fn, args) {
  txt <- args_to_r(fn, args)
  a1 <- .args_plain(args_from_r(fn, txt))
  expect_identical(names(a1), names(args), info = paste(fn, txt))
  a2 <- .args_plain(args_from_r(fn, args_to_r(fn, a1)))
  expect_identical(a2, a1, info = paste(fn, txt))
  txt
}

test_that("a table's plan verbs round trip through R text (tflspec's conversion)", {
  skip_if_not_installed("tflspec")
  r <- tflspec::tfl_r
  # the values as the spec holds them: a named map is a list
  expect_identical(rt("table_plan", list(cols = "TRT", rows = list(group = "variable"), label = "label")),
                   'cols = "TRT", rows = c(group = "variable"), label = "label"')
  # the positional `...` (plan_sort's keys) is written without its name
  expect_identical(rt("plan_sort", list(keys = c(".overall", "-n"), stat = "n")),
                   'c(".overall", "-n"), stat = "n"')
  rt("plan_digits", list(continuous = list(mean = 1, sd = 2), AGE = 0, rounding = "sas"))
  rt("plan_levels", list(SEX = c("F", "M"), .drop_empty = "RACE"))
  rt("plan_labels", list(AGE = "Age (years)", SEX = list(F = "Female", M = "Male")))
  rt("plan_nest", list(RACE = list(ETHNIC = "Hispanic")))
  rt("plan_total", list(label = "Total", position = "last"))
  rt("plan_stat_groups", list(n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high")))
  expect_identical(
    rt("plan_cells", list(default = "{n} ({p:.1f%})", continuous = list(n = "{n}"),
                          stats = "columns", notes = TRUE)),
    'default = "{n} ({p:.1f%})", continuous = list(n = "{n}"), stats = "columns", notes = TRUE')
  expect_identical(rt("plan_filter", list(where = r("SAFFL == \"Y\" & AGE > 65"))),
                   'SAFFL == "Y" & AGE > 65')
  rt("plan_compare", list(ref = "Placebo", stat = "p.value"))
  # an integer argument comes back an integer (tflspec#292)
  rt("plan_stub", list(width = 2.5, indent = 2L))
  expect_identical(rt("plan_paginate_rows", list(max_rows = 26L, split = "group_safe")),
                   'max_rows = 26, split = "group_safe"')
  rt("plan_col_header", list(header = c("A | B", "C"), align_count_pct = TRUE))
  # the positional `...` with no name goes to the schema's key
  a <- .args_plain(args_from_r("plan_sort", '".overall", "-n", stat = "n"'))
  expect_identical(names(a), c("keys", "stat"))
  expect_identical(unlist(a$keys, use.names = FALSE), c(".overall", "-n"))
  expect_identical(.args_plain(args_from_r("plan_sort", '"-n"')), list(keys = "-n"))
  expect_error(args_from_r("plan_total", '"Total"'), "no name")
  # NULL is !r NULL, both ways (an !unset is "back to the default": no argument)
  expect_identical(.args_plain(args_from_r("plan_total", "label = NULL")),
                   list(label = tflspec::tfl_r("NULL")))
  expect_identical(args_to_r("plan_total", list(label = tflspec::tfl_r("NULL"))), "label = NULL")
  expect_identical(args_to_r("plan_total", list(label = tflspec::tfl_unset(), x = 1)), "x = 1")
  # a name R cannot write bare is quoted, and read back
  expect_identical(args_to_r("plan_labels", list("Age (years)" = "x")), "\"Age (years)\" = \"x\"")
  expect_identical(.args_plain(args_from_r("plan_labels", "`Age (years)` = \"x\"")), list("Age (years)" = "x"))
  expect_identical(args_to_r("f", list()), "")
  expect_identical(args_from_r("f", ""), list())
  expect_error(args_from_r("f", "a = ("), "cannot be read")
})

test_that("the check's problems come with the arguments read, as lines for the R dialog", {
  skip_if_not_installed("tflspec")
  a <- args_from_r("plan_paginate_rows", "max_rows = 26, keep = TRUE")
  expect_s3_class(attr(a, "problems"), "data.frame")
  pr <- .args_problems(a)
  expect_length(pr, 1L)
  expect_match(pr, "`keep`", fixed = TRUE)
  expect_identical(.args_problems(args_from_r("plan_total", 'label = "Total"')), character())
  expect_null(attr(.args_plain(a), "problems"))
})

test_that("a function the schema does not know: the app's own conversion", {
  r <- tflspec::tfl_r
  expect_identical(args_to_r("f", list(a = 1L, b = list(c = "x"), d = r("y + 1"))),
                   'a = 1L, b = list(c = "x"), d = y + 1')
  expect_identical(args_from_r("f", "a = list(b = x + 1, c = 2)"),
                   list(a = list(b = r("x + 1"), c = 2)))
  expect_identical(args_from_r("f", "a = NULL"), list(a = r("NULL")))
  expect_error(args_from_r("f", '"x"'), "no name")
})

test_that("a figure's functions round trip through R text; what is no literal stays R", {
  skip_if_not_installed("tflspec")
  r <- tflspec::tfl_r
  # written as the function takes it (aes() for a mapping), read back as
  # the spec holds it, written again the same
  expect_identical(
    rt_text("ggplot", list(data = r("sm"), mapping = list(x = "AVISITN", y = "mean", colour = "TRTA"))),
    "data = sm, mapping = aes(x = AVISITN, y = mean, colour = TRTA)")
  expect_identical(rt_text("geom_line", list(position = r("position_dodge(width = 0.6)"), linewidth = 0.5)),
                   "position = position_dodge(width = 0.6), linewidth = 0.5")
  rt_text("geom_point", list(size = 2, shape = 16L, alpha = 0.8))
  rt_text("geom_errorbar", list(mapping = list(ymin = "lo", ymax = "hi"), width = 0.4,
                                position = r("pd")))
  expect_identical(
    rt_text("scale_x_continuous", list(limits = c(0, 24), breaks = r("seq(0, 24, 4)"), expand = c(0.01, 0))),
    "limits = c(0, 24), breaks = seq(0, 24, 4), expand = c(0.01, 0)")
  rt("labs", list(x = "Week", y = "Mean change from baseline (mmHg)", colour = tflspec::tfl_r("NULL")))
  expect_identical(
    rt_text("theme", list(legend.position = "bottom", legend.position.inside = c(0.8, 0.8),
                          axis.text.y = r("element_text(size = 8)"))),
    "legend.position = \"bottom\", legend.position.inside = c(0.8, 0.8), axis.text.y = element_text(size = 8)")
  rt_text("ggsurvfit", list(fit = r("fit"), linewidth = 0.3, type = "survival"))
  rt("add_risktable", list(times = r("x_breaks"), risktable_stats = c("n.risk", "cum.event"), size = 3))
  expect_identical(rt("wrap_plots", list(plots = c("km", "risk"), ncol = 1L, heights = c(3, 1), guides = "collect")),
                   'plots = c("km", "risk"), ncol = 1, heights = c(3, 1), guides = "collect"')
  rt_text("geom_hline", list(yintercept = -30, linetype = "dashed", colour = r("tfl_setting(\"zero_line_colour\")")))
  # a nested call the form cannot split stays R
  expect_identical(.args_plain(args_from_r("theme", "legend.position = c(0.8, 1 - 0.2)")),
                   list(legend.position = r("c(0.8, 1 - 0.2)")))
  # a tfl_call is written as its call
  expect_identical(
    args_to_r("geom_line", list(position = tflspec::tfl_call("position_dodge", list(width = 0.6)))),
    "position = position_dodge(width = 0.6)")
})

# ---- the card read back ----------------------------------------------------------------

test_that("a card's inputs come back as the item's arguments; blanks are left out", {
  skip_if_not_installed("tflspec")
  schema <- list(args = list(
    cols = list(type = "list<column>"),
    rows = list(type = "vec<string, column>|list<column>"),
    max_rows = list(type = "integer"),
    width = list(type = "number"),
    keep = list(type = "logical"),
    notes = list(type = "logical", default = TRUE),
    position = list(type = "enum[last|first]"),
    label = list(type = "string"),
    mapping = list(type = "map<aes, column|r>", keys = c("x", "y", "colour")),
    where = list(type = "r"),
    breaks = list(type = "list<number>|r")))
  input <- list(
    c1__on = TRUE,
    c1__cols = c("TRT", ""),
    c1__rows = character(),
    c1__max_rows = "26",
    c1__width = 0.4,
    c1__keep = TRUE,
    c1__notes = FALSE,
    c1__position = "",
    c1__label = "",
    c1__mapping__x = "AVISITN",
    c1__mapping__y = "",
    c1__mapping__colour__isval = TRUE,
    c1__mapping__colour__val = "grey50",
    c1__where = "SAFFL == \"Y\"",
    c1__breaks__isr = TRUE,
    c1__breaks__r = " seq(0, 24, 4) ")
  got <- builder_card_read(input, "c1", schema)
  expect_identical(got, list(
    cols = "TRT", max_rows = 26L, width = 0.4, keep = TRUE, notes = FALSE,
    mapping = list(x = "AVISITN"),
    where = tflspec::tfl_r("SAFFL == \"Y\""),
    breaks = tflspec::tfl_r("seq(0, 24, 4)"),
    colour = "grey50"))
  # a logical left unticked with no default: not written
  expect_identical(builder_card_read(list(c1__keep = FALSE), "c1", schema), list())
  # switched off: nothing at all
  off <- input
  off$c1__on <- FALSE
  expect_null(builder_card_read(off, "c1", schema))
  # the arguments to read may be given (the formals the schema lacks)
  expect_identical(builder_card_read(list(c1__na.rm = "TRUE", c1__n = "3"), "c1", list(), args = c("na.rm", "n")),
                   list(na.rm = TRUE, n = 3))
})

test_that("a type's kind picks the field", {
  expect_identical(.arg_kind("list<column>"), "list")
  expect_identical(.arg_kind("vec<string, column>|map<string, column|guard>|list<column>"), "list")
  expect_identical(.arg_kind("list<number>|r"), "list")
  expect_identical(.arg_kind("r|list<number>"), "list")
  expect_identical(.arg_kind("enum[sas|r|iec60559]"), "enum")
  expect_identical(.type_enum("enum[sas|r|iec60559]"), c("sas", "r", "iec60559"))
  expect_identical(.arg_kind("map<aes, column|r>"), "map")
  expect_identical(.arg_kind("call<position_dodge|position_identity>|string"), "call")
  expect_identical(.type_call_fn("call<position_dodge|position_identity>|string"),
                   c("position_dodge", "position_identity"))
  expect_identical(.arg_kind("rows<variable, stat>"), "r")
  expect_identical(.arg_kind(NULL), "r")
  expect_identical(.arg_kind("number|enum[fit]"), "number")
  expect_identical(.arg_kind("template|map<string, template>"), "template")
})

# The page's fields as drawn: each <input type="text"> its value (line
# breaks dropped, as a browser does), each <textarea> its text, each
# checkbox ticked or not (the values the page sends when the form is drawn)
.drawn_inputs <- function(html) {
  unesc <- function(x) {
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&#39;", "'", x, fixed = TRUE)
    x <- gsub("&#10;", "\n", x, fixed = TRUE)
    x <- gsub("&#13;", "\r", x, fixed = TRUE)
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
    gsub("&amp;", "&", x, fixed = TRUE)
  }
  out <- list()
  ta <- regmatches(html, gregexpr("<textarea[^>]*id=\"[^\"]+\"[^>]*>[^<]*</textarea>", html))[[1L]]
  for (s in ta) {
    id <- sub("(?s)^.*?id=\"([^\"]+)\".*$", "\\1", s, perl = TRUE)
    out[[id]] <- unesc(sub("(?s)^<textarea[^>]*>([^<]*)</textarea>$", "\\1", s, perl = TRUE))
  }
  # a select: its options marked selected (none: a single one's first
  # option, a multiple one nothing)
  sel <- regmatches(html, gregexpr("(?s)<select[^>]*id=\"[^\"]+\"[^>]*>.*?</select>", html, perl = TRUE))[[1L]]
  for (s in sel) {
    id <- sub("(?s)^<select[^>]*?id=\"([^\"]+)\".*$", "\\1", s, perl = TRUE)
    opts <- regmatches(s, gregexpr("<option[^>]*>", s))[[1L]]
    val <- function(o) unesc(sub("(?s)^.*?value=\"([^\"]*)\".*$", "\\1", o, perl = TRUE))
    on <- vapply(opts[grepl("selected", opts, fixed = TRUE)], val, "", USE.NAMES = FALSE)
    multiple <- grepl("<select[^>]*multiple", s)
    if (length(on)) out[[id]] <- if (multiple) on else on[[1L]]
    else if (!multiple && length(opts)) out[[id]] <- val(opts[[1L]])
  }
  inp <- regmatches(html, gregexpr("<input[^>]*>", html))[[1L]]
  for (s in inp) {
    if (!grepl("id=\"", s, fixed = TRUE)) next
    id <- sub("(?s)^.*?id=\"([^\"]+)\".*$", "\\1", s, perl = TRUE)
    type <- if (grepl("type=\"", s, fixed = TRUE)) sub("(?s)^.*?type=\"([^\"]+)\".*$", "\\1", s, perl = TRUE) else "text"
    if (type %in% c("text", "search")) {
      v <- if (grepl("value=\"", s, fixed = TRUE)) sub("(?s)^.*?value=\"([^\"]*)\".*$", "\\1", s, perl = TRUE) else ""
      # (a one-line field drops line breaks, as the browser does)
      out[[id]] <- gsub("[\r\n]", "", unesc(v))
    } else if (type == "number") {
      v <- if (grepl("value=\"", s, fixed = TRUE)) sub("(?s)^.*?value=\"([^\"]*)\".*$", "\\1", s, perl = TRUE) else ""
      if (nzchar(v)) out[[id]] <- as.numeric(v)
    } else if (type == "checkbox") {
      out[[id]] <- grepl("checked", s, fixed = TRUE)
    }
  }
  out
}

test_that("the ARD preview, the form as drawn and its other arguments leave the study unchanged", {
  skip_on_cran()
  local_home()
  suppressMessages(create_sample_study(run = FALSE))
  for (id in c("T-14-3-1", "T-14-1-1", "T-14-2-3")) {
    shiny::testServer(server_for("SAMPLE-01"), {
      session$setInputs(target = id, nav = "make", step = "content", content_nav = "content",
                        table_nav = "builder")
      session$setInputs(fetch3 = 1)
      session$elapse(2000)
      expect_false(session$userData$dirty())
      bf <- session$userData$bform
      n <- bf$n
      b <- function(x) paste0("b", n, "_", x)
      # the form as the page draws it: the cards (their "Other arguments"
      # among them: drawn folded, there all the same), the column header,
      # the decimals ... (some drawn once the form's first values come)
      drawn <- function() {
        outs <- c("hdr_ui", "hdr_n_ui", "digits_ui", "exc_ui", "arms_ui", "total_ui",
                  paste0("vdef", 1:9))
        html <- vapply(outs, function(o) tryCatch(output[[b(o)]]$html %||% "",
                                                  error = function(e) ""), "")
        html <- paste(c(html, output$builder_form$html %||% ""), collapse = " ")
        v <- .drawn_inputs(html)
        v <- v[startsWith(names(v), b(""))]
        # (and the selects as read: the table's key and rows)
        st <- bf$st
        v[[b("key")]] <- st$key
        v[[b("layers")]] <- if (length(st$hierarchy)) st$row_keys else st$layers
        list(html = html, v = v)
      }
      d <- drawn()
      if (identical(id, "T-14-3-1")) {
        # a row-header cell's text with a line break: a field that keeps it
        expect_match(d$html, "<textarea[^>]*_stub1\"[^>]*>System Organ Class\n", perl = TRUE)
      }
      expect_gt(length(d$v), 0L)
      do.call(session$setInputs, d$v)
      session$elapse(2000)
      d <- drawn()
      do.call(session$setInputs, d$v)
      session$elapse(2000)
      expect_false(session$userData$dirty(), label = paste(id, "dirty"))
    })
  }
})
