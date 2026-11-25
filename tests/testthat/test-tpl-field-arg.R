# A template argument's field read back the way it was drawn, by the
# add-report dialog and the card alike (.tpl_field_arg(), .tpl_dialog_args()):
# an empty or untouched field is "not given" (not written: the template's
# default), "(none)" is !unset, and a list never carries "" entries or a map
# where a sequence is declared.  Fictional values only.

# a list argument whose default the "name | text" rows cannot show (an
# unnamed sequence): drawn as its YAML box
fa_box <- list(name = "keys", type = "list", default = c(".overall", "group1", "-n"))
# a map argument: the "name | text" rows
fa_rows <- list(name = "labels", type = "list")

# What a field sends untouched, drawn from its default (tpl_widget())
fa_sent <- function(x) {
  start <- x$default
  switch(.tpl_type(x),
    flag = isTRUE(start),
    number = .num_or_null(start),
    text = if (is.null(start)) "" else .tpl_show(start),
    r = .tpl_r_text(start),
    list = .tpl_list_text(start, x) %||% .tpl_yaml_block(start),
    columns = .tpl_list_text(start, x),
    lines = .tpl_lines_text(start),
    table = .tpl_table_state(start, x),
    vars = , chips = , choices = , picks = as.character(start %||% character()),
    levels = if (isTRUE(x$multiple)) as.character(start %||% character()) else
      as.character(start %||% "")[1L],
    as.character(start %||% "")[1L])
}

# The dialog of a series template, every field untouched but `set`
fa_dialog <- function(name, set = list()) {
  prm <- .tpl_params(name)
  shown <- .tpl_shown(prm)
  raws <- lapply(shown, function(x) tryCatch(fa_sent(x), error = function(e) NULL))
  names(raws) <- vapply(shown, function(x) x$name, "")
  raws[names(set)] <- set
  .tpl_dialog_args(prm, raws)
}

# The card's read of an edit, then the report's args it gives
fa_card <- function(raw, type, prm, shown = prm$default) {
  .tpl_set_arg(list(), prm, .tpl_field_arg(raw, type, prm, shown, echo = FALSE))
}

test_that("a list drawn as its YAML box is read as YAML: untouched it is no argument", {
  box <- .tpl_yaml_block(fa_box$default)
  expect_identical(.tpl_type(fa_box), "list")
  expect_null(.tpl_list_text(fa_box$default, fa_box))
  # untouched: the default, not a map of "" (the add-report bug)
  expect_identical(.tpl_untouched(fa_box$default, "list", fa_box), fa_box$default)
  expect_null(.tpl_field_arg(box, "list", fa_box, fa_box$default))
  expect_length(.tpl_dialog_args(list(keys = fa_box), list(keys = box)), 0L)
  # empty, blank, all entries empty: not given
  for (raw in list("", " \n  \n", "- ''\n- ''")) {
    expect_null(.tpl_field_arg(raw, "list", fa_box, fa_box$default), info = raw)
    expect_length(.tpl_dialog_args(list(keys = fa_box), list(keys = raw)), 0L)
  }
  # "(none)": !unset
  expect_s3_class(.tpl_field_arg("(none)", "list", fa_box, fa_box$default), "tfl_unset")
  expect_s3_class(.tpl_dialog_args(list(keys = fa_box), list(keys = "(none)"))$keys, "tfl_unset")
  # an edit: a sequence, its empty entries dropped
  expect_identical(.tpl_field_arg("- .overall\n- ''\n- label", "list", fa_box, fa_box$default),
                   c(".overall", "label"))
  expect_identical(.tpl_dialog_args(list(keys = fa_box), list(keys = "- .overall\n- label"))$keys,
                   c(".overall", "label"))
  # not YAML: an error the card says; the dialog writes nothing
  expect_error(.tpl_field_arg("- a\n b: [", "list", fa_box, fa_box$default))
  expect_length(.tpl_dialog_args(list(keys = fa_box), list(keys = "- a\n b: [")), 0L)
})

test_that("the card reads a field as the dialog does", {
  box <- .tpl_yaml_block(fa_box$default)
  # untouched, empty, blank: no argument (the default)
  for (raw in list(box, "", " \n ")) expect_length(fa_card(raw, "list", fa_box), 0L)
  expect_s3_class(fa_card("(none)", "list", fa_box)$keys, "tfl_unset")
  expect_identical(fa_card("- label", "list", fa_box)$keys, "label")
  # the rows field: an empty value cell is not given
  expect_length(fa_card("SEX", "list", fa_rows, NULL), 0L)
  expect_identical(fa_card("AGE | Age\nSEX |", "list", fa_rows, NULL)$labels, c(AGE = "Age"))
})

test_that("a sequence of plain values gets the value-a-line field, not the rows", {
  expect_identical(.tpl_param_type(list(type = "list<string>", input = "text")), "lines")
  expect_identical(.tpl_param_type(list(type = "list<number>", input = "text")), "lines")
  expect_identical(.tpl_param_type(list(type = "list<integer>")), "lines")
  # a map, or a list beside R, keeps its own field
  expect_identical(.tpl_param_type(list(type = "list<number>|r", input = "text")), "list")
  expect_identical(.tpl_param_type(list(type = "list<column>", input = "pick")), "vars")
  expect_identical(.tpl_type(list(name = "x", type = "lines", multiple = TRUE)), "lines")
  # its text a value a line, read back so
  expect_identical(.tpl_lines_text(c("Body system", "  Term")), "Body system\n  Term")
  expect_identical(.tpl_lines_text(NULL), "")
  expect_identical(.tpl_lines_parse("Body system\n\n  Term  \n"), c("Body system", "  Term"))
  expect_null(.tpl_lines_parse(" \n "))
  # a map where a sequence belongs (a map of "" the dialog once wrote): its
  # values only, so it reads back as not given
  bad <- list(`- .overall` = "", `- label` = "")
  expect_identical(.tpl_lines_text(bad), "")
  expect_null(.tpl_field_arg(.tpl_lines_text(bad), "lines", list(name = "sort", type = "lines")))
})

test_that("tflspec_hierarchy's heading: untouched, blank, (none) and lines", {
  skip_if_not_installed("tflspec")
  h <- "tflspec_v1/tflspec_hierarchy"
  prm <- .tpl_params(h)
  skip_if(is.null(prm$heading), "tflspec has no tflspec_hierarchy heading")
  expect_identical(.tpl_type(prm$heading), "lines")
  req <- list(from = "ADAE", by = "TRT01A", hierarchy = c("AEBODSYS", "AEDECOD"))
  # every field untouched: only the required arguments are written
  a <- fa_dialog(h, req)
  expect_null(attr(a, "missing"))
  expect_setequal(names(a), names(req))
  # blank: not written; (none): !unset; lines: a sequence
  for (raw in list("", "  \n \n")) expect_null(fa_dialog(h, c(req, list(heading = raw)))$heading)
  expect_s3_class(fa_dialog(h, c(req, list(heading = "(none)")))$heading, "tfl_unset")
  expect_identical(fa_dialog(h, c(req, list(heading = "Body system\n  Term")))$heading,
                   c("Body system", "  Term"))
  # the card: the same
  x <- prm$heading
  expect_length(fa_card("", "lines", x), 0L)
  expect_length(fa_card(" \n ", "lines", x), 0L)
  expect_s3_class(fa_card("(none)", "lines", x)$heading, "tfl_unset")
  expect_identical(fa_card("Body system\n  Term", "lines", x)$heading, c("Body system", "  Term"))
})

test_that("km_cards' x_breaks: a number a line", {
  skip_if_not_installed("tflspec")
  k <- "tflspec_v1/km_cards"
  prm <- .tpl_params(k)
  skip_if(is.null(prm$x_breaks), "tflspec has no km_cards x_breaks")
  x <- prm$x_breaks
  expect_identical(.tpl_type(x), "lines")
  expect_identical(.tpl_field_arg("0\n30", "lines", x), c(0, 30))
  expect_equal(.tpl_field_arg("- 0\n- 30", "lines", x), c(0, 30))
  expect_identical(fa_card("0\n30", "lines", x)$x_breaks, c(0, 30))
  expect_null(.tpl_field_arg("", "lines", x))
  expect_s3_class(.tpl_field_arg("(none)", "lines", x), "tfl_unset")
  # the dialog: untouched writes no x_breaks; typed, the numbers
  req <- list(by = "TRT01P", param = "OS")
  expect_null(fa_dialog(k, req)$x_breaks)
  expect_identical(fa_dialog(k, c(req, list(x_breaks = "0\n30")))$x_breaks, c(0, 30))
})

test_that("a list of one entry is a list, as typed; a lone word is not a list", {
  expect_identical(.tpl_yaml_read("- label"), "label")
  expect_identical(.tpl_yaml_read("# keys\n\n- label\n"), "label")
  expect_identical(.tpl_yaml_read("[label]"), "label")
  expect_identical(.tpl_dialog_args(list(keys = fa_box), list(keys = "- label"))$keys, "label")
  expect_error(.tpl_yaml_read("label"), "is expected")
})
