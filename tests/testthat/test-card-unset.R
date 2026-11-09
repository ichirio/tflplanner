# "(none)" (`!unset`) on the template cards and the code SPEC cards: shown for
# a value that is tfl_unset(), chosen on any optional argument, written as
# `!unset` and read back as it.  The Page tab's pattern (page_form.R,
# page_spec.R), on the other two kinds of card.

U <- function() tflspec::tfl_unset()

# the template's parameters as a mock gives them: unset defaults on a select,
# a many-valued select, a text, a number and a flag
.unset_prm <- function(listing_params) {
  prm <- listing_params
  p1 <- function(name, type, ...) structure(utils::modifyList(
    list(name = name, default = U(), required = FALSE, doc = name, type = type, multiple = FALSE),
    list(...)), class = "tfl_param")
  prm$by_break$default <- U()
  prm$by$default <- U()
  prm$txtx <- p1("txtx", "text")
  prm$numx <- p1("numx", "number")
  prm$flagx <- p1("flagx", "flag")
  prm$txty <- p1("txty", "text", default = "keep")
  prm
}

# a study with a template listing (the sample's, fictional data)
.unset_study <- function(id = "L-TPL") {
  home <- local_home(parent.frame())
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  args <- list(data = "ADAE",
               columns = list(list(vars = "USUBJID", label = "Subject"),
                              list(vars = "AEDECOD", label = "Term", width = 30L)))
  o <- tflspec::tfl_output(id, "listing", label = "AE listing",
                           template = list(name = "listing", args = args))
  s$planner <- .spec_add_output(s$planner, o, label = "AE listing")
  suppressWarnings(suppressMessages(save_study(s, home = home)))
}
.tag_of <- function(html, id, tag) {
  regmatches(html, regexpr(sprintf('<%s[^>]*id="%s"[^>]*>', tag, id), html))
}
.select_of <- function(html, id) {
  regmatches(html, regexpr(sprintf('(?s)<select[^>]*id="%s".*?</select>', id), html, perl = TRUE))
}

test_that("a template param whose default is unset shows (none), one per widget type", {
  skip_on_cran()
  orig <- .tpl_params
  prm <- .unset_prm(orig("listing"))
  local_mocked_bindings(.tpl_params = function(name) prm, .package = "tflplanner")
  s <- .unset_study()
  shiny::testServer(function(input, output, session) app_server(input, output, session, s), {
    session$setInputs(target = "L-TPL", nav = "make", step = "content", content_nav = "content")
    h <- output$tpl_card_lf$html
    # a select: "(none)" first and selected, the blank "(default)" no longer
    sel <- .select_of(h, "tpl_listing__by_break")
    expect_match(sel, '<option value="(none)" selected>(none)</option>', fixed = TRUE)
    expect_false(grepl('<option value="" selected', sel, fixed = TRUE))
    # a selectize with several: the same
    expect_match(.select_of(h, "tpl_listing__by"), '<option value="(none)" selected>(none)</option>', fixed = TRUE)
    # a text: "(none)" as its value, and the button
    expect_match(.tag_of(h, "tpl_listing__txtx", "input"), 'value="(none)"', fixed = TRUE)
    expect_match(h, "tplNone(&#39;tpl_listing__txtx&#39;)", fixed = TRUE)
    # a number: the text "(none)" (a number input cannot hold it)
    num <- .tag_of(h, "tpl_listing__numx", "input")
    expect_match(num, 'value="(none)"', fixed = TRUE)
    expect_match(num, 'type="text"', fixed = TRUE)
    # a flag: off, with the note
    expect_false(grepl("checked", .tag_of(h, "tpl_listing__flagx", "input"), fixed = TRUE))
    expect_match(h, "none-note none-on", fixed = TRUE)
    # a text with a default of its value: its value, and the button still offered
    expect_match(.tag_of(h, "tpl_listing__txty", "input"), 'value="keep"', fixed = TRUE)
    expect_match(h, "tplNone(&#39;tpl_listing__txty&#39;)", fixed = TRUE)
  })
})

test_that("choosing (none) writes tfl_unset into template.args; another value or clearing replaces it", {
  skip_on_cran()
  s <- .unset_study()
  shiny::testServer(function(input, output, session) app_server(input, output, session, s), {
    rv <- session$userData$rv
    session$setInputs(target = "L-TPL", nav = "make", step = "content", content_nav = "content")
    # a select (by_break: default "page")
    expect_match(.select_of(output$tpl_card_lf$html, "tpl_listing__by_break"), 'value="page" selected', fixed = TRUE)
    session$setInputs(tpl_listing__by_break = "(none)")
    expect_s3_class(.tpl_of(rv$p, "L-TPL")$args$by_break, "tfl_unset")
    expect_match(.select_of(output$tpl_card_lf$html, "tpl_listing__by_break"),
                 '<option value="(none)" selected>(none)</option>', fixed = TRUE)
    # a text (by_heading: no default)
    session$setInputs(tpl_listing__by_heading = "(none)")
    expect_s3_class(.tpl_of(rv$p, "L-TPL")$args$by_heading, "tfl_unset")
    # another value is written as it is
    session$setInputs(tpl_listing__by_break = "block")
    expect_identical(.tpl_of(rv$p, "L-TPL")$args$by_break, "block")
    session$setInputs(tpl_listing__by_heading = "Heading {by}")
    expect_identical(.tpl_of(rv$p, "L-TPL")$args$by_heading, "Heading {by}")
    # cleared: the argument is taken out, the template's default applies again
    session$setInputs(tpl_listing__by_heading = "(none)")
    session$setInputs(tpl_listing__by_heading = "")
    expect_false("by_heading" %in% names(.tpl_of(rv$p, "L-TPL")$args))
    session$setInputs(tpl_listing__by_break = "(none)")
    session$setInputs(tpl_listing__by_break = "")
    expect_false("by_break" %in% names(.tpl_of(rv$p, "L-TPL")$args))
  })
})

test_that("a template argument that is unset is passed as !unset, and the template leaves it out of its call", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  o <- tflspec::tfl_output("T-TPL", "table", label = "Demographics", population = "SAF",
                           template = list(name = "demog", args = list(population = "SAF", sd_missing = U())))
  s$planner <- .spec_add_output(s$planner, o, label = "Demographics", population = "SAF")
  expect_s3_class(.tpl_of(s$planner, "T-TPL")$args$sd_missing, "tfl_unset")
  # the SPEC YAML keeps it
  s <- suppressMessages(save_study(s, home = home))
  y <- readLines(file.path(s$path, "spec", "outputs", "T-TPL.yml"), warn = FALSE)
  expect_true(any(grepl("sd_missing: !unset", y, fixed = TRUE)))
  # the program hands it to the template as an unset, which takes the argument out of its own call
  code <- paste(program_code(s$planner, "T-TPL"), collapse = "\n")
  expect_match(code, "sd_missing = tflspec::tfl_unset()", fixed = TRUE)
  saf <- tflspec::adsl[tflspec::adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE))
  with <- tflspec::tfl_template_run("summary", ard, population = "SAF", continuous = "AGE", sd_missing = U())
  without <- tflspec::tfl_template_run("summary", ard, population = "SAF", continuous = "AGE")
  expect_equal(with, without)
})

test_that("a flag's (none) button is its own input; the switch turned on is a value", {
  skip_on_cran()
  orig <- .tpl_params
  prm <- .unset_prm(orig("listing"))
  prm$flagx$default <- FALSE
  local_mocked_bindings(.tpl_params = function(name) prm, .package = "tflplanner")
  s <- .unset_study()
  shiny::testServer(function(input, output, session) app_server(input, output, session, s), {
    rv <- session$userData$rv
    session$setInputs(target = "L-TPL", nav = "make", step = "content", content_nav = "content")
    expect_false(grepl("none-note none-on", output$tpl_card_lf$html, fixed = TRUE))
    session$setInputs(tpl_listing__flagx__none = TRUE)
    expect_s3_class(.tpl_of(rv$p, "L-TPL")$args$flagx, "tfl_unset")
    expect_match(output$tpl_card_lf$html, "none-note none-on", fixed = TRUE)
    session$setInputs(tpl_listing__flagx__none = FALSE, tpl_listing__flagx = TRUE)
    expect_identical(.tpl_of(rv$p, "L-TPL")$args$flagx, TRUE)
  })
})

test_that("the dialog: an optional argument offers (none), a required one does not", {
  skip_on_cran()
  orig <- .tpl_params
  prm <- .unset_prm(orig("listing"))
  local_mocked_bindings(.tpl_params = function(name) prm, .package = "tflplanner")
  local_home()
  create_study("U1")
  shiny::testServer(server_for("U1"), {
    session$setInputs(add = 1, modal_type = "listing", modal_how = "template", modal_tpl_listing = "listing",
                      modal_id = "L-AE", modal_desc = "AEs")
    # (the required ones, data and columns, are drawn with the dialog, apart from the others below:
    # a required field has no (none), as the wrapper's own test shows)
    # the others: the item and the button
    more <- as.character(output$tpl_modal_more$html)
    expect_match(.select_of(more, "mtpl_listing__by_break"), '<option value="(none)" selected>(none)</option>', fixed = TRUE)
    expect_match(more, "tplNone(&#39;mtpl_listing__txtx&#39;)", fixed = TRUE)
    expect_match(more, "tplNone(&#39;mtpl_listing__flagx&#39;)", fixed = TRUE)
    expect_false(grepl("mtpl_listing__data", more, fixed = TRUE))
    expect_false(grepl("mtpl_listing__columns", more, fixed = TRUE))
    # chosen in the dialog: written as !unset where the start was a value; a start that is unset is no change
    session$setInputs(mtpl_listing__data = "ADAE", mtpl_listing__columns = "USUBJID | Subject",
                      mtpl_listing__txty = "(none)", mtpl_listing__txtx = "(none)",
                      mtpl_listing__flagx__none = TRUE)
    session$setInputs(add_ok = 1)
    args <- .tpl_of(session$userData$rv$p, "L-AE")$args
    expect_s3_class(args$txty, "tfl_unset")
    expect_false("txtx" %in% names(args))
    expect_false("flagx" %in% names(args))
  })
})

test_that("(none) text is read as unset on an optional argument only; among items it is not one", {
  opt <- list(name = "x", default = "a", required = FALSE)
  req <- list(name = "x", default = NULL, required = TRUE)
  expect_s3_class(.tpl_value("(none)", "text", opt), "tfl_unset")
  expect_s3_class(.tpl_value(" (none) ", "number", opt), "tfl_unset")
  expect_s3_class(.tpl_value("(none)", "choice", opt), "tfl_unset")
  expect_s3_class(.tpl_value("(none)", "flag", opt), "tfl_unset")
  expect_s3_class(.tpl_value("(none)", "list", opt), "tfl_unset")
  expect_s3_class(.tpl_value("(none)", "vars", opt), "tfl_unset")
  expect_identical(.tpl_value(c("(none)", "AGE"), "vars", opt), "AGE")
  expect_identical(.tpl_value("(none)", "text", req), "(none)")
  # an unset is never "the default itself" or "the same as" a value
  expect_s3_class(.tpl_set_arg(list(), opt, U())$x, "tfl_unset")
  expect_null(.tpl_set_arg(list(x = U()), opt, "a")$x)
  expect_identical(.tpl_set_arg(list(), list(name = "x", default = "", required = FALSE), "z")$x, "z")
  expect_s3_class(.tpl_set_arg(list(), list(name = "x", default = "", required = FALSE), U())$x, "tfl_unset")
  expect_identical(.same_unset(U(), U()), TRUE)
  expect_identical(.same_unset(U(), ""), FALSE)
  expect_true(is.na(.same_unset("a", "a")))
  expect_identical(.tpl_show(U()), "(none)")
})

test_that("fields without an unset render as before; a required one has no (none)", {
  prm_opt <- list(name = "x", default = NULL, required = FALSE)
  prm_req <- list(name = "x", default = NULL, required = TRUE)
  widgets <- list(
    text = shiny::textInput("w", "W", value = "abc"),
    number = shiny::numericInput("w", "W", value = 3),
    flag = bslib::input_switch("w", "W", value = TRUE),
    choice = shiny::selectInput("w", "W", c("", "a", "b"), selected = "a"),
    vars = shiny::selectizeInput("w", "W", c("A", "B"), selected = "A", multiple = TRUE),
    list = shiny::textAreaInput("w", "W", value = "a | b"))
  for (ty in names(widgets)) {
    w <- widgets[[ty]]
    base <- as.character(w)
    # (required: the field as it was, to the byte)
    expect_identical(as.character(.tpl_none_wrap(w, "w", ty, prm_req, FALSE)), base, info = ty)
    # (optional: the same field, with the item / the button added)
    r <- .tpl_none_wrap(w, "w", ty, prm_opt, FALSE)
    h <- as.character(r)
    expect_match(h, "(none)", fixed = TRUE, info = ty)
    if (ty %in% c("text", "number", "list")) {
      expect_identical(as.character(r[[1L]]), base, info = ty)
    } else if (identical(ty, "flag")) {
      expect_identical(sub(" tpl-flag-field", "", as.character(r[[1L]]), fixed = TRUE), base)
    } else {
      expect_identical(sub('<option value="(none)">(none)</option>\n', "", h, fixed = TRUE), base, info = ty)
    }
  }
  # (the columns field has its own table: none)
  expect_identical(.tpl_none_wrap(widgets$text, "w", "columns", prm_opt, FALSE), widgets$text)
  # (tflspec's `unset`: "stop" cannot be left unset, so no (none); "omit", "step_off" and NA offer it)
  for (ty in c("text", "choice")) {
    w <- widgets[[ty]]
    stop_prm <- c(prm_opt, list(unset = "stop"))
    expect_identical(as.character(.tpl_none_wrap(w, "w", ty, stop_prm, FALSE)), as.character(w), info = ty)
    for (u in list("omit", "step_off", NA_character_)) {
      expect_match(as.character(.tpl_none_wrap(w, "w", ty, c(prm_opt, list(unset = u)), FALSE)), "(none)",
                   fixed = TRUE, info = paste(ty, u))
    }
  }
})

test_that("a code SPEC card: !unset shows (none), is chosen, kept in the SPEC YAML and out of the program", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  id <- "T-14-1-2"
  it0 <- Filter(function(i) i$fn == "plan_blanks", builder_items(s$planner, id))[[1L]]
  # the card before: the argument a value of the study standard, no (none) chosen
  h0 <- as.character(.plan_form_body("c1", "plan_blanks", it0))
  expect_false(grepl("rp-field-none", h0, fixed = TRUE))
  expect_match(h0, 'id="c1__first__none"', fixed = TRUE)
  # chosen on the card: its fields read back as unset
  r <- builder_card_read(list(c1__first__none = TRUE, c1__last = TRUE), "c1", .plan_form_schema("plan_blanks"),
                         args = c("first", "last"), logical_false = TRUE)
  expect_s3_class(r$first, "tfl_unset")
  expect_true(r$last)
  args <- c(it0$args[c("where", "last", "counted")], list(first = r$first))
  s$planner <- .builder_item_write(s$planner, id, "plan_blanks", args, 1L)
  it1 <- Filter(function(i) i$fn == "plan_blanks", builder_items(s$planner, id))[[1L]]
  expect_s3_class(it1$args$first, "tfl_unset")
  h1 <- as.character(.plan_form_body("c1", "plan_blanks", it1))
  expect_match(h1, 'class="rp-field rp-field-logical rp-field-none rp-has-value" data-arg="first"', fixed = TRUE)
  expect_match(h1, '<div class="rp-none-show">(none)</div>', fixed = TRUE)
  expect_match(h1, 'id="c1__first__none" class="rp-isnone" data-init="1" checked', fixed = TRUE)
  # the SPEC YAML keeps it, and a fresh open shows it again
  s <- suppressMessages(save_study(s, home = home))
  y <- readLines(file.path(s$path, "spec", "outputs", paste0(id, ".yml")), warn = FALSE)
  expect_true(any(grepl("first: !unset", y, fixed = TRUE)))
  s2 <- suppressMessages(open_study(basename(s$path), home = home))
  it2 <- Filter(function(i) i$fn == "plan_blanks", builder_items(s2$planner, id))[[1L]]
  expect_s3_class(it2$args$first, "tfl_unset")
  expect_match(as.character(.plan_form_body("c1", "plan_blanks", it2)), "rp-field-none", fixed = TRUE)
  # the program leaves it out
  code <- paste(program_code(s2$planner, id), collapse = "\n")
  expect_match(code, "plan_blanks(where", fixed = TRUE)
  expect_false(grepl("first =", code, fixed = TRUE))
  # (none) taken off: the argument is the study standard's again
  s2$planner <- .builder_item_write(s2$planner, id, "plan_blanks", it0$args[c("where", "last", "counted")], 1L)
  it3 <- Filter(function(i) i$fn == "plan_blanks", builder_items(s2$planner, id))[[1L]]
  expect_false(.is_unset(it3$args$first))
})

test_that("a code SPEC card's required argument has no (none); one kind after another has the same toggle", {
  req <- as.character(arg_field("c", "x", "string", required = TRUE))
  expect_false(grepl("rp-none|rp-isnone|\\(none\\)", req))
  opt <- as.character(arg_field("c", "x", "string", value = "a"))
  expect_match(opt, 'id="c__x__none" class="rp-isnone"', fixed = TRUE)
  expect_false(grepl("rp-field-none", opt, fixed = TRUE))
  for (ty in c("string", "number", "logical", "column", "enum[a|b]", "list<column>", "r")) {
    h <- as.character(arg_field("c", "x", ty, value = U(), choices = c("a", "b")))
    expect_match(h, "rp-field-none", fixed = TRUE, info = ty)
    expect_match(h, 'class="rp-isnone" data-init="1" checked', fixed = TRUE, info = ty)
  }
  # read back
  sch <- list(args = list(x = list(type = "string")), formals = "x")
  expect_s3_class(builder_card_read(list(c__x__none = TRUE, c__x = "a"), "c", sch)$x, "tfl_unset")
  expect_identical(builder_card_read(list(c__x__none = FALSE, c__x = "a"), "c", sch)$x, "a")
})
