# Template SPEC reports (#440): the inputs of a template's arguments, and
# the builder's card rules

test_that("an argument's input follows the type tflspec declares", {
  p <- function(...) utils::modifyList(list(name = "x", default = NULL, required = FALSE), list(...))
  expect_identical(.tpl_type(p(type = "vars", multiple = TRUE)), "vars")
  expect_identical(.tpl_type(p(type = "vars", multiple = FALSE)), "var")
  expect_identical(.tpl_type(p(type = "levels", of = "by")), "levels")
  expect_identical(.tpl_type(p(type = "flag")), "flag")
  expect_identical(.tpl_type(p(type = "text", multiple = TRUE)), "chips")
  expect_identical(.tpl_type(p(type = "choice", choices = c("a", "b"))), "choice")
  expect_identical(.tpl_type(p(type = "list", fields = c("name", "text"))), "list")
  # population and data: the study's
  expect_identical(.tpl_type(p(name = "population", type = "text")), "population")
  expect_identical(.tpl_type(p(name = "data", type = "text")), "data")
  # no type declared: guessed from the default
  expect_identical(.tpl_type(p(default = TRUE)), "flag")
  expect_identical(.tpl_type(p(default = 2)), "number")
})

test_that("a levels argument's variable is another argument's value, or a name", {
  params <- list(by = list(name = "by", default = "TRT01A"))
  expect_identical(.tpl_levels_of(list(of = "by"), params, list()), "TRT01A")
  expect_identical(.tpl_levels_of(list(of = "by"), params, list(by = "ARM")), "ARM")
  expect_identical(.tpl_levels_of(list(of = "PARAMCD"), params, list()), "PARAMCD")
  d <- list(ADSL = data.frame(TRT01A = c("B", "A", "B")))
  expect_identical(.tpl_levels(d, "TRT01A"), c("A", "B"))
})

test_that("a list argument reads and writes as lines of a small table", {
  named <- list(name = "labels", type = "list", fields = c("name", "text"))
  x <- c(AGE = "Age (years)", SEX = "Sex, n (%)")
  txt <- .tpl_list_text(x, named)
  expect_identical(txt, "AGE | Age (years)\nSEX | Sex, n (%)")
  expect_identical(.tpl_list_parse(txt, named), x)
  rec <- list(name = "columns", type = "list", multiple = TRUE,
              fields = c("vars", "label", "width"))
  v <- list(list(vars = "USUBJID", label = "Subject", width = 12L),
            list(vars = c("AETERM", "AEDECOD"), label = "Term"))
  txt <- .tpl_list_text(v, rec)
  expect_identical(txt, "USUBJID | Subject | 12\nAETERM, AEDECOD | Term")
  expect_identical(.tpl_list_parse(txt, rec), v)
  # nested deeper: read only (NULL)
  expect_null(.tpl_list_text(list(a = list(b = 1)), named))
})

test_that("an argument's origin: the report's, the study standard's, the default", {
  study <- list(defaults = list(content = list(
    list(fn = "plan_digits", args = list(rounding = "sas")))))
  prm <- list(name = "rounding", study = "plan_digits.rounding")
  expect_identical(.tpl_arg_origin(study, prm, list())$from, "study")
  expect_identical(.tpl_arg_origin(study, prm, list(rounding = "r"))$from, "own")
  expect_identical(.tpl_arg_origin(study, list(name = "x"), list())$from, "default")
})

test_that("the cards: required + the items in effect + added; no switches", {
  it <- function(fn, from = "own", below = NULL, revert = FALSE) {
    list(fn = fn, args = list(), from = from, path = fn, below = below, revert = revert,
         present = TRUE)
  }
  items <- list(it("normalize_ard", "defaults"), it("table_plan"), it("plan_cells", "defaults"),
                it("plan_stub", "defaults"), it("plan_filter"))
  cards <- .builder_items_cards(items)
  fns <- vapply(cards, function(x) x$fn, "")
  expect_setequal(fns, c("normalize_ard", "table_plan", "plan_cells", "plan_stub", "plan_filter"))
  expect_false(any(vapply(cards, function(x) isTRUE(x$switch), NA)))
  rm <- stats::setNames(vapply(cards, function(x) isTRUE(x$removable), NA), fns)
  # only the report's own item can be removed; the study standard's cannot
  expect_true(rm[["plan_filter"]])
  expect_false(rm[["plan_stub"]])
  expect_false(rm[["table_plan"]])
  # a new report not saved: the usual ones too, empty (not written)
  fresh <- .builder_items_cards(items, fresh = TRUE)
  fns2 <- vapply(fresh, function(x) x$fn, "")
  expect_true(all(.plan_tier_fns("usual") %in% fns2))
  expect_false(any(vapply(fresh[fns2 == "plan_levels"], function(x) isTRUE(x$present), NA)))
})

test_that("a template report shows only the cards it overrides or adds", {
  it <- function(fn, from, below = NULL) {
    list(fn = fn, args = list(), from = from, path = fn, below = below,
         revert = identical(from, "own") && !is.null(below), present = TRUE)
  }
  items <- list(it("table_plan", "template:demog"), it("normalize_ard", "template:demog"),
                it("plan_cells", "defaults"), it("plan_sort", "own", below = list()),
                it("plan_labels", "own"))
  cards <- .builder_items_cards(items, template = TRUE)
  expect_identical(vapply(cards, function(x) x$fn, ""), c("plan_sort", "plan_labels"))
  # no required card in a template report
  expect_false(any(vapply(cards, function(x) "required" %in% names(x) && isTRUE(x$required), NA)))
  # + Add a card: the template's pipe first, never its call's own verbs
  menu <- .plan_add_menu(cards, made = c("table_plan", "normalize_ard", "plan_cells", "plan_sort"),
                         template = TRUE)
  fns <- vapply(menu, function(m) m$fn, "")
  expect_false(any(c("normalize_ard", "table_plan") %in% fns))
  expect_identical(menu[[1L]]$group, "made")
  expect_true("plan_cells" %in% fns)
  expect_false(menu[[which(fns == "plan_sort")]]$ok)
})

test_that("a template report: one card a line after its call -- the study standard's too", {
  it <- function(fn, from, arg_from = from) {
    list(fn = fn, args = list(), from = from, arg_from = list(x = arg_from), path = fn,
         below = list(), revert = FALSE, present = TRUE)
  }
  items <- list(it("table_plan", "template:demog"), it("plan_cells", "template:demog",
                                                       c("defaults", "template:demog")),
                it("plan_labels", "template:demog"), it("plan_digits", "defaults"))
  cards <- .builder_items_cards(items, template = TRUE, code_fns = c("plan_cells", "plan_digits"))
  expect_identical(vapply(cards, function(x) x$fn, ""), c("plan_cells", "plan_digits"))
  # the study standard's mark, and no Remove
  expect_true(all(vapply(cards, function(x) identical(x$from, "defaults"), NA)))
  expect_false(any(vapply(cards, function(x) isTRUE(x$removable), NA)))
})

test_that("a study standard's card edited in a template report: the report's own, in its YAML", {
  skip_on_cran()
  # (summary: its stub is the study standard's card; demog takes it as arguments now)
  p <- .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
  o <- tflspec::tfl_output("T-99", "table", population = "SAF",
                           template = list(name = "summary",
                                         args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  p <- .spec_add_output(p, o, population = "SAF")
  it <- Filter(function(x) identical(x$fn, "plan_stub"), builder_items(p, "T-99"))[[1L]]
  expect_identical(it$from, "defaults")
  q <- .builder_item_write(p, "T-99", "plan_stub", utils::modifyList(it$args, list(indent = 3L)))
  own <- q$spec$outputs[["T-99"]]$content
  expect_true("plan_stub" %in% vapply(own, function(x) x$fn, ""))
  expect_identical(q$spec$outputs[["T-99"]]$template$name, "summary")
  it2 <- Filter(function(x) identical(x$fn, "plan_stub"), builder_items(q, "T-99"))[[1L]]
  expect_identical(it2$from, "own")
  expect_true(any(grepl("indent: 3", unlist(q$spec$text), fixed = TRUE)))
  # the file written for it: the template, its arguments and the card
  txt <- paste(q$spec$text[[q$spec$where[["T-99"]]]], collapse = "\n")
  expect_match(txt, "template:", fixed = TRUE)
  expect_match(txt, "plan_stub", fixed = TRUE)
})

test_that("an argument tflspec filled from the study is the study standard's, with its source", {
  prm <- list(name = "population", study = "output.population")
  og <- .tpl_arg_origin(list(), prm, list(), filled = list(study = "population",
                                                           args = list(population = "SAF")))
  expect_identical(og$from, "study")
  expect_identical(og$study, "SAF")
  expect_identical(og$source, "output.population")
  expect_identical(.tpl_arg_origin(list(), prm, list(population = "FAS"))$from, "own")
})

test_that("plan_digits in a template report: a variable's decimals the report's own, the rounding every report's", {
  skip_on_cran()
  # (summary: its plan_digits is the study standard's card; demog has no rounding argument now)
  p <- .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
  o <- tflspec::tfl_output("T-99", "table", population = "SAF",
                           template = list(name = "summary",
                                         args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  p <- .spec_add_output(p, o, population = "SAF")
  it <- Filter(function(x) identical(x$fn, "plan_digits"), builder_items(p, "T-99"))[[1L]]
  expect_identical(it$from, "defaults")
  # a variable's decimals: this table's (the rounding stays the study's)
  q <- .spec_set_item(p, "T-99", "plan_digits", c(it$args, list(AGE = 1L)), below_args = "rounding")
  own <- Filter(function(x) identical(x$fn, "plan_digits"), q$spec$outputs[["T-99"]]$content)
  expect_length(own, 1L)
  expect_identical(own[[1L]]$args$AGE, 1L)
  expect_null(own[[1L]]$args$rounding)
  expect_identical(Filter(function(x) identical(x$fn, "plan_digits"), builder_items(q, "T-99"))[[1L]]$from, "own")
  # the rounding: the study's defaults.yml, every report's -- not the report's file
  r <- .spec_set_rounding(q, "r")
  expect_true(any(grepl("rounding: r", r$spec$text[["spec/defaults.yml"]], fixed = TRUE)))
  expect_false(any(grepl("rounding", r$spec$text[[r$spec$where[["T-99"]]]], fixed = TRUE)))
  # the field says so
  h <- as.character(.rounding_field("c", "r"))
  expect_match(h, "Applies to every report", fixed = TRUE)
})

test_that("a template report converted to code: its items on the usual cards, its program without the template", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  o <- tflspec::tfl_output("T-TPL", "table", label = "Demographics", population = "SAF",
                           template = list(name = "demog",
                                         args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  s$planner <- .spec_add_output(s$planner, o, label = "Demographics", population = "SAF")
  expect_false(is.null(.tpl_of(s$planner, "T-TPL")))
  p <- .tpl_to_code(s$planner, "T-TPL")
  expect_null(.tpl_of(p, "T-TPL"))
  b <- p$spec$outputs[["T-TPL"]]
  expect_null(b$template)
  expect_gt(length(b$content), 0L)
  expect_identical(b$label, "Demographics")
  # saved: the file and the program say the items, no template call
  s$planner <- p
  s <- suppressMessages(save_study(s, home = home))
  expect_null(tflspec::tfl_read_output(file.path(s$path, "spec", "outputs", "T-TPL.yml"))$template)
  prog <- readLines(file.path(s$path, report_info(s$planner, "T-TPL")$program_path %||%
                                file.path("programs", "tfl", "T-TPL.R")), warn = FALSE)
  expect_false(any(grepl("tfl_tpl(", prog, fixed = TRUE)))
  expect_true(any(grepl("plan_cells", prog, fixed = TRUE)))
  # (not a template report: said)
  expect_error(.tpl_to_code(p, "T-TPL"), "not a template report")
})

test_that("the step an argument is drawn in: the table, the name's default, else ARD (#514)", {
  tab <- .tpl_section_table()
  expect_identical(names(tab), c("template", "argument", "section"))
  expect_true(all(tab$section %in% .tpl_sections))
  expect_false(anyDuplicated(tab[c("template", "argument")]) > 0L)
  # a table hit wins over the name's default (param_order filters the data,
  # and is drawn with the content: the user's decision)
  expect_identical(.tpl_arg_section(tab$template[1L], tab$argument[1L]), tab$section[1L])
  i <- which(tab$argument == "param_order")[1L]
  expect_identical(.tpl_arg_section(tab$template[i], "param_order"), "content")
  # not in the table (a company template, a variant): by the name
  for (a in c("population", "subset", "where", "data", "param", "paramcd")) {
    expect_identical(.tpl_arg_section("company_tpl", a), "data")
  }
  for (a in c("labels", "heading", "order", "sort", "formats", "total_label", "visit_order")) {
    expect_identical(.tpl_arg_section("company_tpl", a), "content")
  }
  for (a in c("title", "param_page", "param_heading")) {
    expect_identical(.tpl_arg_section("company_tpl", a), "page")
  }
  # anything else: the ARD
  expect_identical(.tpl_arg_section("company_tpl", "statistics"), "ard")
  expect_identical(.tpl_arg_section("company_tpl", "a_new_argument"), "ard")
})

test_that("a code report's steps and a template report's: the arguments drawn once (#514)", {
  ui <- as.character(app_ui())
  at <- function(id) regexpr(sprintf('id="%s"', id), ui, fixed = TRUE)[[1L]]
  # the analysis data before the ARD; the template's arguments in their steps
  step_at <- function(v) regexpr(sprintf('data-value="%s"', v), ui, fixed = TRUE)[[1L]]
  expect_true(step_at("data") < step_at("ard") && step_at("ard") < step_at("content") &&
                step_at("content") < step_at("page"))
  for (id in c("ard_adata", "ard_report_pop", "adata_detail", "tpl_card_data", "tpl_card_page",
               "ard_outline")) {
    expect_equal(lengths(gregexpr(sprintf('id="%s"', id), ui, fixed = TRUE)), 1L, info = id)
  }
  # the analysis data is the first step's, the analyses the ARD step's
  expect_true(at("ard_adata") < at("ard_outline"))
  expect_true(at("tpl_card_data") < at("ard_outline"))
  expect_true(at("tpl_card_page") > at("ard_outline"))
})

test_that("a table's rework is at the top of the Content step, once", {
  ui <- as.character(app_ui())
  at <- function(id) regexpr(sprintf('id="%s"', id), ui, fixed = TRUE)[[1L]]
  expect_equal(lengths(gregexpr('id="rework_table"', ui, fixed = TRUE)), 1L)
  expect_true(at("rework_table") > at("ard_outline"))
  expect_true(at("rework_table") < at("content_mode_bar"))
  # a listing's stays in the Analysis data step
  expect_true(at("rework_data") < at("ard_outline"))
})

test_that("post_header: an emptied field goes back to the default, a text argument in general keeps ''", {
  ph <- list(name = "post_header", default = "{col}", required = FALSE)
  # (the field's own value for an empty field is NULL; "" is taken the same)
  expect_null(.tpl_value("", "text", ph))
  expect_length(.tpl_set_arg(list(post_header = "x {col}"), ph, .tpl_value("  ", "text", ph)), 0L)
  # a value stays
  expect_identical(.tpl_set_arg(list(), ph, "Post {col}"), list(post_header = "Post {col}"))
  # not text arguments in general: "" is a value (sd_missing)
  sm <- list(name = "sd_missing", default = "NA", required = FALSE)
  expect_identical(.tpl_set_arg(list(), sm, ""), list(sd_missing = ""))
})

test_that("post_header: the soft warning says which, or nothing", {
  expect_identical(.tpl_header_checked, "post_header")
  nocol <- "The header has no {col}: each category's column gets the same text."
  braces <- "The header's braces don't match: it prints as typed."
  expect_null(.tpl_header_problem("{col}"))
  expect_null(.tpl_header_problem("Post {col}"))
  expect_null(.tpl_header_problem("{col1} / {col2}"))
  expect_null(.tpl_header_problem("Post\\n{col}"))
  expect_null(.tpl_header_problem("{{col}}"))
  expect_identical(.tpl_header_problem("Post-baseline"), nocol)
  expect_identical(.tpl_header_problem("{n}"), nocol)
  expect_identical(.tpl_header_problem("{col"), braces)
  expect_identical(.tpl_header_problem("col}"), braces)
  expect_identical(.tpl_header_problem("{col}}"), braces)
  expect_identical(.tpl_header_problem("}{col}{"), braces)
  expect_identical(.tpl_header_problem("{{col}"), braces)
  # blank, or another argument: nothing
  expect_null(.tpl_header_problem(""))
  expect_null(.tpl_header_problem(NULL))
  expect_null(.tpl_header_problem("no braces", name = "heading"))
  # both texts have their Japanese
  for (m in c(nocol, braces)) expect_false(identical(tr(m, "ja"), m))
})

test_that("the Japanese says one word for post-baseline, and the tip has its \\n row", {
  s <- .strings()
  ja <- s$ja
  expect_false(any(grepl("\u30dd\u30b9\u30c8\u30d9\u30fc\u30b9\u30e9\u30a4\u30f3", ja)))
  expect_identical(tr("Worst post-baseline category", "ja"), "\u30d9\u30fc\u30b9\u30e9\u30a4\u30f3\u5f8c\u306e\u6700\u60aa\u306e\u30ab\u30c6\u30b4\u30ea")
  tip <- "the header over each post-baseline category (`{col}`: the category; a typed \\n breaks the line)"
  expect_true(tip %in% s$en)
  expect_match(tr(tip, "ja"), "\u3068\u5165\u529b\u3059\u308b\u3068\u6539\u884c\uff09$")
  expect_true(grepl("\\n", tr(tip, "ja"), fixed = TRUE))
  # the code-list copy dialog's button has its own text; the common Copy stays
  expect_identical(tr("Copy the lists", "ja"), "\u30b3\u30d4\u30fc")
  expect_identical(tr("Copy", "ja"), "\u8907\u88fd")
})

test_that("a template report's preview is its content run on its ARD, not the study defaults' sheets", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  # the lab data the shift table reads (tflspec's simulated ADLB)
  saveRDS(tflspec::adlb, file.path(s$path, "data", "adam", "adlb.rds"))
  y <- readLines(file.path(s$path, "spec", "data.yml"), warn = FALSE)
  i <- grep("path: data/adam/advs.rds", y, fixed = TRUE)
  writeLines(append(y, c("  - dataset: ADLB", "    level: ADaM", "    path: data/adam/adlb.rds"),
                    after = i),
             file.path(s$path, "spec", "data.yml"))
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  o <- tflspec::tfl_output("T-TPL", "table", label = "Shift", population = "SAF",
                           template = list(name = "shift",
                                           args = list(population = "SAF",
                                                       post_header = "Worst: {col}")))
  s$planner <- .spec_add_output(s$planner, o, label = "Shift", population = "SAF")
  s <- suppressMessages(save_study(s, home = home))
  # a template report is drawn by its content; a plain table of the ARD, not
  expect_true(.preview_by_content(s$planner, "T-TPL"))
  expect_false(.preview_by_content(s$planner, "T-14-1-2"))
  suppressMessages(fetch_ard(s, "T-TPL", home = home))
  ard <- ard_raw(s, "T-TPL", home = home)
  skip_if(is.null(ard), "the report's ARD was not made")
  pages <- .preview_content_pages(s$planner, "T-TPL", list(ard = ard))
  hdr <- lapply(pages[[1L]]$col_header, function(l) {
    if (is.list(l)) vapply(l, function(z) z$label, "") else l
  })
  # the template's own header: the argument's text over each category, the
  # arms' N over their columns (not the defaults' "LOW NORMAL HIGH" over NA)
  expect_true(all(c("Worst: LOW", "Worst: NORMAL", "Worst: HIGH") %in% unlist(hdr)))
  expect_true(any(grepl("^Drug X \\(N=[0-9]+\\)$", unlist(hdr))))
  expect_false(any(grepl("N=NA", unlist(hdr), fixed = TRUE)))
})

test_that("a template figure and a template listing have the table's five tabs too (#514)", {
  ui <- htmltools::tagQuery(app_ui())
  for (id in c("fig_right", "lf_right")) {
    links <- ui$find(paste0("#", id))$find("a")$selectedTags()
    v <- vapply(links, function(x) x$attribs[["data-value"]] %||% "", "")
    expect_identical(unname(v[nzchar(v)]), c("spec", "ard_code", "ard", "code", "result"), info = id)
  }
  html <- as.character(app_ui())
  # each pane's output is in the page once (an output id twice breaks the page)
  ids <- regmatches(html, gregexpr('id="(tpl_(ard|spec|code)[a-z_]*|program_[a-z]+)"', html))[[1L]]
  expect_identical(anyDuplicated(ids), 0L)
  for (o in c("tpl_spec_yaml_lf", "tpl_spec_yaml_fig", "tpl_ard_pane_lf", "tpl_ard_pane_fig",
              "tpl_code_expanded_fig", "program_fig", "tpl_ard_code_lf")) {
    expect_true(grepl(sprintf('id="%s"', o), html, fixed = TRUE), info = o)
  }
  # the tab rules and the greying are for the three
  expect_true(grepl("#table_right, #fig_right, #lf_right", .result_tabs_css, fixed = TRUE))
  expect_true(grepl("#table_right, #fig_right, #lf_right", .tpl_js, fixed = TRUE))
})

test_that("a template report's step has five tabs in the order they are made, the ARD's two greyed out when it has none (#514)", {
  ui <- htmltools::tagQuery(app_ui())
  links <- ui$find("#table_right")$find("a")$selectedTags()
  v <- vapply(links, function(x) x$attribs[["data-value"]] %||% "", "")
  expect_identical(unname(v[nzchar(v)]),
                   c("spec", "ard_code", "ard", "code", "result"))
  # the other lists of tabs are the three, named plainly
  tabs <- as.character(result_tabs_ui("x", "s", "c", "r"))
  expect_false(grepl("ard", tabs, fixed = TRUE))
  expect_false(grepl("rp-lab-tpl", tabs, fixed = TRUE))
  # a template report's are named for what they are, shown with body.rp-tpl
  tpl <- as.character(result_tabs_ui("x", "s", "c", "r", ard = "a", ard_code = "b"))
  expect_true(grepl("Template SPEC", tpl, fixed = TRUE) && grepl("Report result", tpl, fixed = TRUE))
  # in the HTML: Template SPEC, ARD code, ARD result, Report code, Report result
  at <- vapply(c("Template SPEC", "ARD code", "ARD result", "Report code", "Report result"),
               function(x) regexpr(x, tpl, fixed = TRUE)[[1L]], 1L)
  expect_true(all(at > 0L) && !is.unsorted(at, strictly = TRUE))
  expect_true(grepl("body.rp-tpl :is(#table_right, #fig_right, #lf_right) .rp-lab-plain", .result_tabs_css, fixed = TRUE))
  # a code report has no ARD tabs; a template report's are disabled (not hidden) without an ARD
  expect_true(grepl("body:not(.rp-tpl) :is(#table_right, #fig_right, #lf_right) > .nav-item", .result_tabs_css, fixed = TRUE))
  expect_true(grepl("rp-ard-tabs", .tpl_js, fixed = TRUE))
  expect_true(grepl("toggleClass('disabled', !on)", .tpl_js, fixed = TRUE))
  # ARD code is read as Report code is: numbered lines and Copy
  html <- as.character(app_ui())
  expect_true(grepl('data-copy="tpl_ard_code"', html, fixed = TRUE))
  expect_false(grepl("text-decoration: line-through", .result_tabs_css, fixed = TRUE))
  # an outdated ARD: Report result says so in one line (both languages)
  expect_false(identical(tr("ARD outdated: Update preview", "ja"), "ARD outdated: Update preview"))
  # their names in Japanese
  expect_identical(tr("Report code", "ja"), "\u5e33\u7968\u30b3\u30fc\u30c9")
  expect_identical(tr("ARD code", "ja"), "ARD \u30b3\u30fc\u30c9")
  expect_identical(tr("ARD result", "ja"), "ARD \u7d50\u679c")
})


# ---- a template listing is written and previewed from its content ----------

.listing_tpl_study <- function(extra = list(), id = "L-TPL") {
  home <- local_home(parent.frame())
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  args <- utils::modifyList(
    list(data = "ADAE",
         columns = list(list(vars = "USUBJID", label = "Subject"),
                        list(vars = "AEDECOD", label = "Term", width = 30L))),
    extra)
  o <- tflspec::tfl_output(id, "listing", label = "AE listing",
                           template = list(name = "listing", args = args))
  s$planner <- .spec_add_output(s$planner, o, label = "AE listing")
  suppressWarnings(suppressMessages(save_study(s, home = home)))
}
.listing_has_by <- function() "by" %in% names(tflspec::tfl_template_params("listing"))

test_that("a template listing's program is the template's plan, not the listing sheets' code", {
  skip_on_cran()
  s <- .listing_tpl_study()
  expect_true(.is_template_listing(s$planner, "L-TPL"))
  expect_false(.is_template_listing(s$planner, "L-16-2-7"))
  code <- paste(program_code(s$planner, "L-TPL"), collapse = "\n")
  expect_match(code, "plan_paginate_rows(max_rows = 22)", fixed = TRUE)
  expect_match(code, "plan_listing(", fixed = TRUE)
  expect_false(grepl("listing_spec", code, fixed = TRUE))
  expect_false(grepl("as_rtftables", code, fixed = TRUE))
  # a listing defined in rows keeps its code
  old <- paste(program_code(s$planner, "L-16-2-7"), collapse = "\n")
  expect_match(old, "listing_spec", fixed = TRUE)
  expect_false(grepl("plan_paginate_rows", old, fixed = TRUE))
})

test_that("a template listing's preview is its content: pages at 22 rows, as the program's", {
  skip_on_cran()
  s <- .listing_tpl_study()
  expect_true(.preview_by_content(s$planner, "L-TPL"))
  pages <- preview_listing(s, "L-TPL")
  expect_gt(length(pages), 1L)
  expect_true(all(vapply(pages, function(p) nrow(p$data), 1L) <= 22L))
  # (the program writes the same number of pages: a section a page)
  owd <- setwd(s$path)
  on.exit(setwd(owd), add = TRUE)
  out <- system2(file.path(R.home("bin"), "Rscript"), "programs/tfl/L-TPL.R", stdout = TRUE, stderr = TRUE)
  expect_true(file.exists("output/tfl/L-TPL.rtf"), info = paste(out, collapse = "\n"))
  rtf <- readLines("output/tfl/L-TPL.rtf", warn = FALSE)
  expect_equal(sum(lengths(regmatches(rtf, gregexpr("sbkpage", rtf, fixed = TRUE)))), length(pages))
})

test_that("a template listing's `by` divides its pages in the preview and the program", {
  skip_on_cran()
  skip_if_not(.listing_has_by(), "the installed tflspec has no `by` for a listing template")
  s <- .listing_tpl_study(list(by = "AESEV"))
  code <- paste(program_code(s$planner, "L-TPL"), collapse = "\n")
  expect_match(code, "page_by = \"AESEV\"", fixed = TRUE)
  pages <- preview_listing(s, "L-TPL")
  n <- length(unique(readRDS(file.path(s$path, "data/adam/adae.rds"))$AESEV))
  expect_gte(length(pages), n)
})

test_that("a template listing converted to code prints the same pages: `by`, `by_break`, `by_heading` kept (#605)", {
  skip_on_cran()
  skip_if_not(.listing_has_by(), "the installed tflspec has no `by` for a listing template")
  # a child R runs the program; its RTF: the pages, and each page's heading
  run_rtf <- function(s) {
    owd <- setwd(s$path)
    on.exit(setwd(owd), add = TRUE)
    unlink("output/tfl/L-TPL.rtf")
    out <- system2(file.path(R.home("bin"), "Rscript"), "programs/tfl/L-TPL.R", stdout = TRUE, stderr = TRUE)
    expect_true(file.exists("output/tfl/L-TPL.rtf"), info = paste(out, collapse = "
"))
    rtf <- readLines("output/tfl/L-TPL.rtf", warn = FALSE)
    list(pages = sum(lengths(regmatches(rtf, gregexpr("sbkpage", rtf, fixed = TRUE)))),
         text = unlist(regmatches(rtf, gregexpr("Severity [A-Z]+", rtf))))
  }
  for (case in list(list(by_break = "page", by_heading = "Severity {value}"),
                    list(by_break = "block"))) {
    s <- .listing_tpl_study(c(list(by = "AESEV"), case))
    home <- dirname(dirname(s$path))  # (the study's home)
    before <- run_rtf(s)
    expect_gt(before$pages, 1L)
    # Convert to code, written and run again
    s$planner <- .tpl_to_code(s$planner, "L-TPL")
    expect_false(.is_template_listing(s$planner, "L-TPL") && !is.null(.code_spec(s$planner)$outputs[["L-TPL"]]$template))
    code <- paste(program_code(s$planner, "L-TPL"), collapse = "
")
    expect_false(grepl("listing_spec", code, fixed = TRUE))
    expect_match(code, "plan_listing(", fixed = TRUE)
    s <- suppressWarnings(suppressMessages(save_study(s, home = home)))
    after <- run_rtf(s)
    expect_equal(after$pages, before$pages)
    expect_equal(after$text, before$text)
    # (the preview is the same pages too)
    expect_length(preview_listing(s, "L-TPL"), before$pages)
    if (identical(case$by_break, "page")) expect_gt(length(before$text), 0L)
    # the converted report reopens as a code one, its pages unchanged
    s2 <- open_study(s$path)
    expect_length(preview_listing(s2, "L-TPL"), before$pages)
  }
})

test_that("a converted template listing follows an edit of its columns: program, preview and YAML (#605)", {
  skip_on_cran()
  skip_if_not(.listing_has_by(), "the installed tflspec has no `by` for a listing template")
  s <- .listing_tpl_study(list(by = "AESEV", by_break = "page", by_heading = "Severity {value}"))
  home <- dirname(dirname(s$path))
  p <- .tpl_to_code(s$planner, "L-TPL")
  # what the Columns grid writes: a width and a heading
  d <- lf_rows(p, "listing_cols", "L-TPL")
  d$output_id <- NULL
  d$width[d$vars == "AEDECOD"] <- "41"
  d$label[d$vars == "AEDECOD"] <- "Event term"
  s$planner <- .lf_follow_content(set_lf_rows(p, "listing_cols", "L-TPL", d), "L-TPL")
  expect_true(.is_template_listing(s$planner, "L-TPL"))
  code <- paste(program_code(s$planner, "L-TPL"), collapse = "
")
  expect_match(code, "listing_col(vars = \"AEDECOD\", label = \"Event term\", width = 41)", fixed = TRUE)
  expect_match(code, "page_by = \"AESEV\"", fixed = TRUE)
  expect_false(grepl("listing_spec", code, fixed = TRUE))
  expect_gt(length(preview_listing(s, "L-TPL")), 5L)
  s <- suppressWarnings(suppressMessages(save_study(s, home = home)))
  y <- paste(readLines(file.path(s$path, "spec/outputs/L-TPL.yml"), warn = FALSE), collapse = "
")
  expect_match(y, "label: Event term", fixed = TRUE)
  expect_match(y, "width: 41", fixed = TRUE)
  expect_match(y, "page_by: AESEV", fixed = TRUE)
  expect_match(paste(program_code(open_study(s$path)$planner, "L-TPL"), collapse = "
"), "width = 41", fixed = TRUE)
})

test_that("a template listing whose definition does not hold says so once, in the preview and the program", {
  skip_on_cran()
  skip_if_not(.listing_has_by(), "the installed tflspec has no `by` for a listing template")
  s <- suppressWarnings(.listing_tpl_study(list(by_break = "block")))
  expect_error(suppressWarnings(preview_listing(s, "L-TPL")), "The definition of L-TPL does not hold: .*by_break", perl = TRUE)
  # (the program is still written, and stops with that)
  code <- suppressWarnings(program_code(s$planner, "L-TPL"))
  expect_true(any(grepl("^stop\\(\"tflplanner: the definition of L-TPL does not hold", code)))
})

test_that("the listing preview's guard is for a listing of rows: a template listing has its columns as an argument", {
  skip_on_cran()
  s <- .listing_tpl_study()
  shiny::testServer(function(input, output, session) app_server(input, output, session, s), {
    session$setInputs(target = "L-TPL")
    session$setInputs(lf_preview = 1)
    html <- as.character(output$lf_preview_out$html)
    expect_false(grepl("has no columns yet", html, fixed = TRUE))
    expect_match(html, "pages")
  })
  # (the sheets' listings: a listing with no columns says so)
  expect_false(grepl("Start the listing from the data", t("The listing has no columns yet: add them to the table of columns (below the form)."), fixed = TRUE))
})

test_that("a template figure and listing have Update preview above their tabs, one label; a listing in rows keeps its button (#514)", {
  html <- as.character(app_ui())
  for (o in c("tpl_fig_bar", "tpl_lf_bar")) expect_true(grepl(sprintf('id="%s"', o), html, fixed = TRUE), info = o)
  # the bar is above the tabs, in the pane that stays in view
  expect_true(grepl('rp-tpl-right[^<]*">\\s*<div id="tpl_fig_bar"', html, perl = TRUE))
  # the listing's own button is for a listing in rows only
  expect_true(grepl('rp-code-only">\\s*<button[^>]*id="lf_preview"', html, perl = TRUE))
  expect_true(grepl("tpl_ard_of", paste(deparse(.srv_add_report), collapse = " "), fixed = TRUE))
})

test_that("the right pane of a template figure, listing and table fills its column, and only the tab shown scrolls (#612)", {
  css <- gsub("[[:space:]]+", " ", .result_tabs_css)
  # (no align-self: start on the figure's and listing's pane: it shrank to its content)
  pane <- regmatches(css, regexpr("[.]rp-tpl-right [{] position: sticky;[^}]*[}]", css))
  expect_length(pane, 1L)
  expect_false(grepl("align-self", pane, fixed = TRUE))
  # the tabs of the table's pane fill it
  expect_true(grepl(".rp-b-right > .rp-tabs { align-self: stretch; }", css, fixed = TRUE))
  # the tab row stays: the pane's box does not scroll, the tab content does
  expect_true(grepl(".rp-tpl-right > .rp-tabs { position: static;", css, fixed = TRUE))
  expect_true(grepl("overflow: hidden;", regmatches(css, regexpr("[.]rp-tpl-right > [.]rp-tabs [{][^}]*[}]", css)), fixed = TRUE))
  expect_true(grepl(".rp-tpl-right > .rp-tabs > .tabbable > .tab-content { flex: 1 1 auto; min-height: 0; overflow-y: auto; }", css, fixed = TRUE))
})

test_that("a template listing with no columns left says so in Report result, not old pages (#514)", {
  skip_on_cran()
  s <- .listing_tpl_study(list(columns = NULL))
  shiny::testServer(function(input, output, session) app_server(input, output, session, s), {
    session$setInputs(target = "L-TPL")
    html <- as.character(output$lf_preview_out$html)
    expect_match(html, "Required arguments are empty: columns", fixed = TRUE)
    expect_false(grepl("pages", html, fixed = TRUE))
  })
})
