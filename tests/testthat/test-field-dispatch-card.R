# The one field dispatcher draws every card field as before (step 1 of
# s1-tools one-dispatcher-plan-2026-11-25.md: no visible change).  A walk,
# not cases: every argument of every function in tflspec's schema, drawn
# by arg_field() -- now the card surface of field_ui() (R/field_ui.R) --
# and by .legacy_arg_field(), the verbatim copy of arg_field() before the
# move (helper-legacy-fields.R), with the arguments a card gives
# (.spec_field()).  The two HTML strings must be the same.  Values: none,
# "(none)" where the argument may be left out, R, a list of two texts and
# the values field_samples() (helper-card-fields.R) gives its kind; English
# for all, Japanese for a few functions; a field with every frame mark
# (required, badge, nav, help link, the card's sources); a field inside a
# call's small form (nested).

# the functions of the schema
.dispatch_fns <- local({
  sch <- tryCatch(tflspec::tfl_schema(), error = function(e) NULL)
  sort(names(sch))
})
# the columns a card offers, and what it knows for the choice words
.dispatch_cols <- c("row_label", ".values", "AGE", "SEX", "RACE", "TRT01A")
.dispatch_sources <- list(
  vars = c("AGE", "SEX", "RACE", "TRT01A"), levels = c("Placebo", "Drug A"),
  codelist = c("C1", "C2"), columns = c("COL1", "COL2"), populations = c("SAF", "ITT"),
  report_data = c("adsl", "adae"), study_data = c("ADSL", "ADAE"), ard = c("ard_1"),
  study_info = c("STUDYID"), tokens = c("{n}", "{p}"), stats = c("n", "mean", "sd"),
  styles = c("default"))
# the functions drawn in Japanese too
.dispatch_ja <- c("plan_style", "plan_filter", "plan_columns", "plan_titles", "normalize_ard",
                  "rtf_border")

# a field's HTML (an error: its message -- both must fail alike); an id
# drawn at random by a widget, if any, made the same in both
.dispatch_html <- function(f, args) {
  h <- tryCatch(as.character(htmltools::doRenderTags(do.call(f, args))),
                error = function(e) paste("<error>", conditionMessage(e)))
  .dispatch_r_switch_as_before(gsub("(bslib-[a-z]+-)[0-9]+", "\\1N", h))
}

# The R switch's frame changed on purpose after the GUI review of #855 (its
# rule, R/field_ui.R .r_carry(): the field's data-carry, data-r-fns,
# data-r-inner and data-r-num; the switch's tooltip and notes; the R box's placeholder): left
# out of both, so that the rest is still compared with the copy
.dispatch_r_switch_as_before <- function(h) {
  h <- gsub(' data-(carry|r-fns|r-inner|r-num)="[^"]*"', "", h)
  h <- gsub('<label class="rp-r-toggle"[^>]*>', '<label class="rp-r-toggle">', h)
  gsub('(rp-r-input" value="[^"]*") placeholder="[^"]*"', "\\1", h)
}

# a value in a failure's message
.dispatch_show <- function(v) paste(deparse(v, width.cutoff = 200L), collapse = " ")

# the values an argument is drawn with
.dispatch_values <- function(d, fn, a) {
  sm <- tryCatch(field_samples(d, fn, a), error = function(e) list())
  no_unset <- isTRUE(d$required) || identical(d$unset, "stop")
  c(list(NULL), if (!no_unset) list(tflspec::tfl_unset()), list(tflspec::tfl_r("toupper(x)")),
    list(list("Text A", "Text B")), sm)
}

# arg_field()'s arguments as a card gives them (.spec_field()); `help`:
# .arg_help()'s
.dispatch_args <- function(fn, a, d, v, help, lang = "en") {
  if (isTRUE(d$dots)) v <- .spec_dots_text(fn, a, v)
  cols <- if (.type_has_column(d$def_type %||% d$type) || identical(.def_input(d), "condition")) {
    .dispatch_cols
  }
  list(id = "c", arg = a, type = d$type, label = d$label, value = v, default = d$default,
       choices = cols, from = "own", help = help, lang = lang, def = d)
}

# the same HTML from both; `what` names the case
.dispatch_same <- function(args, what) {
  old <- .dispatch_html(.legacy_arg_field, args)
  new <- .dispatch_html(arg_field, args)
  expect_identical(new, old, info = what)
}

for (fn in .dispatch_fns) local({
  fn <- fn
  test_that(sprintf("%s(): every card field draws as before the dispatcher", fn), {
    skip_on_cran()
    defs <- tryCatch(spec_card_defs(fn), error = function(e) NULL)
    if (is.null(defs)) skip(sprintf("%s: no card definitions", fn))
    langs <- if (fn %in% .dispatch_ja) c("en", "ja") else "en"
    for (a in setdiff(defs$formals, .data_args)) {
      d <- defs$args[[a]]
      vs <- .dispatch_values(d, fn, a)
      help <- .arg_help(fn, a, defs, "en")
      for (lang in langs) {
        hl <- if (identical(lang, "en")) help else .arg_help(fn, a, defs, lang)
        for (v in vs) {
          .dispatch_same(.dispatch_args(fn, a, d, v, hl, lang),
                         sprintf("%s(%s = %s) [%s]", fn, a, .dispatch_show(v), lang))
        }
      }
      # (every mark of the frame, and the card's sources for the choice words)
      v1 <- vs[[length(vs)]]
      rich <- utils::modifyList(.dispatch_args(fn, a, d, v1, help),
                                list(required = TRUE, from = "defaults", own = "This table",
                                     nav = "A hint for the field.",
                                     help_link = shiny::tags$a(href = "#", "Help"),
                                     ctx = .dispatch_sources))
      .dispatch_same(rich, sprintf("%s(%s = %s) [frame marks, sources]", fn, a, .dispatch_show(v1)))
      # (inside a call's small form; the R switch said by the caller)
      for (r_ok in list(NULL, TRUE, FALSE)) {
        nest <- c(.dispatch_args(fn, a, d, v1, help), list(nested = TRUE, r_ok = r_ok))
        .dispatch_same(nest, sprintf("%s(%s = %s) [nested, r_ok = %s]", fn, a, .dispatch_show(v1),
                                     .dispatch_show(r_ok)))
      }
    }
    succeed()
  })
})

test_that("the fields of a call's small form draw as before the dispatcher (nested)", {
  skip_on_cran()
  # (the functions a call<fn> type names, as .field_call() draws them:
  # each argument nested, its help the schema's)
  types <- unlist(lapply(.dispatch_fns, function(fn) {
    lapply(.schema_of(fn)$args, function(x) x$type)
  }))
  call_fns <- unique(unlist(lapply(types, .type_call_fn)))
  expect_true("rtf_border" %in% call_fns)
  for (cf in call_fns) {
    ent <- .schema_of(cf)
    if (is.null(ent)) next
    for (a in names(ent$args)) {
      d <- ent$args[[a]]
      for (v in .dispatch_values(d, cf, a)) {
        for (lang in c("en", "ja")) {
          args <- list(id = "c__border", arg = a, type = d$type, label = d$label, value = v,
                       default = d$default, help = .schema_help(cf, a), lang = lang, nested = TRUE,
                       def = d)
          .dispatch_same(args, sprintf("%s(%s = %s) [nested, %s]", cf, a, .dispatch_show(v), lang))
        }
      }
    }
  }
  # (a call<rtf_border> field with a call as its value: its small form, or
  # its R while rtf_border() has no main argument)
  b <- tflspec::tfl_call("rtf_border", list(top = "single", all = TRUE))
  for (ty in c("call<rtf_border>", "string|call<rtf_border>|r")) {
    args <- list(id = "c", arg = "border", type = ty, value = b, def = list(type = ty, input = "text"))
    .dispatch_same(args, sprintf("border (%s) = rtf_border(...)", ty))
  }
})

test_that("the dispatcher's card surface and its word", {
  # (field_look(): the kind arg_field() draws, the class it gives)
  d <- list(type = "list<column>", input = "pick", ordered = TRUE)
  expect_identical(field_look(d), "pick_many")
  h <- as.character(htmltools::doRenderTags(arg_field("c", "keep", d$type, def = d)))
  expect_match(h, "rp-field rp-field-list", fixed = TRUE)
  expect_match(h, "rp-ordered-hint", fixed = TRUE)
  expect_identical(field_look(list(type = "string", input = "pick", choices = "populations")), "string")
  expect_identical(field_look(list(type = "map<column,r>", input = "table")), "table")
  expect_identical(field_look(NULL), "r")
  # (field_ui() with a field_ctx() is arg_field())
  fc <- field_ctx("card", type = "integer", default = 26L, label = "Rows")
  expect_identical(as.character(htmltools::doRenderTags(field_ui("c", "max_rows", NULL, 3L, fc))),
                   as.character(htmltools::doRenderTags(arg_field("c", "max_rows", "integer", label = "Rows",
                                                                  value = 3L, default = 26L))))
  # (the template surface is tpl_widget()'s until step 2)
  expect_error(field_ui("c", "x", list(type = "string"), NULL, field_ctx("template")),
               "tpl_widget", fixed = TRUE)
  expect_error(field_look(list(type = "string"), fc = field_ctx("template")), "tpl_widget", fixed = TRUE)
})
