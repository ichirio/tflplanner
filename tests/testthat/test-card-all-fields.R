# Every field of every card draws, reads back, saves and reopens (RULES
# section 4: "whether everything is there" is a test).  A walk, not cases:
#   * every function the generated cards draw (R/spec_card.R: the table
#     verbs of .plan_form_fns and the arguments under the forms of their
#     own, .special_covered), every argument but the data (.data_args);
#   * every parameter of every template the template cards draw, but the
#     hidden ones;
# each with the values field_samples() (helper-card-fields.R) gives its
# kind of field -- none the default, so that a field that is not there
# fails, never passes on a default filled in.  For each value:
#   draw      the card drawn from args = list(<arg> = value) shows it;
#   read      the inputs the drawn card sends read back to the same args;
#   save      the item in a report, the study saved (save_study()) and
#             opened again: the report says the same value.
# A case that fails for a reason not fixed yet is in the table below, with
# its issue: it is skipped (and listed as skipped), so the table shrinks
# visibly as the issues are closed.

.walk_skips <- data.frame(
  what = character(),
  arg = character(),
  step = character(),
  issue = character(),
  stringsAsFactors = FALSE)

# the issue a case is skipped for (NA: none)
.walk_skip <- function(what, arg, step) {
  at <- .walk_skips$what == what & .walk_skips$arg == arg & .walk_skips$step == step
  if (any(at)) .walk_skips$issue[at][[1L]] else NA_character_
}

# a value in a failure's message
.walk_show <- function(v) paste(deparse(v, width.cutoff = 200L), collapse = " ")

# ---- the code SPEC's cards ---------------------------------------------------------

# the functions the generated cards draw, and the arguments each draws
.walk_fns <- c(.plan_form_fns, names(.special_covered))
.walk_args <- function(fn, defs = spec_card_defs(fn)) {
  if (fn %in% names(.special_covered)) .special_args(fn) else setdiff(defs$formals, .data_args)
}
# the columns the cards offer (the sample study's)
.walk_ctx <- list(choices = c("row_label", ".values", "AGE", "SEX", "RACE", "TRT01A"))

# One value through a card: drawn, its inputs read back as the page sends
# them.  The card is drawn with the argument's field and its main fields
# (spec_card(only = ): the fields as arg_field() draws them on the whole
# card; the main ones tell the read that the card is on the page) -- the
# whole card's HTML takes a second to render.  A form of its own reads its
# fields below as the builder does (.special_read(), .special_args_put());
# the others as the generated card.
.walk_card_trip <- function(fn, a, v, defs) {
  args <- stats::setNames(list(v), a)
  it <- list(fn = fn, args = args, own_args = args)
  special <- fn %in% names(.special_covered)
  only <- if (special) a else unique(c(a, defs$visible))
  h <- htmltools::doRenderTags(spec_card("c", fn, it, ctx = .walk_ctx, defs = defs, only = only))
  h <- as.character(h)
  inp <- sent_inputs(h)
  got <- if (special) {
    .special_args_put(it$args, .special_read(inp, "c", fn))
  } else {
    spec_card_args(spec_card_read(inp, "c", fn, defs = defs), it, fn, defs = defs)
  }
  list(html = h, args = args, got = got)
}

# The save step, for every function at once: one report of the sample
# study a round, each function's item given its arguments' k-th values (an
# argument with fewer values: none in that round), written as the builder
# writes an item (.spec_set_items()); the study saved and opened again.
# Kept for the file's tests (one save: about 10 s).  Each entry: the
# function, the argument, the value written, and the report's value after
# the reopen (its own over its layers', as the card shows it).
.walk_saved <- local({
  got <- NULL
  function() {
    if (!is.null(got)) return(got)
    home <- local_home()
    s <- suppressMessages(sample_study_copy(home))
    p <- s$planner
    base <- p$spec$outputs[["T-14-1-1"]]
    out <- list()
    for (k in 1:3) {
      id <- sprintf("T-WALK-%d", k)
      o <- base
      o$id <- id
      p <- .spec_add_output(p, o, label = id)
      its <- builder_items(p, id)
      items <- list()
      for (fn in .walk_fns) {
        defs <- spec_card_defs(fn)
        at <- Filter(function(x) identical(x$fn, fn), its)
        args <- if (length(at)) at[[1L]]$args else list()
        for (a in .walk_args(fn, defs)) {
          sm <- tryCatch(field_samples(defs$args[[a]], fn, a), error = function(e) list())
          if (length(sm) < k) next
          args[a] <- list(sm[[k]])
          out[[length(out) + 1L]] <- list(id = id, fn = fn, arg = a, value = sm[[k]])
        }
        items[[length(items) + 1L]] <- list(fn = fn, args = args)
      }
      p <- .spec_set_items(p, id, items)
    }
    s$planner <- p
    s <- suppressWarnings(suppressMessages(save_study(s, home = home)))
    s2 <- suppressWarnings(suppressMessages(open_study(basename(s$path), home = home)))
    its <- lapply(stats::setNames(nm = unique(vapply(out, function(e) e$id, ""))),
                  function(id) builder_items(s2$planner, id))
    for (i in seq_along(out)) {
      e <- out[[i]]
      at <- Filter(function(x) identical(x$fn, e$fn), its[[e$id]])
      now <- if (length(at)) at[[1L]]$args else list()
      v <- now[[e$arg]]
      # (a map's entries over its layers': the layers' other entries kept,
      # tflspec's merge -- the report's own entries compared)
      if (!is.null(v) && !is.null(names(e$value)) && !is.null(names(v)) &&
            !inherits(e$value, c("tfl_call", "tfl_r")) && !inherits(v, c("tfl_call", "tfl_r", "tfl_unset"))) {
        v <- v[intersect(names(v), names(e$value))]
      }
      out[[i]]$saved <- if (is.null(v)) list() else stats::setNames(list(v), e$arg)
    }
    got <<- out
    got
  }
})

for (fn in .walk_fns) local({
  fn <- fn
  defs <- spec_card_defs(fn)
  args <- .walk_args(fn, defs)
  test_that(sprintf("%s(): every field draws, reads back, saves and reopens", fn), {
    skip_on_cran()
    expect_gt(length(args), 0L)
    for (a in args) {
      sm <- field_samples(defs$args[[a]], fn, a)
      expect_gt(length(sm), 0L)
      for (v in sm) {
        what <- sprintf("%s(%s = %s)", fn, a, .walk_show(v))
        rt <- .walk_card_trip(fn, a, v, defs)
        # (the field is of the kind its values were made for)
        cls <- field_class(field_kind(defs$args[[a]]))
        expect_match(rt$html, sprintf('<div class="rp-field rp-field-%s[ "][^>]*data-arg="%s"', cls, a),
                     info = paste(what, "is not drawn as a field of kind", cls))
        expect_true(field_shows(rt$html, paste0("c__", a), v), info = paste(what, "is not drawn"))
        if (is.na(.walk_skip(fn, a, "read"))) {
          expect_identical(value_key(rt$got[a]), value_key(rt$args), info = paste(what, "reads back otherwise"))
          expect_identical(args_to_r(fn, rt$got), args_to_r(fn, rt$args), info = what)
        }
      }
    }
    for (e in Filter(function(e) identical(e$fn, fn), .walk_saved())) {
      if (!is.na(.walk_skip(fn, e$arg, "save"))) next
      expect_identical(value_key(e$saved), value_key(stats::setNames(list(e$value), e$arg)),
                       info = sprintf("%s(%s = %s) in %s: saved and reopened otherwise", fn, e$arg,
                                      .walk_show(e$value), e$id))
    }
  })
})

# ---- the template cards -------------------------------------------------------------

# the templates and the parameters their cards draw
.walk_tpls <- local({
  tt <- tflspec::tfl_templates()
  stats::setNames(tt$kind, tt$name)
})

# The draw and read steps, for every template at once (one server: the
# template cards' fields are drawn by tpl_widget(), app_server()'s): each
# parameter drawn with each value, its input as the drawn field sends it
# read back as the card reads it (.tpl_value()) and set (.tpl_set_arg()).
# Each entry: the template, the parameter, the value, its type, whether it
# is drawn, and the args read back.
.walk_tpl_drawn <- local({
  got <- NULL
  function() {
    if (!is.null(got)) return(got)
    home <- local_home()
    s <- suppressMessages(sample_study_copy(home))
    # (the entries kept in an environment: the server's code runs in its own)
    acc <- new.env()
    acc$out <- list()
    suppressWarnings(suppressMessages(shiny::testServer(
      function(input, output, session) app_server(input, output, session, s), {
        # (tpl_widget() is a function of the server's environment)
        env <- Filter(function(e) is.environment(e) && exists("tpl_widget", envir = e),
                      lapply(session$userData, function(x) if (is.function(x)) environment(x) else x))
        tpl_widget <- get("tpl_widget", envir = env[[1L]])
        for (n in names(.walk_tpls)) {
          prm <- .tpl_params(n)
          for (x in .tpl_shown(prm)) {
            type <- .tpl_type(x)
            sm <- tryCatch(field_samples(x, n, x$name, kind = paste0("tpl:", type)),
                           error = function(e) list(e))
            for (v in sm) {
              if (inherits(v, "error")) {
                acc$out[[length(acc$out) + 1L]] <- list(tpl = n, arg = x$name, error = conditionMessage(v))
                next
              }
              fid <- paste0("tpl_", .tpl_id_part(n), "__", x$name)
              h <- as.character(htmltools::doRenderTags(
                shiny::isolate(tpl_widget(fid, x, v, params = prm, tpl = n, name = n))))
              none <- grepl("none-note none-on", h, fixed = TRUE) ||
                identical(shown_value(h, fid), .none_word) || identical(sent_value(h, fid), tr(.none_word))
              raw <- if (identical(type, "flag") && none) .none_word else
                if (identical(type, "columns")) .tpl_list_text(v, x) else sent_value(h, fid)
              val <- tryCatch(.tpl_value(raw, type, x), error = function(e) e)
              back <- if (inherits(val, "error")) val else .tpl_set_arg(list(), x, val)
              acc$out[[length(acc$out) + 1L]] <- list(
                tpl = n, arg = x$name, type = type, value = v,
                drawn = if (identical(type, "columns")) TRUE else field_shows(h, fid, v, none = none),
                back = back)
            }
          }
        }
      })))
    got <<- acc$out
    got
  }
})

# The save step of a template report: its `template:` with the args of
# every parameter's k-th value written as the app writes a new report's
# (.tpl_write_args(), by step), the report's file written and read as the
# save and the reopen do (tflspec::tfl_write_output(), the text
# save_study() writes; tflspec::tfl_read_output()), its args read as the
# card reads them (.tpl_args_of())
.walk_tpl_saved <- function(n, entries) {
  dir <- withr::local_tempdir()
  out <- list()
  for (k in 1:3) {
    args <- list()
    for (x in .tpl_shown(.tpl_params(n))) {
      vs <- Filter(function(e) identical(e$arg, x$name) && is.null(e$error), entries)
      if (length(vs) < k) next
      args <- .tpl_set_arg(args, x, vs[[k]]$value)
      out[[length(out) + 1L]] <- list(arg = x$name, value = vs[[k]]$value, k = k)
    }
    o <- tflspec::tfl_output(sprintf("T-WALK-%d", k), .walk_tpls[[n]],
                             template = .tpl_write_args(list(name = n), args, by_step = TRUE))
    f <- file.path(dir, sprintf("T-WALK-%d.yml", k))
    tflspec::tfl_write_output(o, f)
    back <- .tpl_args_of(tflspec::tfl_read_output(f)$template)
    for (i in seq_along(out)) {
      if (out[[i]]$k == k) out[[i]]$saved <- back[out[[i]]$arg]
    }
  }
  out
}

for (n in names(.walk_tpls)) local({
  n <- n
  test_that(sprintf("template %s: every parameter draws, reads back, saves and reopens", n), {
    skip_on_cran()
    es <- Filter(function(e) identical(e$tpl, n), .walk_tpl_drawn())
    expect_gt(length(es), 0L)
    for (e in es) {
      if (!is.null(e$error)) {
        fail(e$error)
        next
      }
      what <- sprintf("%s(%s = %s)", n, e$arg, .walk_show(e$value))
      if (is.na(.walk_skip(n, e$arg, "draw"))) {
        expect_true(isTRUE(e$drawn), info = paste(what, "is not drawn"))
      }
      if (is.na(.walk_skip(n, e$arg, "read"))) {
        expect_identical(value_key(e$back), value_key(stats::setNames(list(e$value), e$arg)),
                         info = paste(what, "reads back otherwise"))
      }
    }
    for (e in .walk_tpl_saved(n, es)) {
      if (!is.na(.walk_skip(n, e$arg, "save"))) next
      expect_identical(value_key(e$saved), value_key(stats::setNames(list(e$value), e$arg)),
                       info = sprintf("%s(%s = %s): saved and reopened otherwise", n, e$arg,
                                      .walk_show(e$value)))
    }
  })
})

# ---- the cases skipped, each with its issue ---------------------------------------------

for (i in seq_len(nrow(.walk_skips))) local({
  sk <- .walk_skips[i, ]
  test_that(sprintf("%s(%s = ): %s (skipped)", sk$what, sk$arg, sk$step), {
    skip(sk$issue)
  })
})

test_that("the walk's value maker knows every kind of field, and fails on one it does not", {
  expect_error(field_samples(list(type = "x"), "f", "a", kind = "a_kind_not_known"),
               "f(a = ): a field of kind 'a_kind_not_known'", fixed = TRUE)
  # (none of a value is its argument's default)
  for (fn in .walk_fns) {
    defs <- spec_card_defs(fn)
    for (a in .walk_args(fn, defs)) {
      d <- defs$args[[a]]$default
      if (!is.null(d)) {
        expect_false(any(vapply(field_samples(defs$args[[a]], fn, a), identical, NA, d)),
                     info = paste(fn, a))
      }
    }
  }
})
