# The R switch of a card's field (R/field_ui.R, .field_frame()): its two
# sides -- the field's own control and its R box -- are one value.  When
# the switch turns, the page moves the value across (.r_carry(): the
# field's data-carry; .builder_cards_js); the read (builder_card_read())
# reads the side the switch shows, the other only where that one is empty,
# so a value is never lost by the switch.  GUI review of #855 (must-fix 1):
# plan_cell_style(border = ) emptied and dropped from the code by a toggle.
# The border fields of plan_cell_style(), plan_style(), plan_titles() and
# plan_footnotes() -- a call<rtf_border> typed as its R -- in both
# directions, with the page's move and without it, then saved.

html <- function(...) as.character(htmltools::tagList(...))

# (sent_inputs() and shown_value(): helper-card-fields.R)

# (a border as the SPEC holds it, {fn: rtf_border, args: {bottom: single}})
.B <- tflspec::tfl_call("rtf_border", list(bottom = "single"))
.B_R <- 'rtf_border(bottom = "single")'
# the border fields: (function, argument)
.border_fields <- list(c("plan_cell_style", "border"), c("plan_style", "border_header"),
                       c("plan_style", "border_body"), c("plan_titles", "border"),
                       c("plan_footnotes", "border"))

# One turn of the switch: the card of `args` drawn, its inputs as the page
# sends them, the switch of `a` turned to `on`; `moved`: the page moved the
# value across as .r_carry() says (into the R box, the control emptied; or
# back) -- FALSE: it did not (the read alone keeps it).  `typed`: the
# control's text after the turn.  The read (`read`) and the item's
# arguments after it (`args`: a turn with no edit is no change -- the
# stored form kept).
.turn <- function(fn, a, args, on, moved = TRUE, typed = NULL) {
  it <- list(fn = fn, args = args, own_args = args)
  h <- html(spec_card("c", fn, it))
  inp <- sent_inputs(h)
  fid <- paste0("c__", a)
  if (!is.null(on)) {
    if (moved && on) {
      inp[[paste0(fid, "__r")]] <- shown_value(h, fid)
      inp[[fid]] <- ""
    }
    if (moved && !on) {
      inp[[fid]] <- shown_value(h, paste0(fid, "__r"))
      inp[[paste0(fid, "__r")]] <- NULL
    }
    inp[[paste0(fid, "__isr")]] <- on
  }
  if (!is.null(typed)) inp[[fid]] <- typed
  r <- spec_card_read(inp, "c", fn)
  list(html = h, read = r, args = spec_card_args(r, it, fn))
}

test_that("a border field's switch carries the value across: the field says how (data-carry)", {
  skip_on_cran()
  for (fa in .border_fields) {
    h <- html(spec_card("c", fa[[1L]], list(fn = fa[[1L]], args = stats::setNames(list(.B), fa[[2L]]))))
    fld <- regmatches(h, regexpr(sprintf('<div class="rp-field[^"]*" data-arg="%s"[^>]*>', fa[[2L]]), h))
    expect_length(fld, 1L)
    expect_match(fld, 'data-carry="rtext"', fixed = TRUE, info = paste(fa, collapse = " "))
    expect_match(fld, 'data-r-fns="rtf_border"', fixed = TRUE, info = paste(fa, collapse = " "))
    # (off: the call's R text in the field, the R box empty)
    expect_identical(shown_value(h, paste0("c__", fa[[2L]])), .B_R, info = paste(fa, collapse = " "))
    expect_false(shown_value(h, paste0("c__", fa[[2L]], "__isr")))
  }
  # (a select's field: a value moved as its literal; plan_style(border = )
  # an enum or a call)
  h <- html(spec_card("c", "plan_style", list(fn = "plan_style", args = list(border = "none"))))
  expect_match(regmatches(h, regexpr('<div class="rp-field[^"]*" data-arg="border"[^>]*>', h)),
               'data-carry="one"', fixed = TRUE)
  # (the switch says what its states are, and the notes it may give)
  expect_match(h, "R switch. On: the value is R code", fixed = TRUE)
  expect_match(h, 'data-note-kept="This R cannot be shown in the field: it stays R."', fixed = TRUE)
  hj <- html(spec_card("c", "plan_cell_style", list(fn = "plan_cell_style", args = list()), lang = "ja"))
  expect_false(grepl("R switch. On", hj, fixed = TRUE))
  # (the empty call field shows the call's shape)
  expect_match(hj, 'placeholder="rtf_border(...)"', fixed = TRUE)
})

test_that("off -> on keeps the border, moved by the page or not", {
  skip_on_cran()
  for (fa in .border_fields) {
    fn <- fa[[1L]]
    a <- fa[[2L]]
    args <- stats::setNames(list(.B), a)
    info <- paste(fn, a)
    # (the page moved it: the R box says the call -- read as R, the same
    # value, so the stored form is kept)
    t1 <- .turn(fn, a, args, on = TRUE)
    expect_identical(t1$read[[a]], tflspec::tfl_r(.B_R), info = info)
    expect_identical(t1$args, args, info = info)
    expect_match(args_to_r(fn, t1$args), paste0(a, " = ", .B_R), fixed = TRUE, info = info)
    # (it did not: the R box empty, the field's value read -- kept as it was)
    t2 <- .turn(fn, a, args, on = TRUE, moved = FALSE)
    expect_identical(args_to_r(fn, t2$args), args_to_r(fn, args), info = info)
    expect_match(args_to_r(fn, t2$args), paste0(a, " = ", .B_R), fixed = TRUE, info = info)
  }
})

test_that("on -> off keeps the border: a call goes back to the field as the call, other R stays R", {
  skip_on_cran()
  for (fa in .border_fields) {
    fn <- fa[[1L]]
    a <- fa[[2L]]
    info <- paste(fn, a)
    args <- stats::setNames(list(tflspec::tfl_r(.B_R)), a)
    h <- html(spec_card("c", fn, list(fn = fn, args = args)))
    expect_true(shown_value(h, paste0("c__", a, "__isr")), info = info)
    expect_identical(shown_value(h, paste0("c__", a, "__r")), .B_R, info = info)
    # (moved back: the field's call, as the spec holds a call; the same
    # value, so the stored form is kept)
    t1 <- .turn(fn, a, args, on = FALSE)
    expect_s3_class(t1$read[[a]], "tfl_call")
    expect_identical(t1$read[[a]]$fn, "rtf_border", info = info)
    expect_identical(t1$read[[a]]$args, list(bottom = "single"), info = info)
    expect_identical(t1$args, args, info = info)
    expect_match(args_to_r(fn, t1$args), paste0(a, " = ", .B_R), fixed = TRUE, info = info)
    # (not moved -- the field empty, the R in the box: kept)
    t2 <- .turn(fn, a, args, on = FALSE, moved = FALSE)
    expect_match(args_to_r(fn, t2$args), paste0(a, " = ", .B_R), fixed = TRUE, info = info)
    # (R that is not a call of the field's, my_border: stays R, never lost)
    t3 <- .turn(fn, a, stats::setNames(list(tflspec::tfl_r("my_border")), a), on = FALSE, moved = FALSE)
    expect_identical(t3$args[[a]], tflspec::tfl_r("my_border"), info = info)
  }
})

test_that("a border read and not edited keeps its stored form; a call typed in the field is the call (R stays off)", {
  skip_on_cran()
  for (fa in .border_fields) {
    fn <- fa[[1L]]
    a <- fa[[2L]]
    info <- paste(fn, a)
    args <- stats::setNames(list(.B), a)
    # (no edit: the same arguments, {fn: rtf_border, args: ...} not made R)
    t0 <- .turn(fn, a, args, on = NULL)
    expect_identical(t0$args, args, info = info)
    # (typed in the field with R off: the call, so the card drawn again
    # shows it in the field with the switch off)
    t1 <- .turn(fn, a, list(), on = NULL, typed = 'rtf_border(top = TRUE, bottom = "double")')
    expect_s3_class(t1$args[[a]], "tfl_call")
    expect_identical(t1$args[[a]]$args, list(top = TRUE, bottom = "double"), info = info)
    h <- html(spec_card("c", fn, list(fn = fn, args = t1$args)))
    expect_false(shown_value(h, paste0("c__", a, "__isr")), info = info)
    expect_identical(shown_value(h, paste0("c__", a)), 'rtf_border(top = TRUE, bottom = "double")', info = info)
    # (a side of its own look: rtf_border_line() inside, the call still)
    t2 <- .turn(fn, a, list(), on = NULL,
                typed = 'rtfreporter::rtf_border(bottom = rtf_border_line(style = "double", width = 30L))')
    expect_s3_class(t2$args[[a]], "tfl_call")
    expect_s3_class(t2$args[[a]]$args$bottom, "tfl_call")
    # (not a call of rtf_border(): R)
    t3 <- .turn(fn, a, list(), on = NULL, typed = "my_border")
    expect_s3_class(t3$args[[a]], "tfl_r")
  }
})

test_that("the R switch's other fields: on with the R box empty reads the field, off with the field empty the R", {
  skip_on_cran()
  sch <- list(args = list(s = list(type = "string|r"), n = list(type = "integer|r"),
                          k = list(type = "logical|r")))
  rd <- function(inp) builder_card_read(inp, "c", schema = sch, args = c("s", "n", "k"))
  # (on, its box empty: the field's value)
  r <- rd(list(c__s = "Table 1", c__s__isr = TRUE, c__n = 3L, c__n__isr = TRUE))
  expect_identical(r$s, "Table 1")
  expect_identical(r$n, 3L)
  # (a check is never empty: on with its box empty writes nothing, as before)
  expect_null(rd(list(c__k = FALSE, c__k__isr = TRUE))$k)
  # (on with R: the R wins over the field)
  r <- rd(list(c__s = "Old", c__s__isr = TRUE, c__s__r = "toupper(x)"))
  expect_identical(r$s, tflspec::tfl_r("toupper(x)"))
  # (off, the field empty, R left in the box: kept as R)
  r <- rd(list(c__s = "", c__s__isr = FALSE, c__s__r = "toupper(x)"))
  expect_identical(r$s, tflspec::tfl_r("toupper(x)"))
  # (off with a value in the field: the field)
  r <- rd(list(c__s = "New", c__s__isr = FALSE, c__s__r = "toupper(x)"))
  expect_identical(r$s, "New")
})

test_that("a saved border survives the switch: turned and saved, the SPEC and the program keep it", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  id <- "T-14-2-2"
  item_of <- function(p, fn) Filter(function(i) i$fn == fn, builder_items(p, id))[[1L]]
  write <- function(p, fn, args) .builder_item_write(p, id, fn, args, 1L)
  reopen <- function(s) {
    s <- suppressWarnings(suppressMessages(save_study(s, home = home)))
    suppressWarnings(suppressMessages(open_study(basename(s$path), home = home)))
  }
  # the borders saved as the SPEC holds a call
  s$planner <- write(s$planner, "plan_cell_style", list(cols = "row_label", border = .B))
  s$planner <- write(s$planner, "plan_style", list(border_header = .B))
  s <- reopen(s)
  # turned on: plan_cell_style() as the review did it with no move by the
  # page (the R box empty), plan_style() with the page's move
  p <- s$planner
  for (case in list(list(fn = "plan_cell_style", a = "border", moved = FALSE),
                    list(fn = "plan_style", a = "border_header", moved = TRUE))) {
    it <- item_of(p, case$fn)
    t <- .turn(case$fn, case$a, it$args, on = TRUE, moved = case$moved)
    p <- write(p, case$fn, t$args)
  }
  s$planner <- p
  s <- reopen(s)
  y <- paste(readLines(file.path(s$path, "spec", "outputs", paste0(id, ".yml")), warn = FALSE),
             collapse = "\n")
  expect_identical(lengths(regmatches(y, gregexpr("fn: rtf_border", y, fixed = TRUE))), 2L)
  expect_false(grepl("!r rtf_border", y, fixed = TRUE))
  for (fn in c("plan_cell_style", "plan_style")) {
    expect_s3_class(item_of(s$planner, fn)$args[[if (fn == "plan_style") "border_header" else "border"]],
                    "tfl_call")
  }
  code <- paste(program_code(s$planner, id), collapse = "\n")
  expect_match(code, "plan_cell_style(", fixed = TRUE)
  expect_match(code, 'border = rtf_border(bottom = "single")', fixed = TRUE)
  expect_match(code, 'border_header = rtf_border(bottom = "single")', fixed = TRUE)
  # turned off from R: the border back in its field, saved, still there
  s$planner <- write(s$planner, "plan_cell_style", list(cols = "row_label", border = tflspec::tfl_r(.B_R)))
  s <- reopen(s)
  it <- item_of(s$planner, "plan_cell_style")
  t <- .turn("plan_cell_style", "border", it$args, on = FALSE)
  s$planner <- write(s$planner, "plan_cell_style", t$args)
  s <- reopen(s)
  code <- paste(program_code(s$planner, id), collapse = "\n")
  expect_match(code, 'border = rtf_border(bottom = "single")', fixed = TRUE)
})

# The page's half of the rule (.r_carry_js, window.rpCarry) and the switch's
# handler (.builder_cards_js), run in headless Chrome: the pure functions,
# then the border field drawn by field_ui() turned both ways.  Only when
# asked (TFLPLANNER_E2E=true), with Chrome and chromote, as the other
# browser tests.
test_that("in the browser: the switch moves the border across and back, and keeps R it cannot show", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("TFLPLANNER_E2E"), "true"), "TFLPLANNER_E2E is not true")
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  jq <- system.file("www", "shared", "jquery.min.js", package = "shiny")
  skip_if_not(file.exists(jq), "no jQuery in shiny")
  field <- spec_card("c", "plan_cell_style", list(fn = "plan_cell_style", args = list(border = .B)),
                     only = "border")
  dir <- tempfile("rswitch")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  page <- file.path(dir, "page.html")
  writeLines(c("<!doctype html><html><head><meta charset='utf-8'>",
               sprintf("<script src='%s'></script>", paste0("file:///", normalizePath(jq, "/"))),
               "<script>window.Shiny = {addCustomMessageHandler: function() {}};</script>",
               "<script>", .builder_cards_js, "</script></head><body>",
               as.character(field), "</body></html>"), page, useBytes = TRUE)
  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file:///", normalizePath(page, "/")), wait_ = FALSE)
  b$wait_for(loaded)
  js <- function(code) {
    r <- b$Runtime$evaluate(code, returnByValue = TRUE)
    if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% "error")
    r$result$value
  }
  # the pure functions
  expect_identical(js("rpCarry.toR('text', 'Table \"1\"', false)"), '"Table \\"1\\""')
  expect_identical(js("rpCarry.fromR('text', '\"Table \\\\\"1\\\\\"\"', []).value"), 'Table "1"')
  expect_identical(js("rpCarry.toR('many', ['2', '4'], true)"), "c(2, 4)")
  expect_identical(js("rpCarry.toR('many', ['A', 'B'], false)"), 'c("A", "B")')
  expect_identical(unlist(js("rpCarry.fromR('many', 'c(\"A\", \\'B\\')', []).value")), c("A", "B"))
  expect_false(js("rpCarry.fromR('one', 'none', []).ok"))
  expect_identical(unlist(js("rpCarry.fromR('one', '\"none\"', []).value")), "none")
  expect_false(js("rpCarry.fromR('rtext', 'my_border', ['rtf_border']).ok"))
  expect_true(js("rpCarry.fromR('rtext', 'rtf_border(top = TRUE)', ['rtf_border']).ok"))
  expect_identical(js("rpCarry.toR('check', true, false)"), "TRUE")
  # (a string the page writes is R's string)
  txt <- js("rpCarry.toR('text', 'A \"q\" \\u00e9 \\\\ b', false)")
  expect_identical(eval(str2lang(txt)), "A \"q\" \u00e9 \\ b")
  # the border field turned on: the call in the R box, the field emptied
  turn <- "(function() { var s = document.getElementById('c__border__isr'); s.checked = !s.checked; $(s).trigger('change'); })()"
  st <- "JSON.stringify({f: $('#c__border').val(), r: $('#c__border__r').val(), on: $('#c__border__isr').prop('checked'), cls: $('.rp-field[data-arg=border]').hasClass('rp-field-r'), note: $('.rp-r-note').text()})"
  s0 <- jsonlite::fromJSON(js(st))
  expect_identical(s0$f, .B_R)
  expect_false(s0$on)
  js(turn)
  s1 <- jsonlite::fromJSON(js(st))
  expect_identical(s1$r, .B_R)
  expect_identical(s1$f, "")
  expect_true(s1$on)
  expect_true(s1$cls)
  # ... and off again: back in the field
  js(turn)
  s2 <- jsonlite::fromJSON(js(st))
  expect_identical(s2$f, .B_R)
  expect_identical(s2$r, "")
  expect_false(s2$on)
  # R the field cannot hold: the switch stays on, the R kept, a note says so
  js(turn)
  js("$('#c__border__r').val('my_border')")
  js(turn)
  s3 <- jsonlite::fromJSON(js(st))
  expect_true(s3$on)
  expect_true(s3$cls)
  expect_identical(s3$r, "my_border")
  expect_match(s3$note, "stays R", fixed = TRUE)
})
