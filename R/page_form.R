# Step 3's form: the page of the report chosen -- or, in the pattern
# dialog, of a pattern or Standard -- read from the YAML spec's `page:`
# layers (R/page_spec.R, #438), each value with where it comes from: this
# report, its pattern, the study's defaults.  It writes only the target's
# own layer, the key changed.  One card an rtfreporter function (its label
# "meaning (function())"), its arguments with a value first, the others
# below ("Not in the code"), in the function's order.  Inputs and buttons
# carry what they edit in data- attributes and say it with one input each
# (pg_edit, pg_act), so the form is drawn again as it is, not observed field
# by field.

# whose value the form edits, as its marker says it: the report's, the
# pattern's (in the pattern dialog), Standard's
.pg_own_word <- function(id, t, kind = NULL) {
  if (is.na(id)) t("Standard") else if (.is_pattern_id(id)) t("this pattern") else
    if (identical(kind, "figure")) t("This figure") else t("This table")
}
# what Revert goes back to, its tooltip
.pg_back_word <- function(id, t) {
  if (is.na(id)) t("Clear") else if (.is_pattern_id(id)) t("Back to Standard") else
    t("Back to the study default")
}
# the form's target in the browser: a report's id, @name, or "" (Standard)
.pg_target <- function(id) if (is.na(id)) "" else id
.pg_target_id <- function(v) if (is.null(v) || !nzchar(v)) NA_character_ else v

# where a value comes from, as the form says it: the layer's name
# (defaults, pattern:<name>, style:<name>); in a pattern's form the
# defaults are Standard
.page_from_label <- function(from, id, t) {
  kind <- sub(":.*$", "", from %||% "")
  name <- sub("^[^:]*:", "", from %||% "")
  switch(kind,
         own = .pg_own_word(id, t),
         defaults = if (!is.na(id) && .is_pattern_id(id)) t("Standard") else t("Study default"),
         pattern = sprintf(t("pattern %s"), name),
         style = sprintf(t("style %s"), name),
         "")
}

.pg_badge <- function(from, id, t, kind = NULL) {
  if (is.null(from) || is.na(from) || !nzchar(from)) return(NULL)
  own <- identical(from, "own")
  shiny::span(class = paste("pg-badge", if (own) "pg-badge-own"),
              if (own) .pg_own_word(id, t, kind) else .page_from_label(from, id, t))
}

.pg_btn <- function(act, label, title = NULL, ...) {
  shiny::tags$button(type = "button", class = "pg-act btn btn-link", `data-act` = act,
                     title = title, ..., label)
}

.page_form_js <- "
// the form's fields: a change (on leaving the field, or Enter) says what it
// edits; a button what it does.  The field in focus is found again when the
// form is drawn again (its markers change with what is typed).
$(document).on('change', '.pg-in', function() {
  var d = $(this).data();
  Shiny.setInputValue('pg_edit', {target: $(this).closest('[data-pg-target]').attr('data-pg-target'),
    kind: d.kind, fn: d.fn || '', arg: d.arg || '', band: d.band || '',
    line: String(d.line || ''), part: d.part || '', name: d.name || '',
    value: $(this).val(), n: Math.random()}, {priority: 'event'});
});
$(document).on('click', '.pg-act', function() {
  var d = $(this).data();
  Shiny.setInputValue('pg_act', {target: $(this).closest('[data-pg-target]').attr('data-pg-target'),
    act: d.act, fn: d.fn || '', arg: d.arg || '', band: d.band || '',
    line: String(d.line || ''), name: d.name || '', n: Math.random()}, {priority: 'event'});
});
$(document).on('click', '.pg-token-add', function() {
  var box = $(this).closest('.pg-add-token');
  Shiny.setInputValue('pg_act', {target: $(this).closest('[data-pg-target]').attr('data-pg-target'),
    act: 'token_add', fn: '', arg: '', band: '', line: '',
    name: box.find('.pg-new-name').val() || '', value: box.find('.pg-new-value').val() || '',
    n: Math.random()}, {priority: 'event'});
});
$(document).on('focusin', '.pg-in', function() {
  window.pgFocus = $(this).data('key');
  if (this.tagName === 'INPUT') {
    $('.pg-in.border-primary').removeClass('border-primary');
    window.pgLast = $(this).data('key'); $(this).addClass('border-primary');
  }
});
// the insert list: its token into the field last clicked, at the cursor
$(document).on('change', '.pg-ins-sel', function() {
  var tk = $(this).val();
  $(this).val('');
  var f = $('.pg-in').filter(function() { return $(this).data('key') === window.pgLast; })[0];
  if (!tk || !f) return;
  var a = f.selectionStart || f.value.length, b = f.selectionEnd || a;
  f.value = f.value.slice(0, a) + tk + f.value.slice(b);
  f.focus(); f.selectionStart = f.selectionEnd = a + tk.length;
  $(f).trigger('change');
});
// drawn again: the field in focus, and where the page was (no jump)
$(document).on('shiny:value', function(e) {
  if (e.name !== 'page_form' && e.name !== 'pat_form') return;
  var y = window.scrollY;
  setTimeout(function() {
    window.scrollTo(0, y);
    if (!window.pgFocus) return;
    var el = $('.pg-in').filter(function() { return $(this).data('key') === window.pgFocus; });
    if (el.length) el[0].focus({preventScroll: true});
  }, 0);
});
"

.page_form_css <- "
.pg-bar .form-group { margin-bottom: 0; }
.pg-bar select { height: calc(1.5em + .5rem + 2px); padding-top: .25rem; padding-bottom: .25rem;
  font-size: .875rem; }
.pg-card { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .375rem;
  padding: .5rem .75rem; margin-bottom: .75rem; }
.pg-card h6 { font-size: .85rem; font-weight: 600; margin-bottom: .4rem; }
.pg-card h6 .pg-fn, .pg-field .pg-fn { font-family: Consolas, 'Courier New', monospace;
  font-weight: normal; color: var(--bs-secondary-color, #6b7280); font-size: .9em; }
.pg-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(10.5rem, 1fr)); gap: .4rem .75rem; }
.pg-card { container-type: inline-size; }
.pg-grid.pg-grid-4 { grid-template-columns: repeat(4, minmax(0, 1fr)); gap: .4rem .5rem; margin-bottom: .4rem; }
@container (max-width: 300px) { .pg-grid.pg-grid-4 { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
.pg-field label { font-size: .75rem; margin-bottom: .1rem; display: block; }
.pg-from { font-size: .7rem; color: var(--bs-secondary-color, #6b7280); }
.pg-own { font-size: .7rem; color: var(--bs-primary, #0d6efd); }
.pg-badge { display: inline-block; font-size: 11px; line-height: 1.3; padding: 0 .4rem;
  border-radius: .6rem; background: var(--bs-tertiary-bg, #f3f4f6); color: #4b5563;
  border: 1px solid #e5e7eb; white-space: nowrap; }
.pg-badge-own { background: #ecfdf5; border-color: #a7f3d0; color: #047857; }
.pg-act.btn-link { font-size: .7rem; padding: 0 .2rem; }
.pg-lower > summary { font-size: .75rem; color: var(--bs-secondary-color, #6b7280); margin-top: .3rem; }
.pg-lower .pg-grid { margin-top: .3rem; }
.pg-lines td { padding: .15rem .3rem; vertical-align: middle; font-size: .8rem; }
.pg-band { table-layout: fixed; width: 100%; }
.pg-band td, .pg-band th { overflow-wrap: anywhere; }
.pg-band th { white-space: nowrap; font-size: .75rem; }
.pg-band tr.pg-line > td { border-bottom-width: 0; }
.pg-band tr.pg-line-src > td { padding-top: 0; font-size: .7rem; }
.pg-band td.pg-num { color: var(--bs-secondary-color, #6b7280); }
.pg-badge-none { background: none; border-style: dashed; color: #6b7280; }
.pg-lines .pg-inh { color: var(--bs-secondary-color, #6b7280); }
.pg-lines .pg-omit { color: var(--bs-secondary-color, #6b7280); font-style: italic; }
.pg-lines td.pg-src { white-space: normal; max-width: 8rem; line-height: 1.2; }
.pg-tokens { width: 100%; }
.pg-tokens td.pg-val { width: 100%; }
.pg-tokens td.pg-val input { min-width: 12rem; }
.pg-info { grid-template-columns: repeat(auto-fill, minmax(15rem, 1fr)); }
.pg-info .pg-from { line-height: 1.25; }
.pg-info input[readonly] { background: var(--bs-secondary-bg, #e9ecef); }
"

# a field's label: its meaning, then (its argument) -- with the unit the
# code has when the field shows another (points as half points, inches as
# twips)
.pg_label <- function(text, arg, kind = "") {
  conv <- switch(kind, pt = " = pt x 2", inches = " = in x 1440", "")
  shiny::tags$label(text, " ", shiny::span(class = "pg-fn", paste0("(", arg, conv, ")")))
}

# rtfreporter's own default of an argument (what the code leaves to it), as
# the form shows it; "" when it has none to show
.pg_fn_default <- function(fn, arg, kind) {
  a <- tryCatch(formals(getExportedValue("rtfreporter", fn))[[arg]], error = function(e) NULL)
  if (is.null(a) || is.symbol(a) || !is.atomic(a) || length(a) != 1L) return("")
  .pgs_show(a, kind)
}

# One argument of an rtfreporter function: the target's own value in the
# input, what the layers below say as its placeholder, where it comes from;
# own: "This table" and Revert; a value below: "(none)" takes it away
# (!unset)
.page_arg_ui <- function(sp, id, fn, f, t, kind = NULL) {
  a <- .pgs_arg(sp, id, fn, f$arg)
  own <- identical(a$from, "own")
  below <- a$below
  inh <- if (!is.null(below$value)) .pgs_show(below$value, f$kind) else
    if (isTRUE(below$unset)) "(none)" else ""
  # (nothing in the spec: rtfreporter's own default)
  dflt <- if (is.null(a$value) && !own) .pg_fn_default(fn, f$arg, f$kind) else ""
  if (!nzchar(inh) && nzchar(dflt)) inh <- dflt
  val <- if (own && a$unset) "(none)" else if (own) .pgs_show(a$value, f$kind) else ""
  key <- paste(fn, f$arg, sep = ":")
  attrs <- list(class = "pg-in form-control form-control-sm", `data-kind` = "arg",
                `data-fn` = fn, `data-arg` = f$arg, `data-key` = key)
  input <- if (f$kind %in% c("choice", "logical")) {
    ch <- if (identical(f$kind, "logical")) c("TRUE", "FALSE") else f$choices
    if (own && a$unset) ch <- c(ch, "(none)")
    opts <- c(list(shiny::tags$option(value = "", if (nzchar(inh))
      sprintf(t("(%s: %s)"), if (!is.na(below$from)) .page_from_label(below$from, id, t) else
        t("rtfreporter's default"), inh) else "")),
      lapply(ch, function(x) shiny::tags$option(
        value = x, selected = if (own && identical(tolower(val), tolower(x))) NA, x)))
    do.call(shiny::tags$select, c(attrs, list(class = "pg-in form-select form-select-sm"), opts))
  } else {
    list_id <- if (!is.null(f$choices)) paste0("pg-list-", fn, "-", f$arg)
    shiny::tagList(
      do.call(shiny::tags$input, c(attrs, list(
        type = "text", value = val, list = list_id,
        placeholder = if (nzchar(inh)) inh else ""))),
      if (!is.null(list_id)) shiny::tags$datalist(
        id = list_id, lapply(f$choices, function(x) shiny::tags$option(value = x))))
  }
  shiny::div(
    class = "pg-field",
    .pg_label(t(f$label), f$arg, f$kind),
    input,
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-1 mt-1",
      .pg_badge(a$from, id, t, kind),
      if (is.na(a$from)) shiny::span(class = "pg-badge pg-badge-none", t("rtfreporter's default")),
      if (own) .pg_btn("reset", t("Revert"), .pg_back_word(id, t), `data-fn` = fn,
                       `data-arg` = f$arg),
      # (a value the layers below say: none here)
      if (!is.null(below$value) && !(own && a$unset))
        .pg_btn("none", "(none)", t("None here (!unset)"), `data-fn` = fn, `data-arg` = f$arg)))
}

# A function's card: the arguments with a value (written in its call)
# first, the others below, folded -- each group in the function's order
.page_fn_ui <- function(sp, id, fn, t, kind = NULL, fields = .pgs_fields[[fn]], title = NULL,
                        extra = NULL) {
  said <- vapply(fields, function(f) {
    a <- .pgs_arg(sp, id, fn, f$arg)
    !is.null(a$value) || identical(a$from, "own")
  }, NA)
  ui <- function(fs) shiny::div(class = "pg-grid", lapply(fs, function(f) .page_arg_ui(sp, id, fn, f, t, kind)))
  shiny::div(
    class = "pg-card", `data-fn` = fn,
    shiny::h6(t(title %||% .pgs_cards[[fn]]), " ",
              shiny::span(class = "pg-fn", .pgs_call_names[[fn]] %||% paste0("(", fn, "())"))),
    if (any(said)) ui(fields[said]),
    if (any(!said)) shiny::tags$details(
      class = "pg-lower",
      shiny::tags$summary(sprintf("%s (%d)", t("Not in the code"), sum(!said))),
      ui(fields[!said])),
    extra)
}

# The paper card (rtf_document(page =)): on screen in two rows -- paper,
# orientation, the font (rtf_document()) and its size
# (rtf_default_format()); then the four margins -- and the paper's other
# fields after them.  Only the screen's order: the YAML and the code keep
# theirs.  Written fields above (in this order), the others folded below,
# as every card, but for the two rows: always above.
.page_paper_rows <- list(
  list(c("rtf_page", "paper_size"), c("rtf_page", "orientation"),
       c("rtf_document", "font_table"), c("rtf_default_format", "font_size_half_points")),
  list(c("rtf_page", "margin_top_in"), c("rtf_page", "margin_bottom_in"),
       c("rtf_page", "margin_left_in"), c("rtf_page", "margin_right_in")))
.page_paper_moved <- list(rtf_document = "font_table", rtf_default_format = "font_size_half_points")

.page_paper_ui <- function(sp, id, t, kind = NULL) {
  fld <- function(fa) {
    f <- .pgs_field(fa[[1L]], fa[[2L]])
    if (is.null(f)) return(NULL)
    f$fn <- fa[[1L]]
    f
  }
  rows <- lapply(.page_paper_rows, function(r) Filter(Negate(is.null), lapply(r, fld)))
  placed <- unlist(lapply(.page_paper_rows, function(r) vapply(r, function(fa) fa[[2L]], "")))
  rest <- lapply(Filter(function(f) !f$arg %in% placed, .pgs_fields$rtf_page),
                 function(f) { f$fn <- "rtf_page"; f })
  rows <- c(rows, list(rest))
  # (a value from any layer -- the study standard's too -- is in the code)
  said <- function(f) {
    a <- .pgs_arg(sp, id, f$fn, f$arg)
    !is.null(a$value) || !is.null(a$below$value) || identical(a$from, "own")
  }
  grid <- function(fs, cls = "pg-grid") if (length(fs)) shiny::div(
    class = cls, lapply(fs, function(f) .page_arg_ui(sp, id, f$fn, f, t, kind)))
  # (the two rows always above -- the paper, the font, the margins are
  # what a page is; a field not written shows the value the page gets)
  up <- c(rows[1:2], list(Filter(said, rows[[3L]])))
  low <- Filter(Negate(said), rows[[3L]])
  shiny::div(
    class = "pg-card", `data-fn` = "rtf_page",
    shiny::h6(t(.pgs_cards[["rtf_page"]]), " ", shiny::span(class = "pg-fn", .pgs_call_names[["rtf_page"]])),
    lapply(up[1:2], grid, cls = "pg-grid pg-grid-4"),
    grid(up[[3L]]),
    if (length(low)) shiny::tags$details(
      class = "pg-lower",
      shiny::tags$summary(sprintf("%s (%d)", t("Not in the code"), length(low))),
      grid(low)))
}

# a band's lines: the target's own editable, the others read and acted on
.page_lines_ui <- function(sp, id, band, t, kind = NULL) {
  d <- .pgs_lines(sp, id, band)
  blank_line <- function(r) all(is.na(unlist(r[c("left", "center", "right")])) |
                                  !nzchar(trimws(unlist(r[c("left", "center", "right")]))))
  cell <- function(r, part, own) {
    v <- r[[part]]
    v <- if (is.na(v)) "" else v
    # a line with nothing in it is a blank line: spacing, said so
    if (!own && identical(part, "center") && blank_line(r)) {
      return(shiny::span(class = "text-muted fst-italic", t("(blank line)")))
    }
    if (own) shiny::tags$input(
      type = "text", class = "pg-in form-control form-control-sm", value = v,
      `data-kind` = "line", `data-band` = band, `data-line` = r$line, `data-part` = part,
      `data-key` = paste(band, r$line, part, sep = ":"))
    else shiny::span(v)
  }
  btn <- function(act, line, label, title = NULL) .pg_btn(act, label, title, `data-band` = band,
                                                          `data-line` = line)
  # each line: its cells (left : center : right = 2 : 1 : 1), then a small
  # row of where it comes from and what can be done; numbered in order (the
  # key, its place among the layers' lines, in the tooltip)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, , drop = FALSE]
    own <- identical(r$from, "own")
    num <- shiny::tags$td(class = "pg-num", title = paste0("'", r$line, "'"), i)
    acts <- if (own && r$omitted) btn("drop", r$line, t("Revert"), .pg_back_word(id, t)) else
      if (own) btn("drop", r$line, if (is.na(id)) t("Remove the line") else t("Revert"),
                   .pg_back_word(id, t))
      else shiny::tagList(btn("own", r$line, t("Change here")),
                          btn("omit", r$line, t("Leave out here"), "!unset"))
    main <- if (own && r$omitted) shiny::tags$tr(
      class = "pg-line", num,
      shiny::tags$td(colspan = 3, class = "pg-omit", t("(none: left out here)")))
    else shiny::tags$tr(
      class = paste("pg-line", if (!own) "pg-inh"), num,
      shiny::tags$td(cell(r, "left", own)), shiny::tags$td(cell(r, "center", own)),
      shiny::tags$td(cell(r, "right", own)))
    shiny::tagList(main, shiny::tags$tr(
      class = "pg-line-src", shiny::tags$td(),
      shiny::tags$td(colspan = 3, .pg_badge(r$from, id, t, kind), acts)))
  })
  shiny::tagList(
    if (nrow(d)) shiny::tags$table(
      class = "table table-sm pg-lines pg-band mb-1",
      shiny::tags$colgroup(shiny::tags$col(style = "width: 2rem"), shiny::tags$col(style = "width: 50%"),
                           shiny::tags$col(style = "width: 25%"), shiny::tags$col(style = "width: 25%")),
      shiny::tags$thead(shiny::tags$tr(lapply(c("#", t("Left"), t("Center"), t("Right")),
                                              shiny::tags$th))),
      shiny::tags$tbody(rows))
    else shiny::p(class = "small text-muted mb-1", t("(no lines)")),
    shiny::tags$button(type = "button", class = "pg-act btn btn-sm btn-outline-secondary py-0",
                       `data-act` = "addline", `data-band` = band,
                       if (is.na(id)) t("Add a line") else if (.is_pattern_id(id))
                         t("Add a line to this pattern") else t("Add a line for this report")))
}

# The tokens a page says but does not set: the report list's (its ID and
# label, its title, analysis set and section -- each changed where it comes
# from) and rtfreporter's.  A page's own token has another name.
.pg_report_tokens <- c("OUTPUT_ID", "OUTPUT_LABEL", "OUTPUT_TITLE",
                       "OUTPUT_POPULATION", "OUTPUT_SECTION")
.pg_token_taken <- function(name) name %in% c(.pg_report_tokens, .pg_builtin_names())
.pg_token_taken_msg <- function(name, t) {
  sprintf(if (name %in% .pg_builtin_names())
    t("{%s} is rtfreporter's, filled as the RTF is written: give your token another name.")
    else t("{%s} is the report list's: change it where it comes from (the list below says where), or give your token another name."),
    name)
}

.page_tokens_ui <- function(sp, id, t, kind = NULL) {
  d <- .pgs_tokens(sp, id)
  d <- d[!d$name %in% c(.pg_report_tokens, .pg_builtin_names()), , drop = FALSE]
  btn <- function(act, name, label, title = NULL) .pg_btn(act, label, title, `data-name` = name)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, , drop = FALSE]
    own <- identical(r$from, "own")
    below <- if (own) .pgs_find(.pgs_layers(sp, id)[-1L], c("tokens", r$name)) else NULL
    v <- if (r$omitted) "(none)" else if (is.na(r$value)) "" else r$value
    shiny::tags$tr(
      class = if (!own) "pg-inh",
      shiny::tags$td(shiny::code(paste0("{", r$name, "}"))),
      shiny::tags$td(class = "pg-val", if (own && !r$omitted) shiny::tags$input(
        type = "text", class = "pg-in form-control form-control-sm", value = v,
        `data-kind` = "token", `data-name` = r$name, `data-key` = paste("token", r$name, sep = ":"))
        else shiny::span(class = if (r$omitted) "pg-omit", v)),
      shiny::tags$td(class = "text-nowrap", .pg_badge(r$from, id, t, kind)),
      shiny::tags$td(class = "text-nowrap",
        if (own) btn("token_reset", r$name,
                     if (!is.na(below$from)) t("Revert") else t("Remove the token"),
                     .pg_back_word(id, t))
        else shiny::tagList(btn("token_own", r$name, t("Change here")),
                            btn("token_omit", r$name, t("Leave out here"), "!unset"))))
  })
  shiny::tagList(
    if (length(rows)) shiny::tags$table(class = "table table-sm pg-lines pg-tokens mb-1",
                                        shiny::tags$tbody(rows))
    else shiny::p(class = "small text-muted mb-1", t("(no tokens)")),
    # a token of one's own: its name (upper case) and its text
    shiny::div(
      class = "d-flex gap-1 align-items-center pg-add-token",
      shiny::tags$input(type = "text", class = "form-control form-control-sm pg-new-name",
                        placeholder = t("NAME"), style = "max-width: 10rem"),
      shiny::tags$input(type = "text", class = "form-control form-control-sm pg-new-value",
                        placeholder = t("its text")),
      shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary py-0 pg-token-add",
                         t("Add"))))
}

# A report's information changed (NA: back to its default), on the sheets
# (the TOC import): the label, title and analysis set text are its rows of
# the tokens sheet; the section is the report list's (a row of the tokens
# sheet the TOC wrote before goes, so there is one)
.set_report_info <- function(x, id, name, value) {
  if (!name %in% .pg_info_items) stop("Not a report's information: ", name, call. = FALSE)
  if (.is_blank(value)) value <- NA_character_
  if (identical(name, "OUTPUT_SECTION")) {
    i <- match(id, x$outputs$output_id)
    if (is.null(x$outputs$section)) x$outputs$section <- NA_character_
    x$outputs$section[i] <- value
    if ("OUTPUT_SECTION" %in% sheet_rows(x, "tokens", id)$name) {
      x <- set_page_token(x, id, "OUTPUT_SECTION", NA_character_)
    }
    return(x)
  }
  set_page_token(x, id, name, value)
}

# ... from step 3's form: the label, title and analysis set text are the
# report's page.tokens in its YAML; the section the report list's, as above
.set_report_info_spec <- function(x, id, name, value) {
  if (!name %in% .pg_info_items) stop("Not a report's information: ", name, call. = FALSE)
  if (identical(name, "OUTPUT_SECTION")) return(.set_report_info(x, id, name, value))
  .pgs_set_token(x, id, name, if (.is_blank(value)) NULL else value)
}

# rtfreporter's tokens, as it lists them (rtf_text_tokens()): the name, what
# it is, an example.  An older rtfreporter: the names alone.
.pg_builtins <- function() {
  d <- tryCatch(rtfreporter::rtf_text_tokens(), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) {
    return(data.frame(name = c("PAGE", "TOTAL_PAGES", "PROGRAM", "DATETIME"),
                      description = "", example = "", stringsAsFactors = FALSE))
  }
  d <- d[d$kind != "own", , drop = FALSE]
  data.frame(name = gsub("^[{]|[}]$", "", d$token), description = d$description,
             example = ifelse(is.na(d$example), "", d$example), stringsAsFactors = FALSE)
}
.pg_builtin_names <- function() .pg_builtins()$name

# the tokens a report's lines say (its header, footer, titles, footnotes)
.page_said <- function(sp, id) {
  txt <- unlist(lapply(c("header", "footer", "titles", "footnotes"), function(band) {
    d <- .pgs_lines(sp, id, band)
    d <- d[!d$omitted, , drop = FALSE]
    unlist(d[c("left", "center", "right")])
  }))
  txt <- txt[!is.na(txt)]
  m <- unlist(regmatches(txt, gregexpr("[{][A-Z][A-Z0-9_]*[}]", txt)))
  unique(substr(m, 2L, nchar(m) - 1L))
}

# The report's information -- the TOC's, edited here, one value each: its
# label, title and analysis set text (the report's page.tokens) and its
# section (the report list's).  Each field says what a blank gives (its
# placeholder); where a value came from is the TOC import's to say.
.pg_info_items <- c("OUTPUT_LABEL", "OUTPUT_TITLE", "OUTPUT_POPULATION", "OUTPUT_SECTION")

.page_report_info_ui <- function(x, sp, id, t) {
  o <- x$outputs
  i <- match(id, o$output_id)
  sec <- if (!is.null(o$section)) o$section[i] else NA_character_
  rows <- .pgs_tokens(sp, id)
  # the defaults: the tokens as the program has them, without the report's
  # own values (a line saying them all, for the copy only)
  x0 <- x
  tk <- .normalize_sheet(x0$sheets$tokens, "tokens")
  x0$sheets$tokens <- tk[!(!is.na(tk$output_id) & tk$output_id == id &
                             trimws(tk$name) %in% .pg_info_items), , drop = FALSE]
  if (!is.null(x0$outputs$section)) x0$outputs$section[i] <- NA_character_
  x0$sheets$titles <- rbind(
    .normalize_sheet(x0$sheets$titles, "titles"),
    .normalize_sheet(data.frame(output_id = id, line = "999",
                                center = paste0("{", .pg_info_items, "}", collapse = " ")), "titles"))
  dflt <- tryCatch(tflspec::tfl_report_tokens(
    .spec_object(.with_population_tokens(x0), c(table_sheets(), report_sheets()),
                 unique(c(.study_keys$table, .study_keys$report))), id),
    error = function(e) list())
  go <- function(where, label) shiny::tags$button(
    type = "button", class = "pg-act btn btn-link", `data-act` = "goto", `data-name` = where, label)
  field <- function(k, label) {
    r <- match(k, rows$name)
    own_row <- !is.na(r) && identical(rows$from[r], "own") && !rows$omitted[r]
    # the value: the report's own token; the section, the report list's (a
    # token the TOC wrote before 0.0.2.9196 wins, as it prints)
    v <- if (own_row) rows$value[r] else if (identical(k, "OUTPUT_SECTION") && !.is_blank(sec)) sec else
      NA_character_
    own <- !is.na(v)
    d <- dflt[[k]] %||% ""
    shiny::div(
      class = "pg-field",
      shiny::tags$label(shiny::code(paste0("{", k, "}")), " ", label),
      shiny::tags$input(
        type = "text", class = "pg-in form-control form-control-sm",
        value = if (own) v else "",
        placeholder = switch(k,
          OUTPUT_LABEL = sprintf(t("(blank: the text made from the ID, %s)"), d),
          OUTPUT_POPULATION = if (nzchar(d)) sprintf(t("(blank: step 1's analysis set, %s)"), d)
                              else t("(blank: step 1's analysis set, none chosen)"),
          if (nzchar(d)) d else ""),
        `data-kind` = "info", `data-name` = k, `data-key` = paste0("info:", k)),
      if (identical(k, "OUTPUT_POPULATION"))
        shiny::div(class = "pg-from", go("ard", t("The analysis set: step 1"))))
  }
  shiny::div(
    class = "pg-grid pg-info",
    shiny::div(
      class = "pg-field",
      shiny::tags$label(shiny::code("{OUTPUT_ID}"), " ", t("ID")),
      shiny::tags$input(type = "text", class = "form-control form-control-sm", value = id,
                        readonly = NA),
      shiny::div(class = "pg-from", go("outputs", t("Rename it in the report list")))),
    field("OUTPUT_LABEL", t("Label")),
    field("OUTPUT_TITLE", t("Report title")),   # (t("Title") is the study's)
    field("OUTPUT_POPULATION", t("Analysis set text")),
    field("OUTPUT_SECTION", t("Section")))
}

# rtfreporter's tokens: read only, what each is (folded: ten rows to read)
.page_builtins_ui <- function(t) {
  d <- .pg_builtins()
  shiny::tags$details(
    class = "mt-2",
    shiny::tags$summary(class = "small fw-semibold",
                        sprintf(t("rtfreporter's tokens (%d), filled as the RTF is written"), nrow(d))),
    shiny::tags$table(class = "table table-sm pg-lines mb-1", shiny::tags$tbody(
      lapply(seq_len(nrow(d)), function(i) shiny::tags$tr(
        shiny::tags$td(shiny::code(paste0("{", d$name[i], "}"))),
        shiny::tags$td(class = "pg-inh", t(d$description[i])),
        shiny::tags$td(class = "pg-inh", d$example[i]))))))
}

# the tokens a title or footnote may say, as one list to insert from: the
# report's, the study's, rtfreporter's
.page_insert_ui <- function(sp, id, t) {
  builtin <- .pg_builtin_names()
  study <- setdiff(.pgs_tokens(sp, id)$name, c(.pg_report_tokens, builtin))
  toks <- unique(c(.pg_report_tokens, study, builtin))
  shiny::div(
    class = "small mt-1 d-flex flex-wrap align-items-center gap-1",
    shiny::tags$select(
      class = "pg-ins-sel form-select form-select-sm", style = "max-width: 16rem",
      title = t("Insert (into the field last clicked):"),
      shiny::tags$option(value = "", t("(a token)")),
      lapply(toks, function(k) shiny::tags$option(value = paste0("{", k, "}"), paste0("{", k, "}")))),
    help_tip(t("Insert (into the field last clicked):")))
}

# The Titles card's note: the title the header prints, edited in the card
# above (the sample's and the company standards' headers print it)
.page_heading_ui <- function(sp, id, t) {
  if (is.na(id) || .is_pattern_id(id)) return(NULL)
  if (!"OUTPUT_TITLE" %in% .page_said(sp, id)) return(NULL)
  tk <- .pgs_tokens(sp, id)
  v <- tk$value[match("OUTPUT_TITLE", tk$name)]
  help_tip(sprintf(t("The header prints {OUTPUT_TITLE}: %s (changed in \"Report information and tokens\", above)."),
                   if (is.na(v) || !nzchar(v)) t("(empty)") else v))
}

# the whole form of a target's page (a report, @pattern, NA: Standard)
.page_form_ui <- function(x, id, t) {
  sp <- .pgs_spec(x)
  if (is.null(sp)) return(shiny::p(class = "small text-muted", t("(no YAML spec)")))
  report <- !is.na(id) && !.is_pattern_id(id)
  if (report && is.null(sp$outputs[[id]])) return(NULL)
  kind <- if (report) sp$outputs[[id]]$kind %||% "table"
  card <- function(title, call = NULL, ...) shiny::div(
    class = "pg-card",
    shiny::h6(title, if (!is.null(call)) shiny::tagList(" ", shiny::span(class = "pg-fn", call))), ...)
  fig <- lapply(c("width_twips", "height_twips"), function(a) .pgs_arg(sp, id, "rtf_figures", a))
  fig_said <- vapply(fig, function(a) tolower(as.character(a$value %||% "")), "")
  fig_own <- vapply(fig, function(a) identical(a$from, "own"), NA)
  size <- function(fn) shiny::div(class = "pg-grid mb-1", .page_arg_ui(sp, id, fn, .pgs_fields[[fn]][[1L]],
                                                                       t, kind))
  shiny::div(
    `data-pg-target` = .pg_target(id),
    # every token the page can say, in one place: the report's information
    # (the TOC's, edited here), its own tokens, rtfreporter's
    if (report) card(t("Report information and tokens"), "(rtf_document(tokens =))",
      .page_report_info_ui(x, sp, id, t),
      shiny::div(class = "small fw-semibold mt-2", t("This report's own tokens")),
      .page_tokens_ui(sp, id, t, kind),
      .page_builtins_ui(t))
    else card(t("Tokens"), "(rtf_document(tokens =))", .page_tokens_ui(sp, id, t, kind), .page_builtins_ui(t)),
    .page_paper_ui(sp, id, t, kind),
    .page_fn_ui(sp, id, "rtf_default_format", t, kind,
                fields = Filter(function(f) !f$arg %in% .page_paper_moved$rtf_default_format,
                                .pgs_fields$rtf_default_format)),
    .page_fn_ui(sp, id, "rtf_document", t, kind,
                fields = Filter(function(f) !f$arg %in% .page_paper_moved$rtf_document,
                                .pgs_fields$rtf_document)),
    # the content's call: rtf_tables() for a table or a listing, rtf_figures()
    # for a figure (a pattern's or Standard's: both)
    if (!identical(kind, "figure")) .page_fn_ui(sp, id, "rtf_tables", t, kind),
    if (!report || identical(kind, "figure")) .page_fn_ui(
      sp, id, "rtf_figures", t, kind,
      extra = shiny::div(class = "d-flex flex-wrap gap-2 align-items-center",
        if (report) shiny::span(class = "pg-from", sprintf(t("(its own: %s)"),
          paste(format(.fig_native_size(x, id)), collapse = " x "))),
        if (!all(fig_said == "fit")) .pg_btn("fig_fit", t("Fit to the page")),
        if (any(fig_own)) .pg_btn("fig_native", t("Its own size")),
        help_tip(t("Blank: the figure's own size (the company's figure style for its kind; a figure of your own code, 6.5 x 4.5 in), as rtfreporter places an image. A number: that size in inches. fit: the page's body (its width; its height less the titles and footnotes). The figure is drawn at this size, its text as its theme says; the dpi (step 2) changes only its pixels.")))),
    card(t("Header"), "(rtf_section(header =))", .page_lines_ui(sp, id, "header", t, kind)),
    card(shiny::tagList(t("Titles"), " ", .page_heading_ui(sp, id, t)), "(rtf_titles())",
         size("rtf_titles"), .page_lines_ui(sp, id, "titles", t, kind), .page_insert_ui(sp, id, t)),
    card(t("Footnotes"), "(rtf_footnotes())", size("rtf_footnotes"),
         .page_lines_ui(sp, id, "footnotes", t, kind), .page_insert_ui(sp, id, t)),
    card(t("Footer"), "(rtf_section(footer =))", .page_lines_ui(sp, id, "footer", t, kind)))
}

# A line number for a new line of a band: after the last line the page has
# (a run line at 99 kept last: before it).  `x` a planner (its sheets) or,
# with `sp`, the spec.
.page_new_line <- function(x, sheet, id, sp = NULL) {
  lines <- if (!is.null(sp)) .pgs_lines(sp, id, sheet)$line else page_lines_from(x, sheet, id)$line
  n <- suppressWarnings(as.integer(lines))
  n <- n[!is.na(n)]
  below <- n[n < 90L]
  as.character(if (length(below)) max(below) + 1L else if (length(n)) min(n) - 1L else 1L)
}
