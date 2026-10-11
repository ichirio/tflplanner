# Step 3's form (the page-patterns design, part A): the page of the report
# chosen, each value with where it comes from -- this report, its pattern,
# Standard -- the paper, margins and font, the header and footer lines, the
# tokens.  It writes only the report's own rows; the sheets' grids stay
# below, folded ("Details: edit the sheets").  Inputs and buttons carry
# what they edit in data- attributes and say it with one input each
# (pg_edit, pg_act), so the form is drawn again as it is, not observed field
# by field.

.page_fields <- list(
  list(sheet = "page", col = "paper_size", label = "Paper", kind = "choice",
       choices = c("letter", "legal", "A4", "A3", "A5"), default = "letter"),
  list(sheet = "page", col = "orientation", label = "Orientation", kind = "choice",
       choices = c("landscape", "portrait"), default = "landscape"),
  # (the defaults are rtfreporter's: rtf_page(), Courier 9 pt)
  list(sheet = "page", col = "margin_top_in", label = "Top (in)", kind = "number", default = "0.75"),
  list(sheet = "page", col = "margin_bottom_in", label = "Bottom (in)", kind = "number", default = "0.75"),
  list(sheet = "page", col = "margin_left_in", label = "Left (in)", kind = "number", default = "0.75"),
  list(sheet = "page", col = "margin_right_in", label = "Right (in)", kind = "number", default = "0.75"),
  list(sheet = "page", col = "font", label = "Font", kind = "text", default = "Courier"),
  list(sheet = "page", col = "font_size_half_points", label = "Size (pt)", kind = "pt", default = "9"))

# where a value comes from, as the form says it
.page_from_label <- function(from, pattern, t) {
  switch(from %||% "",
         own = t("this report"),
         pattern = sprintf(t("pattern %s"), pattern),
         standard = t("Standard"),
         "")
}

.page_form_js <- "
// the form's fields: a change (on leaving the field, or Enter) says what it
// edits; a button what it does.  The field in focus is found again when the
// form is drawn again (its markers change with what is typed).
$(document).on('change', '.pg-in', function() {
  var d = $(this).data();
  Shiny.setInputValue('pg_edit', {kind: d.kind, sheet: d.sheet || '', col: d.col || '',
    line: String(d.line || ''), part: d.part || '', name: d.name || '',
    value: $(this).val(), n: Math.random()}, {priority: 'event'});
});
$(document).on('click', '.pg-act', function() {
  var d = $(this).data();
  Shiny.setInputValue('pg_act', {act: d.act, sheet: d.sheet || '', col: d.col || '',
    line: String(d.line || ''), name: d.name || '', n: Math.random()}, {priority: 'event'});
});
$(document).on('focusin', '.pg-in', function() { window.pgFocus = $(this).data('key'); });
$(document).on('shiny:value', function(e) {
  if (e.name !== 'page_form' || !window.pgFocus) return;
  setTimeout(function() {
    var el = $('.pg-in').filter(function() { return $(this).data('key') === window.pgFocus; });
    if (el.length) el[0].focus();
  }, 0);
});
"

.page_form_css <- "
.pg-card { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .375rem;
  padding: .5rem .75rem; margin-bottom: .75rem; }
.pg-card h6 { font-size: .85rem; font-weight: 600; margin-bottom: .4rem; }
.pg-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(10.5rem, 1fr)); gap: .4rem .75rem; }
.pg-field label { font-size: .75rem; margin-bottom: .1rem; display: block; }
.pg-from { font-size: .7rem; color: var(--bs-secondary-color, #6b7280); }
.pg-own { font-size: .7rem; color: var(--bs-primary, #0d6efd); }
.pg-act.btn-link { font-size: .7rem; padding: 0 .2rem; }
.pg-lines td { padding: .15rem .3rem; vertical-align: middle; font-size: .8rem; }
.pg-lines .pg-inh { color: var(--bs-secondary-color, #6b7280); }
.pg-lines .pg-omit { color: var(--bs-secondary-color, #6b7280); font-style: italic; }
"

# one field of a one-row sheet: its own value in the input, the value it
# inherits as the placeholder, and where the value comes from
.page_field_ui <- function(x, id, f, pattern, t) {
  v <- page_value_from(x, f$sheet, f$col, id)
  ih <- .inherited_from(x, f$sheet, f$col, id)
  inh <- ih$value
  own <- identical(v$from, "own")
  shown <- function(val) if (identical(f$kind, "pt")) .points(val) else val
  key <- paste(f$sheet, f$col, sep = ":")
  attrs <- list(class = "pg-in form-control form-control-sm", `data-kind` = "cell",
                `data-sheet` = f$sheet, `data-col` = f$col, `data-key` = key)
  inh_txt <- if (!is.na(inh)) shown(inh) else ""
  input <- if (identical(f$kind, "choice")) {
    opts <- c(list(shiny::tags$option(value = "", if (nzchar(inh_txt))
      sprintf(t("(%s: %s)"), .page_from_label(ih$from, pattern, t), inh_txt) else
        if (!is.null(f$default)) sprintf(t("(default: %s)"), f$default) else "")),
      lapply(f$choices, function(ch) shiny::tags$option(
        value = ch, selected = if (own && identical(tolower(v$value), tolower(ch))) NA, ch)))
    do.call(shiny::tags$select, c(attrs, list(class = "pg-in form-select form-select-sm"), opts))
  } else {
    do.call(shiny::tags$input, c(attrs, list(
      type = "text", value = if (own) shown(v$value) else "",
      placeholder = if (nzchar(inh_txt)) inh_txt else
        if (!is.null(f$default)) sprintf(t("(default: %s)"), f$default) else "")))
  }
  shiny::div(
    class = "pg-field",
    shiny::tags$label(t(f$label)),
    input,
    if (own) shiny::div(
      shiny::span(class = "pg-own", "\u25cf ", t("this report")),
      shiny::tags$button(type = "button", class = "pg-act btn btn-link",
                         `data-act` = "reset", `data-sheet` = f$sheet, `data-col` = f$col,
                         if (is.na(pattern)) t("Back to Standard") else t("Back to the pattern")))
    else if (!is.na(v$from)) shiny::div(class = "pg-from", .page_from_label(v$from, pattern, t)))
}

# a band's lines: the report's own editable, the others read and acted on
.page_lines_ui <- function(x, id, sheet, pattern, t) {
  d <- page_lines_from(x, sheet, id)
  blank_line <- function(r) all(is.na(r[c("left", "center", "right")]) |
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
      `data-kind` = "line", `data-sheet` = sheet, `data-line` = r$line, `data-part` = part,
      `data-key` = paste(sheet, r$line, part, sep = ":"))
    else shiny::span(v)
  }
  btn <- function(act, line, label) shiny::tags$button(
    type = "button", class = "pg-act btn btn-link", `data-act` = act,
    `data-sheet` = sheet, `data-line` = line, label)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, , drop = FALSE]
    own <- identical(r$from, "own")
    if (own && r$omitted) {
      return(shiny::tags$tr(
        shiny::tags$td(r$line),
        shiny::tags$td(colspan = 3, class = "pg-omit", t("(left out in this report)")),
        shiny::tags$td(shiny::span(class = "pg-own", "\u25cf ", t("this report"))),
        shiny::tags$td(btn("drop", r$line, t("Print it again")))))
    }
    shiny::tags$tr(
      class = if (!own) "pg-inh",
      shiny::tags$td(r$line),
      shiny::tags$td(cell(r, "left", own)), shiny::tags$td(cell(r, "center", own)),
      shiny::tags$td(cell(r, "right", own)),
      shiny::tags$td(if (own) shiny::span(class = "pg-own", "\u25cf ", t("this report"))
                     else shiny::span(class = "pg-from", .page_from_label(r$from, pattern, t))),
      shiny::tags$td(class = "text-nowrap",
        if (own) btn("drop", r$line, if (is.na(pattern)) t("Back to Standard") else t("Back to the pattern"))
        else shiny::tagList(btn("own", r$line, t("Change here")),
                            btn("omit", r$line, t("Leave out here")))))
  })
  shiny::tagList(
    if (nrow(d)) shiny::tags$table(
      class = "table table-sm pg-lines mb-1",
      shiny::tags$thead(shiny::tags$tr(lapply(c("#", t("Left"), t("Center"), t("Right"), "", ""),
                                              shiny::tags$th))),
      shiny::tags$tbody(rows))
    else shiny::p(class = "small text-muted mb-1", t("(no lines)")),
    shiny::tags$button(type = "button", class = "pg-act btn btn-sm btn-outline-secondary py-0",
                       `data-act` = "addline", `data-sheet` = sheet, t("Add a line for this report")))
}

.page_tokens_ui <- function(x, id, pattern, t) {
  d <- page_tokens_from(x, id)
  if (!nrow(d)) return(shiny::p(class = "small text-muted mb-1", t("(no tokens)")))
  btn <- function(act, name, label) shiny::tags$button(
    type = "button", class = "pg-act btn btn-link", `data-act` = act, `data-name` = name, label)
  shiny::tags$table(
    class = "table table-sm pg-lines mb-1",
    shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, , drop = FALSE]
      own <- identical(r$from, "own")
      v <- if (is.na(r$value)) "" else r$value
      shiny::tags$tr(
        class = if (!own) "pg-inh",
        shiny::tags$td(shiny::code(paste0("{", r$name, "}"))),
        shiny::tags$td(if (own) shiny::tags$input(
          type = "text", class = "pg-in form-control form-control-sm", value = v,
          `data-kind` = "token", `data-name` = r$name, `data-key` = paste("token", r$name, sep = ":"))
          else shiny::span(v)),
        shiny::tags$td(if (own) shiny::span(class = "pg-own", "\u25cf ", t("this report"))
                       else shiny::span(class = "pg-from", .page_from_label(r$from, pattern, t))),
        shiny::tags$td(class = "text-nowrap",
          if (own) btn("token_reset", r$name, if (is.na(pattern)) t("Back to Standard") else t("Back to the pattern"))
          else btn("token_own", r$name, t("Change here"))))
    })))
}

# the whole form of a report's page
.page_form_ui <- function(x, id, t) {
  pattern <- report_pattern(x, id)
  card <- function(title, ...) shiny::div(class = "pg-card", shiny::h6(title), ...)
  shiny::tagList(
    card(t("Paper, margins and font"),
         shiny::div(class = "pg-grid", lapply(.page_fields, .page_field_ui, x = x, id = id,
                                              pattern = pattern, t = t))),
    card(t("Header"), .page_lines_ui(x, id, "header", pattern, t)),
    card(t("Footer"), .page_lines_ui(x, id, "footer", pattern, t)),
    card(t("Tokens"), .page_tokens_ui(x, id, pattern, t)))
}

# A line number for a report's new line: after the last line its page has
# (a run line at 99 kept last: before it)
.page_new_line <- function(x, sheet, id) {
  n <- suppressWarnings(as.integer(page_lines_from(x, sheet, id)$line))
  n <- n[!is.na(n)]
  below <- n[n < 90L]
  as.character(if (length(below)) max(below) + 1L else if (length(n)) min(n) - 1L else 1L)
}
