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
  list(sheet = "page", col = "font", label = "Font", kind = "text", default = "Courier",
       choices = c("Courier", "Courier New")),
  list(sheet = "page", col = "font_size_half_points", label = "Size (pt)", kind = "pt", default = "9"))

# whose value the form edits, as its marker and its button say it: the
# report's, the pattern's (in the pattern dialog), Standard's
.pg_own_word <- function(id, t) {
  if (is.na(id)) t("Standard") else if (.is_pattern_id(id)) t("this pattern") else t("this report")
}
.pg_back_word <- function(id, pattern, t) {
  if (is.na(id)) t("Clear") else if (.is_pattern_id(id) || is.na(pattern)) t("Back to Standard") else
    t("Back to the pattern")
}
# the form's target in the browser: a report's id, @name, or "" (Standard)
.pg_target <- function(id) if (is.na(id)) "" else id
.pg_target_id <- function(v) if (is.null(v) || !nzchar(v)) NA_character_ else v

# where a value comes from, as the form says it
.page_from_label <- function(from, pattern, t) {
  switch(from %||% "",
         own = t("this report"),
         pattern = sprintf(t("pattern %s"), pattern),
         # (a report of a pattern: the pattern says the same as Standard)
         standard = if (is.na(pattern %||% NA)) t("Standard") else
           sprintf(t("Standard (= %s)"), pattern),
         "")
}

.page_form_js <- "
// the form's fields: a change (on leaving the field, or Enter) says what it
// edits; a button what it does.  The field in focus is found again when the
// form is drawn again (its markers change with what is typed).
$(document).on('change', '.pg-in', function() {
  var d = $(this).data();
  Shiny.setInputValue('pg_edit', {target: $(this).closest('[data-pg-target]').attr('data-pg-target'),
    kind: d.kind, sheet: d.sheet || '', col: d.col || '',
    line: String(d.line || ''), part: d.part || '', name: d.name || '',
    value: $(this).val(), n: Math.random()}, {priority: 'event'});
});
$(document).on('click', '.pg-act', function() {
  var d = $(this).data();
  Shiny.setInputValue('pg_act', {target: $(this).closest('[data-pg-target]').attr('data-pg-target'),
    act: d.act, sheet: d.sheet || '', col: d.col || '',
    line: String(d.line || ''), name: d.name || '', n: Math.random()}, {priority: 'event'});
});
$(document).on('click', '.pg-token-add', function() {
  var box = $(this).closest('.pg-add-token');
  Shiny.setInputValue('pg_act', {target: $(this).closest('[data-pg-target]').attr('data-pg-target'),
    act: 'token_add', sheet: '', col: '', line: '',
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
// an insert button: its token into the field last clicked, at the cursor
$(document).on('mousedown', '.pg-ins', function(e) { e.preventDefault(); });
$(document).on('click', '.pg-ins', function() {
  var tk = $(this).data('token');
  var f = $('.pg-in').filter(function() { return $(this).data('key') === window.pgLast; })[0];
  if (!f) return;
  var a = f.selectionStart || f.value.length, b = f.selectionEnd || a;
  f.value = f.value.slice(0, a) + tk + f.value.slice(b);
  f.focus(); f.selectionStart = f.selectionEnd = a + tk.length;
  $(f).trigger('change');
});
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
.pg-lines td.pg-src { white-space: normal; max-width: 8rem; line-height: 1.2; }
.pg-tokens { width: 100%; }
.pg-tokens td.pg-val { width: 100%; }
.pg-tokens td.pg-val input { min-width: 12rem; }
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
    list_id <- if (!is.null(f$choices)) paste0("pg-list-", f$col)
    shiny::tagList(
      do.call(shiny::tags$input, c(attrs, list(
        type = "text", value = if (own) shown(v$value) else "",
        list = list_id,
        placeholder = if (nzchar(inh_txt)) inh_txt else
          if (!is.null(f$default)) sprintf(t("(default: %s)"), f$default) else ""))),
      if (!is.null(list_id)) shiny::tags$datalist(
        id = list_id, lapply(f$choices, function(ch) shiny::tags$option(value = ch))))
  }
  shiny::div(
    class = "pg-field",
    shiny::tags$label(t(f$label)),
    input,
    if (own) shiny::div(
      shiny::span(class = "pg-own", "\u25cf ", .pg_own_word(id, t)),
      shiny::tags$button(type = "button", class = "pg-act btn btn-link",
                         `data-act` = "reset", `data-sheet` = f$sheet, `data-col` = f$col,
                         .pg_back_word(id, pattern, t)))
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
        shiny::tags$td(class = "text-nowrap", shiny::span(class = "pg-own", "\u25cf ", .pg_own_word(id, t))),
        shiny::tags$td(btn("drop", r$line, t("Print it again")))))
    }
    shiny::tags$tr(
      class = if (!own) "pg-inh",
      shiny::tags$td(r$line),
      shiny::tags$td(cell(r, "left", own)), shiny::tags$td(cell(r, "center", own)),
      shiny::tags$td(cell(r, "right", own)),
      shiny::tags$td(class = if (own) "text-nowrap" else "pg-src",
                     if (own) shiny::span(class = "pg-own", "\u25cf ", .pg_own_word(id, t))
                     else shiny::span(class = "pg-from", .page_from_label(r$from, pattern, t))),
      shiny::tags$td(class = "text-nowrap",
        if (own) btn("drop", r$line, if (is.na(id)) t("Remove the line") else .pg_back_word(id, pattern, t))
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
                       `data-act` = "addline", `data-sheet` = sheet,
                       if (is.na(id)) t("Add a line") else if (.is_pattern_id(id))
                         t("Add a line to this pattern") else t("Add a line for this report")))
}

.page_tokens_ui <- function(x, id, pattern, t, skip = character()) {
  d <- page_tokens_from(x, id)
  d <- d[!d$name %in% skip, , drop = FALSE]
  # the names a level below has: "back" there, else the token goes
  below <- if (!is.na(id)) {
    lv <- .page_levels(x, id)[-1L]
    tk <- .normalize_sheet(x$sheets$tokens, "tokens")
    unique(trimws(unlist(lapply(lv, function(l) .rows_of(tk, l)$name))))
  } else character()
  btn <- function(act, name, label) shiny::tags$button(
    type = "button", class = "pg-act btn btn-link", `data-act` = act, `data-name` = name, label)
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, , drop = FALSE]
    own <- identical(r$from, "own")
    v <- if (is.na(r$value)) "" else r$value
    shiny::tags$tr(
      class = if (!own) "pg-inh",
      shiny::tags$td(shiny::code(paste0("{", r$name, "}"))),
      shiny::tags$td(class = "pg-val", if (own) shiny::tags$input(
        type = "text", class = "pg-in form-control form-control-sm", value = v,
        `data-kind` = "token", `data-name` = r$name, `data-key` = paste("token", r$name, sep = ":"))
        else shiny::span(v)),
      shiny::tags$td(class = "text-nowrap",
                     if (own) shiny::span(class = "pg-own", "\u25cf ", .pg_own_word(id, t))
                     else shiny::span(class = "pg-from", .page_from_label(r$from, pattern, t))),
      shiny::tags$td(class = "text-nowrap",
        if (own) btn("token_reset", r$name,
                     if (r$name %in% below) .pg_back_word(id, pattern, t) else t("Remove the token"))
        else btn("token_own", r$name, t("Change here"))))
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

# The tokens a header, footer, title or footnote of this report can say,
# with what each holds now -- where it comes from, to read (the ones to set
# are on the left)
.page_token_ref_ui <- function(x, id, t) {
  tok <- tryCatch(tflspec::tfl_report_tokens(
    .spec_object_last(x, c(table_sheets(), report_sheets()),
                      unique(c(.study_keys$table, .study_keys$report))), id),
    error = function(e) character())
  tok[["OUTPUT_ID"]] <- id
  val <- function(k) if (k %in% names(tok) && nzchar(tok[[k]])) tok[[k]] else t("(empty)")
  group <- function(title, keys, note = NULL) shiny::tagList(
    shiny::div(class = "small fw-semibold mt-1", title),
    if (!is.null(note)) shiny::div(class = "pg-from", note),
    shiny::tags$table(class = "table table-sm pg-lines mb-1", shiny::tags$tbody(
      lapply(keys, function(k) shiny::tags$tr(
        shiny::tags$td(shiny::code(paste0("{", k, "}"))),
        shiny::tags$td(class = "pg-inh", if (is.null(names(keys))) val(k) else names(keys)[keys == k]))))))
  report_own <- c("OUTPUT_TITLE", "OUTPUT_POPULATION", "OUTPUT_SECTION")
  study <- setdiff(names(tok), c(report_own, "OUTPUT_ID", "OUTPUT_LABEL", "STUDY_ID"))
  shiny::tagList(
    group(t("Made from the report's ID"), c("OUTPUT_ID", "OUTPUT_LABEL")),
    group(t("The report's, from the TOC taken in (or typed on the left)"), report_own),
    if (length(study)) group(t("The study's (its tokens sheet, Standard)"), study),
    group(t("rtfreporter's, as the RTF is written"),
          stats::setNames(c("PAGE", "TOTAL_PAGES", "PROGRAM", "DATETIME"),
                          c(t("the page"), t("the pages"), t("the program's path"),
                            t("the date and time")))))
}

# the tokens a title or footnote may say, as insert buttons: the report's,
# the study's (its tokens sheet), rtfreporter's
.page_insert_ui <- function(x, id, t) {
  study <- setdiff(page_tokens_from(x, id)$name, c("OUTPUT_TITLE", "OUTPUT_POPULATION",
                                                     "OUTPUT_LABEL"))
  toks <- c("OUTPUT_LABEL", "OUTPUT_TITLE", "OUTPUT_POPULATION", study,
            "PAGE", "TOTAL_PAGES", "PROGRAM", "DATETIME")
  shiny::div(
    class = "small mt-1",
    shiny::span(class = "text-muted me-1", t("Insert (into the field last clicked):")),
    lapply(unique(toks), function(k) shiny::tags$button(
      type = "button", class = "pg-ins btn btn-sm btn-outline-secondary py-0 me-1 mb-1",
      `data-token` = paste0("{", k, "}"), paste0("{", k, "}"))))
}

# The heading: the report's {OUTPUT_TITLE}, when its header prints one (the
# sample's and the company standards' do: their titles are the header's)
.page_heading_ui <- function(x, id, t) {
  # (a report's own token: not a pattern's or Standard's)
  if (is.na(id) || .is_pattern_id(id)) return(NULL)
  hdr <- page_lines_from(x, "header", id)
  hdr <- hdr[!hdr$omitted, , drop = FALSE]
  uses <- any(grepl("{OUTPUT_TITLE}", unlist(hdr[c("left", "center", "right")]), fixed = TRUE))
  if (!uses) return(NULL)
  tk <- page_tokens_from(x, id)
  k <- match("OUTPUT_TITLE", tk$name)
  own <- !is.na(k) && identical(tk$from[k], "own")
  shiny::div(
    class = "pg-field mb-2",
    shiny::tags$label(with_tip(t("The header's {OUTPUT_TITLE}"),
      t("The text the header prints where it says {OUTPUT_TITLE}: this report's title. A TOC taken in writes it; it is not a title line of its own (the lines below are)."))),
    shiny::tags$input(
      type = "text", class = "pg-in form-control form-control-sm",
      value = if (own) tk$value[k] else "",
      placeholder = if (!is.na(k)) tk$value[k] %||% "" else "",
      `data-kind` = "token", `data-name` = "OUTPUT_TITLE", `data-key` = "token:OUTPUT_TITLE"))
}

# the whole form of a report's page
.page_form_ui <- function(x, id, t) {
  pattern <- report_pattern(x, id)
  # (the heading edits {OUTPUT_TITLE}: not listed again with the tokens)
  heading <- .page_heading_ui(x, id, t)
  card <- function(title, ...) shiny::div(class = "pg-card", shiny::h6(title), ...)
  shiny::div(
    `data-pg-target` = .pg_target(id),
    card(t("Paper, margins and font"),
         shiny::div(class = "pg-grid", lapply(.page_fields, .page_field_ui, x = x, id = id,
                                              pattern = pattern, t = t))),
    card(t("Tokens"),
         bslib::layout_columns(
           col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
           shiny::div(
             shiny::div(class = "pg-from mb-1", t("This page's tokens: change, add, remove.")),
             .page_tokens_ui(x, id, pattern, t, skip = if (!is.null(heading)) "OUTPUT_TITLE")),
           if (!is.na(id) && !.is_pattern_id(id)) shiny::div(
             shiny::div(class = "pg-from mb-1", t("What a header, footer, title or footnote can say:")),
             .page_token_ref_ui(x, id, t)))),
    card(t("Header"), .page_lines_ui(x, id, "header", pattern, t)),
    card(t("Titles"), heading,
         .page_lines_ui(x, id, "titles", pattern, t), .page_insert_ui(x, id, t)),
    card(t("Footnotes"), .page_lines_ui(x, id, "footnotes", pattern, t),
         .page_insert_ui(x, id, t)),
    card(t("Footer"), .page_lines_ui(x, id, "footer", pattern, t)))
}

# A line number for a report's new line: after the last line its page has
# (a run line at 99 kept last: before it)
.page_new_line <- function(x, sheet, id) {
  n <- suppressWarnings(as.integer(page_lines_from(x, sheet, id)$line))
  n <- n[!is.na(n)]
  below <- n[n < 90L]
  as.character(if (length(below)) max(below) + 1L else if (length(n)) min(n) - 1L else 1L)
}
