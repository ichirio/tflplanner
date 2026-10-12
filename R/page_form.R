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
$(document).on('shiny:value', function(e) {
  if (e.name !== 'page_form' || !window.pgFocus) return;
  setTimeout(function() {
    var el = $('.pg-in').filter(function() { return $(this).data('key') === window.pgFocus; });
    if (el.length) el[0].focus();
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
.pg-info { grid-template-columns: repeat(auto-fill, minmax(15rem, 1fr)); }
.pg-info .pg-from { line-height: 1.25; }
.pg-info input[readonly] { background: var(--bs-secondary-bg, #e9ecef); }
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

.page_tokens_ui <- function(x, id, pattern, t) {
  d <- page_tokens_from(x, id)
  d <- d[!d$name %in% c(.pg_report_tokens, .pg_builtin_names()), , drop = FALSE]
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

# A report's information changed (NA: back to its default): the label,
# title and analysis set text are its rows of the tokens sheet; the section
# is the report list's (a row of the tokens sheet the TOC wrote before goes,
# so there is one)
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
.page_said <- function(x, id) {
  txt <- unlist(lapply(c("header", "footer", "titles", "footnotes"), function(sh) {
    d <- page_lines_from(x, sh, id)
    d <- d[!d$omitted, , drop = FALSE]
    unlist(d[c("left", "center", "right")])
  }))
  txt <- txt[!is.na(txt)]
  m <- unlist(regmatches(txt, gregexpr("[{][A-Z][A-Z0-9_]*[}]", txt)))
  unique(substr(m, 2L, nchar(m) - 1L))
}

# The report's information -- the TOC's, edited here, one value each: its
# label, title and analysis set text (the report's rows of the tokens sheet)
# and its section (the report list's).  Each field says what a blank gives
# (its placeholder); where a value came from is the TOC import's to say.
.pg_info_items <- c("OUTPUT_LABEL", "OUTPUT_TITLE", "OUTPUT_POPULATION", "OUTPUT_SECTION")

.page_report_info_ui <- function(x, id, t) {
  o <- x$outputs
  i <- match(id, o$output_id)
  pop <- if (!is.null(o$population)) o$population[i] else NA_character_
  sec <- if (!is.null(o$section)) o$section[i] else NA_character_
  rows <- page_tokens_from(x, id)
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
    own_row <- !is.na(r) && identical(rows$from[r], "own")
    # the value: the report's row; the section, the report list's (a row the
    # TOC wrote before 0.0.2.9190 wins, as it prints)
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
    field("OUTPUT_TITLE", t("Title")),
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
# report's, the study's (its tokens sheet), rtfreporter's
.page_insert_ui <- function(x, id, t) {
  builtin <- .pg_builtin_names()
  study <- setdiff(page_tokens_from(x, id)$name, c(.pg_report_tokens, builtin))
  toks <- unique(c(.pg_report_tokens, study, builtin))
  shiny::div(
    class = "small mt-1 d-flex flex-wrap align-items-center gap-1",
    shiny::span(class = "text-muted", t("Insert (into the field last clicked):")),
    shiny::tags$select(
      class = "pg-ins-sel form-select form-select-sm", style = "max-width: 16rem",
      shiny::tags$option(value = "", t("(a token)")),
      lapply(toks, function(k) shiny::tags$option(value = paste0("{", k, "}"), paste0("{", k, "}")))))
}

# The Titles card's note: the title the header prints, edited in the card
# above (the sample's and the company standards' headers print it)
.page_heading_ui <- function(x, id, t) {
  if (is.na(id) || .is_pattern_id(id)) return(NULL)
  if (!"OUTPUT_TITLE" %in% .page_said(x, id)) return(NULL)
  tk <- page_tokens_from(x, id)
  v <- tk$value[match("OUTPUT_TITLE", tk$name)]
  shiny::div(class = "pg-from mb-2",
             sprintf(t("The header prints {OUTPUT_TITLE}: %s (changed in \"Report information and tokens\", above)."),
                     if (is.na(v) || !nzchar(v)) t("(empty)") else v))
}

# the whole form of a report's page
.page_form_ui <- function(x, id, t) {
  pattern <- report_pattern(x, id)
  report <- !is.na(id) && !.is_pattern_id(id)
  card <- function(title, ...) shiny::div(class = "pg-card", shiny::h6(title), ...)
  shiny::div(
    `data-pg-target` = .pg_target(id),
    # every token the page can say, in one place: the report's information
    # (the TOC's, edited here), its own tokens, rtfreporter's
    if (report) card(t("Report information and tokens"),
      shiny::div(class = "pg-from mb-1", t("The report's information is the TOC's, edited here (the report list shows the same). Each is a {TOKEN} a header, footer, title or footnote can say.")),
      .page_report_info_ui(x, id, t),
      shiny::div(class = "small fw-semibold mt-2", t("This report's own tokens")),
      .page_tokens_ui(x, id, pattern, t),
      .page_builtins_ui(t))
    else card(t("Tokens"), .page_tokens_ui(x, id, pattern, t), .page_builtins_ui(t)),
    card(t("Paper, margins and font"),
         shiny::div(class = "pg-grid", lapply(.page_fields, .page_field_ui, x = x, id = id,
                                              pattern = pattern, t = t))),
    card(t("Header"), .page_lines_ui(x, id, "header", pattern, t)),
    card(t("Titles"), .page_heading_ui(x, id, t),
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
