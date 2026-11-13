# The four-steps report screen: its layout (provisional).  One structure for
# a template report and a code-SPEC report -- a Common block on top, then four
# steps -- drawn from the model of R/foursteps_read.R.  Display only for now:
# the edit buttons are drawn and off until the tflspec readers land.
# (PAGE-PART 7.1, DATA-PART 5.4.)  A value in use is dark text whatever its
# badge; light text is only for "(not set)" (canon 1.39).

.fs_css <- "
.rp-fs-card { border: 1px solid #dee2e6; border-radius: .375rem; padding: .5rem .75rem; margin-bottom: .6rem; }
.rp-fs-card > .rp-fs-head { font-weight: 600; font-size: .9rem; margin-bottom: .3rem; }
.rp-fs-group { border-top: 1px solid #dee2e6; margin-top: .5rem; padding-top: .3rem; }
.rp-fs-group > .rp-fs-ghead { font-weight: 600; font-size: .85rem; margin-bottom: .2rem; }
.rp-fs-row { display: flex; gap: .5rem; align-items: baseline; padding: .2rem 0; border-bottom: 1px solid #f1f3f5; }
.rp-fs-row:last-child { border-bottom: none; }
.rp-fs-name { flex: 0 0 38%; font-size: .85rem; }
.rp-fs-kv { display: grid; grid-template-columns: 12rem minmax(0, 24rem) auto; column-gap: .6rem;
  align-items: baseline; justify-content: start; }
.rp-fs-kv > .rp-fs-name { flex: none; }
.rp-fs-row:not(.rp-fs-kv) > .rp-fs-val { flex: 0 1 auto; max-width: 30rem; }
.rp-fs-dtable { font-size: .8rem; margin-bottom: .3rem; }
.rp-fs-dtable th { font-weight: 600; color: #495057; white-space: nowrap; }
.rp-fs-dtable td { vertical-align: top; }
.rp-fs-code { font-size: .8rem; overflow-wrap: anywhere; }
.rp-fs-param { font-size: .68rem; color: #6c757d; font-family: var(--bs-font-monospace, monospace); }
.rp-fs-locked { font-family: var(--bs-font-monospace, monospace); font-size: .8rem; }
.rp-fs-grey { opacity: .5; }
.rp-fs-srcline { display: flex; flex-wrap: wrap; gap: .25rem; align-items: baseline; margin-bottom: .15rem; }
.rp-fs-pre { font-size: .78rem; background: #f8f9fa; border: 1px solid #e9ecef; border-radius: .25rem;
  padding: .5rem .7rem; white-space: pre-wrap; overflow-wrap: anywhere; }
.rp-fs-sticky { position: sticky; top: .5rem; }
.rp-fs-mini { font-size: .72rem; margin-bottom: .2rem; }
.rp-fs-mini th, .rp-fs-mini td { white-space: nowrap; padding: .1rem .35rem; }
.rp-fs-minibox { overflow-x: auto; }
.rp-fs-val { flex: 1 1 auto; min-width: 0; font-family: var(--bs-font-monospace, monospace); font-size: .82rem;
  overflow-wrap: anywhere; }
.rp-fs-val.rp-fs-hint { color: #8a8f98; }
.rp-fs-num { flex: 0 0 2.6rem; text-align: right; color: #6c757d; font-size: .8rem; }
.rp-fs-mark { font-size: .68rem; color: #6c757d; margin-left: .2rem; }
.rp-fs-src { flex: 0 0 auto; }
.rp-fs-removed .rp-fs-val { text-decoration: line-through; opacity: .5; }
.rp-fs-note { font-size: .78rem; color: #6c757d; }
.rp-fs-tools { display: flex; flex-wrap: wrap; gap: .3rem; margin-top: .4rem; }
.rp-fs-common { background: #f8f9fa; }
.rp-fs-sticky.rp-fs-stale .card-body { opacity: .5; }
#fs_update.rp-fs-stale::after { content: ' \\25cf'; font-size: .7em; vertical-align: .1em; }
.rp-fs-q { display: inline-block; width: 1.1rem; height: 1.1rem; line-height: 1rem; text-align: center;
  border: 1px solid #adb5bd; border-radius: 50%; font-size: .72rem; font-weight: 600; color: #495057;
  cursor: help; margin-left: .35rem; }
"

# The report is read only when Update is pressed -- by the button or Ctrl+Enter.
# A field changed meanwhile only marks the preview as out of date (client side,
# nothing is sent).  Ctrl+Enter first leaves the focused field (so a value
# typed is committed) and then presses the button.
.fs_js <- "
(function() {
  if (window.rpFsKeys) return;
  window.rpFsKeys = true;
  function stale(on) {
    $('#fs_update').toggleClass('rp-fs-stale', on);
    $('.rp-fs-sticky').toggleClass('rp-fs-stale', on);
  }
  $(document).on('input change', '#fs_screen .rp-field input, #fs_screen .rp-field select, #fs_screen .rp-field textarea', function() {
    // (typing in a select's search box changes no value)
    if (!$(this).closest('.selectize-input').length) stale(true);
  });
  $(document).on('click', '#fs_screen .rp-field .rp-reset', function() { stale(true); });
  $(document).on('click', '#fs_update', function() { stale(false); });
  // (capture phase, stopped: a select's own Enter would take the highlighted
  // option; only while the Update button is on screen; the focus stays)
  document.addEventListener('keydown', function(e) {
    if (!(e.ctrlKey || e.metaKey) || e.key !== 'Enter') return;
    if (!$('#fs_update').is(':visible')) return;
    e.preventDefault();
    e.stopImmediatePropagation();
    var a = document.activeElement;
    if (a && $(a).is('input:not(.selectize-input input), textarea')) $(a).trigger('change');
    setTimeout(function() { $('#fs_update').trigger('click'); }, 30);
  }, true);
})();
"

# a standing sentence behind a "?"
.fs_help <- function(text) {
  bslib::popover(shiny::tags$span(class = "rp-fs-q", tabindex = "0", role = "button", "?"),
                 text, placement = "auto")
}

# the badge that says where a value comes from
.fs_badge <- function(origin, t, config = NULL, kind = "table", text = NULL) {
  txt <- switch(origin,
    own = t(if (identical(kind, "table")) "This table" else "This report"),
    default = t("Template default"),
    fixed = t("Fixed by the template"),
    config = sprintf(t("Page configuration %s"), config %||% ""),
    study = t("Study"),
    template = t("Template default"),
    derived = t(text),
    NULL)
  if (is.null(txt)) return(NULL)
  cls <- switch(origin, own = "own", config = "study", study = "study", derived = "study", "default")
  shiny::span(class = paste0("badge rounded-pill rp-tpl-badge rp-tpl-badge-", cls), txt)
}

# the role of a step-1 name in words, for the denominator choice
.fs_den_label <- function(r, m, t) {
  phrase <- switch(r$role,
    "analysis set" = sprintf(t("Subjects of the analysis set (%s)"), m$common$population$value),
    denominator = t("Subjects tested"),
    sprintf(t("Records of %s"), r$input$value))
  paste0(r$id, " \u2014 ", phrase)
}

# a choice among the analysis data of step 1: "adsl_sub -- subjects of the analysis set (SAF)"
.fs_den_select <- function(value, m, t) {
  tab <- m$steps$data$table
  ids <- vapply(tab, function(r) r$id, "")
  lab <- vapply(tab, .fs_den_label, "", m = m, t = t)
  shiny::tags$select(
    class = "form-select form-select-sm", disabled = "disabled", style = "max-width: 22rem",
    lapply(seq_along(ids), function(i) shiny::tags$option(
      value = ids[[i]], selected = if (identical(ids[[i]], value)) "selected", lab[[i]])))
}

.fs_value_el <- function(r, m, t) {
  # a fixed value: plain locked text
  if (identical(r$origin, "fixed")) {
    return(shiny::span(class = "rp-fs-val", r$value))
  }
  if (identical(r$name, "denominator")) return(.fs_den_select(r$value, m, t))
  if (identical(r$name, "dataset")) {
    return(shiny::tags$input(type = "text", class = "form-control form-control-sm", disabled = "disabled",
                             style = "max-width: 22rem", value = r$value))
  }
  empty <- !nzchar(r$value)
  shiny::span(class = paste0("rp-fs-val", if (empty) " rp-fs-hint" else ""),
              if (empty) t("(not set)") else r$value)
}

.fs_name <- function(n, t) {
  lab <- if (n %in% names(.fs_labels)) .fs_labels[[n]]
  if (is.null(lab)) n else t(lab)
}

# one row of a step: the label ("meaning (name)"), the value, where it comes from
.fs_row_ui <- function(r, m, t) {
  shiny::div(
    class = "rp-fs-row rp-fs-kv",
    shiny::span(class = "rp-fs-name", .fs_name(r$name, t)),
    .fs_value_el(r, m, t),
    shiny::span(class = "rp-fs-src", .fs_badge(r$origin, t, r$config, m$kind)))
}

.fs_card <- function(title, ..., help = NULL) {
  shiny::div(class = "rp-fs-card",
             shiny::div(class = "rp-fs-head", title, if (!is.null(help)) .fs_help(help)),
             ...)
}

.fs_btn <- function(label) {
  shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary py-0",
                     disabled = "disabled", label)
}

# ------------------------------------------------------------ the common block

.fs_common_ui <- function(m, t) {
  cm <- m$common
  opt <- paste0(cm$population$options,
                ifelse(nzchar(cm$population$labels), paste0(" (", cm$population$labels, ")"), ""))
  line <- function(label, el, origin) shiny::div(
    class = "rp-fs-row rp-fs-kv",
    shiny::span(class = "rp-fs-name", t(label)), shiny::span(class = "rp-fs-val", el),
    shiny::span(class = "rp-fs-src", .fs_badge(origin, t, kind = m$kind)))
  sel <- shiny::tags$select(
    class = "form-select form-select-sm", disabled = "disabled", style = "max-width: 22rem",
    shiny::tags$option(value = "", t("(none)")),
    lapply(seq_along(opt), function(i) shiny::tags$option(
      value = cm$population$options[[i]], selected = if (identical(cm$population$options[[i]], cm$population$value)) "selected",
      opt[[i]])))
  txt <- function(v) if (nzchar(v)) v else shiny::span(class = "rp-fs-hint", t("(not set)"))
  shiny::div(
    class = "rp-fs-card rp-fs-common",
    shiny::div(class = "rp-fs-head", t("Common"), .fs_help(t("Read by more than one step"))),
    line("Analysis set (population)", sel, cm$population$origin),
    line("Treatment variable (treatment)", txt(cm$treatment$value), cm$treatment$origin),
    line("Analysis set label (population_label)", txt(cm$population_label$value), cm$population_label$origin))
}

# ------------------------------------------------------------------- step 1

# a value cell of the table: the text (locked when the template fixes it), the
# param it is written under
.fs_dcell <- function(c, t) {
  shiny::tagList(
    if (!nzchar(c$value)) shiny::span(class = "rp-fs-hint", t("None")) else
      if (identical(c$origin, "fixed")) shiny::span(class = "rp-fs-locked", c$value) else
        shiny::tags$code(class = "rp-fs-code", c$value),
    if (!is.null(c$param)) shiny::div(class = "rp-fs-param", c$param))
}

# the analysis set cell: "SAF: Safety Analysis Set (SAFFL == "Y")" for the
# first data, "its subjects" for the others (read-only: only adsl_sub reads
# the set)
.fs_set_cell <- function(set, t) {
  if (identical(set$type, "derived")) {
    return(shiny::span(class = "small", sprintf(t("Subjects of %s (%s)"), set$of, set$id)))
  }
  txt <- paste0(set$id, if (nzchar(set$label)) paste0(": ", set$label) else "",
                if (nzchar(set$cond)) paste0(" (", set$cond, ")") else "")
  shiny::tagList(shiny::span(class = "small", txt), shiny::div(class = "rp-fs-param", "population"))
}

# the Set by cell: one line (param, badge) for each value the report or the
# template sets; the analysis set is the Common block's
.fs_src_cell <- function(r, m, t) {
  ln <- function(param, badge) shiny::div(class = "rp-fs-srcline",
                                          if (!is.null(param)) shiny::span(class = "rp-fs-param", param), badge)
  lines <- list()
  if (identical(r$set$type, "common")) lines[[1L]] <- ln("population", .fs_badge("derived", t, text = "Common"))
  for (c in list(r$input, r$filter)) {
    if (!is.null(c$param)) lines[[length(lines) + 1L]] <- ln(c$param, .fs_badge(c$origin, t, kind = m$kind))
  }
  if (!length(lines)) lines[[1L]] <- ln(NULL, .fs_badge(if (identical(r$source, "own")) "own" else "study", t, kind = m$kind))
  lines
}

.fs_data_row <- function(r, m, t) {
  shiny::tags$tr(
    class = if (isTRUE(r$optional)) "rp-fs-grey",
    shiny::tags$td(
      shiny::tags$code(r$id),
      if (isTRUE(r$implicit)) .fs_help(t("Always made first, even when the report does not write it.")),
      if (!is.null(r$note)) shiny::div(class = "rp-fs-note", t(r$note))),
    shiny::tags$td(.fs_dcell(r$input, t)),
    shiny::tags$td(.fs_set_cell(r$set, t)),
    shiny::tags$td(.fs_dcell(r$filter, t)),
    shiny::tags$td(.fs_src_cell(r, m, t)))
}

# step 1: the analysis data of the report in one table, in build order
.fs_data_ui <- function(m, t) {
  d <- m$steps$data
  head <- shiny::tags$tr(lapply(c("Name", "Input data", "Analysis set", "Filter", "Set by"),
                                function(h) shiny::tags$th(t(h))))
  tbl <- shiny::div(class = "table-responsive", shiny::tags$table(
    class = "table table-sm rp-fs-dtable",
    shiny::tags$thead(head),
    shiny::tags$tbody(lapply(d$table, .fs_data_row, m = m, t = t))))
  .fs_card(
    if (is.null(m$template)) t("Analysis data") else sprintf(t("Template %s: analysis data"), m$template),
    tbl,
    if (is.null(m$template)) shiny::div(class = "rp-fs-tools", .fs_btn(t("+ Add analysis data"))),
    # the template's denominator param (ae_soc_pt; demog fixes it, so has no row)
    lapply(Filter(function(r) identical(r$name, "denominator"), d$rows), .fs_row_ui, m = m, t = t),
    help = t("The report's data: part is made from these values; the ARD step picks the denominator from them."))
}

# ------------------------------------------------------------------- step 2

.fs_ard_ui <- function(m, t) {
  d <- m$steps$ard
  if (!is.null(m$template)) {
    return(.fs_card(sprintf(t("Template %s: ARD"), m$template),
                    if (length(d$rows)) lapply(d$rows, .fs_row_ui, m = m, t = t) else
                      shiny::div(class = "rp-fs-note", t("No ARD values of its own."))))
  }
  shiny::tagList(lapply(d$analyses, function(a) {
    rows <- lapply(setdiff(names(a), "id"), function(n) .fs_row_ui(
      .fs_row(n, .fs_show(a[[n]]), "own"), m, t))
    .fs_card(paste0(a$id %||% "", if (!is.null(a$method)) paste0(" (", a$method, ")") else ""), rows)
  }))
}

# ------------------------------------------------------------------- step 3

.fs_content_ui <- function(m, t) {
  d <- m$steps$content
  if (!is.null(m$template)) {
    sw <- shiny::tags$div(
      class = "form-check form-switch small mt-1",
      shiny::tags$input(class = "form-check-input", type = "checkbox", disabled = "disabled"),
      shiny::tags$label(class = "form-check-label", t("Show study-standard values")))
    grp <- shiny::div(
      class = "rp-fs-group",
      shiny::div(class = "rp-fs-ghead", t("Pagination (plan_paginate_rows())"),
                 .fs_help(t("Where a page ends: how, how many rows, and what starts a page."))),
      lapply(d$pagebreak, .fs_row_ui, m = m, t = t))
    return(.fs_card(
      sprintf(t("Template %s: content"), m$template),
      .fs_row_ui(d$extends, m, t), sw,
      lapply(d$rows, .fs_row_ui, m = m, t = t),
      grp))
  }
  shiny::tagList(lapply(d$verbs, function(v) {
    verb <- names(v)[1L]
    args <- v[[1L]]
    rows <- lapply(names(args), function(n) {
      .fs_row_ui(.fs_row(n, .fs_show(args[[n]]), "own"), m, t)
    })
    .fs_card(if (identical(verb, "plan_paginate_rows")) t("Pagination (plan_paginate_rows())") else verb, rows)
  }))
}

# ------------------------------------------------------------------- step 4

.fs_line_src <- function(i, config, m, t) {
  if (identical(i$src, "removed") || isTRUE(i$removed)) {
    return(shiny::span(class = "badge rounded-pill rp-tpl-badge rp-tpl-badge-fallback",
                       t("Removed in this table")))
  }
  switch(i$src,
         config = .fs_badge("config", t, config),
         template = .fs_badge("default", t),
         own = .fs_badge("own", t, kind = m$kind))
}

.fs_band_ui <- function(b, m, t) {
  bd <- m$steps$page$bands[[b]]
  cfg <- m$steps$page$config
  head <- t(.fs_band_labels[[b]])
  help <- if (identical(b, "titles")) t("Title lines are the report's own; the configuration prints the table number, title and analysis set in its Header, with tokens.")
  if (isTRUE(bd$off)) {
    return(.fs_card(head, shiny::div(class = "rp-fs-note", t("(none): this band is not printed.")), help = help))
  }
  items <- lapply(bd$items, function(i) {
    gone <- identical(i$src, "removed") || isTRUE(i$removed)
    shiny::div(
      class = paste0("rp-fs-row", if (gone) " rp-fs-removed" else ""),
      shiny::span(class = "rp-fs-num", i$num,
                  if (identical(i$src, "config")) shiny::span(class = "rp-fs-mark", t(i$block))),
      shiny::span(class = "rp-fs-val", i$text),
      shiny::span(class = "rp-fs-src", .fs_line_src(i, cfg, m, t)),
      if (gone) shiny::span(.fs_btn(t("Restore"))) else
        if (identical(i$src, "config")) shiny::span(.fs_btn(t("Remove"))) else
          shiny::span(.fs_btn(t("Edit")), " ", .fs_btn(t("Remove"))))
  })
  none <- if (!length(items)) shiny::div(class = "rp-fs-note", t("(no lines)"))
  tools <- shiny::div(
    class = "rp-fs-tools", .fs_btn(t("Add a line")), .fs_btn(t("Replace all")),
    if (!is.null(m$template) && isTRUE(bd$own)) .fs_btn(t("Reset band to the template")))
  .fs_card(head, items, none, tools, help = help)
}

.fs_page_ui <- function(m, t) {
  pg <- m$steps$page
  by <- switch(m$style$chosen_by,
               own = t("This table"), template = t("Template default"),
               study = t("Study default"), t("Built-in default"))
  nm <- names(m$configs)
  picker <- shiny::div(
    class = "rp-fs-card",
    shiny::div(class = "rp-fs-head", t("Page configuration"),
               shiny::span(class = "rp-fs-note ms-2", sprintf(t("set by: %s"), by))),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::tags$select(
        class = "form-select form-select-sm", disabled = "disabled", style = "max-width: 16rem",
        lapply(nm, function(n) shiny::tags$option(
          value = n, selected = if (identical(n, m$style$name)) "selected",
          paste0(n, if (identical(m$configs[[n]]$source, "built-in")) paste0(" (", t("built-in"), ")") else "")))),
      .fs_btn(t("Copy this configuration as..."))))
  bands <- lapply(.fs_bands, .fs_band_ui, m = m, t = t)
  rtf_row <- function(r, value, badge) shiny::div(
    class = "rp-fs-row rp-fs-kv", shiny::span(class = "rp-fs-name", t(.fs_rtf_label(r$fn, r$arg))),
    value, shiny::span(class = "rp-fs-src", badge))
  settings <- .fs_card(
    t("RTF arguments (rtf_*)"),
    lapply(pg$settings, function(r) rtf_row(
      r, shiny::span(class = "rp-fs-val", r$value), .fs_badge(r$origin, t, r$config, m$kind))),
    if (length(pg$unset)) shiny::tags$details(
      class = "mt-2",
      shiny::tags$summary(class = "small", t("Arguments not set")),
      lapply(pg$unset, function(r) rtf_row(
        r, shiny::span(class = "rp-fs-val rp-fs-hint", t("(not set)")), NULL))))
  tokens <- .fs_card(
    t("Tokens"),
    if (length(pg$tokens)) lapply(pg$tokens, function(r) shiny::div(
      class = "rp-fs-row rp-fs-kv", shiny::span(class = "rp-fs-name", paste0("{", r$name, "}")),
      shiny::span(class = "rp-fs-val", r$value),
      shiny::span(class = "rp-fs-src", .fs_badge(r$origin, t, kind = m$kind, text = r$text)))) else
        shiny::div(class = "rp-fs-note", t("(none)")))
  shiny::tagList(picker, bands, settings, tokens)
}

# ------------------------------------------------------------ the screen

# the step names as this screen shows them ("3 Content" without the app's
# longer gloss; the trailing no-break space only makes the string its own
# key in inst/i18n)
.fs_step_labels <- c(data = "1 Analysis data", ard = "2 ARD", content = "3 Content\u00a0",
                     page = "4 Page and output")

# the Preview card: follows the chosen step; Code for every step, Data for step 1
.fs_preview_ui <- function(m, step, t) {
  badge <- shiny::span(class = "badge text-bg-warning rp-fs-sample", t("Sample"))
  code <- if (identical(step, "data")) shiny::tags$pre(class = "rp-fs-pre", .fs_code_data(m)) else
    shiny::div(class = "rp-fs-note", t("The code of this step (it comes when the step is connected)"))
  panels <- list(bslib::nav_panel(t("Code"), value = "code", shiny::div(class = "mt-2", badge, code)))
  if (identical(step, "data")) {
    got <- .fs_data_read(m)
    body <- if (!length(got)) shiny::div(class = "rp-fs-note", t("No data (sample)")) else
      lapply(got, function(g) shiny::div(
        shiny::div(shiny::tags$code(g$id), shiny::span(
          class = "rp-fs-note ms-2",
          sprintf(t("%s rows, %s subjects"), format(g$n_rows), format(g$n_subj)))),
        shiny::div(class = "rp-fs-minibox", shiny::tags$table(
          class = "table table-sm table-striped rp-fs-mini",
          shiny::tags$thead(shiny::tags$tr(lapply(utils::head(names(g$head), 8L), shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(g$head)), function(i) shiny::tags$tr(
            lapply(utils::head(names(g$head), 8L), function(n) shiny::tags$td(as.character(g$head[[n]][i]))))))))))
    panels[[2L]] <- bslib::nav_panel(t("Data"), value = "data", shiny::div(class = "mt-2", badge, body))
  }
  do.call(bslib::navset_tab, c(list(id = "fs_prev"), panels))
}

# the Preview card itself: drawn once; its body is made only when "Update" is
# pressed (never on an input change)
.fs_preview_card <- function(t) {
  bslib::card(
    class = "rp-fs-sticky", fill = FALSE,
    bslib::card_header(shiny::div(
      class = "d-flex justify-content-between align-items-center",
      t("Preview"),
      shiny::actionButton("fs_update", t("Update"), class = "btn btn-sm btn-primary",
                          title = t("Press Update (Ctrl+Enter) to apply the changes")))),
    bslib::card_body(fill = FALSE, shiny::uiOutput("fs_preview_body")))
}

.fs_screen_ui <- function(m, t = identity) {
  head <- shiny::div(
    class = "d-flex flex-wrap gap-2 align-items-baseline mb-2",
    shiny::strong(m$id), shiny::span(m$label),
    shiny::span(class = "badge text-bg-light border", t(.type_labels[[m$kind]] %||% m$kind)),
    shiny::span(class = "badge text-bg-secondary",
                if (is.null(m$template)) t("Code SPEC") else sprintf(t("Template %s"), m$template)))
  pane <- function(step, body) bslib::nav_panel(t(.fs_step_labels[[step]]), value = step, shiny::div(class = "mt-2", body))
  shiny::tagList(
    shiny::tags$style(shiny::HTML(.fs_css)),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)), fill = FALSE,
      shiny::div(
        head, .fs_common_ui(m, t),
        bslib::navset_pill(
          id = "fs_step",
          pane("data", .fs_data_ui(m, t)), pane("ard", .fs_ard_ui(m, t)),
          pane("content", .fs_content_ui(m, t)), pane("page", .fs_page_ui(m, t)))),
      .fs_preview_card(t)))
}

# the tab: the study folder, the report, the screen.  ("Report" with a
# trailing no-break space is its own key, as the tab's word.)
.fs_panel_ui <- function(t) {
  bslib::nav_panel(
    t("Report\u00a0"), value = "foursteps",
    shiny::tags$script(shiny::HTML(.fs_js)),
    shiny::div(
      class = "alert alert-warning py-1 small",
      t("Provisional screen: it reads the report's YAML directly and only shows it; editing comes with the tflspec readers.")),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end mb-2",
      shiny::selectInput("fs_report", t("Report"), choices = character(), width = "16rem")),
    shiny::uiOutput("fs_screen"))
}
