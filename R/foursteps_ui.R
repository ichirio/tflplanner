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
.rp-fs-q { display: inline-block; width: 1.1rem; height: 1.1rem; line-height: 1rem; text-align: center;
  border: 1px solid #adb5bd; border-radius: 50%; font-size: .72rem; font-weight: 600; color: #495057;
  cursor: help; margin-left: .35rem; }
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

# a choice among the report's datasets: "adsl_sub (analysis set)"
.fs_den_select <- function(value, m, t) {
  ids <- m$steps$data$ids
  lab <- paste0(ids, " (", vapply(m$steps$data$id_roles, t, ""), ")")
  shiny::tags$select(
    class = "form-select form-select-sm", disabled = "disabled", style = "max-width: 22rem",
    lapply(seq_along(ids), function(i) shiny::tags$option(
      value = ids[[i]], selected = if (identical(ids[[i]], value)) "selected", lab[[i]])))
}

.fs_value_el <- function(r, m, t) {
  # a fixed value: plain locked text
  if (identical(r$origin, "fixed")) {
    lab <- if (identical(r$name, "denominator")) paste0(r$value, " (", t("analysis set"), ")") else r$value
    return(shiny::span(class = "rp-fs-val", lab))
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
    class = "rp-fs-row",
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
    class = "rp-fs-row",
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

.fs_data_ui <- function(m, t) {
  d <- m$steps$data
  if (!is.null(m$template)) {
    ds <- lapply(d$datasets, function(x) shiny::div(
      class = "rp-fs-row",
      shiny::span(class = "rp-fs-name", paste0(x$id, " (", t(x$role), ")")),
      shiny::span(class = "rp-fs-val", paste0(x$text, if (nzchar(x$subset)) paste0("; ", x$subset) else ""))))
    return(shiny::tagList(
      .fs_card(sprintf(t("Template %s: analysis data"), m$template),
               lapply(d$rows, .fs_row_ui, m = m, t = t)),
      .fs_card(t("Datasets this report builds"), ds,
               help = t("The report's data: part is made from these values; the ARD step picks the denominator from them."))))
  }
  shiny::tagList(lapply(d$defs, function(x) {
    title <- paste0(x$id, " (", t(x$role), ")")
    summary <- shiny::div(class = "rp-fs-row", shiny::span(class = "rp-fs-val", x$text),
                          if (isTRUE(x$implicit)) shiny::span(class = "rp-fs-src", .fs_badge("study", t, kind = m$kind)))
    rows <- lapply(names(x$fields), function(n) .fs_row_ui(
      .fs_row(n, .fs_show(x$fields[[n]]), "own"), m, t))
    .fs_card(title, summary, rows,
             help = if (isTRUE(x$implicit)) t("Always made first, even when the report does not write it."))
  }))
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
    class = "rp-fs-row", shiny::span(class = "rp-fs-name", t(.fs_rtf_label(r$fn, r$arg))),
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
      class = "rp-fs-row", shiny::span(class = "rp-fs-name", paste0("{", r$name, "}")),
      shiny::span(class = "rp-fs-val", r$value),
      shiny::span(class = "rp-fs-src", .fs_badge(r$origin, t, kind = m$kind, text = r$text)))) else
        shiny::div(class = "rp-fs-note", t("(none)")))
  shiny::tagList(picker, bands, settings, tokens)
}

# ------------------------------------------------------------ the screen

.fs_screen_ui <- function(m, t = identity) {
  head <- shiny::div(
    class = "d-flex flex-wrap gap-2 align-items-baseline mb-2",
    shiny::strong(m$id), shiny::span(m$label),
    shiny::span(class = "badge text-bg-light border", t(.type_labels[[m$kind]] %||% m$kind)),
    shiny::span(class = "badge text-bg-secondary",
                if (is.null(m$template)) t("Code SPEC") else sprintf(t("Template %s"), m$template)))
  pane <- function(step, body) bslib::nav_panel(t(.step_labels[[step]]), value = step, shiny::div(class = "mt-2", body))
  shiny::tagList(
    shiny::tags$style(shiny::HTML(.fs_css)),
    head, .fs_common_ui(m, t),
    bslib::navset_pill(
      id = "fs_step",
      pane("data", .fs_data_ui(m, t)), pane("ard", .fs_ard_ui(m, t)),
      pane("content", .fs_content_ui(m, t)), pane("page", .fs_page_ui(m, t))))
}

# the tab: the study folder, the report, the screen
.fs_panel_ui <- function(t) {
  bslib::nav_panel(
    t("Report (four steps)"), value = "foursteps",
    shiny::div(
      class = "alert alert-warning py-1 small",
      t("Provisional screen: it reads the report's YAML directly and only shows it; editing comes with the tflspec readers.")),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end mb-2",
      shiny::textInput("fs_study", t("Study folder"), value = .fs_study_default(), width = "26rem"),
      shiny::selectInput("fs_report", t("Report"), choices = character(), width = "16rem")),
    shiny::uiOutput("fs_screen"))
}
