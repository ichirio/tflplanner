# The four-steps report screen: its layout (provisional).  One structure for
# a template report and a code-SPEC report -- a Common block on top, then four
# steps -- drawn from the model of R/foursteps_read.R.  Display only for now:
# the edit buttons are drawn and off until the tflspec readers land.
# (PAGE-PART 7.1, DATA-PART 5.4.)

.fs_css <- "
.rp-fs-card { border: 1px solid #dee2e6; border-radius: .375rem; padding: .5rem .75rem; margin-bottom: .6rem; }
.rp-fs-card > .rp-fs-head { font-weight: 600; font-size: .9rem; margin-bottom: .3rem; }
.rp-fs-row { display: flex; gap: .5rem; align-items: baseline; padding: .2rem 0; border-bottom: 1px solid #f1f3f5; }
.rp-fs-row:last-child { border-bottom: none; }
.rp-fs-name { flex: 0 0 34%; font-size: .85rem; }
.rp-fs-val { flex: 1 1 auto; min-width: 0; font-family: var(--bs-font-monospace, monospace); font-size: .82rem;
  overflow-wrap: anywhere; }
.rp-fs-val.rp-fs-hint { color: #8a8f98; }
.rp-fs-num { flex: 0 0 2rem; text-align: right; color: #6c757d; font-size: .8rem; }
.rp-fs-src { flex: 0 0 auto; }
.rp-fs-line-cfg .rp-fs-val { color: #495057; }
.rp-fs-note { font-size: .78rem; color: #6c757d; }
.rp-fs-tools { display: flex; flex-wrap: wrap; gap: .3rem; margin-top: .4rem; }
.rp-fs-common { background: #f8f9fa; }
"

# the badge that says where a value comes from
.fs_badge <- function(origin, t, config = NULL, kind = "table") {
  txt <- switch(origin,
    own = t(if (identical(kind, "table")) "This table" else "This report"),
    default = t("Template default"),
    fixed = t("Fixed by the template"),
    config = sprintf(t("Page configuration %s"), config %||% ""),
    study = t("Study"),
    template = t("Template default"),
    NULL)
  if (is.null(txt)) return(NULL)
  cls <- switch(origin, own = "own", config = "study", study = "study", "default")
  shiny::span(class = paste0("badge rounded-pill rp-tpl-badge rp-tpl-badge-", cls), txt)
}

# a value that is a choice of the report's datasets: "adsl_sub (analysis set)"
.fs_den_select <- function(value, m, t) {
  ids <- m$steps$data$ids
  lab <- paste0(ids, " (", vapply(m$steps$data$id_roles, t, ""), ")")
  shiny::tags$select(
    class = "form-select form-select-sm", disabled = "disabled", style = "max-width: 22rem",
    lapply(seq_along(ids), function(i) shiny::tags$option(
      value = ids[[i]], selected = if (identical(ids[[i]], value)) "selected", lab[[i]])))
}

.fs_value_el <- function(r, m, t) {
  if (identical(r$name, "denominator")) return(.fs_den_select(r$value, m, t))
  hint <- r$origin %in% c("default", "none")
  empty <- !nzchar(r$value)
  shiny::span(class = paste0("rp-fs-val", if (hint || empty) " rp-fs-hint" else ""),
              if (empty) t("(not set)") else r$value)
}

# one row of a step: the label ("meaning (name)"), the value, where it comes from
.fs_row_ui <- function(r, m, t) {
  lab <- if (r$name %in% names(.fs_labels)) .fs_labels[[r$name]]
  shiny::div(
    class = "rp-fs-row",
    shiny::span(class = "rp-fs-name", if (is.null(lab)) r$name else t(lab)),
    .fs_value_el(r, m, t),
    shiny::span(class = "rp-fs-src", .fs_badge(r$origin, t, r$config, m$kind)))
}

.fs_card <- function(title, ..., note = NULL) {
  shiny::div(class = "rp-fs-card",
             shiny::div(class = "rp-fs-head", title),
             if (!is.null(note)) shiny::div(class = "rp-fs-note mb-1", note),
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
  shiny::div(
    class = "rp-fs-card rp-fs-common",
    shiny::div(class = "rp-fs-head", t("Common"),
               shiny::span(class = "rp-fs-note ms-2", t("Read by more than one step"))),
    line("Analysis set (population)", sel, cm$population$origin),
    line("Treatment variable (treatment)",
         if (nzchar(cm$treatment$value)) cm$treatment$value else shiny::span(class = "rp-fs-hint", t("(not set)")),
         cm$treatment$origin),
    line("Analysis set label (population_label)",
         if (nzchar(cm$population_label$value)) cm$population_label$value else shiny::span(class = "rp-fs-hint", t("(not set)")),
         cm$population_label$origin))
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
               note = t("The report's data: part is made from these values; the ARD step picks the denominator from them."))))
  }
  shiny::tagList(lapply(d$defs, function(x) {
    title <- paste0(x$id, " (", t(x$role), ")")
    rows <- lapply(names(x$fields), function(n) .fs_row_ui(
      .fs_row(n, .fs_show(x$fields[[n]]), "own"), m, t))
    if (isTRUE(x$implicit)) {
      .fs_card(title, shiny::div(class = "rp-fs-row", shiny::span(class = "rp-fs-val", x$text),
                                 shiny::span(class = "rp-fs-src", .fs_badge("study", t, kind = m$kind))),
               note = t("Always made first, even when the report does not write it."))
    } else .fs_card(title, rows)
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
    return(shiny::tagList(
      .fs_card(t("Page breaks"), lapply(d$pagebreak, .fs_row_ui, m = m, t = t),
               note = t("Where a page ends: how, how many rows, and what starts a page.")),
      .fs_card(sprintf(t("Template %s: content"), m$template),
               if (length(d$rows)) lapply(d$rows, .fs_row_ui, m = m, t = t) else
                 shiny::div(class = "rp-fs-note", t("No content values of its own.")))))
  }
  shiny::tagList(lapply(d$verbs, function(v) {
    verb <- names(v)[1L]
    args <- v[[1L]]
    rows <- lapply(names(args), function(n) {
      .fs_row_ui(.fs_row(n, .fs_show(args[[n]]), "own"), m, t)
    })
    .fs_card(if (identical(verb, "plan_paginate_rows")) paste0(t("Page breaks"), " (plan_paginate_rows)") else verb, rows)
  }))
}

# ------------------------------------------------------------------- step 4

.fs_line_src <- function(i, config, m, t) {
  switch(i$src,
         config = .fs_badge("config", t, config),
         template = .fs_badge("default", t),
         own = .fs_badge("own", t, kind = m$kind))
}

.fs_band_ui <- function(b, m, t) {
  bd <- m$steps$page$bands[[b]]
  cfg <- m$steps$page$config
  head <- t(.fs_band_labels[[b]])
  if (isTRUE(bd$off)) {
    return(.fs_card(head, shiny::div(class = "rp-fs-note", t("(none): this band is not printed."))))
  }
  items <- lapply(bd$items, function(i) shiny::div(
    class = paste0("rp-fs-row", if (identical(i$src, "config")) " rp-fs-line-cfg" else ""),
    shiny::span(class = "rp-fs-num", if (is.na(i$num)) "" else i$num),
    shiny::span(class = "rp-fs-val", i$text),
    shiny::span(class = "rp-fs-src", .fs_line_src(i, cfg, m, t)),
    if (!identical(i$src, "config")) shiny::span(.fs_btn(t("Edit")), " ", .fs_btn(t("Remove")))))
  none <- if (!length(items)) shiny::div(class = "rp-fs-note", t("(no lines)"))
  note <- if (identical(b, "titles")) t("Title lines are the report's own; the configuration prints the table number, title and set in its Header, with tokens.")
  tools <- shiny::div(
    class = "rp-fs-tools", .fs_btn(t("Add a line")), .fs_btn(t("Replace all")),
    if (!is.null(m$template)) .fs_btn(t("Back to the template")))
  .fs_card(head, items, none, tools, note = note)
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
               shiny::span(class = "rp-fs-note ms-2", sprintf(t("chosen by: %s"), by))),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::tags$select(
        class = "form-select form-select-sm", disabled = "disabled", style = "max-width: 16rem",
        lapply(nm, function(n) shiny::tags$option(
          value = n, selected = if (identical(n, m$style$name)) "selected",
          paste0(n, if (identical(m$configs[[n]]$source, "built-in")) paste0(" (", t("built-in"), ")") else "")))),
      .fs_btn(t("Copy this configuration as..."))))
  bands <- lapply(.fs_bands, .fs_band_ui, m = m, t = t)
  settings <- .fs_card(
    t("Page settings"),
    if (length(pg$settings)) lapply(pg$settings, function(r) shiny::div(
      class = "rp-fs-row", shiny::span(class = "rp-fs-name", r$name),
      shiny::span(class = "rp-fs-val", r$value),
      shiny::span(class = "rp-fs-src", .fs_badge(r$origin, t, r$config, m$kind)))))
  tokens <- .fs_card(
    t("Tokens"),
    if (length(pg$tokens)) lapply(pg$tokens, function(r) shiny::div(
      class = "rp-fs-row", shiny::span(class = "rp-fs-name", paste0("{", r$name, "}")),
      shiny::span(class = "rp-fs-val", r$value),
      shiny::span(class = "rp-fs-src", .fs_badge(r$origin, t, kind = m$kind)))) else
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
