# app_server(): the Template series card (Study tab).  Run in
# app_server()'s environment through .srv_study() (.srv_part()); the card
# itself is .series_server(), which a test starts on its own.
.srv_series <- function() {
  study_dir <- shiny::reactiveVal(NULL)
  shiny::observe({
    study_dir(rv$study$path)
    # (the template cards read a template from the open study's copy)
    .tpl_study_dir(rv$study$path)
  })
  session$onSessionEnded(function() .tpl_study_dir(NULL))
  # (the card writes only the study's copies, spec/series/: no file the
  # study's save knows)
  .series_server(input, output, session, study_dir = study_dir, t = t,
                 notify = notify, copies_ver = tpl_copies_ver)
}

# The study's copies of the series (<study>/spec/series, canon 2026-11-19)
.series_copy_root <- function(dir) file.path(dir, study_layout()[["spec"]], "series")

# whether a study holds its own copy of a series (its series.yml)
.series_copied <- function(dir, series) {
  !is.null(dir) && !is.null(series) &&
    file.exists(file.path(.series_copy_root(dir), series, "series.yml"))
}

# The series' values in the study's copy, NULL when the study has none
.series_copy_values <- function(dir, series) {
  if (!.series_copied(dir, series)) return(NULL)
  x <- .series_fn("tfl_series_read")(series, dir = .series_copy_root(dir))
  as.list(x$values %||% list())
}

# A value as tflspec's tfl_series_values_set() is to write it (it writes
# with yaml::as.yaml()): a tagged one (the source's !study K, !fixed v ...)
# as its value with the tag as the "tag" attribute, which as.yaml() writes
# as the tag (a "verbatim" text that starts with "!" is quoted: a string,
# not the tag); inside a list the same; a single true / false as true /
# false (as.yaml()'s own is yes / no)
.series_value_out <- function(v) {
  tag <- function(x, t) structure(x, tag = t)
  if (inherits(v, "tfl_series_ref")) return(tag(as.character(v$key), "!series"))
  if (inherits(v, "tfl_study_ref")) {
    if (!"fallback" %in% names(v)) return(tag(as.character(v$key), "!study"))
    # the map form: as.yaml() writes the tag before the map (a tagged map)
    return(tag(list(key = as.character(v$key), fallback = .series_value_out(v$fallback)), "!study"))
  }
  if (inherits(v, "tfl_fixed_ref")) return(tag(.series_value_out(v$value), "!fixed"))
  if (inherits(v, "tfl_unset")) return(tag("", "!unset"))
  if (is.logical(v) && length(v) == 1L && !is.na(v) && is.null(names(v))) {
    return(structure(if (v) "true" else "false", class = "verbatim"))
  }
  if (is.list(v) && !is.object(v)) {
    out <- lapply(v, function(x) if (is.null(x)) x else .series_value_out(x))
    return(if (identical(out, v)) v else out)
  }
  v
}

# `study_dir`: a reactive of the open study's folder (NULL: none open).
# `notify(msg, type)`: how a message is shown; tflspec's errors as they are.
# `copies_ver`: a reactive value the app moves when it copies a template
# into the study (a template report added, R/srv_add_report.R): the copies
# are read again.
.series_server <- function(input, output, session, study_dir, t = identity,
                           notify = function(msg, type = "message") {
                             shiny::showNotification(msg, type = type)
                           },
                           copies_ver = shiny::reactiveVal(0L)) {
  .series_dir_option()
  ok <- .series_available()
  # list_ver: the series read again; copy_ver: the study's copies changed
  rv <- shiny::reactiveValues(list_ver = 0L, copy_ver = 0L, saved = NULL,
                              draft = NULL, series = NULL, recopy = NULL)
  fail <- function(e) {
    notify(conditionMessage(e), "error")
    NULL
  }

  output$series_card <- shiny::renderUI({
    if (!ok) {
      return(shiny::p(class = "small text-muted mb-0",
                      sprintf(t("This tflspec (%s) has no template series."),
                              as.character(utils::packageVersion("tflspec")))))
    }
    shiny::tagList(
      shiny::h6(class = "mb-1", t("Series (tfl_series_list)")),
      DT::DTOutput("series_list", height = "auto", fill = FALSE),
      shiny::uiOutput("series_values"),
      shiny::uiOutput("series_copies"))
  })
  if (!ok) return(invisible(rv))

  series_list <- shiny::reactive({
    rv$list_ver
    tryCatch(.series_fn("tfl_series_list")(), error = fail)
  })
  output$series_list <- DT::renderDT({
    d <- series_list()
    sel <- match(shiny::isolate(rv$series), d$name)
    if (is.na(sel %||% NA)) sel <- if (NROW(d)) 1L else integer()
    DT::datatable(.series_list_table(d, t), rownames = FALSE,
                  selection = list(mode = "single", selected = sel, target = "row"),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE),
                  class = "compact")
  })
  # the study's copies (tfl_template_status()), read again when they change
  copies <- shiny::reactive({
    rv$copy_ver
    copies_ver()
    dir <- study_dir()
    if (is.null(dir) || !dir.exists(dir)) return(NULL)
    tryCatch(.series_fn("tfl_template_status")(dir), error = fail)
  })
  # the series chosen: the row clicked, else one the study has copied,
  # else the first
  shiny::observe({
    d <- series_list()
    if (!NROW(d)) {
      rv$series <- NULL
      return()
    }
    i <- input$series_list_rows_selected
    want <- if (length(i)) d$name[i[1L]] else c(intersect(copies()$series, d$name), d$name)[1L]
    if (!identical(want, shiny::isolate(rv$series))) rv$series <- want
  })
  chosen <- shiny::reactive({
    s <- rv$series
    rv$list_ver
    if (is.null(s)) return(NULL)
    tryCatch(.series_fn("tfl_series_read")(s), error = fail)
  })
  # the study's own values for the chosen series, as saved: its copy's
  # values that are not the source's; the draft starts from them (each
  # value as YAML text)
  load_block <- function() {
    src <- chosen()$values %||% list()
    cp <- tryCatch(.series_copy_values(study_dir(), rv$series), error = fail) %||% list()
    own <- cp[vapply(names(cp), function(k) !identical(cp[[k]], src[[k]]), NA)]
    rv$saved <- lapply(own, .series_flow)
    rv$draft <- rv$saved
  }
  shiny::observe({
    rv$series
    study_dir()
    rv$copy_ver
    copies_ver()
    shiny::isolate(load_block())
  })
  # what is typed is taken into the draft before the table is drawn again
  # (the inputs are numbered by the table's rows)
  take_typed <- function() {
    keys <- union(names(chosen()$values), names(rv$draft))
    d <- rv$draft
    for (k in names(d)) {
      v <- input[[paste0("series_edit_", match(k, keys))]]
      if (!is.null(v)) d[[k]] <- v
    }
    rv$draft <- d
  }
  shiny::observeEvent(input$series_change, {
    k <- input$series_change$key
    v <- chosen()$values
    take_typed()
    if (!k %in% names(v) || k %in% names(rv$draft)) return()
    d <- rv$draft
    d[[k]] <- .series_edit_text(v[[k]])
    rv$draft <- d
  })
  shiny::observeEvent(input$series_back, {
    take_typed()
    d <- rv$draft
    d[[input$series_back$key]] <- NULL
    rv$draft <- d
  })
  shiny::observeEvent(input$series_save, {
    dir <- study_dir()
    s <- rv$series
    if (is.null(dir) || is.null(s) || !.series_copied(dir, s)) return()
    take_typed()
    d <- rv$draft
    src <- chosen()$values %||% list()
    vals <- tryCatch({
      out <- lapply(d, .series_yaml_load)
      # a key taken back to the series: the source's value again (a key
      # the source does not have: NULL, which takes it out of the copy)
      for (k in setdiff(names(rv$saved), names(d))) out[k] <- list(src[[k]])
      out <- lapply(out, function(v) if (is.null(v)) v else .series_value_out(v))
      if (length(out)) out else stats::setNames(list(), character())
    }, error = function(e) {
      notify(paste0(t("A value is not YAML:"), " ", conditionMessage(e)), "error")
      FALSE
    })
    if (isFALSE(vals)) return()
    done <- tryCatch({
      .series_fn("tfl_series_values_set")(dir, s, vals)
      TRUE
    }, error = fail)
    if (is.null(done)) return()
    rv$copy_ver <- rv$copy_ver + 1L
    notify(sprintf(t("Saved this study's values for %s (its copy's series.yml)."), s))
  })

  output$series_values <- shiny::renderUI({
    x <- chosen()
    if (is.null(x)) return(NULL)
    dir <- study_dir()
    rv$copy_ver
    copied <- .series_copied(dir, x$name)
    changed <- !identical(rv$draft, rv$saved)
    shiny::tagList(
      shiny::h6(class = "mt-3 mb-1",
                with_tip(sprintf(t("Defaults shared by the %s series' templates (values:)"), x$name),
                         t("The series' values as written (a tag as written: !series, !study, !fixed, !unset). The study reads its own copy of the series (spec/series/<series>/series.yml): a value changed for this study is written there."))),
      if (length(x$path)) shiny::p(class = "small text-muted mb-1", shiny::tags$code(x$path)),
      if (is.null(dir) || !copied) {
        shiny::tagList(
          .series_values_ui(x$values, list(), list(), t, x$descriptions),
          shiny::p(class = "small text-muted",
                   if (is.null(dir)) t("Open a study to change its values.") else
                     t("This study has no copy of this series yet: adding a report from one of its templates copies it.")))
      } else {
        shiny::tagList(
          .series_values_ui(x$values, rv$draft, rv$saved, t, x$descriptions),
          shiny::div(
            class = "d-flex gap-2 align-items-center",
            .btn("series_save", t("Save this study's values (tfl_series_values_set)"),
                 class = if (changed) "btn-sm btn-primary" else "btn-sm btn-outline-secondary"),
            if (changed) shiny::span(class = "small text-warning", t("unsaved"))))
      })
  })

  # the study's copies: their state, a template's differences and a copy
  # again from its source (asked first when the study's own edits go)
  output$series_copies <- shiny::renderUI({
    dir <- study_dir()
    if (is.null(dir)) return(NULL)
    shiny::tagList(
      shiny::h6(class = "mt-3 mb-1",
                with_tip(t("This study's copies (spec/series/)"),
                         t("The series and templates this study reads, copied from their source when a report first used them (tfl_template_status). A source changed since is not taken in by itself: look at the differences and copy the template again."))),
      .series_copies_ui(copies(), t))
  })
  status_of <- function(id) {
    d <- copies()
    i <- match(id, paste0(d$series, "/", d$template))
    if (is.na(i)) NA_character_ else d$status[i]
  }
  shiny::observeEvent(input$series_diff, {
    dir <- study_dir()
    id <- input$series_diff$key
    if (is.null(dir) || is.null(id)) return()
    lines <- tryCatch(.series_fn("tfl_template_diff")(dir, id), error = fail)
    if (is.null(lines)) return()
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Differences of %s: this study's copy against the source"), id),
      size = "l", easyClose = TRUE,
      shiny::p(class = "small mb-2", .series_status_label(status_of(id), t)),
      .series_diff_ui(lines, t),
      footer = shiny::tagList(
        shiny::modalButton(t("Close")),
        .series_act("series_recopy", id, t("Copy again from the source"),
                    class = "btn btn-outline-primary"))))
  })
  recopy <- function(dir, id) {
    # (the study's edits lost: asked before, so tflspec's warning is not said again)
    out <- tryCatch(suppressWarnings(.series_fn("tfl_recopy_templates")(dir, id)), error = fail)
    if (is.null(out)) return()
    rv$copy_ver <- rv$copy_ver + 1L
    notify(sprintf(t("Copied %s again from the source."), id))
  }
  shiny::observeEvent(input$series_recopy, {
    dir <- study_dir()
    id <- input$series_recopy$key
    if (is.null(dir) || is.null(id)) return()
    shiny::removeModal()
    if (!.series_own_edits(status_of(id))) return(recopy(dir, id))
    rv$recopy <- id
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Copy %s again from the source?"), id),
      shiny::p(t("This study's copy has edits of its own. Copying it again from the source loses them.")),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        .btn("series_recopy_ok", t("Copy again and lose this study's edits"), class = "btn-danger")),
      easyClose = FALSE))
  })
  shiny::observeEvent(input$series_recopy_ok, {
    shiny::removeModal()
    dir <- study_dir()
    id <- rv$recopy
    rv$recopy <- NULL
    if (!is.null(dir) && !is.null(id)) recopy(dir, id)
  })
  invisible(rv)
}
