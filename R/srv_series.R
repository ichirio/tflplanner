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
  # (the card has no Save of its own (#753): its edits go into the study in
  # memory, `series_edits`, and the top-right Save writes them into the
  # study's copies, spec/series/<series>/series.yml -- .series_save_edits())
  .series_server(input, output, session, study_dir = study_dir, t = t,
                 notify = notify, copies_ver = tpl_copies_ver,
                 planner = list(get = function() rv$p, set = function(p) rv$p <- p))
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

# The study's own values of a series as saved: its copy's values that are
# not the source's `src`, each as YAML text (.series_flow())
.series_own_text <- function(dir, series, src) {
  cp <- .series_copy_values(dir, series) %||% list()
  own <- cp[vapply(names(cp), function(k) !identical(cp[[k]], src[[k]]), NA)]
  lapply(own, .series_flow)
}

# What tfl_series_values_set() is given for a draft (key -> YAML text):
# each value read from its text; a key of `saved` (the study's own values
# as saved) the draft took back to the series gets the source's value
# again (a key the source does not have: NULL, which takes it out of the
# copy).  A text that is not YAML stops.
.series_values_to_set <- function(draft, src, saved) {
  out <- lapply(draft, .series_yaml_load)
  for (k in setdiff(names(saved), names(draft))) out[k] <- list(src[[k]])
  out <- lapply(out, function(v) if (is.null(v)) v else .series_value_out(v))
  if (length(out)) out else stats::setNames(list(), character())
}

# The series card's unsaved values (a planner's `series_edits`: series ->
# draft) written into the study's copies, by the study's save: the files
# written (file, status), as save_study() lists them
.series_save_edits <- function(root, edits) {
  files <- data.frame(file = character(), status = character(), stringsAsFactors = FALSE)
  for (s in names(edits)) {
    if (!.series_copied(root, s)) {
      stop("The study has no copy of the series ", s, " (spec/series/", s,
           "/series.yml): its values cannot be saved.", call. = FALSE)
    }
    src <- .series_fn("tfl_series_read")(s)$values %||% list()
    vals <- tryCatch(.series_values_to_set(edits[[s]], src, .series_own_text(root, s, src)),
                     error = function(e) stop("A value of the series ", s, " is not YAML: ",
                                              conditionMessage(e), call. = FALSE))
    .series_fn("tfl_series_values_set")(root, s, vals)
    files[nrow(files) + 1L, ] <- list(file.path(.series_copy_root(root), s, "series.yml"), "written")
  }
  files
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
  if (inherits(v, "tfl_study_ref")) return(tag(as.character(v$key), "!study"))
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
# `planner`: the study in memory, list(get, set) (the app's rv$p).  The
# card has no Save of its own (#753): an edit of the study's values goes at
# once into the planner's `series_edits` (series -> the study's own values
# as YAML texts; the top-right indicator then says "Unsaved changes"), and
# the top-right Save writes them into the study's copy
# (.series_save_edits()).  Undo under the card puts back what the card
# wrote since the series was chosen; a save or a discard calls the hook
# the card puts in session$userData$series_rebase.
.series_server <- function(input, output, session, study_dir, t = identity,
                           notify = function(msg, type = "message") {
                             shiny::showNotification(msg, type = type)
                           },
                           copies_ver = shiny::reactiveVal(0L),
                           planner = NULL) {
  .series_dir_option()
  ok <- .series_available()
  if (is.null(planner)) {
    held <- shiny::reactiveVal(list())
    planner <- list(get = function() held(), set = function(p) held(p))
  }
  # list_ver: the series read again; copy_ver: the study's copies changed;
  # disk_ver: the copy's values read again (a save, a discard); redraw: the
  # values' fields drawn again (not at an edit typed in them)
  rv <- shiny::reactiveValues(list_ver = 0L, copy_ver = 0L, disk_ver = 0L,
                              redraw = 0L, saved = NULL, draft = NULL,
                              series = NULL, recopy = NULL, problem = NULL)
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
  # values that are not the source's (each value as YAML text)
  saved_now <- shiny::reactive({
    s <- rv$series
    dir <- study_dir()
    rv$copy_ver
    rv$disk_ver
    copies_ver()
    if (is.null(s)) return(NULL)
    src <- chosen()$values %||% list()
    tryCatch(.series_own_text(dir, s, src), error = fail) %||% list()
  })
  # the draft: the study's unsaved values of the series, else as saved
  live <- .form_live(session, settle = 0)
  typed <- new.env(parent = emptyenv())
  shiny::observe({
    s <- rv$series
    sv <- saved_now()
    d <- planner$get()$series_edits[[s %||% ""]] %||% sv
    shiny::isolate({
      rv$saved <- sv
      # (an edit typed in the fields is not drawn again: the field keeps it)
      if (!identical(d, typed$d) || !identical(names(d), names(rv$draft))) {
        rv$redraw <- rv$redraw + 1L
      }
      typed$d <- NULL
      rv$draft <- d
    })
  })
  # (another series, or another study: Undo starts from the study as it is)
  shiny::observe({
    rv$series
    study_dir()
    shiny::isolate(live$open(planner$get()))
  })
  # the draft put into the study in memory: a draft that is the copy's
  # values again is no change
  put_draft <- function(d) {
    s <- rv$series
    p <- planner$get()
    e <- p$series_edits %||% list()
    e[s] <- if (identical(d, rv$saved)) list(NULL) else list(d)
    e <- e[!vapply(e, is.null, NA)]
    p$series_edits <- if (length(e)) e
    if (identical(p, planner$get())) return(invisible(FALSE))
    planner$set(p)
    live$wrote(p)
    invisible(TRUE)
  }
  editable <- function() {
    dir <- study_dir()
    !is.null(dir) && !is.null(rv$series) && .series_copied(dir, rv$series)
  }
  # what is typed goes into the draft as it is typed (a text that is not
  # YAML is said under the card, and not taken in).  A field's value is
  # taken only when it changed since last seen, and is not the first value
  # back after the field was drawn, its text (its echo): a row's
  # field keeps its old text after its key went back to the series, and is
  # no edit when the key is changed again.
  seen <- new.env(parent = emptyenv())
  drawn <- new.env(parent = emptyenv())
  shiny::observe({
    keys <- union(names(chosen()$values), names(rv$draft))
    d <- shiny::isolate(rv$draft)
    if (!length(d)) return()
    got <- lapply(stats::setNames(names(d), names(d)), function(k)
      input[[paste0("series_edit_", match(k, keys))]])
    shiny::isolate({
      if (!editable()) return()
      new <- d
      for (k in names(got)) {
        iid <- paste0("series_edit_", match(k, keys))
        v <- got[[k]]
        if (is.null(v) || identical(v, seen[[iid]])) next
        seen[[iid]] <- v
        echo <- identical(v, drawn[[iid]])
        drawn[[iid]] <- NULL
        if (echo) next
        new[[k]] <- v
      }
      bad <- names(new)[vapply(new, function(x)
        inherits(tryCatch(.series_yaml_load(x), error = function(e) e), "error"), NA)]
      rv$problem <- if (length(bad)) sprintf(t("Not YAML, so not taken in: %s"), paste(bad, collapse = ", "))
      if (length(bad) || identical(new, d)) return()
      typed$d <- new
      put_draft(new)
    })
  })
  shiny::observeEvent(input$series_change, {
    k <- input$series_change$key
    v <- chosen()$values
    if (!editable() || !k %in% names(v) || k %in% names(rv$draft)) return()
    d <- rv$draft
    d[[k]] <- .series_edit_text(v[[k]])
    put_draft(d)
  })
  shiny::observeEvent(input$series_back, {
    if (!editable()) return()
    d <- rv$draft
    d[[input$series_back$key]] <- NULL
    put_draft(d)
  })
  shiny::observeEvent(input$series_undo, {
    if (!live$changed()) return()
    # (the card's own part only: what else changed meanwhile stays)
    p <- planner$get()
    part <- function(x) list(series_edits = x$series_edits)
    p$series_edits <- .form_restore(part(p), part(live$p0), part(live$mine))$series_edits
    planner$set(p)
    live$open(p)
    rv$problem <- NULL
  })
  # the study saved, or its changes discarded (the app's forms_rebase()):
  # the copy read again, Undo from the study as it is now
  session$userData$series_rebase <- function(discarded = FALSE) {
    rv$disk_ver <- rv$disk_ver + 1L
    rv$problem <- NULL
    live$rebase(planner$get())
    invisible()
  }

  output$series_values <- shiny::renderUI({
    x <- chosen()
    if (is.null(x)) return(NULL)
    dir <- study_dir()
    rv$copy_ver
    rv$redraw
    copied <- .series_copied(dir, x$name)
    shiny::tagList(
      shiny::h6(class = "mt-3 mb-1",
                with_tip(sprintf(t("Defaults shared by the %s series' templates (values:)"), x$name),
                         t("The series' values as written (a tag as written: !series, !study, !fixed, !unset). The study reads its own copy of the series (spec/series/<series>/series.yml). A value changed here goes into the study at once; the top-right Save writes it into the copy (tfl_series_values_set)."))),
      if (length(x$path)) shiny::p(class = "small text-muted mb-1", shiny::tags$code(x$path)),
      if (is.null(dir) || !copied) {
        shiny::tagList(
          .series_values_ui(x$values, list(), list(), t, x$descriptions),
          shiny::p(class = "small text-muted",
                   if (is.null(dir)) t("Open a study to change its values.") else
                     t("This study has no copy of this series yet: adding a report from one of its templates copies it.")))
      } else {
        # (the fields' texts as drawn: their values coming back are no edit)
        d <- shiny::isolate(rv$draft)
        keys <- union(names(x$values), names(d))
        for (k in names(d)) drawn[[paste0("series_edit_", match(k, keys))]] <- d[[k]]
        shiny::tagList(
          .series_values_ui(x$values, d, shiny::isolate(rv$saved), t,
                            x$descriptions),
          shiny::uiOutput("series_problem"),
          shiny::uiOutput("series_undo_ui"))
      }
    )
  })
  output$series_problem <- shiny::renderUI({
    if (is.null(rv$problem)) return(NULL)
    shiny::div(class = "alert alert-warning small py-1 my-1", rv$problem)
  })
  output$series_undo_ui <- shiny::renderUI({
    # (shown once the card has written since the series was chosen)
    planner$get()$series_edits
    if (!live$changed()) return(NULL)
    .btn("series_undo", t("Undo"), class = "btn-sm btn-outline-secondary",
         title = t("Back to the values when the series was chosen"))
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
