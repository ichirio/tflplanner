# app_server(): the Template series card (Study tab).  Run in
# app_server()'s environment through .srv_study() (.srv_part()); the card
# itself is .series_server(), which a test starts on its own.
.srv_series <- function() {
  study_dir <- shiny::reactiveVal(NULL)
  shiny::observe(study_dir(rv$study$path))
  .series_server(input, output, session, study_dir = study_dir, t = t,
                 notify = notify)
}

# `study_dir`: a reactive of the open study's folder (NULL: none open).
# `notify(msg, type)`: how a message is shown; tflspec's errors as they are.
.series_server <- function(input, output, session, study_dir, t = identity,
                           notify = function(msg, type = "message") {
                             shiny::showNotification(msg, type = type)
                           }) {
  .series_dir_option()
  ok <- .series_available()
  # list_ver: the series read again; lock_ver: spec/series.lock written
  rv <- shiny::reactiveValues(list_ver = 0L, lock_ver = 0L, saved = NULL,
                              draft = NULL, series = NULL)
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
      shiny::uiOutput("series_lock"))
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
  # the series chosen: the row clicked, else the study's (its block in
  # series_settings), else the first
  shiny::observe({
    d <- series_list()
    if (!NROW(d)) {
      rv$series <- NULL
      return()
    }
    i <- input$series_list_rows_selected
    want <- if (length(i)) d$name[i[1L]] else {
      dir <- study_dir()
      have <- if (!is.null(dir)) {
        names(tryCatch(.series_fn("tfl_series_settings")(dir), error = function(e) NULL))
      }
      c(intersect(have, d$name), d$name)[1L]
    }
    if (!identical(want, shiny::isolate(rv$series))) rv$series <- want
  })
  chosen <- shiny::reactive({
    s <- rv$series
    rv$list_ver
    if (is.null(s)) return(NULL)
    tryCatch(.series_fn("tfl_series_read")(s), error = fail)
  })
  # the study's block for the chosen series, as saved; the draft starts
  # from it (each value as YAML text)
  load_block <- function() {
    s <- rv$series
    dir <- study_dir()
    b <- if (!is.null(s) && !is.null(dir)) {
      tryCatch(.series_fn("tfl_series_settings")(dir, s), error = fail)
    }
    # (tflspec gives the series' values with the study's over them, and
    # says which are the study's: only those are this study's own)
    o <- attr(b, "origins")
    if (!is.null(o)) b <- b[names(b) %in% names(o)[o == "study"]]
    b <- as.list(b %||% list())
    rv$saved <- lapply(b, .series_flow)
    rv$draft <- rv$saved
  }
  shiny::observe({
    rv$series
    study_dir()
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
    if (is.null(dir) || is.null(s)) return()
    take_typed()
    d <- rv$draft
    vals <- tryCatch({
      out <- lapply(d, .series_yaml_load)
      # a key taken back to the series: NULL, which takes it out of the block
      for (k in setdiff(names(rv$saved), names(d))) out[k] <- list(NULL)
      if (length(out)) out else stats::setNames(list(), character())
    }, error = function(e) {
      notify(paste0(t("A value is not YAML:"), " ", conditionMessage(e)), "error")
      FALSE
    })
    if (isFALSE(vals)) return()
    done <- tryCatch({
      .series_fn("tfl_series_settings_set")(dir, s, vals)
      TRUE
    }, error = fail)
    if (is.null(done)) return()
    load_block()
    notify(sprintf(t("Saved this study's values for %s (study.yml series_settings)."), s))
  })

  output$series_values <- shiny::renderUI({
    x <- chosen()
    if (is.null(x)) return(NULL)
    dir <- study_dir()
    changed <- !identical(rv$draft, rv$saved)
    shiny::tagList(
      shiny::h6(class = "mt-3 mb-1",
                with_tip(sprintf(t("Values of %s (values:)"), x$name),
                         t("The series' values as written (a tag as written: !series, !study, !fixed, !unset). A value changed for this study is kept in study.yml's series_settings; the templates read it first."))),
      if (length(x$path)) shiny::p(class = "small text-muted mb-1", shiny::tags$code(x$path)),
      if (is.null(dir)) {
        shiny::tagList(
          .series_values_ui(x$values, list(), list(), t),
          shiny::p(class = "small text-muted", t("Open a study to change its values.")))
      } else {
        shiny::tagList(
          .series_values_ui(x$values, rv$draft, rv$saved, t),
          shiny::div(
            class = "d-flex gap-2 align-items-center",
            .btn("series_save", t("Save this study's values (tfl_series_settings_set)"),
                 class = if (changed) "btn-sm btn-primary" else "btn-sm btn-outline-secondary"),
            if (changed) shiny::span(class = "small text-warning", t("unsaved"))))
      })
  })

  # spec/series.lock: its pins, the button that writes it, and what
  # tfl_series_check_lock() warns of
  shiny::observeEvent(input$series_pin, {
    dir <- study_dir()
    if (is.null(dir)) return()
    warns <- character()
    done <- tryCatch(withCallingHandlers({
      .series_fn("tfl_pin_series")(dir)
      TRUE
    }, warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = fail)
    for (w in warns) notify(w, "warning")
    rv$lock_ver <- rv$lock_ver + 1L
    if (isTRUE(done)) notify(t("Pinned the series in spec/series.lock."))
  })
  output$series_lock <- shiny::renderUI({
    rv$lock_ver
    dir <- study_dir()
    if (is.null(dir)) return(NULL)
    f <- file.path(dir, "spec", "series.lock")
    text <- lock <- NULL
    notes <- character()
    if (file.exists(f)) {
      text <- readLines(f, encoding = "UTF-8", warn = FALSE)
      lock <- tryCatch(yaml::read_yaml(f), error = function(e) {
        notes <<- c(notes, conditionMessage(e))
        NULL
      })
      tryCatch(withCallingHandlers(
        .series_fn("tfl_series_check_lock")(dir),
        warning = function(w) {
          notes <<- c(notes, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        # (a series read from another folder than the one pinned)
        message = function(m) {
          notes <<- c(notes, sub("\n$", "", conditionMessage(m)))
          invokeRestart("muffleMessage")
        }), error = function(e) notes <<- c(notes, conditionMessage(e)))
    }
    shiny::tagList(
      shiny::h6(class = "mt-3 mb-1",
                with_tip(t("Pins (spec/series.lock)"),
                         t("The series and templates this study was made with, by their files' md5. Written by tflspec (tfl_pin_series); a pinned file changed since is noted here."))),
      .series_lock_ui(lock, text, notes, t),
      shiny::div(class = "mt-1",
                 .btn("series_pin", t("Pin the series (tfl_pin_series)"),
                      class = "btn-sm btn-outline-primary")))
  })
  invisible(rv)
}
