# app_server(): ard own -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_ard_own <- function() {

  # ---- ARD functions of one's own ------------------------------------------
  # The company's and the study's side by side (a study's copy wins); one
  # chosen: use it in the study, try it (in a separate R process, kept),
  # see its code (edited outside the app), compare it with the company's
  # and take the company's.  New ones from tflspec's templates.  Writing a
  # file (a copy, a new one) saves at once: the study key `source` and the
  # files must agree.
  own_ver <- shiny::reactiveVal(0L)
  own_data <- shiny::reactive({
    # the files are edited outside the app: read again when the tab is
    # opened, a function chosen, Try opened, or Refresh pressed
    own_ver()
    own_pick()
    input$ard_right
    input$own_refresh
    # the study's ARD definition (its `source`, its functions): not every
    # edit of the study
    p_ard()
    shiny::req(has_study())
    d <- tryCatch(own_ard_functions(shiny::isolate(imp_study())), error = function(e) {
      notify(conditionMessage(e), "error")
      NULL
    })
    # the company's first, then the study's copies, then the study's own
    if (!is.null(d) && nrow(d)) {
      d <- d[order(match(d$where, names(own_where_words))), , drop = FALSE]
      rownames(d) <- NULL
    }
    d
  })
  own_where_words <- c(company = "company (every study)", both = "this study's copy of the company's",
                       study = "this study only")
  output$own_counts <- shiny::renderUI({
    d <- own_data()
    shiny::req(d)
    n <- function(w) sum(d$where %in% w)
    shiny::p(class = "small mb-1",
             shiny::span(class = "badge text-bg-secondary me-1", n(c("company", "both"))),
             t("company's (every study)"),
             shiny::span(class = "badge text-bg-primary ms-3 me-1", n(c("study", "both"))),
             t("this study's"))
  })
  output$own_list <- DT::renderDT({
    d <- own_data()
    shiny::req(d)
    where <- t(unname(own_where_words[d$where]))
    star <- ifelse(d$where == "both" & d$differs %in% TRUE, " \u2605", "")
    loaded <- ifelse(d$loaded, t("loaded"), t("not loaded"))
    tried <- ifelse(is.na(d$tried), "",
                    paste0(d$tried, " (", ifelse(is.na(d$problems), t("could not be tried"),
                                                 sprintf(t("%d problems"), d$problems)), ")",
                           ifelse(d$stale, paste0(" ", t("-- the file changed since")), "")))
    v <- data.frame(d$name, paste0(where, star), loaded,
                    ifelse(is.na(d$used_by), "", d$used_by), tried,
                    stringsAsFactors = FALSE)
    names(v) <- t(c("Function", "Where", "In this study", "Used by", "Tried"))
    # the function chosen stays chosen when the list is drawn again
    k <- match(shiny::isolate(own_pick()) %||% "", d$name)
    .dt(v, selection = list(mode = "single", selected = if (!is.na(k)) k),
        language = list(emptyTable = t("No ARD function of one's own yet: New function... starts one from a template.")))
  })
  own_pick <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$own_list_rows_selected, {
    d <- own_data()
    i <- input$own_list_rows_selected
    if (!is.null(d) && length(i) && i <= nrow(d)) own_pick(d$name[i])
  }, ignoreNULL = FALSE)
  own_sel <- shiny::reactive({
    d <- own_data()
    k <- match(own_pick() %||% "", d$name)
    if (is.null(d) || is.na(k)) NULL else d[k, , drop = FALSE]
  })
  output$own_detail <- shiny::renderUI({
    r <- own_sel()
    if (is.null(r)) return(shiny::p(class = "small text-muted mt-2", t("Choose a function in the list.")))
    file_line <- function(lab, f) if (!is.na(f)) shiny::div(class = "small", shiny::strong(lab), " ", shiny::code(f))
    shiny::div(
      class = "rp-b-card mt-2",
      shiny::h6(r$name, if (!is.na(r$title)) shiny::span(class = "fw-normal", paste0(" \u2014 ", r$title))),
      if (!is.na(r$description)) shiny::p(class = "small", r$description),
      shiny::p(class = "small text-muted",
               t("The title and description are the first lines of the comment (#') above the function in its file.")),
      file_line(t("The study's file:"), r$study_file),
      file_line(t("The company's file:"), r$company_file),
      shiny::div(class = "small", shiny::strong(t("Keywords:")), " ",
                 if (is.na(r$keywords %||% NA)) shiny::span(
                   class = "text-muted",
                   t("none: the words the ARD form's search finds it by go in a comment line right above the function, # tflplanner-keywords: odds ratio, PROC LOGISTIC"))
                 else r$keywords),
      shiny::div(class = "small", shiny::strong(t("Statistics:")), " ",
                 if (is.na(r$stat_names)) t("not declared (cards::as_cards_fn(stat_names = ) lets a check see them)")
                 else gsub(" | ", ", ", r$stat_names, fixed = TRUE)),
      if (r$where == "both" && isTRUE(r$differs)) shiny::div(
        class = "alert alert-warning py-1 small mt-2",
        t(switch(r$newer,
          company = "The company's file changed since the study copied it; the study uses its copy.",
          study = "The study's copy was changed in the study; the company's is as it was copied.",
          both = "Both changed since the study copied it: the study's copy and the company's file.",
          "The study's copy and the company's file differ (no record of when it was copied).")),
        shiny::div(class = "d-flex gap-2 mt-1",
                   .btn("own_diff", t("See the difference"), class = "btn-sm btn-outline-secondary py-0"),
                   if (!identical(r$newer, "study"))
                     .btn("own_replace", t("Take the company's..."), class = "btn-sm btn-outline-danger py-0"))),
      if (!is.na(r$tried) && isTRUE(r$stale)) shiny::p(
        class = "small text-muted mt-1", t("The file changed since it was last tried: try it again.")),
      shiny::div(
        class = "d-flex flex-wrap gap-2 mt-2",
        if (!r$loaded) .btn("own_use", t("Use in this study"), class = "btn-sm btn-primary"),
        .btn("own_try", t("Try..."), class = "btn-sm btn-outline-primary"),
        .btn("own_code", t("See the code"), class = "btn-sm btn-outline-secondary")))
  })
  # use the company's in the study: copied, loaded, saved at once
  shiny::observeEvent(input$own_use, {
    r <- own_sel()
    shiny::req(r)
    s <- imp_study()
    s <- if (r$where == "company") guarded(use_company_ard_function(s, r$name)) else
      .add_source(s, r$study_file)
    if (is.null(s)) return()
    rv$p <- s$planner
    do_save()
    own_ver(own_ver() + 1L)
    notify(sprintf(t("%s is loaded by the study's ARD programs: an analysis can name it as its method. Every report's ARD is outdated now (the files the programs load are part of it)."), r$name))
  })
  own_file_of <- function(r) {
    if (!is.na(r$study_file)) file.path(rv$study$path, r$study_file) else r$company_file
  }
  shiny::observeEvent(input$own_code, {
    r <- own_sel()
    shiny::req(r)
    f <- own_file_of(r)
    shiny::showModal(shiny::modalDialog(
      title = r$name, size = "l", easyClose = TRUE,
      shiny::p(class = "small", shiny::code(f)),
      shiny::p(class = "small text-muted",
               t("Edit it outside the app (RStudio ...), then try it here.")),
      shiny::div(class = "rp-code", shiny::tags$pre(paste(readLines(f, warn = FALSE), collapse = "\n"))),
      footer = shiny::modalButton(t("Close"))))
  })
  shiny::observeEvent(input$own_diff, {
    r <- own_sel()
    shiny::req(r)
    sl <- readLines(file.path(rv$study$path, r$study_file), warn = FALSE)
    cl <- readLines(r$company_file, warn = FALSE)
    # a line the other file has not: marked
    pane <- function(lab, lines, other) shiny::div(
      shiny::h6(lab), shiny::div(class = "rp-code", shiny::tags$pre(
        style = "max-height: 60vh; overflow: auto;",
        lapply(lines, function(l) shiny::tags$div(
          style = if (!l %in% other) "background: #fef3c7;", if (nzchar(l)) l else " ")))))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("%s: the study's and the company's"), r$name), size = "xl", easyClose = TRUE,
      shiny::p(class = "small text-muted", t("Marked: a line the other file has not.")),
      bslib::layout_columns(col_widths = c(6, 6),
                            pane(t("The study's"), sl, cl),
                            pane(t("The company's"), cl, sl)),
      footer = shiny::modalButton(t("Close"))))
  })
  shiny::observeEvent(input$own_replace, {
    r <- own_sel()
    shiny::req(r)
    shiny::showModal(shiny::modalDialog(
      title = t("Take the company's"), easyClose = TRUE,
      shiny::p(sprintf(t("The study's %s is replaced by the company's. The study's results change when its ARD is made again."),
                       r$study_file)),
      if (r$newer %in% c("both", "unknown")) shiny::div(
        class = "alert alert-danger py-1 small",
        t("What was changed in the study's copy is lost.")),
      if (!is.na(r$used_by)) shiny::p(
        class = "small", sprintf(t("The analyses that use it (their reports' ARDs become outdated): %s"),
                                 gsub(" | ", ", ", r$used_by, fixed = TRUE))),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("own_replace_ok", t("Replace"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$own_replace_ok, {
    r <- own_sel()
    shiny::req(r)
    if (is.null(guarded(take_company_ard_function(imp_study(), r$name)))) return()
    shiny::removeModal()
    own_ver(own_ver() + 1L)
    notify(sprintf(t("%s: the company's file is the study's now."), r$name))
  })
  # trying one: on the data an analysis that uses it reads, with its
  # arguments; else cards' example data
  shiny::observeEvent(input$own_try, {
    own_ver(own_ver() + 1L)
    r <- own_sel()
    shiny::req(r)
    a <- rv$p$ard$analyses
    u <- a[!is.na(a$method) & a$method == r$name, , drop = FALSE]
    data_now <- if (nrow(u)) .an_data_value(u$dataset[1L], u$population_id[1L]) else ""
    args_now <- if (nrow(u)) {
      paste(c(if (!.is_blank(u$by[1L])) paste0("by = ", .vars(u$by[1L])),
              if (!.is_blank(u$variables[1L])) paste0("variables = ", .vars(u$variables[1L])),
              if (!.is_blank(u$args[1L])) u$args[1L]), collapse = ", ")
    } else "by = ARM, variables = AGE"
    dch <- .an_data_choices(rv$p$ard$datasets$dataset, rv$p$ard$populations, now = data_now,
                             words = data_words_list(), counts = data_counts())
    ch <- c(stats::setNames("__cards__", t("cards' example data (cards::ADSL)")), dch[dch != ""])
    if (!nzchar(data_now)) data_now <- "__cards__"
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Try %s"), r$name), size = "l", easyClose = TRUE,
      shiny::selectInput("own_try_data", t("Data"), ch, data_now, width = "100%"),
      shiny::textInput("own_try_args", t("Its arguments (as an analysis row gives them)"),
                       args_now, width = "100%"),
      shiny::p(class = "small text-muted",
               t("Run in a separate R process with the files the study's ARD programs load; up to 60 seconds.")),
      shiny::uiOutput("own_try_result"),
      footer = shiny::tagList(shiny::modalButton(t("Close")),
                              .btn("own_try_go", t("Try"), class = "btn-primary"))))
    output$own_try_result <- shiny::renderUI(NULL)
  })
  shiny::observeEvent(input$own_try_go, {
    r <- own_sel()
    shiny::req(r)
    dv <- input$own_try_data %||% "__cards__"
    sp <- if (nzchar(dv) && dv != "__cards__") .an_data_split(dv) else
      list(dataset = NA_character_, pop = NA_character_)
    res <- shiny::withProgress(
      message = sprintf(t("Trying %s (up to 60 seconds) ..."), r$name), value = 0.3,
      guarded(try_ard_function(imp_study(), r$name, dataset = sp$dataset,
                               population_id = sp$pop,
                               args = input$own_try_args %||% "")))
    own_ver(own_ver() + 1L)
    if (is.null(res)) return()
    p <- .check_view(res$problems, t)
    output$own_try_result <- shiny::renderUI(shiny::tagList(
      if (!is.null(res$error)) shiny::div(class = "alert alert-danger py-1 small", own_stop_words(res$error))
      else if (!is.null(p) && nrow(p)) shiny::tags$table(
        class = "table table-sm small",
        shiny::tags$tbody(lapply(seq_len(nrow(p)), function(i) shiny::tags$tr(
          shiny::tags$td(class = switch(p$level[i], error = "text-danger", warning = "text-warning", ""),
                         t(p$level[i])),
          shiny::tags$td(p$message[i])))))
      else shiny::div(class = "alert alert-success py-1 small",
                      sprintf(t("It behaves: a cards ARD with the statistics it declares (%.1f s)."), res$seconds)),
      if (!is.null(res$ard) && nrow(res$ard)) shiny::div(
        class = "small", shiny::strong(t("The first rows of its ARD")),
        shiny::div(style = "max-height: 40vh; overflow: auto;",
                   shiny::tableOutput("own_try_ard")))))
    output$own_try_ard <- shiny::renderTable(ard_view(res$ard))
  })
  # where a try stopped, said: loading the files, making the data, or in
  # the function
  own_stop_words <- function(e) {
    at <- regmatches(e, regexpr("^\\[(files|data|function)\\]", e))
    msg <- sub("^\\[(files|data|function)\\] ", "", e)
    if (!length(at)) return(e)
    paste(t(switch(at, "[files]" = "Stopped while loading the study's files:",
                   "[data]" = "Stopped while making the data:",
                   "Stopped in the function:")), msg)
  }
  # a new one, from a template
  own_type_words <- c(
    summary = "Statistics of one's own on numeric variables, by group (cards::ard_summary(statistic = ))",
    test = "A test or a model made an ARD (cards::tidy_as_ard(): its errors go into the ARD)",
    free = "Any calculation, group by group (cards::ard_strata() + ard_identity())")
  shiny::observeEvent(input$own_new, {
    shiny::req(has_study())
    can_company <- .company_writable()
    shiny::showModal(shiny::modalDialog(
      title = t("New ARD function"), easyClose = TRUE,
      shiny::textInput("own_new_name", t("Its name (starts with ard_)"), "ard_", width = "100%"),
      shiny::radioButtons("own_new_type", t("Kind"),
                          stats::setNames(names(own_type_words), t(unname(own_type_words))),
                          width = "100%"),
      if (can_company) shiny::radioButtons(
        "own_new_where", t("Where"),
        stats::setNames(c("study", "company"),
                        c(t("This study (programs/ard/functions/; loaded at once)"),
                          t("The company standards (a study uses it with Use in this study)"))),
        width = "100%"),
      if (!can_company) shiny::p(class = "small text-muted",
                                 sprintf(t("It is written for this study: the company standards' folder (%s) cannot be written from here."),
                                         .own_fun_dir())),
      shiny::checkboxInput("own_new_test", t("Write its test (test-<name>.R) next to it"), TRUE),
      shiny::p(class = "small text-muted",
               t("The skeleton is written to be edited outside the app (RStudio ...); try it afterwards.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("own_new_ok", t("Write it"), class = "btn-primary"))))
  })
  # the folder a function was written to, opened in the file manager
  own_last_dir <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$own_open_dir, {
    d <- own_last_dir()
    if (!is.null(d) && dir.exists(d)) utils::browseURL(normalizePath(d))
  })
  shiny::observeEvent(input$own_new_ok, {
    nm <- trimws(input$own_new_name %||% "")
    where <- input$own_new_where %||% "study"
    s <- guarded(new_ard_function(imp_study(), nm, type = input$own_new_type %||% "summary",
                                  where = where, test = isTRUE(input$own_new_test)))
    if (is.null(s)) return()
    shiny::removeModal()
    if (where == "study") {
      rv$p <- s$planner
      do_save()
    }
    own_ver(own_ver() + 1L)
    dir <- if (where == "study") file.path(rv$study$path, .study_fun_dir) else .own_fun_dir()
    own_last_dir(dir)
    shiny::showNotification(
      shiny::tagList(sprintf(t("%s is written: edit it outside the app, then try it."), nm),
                     shiny::br(), shiny::code(file.path(dir, paste0(nm, ".R"))), shiny::br(),
                     .btn("own_open_dir", t("Open the folder"), class = "btn-sm btn-link p-0")),
      duration = 15)
  })

  # ---- ARDs taken in -------------------------------------------------------
  # Taking in, using, stopping, replacing and taking out write the record
  # (input/ard/imports.csv) at once; so each one is saved at once too --
  # the reports' ard_source and their programs -- or the record and what
  # the reports read would part (S1's review of #90).
  imp_ver <- shiny::reactiveVal(0L)
  imp_mode <- shiny::reactiveVal(list(mode = "new"))
  imp_n <- shiny::reactiveVal(0L)   # each dialog's own inputs
  imp_id <- function(x) paste0("imp", imp_n(), "_", x)
  # the study as the import functions read it: with the definition on screen
  imp_study <- function() {
    s <- rv$study
    s$planner <- rv$p
    s
  }
  # save the reports touched, and their programs (the other unsaved changes
  # go with them: said so)
  imp_save <- function(ids = character()) {
    if (isTRUE(shiny::isolate(dirty()))) {
      notify(t("Saved at once, with the other unsaved changes: the record of ARDs taken in and the reports must agree."))
    }
    do_save()
  }
  imp_data <- shiny::reactive({
    imp_ver()
    rv$p
    shiny::req(has_study())
    .imports_view(ard_imports(rv$study), rv$p$sheets$report)
  })
  output$imp_list <- DT::renderDT({
    d <- imp_data()
    state <- ifelse(d$state %in% "removed", t("taken out"), t("active"))
    note <- .imports_notes(d, list(removed_used = t("Taken out, but a report still uses it"),
                                   error = t("Cannot be used: its check found errors")))
    v <- data.frame(d$import_id, d$file, d$source, d$imported, d$outputs,
                    d$rows, d$check, state, d$used_by, note, stringsAsFactors = FALSE)
    names(v) <- t(c("ID", "File", "Source", "Taken in", "Reports", "Rows",
                    "Check", "State", "Used by", "Note"))
    DT::formatStyle(
      .dt(v, selection = "single", language = list(emptyTable = t("No ARD taken in yet."))),
      names(v)[10L], color = "#b91c1c")
  })
  imp_sel <- shiny::reactive({
    d <- imp_data()
    i <- input$imp_list_rows_selected
    if (is.null(i) || !length(i) || i > nrow(d)) NULL else d[i, , drop = FALSE]
  })
  # the actions on a chosen ARD: not pressable until one is chosen
  output$imp_actions <- shiny::renderUI({
    r <- imp_sel()
    off <- if (is.null(r)) list(disabled = TRUE) else list()
    b <- function(id, label, class) do.call(.btn, c(list(id, label, class = class), off))
    shiny::div(
      class = "d-flex flex-wrap gap-2 mt-2",
      b("imp_use", t("Use it for the report chosen (Make a report)"), "btn-sm btn-outline-primary"),
      b("imp_stop", t("Stop using it"), "btn-sm btn-outline-secondary"),
      b("imp_compare", t("Compare with the report's own ARD"), "btn-sm btn-outline-secondary"),
      b("imp_replace", t("Replace..."), "btn-sm btn-outline-secondary"),
      b("imp_remove", t("Take out"), "btn-sm btn-outline-danger"),
      if (is.null(r)) shiny::span(class = "small text-muted align-self-center",
                                  t("Choose an ARD in the list.")))
  })
  imp_row <- function() {
    r <- imp_sel()
    if (is.null(r)) notify(t("Choose an ARD in the list first."), "warning")
    r
  }
  report_ids <- function() {
    o <- rv$p$outputs
    o$output_id[!is.na(o$output_id)]
  }
  # taking in (new, or replacing one): a fresh dialog each time
  imp_modal <- function(title, outs = character(), source = "", replacing = FALSE) {
    imp_n(imp_n() + 1L)
    imp_done(FALSE)
    shiny::showModal(shiny::modalDialog(
      title = title, size = "l",
      shiny::fileInput(imp_id("file"), t("ARD file (rds, json, yaml, xpt, csv)"),
                       accept = c(".rds", ".json", ".yaml", ".yml", ".xpt", ".csv")),
      shiny::uiOutput(imp_id("summary")),
      shiny::textInput(imp_id("source"), t("Made by (a CRO, a program)"),
                       value = source, width = "100%"),
      shiny::selectizeInput(imp_id("outputs"), t("Reports it is for"), report_ids(),
                            selected = outs, multiple = TRUE, width = "100%"),
      if (!replacing) shiny::checkboxInput(imp_id("use_it"), t("Use it for these reports"), TRUE),
      shiny::uiOutput(imp_id("result")),
      footer = shiny::tagList(shiny::modalButton(t("Close")),
                              shiny::uiOutput(imp_id("do_btn"), inline = TRUE))))
    n <- imp_n()
    output[[paste0("imp", n, "_summary")]] <- shiny::renderUI(imp_summary_ui())
    output[[paste0("imp", n, "_result")]] <- shiny::renderUI(NULL)
    output[[paste0("imp", n, "_do_btn")]] <- shiny::renderUI(
      if (!imp_done()) .btn(paste0("imp", n, "_do"),
                            if (replacing) t("Replace") else t("Take it in"),
                            class = "btn-primary"))
  }
  imp_done <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$imp_new, {
    imp_mode(list(mode = "new"))
    imp_modal(t("Take in an ARD"))
  })
  shiny::observeEvent(input$imp_replace, {
    r <- imp_row()
    shiny::req(r)
    o <- trimws(strsplit(r$outputs %||% "", "|", fixed = TRUE)[[1L]])
    imp_mode(list(mode = "replace", id = r$import_id, file = r$file))
    imp_modal(sprintf(t("Replace %s (%s)"), r$import_id, r$file), o[nzchar(o)],
              source = if (is.na(r$source)) "" else r$source, replacing = TRUE)
  })
  imp_path <- function() {
    f <- input[[imp_id("file")]]
    if (is.null(f)) return(NULL)
    # the file under the name it was chosen by (the record keeps that name)
    d <- tempfile("imp")
    dir.create(d)
    p <- file.path(d, f$name)
    file.copy(f$datapath, p, overwrite = TRUE)
    p
  }
  imp_read <- shiny::reactive({
    path <- imp_path()
    shiny::req(path)
    tryCatch(tflspec::tfl_read_ard(path), error = function(e) e)
  })
  # what the file holds, as soon as it is chosen; a file with no output_id
  # column names no report (the field is emptied: choose them)
  imp_summary_ui <- function() {
    a <- imp_read()
    if (inherits(a, "error")) {
      return(shiny::div(class = "alert alert-danger py-1 small",
                        sprintf(t("The file cannot be read as an ARD: %s"), conditionMessage(a))))
    }
    outs <- if ("output_id" %in% names(a)) unique(stats::na.omit(as.character(a$output_id)))
    if (!identical(imp_mode()$mode, "replace")) {
      shiny::updateSelectizeInput(session, imp_id("outputs"),
                                  selected = intersect(outs, report_ids()))
    }
    shiny::p(class = "small text-muted",
             sprintf(t("%d rows; reports in its output_id column: %s"), nrow(a),
                     if (length(outs)) paste(outs, collapse = ", ") else t("(none: choose them below)")))
  }
  shiny::observeEvent(input[[imp_id("do")]], {
    shiny::req(!imp_done())
    path <- imp_path()
    if (is.null(path)) return(notify(t("Choose the ARD file."), "warning"))
    outs <- input[[imp_id("outputs")]]
    if (!length(outs)) return(notify(t("Choose the reports it is for."), "warning"))
    src <- trimws(input[[imp_id("source")]] %||% "")
    if (!nzchar(src)) notify(t("Who made it is blank: the record will not say."), "warning")
    src <- if (nzchar(src)) src else NA_character_
    m <- imp_mode()
    res <- guarded(if (identical(m$mode, "replace")) {
      used <- .imports_used_by(rv$p$sheets$report, m$file)
      c(replace_imported_ard(imp_study(), m$id, path, source = src,
                             name = basename(path), output_id = outs),
        list(moved = used))
    } else {
      list(row = import_ard(imp_study(), path, output_id = outs,
                            source = src, name = basename(path)))
    })
    if (is.null(res)) return()
    # the record names the file chosen, not the upload's temporary place
    .imports_set_original(rv$study, res$row$import_id, basename(path))
    probs <- attr(res$row, "check")
    bad <- !is.null(probs) && nrow(probs) && any(probs$level == "error")
    touched <- character()
    if (!is.null(res$planner)) {
      rv$p <- res$planner
      touched <- res$moved
    }
    if (!identical(m$mode, "replace") && isTRUE(input[[imp_id("use_it")]]) && !bad) {
      for (id in outs) rv$p <- use_imported_ard(rv$p, id, res$row$file)
      touched <- outs
    }
    if (length(touched)) imp_save(touched)
    imp_done(TRUE)
    imp_ver(imp_ver() + 1L)
    msg <- if (identical(m$mode, "replace")) {
      sprintf(t("%s is replaced by %s (%s)%s."), m$id, res$row$import_id, res$row$file,
              if (length(touched)) sprintf(t("; %s now use it"), paste(touched, collapse = ", ")) else "")
    } else if (bad) {
      sprintf(t("%s is taken in as %s, but its check found errors: no report uses it yet."),
              basename(path), res$row$file)
    } else {
      sprintf(t("%s is taken in as %s (check: %s)."), basename(path), res$row$file, res$row$check)
    }
    output[[imp_id("result")]] <- shiny::renderUI(shiny::tagList(
      shiny::div(class = if (bad) "alert alert-danger py-1 small" else "alert alert-success py-1 small",
                 msg),
      if (!is.null(probs) && nrow(probs)) {
        # the check's messages in the session's language (S1's .check_view())
        pv <- .check_view(probs, t)
        shiny::tags$table(
          class = "table table-sm small",
          shiny::tags$tbody(lapply(seq_len(nrow(pv)), function(i) shiny::tags$tr(
            shiny::tags$td(pv$level[i]),
            shiny::tags$td(if ("output_id" %in% names(pv)) pv$output_id[i] else ""),
            shiny::tags$td(pv$message[i] %||% "")))))
      }))
  })
  shiny::observeEvent(input$imp_use, {
    r <- imp_row()
    shiny::req(r, !is.null(current()))
    if (identical(r$state, "removed")) {
      return(notify(t("This ARD was taken out: take it in again to use it."), "warning"))
    }
    rv$p <- use_imported_ard(rv$p, current(), r$file)
    imp_save(current())
    imp_ver(imp_ver() + 1L)
    notify(sprintf(t("%s now uses %s."), current(), r$file))
  })
  shiny::observeEvent(input$imp_stop, {
    r <- imp_row()
    shiny::req(r)
    used <- .imports_used_by(rv$p$sheets$report, r$file)
    if (!length(used)) return(notify(t("No report uses it."), "message"))
    shiny::showModal(shiny::modalDialog(
      title = t("Stop using it?"),
      sprintf(t("%s go back to their own ARD definition."), paste(used, collapse = ", ")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("imp_stop_ok", t("Stop using it"), class = "btn-primary"))))
  })
  shiny::observeEvent(input$imp_stop_ok, {
    shiny::removeModal()
    r <- imp_row()
    shiny::req(r)
    used <- .imports_used_by(rv$p$sheets$report, r$file)
    for (id in used) rv$p <- use_imported_ard(rv$p, id, NULL)
    imp_save(used)
    imp_ver(imp_ver() + 1L)
  })
  shiny::observeEvent(input$imp_remove, {
    r <- imp_row()
    shiny::req(r)
    used <- .imports_used_by(rv$p$sheets$report, r$file)
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Take out %s?"), r$import_id),
      t("The file and its record stay in input/ard/ (marked taken out), so what a report was made from is not forgotten."),
      if (length(used)) shiny::tagList(
        shiny::p(class = "mt-2 text-warning",
                 sprintf(t("Used by %s."), paste(used, collapse = ", "))),
        shiny::checkboxInput("imp_remove_back", t("Set them back to their own ARD definition"), TRUE)),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("imp_remove_ok", t("Take out"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$imp_remove_ok, {
    shiny::removeModal()
    r <- imp_row()
    shiny::req(r)
    if (is.null(guarded(remove_imported_ard(rv$study, r$import_id)))) return()
    used <- .imports_used_by(rv$p$sheets$report, r$file)
    if (isTRUE(input$imp_remove_back) && length(used)) {
      for (id in used) rv$p <- use_imported_ard(rv$p, id, NULL)
      imp_save(used)
    }
    imp_ver(imp_ver() + 1L)
  })
  .srv_keep(environment())
}
