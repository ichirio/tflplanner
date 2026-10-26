# app_server(): report list -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_report_list <- function() {
  # the list to choose a report from: searched, by section, by state
  # (the marks a run in the background changed come as run_marks(), put in
  # place: the list is not drawn again for them; `seq` says which is newer)
  mark_clock <- new.env()
  mark_clock$n <- 0
  picker_rows <- shiny::reactive({
    rv$ver
    rv$status_ver
    # what the list shows changed (not a table's cell, say)
    p_list()
    p <- shiny::isolate(rv$p)
    if (is.null(p) || !has_study()) return(.report_rows(new_planner()))
    # the ARD's state as the ARD step has it; the runs' from their files
    # (study_status() writes every program again: slow for a big study)
    d <- .report_rows(p, tryCatch(shiny::isolate(ard_state()), error = function(e) NULL),
                      tryCatch(.report_run_light(shiny::isolate(current_study())),
                               error = function(e) NULL))
    mark_clock$n <- mark_clock$n + 1
    attr(d, "seq") <- mark_clock$n
    d
  })
  run_marks <- shiny::reactiveVal(NULL)
  # -- the review (#288) -----------------------------------------------------
  # One review the whole app reads: the Review tab, the report head's
  # counts, the picker's numbers, the analyses' badge.  The light one --
  # the definition, the catalogs, the facts of the data kept from the last
  # read, no file read -- made when a study is opened, 2 s after edits
  # pause, after a save or an ARD made; "Review with the data" reads the
  # files whose facts are stale, "Check as the programs read it" adds the
  # deep check's rows (kept while the definition is the one checked).
  review_ver <- shiny::reactiveVal(0L)
  review_deep <- shiny::reactiveVal(NULL)
  review_p <- shiny::debounce(shiny::reactive(rv$p), 2000)
  # (not in the session's first flush: the page is drawn first, the
  # review's counts come a moment later -- #440, the first render)
  review_go <- shiny::reactiveVal(FALSE)
  session$onFlushed(function() if (!session$isClosed()) review_go(TRUE), once = TRUE)
  # (nor while a report is being opened: the last review is kept till it
  # is drawn -- tpl_mode_send())
  review_hold <- shiny::reactiveVal(FALSE)
  review_last <- new.env()
  shiny::observe({
    if (!review_hold()) return()
    shiny::invalidateLater(8000)
    if (!is.null(review_last$held) && difftime(Sys.time(), review_last$held, units = "secs") > 7) {
      review_last$held <- NULL
      review_hold(FALSE)
    } else if (is.null(review_last$held)) review_last$held <- Sys.time()
  })
  review_now <- shiny::reactive({
    if (!has_study() || !review_go()) return(NULL)
    if (review_hold()) return(review_last$r)
    review_p()
    review_ver()
    rv$status_ver
    rv$ard_ver
    study_open()
    s <- shiny::isolate(current_study())
    r <- tryCatch(study_review(s, data = "cached", ard = TRUE, lang = lang),
                  error = function(e) {
                    message("tflplanner: the review: ", conditionMessage(e))
                    NULL
                  })
    if (is.null(r)) return(NULL)
    dp <- review_deep()
    if (!is.null(dp) && identical(dp$p, s$planner) && nrow(dp$rows)) {
      keep <- attributes(r)
      keep <- keep[intersect(names(keep), c("facts_made", "facts_of", "off"))]
      r <- .review_bind(list(r, dp$rows))
      for (k in names(keep)) attr(r, k) <- keep[[k]]
    }
    review_last$r <- r
    r
  })
  session$userData$review_now <- review_now
  review_counts <- shiny::reactive({
    r <- review_now()
    if (is.null(r) || !nrow(r)) NULL else .review_counts(r)
  })
  # a row of the review, gone to: the report chosen, its page, the tabs,
  # the inputs (an analysis, an analysis data, a code list, a figure's
  # piece) and the grid's row flashed (R/review_ui.R)
  # (called from the Review tab's module: run in the app's own session, so
  # the tabs it chooses are the app's, not ids inside the module)
  review_jump <- function(row) shiny::withReactiveDomain(session, {
    id <- row$output_id
    if (!is.na(id) && id %in% rv$p$outputs$output_id) {
      shiny::updateSelectInput(session, "target", selected = id)
    }
    tg <- .review_target(row, rv$p)
    if (!is.null(tg$go)) go(tg$go)
    for (n in names(tg$nav)) bslib::nav_select(n, tg$nav[[n]])
    msg <- list(inputs = tg$inputs, grid = tg$grid, keycols = as.list(tg$keycols),
                key = tg$key, field = tg$field,
                row_index = if (is.na(tg$row_index)) NULL else tg$row_index)
    session$onFlushed(function() session$sendCustomMessage("review-focus", msg),
                      once = TRUE)
  })
  session$userData$review_jump <- review_jump
  # a figure's advice applied (its one-step fix)
  review_fix <- function(row) {
    fx <- row$fix[[1L]]
    id <- row$output_id
    d <- fig_design(rv$p, id)
    if (is.null(fx) || is.null(d)) return()
    d2 <- guarded(tflspec::tfl_fig_apply_fix(d, fx))
    if (is.null(d2)) return()
    p2 <- guarded(set_fig_design(rv$p, id, d2))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    notify(sprintf(t("%s: the advice applied (save to keep it)."), id))
  }
  review_mod <- review_server(
    "review", review = review_now, rows = picker_rows,
    jump = review_jump, apply_fix = review_fix,
    run_light = function() review_ver(review_ver() + 1L),
    run_data = function() {
      s <- current_study()
      n <- max(1L, nrow(rv$p$ard$datasets))
      shiny::withProgress(message = t("Reading the data for the review"), value = 0, {
        guarded(review_facts(s, progress = function(d)
          shiny::incProgress(1 / n, detail = d)))
      })
      review_ver(review_ver() + 1L)
    },
    run_deep = function() {
      p <- rv$p
      rows <- NULL
      shiny::withProgress(message = t("Reading the definition as the programs do"), {
        rows <- guarded(.rule_p01(p))
      })
      rows <- .review_language(rows %||% .review_bind(list()), lang)
      review_deep(list(p = p, rows = rows))
      if (!nrow(rows)) notify(t("The definition reads without errors")) else
        notify(t("The definition has errors (see the Review tab)"), "error")
      review_ver(review_ver() + 1L)
    },
    lang = lang)
  # the report head's counts: to the Review tab, that report and level
  shiny::observeEvent(input$review_goto, {
    g <- input$review_goto
    bslib::nav_select("nav", "review")
    review_mod$set_filter(g$id, g$level)
  })

  report_picker_server(
    "rp", picker_rows, now = shiny::reactive(input$target),
    pick = function(v) shiny::updateSelectInput(session, "target", selected = v),
    fixed = stats::setNames(list(t("Study defaults"), t("ALL (every row)")),
                            c(.default_rows, .all_rows)),
    lang = lang, folded = shiny::reactive(identical(input$side, FALSE)),
    counts = review_counts, marks = run_marks)
  # the report list's and the runs' search: DT's own, on a hidden column
  # of the normalized text (the rows keep their numbers)
  shiny::observeEvent(input$outputs_q, .report_dt_set_search(session, "outputs", input$outputs_q))
  shiny::observeEvent(input$status_q, .report_dt_set_search(session, "status", input$status_q))
  target <- shiny::reactive(.target_value(input$target))
  current_now <- shiny::reactive({
    # (a report the server has just chosen -- added, copied -- is the one
    # before the page says so: tgt_sent, #440)
    tg0 <- target()
    tg <- tgt_pick() %||% tg0
    if (!is.null(rv$p) && !is.na(tg) && nzchar(tg) &&
        tg %in% rv$p$outputs$output_id) tg else NULL
  })
  # The report chosen, passed on only when it changes.  It reads rv$p (is
  # the report still there?), which every edit writes: as a reactive, each
  # edit invalidated everything drawn for the report -- the table builder's
  # form among them, redrawn (closed, the text lost) while being typed in.
  current <- shiny::reactiveVal(NULL)
  shiny::observe(current(current_now()), priority = 100)
  # (another report chosen: a new table only drawn, nothing changed, is
  # put back as it was)
  shiny::observeEvent(current(), {
    if (is.null(held$p)) return()
    if (identical(rv$p, held$p)) rv$p <- held$before
    held$p <- held$before <- NULL
  }, ignoreInit = TRUE)

  # -- where the user is ---------------------------------------------------
  # A report's screens are the Reports tab's sub-tabs (the list, its
  # content by its kind, its page, its code); the logic knows them by the
  # pages they were: tables / lf / designer (content), report_spec (page).
  report_kind <- shiny::reactive({
    id <- current()
    if (is.null(id)) "" else report_info(rv$p, id)$type
  })
  output$report_kind <- shiny::renderText(report_kind())
  shiny::outputOptions(output, "report_kind", suspendWhenHidden = FALSE)
  page <- shiny::reactive({
    nav <- input$nav %||% ""
    if (!identical(nav, "make")) return(nav)
    switch(input$step %||% "ard",
      ard = "ard",
      content = if (identical(input$content_nav, "code")) "code" else
        switch(report_kind(), table = "tables", listing = "lf",
               figure = "designer", "outputs"),
      page = if (identical(input$page_right, "code")) "code" else "report_spec",
      "outputs")
  })
  # a page: its top tab, and for a report's, its step (and the step's tab)
  go <- function(where) {
    step <- switch(where, ard = "ard",
                   tables = , lf = , designer = , code = "content",
                   report_spec = "page", NULL)
    if (is.null(step)) return(bslib::nav_select("nav", where))
    # (a template report has no ARD step: its step 1 is the content's)
    if (identical(step, "ard") && !is.null(current()) && !is.null(.tpl_of(rv$p, current()))) {
      step <- "content"
    }
    bslib::nav_select("nav", "make")
    bslib::nav_select("step", step)
    if (identical(step, "content")) {
      bslib::nav_select("content_nav", if (identical(where, "code")) "code" else "content")
    }
    if (identical(step, "page")) bslib::nav_select("page_right", "spec")
  }
  # The report chosen in the list: where its kind is made, as buttons
  output$report_moves <- shiny::renderUI({
    id <- current()
    moves <- .type_moves[[report_kind()]]
    if (is.null(id) || !length(moves)) return(NULL)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center my-2",
      shiny::span(class = "small text-muted",
                  sprintf(t("%s (%s):"), id, t(.type_labels[[report_kind()]]))),
      lapply(names(moves), function(to) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary",
        onclick = sprintf("Shiny.setInputValue('report_move', '%s', {priority: 'event'})", to),
        t(moves[[to]]))))
  })
  shiny::observeEvent(input$report_move, go(input$report_move))
  # the steps' names: the kind's own (a figure's "1 Data", "2 Figure")
  shiny::observe({
    k <- report_kind()
    # (a template report's two steps: R/template_report.R)
    id <- current()
    tpl <- !is.null(id) && !is.null(.tpl_of(rv$p, id))
    session$sendCustomMessage("rp-step-names", tpl_names(tpl, k))
  })
  # the tabs this kind of report has nothing on, faded (still clickable)
  shiny::observe({
    k <- report_kind()
    idle <- .type_idle_tabs[[k]] %||% character()
    src <- if (identical(k, "figure") && !is.null(current())) .fig_ard_source(rv$p, current())
    if (!is.null(src) && src$kind != "none") idle <- setdiff(idle, "ard")
    session$sendCustomMessage("rp-idle-tabs", as.list(idle))
    # the ARD definition: a figure's only when it has its own analyses
    session$sendCustomMessage("rp-ard-def", !identical(k, "figure") ||
                                identical(src$kind, "own"))
  })
  output$ard_kind_note <- shiny::renderUI({
    k <- report_kind()
    if (identical(k, "figure") && !is.null(current())) return(fig_ard_ui(current()))
    # a report whose ARD was taken in: its analyses here are not used
    f <- if (!is.null(current())) .ard_import_of(rv$p, current())
    if (!is.null(f)) {
      own <- nrow(ard_rows(rv$p, "analyses", current())) > 0L
      imp_ver()
      log <- ard_imports(rv$study)
      out <- identical(log$state[match(f, log$file)], "removed")
      return(shiny::div(
        class = paste("alert py-1 small d-flex flex-wrap gap-2 align-items-center",
                      if (out) "alert-danger" else "alert-warning"),
        shiny::span(sprintf(if (out) t("%s uses an ARD that was taken out (%s): take it in again, or go back to its own ARD definition.") else
          t("%s uses an ARD taken in (%s): the analyses here are not used for it."),
                            current(), f)),
        if (own) .btn("imp_compare_cur", t("Compare"), class = "btn-sm btn-outline-secondary py-0")))
    }
    if (!k %in% names(.type_idle_tabs) || !"ard" %in% .type_idle_tabs[[k]]) {
      return(NULL)
    }
    shiny::div(class = "alert alert-info py-1 small",
               sprintf(t("%s is a %s: it has no ARD (its program reads its data). The ARD here is the tables'."),
                       current(), t(.type_labels[[k]])))
  })
  .srv_keep(environment())
}
