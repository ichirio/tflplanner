# app_server(): data run -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_data_run <- function() {
  # the data files the catalog (ARD tab > datasets) does not have yet go in
  # (nobody edited anything: a study that was saved is saved again)
  to_catalog <- function() {
    if (!has_study()) return()
    was_clean <- !isTRUE(dirty())
    p2 <- guarded(catalog_add_files(rv$p, data_files()))
    added <- attr(p2, "added")
    if (!length(added)) return()
    attr(p2, "added") <- NULL
    rv$p <- p2
    bump()
    if (was_clean) do_save()
    notify(sprintf(t("In the data catalog now: %s"), paste(added, collapse = ", ")))
  }
  shiny::observeEvent(input$data_open, .open_folder(
    file.path(rv$study$path, study_layout()[[data_pill()]])))
  # the Data tab opened: the files that have no row get one (once a study)
  data_seen <- shiny::reactiveVal("")
  shiny::observeEvent(list(input$nav, study_open()), {
    if (!identical(input$nav, "data") || !has_study()) return()
    id <- rv$study$meta$study_id
    if (identical(data_seen(), id)) return()
    data_seen(id)
    to_catalog()
  })
  data_head <- shiny::reactive({
    # a row selection can outlive the list it was made in (the list is
    # drawn again when the study or the folder changes): read only a file
    # that is there
    r <- input$hot_ard_datasets_select$select$r
    shiny::req(length(r) == 1L)
    d <- ds_sub()
    shiny::req(r >= 1L, r <= nrow(d), !is.na(d$path[r]))
    path <- .data_file(rv$study$path, d$path[r])
    shiny::req(file.exists(path))
    guarded(read_data_head(path, n = Inf))
  })
  output$data_head <- DT::renderDT({
    d <- data_head()
    shiny::req(d)
    DT::datatable(d, rownames = FALSE, selection = "none", class = "compact nowrap",
                  options = list(dom = "tip", pageLength = 50L, scrollX = TRUE,
                                 scrollY = "420px"))
  })
  output$data_dim <- shiny::renderUI({
    # nothing chosen yet: say how to see one (a blank card looked broken)
    if (length(input$hot_ard_datasets_select$select$r) != 1L) {
      return(shiny::p(class = "small text-muted",
                      t("Click a dataset in the list to see its first rows.")))
    }
    d <- data_head()
    shiny::req(d)
    full <- attr(d, "dim_full")
    shiny::p(class = "small text-muted",
             sprintf(t("%s rows x %s columns"),
                     format(full[1L], big.mark = ","), full[2L]))
  })

  # -- results -------------------------------------------------------------
  status <- shiny::reactive({
    rv$status_ver
    runs_ver()
    input$status_refresh
    shiny::req(has_study())
    study_status(current_study())
  })
  output$status_loading_txt <- shiny::renderText({
    shiny::req(has_study())
    sprintf(t("Reading the state of %d reports..."), nrow(rv$p$outputs))
  })
  output$status <- DT::renderDT({
    d <- status()
    word <- .run_state[d$status]
    word[is.na(word)] <- "not built"
    v <- data.frame(
      a = d$output_id, b = t(unname(.type_labels[d$type])),
      c = t(unname(.ard_state_labels[word])),
      d = ifelse(word == "built" & !is.na(d$rtf), d$rtf, ""),
      e = ifelse(word == "built", "", paste0(t(unname(.status_labels[d$status])),
                                             .why_text(d$why %||% rep("", nrow(d)), t))),
      f = paste0(d$program, ifelse(d$program_state %in% "edited",
                                   paste0(" (", t("edited by hand"), ")"), "")),
      stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Type", "State", "When", "Why", "Program"))
    v$.key <- .report_search_keys(shiny::isolate(picker_rows()), d$output_id)
    DT::formatStyle(
      .dt(v, selection = "multiple",
          columnDefs = list(list(visible = FALSE, targets = ncol(v) - 1L)),
          search = list(search = .report_dt_search(shiny::isolate(input$status_q)))),
      names(v)[3L],
      color = DT::styleEqual(t(unname(.ard_state_labels)),
                             c("#15803d", "#b45309", "#6b7280", "#b91c1c")))
  })
  selected_status <- shiny::reactive({
    d <- status()
    i <- input$status_rows_selected
    # no row chosen: the report chosen in the sidebar
    if (!length(i) && !is.null(current())) i <- match(current(), d$output_id)
    d[stats::na.omit(i), , drop = FALSE]
  })
  output$log <- shiny::renderText({
    bi <- input$batches_rows_selected
    if (length(bi)) {
      b <- batches()[bi, , drop = FALSE]
      info <- file.path(b$path, "batch.txt")
      run <- file.path(b$path, "run.csv")
      r <- if (file.exists(run)) utils::read.csv(run, colClasses = "character")
      return(paste(c(
        if (file.exists(info)) readLines(info, warn = FALSE, encoding = "UTF-8"),
        "",
        if (!is.null(r)) utils::capture.output(print(
          r[c("part", "program", "status", "errors", "warnings", "seconds",
              "note")], right = FALSE, row.names = FALSE))),
        collapse = "\n"))
    }
    d <- selected_status()
    rv$status_ver
    if (!nrow(d)) {
      return(t("Choose a report to see what its last preview printed, or a run folder to see its run."))
    } else {
      f <- d$log[1L]
      if (is.na(f)) return(sprintf(t("%s has not been run yet."),
                                   d$output_id[1L]))
    }
    paste(utils::tail(readLines(f, warn = FALSE, encoding = "UTF-8"), 300L),
          collapse = "\n")
  })
  output$rtf_download <- shiny::downloadHandler(
    filename = function() {
      d <- selected_status()
      if (nrow(d)) basename(d$rtf_path[1L]) else "none.rtf"
    },
    content = function(file) {
      d <- selected_status()
      if (!nrow(d) || !file.exists(d$rtf_path[1L])) stop("No RTF")
      file.copy(d$rtf_path[1L], file)
    })
  shiny::observeEvent(input$tfl_open, .open_folder(
    file.path(rv$study$path, study_layout()[["tfl"]])))

  start_run <- function(ids, what) {
    if (!is.null(rv$job)) return(notify(t("A run is going on"), "warning"))
    if (dirty() && !do_save()) return()
    px <- guarded(run_study(rv$study, ids, wait = FALSE))
    if (is.null(px)) return()
    rv$job <- px
    rv$job_kind <- "preview"
    rv$job_what <- what
    rv$job_study <- rv$study$meta$study_id
  }
  # a new copy of the sample: its official run (the ARD and the reports)
  # in the background, as the Runs tab starts one; the app stays usable and
  # the study list says it is running
  start_sample_run <- function(s) {
    miss <- .sample_missing()
    if (length(miss)) {
      return(notify(sprintf(
        t("The sample study is in place; to make its ARD and reports, install %s and start an official run (Runs)."),
        paste(miss, collapse = ", ")), "warning"))
    }
    if (!is.null(rv$job)) return(notify(t("A run is going on"), "warning"))
    px <- guarded(run_batch(s, c("ard", "tfl"), wait = FALSE))
    if (is.null(px)) return()
    rv$job <- px
    rv$job_kind <- "official"
    rv$job_what <- sprintf(t("the official run of %s (its ARD and reports)"),
                           s$meta$study_id)
    rv$job_study <- s$meta$study_id
    notify(sprintf(t("%s is copied; its ARD and reports are being made in the background (a few minutes). You can use the app meanwhile."),
                   s$meta$study_id))
  }
  shiny::observeEvent(input$run_all, start_run(NULL, t("every report")))
  start_batch <- function(parts, code, exclude = NULL, set = NULL, data = FALSE) {
    if (!is.null(rv$job)) return(notify(t("A run is going on"), "warning"))
    if (dirty() && !do_save()) return()
    parts <- if (identical(parts, "all")) c("ard", "tfl") else parts
    # the runner that can leave reports out (a study saved by an earlier
    # tflplanner has an older one)
    if ((length(exclude) || length(set) || isTRUE(data)) &&
        is.null(guarded(.save_batch_programs(rv$p, rv$study$path)))) {
      return()
    }
    before <- list_batches(rv$study)$batch
    px <- guarded(run_batch(rv$study, parts, code = code, data = isTRUE(data),
                            exclude = exclude, batch = set, wait = FALSE))
    if (is.null(px)) return()
    # the batch folder of this run: the one that was not there before (the
    # runs table says it is running, and the end says how it went)
    rv$job_batches_before <- before
    rv$job <- px
    rv$job_kind <- "official"
    rv$job_study <- rv$study$meta$study_id
    rv$job_what <- paste(t("the official run:"),
                         paste(t(c(ard = "ARD", tfl = "Reports")[parts]),
                               collapse = ", "),
                         if (length(set)) sprintf(t("(the report set %s)"), set),
                         if (length(exclude)) sprintf(t("(%d left out)"), length(exclude)))
  }
  # the reports of the run: every report, ticked; a report unticked is left
  # out (its ARD program and its report program)
  batch_ids <- shiny::reactive({
    shiny::req(has_study())
    rv$p$outputs$output_id
  })
  output$batch_reports_ui <- shiny::renderUI({
    ids <- batch_ids()
    if (!length(ids)) return(NULL)
    o <- rv$p$outputs
    lab <- ifelse(is.na(o$description) | !nzchar(o$description), ids,
                  paste0(ids, "  ", o$description))
    keep <- shiny::isolate(input$batch_reports)
    sel <- if (is.null(keep)) ids else intersect(keep, ids)
    sets <- names(batch_sets(rv$p))
    now <- shiny::isolate(chosen_set())
    if (!now %in% sets) now <- ""
    shiny::div(
      class = "mb-2",
      # the report set: always at hand (choosing one ticks its reports and
      # the count below follows); the list of reports is folded
      shiny::div(
        class = "rp-inline my-1",
        shiny::span(class = "small", with_tip(
          t("Report set"),
          t("A report set is a group of reports an official run takes by name (Topline, Interim Analysis, Final): choosing one ticks its reports. Which sets a report is in shows in the report list's Report sets column; the full run is every report and has no name."))),
        # (the full run: a value of its own -- selectize hides an empty one)
        shiny::selectInput("batch_set", NULL,
          c(stats::setNames(".all", t("(every report)")), stats::setNames(sets, sets)),
          selected = if (nzchar(now)) now else ".all", width = "220px", selectize = FALSE),
        .btn("batch_save", t("Save the ticks as..."), class = "btn-sm btn-outline-primary"),
        .btn("batch_rename", t("Rename..."), class = "btn-sm btn-outline-secondary"),
        .btn("batch_delete", t("Delete..."), class = "btn-sm btn-outline-danger")),
      shiny::tags$details(
        open = if (length(sets) || length(sel) < length(ids)) NA,
        shiny::tags$summary(class = "small", shiny::textOutput("batch_reports_n", inline = TRUE)),
        shiny::div(
          class = "rp-inline my-1",
          .btn("batch_all", t("All"), class = "btn-sm btn-outline-secondary"),
          .btn("batch_none", t("None"), class = "btn-sm btn-outline-secondary"),
          shiny::tags$input(id = "batch_q", type = "search", class = "form-control form-control-sm",
                            style = "max-width: 16rem;",
                            placeholder = t("Filter the reports (ID, description)"),
                            oninput = "rpBatchFilter()"),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                             onclick = "rpBatchShown('add')", t("Tick the shown")),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                             onclick = "rpBatchShown('remove')", t("Untick the shown")),
          shiny::tags$script(shiny::HTML(.batch_filter_js))),
        shiny::div(
          class = "border rounded px-2 small", style = "max-height: 16rem; overflow-y: auto;",
          shiny::checkboxGroupInput("batch_reports", NULL,
                                    stats::setNames(ids, lab), selected = sel,
                                    width = "100%"))))
  })
  output$batch_reports_n <- shiny::renderText({
    ids <- batch_ids()
    ticks <- intersect(input$batch_reports %||% ids, ids)
    n <- length(ticks)
    s <- chosen_set()
    paste0(
      if (n == length(ids)) sprintf(t("Reports in the run: all %d"), n) else
        sprintf(t("Reports in the run: %d of %d (%d left out)"), n, length(ids),
                length(ids) - n),
      # the ticks changed since the batch was chosen: the run has no name
      if (nzchar(s) && !setequal(ticks, batch_sets(rv$p)[[s]]))
        paste0(" ", sprintf(t("(not the report set %s any more: the run has no name)"), s)))
  })
  shiny::outputOptions(output, "batch_reports_n", suspendWhenHidden = FALSE)
  # the named batch chosen ("": none, the full run)
  chosen_set <- function() {
    s <- input$batch_set %||% ""
    if (s %in% c("", ".all")) "" else s
  }
  # a named batch chosen: its reports ticked (none: every report)
  shiny::observeEvent(input$batch_set, {
    s <- chosen_set()
    ids <- if (nzchar(s)) batch_sets(rv$p)[[s]] else batch_ids()
    shiny::updateCheckboxGroupInput(session, "batch_reports", selected = ids)
  }, ignoreInit = TRUE)
  batch_name_dialog <- function(title, id, value, note) {
    shiny::showModal(shiny::modalDialog(
      title = title, easyClose = TRUE,
      shiny::textInput(id, t("Name"), value, width = "100%"),
      shiny::p(class = "small text-muted", note),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        .btn(paste0(id, "_ok"), t("OK"), class = "btn-primary"))))
  }
  shiny::observeEvent(input$batch_save, {
    ticks <- intersect(input$batch_reports %||% character(), batch_ids())
    if (!length(ticks)) return(notify(t("Tick the reports of the report set first."), "warning"))
    batch_name_dialog(sprintf(t("Save the %d ticked reports as a report set"), length(ticks)),
                      "batch_save_name", chosen_set(),
                      t("A name the study has already is overwritten: it becomes these reports. Letters, digits, spaces, . _ -"))
  })
  shiny::observeEvent(input$batch_save_name_ok, {
    name <- trimws(input$batch_save_name %||% "")
    ticks <- intersect(input$batch_reports %||% character(), batch_ids())
    p2 <- guarded(set_batch(rv$p, name, ticks))
    if (is.null(p2)) return()
    shiny::removeModal()
    rv$p <- p2
    bump()
    shiny::updateSelectInput(session, "batch_set", selected = name)
    notify(sprintf(t("The report set %s: %d reports (save the study to keep it)."), name, length(ticks)))
  })
  shiny::observeEvent(input$batch_rename, {
    s <- chosen_set()
    if (!nzchar(s)) return(notify(t("Choose a report set first."), "warning"))
    batch_name_dialog(sprintf(t("Rename the report set %s"), s), "batch_rename_name", s,
                      t("Its reports stay as they are."))
  })
  shiny::observeEvent(input$batch_rename_name_ok, {
    from <- chosen_set()
    to <- trimws(input$batch_rename_name %||% "")
    p2 <- guarded(rename_batch(rv$p, from, to))
    if (is.null(p2)) return()
    shiny::removeModal()
    rv$p <- p2
    bump()
    shiny::updateSelectInput(session, "batch_set", selected = to)
  })
  shiny::observeEvent(input$batch_delete, {
    s <- chosen_set()
    if (!nzchar(s)) return(notify(t("Choose a report set first."), "warning"))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Delete the report set %s?"), s), easyClose = TRUE,
      shiny::p(t("Only the name goes: its reports stay on the report list.")),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        .btn("batch_delete_ok", t("Delete"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$batch_delete_ok, {
    s <- chosen_set()
    shiny::removeModal()
    rv$p <- remove_batch(rv$p, s)
    bump()
    shiny::updateSelectInput(session, "batch_set", selected = ".all")
  })
  shiny::observeEvent(input$batch_all, shiny::updateCheckboxGroupInput(
    session, "batch_reports", selected = batch_ids()))
  shiny::observeEvent(input$batch_none, shiny::updateCheckboxGroupInput(
    session, "batch_reports", selected = character()))
  # the reports a filter shows, ticked or unticked together
  shiny::observeEvent(input$batch_shown, {
    s <- input$batch_shown
    ids <- intersect(as.character(unlist(s$ids)), batch_ids())
    now <- intersect(input$batch_reports %||% batch_ids(), batch_ids())
    shiny::updateCheckboxGroupInput(session, "batch_reports",
                                    selected = .batch_ticks(now, ids, s$mode))
  })
  # the run: a named batch when the ticks are its reports; else the ticks
  # (the reports unticked left out)
  batch_go <- function() {
    parts <- input$batch_parts %||% "all"
    ids <- batch_ids()
    ticks <- intersect(input$batch_reports %||% ids, ids)
    s <- chosen_set()
    data <- isTRUE(input$batch_data)
    if (nzchar(s) && setequal(ticks, batch_sets(rv$p)[[s]])) {
      return(start_batch(parts, isTRUE(input$batch_code), set = s, data = data))
    }
    start_batch(parts, isTRUE(input$batch_code), setdiff(ids, ticks), data = data)
  }
  # the data the run would keep, ticked: how many files, how large (a
  # warning over tflplanner.batch_data_warn_mb, 500 MB), and those the
  # catalog names that are not there
  output$batch_data_size <- shiny::renderUI({
    if (!isTRUE(input$batch_data)) return(NULL)
    shiny::req(has_study())
    parts <- input$batch_parts %||% "all"
    parts <- if (identical(parts, "all")) c("ard", "tfl") else parts
    ids <- batch_ids()
    ticks <- intersect(input$batch_reports %||% ids, ids)
    d <- tryCatch(batch_data_plan(list(path = rv$study$path, planner = rv$p), parts, ticks),
                  error = function(e) NULL)
    if (is.null(d)) return(NULL)
    if (!nrow(d)) {
      return(shiny::p(class = "small text-muted mb-1",
                      t("The run's programs read no data file of the data catalog.")))
    }
    mb <- sum(d$size, na.rm = TRUE) / 1024^2
    lim <- getOption("tflplanner.batch_data_warn_mb", 500)
    gone <- d$path[is.na(d$size)]
    shiny::tagList(
      shiny::p(class = "small text-muted mb-1", title = paste(d$path, collapse = "\n"),
               sprintf(t("Data kept: %d files, %s."), sum(!is.na(d$size)), .mb_text(mb))),
      if (mb > lim) shiny::p(class = "small text-warning mb-1", sprintf(
        t("Over %s: every run keeps its own copy, so the runs folder grows by this much each time."),
        .mb_text(lim))),
      if (length(gone)) shiny::p(class = "small text-warning mb-1", sprintf(
        t("Not found, not kept: %s."), paste(gone, collapse = ", "))))
  })
  shiny::observeEvent(input$batch_run, {
    parts <- input$batch_parts %||% "all"
    if (parts %in% c("ard", "all") && !is.null(ard_valid())) {
      return(notify(t("Correct the ARD definition first."), "warning"))
    }
    ids <- batch_ids()
    exclude <- setdiff(ids, input$batch_reports %||% ids)
    if (length(exclude) == length(ids)) {
      return(notify(t("Choose at least one report for the run."), "warning"))
    }
    # a report the run makes that reads the ARD of one it leaves out:
    # said before the run (its rows are as an earlier run left them)
    need <- if (identical(parts, "all")) .batch_excluded_needs(rv$p, exclude)
    if (!is.null(need) && nrow(need)) {
      return(shiny::showModal(shiny::modalDialog(
        title = t("A report the run makes reads an ARD it leaves out"),
        shiny::p(t("These reports read the study ARD's rows of a report left out of the run. Its ARD program does not run, so they use those rows as an earlier run left them (or none):")),
        shiny::tags$ul(lapply(seq_len(nrow(need)), function(i) shiny::tags$li(
          sprintf(t("%s reads the ARD of %s"), need$report[i], need$needs[i])))),
        footer = shiny::tagList(
          .btn("batch_need_cancel", t("Cancel"), class = "btn-outline-secondary"),
          .btn("batch_need_go", t("Run anyway"), class = "btn-primary")))))
    }
    batch_go_ai()
  })
  shiny::observeEvent(input$batch_need_cancel, shiny::removeModal())
  shiny::observeEvent(input$batch_need_go, {
    shiny::removeModal()
    batch_go_ai()
  })
  # the AI's reports in the run whose draft changed since they were
  # adopted, or that wait for corrections the agent was asked for: said
  # before the run (it makes them as the study has them)
  batch_go_ai <- function() {
    ids <- batch_ids()
    ids <- intersect(input$batch_reports %||% ids, ids)
    w <- .ai_run_warnings(if (has_study()) rv$study$path, ids)
    if (!length(w$newer) && !length(w$fix)) return(batch_go())
    shiny::showModal(shiny::modalDialog(
      title = t("The AI's reports in this run"),
      if (length(w$newer)) shiny::p(sprintf(
        t("Their draft changed after they were adopted (step 4 of the AI tab shows what changed): %s."),
        paste(w$newer, collapse = ", "))),
      if (length(w$fix)) shiny::p(sprintf(
        t("They wait for corrections you asked the agent for (an assumption answered wrong, or dropped from your list): %s."),
        paste(w$fix, collapse = ", "))),
      shiny::p(class = "small text-muted", t("The run makes them as the study has them now.")),
      footer = shiny::tagList(
        .btn("batch_ai_cancel", t("Cancel"), class = "btn-outline-secondary"),
        .btn("batch_ai_go", t("Run anyway"), class = "btn-primary"))))
  }
  shiny::observeEvent(input$batch_ai_cancel, shiny::removeModal())
  shiny::observeEvent(input$batch_ai_go, {
    shiny::removeModal()
    batch_go()
  })
  batches <- shiny::reactive({
    rv$status_ver
    runs_ver()
    shiny::req(has_study())
    d <- list_batches(current_study())
    # the official run going on: its folder (one not there when it started)
    px <- rv$job
    if (!is.null(px) && !is.null(rv$job_batches_before) && px$is_alive()) {
      new <- !d$batch %in% rv$job_batches_before & d$state == "incomplete"
      d$state[new] <- "running"
      # its folder as soon as it is made, and its time taken, kept up to
      # date while it goes on
      shiny::invalidateLater(5000)
    }
    d
  })
  output$batch_open_ui <- shiny::renderUI({
    if (isTRUE(app_local())) .btn("batch_open", t("Open the run folder"))
  })
  output$batches <- DT::renderDT({
    d <- batches()
    is_local <- isTRUE(app_local())
    esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", x))
    col <- c(running = "#1d4ed8", done = "#15803d", failed = "#b91c1c",
             incomplete = "#6b7280")[d$state]
    st <- sprintf('<span style="color:%s;white-space:nowrap">%s</span>', col,
                  esc(.batch_state_text(d, t = t)))
    v <- data.frame(a = paste0(esc(d$batch), if (is_local) sprintf(
                      paste0(' <a href="#" class="ms-1 text-decoration-none" title="%s" ',
                             'onmousedown="event.stopPropagation();" ',
                             'onclick="event.stopPropagation(); ',
                             "Shiny.setInputValue('batch_open_row', '%s', ",
                             '{priority: \'event\'}); return false;">\U0001F4C2</a>'),
                      esc(t("Open its folder")), esc(d$batch)) else character(nrow(d))),
                    b = d$started,
                    c = t(c(ard = "ARD", report = "Reports",
                            all = "ARD, then reports")[d$what]),
                    d = st,
                    f = ifelse(d$code, "\u2713", ""),
                    f2 = ifelse(d$data, "\u2713", ""),
                    # the programs left out, the first few when many
                    g = vapply(strsplit(d$excluded, ", ", fixed = TRUE), function(x) {
                      x <- basename(x)
                      if (length(x) > 4L) paste0(paste(x[1:3], collapse = ", "),
                                                 sprintf(t(", ... (%d)"), length(x)))
                      else paste(x, collapse = ", ")
                    }, ""),
                    h = esc(d$set),
                    stringsAsFactors = FALSE)
    v$g <- esc(v$g)
    names(v) <- t(c("Run folder", "Started", "Runs", "State",
                    "Code", "Data", "Left out", "Report set"))
    # the row chosen stays chosen while a running one's time is drawn again
    sel <- shiny::isolate(input$batches_rows_selected)
    DT::datatable(v, rownames = FALSE, escape = FALSE,
                  selection = list(mode = "single", selected = sel),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                 scrollX = TRUE))
  })
  # a row's folder icon: the folder opened (not the row chosen: DT chooses
  # on mousedown, which the icon keeps to itself), and said
  # -- a window a background R opens may come up behind the browser
  shiny::observeEvent(input$batch_open_row, {
    if (!isTRUE(app_local())) return()
    d <- batches()
    i <- match(input$batch_open_row, d$batch)
    if (is.na(i)) return()
    ok <- !is.null(guarded(.open_folder(d$path[i])))
    if (ok) notify(sprintf(t("Opened the folder %s (a window behind the browser? see the taskbar)."),
                           d$batch[i]))
  })
  # two official runs compared: the one chosen (else the newest) with the
  # run of the same kind before it, or any two; what changed only
  # a run as the list shows it: its folder, what it ran, how it ended
  cmp_labels <- function(d) {
    what <- t(c(ard = "ARD", report = "Reports", all = "ARD, then reports")[d$what])
    st <- t(c(done = "done", failed = "errors", incomplete = "stopped", running = "running")[d$state])
    stats::setNames(d$batch, paste0(d$batch, "  \u00b7 ", what, " \u00b7 ", st))
  }
  shiny::observeEvent(input$batch_compare, {
    d <- batches()
    if (nrow(d) < 2L) return(notify(t("Two official runs are needed to compare."), "warning"))
    i <- input$batches_rows_selected
    # the run chosen; else the newest that ended
    ended <- which(d$state %in% c("done", "failed"))
    b <- d$batch[if (length(i)) i[1L] else if (length(ended)) ended[1L] else 1L]
    a <- .batch_previous(d, b)
    if (is.na(a)) a <- setdiff(d$batch, b)[1L]
    ch <- cmp_labels(d)
    shiny::showModal(shiny::modalDialog(
      title = t("Compare official runs"), size = "l", easyClose = TRUE,
      shiny::div(
        class = "d-flex flex-wrap gap-3 align-items-end",
        shiny::selectInput("cmp_against", t("Earlier run"), ch, a, width = "22rem"),
        shiny::span(class = "pb-3", "\u2192"),
        shiny::selectInput("cmp_batch", t("Later run"), ch, b, width = "22rem")),
      shiny::uiOutput("cmp_summary"),
      shiny::checkboxInput("cmp_all", t("Show everything compared (not only what changed)"), FALSE),
      DT::DTOutput("cmp_table"),
      footer = shiny::tagList(
        shiny::downloadButton("cmp_csv", t("Download (CSV)"), class = "btn-sm"),
        shiny::modalButton(t("Close")))))
  })
  cmp_result <- shiny::reactive({
    shiny::req(input$cmp_batch, input$cmp_against)
    if (identical(input$cmp_batch, input$cmp_against)) return(NULL)
    tryCatch(compare_batches(current_study(), input$cmp_batch, input$cmp_against),
             error = function(e) e)
  })
  cmp_result_labels <- function(x) {
    lab <- c(identical = t("identical"), `same but the time` = t("same but the time"),
             `same object` = t("same object"), different = t("different"),
             `not made` = t("not made"), new = t("new"), added = t("added"),
             removed = t("removed"), changed = t("changed"), `not kept` = t("not kept"))
    ifelse(x %in% names(lab), lab[x], x)
  }
  output$cmp_summary <- shiny::renderUI({
    r <- cmp_result()
    if (is.null(r)) return(shiny::p(class = "small text-muted", t("Choose two different runs.")))
    if (inherits(r, "error")) return(shiny::p(class = "small text-danger", conditionMessage(r)))
    ch <- attr(r, "changed")
    nk <- unique(r$kind[r$result == "not kept"])
    kind_lab <- c(data = t("data"), code = t("code"))
    few <- function(v) {
      v <- basename(v)
      if (length(v) > 4L) paste0(paste(v[1:3], collapse = ", "), sprintf(t(", ... (%d)"), length(v)))
      else paste(v, collapse = ", ")
    }
    n_out <- sum(r$kind == "output")
    shiny::div(
      class = "small mb-2",
      shiny::div(shiny::strong(t("Inputs changed:")), " ",
                 if (length(ch$inputs)) few(ch$inputs) else t("none"),
                 if (length(nk)) shiny::span(class = "text-muted", sprintf(
                   t(" (%s not kept by both runs)"), paste(kind_lab[nk], collapse = ", ")))),
      shiny::div(shiny::strong(t("Results changed:")), " ",
                 if (length(ch$outputs)) sprintf(t("%d of %d: %s"), length(ch$outputs), n_out,
                                                 few(ch$outputs))
                 else sprintf(t("none of %d"), n_out)))
  })
  output$cmp_table <- DT::renderDT({
    r <- cmp_result()
    shiny::req(is.data.frame(r))
    # what changed only (an input not kept is said above the table)
    if (!isTRUE(input$cmp_all)) {
      r <- r[!r$result %in% c("identical", "same but the time", "same object", "not kept"), ,
             drop = FALSE]
    }
    shiny::validate(shiny::need(nrow(r) > 0L, ""))
    v <- data.frame(a = t(c(data = "data", code = "code", output = "output")[r$kind]),
                    b = r$file, c = cmp_result_labels(r$result), stringsAsFactors = FALSE)
    names(v) <- t(c("Kind", "File", "Compared"))
    .dt(v, selection = "none")
  })
  output$cmp_csv <- shiny::downloadHandler(
    filename = function() paste0("compare_", input$cmp_against, "_", input$cmp_batch, ".csv"),
    content = function(file) {
      r <- cmp_result()
      if (!is.data.frame(r)) r <- data.frame(kind = character(), file = character(),
                                             result = character())
      utils::write.csv(r, file, row.names = FALSE)
    })
  shiny::observeEvent(input$batch_open, {
    if (!isTRUE(app_local())) return()
    d <- batches()
    i <- input$batches_rows_selected
    if (!length(i)) {
      .open_folder(file.path(rv$study$path, study_layout()[["runs"]]))
    } else .open_folder(d$path[i])
  })
  shiny::observeEvent(input$run_selected, {
    d <- selected_status()
    if (!nrow(d)) return(notify(t("Choose a report"), "warning"))
    start_run(d$output_id, paste(d$output_id, collapse = ", "))
  })
  # A run this session did not start, or lost (the app restarted, another
  # session, R): what it writes -- the study ARD's status, a batch's
  # run.csv -- is watched, and only what changed follows it, without a
  # reload and without drawing the page again (#9184): the report list's
  # marks in place, the ARD's state when the study ARD's status changed
  # (the builder reads a table's ARD then), the Runs tab when a batch ended
  runs_ver <- shiny::reactiveVal(0L)
  run_files <- shiny::reactivePoll(3000, session, checkFunc = function() {
    s <- shiny::isolate(rv$study)
    if (is.null(s)) return(NULL)
    .run_files_stamp(s$path)
  }, valueFunc = function() {
    s <- shiny::isolate(rv$study)
    if (is.null(s)) NULL else .run_files_stamp(s$path)
  })
  run_files_seen <- new.env()
  shiny::observe({
    stamp <- run_files()
    s <- shiny::isolate(rv$study)
    if (is.null(stamp) || is.null(s)) return()
    key <- s$path
    before <- run_files_seen[[key]]
    run_files_seen[[key]] <- stamp
    # the first look at a study is no change
    if (is.null(before) || identical(before, stamp)) return()
    shiny::isolate({
      # the ARD's state again: while a run goes on, only when the report
      # chosen has its ARD made or changed (its builder reads it then);
      # every report's once, when the run has ended
      ended <- !identical(before[["runs"]], stamp[["runs"]])
      if (!identical(before[["ard"]], stamp[["ard"]]) || ended) {
        st <- tryCatch(ard_status(current_study()), error = function(e) NULL)
        now <- if (!is.null(st)) stats::setNames(paste(st$state, st$built), st$output_id)
        was <- run_files_seen[[paste0(key, "|ard")]]
        run_files_seen[[paste0(key, "|ard")]] <- now
        id <- current() %||% ""
        if (ended || !identical(now[id], was[id])) ard_state_ver(ard_state_ver() + 1L)
      }
      if (ended) runs_ver(runs_ver() + 1L)
      p <- rv$p
      if (!is.null(p)) {
        m <- tryCatch(.report_marks(p, current_study()), error = function(e) NULL)
        old <- run_marks()
        if (!is.null(m) && !identical(m, old$marks)) {
          mark_clock$n <- mark_clock$n + 1
          run_marks(list(seq = mark_clock$n, marks = m))
        }
      }
    })
  })
  # (for the tests: what a run in the background changes, and what it must not)
  session$userData$run_end <- list(marks = run_marks, rows = picker_rows,
                                   view = outputs_view, runs_ver = runs_ver,
                                   ard_state_ver = ard_state_ver)
  shiny::observe({
    px <- rv$job
    if (is.null(px)) return()
    if (px$is_alive()) {
      shiny::invalidateLater(1000)
      return()
    }
    rc <- px$get_exit_status()
    shiny::isolate({
      # an official run: how it went, from its batch folder
      b <- if (!is.null(rv$job_batches_before) && !is.null(rv$study)) {
        d <- tryCatch(list_batches(rv$study), error = function(e) NULL)
        if (!is.null(d)) d[!d$batch %in% rv$job_batches_before, , drop = FALSE][1L, ]
      }
      if (!is.null(b) && !is.na(b$batch) && b$state %in% c("done", "failed")) {
        took <- .elapsed_text(difftime(as.POSIXct(b$finished, format = "%Y-%m-%d %H:%M:%S"),
                                       as.POSIXct(b$started, format = "%Y-%m-%d %H:%M:%S"),
                                       units = "secs"))
        notify(if (b$state == "done") {
          sprintf(t("The official run is done: %d of %d programs without error (%s, %s)."),
                  b$programs, b$programs, took, b$batch)
        } else {
          sprintf(t("The official run ended with errors: %d of %d programs failed (%s, %s)."),
                  b$errors, b$programs, took, b$batch)
        }, if (b$state == "done") "message" else "warning")
      } else {
        notify(sprintf(if (identical(rc, 0L)) t("Finished: %s") else
                         t("Finished with errors: %s"), rv$job_what),
               if (identical(rc, 0L)) "message" else "warning")
      }
      rv$job_batches_before <- NULL
      rv$job <- NULL
      rv$job_kind <- NULL
      rv$job_study <- NULL
      rv$status_ver <- rv$status_ver + 1L
      rv$ard_ver <- rv$ard_ver + 1L
    })
  })
  output$job <- shiny::renderUI({
    if (is.null(rv$job)) return(NULL)
    shiny::div(class = "alert alert-info py-2 small my-2",
               shiny::span(class = "spinner-border spinner-border-sm me-2"),
               sprintf(t("Running %s ..."), rv$job_what))
  })
  # a study opened with its official run going on (started before this
  # app, or by another): the app follows it as one it started
  shiny::observeEvent(rv$study, {
    if (!is.null(rv$job) || is.null(rv$study)) return()
    rec <- tryCatch(.live_run(rv$study$meta$study_id), error = function(e) NULL)
    if (is.null(rec)) return()
    rv$job_batches_before <- strsplit(rec$before %||% "", "|", fixed = TRUE)[[1L]]
    rv$job <- .attach_run(rec)
    rv$job_kind <- "official"
    rv$job_study <- rec$study_id
    rv$job_what <- sprintf(t("the official run (started %s)"), substr(rec$started, 12L, 16L))
  }, ignoreNULL = TRUE)
  # a preview stops with the session; an official run goes on by itself
  session$onSessionEnded(function() {
    px <- shiny::isolate(rv$job)
    if (!is.null(px) && !identical(shiny::isolate(rv$job_kind), "official") &&
        px$is_alive()) px$kill()
  })
  .srv_keep(environment())
}
