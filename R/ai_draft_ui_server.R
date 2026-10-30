# ============================================================================
#  The AI drafts tab: the module's server (split from R/ai_draft_ui.R)
#  ai_server(): the three steps' reactive logic.  The UI (ai_ui()) and the
#  tab's CSS / JS stay in R/ai_draft_ui.R.
# ============================================================================

# The module's server.  `rv` the app's values (rv$study: the study open);
# `has_study()`, `dirty()` (unsaved changes), `reload_now()` (the study
# read again from its files; FALSE when it was not), `adopt(study)` (a new
# study registered: listed and opened), `open_report(id)` (a report in
# Make a report), `go(where)` (a top tab), `notify(msg, type)`,
# `guarded(expr)` the app's.  Gives list(requests, collected, prepared,
# text) (the tests read them).
#' @noRd
ai_server <- function(id, rv, has_study, dirty, reload_now, adopt, open_report, go, notify,
                      guarded, lang = "en") {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(x) tr(x, lang)
    ns <- session$ns
    # the study's folder: a save makes a new copy of the study, not a new folder
    path <- shiny::reactive(if (has_study()) rv$study$path else NULL)
    ver <- shiny::reactiveVal(0L)
    collected <- shiny::reactiveVal(NULL)
    # (a dialog closed: the page's waiting note taken away)
    browse_done <- function(x) {
      session$sendCustomMessage("ai-browse-done", TRUE)
      x
    }
    # step 4: the list and the copying (R/ai_list_ui.R)
    ai_list_server(input, output, session, path, ver, reload_now, notify, guarded, t, dirty = dirty)
    # the look of past reports (R/ai_style_ui.R)
    ai_style_server(input, output, session, notify, guarded, t)
    # another study opened: its own check, none yet
    was <- NULL
    shiny::observe({
      p <- path()
      if (!identical(p, was)) {
        collected(NULL)
        unbuilt(NULL)
      }
      was <<- p
    })
    # the reports run.R could not build (.ai_unbuilt()), read before each
    # check: the check removes the fix request of an answer that passes.
    # Kept while ai/status.md is the same run's.
    unbuilt <- shiny::reactiveVal(NULL)
    keep_unbuilt <- function() {
      p <- path()
      if (is.null(p)) return(invisible())
      when <- file.mtime(file.path(p, "ai", "status.md"))
      now <- tryCatch(.ai_unbuilt(p), error = function(e) NULL)
      if (is.null(now)) return(invisible())
      old <- unbuilt()
      # (only the old format's: a -build.md stays until run.R removes it)
      if (!is.null(old) && identical(attr(old, "when"), when)) {
        more <- old[old$from == "repair" & !old$request %in% now$request, , drop = FALSE]
        texts <- c(attr(now, "texts"), attr(old, "texts")[setdiff(names(attr(old, "texts")),
                                                                  names(attr(now, "texts")))])
        build <- attr(now, "build")
        now <- rbind(now, more)
        attr(now, "texts") <- texts
        attr(now, "build") <- build
      }
      attr(now, "when") <- when
      unbuilt(now)
      invisible()
    }

    requests <- shiny::reactive({
      ver()
      p <- path()
      if (is.null(p)) return(NULL)
      tryCatch(.ai_requests(p), error = function(e) {
        message("tflplanner: the AI requests: ", conditionMessage(e))
        NULL
      })
    })
    # the text of a request (or of the instructions, or of a fix request)
    text_of <- function(what, id) {
      p <- path()
      if (identical(what, "agent")) return(agent_prompt())
      if (identical(what, "rerun")) return(paste("Rscript run.R", id))
      if (identical(what, "build")) {
        .ai_check_request_name(id)
        return(.ai_read_text(file.path(p, .ai_repairs_dir, paste0(id, "-build.md"))))
      }
      kept <- attr(shiny::isolate(unbuilt()), "texts")
      if (identical(what, "unbuilt")) {
        u <- shiny::isolate(unbuilt())
        u <- u[u$request == id, , drop = FALSE]
        return(paste(c("## Could not be built", "",
                       sprintf("- %s: %s", u$output_id, u$reason)), collapse = "\n"))
      }
      # (a report's fix text, from the last check: tfl_check_fix_text())
      if (identical(what, "fix")) {
        txt <- collected()$checks$fix[id]
        if (is.null(txt) || is.na(txt)) stop("No fix text for ", id, ": check again.", call. = FALSE)
        return(unname(txt))
      }
      if (identical(what, "repair") && !is.null(kept[id]) && !is.na(kept[id]) &&
          !file.exists(file.path(p, .ai_repairs_dir, paste0(id, ".md")))) {
        return(kept[[id]])
      }
      .ai_request_text(p, id, if (identical(what, "repair")) "repair" else "request")
    }
    file_of <- function(what, id) {
      switch(what,
             agent = t("The agent's first message"),
             repair = .ai_rel(.ai_repairs_dir, paste0(id, ".md")),
             fix = sprintf(t("The fix text of %s"), id),
             build = .ai_rel(.ai_repairs_dir, paste0(id, "-build.md")),
             unbuilt = .ai_rel(.ai_repairs_dir, paste0(
               id, if (id %in% attr(shiny::isolate(unbuilt()), "build")) "-build.md" else ".md")),
             rerun = t("To rebuild only this request"),
             file = id,
             paste0(id, ".md"))
    }

    # -- the status line ----------------------------------------------------
    output$status_line <- shiny::renderUI({
      if (!has_study()) {
        return(shiny::span(class = "text-muted", t("No study open: step 1 can make a new one.")))
      }
      r <- requests()
      sid <- rv$study$meta$study_id
      if (is.null(r) || !nrow(r)) {
        return(shiny::tagList(shiny::tags$b(sid), " ",
                              shiny::span(class = "text-muted", t("No requests yet: step 1."))))
      }
      n <- .ai_counts(r, collected())
      st <- if (is.null(collected())) NULL else
        paste(sprintf("%s %d", t(names(n$status)), n$status), collapse = ", ")
      shiny::tagList(
        shiny::tags$b(sid),
        paste(c(sprintf(t("%d reports, %d requests"), n$reports, n$requests),
                sprintf(t("drafts %d / %d"), n$drafts, n$reports), st),
              collapse = "  |  "))
    })

    # -- step 2: the requests ------------------------------------------------
    # a row's buttons: Copy, View (the page tells the server which, by data-*)
    act <- function(what, id, labels = c(copy = t("Copy text"), view = t("View"))) {
      b <- function(kind) sprintf(
        '<button type="button" class="btn btn-sm btn-outline-secondary py-0 ai-act" data-ns="%s" data-kind="%s" data-what="%s" data-id="%s">%s</button>',
        ns(""), kind, what, htmltools::htmlEscape(id, TRUE), htmltools::htmlEscape(labels[[kind]]))
      paste(vapply(names(labels), b, ""), collapse = " ")
    }
    output$requests <- DT::renderDT({
      r <- requests()
      empty <- if (!has_study()) t("Open a study.") else t("No requests yet: step 1.")
      if (is.null(r) || !nrow(r)) {
        return(DT::datatable(data.frame(x = empty), rownames = FALSE, colnames = "",
                             selection = "none", options = list(dom = "t", ordering = FALSE)))
      }
      st <- .ai_status_label(r$request, collected()$reports, t)
      d <- data.frame(
        request = r$request, family = r$family, reports = r$n, outputs = r$outputs,
        chars = format(r$chars, big.mark = ","),
        status = ifelse(is.na(st), "", st),
        drafts = ifelse(r$drafts > 0L, sprintf("%d / %d  %s", r$drafts, r$n, format(r$draft_time, "%m/%d %H:%M")),
                        "\u2014"),
        book = ifelse(is.na(r$book), "", r$book),
        actions = vapply(r$request, function(q) act("request", q), ""),
        stringsAsFactors = FALSE)
      names(d) <- c(t("request"), t("family"), t("reports"), t("outputs"), t("characters"),
                    t("status"), t("drafts"), t("Workbook"), "")
      DT::datatable(d, rownames = FALSE, escape = -9, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                   scrollX = TRUE))
    }, server = FALSE)
    shiny::observeEvent(input$reread, ver(ver() + 1L))
    shiny::observeEvent(input$open_requests, {
      shiny::req(path())
      guarded(.open_folder(file.path(path(), .ai_requests_dir)))
    })

    # Copy: the file read here, written to the clipboard by the page; when
    # the page cannot (no permission, not a safe context) the View dialog,
    # to select it all and copy by hand
    view_modal <- function(what, id, failed = FALSE) {
      txt <- guarded(text_of(what, id))
      if (is.null(txt)) return(invisible())
      shiny::showModal(shiny::modalDialog(
        title = sprintf(t("%s  %s characters"), file_of(what, id),
                        format(nchar(sub("\n$", "", txt)), big.mark = ",")),
        size = "xl", easyClose = TRUE,
        if (failed) shiny::div(class = "alert alert-warning py-1 small",
                               t("The page could not copy it: select it all and press Ctrl+C.")),
        shiny::tags$textarea(id = ns("view_text"), class = "form-control rp-code ai-view",
                             readonly = NA, rows = 24, txt),
        footer = shiny::tagList(
          shiny::tags$button(type = "button", class = "btn btn-outline-secondary ai-select",
                             `data-target` = ns("view_text"), t("Select all")),
          shiny::tags$button(type = "button", class = "btn btn-primary ai-copy-area",
                             `data-target` = ns("view_text"), t("Copy text")),
          shiny::modalButton(t("Close")))))
    }
    shiny::observeEvent(input$act, {
      a <- input$act
      what <- a$what %||% "request"
      if (identical(a$kind, "view")) return(view_modal(what, a$id))
      txt <- guarded(text_of(what, a$id))
      if (is.null(txt)) return()
      session$sendCustomMessage("ai-copy", list(text = txt, what = what, id = a$id,
                                               failed = ns("copy_failed")))
    })
    shiny::observeEvent(input$copy_failed, {
      a <- input$copy_failed
      view_modal(a$what %||% "request", a$id, failed = TRUE)
    })

    check_all <- function() {
      shiny::req(path())
      if (isTRUE(dirty())) {
        notify(t("Save first: the study has unsaved changes."), "warning")
        return()
      }
      keep_unbuilt()
      co <- guarded(.ai_collect(path()))
      if (is.null(co)) return()
      collected(co)
      ver(ver() + 1L)
      # the reports written: the study read from its files at once (no save
      # between, which would see them as made outside the app)
      if (!isTRUE(reload_now())) return()
      ok <- unique(co$reports$output_id[co$reports$status %in% c("ok", "warning")])
      drafts <- any(!.ai_in_study_spec(co$reports$file[co$reports$output_id %in% ok]))
      taken(list(ok = ok, drafts = drafts))
      notify(if (length(ok) && drafts) sprintf(t("%d drafts taken in: %s (step 4: adopt them into the study)"),
                                               length(ok), paste(ok, collapse = ", "))
             else if (length(ok)) sprintf(t("%d reports taken in: %s (Report list)"), length(ok),
                                          paste(ok, collapse = ", "))
             else t("No report could be taken in: see each request's next step."))
    }
    # the last check's reports, said on the page until the next check: how
    # many were taken in, and that step 4 adopts (copies) them into the study
    taken <- shiny::reactiveVal(NULL)
    shiny::observeEvent(path(), taken(NULL), ignoreNULL = FALSE)
    output$taken_note <- shiny::renderUI({
      k <- taken()
      if (is.null(k)) return(NULL)
      if (!length(k$ok)) {
        return(shiny::div(class = "alert alert-secondary py-2 small",
                          t("No report could be taken in: see each request's next step.")))
      }
      shiny::div(
        class = paste("alert py-2 d-flex flex-wrap gap-2 align-items-center",
                      if (k$drafts) "alert-info" else "alert-success"),
        shiny::span(class = "me-auto", if (k$drafts) {
          sprintf(t("%d drafts taken in. Next: adopt (copy) them into the study in step 4; the Report list shows only the adopted ones."),
                  length(k$ok))
        } else sprintf(t("%d reports taken in: on the Report list."), length(k$ok))),
        if (k$drafts) .btn(ns("to_list2"), paste0(t("To step 4"), " (", t("Copy into the study"), ")"),
                           class = "btn-sm btn-primary"))
    })
    shiny::observeEvent(input$to_list2, bslib::nav_select("step", "list", session = session))
    shiny::observeEvent(input$open_report, open_report(input$open_report$id, input$open_report$item))

    # -- step 1: the requests prepared (tfl_ai_prepare()) ----------------------
    prepared <- shiny::reactiveVal(NULL)
    # the folder it was read for, the error it stopped with, the ADaM
    # summary's max_levels
    prep_for <- shiny::reactiveVal(NULL)
    prep_error <- shiny::reactiveVal(NULL)
    used_levels <- shiny::reactiveVal(NULL)
    is_new <- function() !identical(input$study_mode, "open") || !has_study()
    nz <- function(v) if (is.null(v) || !length(v) || is.na(v[1L]) || !nzchar(v[1L])) NULL else v
    # the study's folder the requests go to (a new study's, made on Write)
    target_dir <- function() {
      if (!is_new()) return(path())
      id <- trimws(input$new_id %||% "")
      if (is.null(.ai_new_study_problem(id))) .study_path(studies_root(), id) else
        file.path(tempdir(), "tflplanner-ai-new")
    }
    # no study open: a new one; another study opened: its ADaM folder, and
    # what was read for another folder is gone
    shiny::observe({
      if (!has_study()) shiny::updateRadioButtons(session, "study_mode", selected = "new")
    })
    seen <- NULL
    shiny::observe({
      p <- path()
      # (a save makes a new copy of the study, not another study)
      if (identical(p, seen)) return()
      seen <<- p
      shiny::isolate(if (!is.null(prep_for()) && !identical(prep_for(), p)) {
        prepared(NULL)
        prep_for(NULL)
      })
      # another study: its step 1 (the step last open was the other
      # study's); the study the requests were just written for stays on
      # them (Write opens it)
      if (!is.null(p)) shiny::isolate({
        shiny::updateRadioButtons(session, "study_mode", selected = "open")
        np <- function(x) if (is.null(x)) "" else normalizePath(x, "/", mustWork = FALSE)
        if (!identical(np(prep_for()), np(p))) bslib::nav_select("step", "prepare", session = session)
      })
    })
    # the ADaM folder: the chosen study's (.ai_adam_auto()), following the
    # study chosen and the ID typed; a folder typed or chosen in Browse... is
    # kept (it is not the one put in last)
    adam_auto <- ""
    shiny::observe({
      d <- .ai_adam_auto(if (!is_new()) rv$study, new_id = input$new_id, root = studies_root())
      shiny::isolate({
        now <- trimws(input$adam_dir %||% "")
        if (identical(d, adam_auto)) return()
        if (!nzchar(now) || identical(now, adam_auto)) {
          shiny::updateTextInput(session, "adam_dir", value = d)
          adam_auto <<- d
        }
      })
    })
    output$open_name <- shiny::renderUI({
      if (!identical(input$study_mode, "open")) return(NULL)
      if (!has_study()) {
        return(shiny::div(class = "small text-danger mb-2", t("Open a study, or make a new one.")))
      }
      shiny::div(class = "small text-muted mb-2", shiny::tags$b(rv$study$meta$study_id), " ",
                 rv$study$path)
    })
    output$new_where <- shiny::renderUI({
      id <- input$new_id %||% ""
      msg <- .ai_new_study_problem(id, t = t)
      if (!is.null(msg) && nzchar(trimws(id))) {
        return(shiny::div(class = "small text-danger mb-2", msg))
      }
      shiny::div(class = "small text-muted mb-2",
                 sprintf(t("made in: %s (studies_root)"), studies_root()))
    })
    output$adam_files <- shiny::renderUI({
      d <- trimws(input$adam_dir %||% "")
      if (!nzchar(d)) return(NULL)
      f <- .ai_adam_files(d)
      if (is.null(f)) return(shiny::div(class = "small text-danger mb-2", t("No such folder.")))
      if (!length(f)) {
        return(shiny::div(class = "small text-danger mb-2",
                          t("No .xpt, .sas7bdat or .rds file in it.")))
      }
      shiny::div(class = "small text-muted mb-2",
                 sprintf(t("%d files: %s"), length(f),
                         paste(c(utils::head(f, 8L), if (length(f) > 8L) "..."), collapse = ", ")))
    })
    # Browse...: the computer's own dialog fills the path (as typing it
    # does); cancelled, nothing changes
    # (it opens in the study's folder: .ai_browse_start())
    browse_start <- function(kind, field) {
      .ai_browse_start(kind, field, study_dir = if (!is_new()) path(), root = studies_root(),
                       new_id = input$new_id %||% "")
    }
    shiny::observeEvent(input$adam_browse, {
      d <- browse_done(guarded(.ai_choose_dir(browse_start("adam", input$adam_dir))))
      if (length(d) == 1L && !is.na(d)) shiny::updateTextInput(session, "adam_dir", value = d)
    })
    shiny::observeEvent(input$adam_spec_browse, {
      f <- browse_done(guarded(.ai_choose_adam_spec(browse_start("adam_spec", input$adam_spec))))
      if (length(f) == 1L && !is.na(f)) shiny::updateTextInput(session, "adam_spec", value = f)
    })
    # (a workbook chosen from outside the study: copied into its docs/, the
    # copy read -- .ai_book_into_docs(); a study not made yet: as it is)
    into_docs <- function(f) {
      d <- if (!is_new()) path() else {
        id <- trimws(input$new_id %||% "")
        if (is.null(.ai_new_study_problem(id))) .study_path(studies_root(), id)
      }
      out <- guarded(.ai_book_into_docs(f, d))
      if (is.null(out)) return(f)
      if (!identical(out, f)) notify(sprintf(t("Copied into the study's docs/: %s"), basename(out)))
      out
    }
    shiny::observeEvent(input$toc_browse, {
      f <- browse_done(guarded(.ai_choose_file(browse_start("toc", input$toc_path))))
      if (length(f) == 1L && !is.na(f)) shiny::updateTextInput(session, "toc_path", value = into_docs(f))
    })
    # the workbook: the one given last, uploaded or its path typed
    toc_from <- shiny::reactiveVal("upload")
    shiny::observeEvent(input$toc_file, toc_from("upload"))
    shiny::observeEvent(input$toc_path, {
      if (nzchar(trimws(input$toc_path %||% ""))) toc_from("path")
    })
    toc_src <- function() {
      f <- input$toc_file
      p <- gsub('^"|"$', "", trimws(input$toc_path %||% ""))
      up <- if (!is.null(f) && nrow(f)) list(path = f$datapath[1L], name = f$name[1L])
      typed <- if (nzchar(p)) list(path = p, name = basename(p))
      if (identical(toc_from(), "path")) typed %||% up else up %||% typed
    }
    # the shell workbooks: those uploaded and those whose paths are typed
    shells_src <- function() {
      f <- input$shell_files
      up <- if (!is.null(f) && nrow(f)) lapply(seq_len(nrow(f)), function(i)
        list(path = f$datapath[i], name = f$name[i]))
      lines <- trimws(strsplit(input$shell_paths %||% "", "\n", fixed = TRUE)[[1L]])
      lines <- gsub('^"|"$', "", lines[nzchar(lines)])
      c(up %||% list(), lapply(lines, function(p) list(path = p, name = basename(p))))
    }
    shells_problems <- function() {
      unlist(lapply(shells_src(), function(s) .ai_toc_problem(s$path, t)))
    }
    output$shells_check <- shiny::renderUI({
      s <- shells_src()
      if (!length(s)) return(NULL)
      msg <- shells_problems()
      if (length(msg)) return(shiny::div(class = "small text-danger mb-2", lapply(msg, shiny::div)))
      shiny::div(class = "small text-muted mb-2",
                 sprintf(t("Shells read from: %s"), paste(vapply(s, `[[`, "", "name"), collapse = ", ")))
    })
    shiny::observeEvent(input$shells_browse, {
      f <- browse_done(guarded(.ai_choose_files(browse_start("shells", input$shell_paths))))
      if (!length(f)) return()
      f <- vapply(f, into_docs, "", USE.NAMES = FALSE)
      had <- trimws(input$shell_paths %||% "")
      shiny::updateTextAreaInput(session, "shell_paths",
                                 value = paste(c(if (nzchar(had)) had, f), collapse = "\n"))
    })
    # add to the earlier requests: by default when the study has some
    add_for <- NULL
    shiny::observe({
      p <- path()
      new <- is_new()
      # (set when the study or the choice of a new one changes, not at each save)
      if (identical(add_for, list(p, new))) return()
      add_for <<- list(p, new)
      r <- if (!new && !is.null(p)) tryCatch(tflspec::tfl_ai_requests(p), error = function(e) NULL)
      shiny::updateCheckboxInput(session, "add", value = !new && !is.null(r) && nrow(r) > 0L)
    })
    output$toc_check <- shiny::renderUI({
      src <- toc_src()
      if (is.null(src)) return(NULL)
      msg <- .ai_toc_problem(src$path, t)
      if (!is.null(msg)) return(shiny::div(class = "small text-danger mb-2", msg))
      shiny::div(class = "small text-muted mb-2",
                 sprintf(t("Read: %s (copied into the study's docs/ when the requests are written)"),
                         src$name))
    })
    # the reports a request holds: the agent's, one (each report its own
    # request: it asks before it writes)
    size_now <- function() as.integer(.ai_size_default("agent"))
    # what the read was for: the workbooks and their sheets (changed, the
    # read is out of date: read again); the size it counted the requests at
    read_key <- function() {
      list(toc = toc_src()$path, shells = vapply(shells_src(), `[[`, "", "path"),
           toc_sheet = nz(input$toc_sheet), sheets = sort(as.character(input$sheets)))
    }
    read_with <- shiny::reactiveVal(NULL)
    read_size <- shiny::reactiveVal(NULL)
    # (a sheet-first read: its requests group the
    # shell sheets, not countable again from its TOC -- another size, read
    # again)
    sheet_first <- function(p) !is.null(p$toc$toc_id)
    stale <- shiny::reactive({
      p <- prepared()
      !is.null(p) && isTRUE(p$dry_run) &&
        (!identical(read_with(), read_key()) ||
           (sheet_first(p) && !identical(size_now(), read_size())))
    })
    # the requests to write: the read's, counted again at the size now
    to_write <- shiny::reactive({
      p <- prepared()
      if (is.null(p) || !isTRUE(p$dry_run) || sheet_first(p) ||
          identical(size_now(), read_size())) return(p$requests)
      .ai_dry_requests(p$toc, size_now())
    })
    # the references the person chose (tfl_ai_prepare(docs = )): only
    # these, listed, each removable
    refs <- shiny::reactiveVal(character())
    shiny::observeEvent(path(), refs(character()), ignoreNULL = FALSE)
    ref_add <- function(f) {
      f <- trimws(gsub('^"|"$', "", as.character(f)))
      f <- f[nzchar(f)]
      gone <- f[!file.exists(f) | dir.exists(f)]
      if (length(gone)) notify(sprintf(t("No file %s."), paste(gone, collapse = ", ")), "warning")
      refs(unique(c(refs(), normalizePath(setdiff(f, gone), "/", mustWork = FALSE))))
    }
    shiny::observeEvent(input$ref_browse, {
      f <- browse_done(guarded(.ai_choose_refs(input$ref_path %||% "")))
      if (length(f)) ref_add(f)
    })
    shiny::observeEvent(input$ref_add, {
      ref_add(input$ref_path %||% "")
      shiny::updateTextInput(session, "ref_path", value = "")
    })
    shiny::observeEvent(input$ref_remove, refs(setdiff(refs(), input$ref_remove$id)))
    output$ref_list <- shiny::renderUI({
      r <- refs()
      if (!length(r)) return(shiny::p(class = "small text-muted", t("No reference: the agent reads none.")))
      shiny::tags$ul(class = "small mb-2", lapply(r, function(f) shiny::tags$li(
        shiny::code(basename(f)), " ", shiny::span(class = "text-muted", dirname(f)), " ",
        shiny::tags$a(href = "#", class = "ai-ref-remove", `data-ns` = ns(""), `data-id` = f,
                      onclick = "Shiny.setInputValue(this.dataset.ns + 'ref_remove', {id: this.dataset.id, n: Date.now()}, {priority: 'event'}); return false;",
                      t("Remove")))))
    })
    # (the agent's scope)
    scope_args <- function() {
      mr <- suppressWarnings(as.integer(input$agent_max_reads))
      c(list(agent_reads = as.character(input$agent_reads %||% character())),
        if (length(mr) && !is.na(mr) && mr >= 0L) list(agent_max_reads = mr),
        if (length(refs())) list(docs = refs()))
    }
    args_of <- function(p) {
      all <- if (!is.null(p)) p$adam$datasets$dataset
      ds <- input$datasets
      list(toc_sheet = nz(input$toc_sheet), sheets = if (length(input$sheets)) input$sheets,
           size = size_now(), language = nz(input$language),
           datasets = if (!is.null(all) && length(ds) && !setequal(ds, all)) ds,
           max_levels = as.integer(input$max_levels %||% 15L),
           templates = !isFALSE(input$templates),
           add = isTRUE(input$add) && !is_new(),
           sheet_roles = roles_now(),
           numbers = numbers_now()) |>
        c(scope_args(), .ai_adam_spec_args(input$adam_spec))
    }
    inputs_ok <- function() {
      msg <- .ai_toc_problem(toc_src()$path, t)
      if (!is.null(msg)) {
        notify(msg, "warning")
        return(FALSE)
      }
      if (is.null(.ai_adam_files(input$adam_dir))) {
        notify(t("Give the ADaM folder (a full path)."), "warning")
        return(FALSE)
      }
      msg <- .ai_adam_spec_problem(input$adam_spec, t)
      if (!is.null(msg)) {
        notify(msg, "warning")
        return(FALSE)
      }
      msg <- shells_problems()
      if (length(msg)) {
        notify(msg[1L], "warning")
        return(FALSE)
      }
      TRUE
    }
    # the sheets to choose from, as soon as the workbooks are known (their
    # names only; what is chosen already kept)
    update_sheets <- function(sh, bk, in_toc) {
      lab <- .ai_sheet_labels(sh, bk)
      toc_now <- shiny::isolate(input$toc_sheet) %||% ""
      shiny::updateSelectInput(session, "toc_sheet",
                               choices = c(stats::setNames("", t("guess")),
                                           stats::setNames(sh[in_toc], lab[in_toc])),
                               selected = if (toc_now %in% sh[in_toc]) toc_now else "")
      shiny::updateSelectizeInput(session, "sheets", choices = stats::setNames(sh, lab),
                                  selected = intersect(shiny::isolate(input$sheets), sh))
    }
    shiny::observe({
      src <- toc_src()
      shl <- vapply(shells_src(), `[[`, "", "path")
      if (is.null(src) || !is.null(.ai_toc_problem(src$path))) return()
      s <- .ai_sheet_names(src$path, shl[vapply(shl, function(f) is.null(.ai_toc_problem(f)), TRUE)])
      if (nrow(s)) shiny::isolate(update_sheets(s$sheet, s$book, s$toc))
    })
    # (Read and check, or the sheets' roles applied: read again)
    read_again <- shiny::reactiveVal(0L)
    tab_kept <- new.env()
    read_now <- function() {
      tab_kept$tab <- shiny::isolate(input$prep_tab)
      read_again(read_again() + 1L)
    }
    # the numbers in the sheets, changed on the Shells tab (R/ai_numbers_ui.R)
    numbers_now <- ai_numbers_server(input, output, session, prepared, read_now, t)
    shiny::observeEvent(list(input$read, read_again()), ignoreInit = TRUE, {
      if (is.null(input$read) && identical(read_again(), 0L)) return()
      if (!inputs_ok()) return()
      src <- toc_src()
      a <- args_of(NULL)
      # every dataset read: the ones to send are ticked afterwards
      a$datasets <- NULL
      dir <- target_dir()
      p <- NULL
      shiny::withProgress(message = t("Reading the workbook and the ADaM data"), {
        shl <- vapply(shells_src(), `[[`, "", "path")
        p <- tryCatch(.ai_prepare(dir, src$path, input$adam_dir, a, dry_run = TRUE, shells = shl),
                      error = function(e) {
                        prep_error(conditionMessage(e))
                        NULL
                      })
      })
      if (is.null(p)) {
        prepared(NULL)
        return()
      }
      prep_error(NULL)
      prepared(p)
      prep_for(if (is_new()) NULL else dir)
      used_levels(a$max_levels)
      read_size(as.integer(a$size))
      sh <- p$sheets$sheet
      bk <- p$sheets$book
      # (the TOC is one of the TOC workbook's sheets)
      in_toc <- if (is.null(bk)) TRUE else bk == bk[1L]
      update_sheets(sh, bk, in_toc)
      ds <- p$adam$datasets$dataset
      keep <- intersect(input$datasets, ds)
      shiny::updateCheckboxGroupInput(
        session, "datasets", inline = TRUE,
        choiceNames = lapply(seq_along(ds), function(i) sprintf(
          "%s (%d)", ds[i], p$adam$datasets$variables[i])),
        choiceValues = ds, selected = if (length(keep)) keep else ds)
      read_with(read_key())
      # (read again by an Apply: the tab it was on kept)
      tab <- tab_kept$tab
      tab_kept$tab <- NULL
      bslib::nav_select("prep_tab", tab %||% if (nrow(p$toc) && all(p$toc$shell_found)) "adam" else "toc",
                        session = session)
    })
    write_now <- function(overwrite = FALSE) {
      p <- prepared()
      shiny::req(p)
      if (!inputs_ok()) return()
      src <- toc_src()
      shl <- shells_src()
      a <- args_of(p)
      if (isTRUE(overwrite)) a$overwrite <- TRUE
      # the summary read is sent again unless its values' limit changed
      if (identical(a$max_levels, used_levels())) a$adam_summary <- p$adam
      out <- NULL
      # (tflspec writes a request at a time and says where it is)
      a$progress <- function(step, i, n) {
        if (identical(step, "request") && n > 0L) {
          shiny::setProgress(i / n, detail = sprintf(t("request %d of %d"), i, n))
        }
      }
      shiny::withProgress(message = t("Writing the requests"), {
        out <- tryCatch(if (is_new()) {
          .ai_new_study(input$new_id, src$path, src$name, input$adam_dir, a,
                        title = nz(trimws(input$new_title %||% "")), shells = shl)
        } else {
          list(study = NULL, prepared = .ai_write_requests(path(), src$path, src$name,
                                                           input$adam_dir, a, shells = shl))
        }, error = function(e) e)
      })
      if (inherits(out, "error")) {
        msg <- conditionMessage(out)
        # a report asked for already (add): said, and asked again if wished
        if (.ai_is_asked_again(msg) && !isTRUE(overwrite)) {
          shiny::showModal(shiny::modalDialog(
            title = t("A report is asked for already"), easyClose = TRUE,
            shiny::p(class = "small", msg),
            shiny::p(t("Write its request again (overwrite)? Its earlier request is replaced; the other requests and the answers are kept.")),
            footer = shiny::tagList(
              shiny::modalButton(t("Cancel")),
              .btn(ns("write_overwrite"), paste0(t("Write it again"), " (overwrite)"),
                   class = "btn-primary"))))
        } else {
          message("tflplanner: ", msg)
          notify(.error_view(msg, t), "error")
        }
        return()
      }
      if (!is.null(out$study)) {
        prep_for(out$study$path)
        adopt(out$study)
      } else {
        # (the study's TOC may have new reports, spec/toc.yml: taken in)
        reload_now()
      }
      prepared(out$prepared)
      collected(NULL)
      ver(ver() + 1L)
      r <- out$prepared$requests
      notify(sprintf(t("%d requests written in ai/requests (the longest %s characters)."),
                     nrow(r), format(max(r$chars, 0L, na.rm = TRUE), big.mark = ",")))
      bslib::nav_select("step", "requests", session = session)
    }
    shiny::observeEvent(input$write, {
      if (isTRUE(dirty())) return(notify(t("Save first: the study has unsaved changes."), "warning"))
      shiny::req(prepared())
      if (isTRUE(stale())) return(notify(t(.ai_stale_msg), "warning"))
      if (is_new()) {
        msg <- .ai_new_study_problem(input$new_id, t = t)
        if (!is.null(msg)) return(notify(msg, "warning"))
        return(write_now())
      }
      r <- requests()
      if (is.null(r) || !nrow(r) || isTRUE(input$add)) return(write_now())
      shiny::showModal(shiny::modalDialog(
        title = t("Write the requests again"), easyClose = TRUE,
        shiny::p(sprintf(t("ai/requests holds %d requests: they are written again (the answers are kept)."),
                         nrow(r))),
        footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                                .btn(ns("write_ok"), t("Write the requests"), class = "btn-primary"))))
    })
    shiny::observeEvent(input$write_ok, {
      shiny::removeModal()
      write_now()
    })
    shiny::observeEvent(input$write_overwrite, {
      shiny::removeModal()
      write_now(overwrite = TRUE)
    })
    # Write: after a read, never over unsaved changes
    shiny::observe({
      session$sendCustomMessage("ai-disable", list(id = ns("write"),
                                                   on = isTRUE(dirty()) || is.null(prepared()) ||
                                                     isTRUE(stale())))
    })
    output$write_note <- shiny::renderUI({
      if (isTRUE(dirty())) return(shiny::span(class = "small text-warning", t("Save first")))
      if (is.null(prepared())) return(shiny::span(class = "small text-muted", t("Read first.")))
      if (isTRUE(stale())) shiny::span(class = "small text-warning", t("Read again"))
    })
    output$prep_status <- shiny::renderUI({
      p <- prepared()
      if (is.null(p)) return(NULL)
      if (isTRUE(stale())) return(shiny::div(class = "small mt-2 text-warning", t(.ai_stale_msg)))
      r <- to_write()
      had <- if (!is_new()) requests() else NULL
      shiny::div(
        class = "small mt-2",
        if (isTRUE(p$dry_run)) {
          if (!is.null(p$counts)) sprintf(t("%d requests to write (%d shells, %d TOC items)."), nrow(r),
                                           p$counts[["shells"]], p$counts[["toc_items"]])
          else sprintf(t("%d requests to write (%d reports)."), nrow(r), sum(r$n))
        }
        else sprintf(t("%d requests written; the longest %s characters."), nrow(r),
                     format(max(r$chars, 0L, na.rm = TRUE), big.mark = ",")),
        if (isTRUE(p$dry_run) && !is.null(had) && nrow(had) && !isTRUE(input$add)) shiny::span(
          class = "text-muted ms-1",
          sprintf(t("ai/requests holds %d already (written again; the answers kept)."), nrow(had))),
        if (isTRUE(p$dry_run) && isTRUE(input$add) && !is_new()) {
          again <- .ai_already_asked(path(), p$toc$toc_id %||% p$toc$output_id)
          if (nrow(again)) shiny::div(
            class = "text-warning",
            sprintf(t("Asked for already: %s. Write the requests will ask whether to write them again (overwrite)."),
                    paste(sprintf("%s (%s)", again$output_id, again$request), collapse = ", ")))
          else shiny::span(class = "text-muted ms-1",
                           sprintf(t("Added to the %d requests there (their answers kept)."), nrow(had)))
        })
    })

    # what was read: the TOC, the ADaM summary, the sheets
    output$prep_empty <- shiny::renderUI({
      e <- prep_error()
      if (!is.null(e)) return(shiny::div(class = "alert alert-danger py-2 small", .error_view(e, t)))
      if (is.null(prepared())) shiny::p(class = "small text-muted", t("Read first."))
    })
    output$toc_head <- shiny::renderUI({
      p <- prepared()
      shiny::req(p)
      tc <- p$toc
      id <- tc$toc_id %||% tc$output_id
      no <- id[!tc$shell_found]
      n <- p$counts
      shiny::tagList(
        if (!nrow(tc)) shiny::div(
          class = "alert alert-danger py-2 small",
          sprintf(t("No TOC was found: name its sheet in (toc_sheet). The sheets: %s"),
                  paste(p$sheets$sheet, collapse = ", "))),
        shiny::p(class = "small mb-1",
                 if (!is.null(n)) sprintf(t("%d shells, %d TOC items, %d of them with no shell"),
                                          n[["shells"]], n[["toc_items"]], n[["no_shell"]])
                 else sprintf(t("%d reports, %d with a shell"), nrow(tc), sum(tc$shell_found))),
        if (length(no)) shiny::div(
          class = "alert alert-warning py-2 small",
          sprintf(t("%d TOC items with no shell (%s): their request says no shell. If their shell is a sheet read as something else, set its role on the Sheets tab and read again."),
                  length(no), paste(no, collapse = ", "))),
        if (length(p$hint)) shiny::div(class = "alert alert-warning py-2 small",
                                       lapply(p$hint, shiny::div)))
    })
    # (drawn again each time its tab is opened: a table drawn while hidden
    # can keep its old rows on the page)
    toc_seen <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$prep_tab, if (identical(input$prep_tab, "toc")) toc_seen(toc_seen() + 1L))
    output$toc_table <- DT::renderDT({
      toc_seen()
      p <- prepared()
      shiny::req(p, nrow(p$toc))
      tc <- p$toc
      col <- function(v) if (is.null(v)) rep("", nrow(tc)) else ifelse(is.na(v), "", v)
      d <- data.frame(tc$toc_id %||% tc$output_id, col(tc$title), col(tc$type),
                      ifelse(tc$shell_found, paste("\u2713", col(tc$shell)), "\u2014"),
                      col(tc$request), col(tc$book), col(tc$info),
                      stringsAsFactors = FALSE)
      names(d) <- c(t("TOC item"), t("title"), t("type"), "shell", t("request"), t("Workbook"),
                    t("Other information"))
      dt <- DT::datatable(d, rownames = FALSE, selection = "none",
                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
      # (a TOC item with no shell: its shell cell in red)
      DT::formatStyle(dt, "shell", backgroundColor = DT::styleEqual("\u2014", "#f8d7da"),
                      color = DT::styleEqual("\u2014", "#842029"))
    }, server = FALSE)
    # (the heading drawn also while its tab is hidden: an Apply on another
    # tab keeps that tab open, and the counts must follow the numbers)
    shiny::outputOptions(output, "toc_head", suspendWhenHidden = FALSE)
    # the shells: a sheet each, the TOC items it was taken for
    output$shells_head <- shiny::renderUI({
      p <- prepared()
      shiny::req(p, !is.null(p$shells))
      g <- p$shells$sheet[p$shells$guessed %in% TRUE]
      shiny::tagList(
        shiny::p(class = "small mb-1",
                 t("One request a shell sheet: the TOC items it holds go in it (candidates: the items the sheet was matched with, and why).")),
        if (length(g)) shiny::div(
          class = "alert alert-warning py-2 small",
          sprintf(t("%d shells with no number and no TOC item of their own (%s): the AI is asked to say what they are."),
                  length(g), paste(g, collapse = ", "))))
    })
    output$shells_table <- DT::renderDT({
      p <- prepared()
      shiny::req(p, !is.null(p$shells), nrow(p$shells))
      sh <- p$shells
      col <- function(v) if (is.null(v)) rep("", nrow(sh)) else ifelse(is.na(v), "", as.character(v))
      d <- data.frame(col(sh$sheet), col(sh$request), col(sh$numbers_in_sheet), col(sh$title_in_sheet),
                      ifelse(sh$guessed %in% TRUE, "?", ""), col(sh$candidates), col(sh$book),
                      stringsAsFactors = FALSE)
      names(d) <- c(t("sheet"), t("request"), t("Numbers in the sheet"), t("Title in the sheet"),
                    t("No number"), t("Candidates"), t("Workbook"))
      DT::datatable(d, rownames = FALSE, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    }, server = FALSE)
    output$adam_head <- shiny::renderUI({
      p <- prepared()
      shiny::req(p)
      ds <- p$adam$datasets
      shiny::div(
        class = "small text-muted mb-1",
        sprintf(t("%d datasets, %d variables; no records are sent."), nrow(ds), sum(ds$variables)),
        " ",
        sprintf(t("A variable's values are shown up to %d (max_levels); the data itself is never sent, and the data files go by their names only."),
                used_levels() %||% 15L))
    })
    output$adam_table <- DT::renderDT({
      p <- prepared()
      shiny::req(p)
      v <- p$adam$variables
      keep <- input$datasets
      if (length(keep)) v <- v[v$dataset %in% keep, , drop = FALSE] else v <- v[0, , drop = FALSE]
      d <- v[intersect(c("dataset", "variable", "label", "kind", "levels"), names(v))]
      d[is.na(d)] <- ""
      DT::datatable(d, rownames = FALSE, selection = "none",
                    options = list(dom = "tp", pageLength = 50, ordering = FALSE, scrollX = TRUE))
    }, server = FALSE)
    # the sheets' roles as read (toc / shell / other), changed here: each
    # read after a change says every sheet's (tfl_ai_prepare(sheet_roles = ))
    roles_set <- shiny::reactiveVal(NULL)
    roles_now <- function() {
      r <- roles_set()
      if (is.null(r)) NULL else r
    }
    output$roles_ui <- shiny::renderUI({
      p <- prepared()
      shiny::req(p, nrow(p$sheets))
      sh <- p$sheets
      pick <- function(id, label, role) shiny::selectizeInput(
        ns(id), label, choices = sh$sheet, selected = sh$sheet[sh$role %in% role], multiple = TRUE,
        width = "100%")
      shiny::div(
        class = "small mb-2",
        shiny::p(class = "mb-1", t("What each sheet is read as. Move a sheet, then Apply and read again: a TOC sheet read as a shell (a topline TOC), a shell read as other (its number not in the TOC).")),
        shiny::div(class = "d-flex flex-wrap gap-2",
                   shiny::div(style = "flex: 1 1 12rem;", pick("role_toc", t("TOC sheets"), "toc")),
                   shiny::div(style = "flex: 1 1 12rem;", pick("role_other", t("Other sheets (not shells)"), "other"))),
        shiny::p(class = "text-muted mb-1", t("The rest are shells.")),
        .btn(ns("roles_apply"), t("Apply and read again"), class = "btn-sm btn-outline-primary"))
    })
    shiny::observeEvent(input$roles_apply, {
      p <- prepared()
      shiny::req(p)
      sh <- p$sheets$sheet
      r <- stats::setNames(rep("shell", length(sh)), sh)
      r[intersect(input$role_other, sh)] <- "other"
      r[intersect(input$role_toc, sh)] <- "toc"
      roles_set(r)
      read_now()
    })
    output$sheets_table <- DT::renderDT({
      p <- prepared()
      shiny::req(p)
      DT::datatable(p$sheets, rownames = FALSE, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    }, server = FALSE)

    # -- a coding agent (mode = "agent") ---------------------------------------
    # The study's mode: what was written last here, else the study's
    # AGENTS.md (tfl_ai_prepare(mode = "agent") writes it)
    # (one way now: the agent writes the SPEC drafts)
    mode_now <- shiny::reactive("agent")
    output$mode_now <- shiny::renderText(mode_now())
    shiny::outputOptions(output, "mode_now", suspendWhenHidden = FALSE)
    # the agent's prompt: tfl_ai_prepare()'s, else made again from the
    # study's folder (the app opened again)
    agent_prompt <- function() {
      pr <- shiny::isolate(prepared())
      if (!is.null(pr) && !isTRUE(pr$dry_run) && length(pr$agent_prompt) &&
          !is.na(pr$agent_prompt)) return(pr$agent_prompt)
      .ai_agent_prompt(path())
    }
    # the one prompt for the agent: it works through the requests in the
    # folder's order, asks in its chat, stops and goes on there ("go on")
    output$agent_card <- shiny::renderUI({
      if (is.null(path())) return(NULL)
      ver()
      prepared()
      bslib::card(
        class = "mb-3",
        bslib::card_header(
          class = "d-flex flex-wrap gap-2 align-items-center",
          shiny::span(class = "me-auto", with_tip(
            t("The prompt"),
            t("One prompt for the whole study: paste it into the agent's chat. The agent does the requests below in their order and asks there; to stop and go on, say so in the chat."))),
          shiny::HTML(act("agent", "agent", c(copy = t("Copy the prompt"), view = t("View as text"))))),
        shiny::tags$pre(class = "small mb-0 ai-prompt", agent_prompt()))
    })
    output$check_note_agent <- shiny::renderUI({
      if (isTRUE(dirty())) shiny::span(class = "small text-warning", t("Save first"))
    })
    shiny::observe({
      session$sendCustomMessage("ai-disable", list(id = ns("status_reread"), on = isTRUE(dirty())))
    })
    # Check: the drafts read and checked again (the reports taken in)
    shiny::observeEvent(input$status_reread, check_all())
    # A report's row of the check, opened: what its draft's `ai:` block says
    # -- what the agent assumed, what it left to confirm -- read only; a
    # place to confirm opens the report in the builder, its card in view
    # (the report copied into the study first)
    ai_block <- function(id, file) {
      cf <- collected()$confirm
      cf <- if (is.null(cf)) NULL else cf[cf$output_id == id, , drop = FALSE]
      f <- if (!is.na(file) && nzchar(file)) file.path(path(), file)
      y <- if (!is.null(f) && file.exists(f)) tryCatch(tflspec::tfl_read_output(f)$ai, error = function(e) NULL)
      asm <- y$assumptions %||% list()
      # (copied into the study: its own spec/outputs/<id>.yml -- the check
      # reads the draft, ai/spec/outputs, either way)
      adopted <- file.exists(file.path(path(), "spec", "outputs", paste0(id, ".yml")))
      txt <- function(x) if (is.null(x)) "" else paste(as.character(unlist(x)), collapse = " ")
      shiny::div(
        class = "ms-2",
        if (length(asm)) shiny::tagList(
          shiny::div(class = "fw-bold mt-1", paste0(t("What the agent assumed"), " (ai: assumptions)")),
          shiny::tags$ul(class = "mb-1", lapply(asm, function(a) shiny::tags$li(
            shiny::span(class = "text-muted", txt(a$where)), " ", shiny::code(txt(a$item)), " ",
            txt(a$assumption),
            if (length(a$from)) shiny::span(class = "badge text-bg-info ms-1",
                                            sprintf(t("from %s"), txt(a$from))))))),
        if (!is.null(cf) && nrow(cf)) shiny::tagList(
          shiny::div(class = "fw-bold mt-1", paste0(t("To confirm"), " (ai: confirm)")),
          shiny::tags$ul(class = "mb-1", lapply(seq_len(nrow(cf)), function(i) shiny::tags$li(
            shiny::span(class = "text-muted", cf$where[i] %||% ""), " ", shiny::code(cf$item[i] %||% ""), " ",
            cf$why[i] %||% "", " ",
            if (adopted) shiny::tags$button(
              type = "button", class = "btn btn-sm btn-outline-primary py-0 ai-open",
              `data-ns` = ns(""), `data-id` = id,
              `data-item` = if (identical(cf$where[i], "content")) cf$item[i] else "",
              t("Open in Make a report"))
            else shiny::span(class = "text-muted", t("Copy it into the study first (step 4)")))))),
        if (!length(asm) && (is.null(cf) || !nrow(cf)))
          shiny::div(class = "text-muted", t("Nothing assumed, nothing to confirm.")))
    }
    output$result_all <- shiny::renderUI({
      co <- collected()
      if (is.null(co)) {
        return(shiny::p(class = "small text-muted mt-2",
                        t("Check reads the agent's SPEC drafts. Every report the AI drafted is in step 4.")))
      }
      r <- co$reports
      cls <- c(ok = "text-success", warning = "text-warning", error = "text-danger",
               `no answer` = "text-muted")
      cf <- co$confirm
      n_cf <- function(id) if (is.null(cf)) 0L else sum(cf$output_id == id)
      lv_cls <- c(OK = "text-success", WARNING = "text-warning", ERROR = "text-danger",
                  `no draft` = "text-muted")
      # an ERROR or WARNING row's fix text, for the agent's chat (its
      # request's: one report a request)
      # each report's level and its fix text, tflspec's check of the drafts
      # (tfl_check_reports(), tfl_check_fix_text()); the collect's status
      # where the check said nothing of a report
      ck <- co$checks$reports
      fixes <- co$checks$fix
      level_of <- function(i) {
        k <- if (is.null(ck)) NA else match(r$output_id[i], ck$output_id)
        if (!is.na(k)) ck$level[k] else .ai_check_level(r$status[i])
      }
      fix_btn <- function(i) {
        id <- r$output_id[i]
        if (length(fixes) && id %in% names(fixes) && !is.na(fixes[[id]])) {
          return(shiny::HTML(act("fix", id, c(copy = t("Copy the fix text"), view = t("View")))))
        }
        if (!r$status[i] %in% c("error", "warning") || is.na(r$repair[i])) return(NULL)
        shiny::HTML(act("repair", r$request[i], c(copy = t("Copy the fix text"), view = t("View"))))
      }
      shiny::tagList(
        if (!is.null(co$time)) shiny::div(class = "small text-muted mt-2",
                                          sprintf(t("Checked %s"), format(co$time, "%Y-%m-%d %H:%M"))),
        shiny::tags$table(
          class = "table table-sm small mt-2 ai-result-all",
          shiny::tags$thead(shiny::tags$tr(lapply(c("output_id", t("Check"), t("To confirm"),
                                                    t("first problem"), ""), shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(r)), function(i) {
            k <- n_cf(r$output_id[i])
            shiny::tagList(
              shiny::tags$tr(
                shiny::tags$td(r$output_id[i]),
                shiny::tags$td(class = { lc <- unname(lv_cls[level_of(i)]); if (is.na(lc)) "" else lc },
                               t(level_of(i))),
                shiny::tags$td(if (k) shiny::span(class = "text-warning", sprintf(t("%d to confirm"), k)) else ""),
                shiny::tags$td(if (is.na(r$first[i])) "" else r$first[i]),
                shiny::tags$td(fix_btn(i))),
              # (opened: what the draft's ai: block says)
              if (!r$status[i] %in% "no answer") shiny::tags$tr(shiny::tags$td(
                colspan = 5, class = "border-0 pt-0",
                shiny::tags$details(
                  shiny::tags$summary(class = "text-muted", t("What the agent assumed and left to confirm")),
                  ai_block(r$output_id[i], r$file[i])))))
          }))))
    })

    list(requests = requests, collected = collected, prepared = prepared,
         mode = mode_now,
         text = function(id, what = "request") shiny::isolate(text_of(what, id)))
  })
}
