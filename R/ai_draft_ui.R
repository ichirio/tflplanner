# ============================================================================
#  The AI drafts tab (#431): tflspec's AI table requests and answers
# ----------------------------------------------------------------------------
#  A module, in three steps: 1 Prepare (tfl_ai_prepare(); to come), 2 the
#  requests (ai/requests: copy each into a new chat, or view it), 3 the
#  answers (each pasted and saved, all checked with
#  tfl_ai_collect_answers(); a report's status, the next step, the fix
#  request to copy, the assumptions).  A check writes the reports' files
#  (spec/outputs/<id>.yml): the study is read again from them at once
#  (`reload_now`), so the new reports are on the Report list.  Not while
#  there are unsaved changes.  The functions it calls know no Shiny
#  (R/ai_draft.R).  Behind options(tflplanner.ai_drafts = TRUE) until its
#  place is decided.
# ============================================================================

# the tab is shown (off by default, until its place is decided)
.ai_drafts_on <- function() isTRUE(getOption("tflplanner.ai_drafts", FALSE))

#' @noRd
ai_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(x) tr(x, lang)
  shiny::tagList(
    shiny::tags$style(shiny::HTML(.ai_draft_css)),
    shiny::tags$script(shiny::HTML(.ai_draft_js)),
    shiny::div(class = "small mb-2 ai-status", shiny::uiOutput(ns("status_line"))),
    bslib::navset_card_underline(
      id = ns("step"), selected = "requests",
      bslib::nav_panel(
        paste0("1 ", t("Prepare"), " (tfl_ai_prepare)"), value = "prepare",
        shiny::p(class = "small text-muted",
                 t("For now the requests are written in R: tflspec::tfl_ai_prepare(<study folder>, <TOC / SHELL workbook>); then Read again on step 2."))),
      bslib::nav_panel(
        paste0("2 ", t("Requests"), " (ai/requests)"), value = "requests",
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-2",
          shiny::h6(class = "mb-0 me-auto", with_tip(
            paste0(t("Requests"), " (ai/requests)"),
            t("Paste each request into a NEW chat as one message; the chat's whole answer goes to step 3."))),
          .btn(ns("open_requests"), t("Open the folder")),
          .btn(ns("reread"), t("Read again"))),
        shiny::uiOutput(ns("instructions")),
        DT::DTOutput(ns("requests"))),
      bslib::nav_panel(
        paste0("3 ", t("Answers"), " (tfl_ai_collect_answers)"), value = "answers",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(3, 9)),
          shiny::div(
            shiny::radioButtons(ns("pick"), t("Requests"), choices = character(),
                                selected = character(), width = "100%"),
            shiny::div(
              class = "d-flex flex-column gap-1 align-items-start",
              .btn(ns("check"), paste0(t("Check all answers"), " (tfl_ai_collect_answers)"),
                   class = "btn-sm btn-primary"),
              shiny::uiOutput(ns("check_note")),
              .btn(ns("open_answers"), t("Open ai/answers")))),
          shiny::div(
            shiny::uiOutput(ns("answer_head")),
            shiny::textAreaInput(ns("answer_text"), NULL, value = "", rows = 14,
                                 width = "100%", resize = "vertical",
                                 placeholder = t("Paste the chat's whole answer (from the first ``` to the last)")),
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-center mb-2",
              .btn(ns("save"), paste0(t("Save the answer"), " (tfl_ai_save_answer)"),
                   class = "btn-sm btn-outline-primary"),
              .btn(ns("save_check"), t("Save and check"), class = "btn-sm btn-primary"),
              shiny::uiOutput(ns("saved"), inline = TRUE)),
            shiny::uiOutput(ns("result")))),
        shiny::uiOutput(ns("stray")),
        bslib::card(
          bslib::card_header(
            class = "d-flex flex-wrap gap-2 align-items-center",
            shiny::span(class = "me-auto",
                        paste0(t("Assumptions"), " (spec/_assumptions.yml)")),
            shiny::selectInput(ns("assume_of"), NULL, choices = character(),
                               width = "220px", selectize = FALSE)),
          shiny::uiOutput(ns("assume_empty")),
          DT::DTOutput(ns("assumptions"))))))
}

# The module's server.  `rv` the app's values (rv$study: the study open);
# `has_study()`, `dirty()` (unsaved changes), `reload_now()` (the study
# read again from its files; FALSE when it was not), `notify(msg, type)`,
# `guarded(expr)` the app's.  Gives list(requests, collected, text) (the
# tests read them).
#' @noRd
ai_server <- function(id, rv, has_study, dirty, reload_now, notify, guarded, lang = "en") {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(x) tr(x, lang)
    ns <- session$ns
    # the study's folder: a save makes a new copy of the study, not a new folder
    path <- shiny::reactive(if (has_study()) rv$study$path else NULL)
    ver <- shiny::reactiveVal(0L)
    collected <- shiny::reactiveVal(NULL)
    # another study opened: its own check, none yet
    was <- NULL
    shiny::observe({
      p <- path()
      if (!identical(p, was)) collected(NULL)
      was <<- p
    })

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
      if (identical(what, "instructions")) {
        return(.ai_read_text(file.path(p, .ai_requests_dir, "00-instructions.md")))
      }
      .ai_request_text(p, id, if (identical(what, "repair")) "repair" else "request")
    }
    file_of <- function(what, id) {
      switch(what, instructions = "00-instructions.md",
             repair = .ai_rel(.ai_repairs_dir, paste0(id, ".md")),
             paste0(id, ".md"))
    }

    # -- the status line ----------------------------------------------------
    output$status_line <- shiny::renderUI({
      if (!has_study()) return(shiny::span(class = "text-muted", t("Open a study.")))
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
                sprintf(t("answers %d / %d"), n$answers, n$requests), st),
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
    output$instructions <- shiny::renderUI({
      r <- requests()
      if (is.null(r) || is.na(attr(r, "instructions"))) return(NULL)
      chars <- tryCatch(nchar(text_of("instructions", NA)), error = function(e) NA_integer_)
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center small mb-2",
        shiny::tags$b("00-instructions.md"),
        shiny::span(class = "text-muted", t("(paste first in each new chat)")),
        shiny::span(sprintf(t("%s characters"), format(chars, big.mark = ","))),
        shiny::HTML(act("instructions", "00-instructions")))
    })
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
        answer = ifelse(is.na(r$answer), "\u2014", format(r$answer_time, "%m/%d %H:%M")),
        actions = vapply(r$request, function(q) act("request", q), ""),
        stringsAsFactors = FALSE)
      names(d) <- c(t("request"), t("family"), t("reports"), t("outputs"), t("characters"),
                    t("status"), t("answer"), "")
      DT::datatable(d, rownames = FALSE, escape = -8, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                   scrollX = TRUE))
    }, server = FALSE)
    shiny::observeEvent(input$reread, ver(ver() + 1L))
    shiny::observeEvent(input$open_requests, {
      shiny::req(path())
      guarded(.open_folder(file.path(path(), .ai_requests_dir)))
    })
    shiny::observeEvent(input$open_answers, {
      shiny::req(path())
      guarded(.open_folder(file.path(path(), .ai_answers_dir)))
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

    # -- step 3: the answers -------------------------------------------------
    shiny::observe({
      r <- requests()
      q <- if (is.null(r)) character() else r$request
      st <- .ai_status_label(q, collected()$reports, t)
      if (length(q)) st[is.na(st)] <- ifelse(is.na(r$answer[is.na(st)]), t("no answer"), t("saved"))
      now <- shiny::isolate(input$pick)
      shiny::updateRadioButtons(
        session, "pick",
        choiceNames = lapply(seq_along(q), function(i) shiny::span(
          shiny::tags$b(q[i]), " ", shiny::span(class = "small text-muted", st[i]))),
        choiceValues = q,
        selected = if (!is.null(now) && now %in% q) now else q[1L])
    })
    # the box: the answer saved, when another request is chosen
    shiny::observeEvent(list(input$pick, path()), {
      q <- input$pick
      p <- path()
      if (is.null(p) || is.null(q) || !nzchar(q)) return()
      x <- .ai_answer_text(p, q)
      shiny::updateTextAreaInput(session, "answer_text", value = as.vector(x))
      saved_info(list(request = q, problem = attr(x, "problem")))
    })
    saved_info <- shiny::reactiveVal(NULL)
    output$answer_head <- shiny::renderUI({
      r <- requests()
      q <- input$pick
      if (is.null(r) || !nrow(r)) {
        return(shiny::p(class = "text-muted",
                        if (has_study()) t("No requests yet: step 1.") else t("Open a study.")))
      }
      shiny::req(q %in% r$request)
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center mb-1",
        shiny::tags$b(q), shiny::span(class = "text-muted", sprintf("(%s)", r$outputs[r$request == q])),
        shiny::HTML(act("request", q, c(view = t("View the request")))))
    })
    output$saved <- shiny::renderUI({
      ver()
      s <- saved_info()
      p <- path()
      if (is.null(s) || is.null(p)) return(NULL)
      if (!is.null(s$problem)) {
        return(shiny::span(class = "small text-danger", s$problem))
      }
      f <- .ai_answer_file(p, s$request)
      if (is.na(f)) return(NULL)
      shiny::span(class = "small text-muted", sprintf(t("saved: %s  %s (UTF-8)"), f,
                                                      format(file.mtime(file.path(p, f)), "%m/%d %H:%M")))
    })
    save_answer <- function() {
      q <- input$pick
      txt <- input$answer_text %||% ""
      if (is.null(path()) || is.null(q) || !nzchar(q)) return(FALSE)
      if (!nzchar(trimws(txt))) {
        notify(t("Paste first."), "warning")
        return(FALSE)
      }
      f <- guarded(.ai_save_answer(path(), q, txt))
      if (is.null(f)) return(FALSE)
      saved_info(list(request = q))
      ver(ver() + 1L)
      notify(sprintf(t("Saved: %s"), f))
      TRUE
    }
    check_all <- function() {
      shiny::req(path())
      if (isTRUE(dirty())) {
        notify(t("Save first: the study has unsaved changes."), "warning")
        return()
      }
      co <- guarded(.ai_collect(path()))
      if (is.null(co)) return()
      collected(co)
      ver(ver() + 1L)
      # the reports written: the study read from its files at once (no save
      # between, which would see them as made outside the app)
      if (!isTRUE(reload_now())) return()
      ok <- unique(co$reports$output_id[co$reports$status %in% c("ok", "warning")])
      notify(if (length(ok)) sprintf(t("%d reports taken in: %s (Report list)"), length(ok),
                                     paste(ok, collapse = ", "))
             else t("No report could be taken in: see each request's next step."))
    }
    shiny::observeEvent(input$save, save_answer())
    shiny::observeEvent(input$save_check, if (save_answer()) check_all())
    shiny::observeEvent(input$check, check_all())
    # no check over unsaved changes
    shiny::observe({
      session$sendCustomMessage("ai-disable", list(id = ns("check"), on = isTRUE(dirty())))
    })
    output$check_note <- shiny::renderUI({
      if (isTRUE(dirty())) shiny::span(class = "small text-warning", t("Save first"))
    })

    output$result <- shiny::renderUI({
      co <- collected()
      q <- input$pick
      if (is.null(co) || is.null(q)) {
        return(shiny::p(class = "small text-muted mt-2", t("Check first.")))
      }
      r <- co$reports[co$reports$request == q, , drop = FALSE]
      if (!nrow(r)) return(NULL)
      nxt <- unname(co$next_steps[q])
      cls <- c(ok = "text-success", warning = "text-warning", error = "text-danger",
               `no answer` = "text-muted")
      rep_btn <- if (any(!is.na(r$repair)))
        shiny::HTML(act("repair", q, c(copy = t("Copy the fix request"), view = t("View"))))
      shiny::div(
        class = "mt-2",
        shiny::h6(t("Result of the last check")),
        shiny::tags$table(
          class = "table table-sm small mb-1",
          shiny::tags$thead(shiny::tags$tr(lapply(c(t("output_id"), t("status"), t("first problem")),
                                                  shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(r)), function(i) shiny::tags$tr(
            shiny::tags$td(r$output_id[i]),
            shiny::tags$td(class = cls[[r$status[i]]] %||% "", t(r$status[i])),
            shiny::tags$td(if (is.na(r$first[i])) "" else r$first[i]))))),
        if (length(nxt) && !is.na(nxt)) shiny::p(class = "small mb-1",
                                                shiny::tags$b(t("next:")), " ", nxt),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          rep_btn,
          if (any(r$problems > 0))
            .btn(ns("problems"), t("All problems"), class = "btn-sm btn-outline-secondary py-0")))
    })
    shiny::observeEvent(input$problems, {
      co <- collected()
      q <- input$pick
      shiny::req(co, q)
      r <- co$reports[co$reports$request == q & !is.na(co$reports$problems_text), , drop = FALSE]
      txt <- paste(sprintf("%s (%s)\n%s", r$output_id, r$status, r$problems_text), collapse = "\n\n")
      shiny::showModal(shiny::modalDialog(
        title = sprintf(t("%s: the problems"), q), size = "l", easyClose = TRUE,
        shiny::tags$textarea(class = "form-control rp-code ai-view", readonly = NA, rows = 16, txt),
        footer = shiny::modalButton(t("Close"))))
    })

    # the assumptions the answers made, by report
    shiny::observe({
      a <- collected()$assumptions
      ids <- if (is.null(a)) character() else sort(unique(a$output_id[!is.na(a$output_id)]))
      shiny::updateSelectInput(
        session, "assume_of",
        choices = c(stats::setNames("", t("(all reports)")),
                    stats::setNames(".study", t("(study)")), stats::setNames(ids, ids)),
        selected = shiny::isolate(input$assume_of) %||% "")
    })
    output$assume_empty <- shiny::renderUI({
      a <- collected()$assumptions
      if (is.null(collected())) return(shiny::p(class = "small text-muted", t("Check first.")))
      if (!nrow(a)) return(shiny::p(class = "small text-muted", t("No assumptions.")))
      NULL
    })
    output$assumptions <- DT::renderDT({
      a <- collected()$assumptions
      shiny::req(a, nrow(a))
      f <- input$assume_of %||% ""
      if (identical(f, ".study")) a <- a[is.na(a$output_id), , drop = FALSE]
      else if (nzchar(f)) a <- a[!is.na(a$output_id) & a$output_id == f, , drop = FALSE]
      a$output_id[is.na(a$output_id)] <- t("(study)")
      a[is.na(a)] <- ""
      names(a) <- t(names(a))
      DT::datatable(a, rownames = FALSE, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
    }, server = FALSE)
    output$stray <- shiny::renderUI({
      s <- collected()$stray
      if (!length(s)) return(NULL)
      shiny::div(class = "alert alert-warning py-2 small mt-2",
                 sprintf(t("Not read (no request of that name): %s -- rename it to a request's name."),
                         paste(s, collapse = ", ")))
    })

    list(requests = requests, collected = collected,
         text = function(id, what = "request") shiny::isolate(text_of(what, id)))
  })
}

.ai_draft_css <- "
.ai-view { font-family: var(--bs-font-monospace); font-size: .8rem; white-space: pre; }
.ai-status b { margin-right: .5rem; }
"

# The tab's buttons in the list (Copy / View, by data-*): the server is
# told which; the text it sends is written to the clipboard, else the View
# dialog is asked for.  In the dialog: select all, copy (the clipboard,
# else the browser's copy command).
.ai_draft_js <- "
(function() {
  var last = null;
  function mark(b) {
    if (!b) return;
    var was = b.textContent;
    b.textContent = '\\u2713';
    setTimeout(function() { b.textContent = was; }, 1200);
  }
  $(document).on('click', '.ai-act', function() {
    var b = this;
    if (b.dataset.kind === 'copy') last = b;
    Shiny.setInputValue(b.dataset.ns + 'act',
      {kind: b.dataset.kind, what: b.dataset.what, id: b.dataset.id, n: Date.now()},
      {priority: 'event'});
  });
  function areaCopy(ta, b) {
    ta.focus(); ta.select();
    var ok = false;
    try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
    if (ok) mark(b);
    else if (navigator.clipboard && window.isSecureContext)
      navigator.clipboard.writeText(ta.value).then(function() { mark(b); }, function() {});
  }
  $(document).on('click', '.ai-select', function() {
    var ta = document.getElementById(this.dataset.target);
    if (ta) { ta.focus(); ta.select(); }
  });
  $(document).on('click', '.ai-copy-area', function() {
    var ta = document.getElementById(this.dataset.target);
    if (ta) areaCopy(ta, this);
  });
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('ai-copy', function(x) {
      var b = last;
      last = null;
      var fail = function() {
        Shiny.setInputValue(x.failed, {what: x.what, id: x.id, n: Date.now()}, {priority: 'event'});
      };
      if (!navigator.clipboard || !window.isSecureContext) return fail();
      navigator.clipboard.writeText(x.text).then(function() { mark(b); }, fail);
    });
    Shiny.addCustomMessageHandler('ai-disable', function(x) {
      var b = document.getElementById(x.id);
      if (b) b.disabled = !!x.on;
    });
  });
})();
"
