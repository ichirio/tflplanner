# ============================================================================
#  The AI drafts tab (#431): tflspec's AI table requests and answers
# ----------------------------------------------------------------------------
#  A module, in three steps: 1 Prepare (tfl_ai_prepare(): the TOC / SHELL
#  workbook and the ADaM folder read, what is sent looked at, the requests
#  written -- into the open study or a new one), 2 the
#  requests (ai/requests: copy each into a new chat, or view it), 3 the
#  answers (each pasted and saved, all checked with
#  tfl_ai_collect_answers(); a report's status, the next step, the fix
#  request to copy, the assumptions).  A check writes the reports' files
#  (spec/outputs/<id>.yml): the study is read again from them at once
#  (`reload_now`), so the new reports are on the Report list.  Not while
#  there are unsaved changes.  The functions it calls know no Shiny
#  (R/ai_draft.R).  options(tflplanner.ai_drafts = FALSE) hides the tab.
# ============================================================================

# the tab is shown (options(tflplanner.ai_drafts = FALSE) hides it)
.ai_drafts_on <- function() !isFALSE(getOption("tflplanner.ai_drafts", TRUE))

# A path typed, with Browse... (the computer's dialog) beside it when one
# can be opened: an input group, as tall as shiny's upload field
.ai_path_input <- function(id, label, placeholder, browse_id, browse_label) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(class = "control-label", id = paste0(id, "-label"), `for` = id, label),
    shiny::div(
      class = "input-group",
      shiny::tags$input(id = id, type = "text", class = "form-control", value = "",
                        placeholder = placeholder),
      if (.ai_dialog_ok()) shiny::actionButton(browse_id, browse_label,
                                               class = "btn-outline-secondary")))
}

#' @noRd
ai_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(x) tr(x, lang)
  shiny::tagList(
    shiny::tags$style(shiny::HTML(.ai_draft_css)),
    shiny::tags$script(shiny::HTML(.ai_draft_js)),
    shiny::div(class = "small mb-2 ai-status", shiny::uiOutput(ns("status_line"))),
    bslib::navset_card_underline(
      id = ns("step"), selected = "prepare",
      bslib::nav_panel(
        paste0("1 ", t("Prepare"), " (tfl_ai_prepare)"), value = "prepare",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
          shiny::div(
            class = "ai-form",
            shiny::radioButtons(
              ns("mode"), paste0(t("Who drafts"), " (mode)"), inline = TRUE,
              choiceNames = list(t("A chat (chat)"), t("A coding agent (agent)")),
              choiceValues = c("chat", "agent"), selected = "agent"),
            shiny::radioButtons(
              ns("study_mode"), t("Study"), inline = TRUE,
              choiceNames = list(t("The open study"), paste0(t("New study"), " (tfl_ai_study_init)")),
              choiceValues = c("open", "new"), selected = "open"),
            shiny::uiOutput(ns("open_name")),
            shiny::conditionalPanel(
              "input.study_mode == 'new'", ns = ns,
              shiny::div(
                class = "d-flex flex-wrap gap-2",
                shiny::textInput(ns("new_id"), "ID (study_id)", width = "12rem",
                                 placeholder = "ABC-123"),
                shiny::textInput(ns("new_title"), paste0(t("Title"), " (title)"),
                                 width = "20rem")),
              shiny::uiOutput(ns("new_where"))),
            # the workbook: its path -- Browse... copies one from outside the
            # study into its docs/ -- or, with no dialog (a server), uploaded
            if (.ai_dialog_ok()) {
              .ai_path_input(ns("toc_path"),
                             with_tip(paste0(t("TOC / SHELL workbook"), " (toc)"),
                                      t("Browse... or type its path. A workbook outside the study folder is copied into the study's docs/ when it is chosen with Browse..., and that copy is read; one inside the study is read where it is.")),
                             "C:/.../TOC_SHELL.xlsx", ns("toc_browse"), t("Browse..."))
            } else {
              shiny::div(
                class = "d-flex flex-wrap gap-2 align-items-start",
                shiny::div(style = "flex: 1 1 14rem;",
                           shiny::fileInput(ns("toc_file"),
                                            with_tip(paste0(t("TOC / SHELL workbook"), " (toc)"),
                                                     paste(t("Upload or path: the same workbook is read. Whether a copy is kept in the study's ai/ (what was read, beside the requests): an upload, yes; a path inside the study folder (its docs/, say), no -- the study already holds it; a path outside the study folder, yes."),
                                                           t("The upload's dialog is the browser's: where it opens cannot be chosen."))),
                                            accept = c(".xlsx", ".xlsm"), width = "100%",
                                            buttonLabel = t("Browse..."),
                                            placeholder = t("No file selected"))),
                shiny::div(style = "flex: 1 1 14rem;",
                           .ai_path_input(ns("toc_path"), t("or its path"), "C:/.../TOC_SHELL.xlsx",
                                          ns("toc_browse"), t("Browse..."))))
            },
            shiny::uiOutput(ns("toc_check")),
            # the shells in workbooks of their own (none: in the TOC's)
            if (.ai_dialog_ok()) {
              shiny::div(
                class = "d-flex gap-2 align-items-end",
                shiny::div(style = "flex: 1 1 auto;",
                           shiny::textAreaInput(ns("shell_paths"),
                                                with_tip(paste0(t("Shell workbooks"), " (shells)"),
                                                         t("Only when the shells are not in the TOC workbook. Browse... (several at once), or their paths, one a line. A workbook outside the study is copied into its docs/ when chosen with Browse...")),
                                                rows = 1, width = "100%", resize = "vertical",
                                                placeholder = "C:/.../SHELLS.xlsx")),
                shiny::div(class = "mb-3",
                           .btn(ns("shells_browse"), t("Browse..."), class = "btn-outline-secondary")))
            } else {
              shiny::div(
                class = "d-flex flex-wrap gap-2 align-items-end",
                shiny::div(style = "flex: 1 1 14rem;",
                           shiny::fileInput(ns("shell_files"),
                                            with_tip(paste0(t("Shell workbooks"), " (shells)"),
                                                     paste(t("Only when the shells are not in the TOC workbook; several can be given."),
                                                           t("The upload's dialog is the browser's: where it opens cannot be chosen."))),
                                            multiple = TRUE, accept = c(".xlsx", ".xlsm"),
                                            width = "100%", buttonLabel = t("Browse..."),
                                            placeholder = t("No file selected"))),
                shiny::div(style = "flex: 1 1 14rem;",
                           shiny::textAreaInput(ns("shell_paths"), t("or their paths"),
                                                rows = 1, width = "100%", resize = "vertical",
                                                placeholder = "C:/.../SHELLS.xlsx")))
            },
            shiny::uiOutput(ns("shells_check")),
            .ai_path_input(ns("adam_dir"), paste0(t("ADaM folder"), " (adam)"), "C:/.../adam",
                           ns("adam_browse"), t("Browse...")),
            shiny::uiOutput(ns("adam_files")),
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-end",
              shiny::selectInput(ns("toc_sheet"), paste0(t("TOC sheet"), " (toc_sheet)"),
                                 choices = stats::setNames("", t("guess")), width = "12rem",
                                 selectize = FALSE),
              shiny::selectizeInput(ns("sheets"),
                                    with_tip(paste0(t("Sheets for every request"), " (sheets)"),
                                             t("Usually blank: each request carries the shell sheets found for its reports (the TOC table's shell column). Choose sheets only to add them whole to every request -- a sheet of notes common to all, or the shell of a report none was found for. Several can be chosen.")),
                                    choices = character(), multiple = TRUE, width = "16rem",
                                    options = list(placeholder = t("none (the shells found)")))),
            # (fields side by side: their boxes level, whatever their labels' lines)
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-end",
              # (a coding agent: one a request, the instructions read once)
              shiny::conditionalPanel(
                "input.mode == 'chat'", ns = ns,
                shiny::numericInput(ns("size"), paste0(t("Shells a request holds"), " (size)"),
                                    value = 1, min = 1, max = 20, width = "12rem")),
              shiny::selectInput(ns("language"),
                                 with_tip(paste0(t("Language of the AI's assumption notes"), " (language)"),
                                          t("The language of the notes in which the AI explains what it decided where the shell does not say (its assumptions). Not the language of the reports' titles and labels (those stay as in the shell).")),
                                 choices = stats::setNames(c("Japanese", "English", ""),
                                                           c(t("Japanese"), t("English"), t("the chat's"))),
                                 selected = if (identical(lang, "ja")) "Japanese" else "English",
                                 width = "14rem", selectize = FALSE)),
            shiny::checkboxInput(ns("add"), paste0(t("Add to the earlier requests"), " (add)"),
                                 value = FALSE),
            shiny::checkboxInput(ns("templates"), paste0(t("Use the templates"), " (templates)"),
                                 value = TRUE),
            shiny::conditionalPanel(
              "input.mode == 'chat'", ns = ns,
              shiny::checkboxInput(
                ns("instructions_each"),
                with_tip(paste0(t("Put the common instructions in each request"), " (instructions = \"each\")"),
                         t("Off (the default): the common instructions are written once, to 00-instructions.md -- paste it first -- and left out of the requests. On: every request carries them.")),
                value = FALSE)),
            shiny::tags$details(
              class = "small mb-2",
              shiny::tags$summary(t("More")),
              shiny::div(
                class = "d-flex flex-wrap gap-2 mt-1 align-items-end",
                shiny::selectInput(ns("workbook"), paste0(t("The workbook in the chat"), " (workbook)"),
                                   choices = c("text", "attach", "both"), width = "12rem",
                                   selectize = FALSE),
                shiny::numericInput(ns("max_chars"), paste0(t("A request's length at most"), " (max_chars)"),
                                    value = NA, min = 1000, step = 1000, width = "14rem"))),
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-center",
              .btn(ns("read"), paste0(t("Read and check"), " (dry_run)"), class = "btn-sm btn-outline-primary"),
              help_tip(t("Reads the workbook and the ADaM and writes nothing; what was read shows on the right. Check there: every report found its shell (TOC: the shell column), the reports in parts, the types, how the reports go into requests; the ADaM summary that is sent (no records); the sheets and what each was taken for. Then Write the requests.")),
              .btn(ns("write"), t("Write the requests"), class = "btn-sm btn-primary"),
              shiny::uiOutput(ns("write_note"), inline = TRUE)),
            shiny::uiOutput(ns("prep_status"))),
          bslib::card(
            bslib::card_header(t("What was read (dry_run = TRUE)")),
            shiny::uiOutput(ns("prep_empty")),
            bslib::navset_underline(
              id = ns("prep_tab"),
              bslib::nav_panel(t("TOC items"), value = "toc",
                               shiny::uiOutput(ns("toc_head")),
                               DT::DTOutput(ns("toc_table"))),
              bslib::nav_panel(t("Shells"), value = "shells",
                               shiny::uiOutput(ns("shells_head")),
                               DT::DTOutput(ns("shells_table"))),
              bslib::nav_panel(t("ADaM summary (what is sent)"), value = "adam",
                               shiny::div(
                                 class = "d-flex flex-wrap gap-3 align-items-end mt-2",
                                 shiny::checkboxGroupInput(
                                   ns("datasets"), paste0(t("Datasets to send"), " (datasets)"),
                                   choices = character(), inline = TRUE),
                                 shiny::numericInput(
                                   ns("max_levels"), paste0(t("Values a variable shows"), " (max_levels)"),
                                   value = 15, min = 1, max = 100, width = "12rem")),
                               shiny::uiOutput(ns("adam_head")),
                               DT::DTOutput(ns("adam_table"))),
              bslib::nav_panel(t("Sheets"), value = "sheets",
                               shiny::uiOutput(ns("roles_ui")),
                               DT::DTOutput(ns("sheets_table"))))))),
      bslib::nav_panel(
        paste0("2 ", t("Requests"), " (ai/requests)"), value = "requests",
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-2",
          shiny::h6(class = "mb-0 me-auto", with_tip(
            paste0(t("Requests"), " (ai/requests)"),
            t("Agent: give the agent the first instruction above; it reads these requests itself, in their order. Chat: paste each request into a NEW chat as one message; the chat's whole answer goes to step 3."))),
          .btn(ns("open_requests"), t("Open the folder")),
          .btn(ns("reread"), t("Read again"))),
        shiny::uiOutput(ns("agent_card")),
        shiny::uiOutput(ns("instructions_file")),
        DT::DTOutput(ns("requests"))),
      bslib::nav_panel(
        paste0("3 ", t("Answers"), " (tfl_ai_collect_answers)"), value = "answers",
        # (the study's mode, for the page: the agent's cards or the chat's paste)
        shiny::div(class = "d-none", shiny::textOutput(ns("mode_now"))),
        # (the last check: what was taken in, and step 4 to adopt it)
        shiny::uiOutput(ns("taken_note")),
        shiny::conditionalPanel(
          "output.mode_now == 'agent'", ns = ns,
          bslib::card(
            bslib::card_header(
              class = "d-flex flex-wrap gap-2 align-items-center",
              shiny::span(class = "me-auto", paste0(t("Status"), " (ai/status.md)")),
              .btn(ns("status_reread"), t("Read the status again"), class = "btn-sm btn-primary"),
              shiny::uiOutput(ns("check_note_agent"), inline = TRUE)),
            shiny::uiOutput(ns("status_md")),
            shiny::uiOutput(ns("result_all"))),
          # (the questions: in step 4, with the list -- R/ai_list_ui.R)
          shiny::p(class = "small text-muted", t("The agent's questions are answered in step 4."))),
        shiny::conditionalPanel(
          "output.mode_now != 'agent'", ns = ns,
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
            shiny::uiOutput(ns("result"))))),
        shiny::uiOutput(ns("stray")),
        shiny::uiOutput(ns("compare_card")),
        bslib::card(
          bslib::card_header(
            class = "d-flex flex-wrap gap-2 align-items-center",
            shiny::span(class = "me-auto",
                        paste0(t("Assumptions"), " (spec/_assumptions.yml)")),
            shiny::selectInput(ns("assume_of"), NULL, choices = character(),
                               width = "220px", selectize = FALSE)),
          shiny::uiOutput(ns("assume_empty")),
          DT::DTOutput(ns("assumptions")))),
      # step 4: the reports, adopted into the study (R/ai_list_ui.R)
      ai_list_panel(ns, t)))
}

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
    compared <- shiny::reactiveVal(NULL)
    # step 4: the list and the adopting (R/ai_list_ui.R)
    ai_list_server(input, output, session, path, ver, reload_now, notify, guarded, t)
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
      if (identical(what, "tell")) {
        return(t("I answered the questions in ai/questions.md. Read it again and go on."))
      }
      if (identical(what, "instructions")) {
        return(.ai_read_text(file.path(p, .ai_requests_dir, "00-instructions.md")))
      }
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
                       sprintf("- %s: %s", u$output_id, u$reason)), collapse = "
"))
      }
      if (identical(what, "repair") && !is.null(kept[id]) && !is.na(kept[id]) &&
          !file.exists(file.path(p, .ai_repairs_dir, paste0(id, ".md")))) {
        return(kept[[id]])
      }
      .ai_request_text(p, id, if (identical(what, "repair")) "repair" else "request")
    }
    file_of <- function(what, id) {
      switch(what, instructions = "00-instructions.md",
             agent = t("The agent's first message"), tell = t("To tell the agent"),
             repair = .ai_rel(.ai_repairs_dir, paste0(id, ".md")),
             build = .ai_rel(.ai_repairs_dir, paste0(id, "-build.md")),
             unbuilt = .ai_rel(.ai_repairs_dir, paste0(
               id, if (id %in% attr(shiny::isolate(unbuilt()), "build")) "-build.md" else ".md")),
             rerun = t("To rebuild only this request"),
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
    output$instructions_file <- shiny::renderUI({
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
        book = ifelse(is.na(r$book), "", r$book),
        actions = vapply(r$request, function(q) act("request", q), ""),
        stringsAsFactors = FALSE)
      names(d) <- c(t("request"), t("family"), t("reports"), t("outputs"), t("characters"),
                    t("status"), t("answer"), t("Workbook"), "")
      DT::datatable(d, rownames = FALSE, escape = -9, selection = "none",
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
      # no request: the list emptied, no value sent (updateRadioButtons()
      # would send its first choice, an NA: null in the browser)
      if (!length(q)) {
        session$sendInputMessage("pick", list(options = '<div class="shiny-options-group"></div>'))
        return()
      }
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
      keep_unbuilt()
      co <- guarded(.ai_collect(path()))
      if (is.null(co)) return()
      # (compared before the study is read again: it checks the answers again)
      compared(.ai_compare(path()))
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
        if (k$drafts) .btn(ns("to_list2"), paste0(t("To step 4"), " (", t("List and corrections"), ")"),
                           class = "btn-sm btn-primary"))
    })
    shiny::observeEvent(input$to_list2, bslib::nav_select("step", "list", session = session))
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
            shiny::tags$td(if (is.na(r$first[i])) "" else r$first[i]),
            shiny::tags$td(if (r$status[i] %in% c("ok", "warning") && .ai_in_study_spec(r$file[i]))
              shiny::tags$button(
                type = "button", class = "btn btn-sm btn-outline-primary py-0 ai-open",
                `data-ns` = ns(""), `data-id` = r$output_id[i], t("Open in Make a report"))))))),
        if (length(nxt) && !is.na(nxt)) shiny::p(class = "small mb-1",
                                                shiny::tags$b(t("next:")), " ", nxt),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          rep_btn,
          if (any(co$reports$status %in% c("ok", "warning")))
            .btn(ns("to_list"), paste0("4 ", t("List and corrections")),
                 class = "btn-sm btn-outline-secondary py-0"),
          if (any(r$problems > 0))
            .btn(ns("problems"), t("All problems"), class = "btn-sm btn-outline-secondary py-0")))
    })
    shiny::observeEvent(input$to_list, bslib::nav_select("step", "list", session = session))
    shiny::observeEvent(input$open_report, open_report(input$open_report$id))
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
    # the answers compared (tfl_ai_compare(): ai/compare.csv), under the checks
    output$compare_card <- shiny::renderUI({
      if (is.null(collected()) || is.null(compared())) return(NULL)
      bslib::card(
        bslib::card_header(paste0(t("Compare the answers"), " (compare.csv)")),
        DT::DTOutput(ns("compare")))
    })
    output$compare <- DT::renderDT({
      d <- compared()
      shiny::req(d)
      DT::datatable(d, rownames = FALSE, selection = "none",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    })
    output$stray <- shiny::renderUI({
      s <- collected()$stray
      if (!length(s)) return(NULL)
      shiny::div(class = "alert alert-warning py-2 small mt-2",
                 sprintf(t("Not read (no request of that name): %s -- rename it to a request's name."),
                         paste(s, collapse = ", ")))
    })

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
    # the reports a request holds: 1 for a coding agent (the default), 3 for a chat
    # (tflspec's); a number changed by hand kept when the mode changes
    # (the first mode seen: its default the number put in by itself)
    size_auto <- NULL
    shiny::observeEvent(input$mode, {
      d <- .ai_size_default(input$mode)
      now <- input$size
      same <- function(a, b) !is.null(b) && isTRUE(all.equal(as.numeric(a), b))
      if (is.null(now) || is.na(now) || same(now, size_auto) ||
          (is.null(size_auto) && same(now, d))) {
        if (!same(now, d)) shiny::updateNumericInput(session, "size", value = d)
        size_auto <<- d
      }
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
      d <- guarded(.ai_choose_dir(browse_start("adam", input$adam_dir)))
      if (length(d) == 1L && !is.na(d)) shiny::updateTextInput(session, "adam_dir", value = d)
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
      f <- guarded(.ai_choose_file(browse_start("toc", input$toc_path)))
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
      f <- guarded(.ai_choose_files(browse_start("shells", input$shell_paths)))
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
                 sprintf(t("Read: %s (copied into the study's ai/ when the requests are written)"),
                         src$name))
    })
    # the reports a request holds as the field says (the mode's default when
    # it is empty)
    size_now <- function() {
      size <- input$size
      if (is.null(size) || !length(size) || is.na(size)) size <- .ai_size_default(input$mode)
      as.integer(size)
    }
    # what the read was for: the workbooks and their sheets (changed, the
    # read is out of date: read again); the size it counted the requests at
    read_key <- function() {
      list(toc = toc_src()$path, shells = vapply(shells_src(), `[[`, "", "path"),
           toc_sheet = nz(input$toc_sheet), sheets = sort(as.character(input$sheets)))
    }
    read_with <- shiny::reactiveVal(NULL)
    read_size <- shiny::reactiveVal(NULL)
    # (a sheet-first read, tflspec >= 0.0.24.9230: its requests group the
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
    args_of <- function(p) {
      all <- if (!is.null(p)) p$adam$datasets$dataset
      ds <- input$datasets
      mc <- input$max_chars
      # (always given: the mode's default when the field is empty)
      size <- size_now()
      list(toc_sheet = nz(input$toc_sheet), sheets = if (length(input$sheets)) input$sheets,
           size = as.integer(size), language = nz(input$language),
           workbook = input$workbook %||% "text", instructions = if (isTRUE(input$instructions_each)) "each" else "once",
           max_chars = if (is.null(mc) || is.na(mc)) NULL else mc,
           datasets = if (!is.null(all) && length(ds) && !setequal(ds, all)) ds,
           max_levels = as.integer(input$max_levels %||% 15L),
           mode = input$mode %||% "agent",
           templates = !isFALSE(input$templates),
           add = isTRUE(input$add) && !is_new(),
           sheet_roles = roles_now())
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
    read_now <- function() read_again(read_again() + 1L)
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
      bslib::nav_select("prep_tab", if (nrow(p$toc) && all(p$toc$shell_found)) "adam" else "toc",
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
      # (tflspec >= 0.0.24.9211 writes a request at a time and says where it is)
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
    output$toc_table <- DT::renderDT({
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
    mode_now <- shiny::reactive({
      ver()
      p <- path()
      pr <- prepared()
      if (!is.null(pr) && !isTRUE(pr$dry_run) && !is.null(pr$mode)) return(pr$mode)
      .ai_mode(p)
    })
    output$mode_now <- shiny::renderText(mode_now())
    shiny::outputOptions(output, "mode_now", suspendWhenHidden = FALSE)
    # the agent's first message: tfl_ai_prepare()'s, else made again from
    # the study's folder (the app opened again)
    agent_prompt <- function() {
      pr <- shiny::isolate(prepared())
      if (!is.null(pr) && !isTRUE(pr$dry_run) && length(pr$agent_prompt) &&
          !is.na(pr$agent_prompt)) return(pr$agent_prompt)
      .ai_agent_prompt(path())
    }
    output$agent_card <- shiny::renderUI({
      if (!identical(mode_now(), "agent") || is.null(path())) return(NULL)
      ver()
      prepared()
      bslib::card(
        class = "mb-3",
        bslib::card_header(
          class = "d-flex flex-wrap gap-2 align-items-center",
          shiny::span(class = "me-auto", t("The agent's first message")),
          shiny::HTML(act("agent", "agent", c(copy = t("Copy text"), view = t("View as text"))))),
        shiny::tags$pre(class = "small mb-0 ai-prompt", agent_prompt()))
    })
    output$check_note_agent <- shiny::renderUI({
      if (isTRUE(dirty())) shiny::span(class = "small text-warning", t("Save first"))
    })
    shiny::observe({
      session$sendCustomMessage("ai-disable", list(id = ns("status_reread"), on = isTRUE(dirty())))
    })
    status_ver <- shiny::reactiveVal(0L)
    # the status run.R wrote, read again, and the answers checked (the
    # reports taken in, as Check all does)
    shiny::observeEvent(input$status_reread, {
      status_ver(status_ver() + 1L)
      q_ver(q_ver() + 1L)
      check_all()
    })
    output$status_md <- shiny::renderUI({
      status_ver()
      ver()
      p <- path()
      shiny::req(p)
      f <- file.path(p, "ai", "status.md")
      if (!file.exists(f)) {
        return(shiny::p(class = "small text-muted", t("No ai/status.md yet: run.R writes it.")))
      }
      txt <- tryCatch(.ai_read_text(f), error = function(e) conditionMessage(e))
      shiny::div(class = "small ai-status-md", shiny::markdown(txt))
    })
    output$result_all <- shiny::renderUI({
      co <- collected()
      if (is.null(co)) return(shiny::p(class = "small text-muted mt-2", t("Check first.")))
      r <- co$reports
      cls <- c(ok = "text-success", warning = "text-warning", error = "text-danger",
               `no answer` = "text-muted")
      u <- unbuilt()
      kept <- names(attr(u, "texts"))
      rerun <- .ai_run_has_requests()
      # a report that passed its checks but run.R could not build: the reason
      why <- function(i) {
        if (is.null(u) || !r$status[i] %in% c("ok", "warning")) return(NA_character_)
        x <- u$reason[u$request == r$request[i] & u$output_id == r$output_id[i]]
        if (length(x)) x[1L] else NA_character_
      }
      # a request's buttons, on its first row: its fix request, and the line
      # to rebuild it alone
      req_btns <- function(i) {
        q <- r$request[i]
        if (i > 1L && identical(r$request[i - 1L], q)) return(NULL)
        rows <- r$request == q
        fix <- any(!is.na(r$repair[rows])) || q %in% kept
        shiny::div(
          class = "d-flex flex-wrap gap-1 mt-1",
          if (fix) shiny::HTML(act("repair", q, c(copy = t("Copy the fix request"), view = t("View")))),
          if (q %in% attr(u, "build"))
            shiny::HTML(act("build", q, c(copy = t("Copy the fix request to rebuild"),
                                          view = t("View")))),
          if (rerun && any(r$status[rows] %in% c("ok", "warning")))
            shiny::HTML(act("rerun", q, c(copy = t("Copy: rebuild only this request")))))
      }
      shiny::tags$table(
        class = "table table-sm small mt-2 ai-result-all",
        shiny::tags$thead(shiny::tags$tr(lapply(c(t("request"), "output_id", t("status"),
                                                  t("first problem"), ""), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(r)), function(i) {
          w <- why(i)
          shiny::tags$tr(
            shiny::tags$td(r$request[i], req_btns(i)), shiny::tags$td(r$output_id[i]),
            shiny::tags$td(class = cls[[r$status[i]]] %||% "", t(r$status[i])),
            shiny::tags$td(
              if (!is.na(w)) shiny::tagList(
                shiny::span(class = "text-danger ai-unbuilt",
                            paste0(t("Could not be built:"), " ", .ai_short(w))), " ",
                shiny::HTML(act("unbuilt", r$request[i], c(view = t("View")))))
              else if (is.na(r$first[i])) "" else r$first[i]),
            shiny::tags$td(if (r$status[i] %in% c("ok", "warning") && .ai_in_study_spec(r$file[i]))
              shiny::tags$button(
                type = "button", class = "btn btn-sm btn-outline-primary py-0 ai-open",
                `data-ns` = ns(""), `data-id` = r$output_id[i], t("Open in Make a report"))))
        })))
    })

    # the questions (ai/questions.md): one a row, the open ones first, an
    # answer field each; written back with tfl_ai_answer_questions()
    q_ver <- shiny::reactiveVal(0L)
    questions <- shiny::reactive({
      q_ver()
      p <- path()
      if (is.null(p)) return(NULL)
      # (none yet: an agent writes ai/questions.md when it has questions;
      # a chat's questions are in its assumptions, step 3)
      if (!file.exists(file.path(p, "ai", "questions.md"))) {
        return(structure(data.frame(), why = if (identical(mode_now(), "agent")) "none yet" else "chat"))
      }
      tryCatch(.ai_questions(p), error = function(e) {
        message("tflplanner: ai/questions.md: ", conditionMessage(e))
        NULL
      })
    })
    q_input <- function(id) paste0("qa_", gsub("[^A-Za-z0-9_]", "_", id))
    # a question's status in the app's words (its own keys: "open" is a word
    # of other places too)
    q_status <- function(s) {
      v <- t(paste0("question-status:", s))
      if (startsWith(v, "question-status:")) s else v
    }
    output$questions <- shiny::renderUI({
      q <- questions()
      if (is.null(q) || !nrow(q)) {
        why <- attr(q, "why")
        return(shiny::p(class = "small text-muted", t(
          if (identical(why, "chat")) "No questions: a chat writes its questions in its assumptions (step 3)."
          else if (identical(why, "none yet")) "No questions yet: the agent writes ai/questions.md when it has some."
          else "No questions.")))
      }
      shiny::tagList(lapply(seq_len(nrow(q)), function(i) {
        open <- !identical(q$status[i], "decided")
        shiny::div(
          class = paste("ai-question border-bottom py-2", if (!open) "text-muted"),
          shiny::div(
            class = "d-flex flex-wrap gap-2 align-items-baseline",
            shiny::tags$b(q$id[i]),
            shiny::span(class = paste("badge", if (open) "text-bg-warning" else "text-bg-secondary"),
                        q_status(q$status[i])),
            shiny::span(class = "small", q$output_id[i]),
            if (!is.na(q$where[i]) && nzchar(q$where[i]))
              shiny::span(class = "small text-muted", q$where[i])),
          shiny::div(class = "mt-1", q$question[i]),
          if (!is.na(q$provisional[i]) && nzchar(q$provisional[i])) shiny::div(
            class = "small text-muted",
            shiny::tags$b(t("The agent took, for now:")), " ", q$provisional[i]),
          shiny::textAreaInput(ns(q_input(q$id[i])), NULL,
                               value = if (is.na(q$answer[i])) "" else q$answer[i],
                               rows = 2, width = "100%", resize = "vertical",
                               placeholder = t("Your answer")))
      }))
    })
    shiny::observeEvent(input$questions_write, {
      q <- questions()
      shiny::req(q, nrow(q))
      typed <- vapply(q$id, function(id) input[[q_input(id)]] %||% "", "")
      old <- ifelse(is.na(q$answer), "", q$answer)
      # only what was typed (an answer emptied here is left as it was)
      put <- nzchar(trimws(typed)) & trimws(typed) != trimws(old)
      if (!any(put)) return(notify(t("No new answer to write."), "warning"))
      f <- guarded(.ai_answer_questions(path(), stats::setNames(typed[put], q$id[put])))
      if (is.null(f)) return()
      q_ver(q_ver() + 1L)
      tell(TRUE)
      notify(sprintf(t("%d answers written to ai/questions.md: tell the agent."), sum(put)))
    })
    tell <- shiny::reactiveVal(FALSE)
    output$tell_agent <- shiny::renderUI({
      if (!isTRUE(tell())) return(NULL)
      shiny::HTML(act("tell", "tell", c(copy = t("Copy the line to tell the agent"))))
    })

    list(requests = requests, collected = collected, prepared = prepared,
         questions = questions, mode = mode_now,
         text = function(id, what = "request") shiny::isolate(text_of(what, id)))
  })
}

.ai_draft_css <- "
.ai-view { font-family: var(--bs-font-monospace); font-size: .8rem; white-space: pre; }
.ai-status b { margin-right: .5rem; }
.ai-prompt { white-space: pre-wrap; }
.ai-status-md h1, .ai-status-md h2, .ai-status-md h3 { font-size: 1rem; }
.ai-status-md table { border-collapse: collapse; margin-bottom: .5rem; }
.ai-status-md th, .ai-status-md td { padding: .15rem .6rem;
  border-bottom: 1px solid var(--bs-border-color); }
.ai-form .form-group { margin-bottom: .5rem; }
/* a path field as tall as the upload beside it (both an input group: the
   box and its button) */
.ai-form .input-group > .form-control, .ai-form .input-group .btn {
  height: calc(1.5em + .75rem + 2px); padding-top: .375rem; padding-bottom: .375rem; }
.ai-form .input-group .btn { display: inline-flex; align-items: center; }
.ai-form .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; }
.ai-form .d-flex > .shiny-input-container { width: auto; }
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
  $(document).on('click', '.ai-open', function() {
    Shiny.setInputValue(this.dataset.ns + 'open_report', {id: this.dataset.id, n: Date.now()},
                        {priority: 'event'});
  });
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
