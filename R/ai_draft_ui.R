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
                                               class = "btn-outline-secondary ai-browse")))
}

#' @noRd
ai_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(x) tr(x, lang)
  shiny::tagList(
    shiny::tags$style(shiny::HTML(.ai_draft_css)),
    shiny::tags$script(shiny::HTML(.ai_draft_js)),
    # (while the computer's dialog is open the app waits: said on the page,
    # until the server says it is closed)
    shiny::div(id = ns("browse_wait"), class = "ai-browse-wait",
               style = "display: none; position: fixed; top: 1rem; left: 50%; transform: translateX(-50%); z-index: 2000; max-width: 40rem;",
               shiny::div(class = "alert alert-info shadow",
                          t("The computer's dialog to choose a file or folder is open (it may be behind this window): choose there, or cancel it. The app waits until then."))),
    shiny::tags$script(shiny::HTML(sprintf(
      "$(document).off('click.aibrowse').on('click.aibrowse', '.ai-browse', function() { $('#%s').show(); });
       Shiny.addCustomMessageHandler('ai-browse-done', function(x) { $('#%s').hide(); });",
      ns("browse_wait"), ns("browse_wait")))),
    shiny::div(class = "small mb-2 ai-status", shiny::uiOutput(ns("status_line"))),
    bslib::navset_card_underline(
      id = ns("step"), selected = "prepare",
      # (the agent writes the SPEC drafts itself, asking in its chat:
      # tfl_ai_prepare(mode = "agent", route = "spec") -- the four steps:
      # the prompt, the agent's SPEC, its check, the copy into the study)
      bslib::nav_panel(
        paste0("1 ", t("Write the prompt"), " (tfl_ai_prepare)"), value = "prepare",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
          shiny::div(
            class = "ai-form",
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
                                                     paste(t("Upload or path: the same workbook is read. Whether a copy is kept in the study's docs/ (what was read): an upload, yes; a path inside the study folder (its docs/, say), no -- the study already holds it; a path outside the study folder, yes."),
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
              # (as the TOC's field: one input group, the button at the
              # field's top -- the same height and edges whatever the field
              # above shows, and when several paths make the field taller)
              shiny::div(
                class = "form-group shiny-input-container w-100",
                shiny::tags$label(class = "control-label", id = paste0(ns("shell_paths"), "-label"),
                                  `for` = ns("shell_paths"),
                                  with_tip(paste0(t("Shell workbooks"), " (shells)"),
                                           t("Only when the shells are not in the TOC workbook. Browse... (several at once), or their paths, one a line. A workbook outside the study is copied into its docs/ when chosen with Browse..."))),
                shiny::div(
                  class = "input-group align-items-start",
                  shiny::tags$textarea(id = ns("shell_paths"), class = "form-control textarea-autoresize", rows = 1,
                                       style = "resize: vertical;", placeholder = "C:/.../SHELLS.xlsx"),
                  shiny::actionButton(ns("shells_browse"), t("Browse..."), class = "btn-outline-secondary ai-browse",
                                      style = "align-self: flex-start;")))
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
            .ai_path_input(ns("adam_spec"),
                           paste0(t("ADaM spec (P21 Spec workbook or define.xml)"), " (adam_spec / define)"),
                           "C:/.../define.xml", ns("adam_spec_browse"), t("Browse...")),
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
              shiny::selectInput(ns("language"),
                                 with_tip(paste0(t("Language of the AI's questions and notes"), " (language)"),
                                          t("The language the agent asks in, in its chat, and writes its notes in (its assumptions, what is to confirm): study.yml's ai: language. Not the language of the reports' titles and labels (those stay as in the shell).")),
                                 choices = stats::setNames(c("Japanese", "English", ""),
                                                           c(t("Japanese"), t("English"), t("the chat's"))),
                                 selected = if (identical(lang, "ja")) "Japanese" else "English",
                                 width = "14rem", selectize = FALSE)),
            shiny::checkboxInput(ns("add"), paste0(t("Add to the earlier requests"), " (add)"),
                                 value = FALSE),
            shiny::checkboxInput(ns("templates"), paste0(t("Use the templates"), " (templates)"),
                                 value = TRUE),
            # what the agent may open besides its requests (tflspec's
            # agent_reads, docs, agent_max_reads): none unless chosen here
            shiny::div(
              shiny::checkboxGroupInput(
                ns("agent_reads"),
                with_tip(paste0(t("Files the agent may open"), " (agent_reads)"),
                         t("Off: the agent reads its requests only. The ADaM's needed information (variables, values, codes and their labels) is in the requests as a summary either way.")),
                choiceNames = list(t("The TOC / SHELL workbook"), t("The ADaM folder")),
                choiceValues = c("workbook", "adam"), selected = character(), inline = TRUE),
              shiny::numericInput(
                ns("agent_max_reads"),
                with_tip(paste0(t("Other files or commands a request may use, at most"), " (agent_max_reads)"),
                         t("Besides the request and the files it names. At the limit the agent writes what it does not know as assumptions and questions, and goes on.")),
                value = 3L, min = 0L, step = 1L, width = "14rem"),
              .ai_path_input(ns("ref_path"),
                             with_tip(paste0(t("Add a reference"), " (docs)"),
                                      t("Only the files chosen here (a SAP, say), named in AGENTS.md. One in the study folder is read where it is (a .docx / .pdf / .xlsx gets a text copy in ai/docs/); one from elsewhere is copied into ai/docs/. None: the agent reads none.")),
                             "C:/.../SAP.pdf", ns("ref_browse"), t("Browse...")),
              shiny::div(class = "d-flex gap-2 mb-2",
                         .btn(ns("ref_add"), t("Add"), class = "btn-sm btn-outline-secondary")),
              shiny::uiOutput(ns("ref_list"))),
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
                               DT::DTOutput(ns("toc_table"), fill = FALSE),
                               # (a TOC item with no shell put in a sheet: R/ai_numbers_ui.R)
                               shiny::uiOutput(ns("toc_attach"))),
              bslib::nav_panel(t("Shells"), value = "shells",
                               shiny::uiOutput(ns("shells_head")),
                               DT::DTOutput(ns("shells_table"), fill = FALSE),
                               # (the numbers in the sheets, boxes: R/ai_numbers_ui.R)
                               ai_numbers_ui(ns, t)),
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
        paste0("2 ", t("The agent writes the SPEC"), " (ai/requests)"), value = "requests",
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-2",
          shiny::h6(class = "mb-0 me-auto", with_tip(
            paste0(t("Requests"), " (ai/requests)"),
            t("Give the agent the first message above; it reads these requests itself, in their order, asks in its chat what it cannot decide, and writes each report's SPEC draft (ai/spec/outputs/<id>.yml). Everything else is between you and the agent, in its chat."))),
          .btn(ns("open_requests"), t("Open the folder")),
          .btn(ns("reread"), t("Read again"))),
        shiny::uiOutput(ns("agent_card")),
        DT::DTOutput(ns("requests"))),
      bslib::nav_panel(
        paste0("3 ", t("Check the SPEC"), " (tfl_check_spec)"), value = "answers",
        # (the last check: what was taken in, and step 4 to copy it)
        shiny::uiOutput(ns("taken_note")),
        bslib::card(
          bslib::card_header(
            class = "d-flex flex-wrap gap-2 align-items-center",
            shiny::span(class = "me-auto", with_tip(
              paste0(t("The agent's SPEC drafts"), " (ai/spec/outputs)"),
              t("Check reads every draft and checks it: ERROR, the SPEC does not read (it cannot be copied); WARNING, it reads but no program can be made from it yet; OK, a program can be made (whether the report is right is yours to see). An ERROR or WARNING row's fix text goes to the agent's chat. A row opened shows what the agent assumed and what it left to confirm."))),
            .btn(ns("status_reread"), paste0(t("Check"), " (tfl_ai_collect_answers, tfl_check_spec)"),
                 class = "btn-sm btn-primary"),
            shiny::uiOutput(ns("check_note_agent"), inline = TRUE)),
          shiny::uiOutput(ns("result_all")))),
      # step 4: the reports, adopted into the study (R/ai_list_ui.R)
      ai_list_panel(ns, t),
      # the look of past reports, for every study (R/ai_style_ui.R)
      ai_style_panel(ns, t)))
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
    Shiny.setInputValue(this.dataset.ns + 'open_report', {id: this.dataset.id, item: this.dataset.item || null, n: Date.now()},
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
