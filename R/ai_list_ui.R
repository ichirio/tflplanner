# The AI drafts tab's step 4, the list (tflspec::tfl_ai_outputs()): every
# report the AI was asked for, its draft (ai/spec/outputs) and whether it
# is adopted into the study (spec/outputs).  A row's RTF opens from here;
# the rows ticked are adopted (tflspec::tfl_ai_adopt()) after a dialog
# that says what each one does -- new, replace (with what changes), same,
# no draft -- and the study is read again, so the adopted reports are on
# the Report list.  Design: _archive/ai-list-corrections-tab-design-2026-10-26.md.

# The list as the table shows it (NULL: no study, or tflspec cannot list)
.ai_list <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(NULL)
  d <- tryCatch(suppressWarnings(tflspec::tfl_ai_outputs(dir)), error = function(e) NULL)
  if (!is.data.frame(d)) return(NULL)
  for (k in c("output_id", "title", "population", "split", "status", "adopted", "rtf", "problem",
              "questions", "file", "toc_id", "request")) {
    if (is.null(d[[k]])) d[[k]] <- rep(NA_character_, nrow(d))
  }
  d
}

# What adopting the rows would do (tflspec >= 0.0.24.9237: dry_run), else
# NULL: output_id, status (new / replace / same / no draft)
.ai_adopt_plan <- function(dir, ids) {
  if (!"dry_run" %in% names(formals(tflspec::tfl_ai_adopt))) return(NULL)
  p <- tryCatch(tflspec::tfl_ai_adopt(dir, outputs = ids, dry_run = TRUE), error = function(e) NULL)
  if (!is.data.frame(p)) return(NULL)
  p
}

# The differences of the reports adopting replaces (tflspec's
# tfl_ai_adopted_diff(), when it has it), else NULL
.ai_adopt_diff <- function(dir, ids) {
  f <- tryCatch(getExportedValue("tflspec", "tfl_ai_adopted_diff"), error = function(e) NULL)
  if (is.null(f) || !length(ids)) return(NULL)
  d <- tryCatch(do.call(rbind, lapply(ids, function(id) f(dir, id))), error = function(e) NULL)
  if (is.data.frame(d) && nrow(d)) d else NULL
}

#' @noRd
ai_list_panel <- function(ns, t) {
  bslib::nav_panel(
    paste0("4 ", t("List and corrections"), " (tfl_ai_outputs)"), value = "list",
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2",
      shiny::h6(class = "mb-0 me-auto", with_tip(
        paste0(t("The AI's reports"), " (tfl_ai_outputs)"),
        t("Every report the AI was asked for: its draft (ai/spec/outputs) and whether it is in the study (spec/outputs). Tick the rows to adopt; the Report list shows the adopted ones."))),
      shiny::uiOutput(ns("list_count"), inline = TRUE),
      .btn(ns("list_reread"), t("Read again")),
      .btn(ns("adopt"), paste0(t("Adopt the rows ticked"), " (tfl_ai_adopt)"), class = "btn-sm btn-primary")),
    DT::DTOutput(ns("list_table")),
    shiny::tags$script(shiny::HTML(
      "$(document).off('click.airtf').on('click.airtf', '.ai-open-rtf', function(e) {
         e.preventDefault();
         Shiny.setInputValue(this.dataset.ns + 'open_rtf', {file: this.dataset.file, n: Date.now()},
                             {priority: 'event'});
       });")))
}

# The server part: `path()` the study's folder, `ver()` bumped when the
# answers are checked again, `reload_now()` the app's
#' @noRd
ai_list_server <- function(input, output, session, path, ver, reload_now, notify, guarded, t) {
  ns <- session$ns
  again <- shiny::reactiveVal(0L)
  shiny::observeEvent(input$list_reread, again(again() + 1L))
  listed <- shiny::reactive({
    ver()
    again()
    .ai_list(path())
  })
  output$list_count <- shiny::renderUI({
    d <- listed()
    if (is.null(d)) return(NULL)
    n_draft <- sum(!is.na(d$file) & nzchar(d$file))
    n_adopt <- sum(startsWith(as.character(d$adopted %||% ""), "yes"), na.rm = TRUE)
    shiny::span(class = "small text-muted", sprintf(t("%d reports, %d drafts, %d adopted"),
                                                    nrow(d), n_draft, n_adopt))
  })
  output$list_table <- DT::renderDT({
    d <- listed()
    if (is.null(d) || !nrow(d)) {
      return(.dt(data.frame(x = t("No report listed yet: write the requests (step 1) and check the answers (step 3).")),
                 colnames = ""))
    }
    rtf <- ifelse(!is.na(d$rtf) & nzchar(d$rtf), sprintf(
      "<a href=\"#\" class=\"ai-open-rtf\" data-ns=\"%s\" data-file=\"%s\">%s</a>",
      ns(""), htmltools::htmlEscape(d$rtf, attribute = TRUE), t("Open")), "")
    v <- data.frame(d$output_id, d$title, d$population, d$split, d$status, d$adopted, rtf,
                    d$questions, d$problem, stringsAsFactors = FALSE)
    names(v) <- c("output_id", t("Title"), t("Analysis set"), t("Split"), t("State"),
                  paste0(t("Adopted"), " (adopted)"), "RTF", t("Questions"), t("First problem"))
    v[is.na(v)] <- ""
    DT::datatable(v, rownames = FALSE, escape = setdiff(seq_along(v) - 1L, 6L),
                  selection = "multiple",
                  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
  })
  shiny::observeEvent(input$open_rtf, {
    f <- input$open_rtf$file %||% ""
    p <- path()
    if (!nzchar(f) || is.null(p)) return()
    full <- if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(p, f)
    if (!file.exists(full)) return(notify(sprintf(t("No file %s."), f), "warning"))
    opener <- getOption("tflplanner.open_folder", .external_browser)
    opener(normalizePath(full))
  })
  picked <- shiny::reactive({
    d <- listed()
    i <- input$list_table_rows_selected
    if (is.null(d) || !length(i)) character() else unique(d$output_id[i])
  })
  shiny::observeEvent(input$adopt, {
    ids <- picked()
    if (!length(ids)) return(notify(t("Tick the rows to adopt."), "warning"))
    plan <- .ai_adopt_plan(path(), ids)
    diff <- if (!is.null(plan)) .ai_adopt_diff(path(), plan$output_id[plan$status == "replace"])
    plan_ui <- if (is.null(plan)) {
      shiny::p(paste(ids, collapse = ", "))
    } else {
      p <- data.frame(plan$output_id, t(as.character(plan$status)), stringsAsFactors = FALSE)
      names(p) <- c("output_id", t("What adopting does"))
      shiny::tags$table(class = "table table-sm small",
                        shiny::tags$thead(shiny::tags$tr(lapply(names(p), shiny::tags$th))),
                        shiny::tags$tbody(lapply(seq_len(nrow(p)), function(r)
                          shiny::tags$tr(lapply(p[r, ], shiny::tags$td)))))
    }
    diff_ui <- if (!is.null(diff)) shiny::tags$details(
      shiny::tags$summary(class = "small", sprintf(t("What replacing changes (%d)"), nrow(diff))),
      shiny::tags$table(class = "table table-sm small",
                        shiny::tags$thead(shiny::tags$tr(lapply(
                          c("output_id", t("Where"), t("Change"), t("Adopted"), t("Draft")), shiny::tags$th))),
                        shiny::tags$tbody(lapply(seq_len(nrow(diff)), function(r) shiny::tags$tr(lapply(
                          diff[r, c("output_id", "where", "change", "adopted", "draft")], shiny::tags$td))))))
    replace <- is.null(plan) || any(plan$status == "replace")
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Adopt %d reports into the study"), length(ids)),
      plan_ui, diff_ui,
      if (replace) shiny::checkboxInput(ns("adopt_overwrite"),
                                        paste0(t("Replace the reports already adopted"), " (overwrite)"),
                                        FALSE),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(ns("adopt_ok"), t("Adopt"), class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$adopt_ok, {
    shiny::removeModal()
    ids <- picked()
    out <- guarded(tflspec::tfl_ai_adopt(path(), outputs = ids,
                                         overwrite = isTRUE(input$adopt_overwrite)))
    if (is.null(out)) return()
    again(again() + 1L)
    reload_now()
    notify(sprintf(t("%d reports adopted: on the Report list."), length(ids)))
  })
  invisible(listed)
}

# Whether a report's file (from the study folder) is the study's own,
# spec/outputs -- not a draft in the AI's area, ai/spec/outputs
.ai_in_study_spec <- function(f) {
  f <- as.character(f)
  !is.na(f) & startsWith(gsub("\\\\", "/", f), "spec/")
}
