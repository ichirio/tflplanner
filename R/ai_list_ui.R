# The AI drafts tab's step 4, the list (tflspec::tfl_ai_outputs()): every
# report the AI was asked for, its draft (ai/spec/outputs) and whether it
# is adopted into the study (spec/outputs).  A row's RTF opens from here;
# the reports ticked -- at first the drafts not adopted yet -- are adopted
# (copied, tflspec::tfl_ai_adopt()); those the study has already are
# replaced only after a second question that says what may be lost, with
# what changes (tfl_ai_adopted_diff()).  The study is read again, so the
# adopted reports are on the Report list.
# Design: _archive/ai-list-corrections-tab-design-2026-10-26.md.

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
        t("Every report the AI was asked for: its draft (ai/spec/outputs) and whether it is in the study (spec/outputs). Adopt (copy) puts the reports ticked on the study's Report list. At first only the drafts not adopted yet are ticked."))),
      shiny::uiOutput(ns("list_count"), inline = TRUE),
      .btn(ns("list_reread"), t("Read again"))),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2",
      .btn(ns("list_all"), t("Tick all")),
      .btn(ns("list_none"), t("Untick all")),
      .btn(ns("list_newer"), t("Tick only the updated drafts")),
      shiny::uiOutput(ns("list_ticked"), inline = TRUE),
      .btn(ns("adopt"), paste0(t("Adopt (copy) the reports ticked into the study"), " (tfl_ai_adopt)"),
           class = "btn-sm btn-primary ms-auto")),
    shiny::uiOutput(ns("list_empty")),
    DT::DTOutput(ns("list_table")),
    shiny::tags$script(shiny::HTML(
      "$(document).off('click.airtf').on('click.airtf', '.ai-open-rtf', function(e) {
         e.preventDefault();
         Shiny.setInputValue(this.dataset.ns + 'open_rtf', {file: this.dataset.file, n: Date.now()},
                             {priority: 'event'});
       });
       $(document).off('change.aick').on('change.aick', '.ai-adopt-ck', function() {
         Shiny.setInputValue(this.dataset.ns + 'ck_toggle', {id: this.dataset.id, on: this.checked, n: Date.now()},
                             {priority: 'event'});
       });")))
}

# The rows a draft can be adopted from (its file, a state not an error)
# and, of them, the ones the list ticks at first: not adopted yet
.ai_adoptable <- function(d) {
  !is.na(d$file) & nzchar(d$file) & !(as.character(d$status) %in% c("error", "no answer"))
}
.ai_ticked_first <- function(d) {
  d$output_id[.ai_adoptable(d) & as.character(d$adopted) %in% c("no", NA)]
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
  # the reports ticked: at first the drafts not adopted yet; the boxes, the
  # buttons change it (the table drawn again only for the buttons)
  ticked <- shiny::reactiveVal(character())
  redraw <- shiny::reactiveVal(0L)
  shiny::observeEvent(listed(), {
    d <- listed()
    ticked(if (is.null(d)) character() else .ai_ticked_first(d))
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$ck_toggle, {
    id <- input$ck_toggle$id
    ticked(if (isTRUE(input$ck_toggle$on)) union(ticked(), id) else setdiff(ticked(), id))
  })
  set_ticked <- function(ids) {
    ticked(ids)
    redraw(redraw() + 1L)
  }
  shiny::observeEvent(input$list_all, {
    d <- listed()
    if (!is.null(d)) set_ticked(d$output_id[.ai_adoptable(d)])
  })
  shiny::observeEvent(input$list_none, set_ticked(character()))
  shiny::observeEvent(input$list_newer, {
    d <- listed()
    if (!is.null(d)) set_ticked(d$output_id[.ai_adoptable(d) & as.character(d$adopted) %in% "yes, draft newer"])
  })
  output$list_count <- shiny::renderUI({
    d <- listed()
    if (is.null(d) || !nrow(d)) return(NULL)
    n_draft <- sum(!is.na(d$file) & nzchar(d$file))
    n_adopt <- sum(startsWith(as.character(d$adopted %||% ""), "yes"), na.rm = TRUE)
    shiny::span(class = "small text-muted", sprintf(t("%d reports, %d drafts, %d adopted"),
                                                    nrow(d), n_draft, n_adopt))
  })
  output$list_ticked <- shiny::renderUI({
    if (is.null(listed())) return(NULL)
    shiny::span(class = "small", sprintf(t("%d ticked"), length(ticked())))
  })
  output$list_empty <- shiny::renderUI({
    d <- listed()
    if (is.null(d) || !nrow(d)) {
      shiny::p(class = "small text-muted",
               t("No report listed yet: write the requests (step 1) and check the answers (step 3)."))
    }
  })
  output$list_table <- DT::renderDT({
    d <- listed()
    redraw()
    shiny::req(!is.null(d), nrow(d) > 0L)
    on <- shiny::isolate(ticked())
    ok <- .ai_adoptable(d)
    ck <- sprintf("<input type=\"checkbox\" class=\"ai-adopt-ck\" data-ns=\"%s\" data-id=\"%s\"%s%s>",
                  ns(""), htmltools::htmlEscape(d$output_id, attribute = TRUE),
                  ifelse(d$output_id %in% on, " checked", ""), ifelse(ok, "", " disabled"))
    rtf <- ifelse(!is.na(d$rtf) & nzchar(d$rtf), sprintf(
      "<a href=\"#\" class=\"ai-open-rtf\" data-ns=\"%s\" data-file=\"%s\">%s</a>",
      ns(""), htmltools::htmlEscape(d$rtf, attribute = TRUE), t("Open")), "")
    # (tflspec's words in the app's language: the state, adopted)
    tw <- function(x) ifelse(is.na(x) | !nzchar(x), "", vapply(as.character(x), t, ""))
    v <- data.frame(ck, d$output_id, d$title, d$population, d$split, tw(d$status), tw(d$adopted), rtf,
                    d$questions, d$problem, stringsAsFactors = FALSE)
    names(v) <- c(t("Adopt"), "output_id", t("Title (title)"), t("Analysis set (population)"),
                  t("Split (split)"), t("State (status)"), paste0(t("In the study"), " (adopted)"), "RTF",
                  t("Questions (questions)"), t("First problem"))
    v[is.na(v)] <- ""
    # (the boxes and the RTF links: not escaped)
    DT::datatable(v, rownames = FALSE, escape = which(!names(v) %in% c(t("Adopt"), "RTF")),
                  selection = "none",
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
  # the reports ticked, apart: those not in the study yet, and those it has
  # (adopting them again replaces the study's: asked twice)
  split_ticked <- function() {
    d <- listed()
    ids <- intersect(ticked(), d$output_id[.ai_adoptable(d)])
    plan <- .ai_adopt_plan(path(), ids)
    st <- if (!is.null(plan)) stats::setNames(as.character(plan$status), plan$output_id)
    adopted <- stats::setNames(as.character(d$adopted), d$output_id)[ids]
    over <- ids[!adopted %in% c("no", NA)]
    if (!is.null(st)) over <- over[st[over] %in% "replace"]
    list(fresh = ids[adopted %in% c("no", NA)], over = over,
         same = if (!is.null(st)) names(st)[st == "same"] else character())
  }
  pending <- shiny::reactiveVal(NULL)
  table_of <- function(d, cols, heads) {
    shiny::tags$table(class = "table table-sm small mb-2",
                      shiny::tags$thead(shiny::tags$tr(lapply(heads, shiny::tags$th))),
                      shiny::tags$tbody(lapply(seq_len(nrow(d)), function(r)
                        shiny::tags$tr(lapply(d[r, cols], function(x) shiny::tags$td(as.character(x)))))))
  }
  shiny::observeEvent(input$adopt, {
    if (!length(ticked())) return(notify(t("Tick the reports to adopt."), "warning"))
    s <- split_ticked()
    pending(s)
    if (!length(s$fresh) && !length(s$over)) {
      return(notify(t("The reports ticked are in the study as they are: nothing to adopt."), "message"))
    }
    diff <- .ai_adopt_diff(path(), s$over)
    shiny::showModal(shiny::modalDialog(
      title = t("Adopt (copy) into the study"),
      if (length(s$fresh)) shiny::p(sprintf(t("Not in the study yet, copied to its Report list (%d): %s"),
                                            length(s$fresh), paste(s$fresh, collapse = ", "))),
      if (length(s$over)) shiny::div(
        class = "alert alert-warning py-2 small",
        shiny::strong(sprintf(t("In the study already (%d): %s"), length(s$over), paste(s$over, collapse = ", "))),
        shiny::p(class = "mb-1", t("Copying these replaces the study's reports with the AI's drafts: settings made on the screen may be lost. They are left as they are unless you go on to replace them.")),
        if (!is.null(diff)) shiny::tags$details(
          open = NA, shiny::tags$summary(sprintf(t("What replacing changes (%d)"), nrow(diff))),
          table_of(diff, c("output_id", "where", "change", "adopted", "draft"),
                   c("output_id", t("Where"), t("Change"), t("In the study"), t("Draft"))))),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        if (length(s$over)) .btn(ns("adopt_over"), t("Replace them too..."), class = "btn-outline-danger"),
        if (length(s$fresh)) .btn(ns("adopt_fresh"), sprintf(t("Copy the %d not in the study yet"), length(s$fresh)),
                                  class = "btn-primary")),
      easyClose = TRUE))
  })
  # the second question before replacing: said once more, ticked to go on
  shiny::observeEvent(input$adopt_over, {
    s <- pending()
    if (is.null(s) || !length(s$over)) return()
    shiny::showModal(shiny::modalDialog(
      title = t("Replace the study's reports?"),
      shiny::div(class = "alert alert-danger py-2",
                 sprintf(t("The study's %d reports (%s) are replaced by the AI's drafts. What was set for them in the study and not in the drafts may be lost."),
                         length(s$over), paste(s$over, collapse = ", "))),
      shiny::checkboxInput(ns("adopt_sure"), t("I understand that settings of these reports may be lost"), FALSE),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        shiny::conditionalPanel(
          sprintf("input['%s']", ns("adopt_sure")),
          .btn(ns("adopt_over_ok"), sprintf(t("Replace %d and copy"), length(s$over)), class = "btn-danger"))),
      easyClose = FALSE))
  })
  do_adopt <- function(fresh, over) {
    shiny::removeModal()
    p <- path()
    res <- list()
    if (length(fresh)) res[[1L]] <- guarded(tflspec::tfl_ai_adopt(p, outputs = fresh, overwrite = FALSE))
    if (length(over)) res[[2L]] <- guarded(tflspec::tfl_ai_adopt(p, outputs = over, overwrite = TRUE))
    again(again() + 1L)
    reload_now()
    notify(sprintf(t("%d reports adopted (copied) into the study, %d of them replaced: on the Report list."),
                   length(fresh) + length(over), length(over)))
  }
  shiny::observeEvent(input$adopt_fresh, {
    s <- pending()
    if (!is.null(s)) do_adopt(s$fresh, character())
  })
  shiny::observeEvent(input$adopt_over_ok, {
    s <- pending()
    if (!is.null(s) && isTRUE(input$adopt_sure)) do_adopt(s$fresh, s$over)
  })
  invisible(listed)
}

# Whether a report's file (from the study folder) is the study's own,
# spec/outputs -- not a draft in the AI's area, ai/spec/outputs
.ai_in_study_spec <- function(f) {
  f <- as.character(f)
  !is.na(f) & startsWith(gsub("\\\\", "/", f), "spec/")
}
