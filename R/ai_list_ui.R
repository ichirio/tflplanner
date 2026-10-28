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
    shiny::div(class = "d-flex flex-wrap gap-2 align-items-center mt-2",
               .btn(ns("exp_add"), paste0(t("Add a report the AI missed"), " (expected_outputs.csv)")),
               shiny::uiOutput(ns("exp_line"), inline = TRUE)),
    # the AI's assumptions (R/ai_assumptions_ui.R)
    ai_asm_card(ns, t),
    # the agent's questions (moved here from step 3: R/ai_draft_ui.R draws them)
    bslib::card(
      class = "mt-3",
      bslib::card_header(
        class = "d-flex flex-wrap gap-2 align-items-center",
        shiny::span(class = "me-auto", paste0(t("Questions"), " (ai/questions.md)")),
        .btn(ns("questions_write"), paste0(t("Write the answers back"), " (tfl_ai_answer_questions)"),
             class = "btn-sm btn-primary"),
        shiny::uiOutput(ns("tell_agent"), inline = TRUE)),
      shiny::uiOutput(ns("questions"))),
    # the next round: the corrections in one file, run.R over every request
    bslib::card(
      class = "mt-3",
      bslib::card_header(
        class = "d-flex flex-wrap gap-2 align-items-center",
        shiny::span(class = "me-auto", with_tip(
          paste0(t("The next round"), " (ai/corrections.md)"),
          t("Your list's changes, notes and the answered questions go into one file the agent works through in its next round; then tell the agent."))),
        .btn(ns("corrections"), paste0(t("Write the corrections"), " (tfl_ai_corrections)"),
             class = "btn-sm btn-primary"),
        .btn(ns("run_all"), paste0(t("Run run.R for every request"), " (Rscript run.R)"))),
      shiny::uiOutput(ns("corrections_note")),
      shiny::uiOutput(ns("run_note"))),
    shiny::tags$script(shiny::HTML(
      "$(document).off('click.aiexp').on('click.aiexp', '.ai-exp', function(e) {
         e.preventDefault();
         Shiny.setInputValue(this.dataset.ns + 'exp_act', {act: this.dataset.act, id: this.dataset.id, n: Date.now()},
                             {priority: 'event'});
       });
       $(document).off('click.airtf').on('click.airtf', '.ai-open-rtf', function(e) {
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
ai_list_server <- function(input, output, session, path, ver, reload_now, notify, guarded, t,
                           dirty = function() FALSE) {
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
               t("No report listed yet: write the requests (step 1), then check the answers (step 3) or run run.R (below)."))
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
    # the person's list (ai/expected_outputs.csv): its action and note a
    # row, its added rows below; a row's changes as links
    e <- expected()
    k <- if (is.null(e)) rep(NA_integer_, nrow(d)) else match(d$output_id, e$output_id)
    act <- if (is.null(e)) rep(NA_character_, nrow(d)) else e$action[k]
    note <- if (is.null(e)) rep(NA_character_, nrow(d)) else e$note[k]
    links <- function(id, act) {
      one <- function(a, label) sprintf("<a href=\"#\" class=\"ai-exp me-2\" data-ns=\"%s\" data-act=\"%s\" data-id=\"%s\">%s</a>",
                                        ns(""), a, htmltools::htmlEscape(id, attribute = TRUE), label)
      ifelse(is.na(act), paste0(one("change", t("Change")), one("drop", t("Not wanted"))),
             paste0(one("change", t("Change")), one("keep", t("Set back"))))
    }
    # (a row changed on the person's list: the AI's value, then theirs)
    yours <- function(col) {
      if (is.null(e)) return(d[[col]])
      mine <- e[[col]][k]
      ifelse(act %in% "change" & !is.na(mine) & mine != ifelse(is.na(d[[col]]), "", d[[col]]),
             paste0(ifelse(is.na(d[[col]]), "", d[[col]]), " → ", mine), d[[col]])
    }
    v <- data.frame(ck, d$output_id, yours("title"), yours("population"), yours("split"), tw(d$status),
                    tw(d$adopted), rtf,
                    d$questions, d$problem, tw(act), note, links(d$output_id, act), stringsAsFactors = FALSE)
    if (!is.null(e)) {
      a <- e[e$action %in% "add" & !(e$output_id %in% d$output_id), , drop = FALSE]
      if (nrow(a)) {
        v <- rbind(v, stats::setNames(data.frame("", a$output_id, a$title, a$population, a$split, "", "", "",
                                                 "", "", t("add"), a$note, "", stringsAsFactors = FALSE),
                                      names(v)))
      }
    }
    names(v) <- c(t("Tick to copy"), "output_id", t("Title (title)"), t("Analysis set (population)"),
                  t("Split (split)"), t("State (status)"), paste0(t("Adopted"), " (adopted)"), "RTF",
                  t("Questions (questions)"), t("First problem"), paste0(t("Expected"), " (action)"),
                  t("Note (note)"), t("Edit"))
    v[is.na(v)] <- ""
    # (the boxes, the RTF links and the change links: not escaped)
    DT::datatable(v, rownames = FALSE, escape = which(!names(v) %in% c(t("Tick to copy"), "RTF", t("Edit"))),
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
  # (not while the study has unsaved changes: the adopted files would be
  # taken in over them, or the study left apart from its files)
  unsaved <- function() {
    if (!isTRUE(dirty())) return(FALSE)
    notify(t("Save first: the study has unsaved changes."), "warning")
    TRUE
  }
  shiny::observeEvent(input$adopt, {
    if (unsaved()) return()
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
    if (unsaved()) return()
    p <- path()
    # (each call NULL when it failed: guarded() said why)
    done_fresh <- if (length(fresh)) !is.null(guarded(tflspec::tfl_ai_adopt(p, outputs = fresh, overwrite = FALSE)))
    done_over <- if (length(over)) !is.null(guarded(tflspec::tfl_ai_adopt(p, outputs = over, overwrite = TRUE)))
    again(again() + 1L)
    n_fresh <- if (isTRUE(done_fresh)) length(fresh) else 0L
    n_over <- if (isTRUE(done_over)) length(over) else 0L
    if (!(n_fresh + n_over)) return()
    # (saved too: their programs and batch.R written, for a run)
    saved <- isTRUE(reload_now(save = TRUE))
    notify(if (saved) sprintf(t("%d reports adopted (copied) into the study, %d of them replaced: on the Report list."),
                              n_fresh + n_over, n_over)
           else sprintf(t("%d reports adopted (copied) into the study, but the study was not saved: save it to write their programs."),
                        n_fresh + n_over),
           if (saved) "message" else "warning")
  }
  shiny::observeEvent(input$adopt_fresh, {
    s <- pending()
    if (!is.null(s)) do_adopt(s$fresh, character())
  })
  shiny::observeEvent(input$adopt_over_ok, {
    s <- pending()
    if (!is.null(s) && isTRUE(input$adopt_sure)) do_adopt(s$fresh, s$over)
  })
  # ---- the person's list (R/ai_expected.R): changed, dropped, added at once
  expected <- shiny::reactive({
    ver()
    again()
    p <- path()
    if (is.null(p)) NULL else tryCatch(tflspec::tfl_ai_expected_read(p), error = function(e) NULL)
  })
  output$exp_line <- shiny::renderUI({
    e <- expected()
    if (is.null(e)) {
      return(shiny::span(class = "small text-muted", t("No list of your own yet: the first change makes it (ai/expected_outputs.csv).")))
    }
    n <- function(a) sum(e$action %in% a)
    shiny::span(class = "small text-muted", sprintf(t("Your list: %d changed, %d not wanted, %d added (ai/expected_outputs.csv; run.R compares the reports with it)."),
                                                    n("change"), n("drop"), n("add")))
  })
  exp_save <- function(what, id = NA_character_, fields = list()) {
    ok <- guarded(.ai_expected_change(path(), what, id, fields))
    if (is.null(ok)) return(FALSE)
    shiny::removeModal()
    again(again() + 1L)
    TRUE
  }
  nz <- function(x) if (is.null(x) || !length(x) || is.na(x[1L])) "" else as.character(x[1L])
  exp_row <- function(id) {
    e <- shiny::isolate(expected())
    d <- shiny::isolate(listed())
    r <- if (!is.null(e) && id %in% e$output_id) e[match(id, e$output_id), , drop = FALSE] else
      d[match(id, d$output_id), , drop = FALSE]
    r
  }
  shiny::observeEvent(input$exp_act, {
    a <- input$exp_act$act
    id <- input$exp_act$id
    if (identical(a, "keep")) return(invisible(exp_save("keep", id)))
    r <- exp_row(id)
    if (identical(a, "drop")) {
      shiny::showModal(shiny::modalDialog(
        title = sprintf(t("%s is not wanted"), id),
        shiny::textAreaInput(ns("exp_reason"), paste0(t("Why (required)"), " (reason)"), "", width = "100%", rows = 3),
        shiny::p(class = "small text-muted", t("The row stays on your list, marked not wanted; run.R says if the report is still there.")),
        footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                                .btn(ns("exp_drop_ok"), t("Mark not wanted"), class = "btn-primary")),
        easyClose = TRUE))
      exp_pending(id)
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Change %s on your list"), id),
      shiny::textInput(ns("exp_title"), t("Title (title)"), nz(r$title), width = "100%"),
      shiny::div(class = "d-flex gap-2",
                 shiny::textInput(ns("exp_population"), t("Analysis set (population)"), nz(r$population)),
                 shiny::textInput(ns("exp_split"), t("Split (split)"), nz(r$split))),
      shiny::textAreaInput(ns("exp_note"), paste0(t("What the report should show"), " (note)"),
                           nz(r$note), width = "100%", rows = 3),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(ns("exp_change_ok"), t("Save"), class = "btn-primary")),
      easyClose = TRUE))
    exp_pending(id)
  })
  exp_pending <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$exp_drop_ok, {
    if (!nzchar(trimws(input$exp_reason %||% ""))) return(notify(t("Say why the report is not wanted."), "warning"))
    exp_save("drop", exp_pending(), list(reason = input$exp_reason))
  })
  shiny::observeEvent(input$exp_change_ok, {
    exp_save("change", exp_pending(), list(title = input$exp_title, population = input$exp_population,
                                           split = input$exp_split, note = input$exp_note))
  })
  shiny::observeEvent(input$exp_add, {
    shiny::showModal(shiny::modalDialog(
      title = t("Add a report the AI missed"),
      shiny::div(class = "d-flex gap-2",
                 shiny::textInput(ns("add_sheet"), t("Shell sheet (sheet)"), ""),
                 shiny::textInput(ns("add_toc"), t("TOC number (toc_id)"), ""),
                 shiny::textInput(ns("add_id"), "output_id", "")),
      shiny::textInput(ns("add_title"), t("Title (title)"), "", width = "100%"),
      shiny::div(class = "d-flex gap-2",
                 shiny::textInput(ns("add_population"), t("Analysis set (population)"), ""),
                 shiny::textInput(ns("add_split"), t("Split (split)"), "")),
      shiny::textAreaInput(ns("add_note"), paste0(t("What the report should show"), " (note)"), "",
                           width = "100%", rows = 3),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(ns("exp_add_ok"), t("Add"), class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$exp_add_ok, {
    f <- list(sheet = input$add_sheet, toc_id = input$add_toc, output_id = input$add_id,
              title = input$add_title, population = input$add_population, split = input$add_split,
              note = input$add_note)
    if (!any(nzchar(trimws(unlist(f[c("sheet", "toc_id", "output_id", "title")]))))) {
      return(notify(t("Say at least its sheet, TOC number, output_id or title."), "warning"))
    }
    exp_save("add", fields = f)
  })
  # ---- the next round: the corrections file, run.R over every request
  corr <- shiny::reactiveVal(NULL)
  shiny::observeEvent(path(), corr(NULL), ignoreNULL = FALSE)
  shiny::observeEvent(input$corrections, {
    heads <- guarded(tflspec::tfl_ai_corrections(path()))
    if (is.null(heads)) return()
    corr(list(heads = as.character(heads), n = Sys.time()))
  })
  output$corrections_note <- shiny::renderUI({
    k <- corr()
    if (is.null(k)) return(NULL)
    line <- t("I wrote my corrections and answers in ai/corrections.md: work through them (your next round).")
    shiny::div(
      class = "alert alert-info py-2 small mt-2 mb-0",
      if (length(k$heads)) {
        shiny::div(sprintf(t("%d new corrections written to ai/corrections.md: %s"), length(k$heads),
                           paste(k$heads, collapse = "; ")))
      } else shiny::div(t("No new correction: ai/corrections.md has them all already.")),
      shiny::div(class = "mt-1", t("Then send the agent:"), " ", shiny::code(line)))
  })
  # run.R in the study folder, apart (processx): the reports built, the list
  # read again when it ends
  run <- shiny::reactiveValues(px = NULL, log = NULL, end = NULL)
  shiny::observeEvent(input$run_all, {
    p <- path()
    if (is.null(p) || !file.exists(file.path(p, "run.R"))) {
      return(notify(t("The study has no run.R: write the requests in agent mode (step 1)."), "warning"))
    }
    if (!is.null(run$px) && run$px$is_alive()) return(notify(t("run.R is running already."), "warning"))
    # (the drafts in their own folder first: run.R writes them there)
    .ai_ensure_area(p)
    log <- file.path(p, "ai", "run-from-app.log")
    run$log <- log
    run$end <- NULL
    run$px <- processx::process$new(file.path(R.home("bin"), "Rscript"), "run.R", wd = p,
                                    stdout = log, stderr = "2>&1", cleanup = TRUE)
  })
  # (its end found here, not while drawing: the list read again once)
  shiny::observe({
    px <- run$px
    if (is.null(px) || !is.null(shiny::isolate(run$end))) return()
    if (px$is_alive()) return(shiny::invalidateLater(2000))
    run$end <- px$get_exit_status()
    shiny::isolate(again(again() + 1L))
  })
  output$run_note <- shiny::renderUI({
    if (is.null(run$px)) return(NULL)
    if (is.null(run$end)) {
      return(shiny::div(class = "small text-muted mt-2", t("run.R is running (every request) ...")))
    }
    tail <- tryCatch(utils::tail(readLines(run$log, warn = FALSE, encoding = "UTF-8"), 3L),
                     error = function(e) character())
    shiny::div(class = paste("small mt-2", if (identical(run$end, 0L)) "text-success" else "text-danger"),
               if (identical(run$end, 0L)) t("run.R ended: the list is read again.") else
                 sprintf(t("run.R stopped (exit %s): see ai/run-from-app.log."), run$end),
               if (length(tail)) shiny::tags$pre(class = "small mb-0", paste(tail, collapse = "\n")))
  })
  invisible(listed)
}

# Whether a report's file (from the study folder) is the study's own,
# spec/outputs -- not a draft in the AI's area, ai/spec/outputs
.ai_in_study_spec <- function(f) {
  f <- as.character(f)
  !is.na(f) & startsWith(gsub("\\\\", "/", f), "spec/")
}
