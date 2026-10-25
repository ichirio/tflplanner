# app_server(): study -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_study <- function() {
  lang <- tflplanner_language()
  t <- function(x) tr(x, lang)
  # `want`: the report to select once the sidebar knows it
  rv <- shiny::reactiveValues(
    study = NULL, p = NULL, saved = NULL, meta = NULL, saved_meta = NULL,
    ver = 0L, want = NULL, job = NULL, job_what = NULL, status_ver = 0L,
    studies_ver = 0L, ard_ver = 0L, save_ver = 0L, spec = NULL,
    spec_merge = NULL, adopting = FALSE)
  bump <- function() rv$ver <- rv$ver + 1L
  # an ARD method's name and note as the user reads them: the translation
  # kept by the app under "method:<name>" / "method-note:<name>", else
  # tflspec's English (tflspec's catalog stays the English original)
  method_label <- function(method, label) {
    k <- paste0("method:", method)
    v <- t(k)
    fallback <- ifelse(!is.na(label) & nzchar(label), label, method)
    ifelse(v == k, fallback, v)
  }
  method_note <- function(method, note) {
    k <- paste0("method-note:", method)
    v <- t(k)
    ifelse(v == k, note, v)
  }
  notify <- function(msg, type = "message") {
    shiny::showNotification(msg, type = type, duration = 6)
  }
  guarded <- function(expr) {
    # the detail (the calls) goes to the console / log; the screen gets
    # the message only
    tryCatch(
      withCallingHandlers(expr, error = function(e) {
        calls <- vapply(utils::tail(sys.calls(), 12L), function(cl)
          paste(utils::head(deparse(cl, width.cutoff = 80L), 1L), collapse = ""), "")
        message("tflplanner: ", conditionMessage(e), "\n  ",
                paste(calls, collapse = "\n  "))
      }),
      error = function(e) {
        # a problem's full output (a program that failed) is for the log
        if (inherits(e, "tflplanner_problem") && length(e$detail)) {
          message("tflplanner: the output of what failed:\n",
                  paste(e$detail, collapse = "\n"))
        }
        notify(.error_view(conditionMessage(e), t), "error")
        NULL
      })
  }

  # Every grid's edits come back through read_grid().  A grid whose change
  # cannot be read (rhandsontable could not rebuild the data frame) must not
  # stop the session: the change is dropped, said so, and every grid is
  # drawn again from what the study holds.
  grids_drawn <- shiny::reactiveVal(0L)
  read_grid <- function(h) {
    h$data <- .grid_rows_full(h$data, length(unlist(h$params$rColHeaders)))
    d <- tryCatch(rhandsontable::hot_to_r(h), error = function(e) NULL)
    if (is.null(d)) {
      notify(t("This change could not be taken in; the grid shows the definition as it was."),
             "warning")
      grids_drawn(grids_drawn() + 1L)
    }
    d
  }
  # whether a study is open: what changes is the answer, not each copy of
  # the study a save makes (as a reactive(), a save made every output that
  # asks -- most of them -- be drawn again)
  has_study_v <- shiny::reactiveVal(FALSE)
  shiny::observe(has_study_v(!is.null(rv$study)), priority = 10000)
  # (the answer as of now: a handler that has just opened a study asks
  # before the observer above has run)
  has_study <- function() {
    has_study_v()
    !is.null(shiny::isolate(rv$study))
  }
  # Parts of the definition, each changing only when that part does: an
  # edit of a table's cell does not make the ARD's state, the report list
  # or the own functions be worked out again (each read the whole study
  # before, at every keystroke).  A reactiveVal set to what it holds
  # already tells no one.
  p_part <- function(get) {
    v <- shiny::reactiveVal(NULL)
    shiny::observe(v(get(rv$p)), priority = 10000)
    v
  }
  # what the ARD definitions (and their state) are made from
  p_ard <- p_part(function(p) list(p$ard, p$sheets$codelists, p$setup, p$study))
  # the study open (its folder and id), not each copy of it a save makes
  study_open <- shiny::reactiveVal(NULL)
  shiny::observe(study_open(list(rv$study$path, rv$study$meta$study_id)),
                 priority = 10000)
  # what the report list shows: the reports, their analyses, listings,
  # figures, report rows and titles
  p_list <- p_part(function(p) list(p$outputs, p$ard, p$lf, p$fig_designs,
                                    p$sheets$report, p$sheets$titles, p$sheets$tokens,
                                    p$study))
  # A new table's first drawing in the builder (#417): the description the
  # builder shows is put in the planner -- the preview's, the next edit's
  # -- but held: until something is changed it is no change (not saved, no
  # draft), and leaving the report puts the planner back as it was
  held <- shiny::reactiveValues(p = NULL, before = NULL)
  unheld <- function(p) if (!is.null(held$p) && identical(p, held$p)) held$before else p
  dirty <- shiny::reactive({
    has_study() && (!identical(unheld(rv$p), rv$saved) ||
                       !identical(rv$meta, rv$saved_meta))
  })
  # for shiny::testServer(), which sees only the session
  session$userData$rv <- rv
  session$userData$grids_drawn <- grids_drawn
  session$userData$dirty <- dirty
  session$userData$has_study <- has_study
  # unsaved changes: the page asks before it is left, and a draft keeps
  # them (written once the edits pause) until they are saved or discarded
  shiny::observe(session$sendCustomMessage("tflplanner-dirty", isTRUE(dirty())))
  draft_due <- shiny::debounce(shiny::reactive(list(dirty(), rv$p, rv$meta)),
                               2000)
  shiny::observe({
    unsaved <- isTRUE(draft_due()[[1L]])
    shiny::isolate({
      # (while the dialog of a change made outside the app is up, the
      # draft waits: it is offered once that is decided)
      if (!has_study() || !is.null(rv$draft) || isTRUE(rv$adopting)) return()
      # (a draft not written is said, never the session's end)
      tryCatch(if (unsaved) .write_draft(current_study()) else .drop_draft(rv$study$meta$study_id),
               error = function(e) message("tflplanner: the draft: ", conditionMessage(e)))
    })
  })

  # -- the study ---------------------------------------------------------
  # The definition files were changed outside the app (spec/, study.yml):
  # the same screen as an import asks which parts to take in.  The files
  # as they are go to spec/.backup/ if any part is left out.
  adopt_modal <- function(s) {
    rv$adopting <- TRUE
    sp <- s$spec
    was <- list(planner = sp$was$planner, meta = sp$was$meta)
    sm <- tryCatch(.part_summary(was, s, sp$parts), error = function(e)
      rep("", length(sp$parts)))
    shiny::showModal(shiny::modalDialog(
      title = t("The definition files were changed outside the app"),
      shiny::p(class = "small mb-1",
               t("spec/ or study.yml is not as the app saved it. Choose the parts to take in; the rest stay as the study was.")),
      if (!is.null(sp$files) && nrow(sp$files)) shiny::p(
        class = "small text-muted mb-2",
        paste(sp$files$file, collapse = ", ")),
      shiny::checkboxGroupInput("adopt_parts", t("Parts that differ (take in the ones checked): the study's \u2192 the files'"),
                                stats::setNames(sp$parts, paste0(.part_labels(sp$parts), ": ", sm)),
                                selected = sp$parts, width = "100%"),
      shiny::p(class = "small text-muted",
               t("The files as they are now are kept in spec/.backup/<date>-<time>/ if a part is left out. To change the definition, export it, edit the copy and import it back (Import definition files).")),
      size = "l", easyClose = FALSE,
      footer = shiny::tagList(
        .btn("adopt_leave", t("Leave the study as it was"), class = "btn-outline-secondary"),
        .btn("adopt_ok", t("Take in the parts checked"), class = "btn-primary"))))
  }
  adopt_done <- function(take) {
    s <- rv$study
    rv$adopting <- FALSE
    shiny::removeModal()
    out <- guarded(.apply_adoption(s, take))
    if (is.null(out)) return()
    if (!identical(out, s)) {
      rv$study <- out
      rv$p <- rv$saved <- .study_spec_keys(out$planner)
      rv$meta <- rv$saved_meta <- out$meta[.study_fields]
      rv$spec <- out$spec
      rv$status_ver <- rv$status_ver + 1L
      rv$ard_ver <- rv$ard_ver + 1L
      rv$studies_ver <- rv$studies_ver + 1L
      bump()
      notify(sprintf(t("Taken in: %s. The files before are in %s."),
                     if (length(take)) paste(.part_labels(take), collapse = ", ") else "-",
                     sub(paste0("^", out$path, "/"), "", out$backup)))
    } else {
      rv$spec <- s$spec
      notify(t("The definition files were changed outside tflplanner; the changes were loaded."))
    }
    offer_draft(rv$study)
  }
  shiny::observeEvent(input$adopt_ok, adopt_done(input$adopt_parts %||% character()))
  shiny::observeEvent(input$adopt_leave, adopt_done(character()))
  set_study <- function(s) {
    rv$adopting <- FALSE
    s$planner <- .study_spec_keys(s$planner)
    rv$study <- s
    rv$p <- rv$saved <- s$planner
    rv$meta <- rv$saved_meta <- s$meta[.study_fields]
    rv$want <- output_ids(s$planner)[1L]
    bump()
    rv$status_ver <- rv$status_ver + 1L
    rv$ard_ver <- rv$ard_ver + 1L
    # a study of the old format: said once, until closed
    if (length(.old_format(s$planner))) {
      shiny::showNotification(t(.old_format_msg), type = "warning", duration = NULL)
    }
    # the variables' headings moved from the code lists: said once
    if (!is.null(attr(s$planner, "moved_headings"))) {
      shiny::showNotification(t(.moved_headings_msg), type = "message", duration = NULL)
    }
    # the definition files changed outside tflplanner (#274): taken in, or
    # not readable (the study open as last saved, no save until resolved)
    rv$spec <- s$spec
    if (identical(s$spec$status, "adopted")) {
      # never silently: what changed, the parts to take in (#274)
      adopt_modal(s)
      return(invisible())
    } else if (identical(s$spec$status, "invalid")) {
      spec_modal()
      return(invisible())
    }
    offer_draft(s)
  }
  # the definition files that do not read: what is wrong, and the two ways
  # out (fix them and load them, or write them back from the last save)
  spec_problems_table <- function(pr) {
    if (is.null(pr) || !nrow(pr)) return(NULL)
    v <- function(x) if (is.null(x) || is.na(x)) "" else x
    shiny::tags$table(
      class = "table table-sm small",
      shiny::tags$thead(shiny::tags$tr(lapply(
        t(c("File", "Sheet", "Row", "Column", "Problem")), shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(pr)), function(i) shiny::tags$tr(
        class = if (identical(pr$severity[i], "warning")) "table-warning",
        shiny::tags$td(shiny::code(pr$file[i])),
        shiny::tags$td(v(pr$sheet[i])), shiny::tags$td(v(pr$row[i])),
        shiny::tags$td(v(pr$column[i] %||% NA)),
        shiny::tags$td(shiny::tags$pre(class = "mb-0 small",
                                       style = "white-space: pre-wrap;",
                                       pr$message[i]))))))
  }
  spec_problems_ui <- function(sp, sfx = "") {
    pr <- sp$problems
    shiny::tagList(
      shiny::p(t("The definition files were changed outside tflplanner but could not be read. The study is open as it was last saved. Fix the files and load them, or write the files back from the last save. Until then the study cannot be saved.")),
      spec_problems_table(pr),
      shiny::div(class = "d-flex flex-wrap gap-2",
        .btn(paste0("spec_reload", sfx), t("Load from the definition files"),
             class = "btn-sm btn-primary"),
        .btn(paste0("spec_writeback", sfx), t("Write the files back from the last save"),
             class = "btn-sm btn-outline-danger")))
  }
  spec_modal <- function() {
    shiny::showModal(shiny::modalDialog(
      title = t("The definition files do not read"),
      spec_problems_ui(rv$spec, "_m"), size = "l", easyClose = TRUE,
      footer = shiny::modalButton(t("Close"))))
  }
  # what changed outside tflplanner, and what was taken in
  spec_changes_ui <- function(sp) {
    f <- sp$files
    shiny::tags$details(
      class = "small mb-2",
      shiny::tags$summary(t("What changed")),
      if (!is.null(f) && nrow(f)) shiny::tags$ul(lapply(seq_len(nrow(f)), function(i)
        shiny::tags$li(shiny::code(f$file[i]), " ",
                       shiny::span(class = "text-muted",
                                   sprintf("(%s: %s \u2192 %s)", f$status[i],
                                           ifelse(is.na(f$size_was[i]), "-", f$size_was[i]),
                                           ifelse(is.na(f$size_now[i]), "-", f$size_now[i])))))),
      if (length(sp$parts)) shiny::div(paste0(
        t("Parts"), ": ", paste(.part_labels(sp$parts), collapse = ", "))),
      if (length(sp$outputs)) shiny::div(paste0(
        t("Reports"), ": ", paste(sp$outputs, collapse = ", "))))
  }
  output$spec_card <- shiny::renderUI({
    shiny::req(shows_open())
    sp <- rv$spec
    if (is.null(sp)) return(NULL)
    if (identical(sp$status, "invalid")) {
      return(shiny::div(class = "alert alert-warning", spec_problems_ui(sp)))
    }
    if (identical(sp$status, "adopted")) {
      return(shiny::div(
        class = "alert alert-info py-2",
        shiny::div(t("The definition files were changed outside tflplanner; the changes were loaded.")),
        spec_changes_ui(sp)))
    }
    if (identical(sp$status, "imported")) {
      return(shiny::div(
        class = "alert alert-info py-2",
        shiny::div(sprintf(t("Imported from %s."), sp$from)),
        shiny::div(class = "small",
                   paste0(t("Parts"), ": ", paste(.part_labels(sp$parts), collapse = ", "))),
        if (length(sp$outputs)) shiny::div(class = "small",
          paste0(t("Reports"), ": ", paste(sp$outputs, collapse = ", "))),
        shiny::div(class = "small",
                   sprintf(t("The files before the import are kept in %s."), sp$backup))))
    }
    NULL
  })
  # a draft left by a session that did not save: take it back, or drop it
  offer_draft <- function(s) {
    d <- .read_draft(s$meta$study_id)
    if (is.null(d)) return(invisible())
    # the definition files were taken in on this open: the draft was made
    # against the study before, merged part by part with the files' (#274)
    if (identical(s$spec$status, "adopted") && !is.null(s$spec$was)) {
      # (merged by its sheets: what only its spec held is not taken back)
      d$planner$spec <- NULL
      base <- s
      base$planner <- s$spec$was$planner
      base$meta <- s$spec$was$meta
      m <- .merge_parts(base, d, s)
      if (length(m$conflicts)) {
        files <- m$study
        mine <- files
        for (k in m$conflicts) mine <- .set_study_part(mine, k, .study_parts(d)[[k]])
        ask_conflict(m$conflicts, files, mine, draft = TRUE)
        return(invisible())
      }
      d <- m$study
    }
    if (is.null(d$planner$spec) && .same_planner(d$planner, s$planner) &&
        identical(d$meta[.study_fields], s$meta[.study_fields])) {
      .drop_draft(s$meta$study_id)
      return(invisible())
    }
    rv$draft <- d
    shiny::showModal(shiny::modalDialog(
      title = t("Changes that were not saved"),
      t("The last time this study was open, some changes were not saved. Take them back (they are not saved until you save), or discard them?"),
      footer = shiny::tagList(
        .btn("draft_discard", t("Discard"), class = "btn-outline-secondary"),
        .btn("draft_restore", t("Take them back"), class = "btn-primary")),
      easyClose = FALSE))
  }
  shiny::observeEvent(input$draft_restore, {
    d <- rv$draft
    if (!is.null(d)) {
      rv$p <- .study_spec_keys(d$planner)
      # (a draft is the view, edited: the spec it was made from is the study's)
      if (is.null(rv$p$spec)) rv$p$spec <- rv$saved$spec
      rv$meta <- d$meta[.study_fields]
      bump()
    }
    rv$draft <- NULL
    shiny::removeModal()
  })
  # parts both the files and the unsaved changes changed: keep which
  ask_conflict <- function(parts, files, mine, draft = FALSE) {
    rv$spec_merge <- list(files = files, mine = mine, draft = draft)
    shiny::showModal(shiny::modalDialog(
      title = t("Changes that were not saved"),
      sprintf(t("Both the files and your unsaved changes changed these parts: %s. Keep which?"),
              paste(.part_labels(parts), collapse = ", ")),
      footer = shiny::tagList(
        .btn("merge_files", t("Keep the files' version"), class = "btn-outline-secondary"),
        .btn("merge_mine", t("Keep my unsaved changes"), class = "btn-primary")),
      easyClose = FALSE))
  }
  merge_take <- function(which) {
    m <- rv$spec_merge
    rv$spec_merge <- NULL
    shiny::removeModal()
    if (is.null(m)) return()
    s <- m[[which]]
    rv$p <- .study_spec_keys(s$planner)
    rv$meta <- s$meta[.study_fields]
    rv$draft <- NULL
    bump()
  }
  shiny::observeEvent(input$merge_files, merge_take("files"))
  shiny::observeEvent(input$merge_mine, merge_take("mine"))
  # Load from the definition files: read them all, take them in; unsaved
  # changes are merged part by part
  do_reload <- function() {
    shiny::removeModal()
    shiny::req(has_study())
    id <- rv$study$meta$study_id
    unsaved <- isTRUE(dirty())
    mine <- current_study()
    base <- rv$study
    base$planner <- rv$saved
    base$meta[.study_fields] <- rv$saved_meta
    s <- tryCatch(reload_from_spec(id), tflplanner_spec_invalid = function(e) {
      rv$spec <- list(status = "invalid", problems = e$problems,
                      files = spec_status(id))
      spec_modal()
      NULL
    }, error = function(e) {
      notify(.error_view(conditionMessage(e), t), "error")
      NULL
    })
    if (is.null(s)) return()
    rv$study <- s
    rv$saved <- .study_spec_keys(s$planner)
    rv$saved_meta <- s$meta[.study_fields]
    rv$spec <- s$spec
    rv$status_ver <- rv$status_ver + 1L
    rv$ard_ver <- rv$ard_ver + 1L
    if (unsaved) {
      m <- .merge_parts(base, mine, s)
      if (length(m$conflicts)) {
        mine2 <- m$study
        for (k in m$conflicts) mine2 <- .set_study_part(mine2, k, .study_parts(mine)[[k]])
        ask_conflict(m$conflicts, m$study, mine2)
        return()
      }
      rv$p <- .study_spec_keys(m$study$planner)
      rv$meta <- m$study$meta[.study_fields]
    } else {
      rv$p <- rv$saved
      rv$meta <- rv$saved_meta
    }
    bump()
    notify(if (identical(s$spec$status, "adopted"))
      t("The definition files were changed outside tflplanner; the changes were loaded.") else
      t("Nothing changed in the definition files."))
  }
  # The study read again from its definition files, when a part of the app
  # wrote some (the AI drafts' check, its reports' files): taken in as they
  # are, their fingerprints recorded.  Never over unsaved changes (those
  # are merged by Reload above): FALSE, and nothing changed.
  reload_now <- function() {
    if (!has_study()) return(FALSE)
    s <- tryCatch(.ai_reload_study(rv$study$meta$study_id, unsaved = isTRUE(dirty())),
                  tflplanner_ai_unsaved = function(e) {
                    notify(t("Save first: the study has unsaved changes."), "warning")
                    NULL
                  },
                  tflplanner_spec_invalid = function(e) {
                    rv$spec <- list(status = "invalid", problems = e$problems,
                                    files = spec_status(rv$study$meta$study_id))
                    spec_modal()
                    NULL
                  }, error = function(e) {
                    notify(.error_view(conditionMessage(e), t), "error")
                    NULL
                  })
    if (is.null(s)) return(FALSE)
    held$p <- held$before <- NULL
    rv$study <- s
    rv$p <- rv$saved <- .study_spec_keys(s$planner)
    rv$meta <- rv$saved_meta <- s$meta[.study_fields]
    rv$spec <- s$spec
    rv$status_ver <- rv$status_ver + 1L
    rv$ard_ver <- rv$ard_ver + 1L
    bump()
    TRUE
  }
  if (.ai_drafts_on()) {
    session$userData$ai <- ai_server(
      "ai", rv = rv, has_study = has_study, dirty = dirty, reload_now = reload_now,
      # (a new study made by the tab: listed, and open)
      adopt = function(s) shiny::withReactiveDomain(session, {
        rv$studies_ver <- rv$studies_ver + 1L
        set_study(s)
      }),
      # (in the app's own session, not the module's: its tabs; go() is
      # defined further down -- called, not taken, here)
      open_report = function(id) shiny::withReactiveDomain(session, {
        rv$want <- id
        shiny::updateSelectInput(session, "target", selected = id)
        go("tables")
      }),
      go = function(where) shiny::withReactiveDomain(session, go(where)),
      notify = notify, guarded = guarded, lang = lang)
  }
  for (b in c("spec_reload", "spec_reload_m")) local({
    id <- b
    shiny::observeEvent(input[[id]], do_reload())
  })
  # Write the files back from the last save (the replaced ones kept in
  # spec/.rejected/); unsaved changes stay unsaved
  do_writeback <- function() {
    shiny::removeModal()
    shiny::req(has_study())
    w <- guarded(write_spec(rv$study$meta$study_id))
    if (is.null(w)) return()
    unsaved <- isTRUE(dirty())
    rv$study <- w
    rv$saved <- .study_spec_keys(w$planner)
    rv$saved_meta <- w$meta[.study_fields]
    if (!unsaved) {
      rv$p <- rv$saved
      rv$meta <- rv$saved_meta
    }
    rv$spec <- list(status = "same")
    rv$status_ver <- rv$status_ver + 1L
    bump()
    notify(t("The replaced files were copied to spec/.rejected/."))
  }
  for (b in c("spec_writeback", "spec_writeback_m")) local({
    id <- b
    shiny::observeEvent(input[[id]], do_writeback())
  })
  shiny::observeEvent(input$draft_discard, {
    if (has_study()) .drop_draft(rv$study$meta$study_id)
    rv$draft <- NULL
    shiny::removeModal()
  })
  # (outside any reactive context at startup: its bump() reads rv$ver)
  if (is.null(start)) {
    last <- tflplanner_config()$last_study
    if (!is.null(last) && last %in% list_studies()$study_id) {
      start <- tryCatch(open_study(last), error = function(e) NULL)
    }
  }
  if (!is.null(start)) shiny::isolate(set_study(start))

  shiny::observe({
    for (tb in .study_tabs) {
      if (has_study()) bslib::nav_show("nav", tb) else
        bslib::nav_hide("nav", tb)
    }
  })

  current_study <- function() {
    s <- rv$study
    s$planner <- rv$p
    s$meta[.study_fields] <- rv$meta
    s
  }
  do_save <- function() {
    # (a new table only drawn, nothing changed: saved as it was)
    if (!is.null(held$p)) {
      if (identical(rv$p, held$p)) rv$p <- held$before
      held$p <- held$before <- NULL
    }
    # what this session opened (or last saved): the save merges it with
    # whatever others saved since
    base <- rv$study
    base$planner <- rv$saved
    base$meta[.study_fields] <- rv$saved_meta
    mine <- rv$p
    s <- tryCatch(save_study(current_study(), base = base),
                  tflplanner_spec_changed = function(e) {
                    shiny::showModal(shiny::modalDialog(
                      title = t("The definition files were changed"),
                      shiny::p(t("The definition files were changed outside tflplanner since this study was opened. Load them first; your unsaved changes are kept and merged.")),
                      shiny::tags$ul(lapply(e$files$file[e$files$status != "same"], function(f)
                        shiny::tags$li(shiny::code(f)))),
                      footer = shiny::tagList(
                        shiny::modalButton(t("Close")),
                        .btn("spec_writeback_m", t("Write the files back from the last save"),
                             class = "btn-outline-danger"),
                        .btn("spec_reload_m", t("Load from the definition files"),
                             class = "btn-primary")),
                      easyClose = TRUE))
                    NULL
                  },
                  tflplanner_conflict = function(e) {
                    shiny::showModal(shiny::modalDialog(
                      title = t("Someone else saved the same part"),
                      conditionMessage(e), easyClose = TRUE))
                    NULL
                  },
                  error = function(e) {
                    notify(.error_view(conditionMessage(e), t), "error")
                    NULL
                  })
    if (is.null(s)) return(FALSE)
    # (no report is new after a save: its usual empty cards go -- #440)
    bnew$ids <- character()
    rv$study <- s
    rv$p <- rv$saved <- s$planner
    if (!.same_planner(.study_spec_keys(mine), s$planner)) {
      bump()
      notify(t("Changes others saved meanwhile were merged in."))
    }
    rv$meta <- rv$saved_meta <- s$meta[.study_fields]
    rv$status_ver <- rv$status_ver + 1L
    rv$studies_ver <- rv$studies_ver + 1L
    n <- sum(s$files$status %in% c("written", "rewritten"))
    re <- sum(s$files$status == "rewritten")
    notify(sprintf(t("Saved (%d files written)"), n))
    if (!is.null(s$ard_problem)) notify(paste(
      t("The ARD definition does not hold, so no ARD program was written (the rest is saved). See the Review tab."),
      s$ard_problem), "error")
    # a program edited by hand: written again from the definition (a
    # program is never edited -- the definition is), the edited one kept
    if (re) notify(sprintf(
      t("%d programs edited by hand were written again from the definition; the edited ones are in programs/.edited/. Put what they changed in the definition: the data code, a user-code report, a custom analysis, where / derive."),
      re), "warning")
    TRUE
  }
  shiny::observeEvent(input$save, {
    # the button and the state are drawn anew once done, whatever happened
    # (the page made them say "Saving..." and the button unpressable)
    on.exit(rv$save_ver <- shiny::isolate(rv$save_ver) + 1L, add = TRUE)
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    do_save()
  })
  output$save_state <- shiny::renderUI({
    rv$save_ver
    if (!has_study()) return(NULL)
    if (dirty()) {
      shiny::span(class = "rp-dirty me-2",
                  paste("\u25cf", t("Unsaved changes")))
    } else {
      shiny::span(class = "text-muted me-2", t("Saved"))
    }
  })
  output$study_side <- shiny::renderUI({
    if (!has_study()) {
      return(shiny::p(class = "text-muted", t("Open a study.")))
    }
    m <- rv$meta
    shiny::div(class = "mb-2",
               shiny::div(class = "rp-study", m$study_id),
               if (!is.na(m$title)) shiny::div(class = "small", m$title))
  })

  studies <- shiny::reactive({
    rv$studies_ver
    input$refresh_studies
    list_studies()
  })
  output$studies <- DT::renderDT({
    d <- studies()
    shiny::req(nrow(d) > 0L)
    last <- shiny::isolate(if (has_study()) rv$study$meta$study_id else
      tflplanner_config()$last_study)
    open_id <- shiny::isolate(if (has_study()) rv$study$meta$study_id)
    # a study whose run is going on (the sample's, made in the background;
    # an official run started before this app, which goes on by itself)
    running <- unique(c(if (!is.null(rv$job)) rv$job_study,
                        tryCatch(.live_runs()$study_id, error = function(e) NULL)))
    v <- data.frame(
      a = .study_list_ids(d, open_id, running, t),
      b = d$title, e = substr(d$saved, 1L, 16L),
      stringsAsFactors = FALSE)
    v[is.na(v)] <- ""
    # what picks a study: its ID, its title (whole, wrapped) and when it was
    # saved; the compound, phase and reports are in the detail on the right
    names(v) <- t(c("Study ID", "Title", "Last saved"))
    # the study to show selected: the open one, else the last one opened
    # (none, e.g. just unregistered: nothing selected)
    sel <- if (length(last) == 1L) match(last, d$study_id) else NA_integer_
    DT::datatable(
      v, rownames = FALSE, class = "compact hover",
      selection = list(mode = "single",
                       selected = if (!is.na(sel)) sel else NULL),
      callback = DT::JS(
        "table.on('dblclick', 'tbody tr', function() {",
        "  var i = table.row(this).index();",
        "  if (i !== undefined) Shiny.setInputValue('studies_dbl', i + 1, {priority: 'event'});",
        "});"),
      options = list(dom = if (nrow(v) > 10L) "ft" else "t", paging = FALSE,
                     ordering = FALSE, scrollY = "50vh",
                     scrollCollapse = TRUE))
  })
  # drawn also while the Study tab is hidden: a study made or copied from
  # another tab (the new study's step moves to the report list) is in the
  # list when the tab is shown again -- a hidden output invalidated waits for
  # an update that showing a tab does not send (the list is cheap, 0.07 s)
  shiny::outputOptions(output, "studies", suspendWhenHidden = FALSE)
  # the study open, in the bar of the tabs on every tab (a study is chosen
  # on the Study tab)
  output$open_study_bar <- shiny::renderUI({
    if (!has_study()) {
      return(shiny::span(class = "small text-muted me-2", t("No study open")))
    }
    m <- rv$meta
    ttl <- if (is.na(m$title %||% NA)) "" else m$title
    shiny::actionLink(
      "open_study_go",
      class = "small ms-3 d-inline-block text-truncate align-middle text-body text-decoration-none",
      style = "max-width: 32em;",
      title = paste0(m$study_id, " ", ttl, " -- ", t("studies are chosen on the Study tab")),
      shiny::tagList(shiny::span(class = "badge text-bg-light border me-1", m$study_id), ttl,
                     if (identical(rv$spec$status, "invalid"))
                       shiny::span(class = "badge text-bg-warning ms-1",
                                   title = t("The definition files do not read"),
                                   "\u26a0")))
  })
  shiny::observeEvent(input$open_study_go, go("study"))
  output$n_studies <- shiny::renderText(nrow(studies()))
  shiny::outputOptions(output, "n_studies", suspendWhenHidden = FALSE)
  output$save_btn <- shiny::renderUI({
    rv$save_ver
    if (has_study()) .btn("save", t("Save"), class = "btn-sm btn-primary",
                          `data-saving` = t("Saving..."))
  })
  # [Discard changes]: the study back to its last save -- every unsaved
  # change of it, as a save would write them (every report, the study's
  # files such as defaults.yml), and its draft gone.  Asked in the page
  # first; nothing to discard, the button cannot be pressed.
  output$discard_btn <- shiny::renderUI({
    if (!has_study()) return(NULL)
    .btn("discard", t("Discard changes"), class = "btn-sm btn-outline-secondary ms-1",
         disabled = !isTRUE(dirty()))
  })
  shiny::observeEvent(input$discard, {
    if (!isTRUE(dirty())) return()
    shiny::showModal(shiny::modalDialog(
      title = t("Discard changes"), easyClose = TRUE,
      shiny::p(t("Every change not saved goes -- in every report of the study and in the study's own files (defaults.yml ...), as a save would have written them -- and the study is as it was last saved. The draft goes too.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("discard_ok", t("Discard changes"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$discard_ok, {
    shiny::removeModal()
    held$p <- held$before <- NULL
    rv$p <- rv$saved
    rv$meta <- rv$saved_meta
    guarded(.drop_draft(rv$study$meta$study_id))
    # (the screens drawn again from it: the cards, the code, the preview)
    rv$bver <- rv$bver + 1L
    rv$ard_ver <- rv$ard_ver + 1L
    bump()
    notify(t("The changes were discarded: the study is as it was last saved."))
  })
  # [Close]: tflplanner stops -- where it runs on this computer only (an
  # app on a server is not one user's to stop).  Unsaved changes are asked
  # about first: save and close, close without saving, or cancel.  The same
  # in every browser, RStudio's windows included (they cannot ask on their
  # own when a window with unsaved changes closes).
  app_local <- shiny::reactive(.is_local_host(session$clientData$url_hostname))
  output$close_btn <- shiny::renderUI({
    if (isTRUE(app_local())) {
      .btn("close_app", t("Close"), class = "btn-sm btn-outline-secondary ms-1")
    }
  })
  shiny::observeEvent(input$close_app, {
    unsaved <- isTRUE(dirty())
    shiny::showModal(shiny::modalDialog(
      title = t("Close tflplanner"), easyClose = TRUE,
      # an official run goes on by itself (its batch folder, when the study
      # is opened again); a preview stops with tflplanner
      if (!is.null(rv$job)) shiny::div(
        class = if (identical(rv$job_kind, "official")) "alert alert-info py-2 small" else
          "alert alert-warning py-2 small",
        sprintf(if (identical(rv$job_kind, "official"))
                  t("%s goes on after tflplanner closes; open the study again to see how it went (Runs).") else
                  t("%s is going on: closing stops it, and its ARD and reports are left unfinished. Wait for it to end, or run it again later (Runs)."),
                sub("^(.)", "\\U\\1", rv$job_what %||% t("A run"), perl = TRUE))),
      shiny::p(if (unsaved) t("There are unsaved changes. Save them before tflplanner stops?") else
        t("tflplanner stops; then its window can be closed.")),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        if (unsaved) .btn("close_nosave", t("Close without saving"),
                          class = "btn-outline-danger"),
        if (unsaved) .btn("close_save", t("Save and close"), class = "btn-primary")
        else .btn("close_nosave", t("Close"), class = "btn-primary"))))
  })
  close_now <- function() {
    shiny::removeModal()
    session$sendCustomMessage("tflplanner-closed",
                              t("tflplanner has stopped. This window can be closed."))
    later::later(function() shiny::stopApp(), 0.5)
  }
  shiny::observeEvent(input$close_save, {
    do_save()
    # the save did not go through (it said why): stay open
    if (isTRUE(shiny::isolate(dirty()))) return()
    close_now()
  })
  shiny::observeEvent(input$close_nosave, close_now())
  # no study yet: the ways to start, the sample first
  output$welcome <- shiny::renderUI({
    if (nrow(studies())) return(NULL)
    shiny::div(
      class = "border rounded p-3 mb-3 bg-light",
      shiny::h5(t("Getting started")),
      shiny::p(t("tflplanner makes a study's tables, listings and figures: the data, the analyses (ARD), each report's definition, and the programs that make the RTFs.")),
      shiny::div(
        class = "d-flex flex-wrap gap-2 mb-2",
        .btn("try_sample", t("Try the sample study (about 1 minute)..."),
             class = "btn btn-primary"),
        .btn("start_empty", t("New study..."),
             class = "btn btn-outline-primary"),
        .btn("register2", t("Register a study folder"),
             class = "btn btn-outline-secondary")),
      shiny::p(class = "small text-muted mb-0",
               t("The sample uses tflspec's oncology sample data (simulated): it makes its ARD, 5 tables, a listing and 2 figures, to look at and change.")))
  })
  # both open the New study dialog: the sample chosen, or nothing chosen
  # yet; the study's ID is given there
  shiny::observeEvent(input$try_sample, open_new_study("sample"))
  shiny::observeEvent(input$start_empty, open_new_study("empty"))
  # SAMPLE-01, or the first SAMPLE-nn not taken
  next_sample_id <- function(root = studies_root()) {
    id <- "SAMPLE-01"
    k <- 1L
    while (id %in% studies()$study_id || dir.exists(file.path(root, id))) {
      k <- k + 1L
      id <- sprintf("SAMPLE-%02d", k)
    }
    id
  }
  shiny::observeEvent(input$register2, show_register())
  # A newer tflplanner / tflspec / rtfreporter: said, not installed -- the
  # app cannot replace packages it has loaded.  The check runs once per R
  # process in the background (.start_update_check()).
  .start_update_check()
  update_found <- shiny::reactive({
    r <- .update_check_result()
    if (is.null(r)) shiny::invalidateLater(2000)
    r
  })
  # a newer version installed while this app was running: this window runs
  # the old one until tflplanner is closed and started again (checked at
  # each session start and once a minute; nothing is stopped for the user)
  stale <- shiny::reactive({
    shiny::invalidateLater(60000)
    .stale_versions()
  })
  output$update_note <- shiny::renderUI({
    s <- stale()
    if (length(s)) {
      return(shiny::div(
        class = "alert alert-warning small py-2 mb-2",
        shiny::strong(t("A newer version is installed:")), " ",
        paste(vapply(names(s), function(p) sprintf("%s %s (%s %s)", p, s[[p]][["installed"]],
                                                      t("this window runs"), s[[p]][["running"]]), ""),
              collapse = ", "), ". ",
        t("Save, then Close (top right) and start tflplanner again: until then this window runs the old version.")))
    }
    r <- update_found()
    if (!length(r) || isTRUE(input$update_note_hide)) return(NULL)
    shiny::div(
      class = "alert alert-info alert-dismissible small py-2 mb-2",
      shiny::strong(t("A newer version is out:")), " ",
      paste(names(r), r, collapse = ", "), ". ",
      t("To update: close tflplanner and start it from the \"update and launch\" shortcut, or run update_tflplanner() in R."),
      shiny::tags$button(
        type = "button", class = "btn-close",
        onclick = "Shiny.setInputValue('update_note_hide', true);"))
  })
  # a home inside the study folders (set before 0.0.2.9200, #359): said,
  # nothing moved -- the user moves it
  output$home_note <- shiny::renderUI({
    rv$studies_ver
    problem <- tryCatch(.home_problem(tflplanner_home(), studies_root(), t),
                        error = function(e) NULL)
    if (is.null(problem)) return(NULL)
    shiny::div(
      class = "alert alert-warning small py-2 mb-2",
      problem, " ",
      help_tip(paste(t("To move it: close tflplanner, move the home folder, then run setup_tflplanner(home = \"<its new place>\") in R."),
                     t(.home_layouts))))
  })
  output$studies_root_note <- shiny::renderUI({
    rv$studies_ver
    shiny::p(class = "small text-muted mb-1",
             t("New study folders go to"), ": ",
             shiny::code(tflplanner_config()$studies_root %||% ""))
  })
  # the study the list points at: the selected row, else the open study
  shown_study <- shiny::reactive({
    d <- studies()
    i <- input$studies_rows_selected
    i <- i[i >= 1L & i <= nrow(d)]
    if (length(i)) return(d[i[1L], , drop = FALSE])
    if (has_study()) {
      k <- match(rv$study$meta$study_id, d$study_id)
      if (!is.na(k)) return(d[k, , drop = FALSE])
    }
    NULL
  })
  shows_open <- shiny::reactive({
    s <- shown_study()
    has_study() && !is.null(s) && identical(s$study_id,
                                            rv$study$meta$study_id)
  })
  output$shows_open <- shiny::renderText(if (shows_open()) "yes" else "no")
  shiny::outputOptions(output, "shows_open", suspendWhenHidden = FALSE)
  output$study_detail_title <- shiny::renderUI({
    s <- shown_study()
    if (is.null(s)) return(t("Study settings"))
    shiny::span(t("Study settings"), ": ", s$study_id, " ",
                shiny::span(class = "small text-muted",
                            if (shows_open()) t("(open)") else
                              t("(not open)")))
  })
  output$settings <- shiny::renderUI({
    rv$studies_ver
    shiny::tagList(
      shiny::p(t("tflplanner keeps the studies here (the master copy):"),
               shiny::br(), shiny::code(tflplanner_home())),
      shiny::div(
        class = "d-flex gap-2 align-items-end",
        shiny::textInput("studies_root", t("New study folders go to"),
                         value = studies_root(), width = "100%"),
        .btn("save_settings", t("Change"), class = "btn-sm mb-3")),
      shiny::div(
        class = "d-flex gap-2 align-items-end",
        shiny::selectInput("language", t("Language"), app_languages(),
                           selected = lang),
        .btn("save_language", t("Change"), class = "btn-sm mb-3")),
      shiny::checkboxInput(
        "check_updates",
        with_tip(t("Look for a newer version when tflplanner starts"),
                 sprintf(t("Updates (%s channel) are installed from R, not from the app: update_tflplanner(), or the \"update and launch\" shortcut."),
                         .update_channel())),
        value = isTRUE(as.logical(tflplanner_config()$check_updates %||% "false"))),
      shiny::div(
        class = "mb-2",
        .btn("add_sample", t("Add the sample study"), class = "btn-sm btn-outline-primary"),
        help_tip(t("The sample study: tflspec's oncology sample data (simulated), its ARD, 5 tables, a listing and 2 figures."))),
      shiny::hr(),
      shiny::p(shiny::strong(with_tip(t("Company standards"),
                                      t("Dropdowns, presets, statistics, ARD methods, listing types, and the defaults a new study starts with."))),
               shiny::br(),
               if (file.exists(.standards_file())) {
                 a <- company_standards()$about
                 sprintf("%s %s (%s)", a$value[a$key == "name"],
                         a$value[a$key == "version"], .standards_file())
               } else t("the built-in draft")),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::downloadButton("std_current", t("Download the standards in use"),
                              class = "btn-sm"),
        shiny::downloadButton("std_draft", t("Download the built-in draft"),
                              class = "btn-sm"),
        .btn("std_builtin", t("Back to the built-in draft"),
             class = "btn-sm btn-outline-secondary")),
      shiny::fileInput("std_upload", t("Install company standards (Excel)"),
                       accept = ".xlsx", width = "100%"))
  })
  shiny::observeEvent(input$add_sample, {
    s <- NULL
    shiny::withProgress(
      message = t("Adding the sample study and making its ARD and reports ..."),
      s <- guarded(suppressMessages(create_sample_study())))
    if (is.null(s)) return()
    rv$studies_ver <- rv$studies_ver + 1L
    notify(sprintf(t("Added %s: double-click it in the list to open it."),
                   s$meta$study_id))
  })
  output$std_current <- shiny::downloadHandler(
    filename = function() "company_standards.xlsx",
    content = function(file) standards_template(file, from = "current"))
  output$std_draft <- shiny::downloadHandler(
    filename = function() "company_standards_draft.xlsx",
    content = function(file) standards_template(file, from = "builtin"))
  shiny::observeEvent(input$std_upload, {
    f <- input$std_upload
    ok <- guarded(suppressMessages(setup_tflplanner(standards = f$datapath)))
    if (is.null(ok)) return()
    rv$studies_ver <- rv$studies_ver + 1L
    bump()
    notify(t("Company standards installed. They apply to what is offered from now on, and to new studies."))
  })
  shiny::observeEvent(input$std_builtin, {
    suppressMessages(setup_tflplanner(standards = "builtin"))
    rv$studies_ver <- rv$studies_ver + 1L
    bump()
    notify(t("Back to the built-in draft."))
  })
  shiny::observeEvent(input$save_settings, {
    r <- trimws(input$studies_root)
    if (!nzchar(r)) return()
    # never the home inside the study folders (#359): said in the app's words
    if (!is.null(.home_problem(tflplanner_home(), r))) {
      return(notify(sprintf(t("This place cannot be set (nothing saved): the home %s is inside it."),
                            tflplanner_home()), "error"))
    }
    if (is.null(guarded(suppressMessages(setup_tflplanner(studies_root = r))))) return()
    rv$studies_ver <- rv$studies_ver + 1L
    notify(sprintf(t("New studies go to %s"), r))
  })
  shiny::observeEvent(input$check_updates, {
    now <- isTRUE(as.logical(tflplanner_config()$check_updates %||% "false"))
    if (!identical(isTRUE(input$check_updates), now)) {
      guarded(suppressMessages(setup_tflplanner(
        check_updates = isTRUE(input$check_updates))))
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$save_language, {
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    guarded(suppressMessages(setup_tflplanner(language = input$language)))
    shiny::showNotification(
      shiny::tagList(t("The language is changed: the page reloads to show it."), " ",
                     shiny::tags$a(href = "javascript:location.reload()",
                                   t("Reload now"))),
      duration = NULL, type = "message")
    session$reload()
  })
  selected_study <- function() {
    d <- studies()
    i <- input$studies_rows_selected
    i <- i[i >= 1L & i <= nrow(d)]
    if (!length(i)) {
      notify(t("Choose a study"), "warning")
      return(NULL)
    }
    d[i[1L], , drop = FALSE]
  }
  shiny::observeEvent(input$register, show_register())
  show_register <- function() {
    shiny::showModal(shiny::modalDialog(
      title = t("Register an existing study folder"),
      shiny::textInput("reg_path", t("Study folder (the one with study.yml)"),
                       width = "100%"),
      shiny::p(class = "small text-muted",
               t("Definition workbooks in its spec/ become the study's definition.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("reg_ok", t("Register"),
                                   class = "btn-primary")),
      easyClose = TRUE))
  }
  shiny::observeEvent(input$reg_ok, {
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    s <- guarded(register_study(trimws(input$reg_path)))
    if (is.null(s)) return()
    shiny::removeModal()
    rv$studies_ver <- rv$studies_ver + 1L
    set_study(s)
    notify(sprintf(t("Registered %s"), s$meta$study_id))
    bslib::nav_select("nav", "outputs")
  })
  to_unregister <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$unregister, {
    d <- selected_study()
    if (is.null(d)) return()
    to_unregister(d$study_id)
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Unregister %s"), d$study_id),
      t("The study goes off the list. Its folder is not deleted; Register a folder brings it back as it was."),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("unregister_ok", t("Unregister"),
                                   class = "btn-danger"))))
  })
  shiny::observeEvent(input$unregister_ok, {
    id <- to_unregister()
    shiny::removeModal()
    to_unregister(NULL)
    if (length(id) != 1L || is.na(id) || !nzchar(id)) return()
    if (has_study() && identical(rv$study$meta$study_id, id)) {
      rv$study <- rv$p <- rv$saved <- rv$meta <- rv$saved_meta <- NULL
      bump()
    }
    if (is.null(guarded(unregister_study(id)))) return()
    rv$studies_ver <- rv$studies_ver + 1L
    notify(sprintf(t("Unregistered %s"), id))
  })
  open_row <- function(d) {
    if (is.null(d)) return()
    if (dirty()) {
      to_open(d)
      return(shiny::showModal(shiny::modalDialog(
        title = sprintf(t("%s has changes that are not saved"),
                        rv$study$meta$study_id),
        sprintf(t("Save them and open %s, or open it without saving them (they are kept as a draft: opening %s again offers them back)."),
                d$study_id, rv$study$meta$study_id),
        footer = shiny::tagList(
          shiny::modalButton(t("Cancel")),
          .btn("open_discard", t("Open without saving"),
               class = "btn-outline-secondary"),
          .btn("open_save", t("Save and open"), class = "btn-primary")))))
    }
    open_now(d)
  }
  to_open <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$open_save, {
    shiny::removeModal()
    if (isTRUE(do_save())) open_now(to_open())
  })
  shiny::observeEvent(input$open_discard, {
    shiny::removeModal()
    guarded(.write_draft(current_study()))
    open_now(to_open())
  })
  open_now <- function(d) {
    if (is.null(d)) return()
    s <- guarded(open_study(d$study_id))
    if (!is.null(s)) {
      set_study(s)
      notify(sprintf(t("Opened %s"), s$meta$study_id))
      rv$studies_ver <- rv$studies_ver + 1L
      bslib::nav_select("nav", "outputs")
    }
  }
  shiny::observeEvent(input$open_study, open_row(selected_study()))
  shiny::observeEvent(input$open_study2, open_row(shown_study()))
  shiny::observeEvent(input$studies_dbl, {
    d <- studies()
    i <- .dt_row_shown(input$studies_dbl, input$studies_rows_current)
    if (!is.na(i) && i >= 1L && i <= nrow(d)) open_row(d[i, , drop = FALSE])
  })
  shiny::observeEvent(input$new_study, open_new_study("sample"))
  open_new_study <- function(from = "sample") {
    shiny::showModal(shiny::modalDialog(
      title = t("New study"),
      shiny::radioButtons(
        "ns_from", t("Start from"),
        {
          ch <- stats::setNames(
            c("sample", "empty", "study"),
            c(t("The sample study: data, ARD definition, tables, listing and figures (made at once)"),
              t("Start empty"),
              t("Copy another study (definition and data code)")))
          # copying needs a study to copy from
          if (nrow(studies())) ch else ch[1:2]
        },
        selected = from, width = "100%"),
      shiny::conditionalPanel(
        "input.ns_from == 'study'",
        shiny::selectInput("ns_src", t("Copy from"),
                           stats::setNames(studies()$study_id,
                                           studies()$study_id))),
      shiny::textInput("ns_id",
                       t("Study ID (the folder name: letters, digits . _ -)"),
                       value = if (identical(from, "sample")) next_sample_id() else ""),
      shiny::uiOutput("ns_id_check"),
      shiny::tags$details(
        class = "mb-2",
        shiny::tags$summary(t("Details (optional)")),
        shiny::textInput("ns_title", t("Title"), width = "100%"),
        shiny::textInput("ns_compound", t("Compound")),
        shiny::textInput("ns_phase", t("Phase")),
        shiny::textAreaInput("ns_description", t("Description"), width = "100%")),
      shiny::tags$details(
        shiny::tags$summary(t("Where the study folder goes")),
        shiny::textInput("ns_root", t("Create the study folder in"),
                         value = studies_root(), width = "100%")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ns_ok", t("Create"),
                                   class = "btn-primary")),
      easyClose = TRUE))
  }
  # the ID is checked where it is typed, not in a message after Create
  ns_id_problem <- function(id) {
    id <- trimws(id %||% "")
    if (!nzchar(id)) return(t("Give the study an ID."))
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", id)) {
      return(t("Use letters, digits, '.', '_' or '-' only (e.g. ABC-101)."))
    }
    if (id %in% studies()$study_id) return(t("A study with this ID is already registered."))
    root <- trimws(input$ns_root %||% "")
    # the place: never one the home is in, nor inside a study (#359)
    path <- tryCatch(.study_path(root, id), error = function(e) e)
    if (inherits(path, "error")) return(.error_view(conditionMessage(path), t))
    if (nzchar(root) && dir.exists(path)) {
      return(sprintf(t("A folder %s is already there: choose another ID, or register that folder (Studies > Register a folder)."),
                     path))
    }
    NULL
  }
  output$ns_id_check <- shiny::renderUI({
    msg <- ns_id_problem(input$ns_id)
    if (!is.null(msg)) shiny::div(class = "small text-danger mb-2", msg)
  })
  # the sample suggests its own ID; another choice leaves the field to you
  shiny::observeEvent(input$ns_from, {
    id <- trimws(input$ns_id %||% "")
    if (identical(input$ns_from, "sample") && !nzchar(id)) {
      shiny::updateTextInput(session, "ns_id", value = next_sample_id())
    } else if (!identical(input$ns_from, "sample") && grepl("^SAMPLE-[0-9]+$", id)) {
      shiny::updateTextInput(session, "ns_id", value = "")
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$ns_ok, {
    if (!is.null(ns_id_problem(input$ns_id))) return()
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    blank <- function(v) if (is.null(v) || !nzchar(trimws(v))) NA else v
    s <- if (identical(input$ns_from, "sample")) {
      # the sample study under this ID: its data, ARD definition, tables,
      # listing and figures copied (seconds); its official run, which makes
      # the ARD and the reports (minutes), runs in the background below
      nz <- function(v) { v <- blank(v); if (is.na(v)) NULL else v }
      out <- NULL
      shiny::withProgress(
        message = t("Copying the sample study ..."),
        out <- guarded(suppressMessages(create_sample_study(
          root = trimws(input$ns_root), study_id = trimws(input$ns_id),
          title = nz(input$ns_title), compound = nz(input$ns_compound),
          phase = nz(input$ns_phase),
          description = nz(input$ns_description), run = FALSE))))
      out
    } else {
      # "empty": no reports, but the company's study defaults, analysis
      # sets and data catalog (create_study() with no planner)
      src <- if (identical(input$ns_from, "study")) guarded(open_study(input$ns_src))
      if (identical(input$ns_from, "study") && is.null(src)) return()
      made <- guarded(create_study(
        trimws(input$ns_id),
        title = blank(input$ns_title), compound = blank(input$ns_compound),
        phase = blank(input$ns_phase),
        description = blank(input$ns_description), planner = src$planner,
        root = trimws(input$ns_root)))
      # a copy: how the study reads its TOC comes too (input/toc/profile.yml),
      # not the TOCs it took in (the copy's TOC is its own)
      if (!is.null(made) && !is.null(src)) toc_copy_profile(src, made)
      made
    }
    if (is.null(s)) return()
    shiny::removeModal()
    rv$studies_ver <- rv$studies_ver + 1L
    set_study(s)
    if (identical(input$ns_from, "sample")) {
      rv$next_steps <- TRUE
      start_sample_run(s)
    }
    notify(sprintf(t("Created %s"), s$meta$study_id))
    bslib::nav_select("nav", "outputs")
  })
  # -- the next steps, once: after the sample is made, where to look ------
  rv$next_steps <- FALSE
  steps_flag <- function() file.path(tflplanner_home(), "next_steps_seen")
  output$next_steps <- shiny::renderUI({
    if (!isTRUE(rv$next_steps) || file.exists(steps_flag())) return(NULL)
    ids <- output_ids(rv$p)
    ft <- c(intersect("T-14-1-1", ids), ids)[1L]
    step <- function(text, id, label) shiny::tags$li(
      class = "mb-1", text, " ",
      .btn(id, label, class = "btn-sm btn-outline-primary py-0"))
    shiny::div(
      class = "border rounded p-3 mb-3 bg-light",
      shiny::div(
        class = "d-flex justify-content-between align-items-start",
        shiny::h5(t("Next steps")),
        .btn("steps_close", t("Close"), class = "btn-sm btn-link")),
      shiny::tags$ol(
        class = "mb-0",
        if (!is.na(ft)) step(sprintf(t("Open %s: its Content builds the table in words, with the table as it will print beside it."), ft),
                             "steps_table", t("Open it")),
        step(t("ARD: where the numbers come from (cards / cardx). A click on a row shows the analysis as a form."),
             "steps_ard", t("Go to ARD")),
        step(t("Runs: preview every report and open its RTF."),
             "steps_runs", t("Go to Runs"))))
  })
  shiny::observeEvent(input$steps_close, {
    rv$next_steps <- FALSE
    try(writeLines(format(Sys.time()), steps_flag()), silent = TRUE)
  })
  shiny::observeEvent(input$steps_table, {
    ids <- output_ids(rv$p)
    ft <- c(intersect("T-14-1-1", ids), ids)[1L]
    shiny::updateSelectInput(session, "target", selected = ft)
    go("tables")
  })
  shiny::observeEvent(input$steps_ard, go("ard"))
  shiny::observeEvent(input$steps_runs, go("results"))

  shiny::observeEvent(input$std_defaults, {
    if (!has_study()) return()
    p2 <- guarded(add_standard_defaults(rv$p, rv$study$meta$study_id))
    if (is.null(p2)) return()
    added <- attr(p2, "added")
    if (!length(added)) {
      return(notify(t("The study has every company default already.")))
    }
    attr(p2, "added") <- NULL
    rv$p <- p2
    bump()
    notify(sprintf(t("Added the company's defaults: %s (save to keep them)"),
                   paste(added, collapse = ", ")))
  })
  output$study_detail <- shiny::renderUI({
    s <- shown_study()
    if (is.null(s)) {
      return(shiny::p(class = "text-muted",
                      t("Choose a study in the list, or create one.")))
    }
    if (!shows_open()) {
      row <- function(k, v) shiny::tags$tr(
        shiny::tags$th(class = "pe-3 fw-normal text-muted", k),
        shiny::tags$td(v))
      na <- function(x) if (is.na(x) || !nzchar(x)) "-" else x
      return(shiny::tagList(
        shiny::tags$table(
          class = "small mb-2",
          row(t("Title"), na(s$title)),
          row(t("Compound"), na(s$compound)),
          row(t("Phase"), na(s$phase)),
          row(t("Description"), na(s$description)),
          row(t("Reports"), s$reports),
          row(t("ARD analyses"), s$analyses),
          row(t("Last saved"), na(s$saved)),
          row(t("Folder"), shiny::tagList(
            shiny::code(s$path),
            if (!s$folder) shiny::span(class = "text-danger ms-1",
                                       t("(not found)"))))),
        .btn("open_study2", t("Open this study"),
             class = "btn-sm btn-primary")))
    }
    rv$ver
    m <- shiny::isolate(rv$meta)
    r <- shiny::isolate(rv$p$study[["rounding"]])
    v <- function(x) if (is.na(x)) "" else x
    shiny::tagList(
      shiny::p(shiny::strong(m$study_id), shiny::br(),
               shiny::span(class = "small text-muted", rv$study$path)),
      .study_section("study", t("The study"), open = TRUE,
      shiny::textInput("m_title", with_token(t("Title"), "STUDY_TITLE"), v(m$title), width = "100%"),
      shiny::div(class = "d-flex flex-wrap gap-2",
                 shiny::textInput("m_short_title", with_token(with_tip(
                   t("Short title"),
                   t("The title for a header or footer.")), "STUDY_SHORT_TITLE"),
                   v(m$short_title)),
                 shiny::div(class = "shiny-input-container",
                   shiny::tags$label(class = "control-label",
                                     with_token(with_tip(t("Study ID"), t("The study's number: the folder's name and the header's. Set when the study is made; it cannot be changed here.")), "STUDY_ID")),
                   shiny::tags$input(type = "text", class = "form-control",
                                     value = m$study_id, readonly = NA))),
      shiny::div(class = "d-flex gap-2",
                 shiny::textInput("m_compound", with_token(t("Compound"), "STUDY_COMPOUND"), v(m$compound)),
                 shiny::textInput("m_phase", with_token(t("Phase"), "STUDY_PHASE"), v(m$phase))),
      shiny::textAreaInput("m_description", with_token(t("Description"), "STUDY_DESCRIPTION"),
                           v(m$description), width = "100%")),
      .study_section("every", t("Every report"), open = TRUE,
      shiny::selectInput(
        "rounding", t("Rounding"),
        stats::setNames(c("", "r", "sas"),
                        t(c("As options(rtfreporter.rounding)",
                            "r (half to even)", "sas (half away from zero)"))),
        selected = v(r)),
      shiny::textInput(
        "study_treatment",
        with_tip(t("Treatment variable (settings.treatment)"),
                 t("The variable of the treatment groups a template's by / group takes when the report does not say it (study.yml settings: treatment:). Blank: the template's own, TRT01A.")),
        v(.study_setting(shiny::isolate(rv$p), "treatment")), placeholder = "TRT01A"),
      shiny::h6(with_tip(t("Every report's header"),
                         t("The study's words in every report's header: {COMPANY}, {ANALYSIS_TYPE}; {STUDY_ID} is the Study ID above. Set once here (the tokens sheet's study rows); each report gives only its own (its label, title, population)."))),
      shiny::div(class = "d-flex flex-wrap gap-2",
        shiny::textInput("tok_COMPANY", with_token(t("Company"), "COMPANY"), v(study_token(rv$p, "COMPANY"))),
        shiny::textInput("tok_ANALYSIS_TYPE", with_token(t("Analysis"), "ANALYSIS_TYPE"),
                         v(study_token(rv$p, "ANALYSIS_TYPE")),
                         placeholder = "Final Analysis")),
      .study_id_token_note(rv$p, m$study_id, t),
      shiny::h6(with_tip(t("Every report's font"),
                         t("The font and size every report is written in, set once here (the page sheet's study row; programs/tfl/report_setup.R says them as options()). Blank: rtfreporter's, Courier 9 pt. The company standards give a new study theirs."))),
      shiny::div(class = "d-flex flex-wrap gap-2",
        shiny::textInput("pg_font", t("Font"), v(study_page_value(rv$p, "font")),
                         placeholder = "Courier"),
        shiny::textInput("pg_font_size", t("Size (pt)"),
                         .points(study_page_value(rv$p, "font_size_half_points")),
                         placeholder = "9", width = "8em")),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center mb-2",
        .btn("std_defaults", t("Add the company's study defaults (only what is missing)"),
             class = "btn-sm btn-outline-primary"),
        help_tip(t("For a study made without them: the table look (stub, blank rows, column headers, widths), headers and footers, the font, populations and data catalog. Nothing already there is changed.")))),
      # the study standard's plan_cells(), and the company standard's
      # items it differs in, copied one by one (#454)
      .study_section("cells", builder_label(.verb_meaning("plan_cells"), "plan_cells", "fn", lang),
                     shiny::div(class = "small text-muted mb-1", t("Study standard")),
                     shiny::uiOutput("std_cells_ui")))
  })
  output$std_cells_ui <- shiny::renderUI({
    shiny::req(shows_open())
    rv$p
    guarded(.cells_std_ui(rv$p, lang))
  })
  shiny::observeEvent(input$std_cells_copy, {
    if (!has_study()) return()
    keys <- input$std_cells_pick
    if (!length(keys)) return(notify(t("Tick the items to copy.")))
    sc <- .cells_company_apply(.cells_study(rv$p), keys)
    p2 <- guarded(.spec_set_study_cells(rv$p, sc))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    notify(sprintf(t("Copied from the company standard: %s (save to keep them)"),
                   paste(keys, collapse = ", ")))
  })
  output$study_actions <- shiny::renderUI({
    shiny::req(shows_open())
    lay <- study_layout()
    # one group each: Export, Import, Restore -- a name and a ? (the long
    # text is the help), nothing between them that reads as another's note
    grp <- function(title, tip, ...) shiny::div(
      class = "border rounded px-2 pt-1 pb-2 mb-2",
      shiny::div(class = "small fw-bold mb-1", with_tip(title, tip)),
      ...)
    .study_section("files", with_tip(t("Files"), t("To edit the definition outside the app: export the files, edit the copy (in Excel; rename it as you like), and import it back. Do not edit spec/ in place: if you do, the app asks before it takes the change in.")),
      shiny::tags$details(
        shiny::tags$summary(t("Folders")),
        shiny::tags$pre(class = "small", paste(
          sprintf("%-15s %s", paste0(lay, "/"),
                  .study_folder_notes(names(lay), t)),
          collapse = "\n"))),
      grp(t("Export"), t("A copy of spec/ and study.yml as a zip (the ARD definition as an Excel workbook): edit the copy, rename it as you like, and bring it back with Import."),
        shiny::downloadButton("spec_files_zip", t("Export the definition files"),
                              class = "btn-sm")),
      grp(t("Import definition files"), t("Edit the exported copy in Excel, then choose it here: a screen shows what differs, and only the parts you check are taken in; the files before are kept in spec/.backup. This is the study's definition, not the company standards."),
        shiny::fileInput(
          "spec_import", NULL, placeholder = t("(an edited copy: workbooks, .yml, .json or the zip)"),
          multiple = TRUE, accept = c(".xlsx", ".yml", ".yaml", ".json", ".zip"),
          width = "100%")),
      grp(t("Restore from a backup"), t("Every import keeps the files as they were in spec/.backup/. Choose one to bring the study back to it (the study as it is now is kept first): the way to undo a wrong edit."),
        .btn("restore_open", t("Restore from a backup ..."),
             class = "btn-sm btn-outline-secondary")),
      shiny::div(class = "d-flex flex-wrap gap-2 align-items-center rp-upload mb-2",
                 .btn("ars_open", t("Export the analyses as CDISC ARS"),
                      class = "btn-sm btn-outline-secondary"),
                 shiny::downloadButton("study_zip",
                                       t("Download the study (zip)"),
                                       class = "btn-sm"),
                 shiny::checkboxInput("zip_data", t("with data"))),
      .btn("study_folder", t("Open the study folder")))
  })
  for (k in c("title", "short_title", "compound", "phase", "description")) local({
    key <- k
    shiny::observeEvent(input[[paste0("m_", key)]], {
      v <- trimws(input[[paste0("m_", key)]])
      v <- if (nzchar(v)) v else NA_character_
      if (!identical(rv$meta[[key]], v)) rv$meta[[key]] <- v
    }, ignoreInit = TRUE)
  })
  for (k in .study_token_names) local({
    key <- k
    shiny::observeEvent(input[[paste0("tok_", key)]], {
      v <- trimws(input[[paste0("tok_", key)]])
      now <- study_token(rv$p, key)
      if (!identical(if (is.na(now)) "" else now, v)) {
        rv$p <- set_study_token(rv$p, key, v)
      }
    }, ignoreInit = TRUE)
  })
  # a field a header, footer, title or footnote says ({STUDY_TITLE} ...) has
  # its token row with the field's value, as the field changes
  shiny::observe({
    shiny::req(has_study())
    p <- rv$p
    p2 <- .meta_tokens(p, rv$meta)
    if (!identical(p2, p)) rv$p <- p2
  })
  shiny::observeEvent(input$tok_study_id_use, {
    rv$p <- set_study_token(rv$p, "STUDY_ID", rv$study$meta$study_id)
    rv$ver <- rv$ver + 1L
  })
  shiny::observeEvent(input$pg_font, {
    v <- trimws(input$pg_font)
    now <- study_page_value(rv$p, "font")
    if (!identical(if (is.na(now)) "" else now, v)) {
      rv$p <- set_study_page_value(rv$p, "font", v)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$pg_font_size, {
    v <- trimws(input$pg_font_size)
    hp <- if (nzchar(v)) .half_points(v) else NA_character_
    if (nzchar(v) && is.na(hp)) {
      return(notify(t("The size is a number of points (9, 10 ...)."), "warning"))
    }
    if (!identical(study_page_value(rv$p, "font_size_half_points"), hp)) {
      rv$p <- set_study_page_value(rv$p, "font_size_half_points", hp)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$rounding, {
    v <- if (nzchar(input$rounding)) input$rounding else NA_character_
    if (!identical(unname(rv$p$study[["rounding"]]), v)) {
      rv$p$study[["rounding"]] <- v
    }
  }, ignoreInit = TRUE)
  # the treatment variable: study.yml's settings: treatment: (blank: none)
  shiny::observeEvent(input$study_treatment, {
    v <- trimws(input$study_treatment)
    v <- if (nzchar(v)) v else NA_character_
    if (!identical(.study_setting(rv$p, "treatment"), v)) {
      rv$p$study[["treatment"]] <- v
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$study_folder, .open_folder(rv$study$path))
  # Export: a copy of the definition files to edit; Import: an edited copy
  # back, checked in a temporary folder first, taken in part by part
  output$spec_files_zip <- shiny::downloadHandler(
    filename = function() paste0(rv$study$meta$study_id, "_definition_",
                                 format(Sys.time(), "%Y%m%d-%H%M"), ".zip"),
    content = function(file) export_spec_files(rv$study$meta$study_id, file))
  # the dialog that follows an import (or a restore): what differs, the
  # parts to take in
  show_import_modal <- function(pv, names) {
    if (!pv$ok) {
      return(shiny::showModal(shiny::modalDialog(
        title = sprintf(t("The files cannot be imported: %s"), names),
        shiny::p(t("Nothing was changed. Fix the copy and import it again.")),
        spec_problems_table(pv$problems),
        size = "l", easyClose = TRUE, footer = shiny::modalButton(t("Close")))))
    }
    if (!length(pv$parts)) {
      return(notify(sprintf(t("%s is the same as the study's definition: nothing to import."), names)))
    }
    shiny::showModal(shiny::modalDialog(
      title = sprintf(if (!is.null(rv$import$backup)) t("Restore a backup: %s") else t("Import definition files: %s"), names),
      # which file was read, and as which of the study's
      shiny::p(class = "small mb-1",
               if (!is.null(rv$import$backup)) t("The backup was read. Choose the parts to bring back.") else
                 t("The chosen files were read. Choose the parts to take in.")),
      if (is.null(rv$import$backup)) shiny::p(class = "small text-muted mb-2",
               sprintf(t("Read as %s."), paste(pv$files, collapse = ", "))),
      shiny::checkboxGroupInput("import_parts", t("Parts that differ (take in the ones checked): the study's \u2192 the copy's"),
                                stats::setNames(pv$parts, paste0(pv$labels, ": ", pv$summary)),
                                selected = pv$parts, width = "100%"),
      shiny::p(class = "small",
               t("A part checked is replaced whole by the copy's version; a part not checked stays as the study has it.")),
      if (length(pv$outputs)) shiny::p(class = "small",
        paste0(t("Reports"), ": ", paste(pv$outputs, collapse = ", "))),
      if (!is.null(pv$problems) && nrow(pv$problems)) shiny::tagList(
        shiny::p(class = "small text-warning", t("Warnings (the import is not stopped by them):")),
        spec_problems_table(pv$problems)),
      shiny::p(class = "small text-muted",
               t("Before anything changes, the study's files as they are now are kept in spec/.backup/<date>-<time>/; then the study is saved. Cancel changes nothing.")),
      size = "l", easyClose = FALSE,
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        .btn("import_ok", t("Import the parts checked"), class = "btn-primary"))))
  }
  shiny::observeEvent(input$spec_import, {
    shiny::req(has_study())
    f <- input$spec_import
    # an import saves the study: unsaved changes would go in with it
    if (isTRUE(dirty())) {
      return(notify(t("There are unsaved changes. Save first, then choose the file again."), "warning"))
    }
    dir <- tempfile("import")
    dir.create(dir)
    tmp <- file.path(dir, f$name)
    file.copy(f$datapath, tmp)
    src <- if (length(tmp) == 1L) tmp else dir
    pv <- guarded(preview_spec_import(rv$study$meta$study_id, src))
    if (is.null(pv)) return()
    names <- paste(f$name, collapse = ", ")
    rv$import <- list(path = src, preview = pv, names = names)
    show_import_modal(pv, names)
  })
  shiny::observeEvent(input$restore_open, {
    shiny::req(has_study())
    b <- list_spec_backups(rv$study$meta$study_id)
    if (!nrow(b)) {
      return(notify(t("There is no backup yet: an import keeps the files as they were.")))
    }
    shiny::showModal(shiny::modalDialog(
      title = t("Restore from a backup"),
      shiny::selectInput("restore_pick", t("Backup (the files as they were before an import)"),
                         stats::setNames(b$name, sprintf("%s (%d)",
                           format(b$time, "%Y-%m-%d %H:%M:%S"), b$files)),
                         width = "100%"),
      shiny::p(class = "small text-muted",
               t("Next you see what would change; nothing is taken in until you choose.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("restore_show", t("Show what would change"),
                                   class = "btn-primary"))))
  })
  shiny::observeEvent(input$restore_show, {
    shiny::req(has_study(), nzchar(input$restore_pick %||% ""))
    id <- rv$study$meta$study_id
    pv <- guarded(preview_spec_backup(id, input$restore_pick))
    if (is.null(pv)) return()
    rv$import <- list(path = NULL, preview = pv, backup = input$restore_pick,
                      names = input$restore_pick)
    shiny::removeModal()
    show_import_modal(pv, input$restore_pick)
  })
  shiny::observeEvent(input$import_ok, {
    im <- rv$import
    shiny::req(has_study(), !is.null(im))
    parts <- input$import_parts
    if (!length(parts)) return(notify(t("Check a part to import"), "warning"))
    if (isTRUE(dirty())) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    shiny::removeModal()
    s <- guarded(if (!is.null(im$backup)) {
      restore_spec_backup(rv$study$meta$study_id, im$backup, parts = parts)
    } else import_spec_files(rv$study$meta$study_id, im$path, parts = parts))
    rv$import <- NULL
    if (is.null(s)) return()
    rv$study <- s
    rv$p <- rv$saved <- .study_spec_keys(s$planner)
    rv$meta <- rv$saved_meta <- s$meta[.study_fields]
    backup <- sub(paste0("^", s$path, "/"), "", s$backup)
    rv$spec <- list(status = "imported", parts = parts, outputs = s$spec$outputs,
                    from = im$names, backup = backup)
    rv$status_ver <- rv$status_ver + 1L
    rv$ard_ver <- rv$ard_ver + 1L
    rv$studies_ver <- rv$studies_ver + 1L
    bump()
    notify(sprintf(t("Imported: %s. The files before are in %s."),
                   paste(.part_labels(parts), collapse = ", "), backup))
  })
  # -- CDISC ARS: a dialog with the checks, then the files ----------------
  # built once when the dialog opens and again when the profile changes;
  # both downloads write what is shown
  ars_built <- shiny::reactiveVal(NULL)
  ars_build_now <- function(profile) {
    b <- guarded(.ars_build(current_study(), profile))
    ars_built(b)
    b
  }
  shiny::observeEvent(input$ars_open, {
    shiny::req(has_study())
    if (!nrow(rv$p$ard$analyses)) {
      return(notify(t("The study has no analyses in its ARD definition."), "warning"))
    }
    if (is.null(ars_build_now("cdisc"))) return()
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("CDISC ARS: %s"), rv$study$meta$study_id),
      shiny::radioButtons("ars_profile", NULL, inline = TRUE,
                          choiceNames = c(t("CDISC (to exchange)"),
                                          t("siera (to run with siera)")),
                          choiceValues = c("cdisc", "siera")),
      shiny::uiOutput("ars_summary"),
      size = "l", easyClose = TRUE,
      footer = shiny::tagList(
        # the three the same size: modalButton() has no class of its own
        shiny::tags$button(type = "button", class = "btn btn-sm btn-default",
                           `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
                           t("Close")),
        shiny::downloadButton("ars_json", t("JSON only"), class = "btn-sm"),
        shiny::downloadButton("ars_zip", t("JSON, Excel and the checks (zip)"),
                              class = "btn-sm btn-primary"))))
  })
  shiny::observeEvent(input$ars_profile, {
    b <- ars_built()
    if (!is.null(b) && !identical(b$profile, input$ars_profile)) {
      ars_build_now(input$ars_profile)
    }
  }, ignoreInit = TRUE)
  output$ars_summary <- shiny::renderUI({
    b <- ars_built()
    shiny::req(b)
    .ars_summary_ui(b, t)
  })
  shiny::observeEvent(input$ars_goto, {
    b <- ars_built()
    i <- suppressWarnings(as.integer(input$ars_goto))
    shiny::req(b, !is.na(i), i >= 1L, i <= nrow(b$problems))
    shiny::removeModal()
    review_jump(.ars_jump_row(b$problems[i, ]))
  })
  ars_file <- function(ext) paste0(rv$study$meta$study_id, "_ars",
                                   if (identical(input$ars_profile, "siera")) "_siera", ext)
  output$ars_json <- shiny::downloadHandler(
    filename = function() ars_file(".json"),
    content = function(file) {
      d <- tempfile("ars")
      f <- .ars_write(ars_built(), d, parts = "json")
      file.copy(f[["json"]], file, overwrite = TRUE)
    })
  output$ars_zip <- shiny::downloadHandler(
    filename = function() ars_file(".zip"),
    content = function(file) {
      d <- tempfile("ars")
      .ars_write(ars_built(), d)
      zip::zipr(file, list.files(d, full.names = TRUE))
    })
  output$study_zip <- shiny::downloadHandler(
    filename = function() paste0(rv$study$meta$study_id, ".zip"),
    content = function(file) {
      s <- rv$study
      top <- list.files(s$path, all.files = FALSE)
      if (!isTRUE(input$zip_data)) top <- setdiff(top, "data")
      zip::zip(file, file.path(basename(s$path), top),
               root = dirname(s$path))
    })

  # -- the report chosen in the sidebar --------------------------------
  # (a report the server chose -- added, copied: after_id_change() -- not
  # come back from the page yet is the one: drawn again meanwhile, the
  # list must not send the page's old one -- the report before would be
  # drawn between, #440)
  tgt_sent <- new.env()
  shiny::observeEvent(input$target, {
    tgt_sent$v <- NULL
  }, priority = 200)
  # (the pick, while it is the page's news: none once the page shows it or
  # after 3 s -- the page sends nothing when it already showed that report)
  tgt_pick <- function() {
    v <- tgt_sent$v
    if (is.null(v)) return(NULL)
    if (identical(shiny::isolate(input$target), v) ||
        difftime(Sys.time(), tgt_sent$t %||% Sys.time(), units = "secs") > 3) {
      tgt_sent$v <- NULL
      return(NULL)
    }
    v
  }
  shiny::observe({
    rv$ver
    p <- shiny::isolate(rv$p)
    ids <- if (is.null(p)) character() else output_ids(p)
    cur <- tgt_pick() %||% shiny::isolate(input$target)
    ch <- if (is.null(p)) c("-" = .all_rows) else
      stats::setNames(list(
        stats::setNames(ids, ids),
        stats::setNames(c(.default_rows, .all_rows),
                        c(t("Study defaults"), t("ALL (every row)")))),
        c(t("Reports"), t("Rows for every report (ARD, Page)")))
    vals <- c(ids, .default_rows, .all_rows)
    want <- shiny::isolate(rv$want)
    rv$want <- NULL
    sel <- if (length(want) && !is.na(want) && want %in% vals) want else
      if (!is.null(cur) && cur %in% vals) cur else
        if (length(ids)) ids[1L] else .default_rows
    shiny::updateSelectInput(session, "target", choices = ch, selected = sel)
  })
  .srv_keep(environment())
}
