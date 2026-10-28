# app_server(): figure page -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_figure_page <- function() {
  # a figure on its page: drawn at its size (a design's, drawn once for a
  # design and size; one of its own code's, its last run in step 2), as
  # wide on the sample as on the page
  page_fig_memo <- new.env(parent = emptyenv())
  page_sample_figure <- function(p, id) {
    sz <- .fig_size_of(p, id)
    body_w <- .fig_size_of(set_page_cell(p, "report", "figure_width_in", id, "fit"), id)$width_in
    pct <- round(min(100, 100 * sz$width_in / body_w))
    caption <- shiny::div(class = "text-muted small text-center", .fig_size_words(sz, t))
    img <- function(bytes) shiny::tags$img(
      src = paste0("data:image/png;base64,", jsonlite::base64_enc(bytes)),
      style = sprintf("width: %s%%; height: auto; display: block; margin: 0 auto;", pct))
    png <- if (.is_code_step(p, id, "content")) {
      res <- uc_res()
      figs <- if (!is.null(res) && is.null(res$error)) .code_preview_parts(res$content)$figures
      if (length(figs)) figs[[1L]]
    } else {
      design <- fig_design(p, id)
      if (!is.null(design)) {
        key <- list(design, sz, id)
        if (!identical(page_fig_memo$key, key)) {
          s <- rv$study
          s$planner <- p
          r <- tryCatch(preview_figure(s, id, design, max_px = 1000), error = function(e) NULL)
          page_fig_memo$key <- key
          page_fig_memo$png <- if (!is.null(r$png) && file.exists(r$png))
            readBin(r$png, "raw", file.size(r$png))
        }
        page_fig_memo$png
      }
    }
    if (is.null(png)) {
      return(shiny::div(class = "text-muted small text-center py-4",
                        if (.is_code_step(p, id, "content")) t("(the figure: [Run the code] in step 2 draws it here)")
                        else t("(the figure: its design in step 2 draws it here)")))
    }
    shiny::tagList(img(png), caption)
  }
  output$builder_preview <- shiny::renderUI({
    # no ARD yet: the note above says it, with its button; here only a word
    if (identical(builder_case(), "meta")) {
      return(shiny::div(class = "small text-muted",
                        t("Nothing yet: the button above makes this table's ARD.")))
    }
    pv <- preview_now()
    if (!is.null(pv$error)) {
      return(shiny::div(class = "small text-muted", pv$error))
    }
    preview_html(pv$pages)
  })
  output$builder_pages <- shiny::renderUI({
    pv <- preview_now()
    if (is.null(pv$pages)) return(NULL)
    shiny::span(class = "small text-muted",
                sprintf(t("%d pages"), length(pv$pages)))
  })

  # -- report list -------------------------------------------------------
  outputs_view <- shiny::reactive({
    # what the list shows changed (not a table's cell, say)
    p_list()
    rv$ver
    rv$status_ver
    p <- shiny::isolate(rv$p)
    empty <- data.frame(output_id = character(), section = character(),
                        batches = character(), type = character(),
                        program = character(), rtf = character(),
                        data = character(), title = character())
    if (is.null(p) || !nrow(p$outputs)) return(empty)
    o <- p$outputs
    info <- lapply(o$output_id, function(id) report_info(p, id))
    type <- vapply(info, `[[`, "", "type")
    # the ARD's state as the ARD step has it (worked out once, for both;
    # a run in the background does not draw the table again for it)
    st <- tryCatch(shiny::isolate(ard_state()), error = function(e) NULL)
    ard_of <- if (!is.null(st)) st$state[match(o$output_id, st$output_id)] else
      rep(NA_character_, nrow(o))
    # the datasets the report reads (lower case, as the files are named);
    # a step of its own code, said after them
    data <- vapply(seq_len(nrow(o)), function(i) {
      ds <- tolower(.report_datasets(p, o$output_id[i], type[i]))
      txt <- if (length(ds)) paste(ds, collapse = " / ") else "-"
      if (.is_code_step(p, o$output_id[i], "data") || .is_code_step(p, o$output_id[i], "content")) {
        note <- t("(your code)")
        # a full-width bracket brings its own space
        txt <- paste0(txt, if (startsWith(note, "\uff08")) "" else " ", note)
      }
      txt
    }, "")
    # the ARD's state, on hover
    data_tip <- ifelse(is.na(ard_of), "",
                       paste("ARD:", t(unname(.ard_state_labels[ard_of]))))
    # the title (the titles sheet's lines; without them, the report's
    # description), cut short in the list; whole on hover
    titles <- vapply(o$output_id, function(id) .report_title(p, id), "",
                     USE.NAMES = FALSE)
    # the report's title its header prints ({OUTPUT_TITLE}, step 3), first
    tk <- .normalize_sheet(p$sheets$tokens, "tokens")
    head_title <- tk$value[match(paste(o$output_id, "OUTPUT_TITLE"),
                                 paste(tk$output_id, trimws(tk$name)))]
    has_head <- !is.na(head_title) & nzchar(trimws(head_title))
    titles[has_head] <- ifelse(nzchar(titles[has_head]),
                               paste(head_title[has_head], titles[has_head], sep = " / "),
                               head_title[has_head])
    titles <- ifelse(nzchar(titles) | is.na(o$description), titles,
                     o$description)
    data.frame(
      output_id = o$output_id,
      section = .report_section(o$output_id, type, o$section),
      batches = ifelse(is.na(o$batches %||% NA_character_), "", o$batches %||% ""),
      type = t(unname(.type_labels[type])),
      program = vapply(info, `[[`, "", "program"),
      rtf = vapply(info, `[[`, "", "file"),
      data = .cell_tip(data, data_tip),
      title = .cell_tip(.ellipsis(titles, 70L), titles),
      stringsAsFactors = FALSE)
  })
  output$outputs <- DT::renderDT({
    v <- outputs_view()
    # (a report just chosen by the server, not come back yet: that one)
    sel <- match(tgt_pick() %||% shiny::isolate(input$target), v$output_id)
    v$output_id <- htmltools::htmlEscape(v$output_id)
    v$section <- htmltools::htmlEscape(v$section)
    v$batches <- htmltools::htmlEscape(v$batches)
    v$program <- htmltools::htmlEscape(v$program)
    v$rtf <- htmltools::htmlEscape(v$rtf)
    # "Title" here is the report's title; t("Title") is the study's
    names(v) <- c(t(c("output_id", "Section", "Report sets", "Type", "Program", "RTF", "Data")),
                  if (identical(lang, "en")) "Title" else t("Report title"))
    v$.key <- .report_search_keys(shiny::isolate(picker_rows()), outputs_view()$output_id)
    DT::datatable(v, rownames = FALSE, escape = FALSE,
                  # a double click opens the report, as in the study list
                  callback = DT::JS(
                    "table.on('dblclick', 'tbody tr', function() {",
                    "  var i = table.row(this).index();",
                    "  if (i !== undefined) Shiny.setInputValue('outputs_dbl', i + 1, {priority: 'event'});",
                    "});"),
                  selection = list(mode = "single",
                                   selected = if (!is.na(sel)) sel),
                  options = list(dom = "t", paging = FALSE,
                                 ordering = FALSE, scrollX = TRUE,
                                 columnDefs = list(list(visible = FALSE, targets = ncol(v) - 1L)),
                                 search = list(search = .report_dt_search(shiny::isolate(input$outputs_q)))))
  })
  shiny::observeEvent(input$target, {
    v <- outputs_view()
    sel <- match(input$target, v$output_id)
    # (the table not drawn yet, its tab not opened: it is drawn with the
    # report selected, so nothing to tell it -- #445)
    if (is.null(input$outputs_rows_all)) return()
    if (!identical(input$outputs_rows_selected, if (!is.na(sel)) sel)) {
      DT::selectRows(DT::dataTableProxy("outputs"), if (!is.na(sel)) sel)
    }
  })
  # the report sets are defined on the Runs tab (a set is chosen there for
  # an official run); the report list only shows them (its Report sets
  # column), and this button goes there
  shiny::observeEvent(input$report_batches, bslib::nav_select("nav", "results"))

  # a double click on a report: open its Content
  shiny::observeEvent(input$outputs_dbl, {
    v <- shiny::isolate(outputs_view())
    id <- v$output_id[.dt_row_shown(input$outputs_dbl, input$outputs_rows_current)]
    if (!length(id) || is.na(id)) return()
    shiny::updateSelectInput(session, "target", selected = id)
    go(switch(report_info(rv$p, id)$type, listing = "lf", figure = "designer",
              "tables"))
  })
  shiny::observeEvent(input$outputs_rows_selected, {
    v <- shiny::isolate(outputs_view())
    id <- v$output_id[input$outputs_rows_selected]
    # (the list drawn again while the server's choice is on its way: its
    # old selection is not a choice -- #440)
    pk <- tgt_pick()
    if (!is.null(pk) && !identical(id, pk)) return()
    if (length(id) && !identical(id, input$target)) {
      shiny::updateSelectInput(session, "target", selected = id)
    }
  })

  # Detail editors follow the chosen report; editing writes it back.
  # Every value the server puts in an editor comes back once, as an
  # input event, unless the editor already showed it -- and a text box
  # reports late (it is debounced), so an echo can arrive after the next
  # fill.  Each fill that will echo is queued; an event matching a queued
  # value is that echo, and anything else is the user's edit, of what the
  # editor shows now: the latest fill.
  filled <- new.env()
  fill <- function(input_id, value, owner, update) {
    f <- filled[[input_id]] %||% list(pending = list())
    shown <- shiny::isolate(input[[input_id]])
    if (!identical(shown, value)) f$pending <- c(f$pending, list(value))
    f$owner <- owner
    filled[[input_id]] <- f
    update(session, input_id, value = value)
  }
  edit_of <- function(input_id, value) {
    f <- filled[[input_id]]
    if (is.null(f)) return(NULL)
    hit <- which(vapply(f$pending, identical, NA, value))
    if (length(hit)) {
      # echoes come in order: one that arrived means those before it
      # will not
      f$pending <- f$pending[-seq_len(hit[1L])]
      filled[[input_id]] <- f
      return(NULL)
    }
    f$owner
  }
  study_key <- function() if (has_study()) rv$study$meta$study_id

  editors <- c(description = "text", section = "text", process_code = "area",
               dc_code = "area", cc_code = "area")
  # an editor and the field it edits: the report list's, or a step's own
  # code (the report sheet's, step_code())
  editor_field <- c(dc_code = "step:data", cc_code = "step:content")
  field_of <- function(e) if (e %in% names(editor_field)) editor_field[[e]] else e
  field_value <- function(id, field) {
    if (startsWith(field, "step:")) return(step_code(rv$p, id, sub("^step:", "", field)))
    rv$p$outputs[[field]][match(id, rv$p$outputs$output_id)]
  }
  shiny::observeEvent(list(current(), rv$ver), {
    id <- current()
    val <- function(v) if (length(v) && !is.na(v)) v else ""
    owner <- if (!is.null(id)) list(study = study_key(), id = id)
    for (e in names(editors)) {
      fill(e, if (is.null(id)) "" else val(field_value(id, field_of(e))), owner,
           if (editors[[e]] == "text") shiny::updateTextInput else
             shiny::updateTextAreaInput)
    }
  })
  output$current_label <- shiny::renderUI({
    id <- current()
    if (is.null(id)) {
      shiny::p(class = "text-muted",
               t("Choose a report above, or in the list on the left (> opens it)."))
    } else {
      shiny::h5(id, shiny::span(class = "badge bg-secondary ms-1",
                                .type_labels[[report_info(rv$p, id)$type]]))
    }
  })
  set_field <- function(input_id, value) {
    owner <- edit_of(input_id, value)
    if (is.null(owner) || !identical(owner$study, study_key())) return()
    field <- field_of(input_id)
    value <- if (is.null(value) || !nzchar(trimws(value))) NA_character_ else
      value
    i <- which(rv$p$outputs$output_id == owner$id)
    if (length(i) && startsWith(field, "step:")) {
      step <- sub("^step:", "", field)
      if (!identical(step_code(rv$p, owner$id, step), value)) {
        rv$p <- set_step_code(rv$p, owner$id, step, value)
      }
      return()
    }
    if (length(i) && !identical(rv$p$outputs[[field]][i], value)) {
      rv$p$outputs[[field]][i] <- value
      # the other editors of the same field show it too
      for (e in setdiff(names(editors)[vapply(names(editors), field_of, "") == field],
                        input_id)) {
        fill(e, if (is.na(value)) "" else value, owner, shiny::updateTextAreaInput)
      }
    }
  }
  for (e in names(editors)) local({
    input_id <- e
    shiny::observeEvent(input[[input_id]], set_field(input_id, input[[input_id]]),
                        ignoreInit = TRUE)
  })

  shiny::observeEvent(rv$ver, {
    s <- if (is.null(rv$p)) NA else rv$p$setup
    fill("setup", if (is.na(s)) "" else s,
         if (has_study()) list(study = study_key()),
         shiny::updateTextAreaInput)
  })
  shiny::observeEvent(input$setup, {
    owner <- edit_of("setup", input$setup)
    if (is.null(owner) || !identical(owner$study, study_key())) return()
    v <- if (nzchar(trimws(input$setup))) input$setup else NA_character_
    if (!identical(rv$p$setup, v)) rv$p$setup <- v
  }, ignoreInit = TRUE)

  program_text <- shiny::reactive({
    id <- current()
    if (is.null(id)) return("")
    paste(program_code(rv$p, id), collapse = "\n")
  })
  output$program <- shiny::renderText(program_text())
  output$program_table <- shiny::renderText(program_text())
  output$program_lf <- shiny::renderText(program_text())
  output$program_dc <- shiny::renderText(program_text())
  output$program_cc <- shiny::renderText(program_text())
  # Step 3: this report's font and size.  Blank: every report's (the study
  # tab, the page sheet's study row), shown greyed; a value is the report's
  # own row.  Only a field changed from what was drawn is written.
  rf_drawn <- new.env()
  # -- step 3: the page pattern and the form (R/page_patterns.R, R/page_form.R)
  # the report half's sheets, a value that changes only when they do (an
  # edit of a table's cell does not draw the form again)
  page_sheets <- shiny::reactiveVal(NULL)
  shiny::observe({
    # (and the spec's pages: an argument no sheet has a column for, #438)
    s <- if (!is.null(rv$p)) list(rv$p$sheets[.pattern_sheets], .pgs_pages(rv$p$spec))
    if (!identical(s, shiny::isolate(page_sheets()))) page_sheets(s)
  })
  page_report <- shiny::reactive({
    id <- target()
    if (is.null(rv$p) || is.na(id) || !id %in% rv$p$outputs$output_id) NULL else id
  })
  # the patterns' names: the bar is drawn again only when they change (or
  # the report does) -- a select drawn again while one edits would send its
  # first choice and change the report's pattern
  pat_names <- shiny::reactiveVal(character())
  shiny::observe({
    page_sheets()
    v <- if (!is.null(rv$p)) .pgs_patterns(.pgs_spec(shiny::isolate(rv$p))) else character()
    if (!identical(v, shiny::isolate(pat_names()))) pat_names(v)
  })
  # (the report, passed on only when it changes: page_report() is worked out
  # again on every edit of the planner)
  page_report_now <- shiny::reactiveVal(NULL)
  shiny::observe({
    id <- page_report()
    if (!identical(id, shiny::isolate(page_report_now()))) page_report_now(id)
  })
  output$page_pattern_bar <- shiny::renderUI({
    pat_names()
    id <- page_report_now()
    if (is.null(id)) {
      return(shiny::p(class = "small text-muted",
                      t("Standard (every report's page) and the page patterns are the sheets' blank and @<name> rows: Details, below.")))
    }
    sp <- .pgs_spec(shiny::isolate(rv$p))
    pats <- .pgs_patterns(sp)
    now <- .pgs_pattern_of(sp, id)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2 pg-bar",
      shiny::tags$label(`for` = "page_pattern", class = "small mb-0",
                        with_tip(t("Page pattern"), t("The set of page defaults this report starts from: Standard (every report's) or a pattern of the study (Compact for the PK tables ...), which changes only what differs from Standard. What this report changes goes over it."))),
      shiny::div(style = "width: 14rem", .for_report(shiny::selectInput(
        "page_pattern", NULL, width = "100%", selectize = FALSE,
        choices = c(stats::setNames("Standard", t("Standard (study default)")), stats::setNames(pats, pats)),
        selected = if (is.na(now)) "Standard" else now), id)),
      shiny::span(class = "small text-muted", "extends: ",
                  shiny::code(paste(sp$outputs[[id]]$extends %||% "-", collapse = ", "))),
      .btn("pat_edit", t("Edit the patterns..."), class = "btn-sm btn-outline-secondary"),
      if (!length(pats)) shiny::span(class = "small text-muted",
        t("(no other pattern yet: Edit the patterns... makes one)")))
  })
  # -- the pattern dialog: Standard and each pattern, on the same form
  pat_now <- shiny::reactiveVal("Standard")
  shiny::observeEvent(input$pat_edit, {
    id <- page_report()
    cur <- if (!is.null(id)) .pgs_pattern_of(.pgs_spec(rv$p), id) else NA
    pat_now(if (is.na(cur)) "Standard" else cur)
    shiny::showModal(shiny::modalDialog(
      title = t("Page patterns"), size = "xl", easyClose = TRUE,
      shiny::uiOutput("pat_head"),
      shiny::uiOutput("pat_form"),
      footer = shiny::modalButton(t("Close"))))
  })
  # (drawn again when the patterns or the one shown change, not on an edit:
  # its select would send its first choice)
  output$pat_head <- shiny::renderUI({
    pats <- pat_names()
    now <- pat_now()
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end mb-2",
      shiny::div(style = "width: 14rem", shiny::selectInput(
        "pat_pick", t("Pattern"), width = "100%", selected = now, selectize = FALSE,
        choices = c(stats::setNames("Standard", t("Standard")), stats::setNames(pats, pats)))),
      shiny::textInput("pat_name", t("Name"), placeholder = "Compact", width = "12rem"),
      .btn("pat_new", t("New pattern"), class = "btn-sm btn-outline-primary mb-3"),
      if (!identical(now, "Standard")) shiny::tagList(
        .btn("pat_rename", t("Rename"), class = "btn-sm btn-outline-secondary mb-3"),
        .btn("pat_remove", t("Remove"), class = "btn-sm btn-outline-danger mb-3")),
      shiny::uiOutput("pat_count", inline = TRUE),
      shiny::p(class = "small text-muted w-100 mb-0",
               if (identical(now, "Standard")) t("Standard: every report's page, unless its pattern or the report changes it.")
               else sprintf(t("%s: only what differs from Standard; each value says where it comes from."), now),
               " ", t("The paper is the page's first property: a pattern that changes it says so in its name (A4_Compact).")))
  })
  output$pat_count <- shiny::renderUI({
    page_sheets()
    sp <- .pgs_spec(shiny::isolate(rv$p))
    now <- pat_now()
    n <- if (identical(now, "Standard")) {
      length(setdiff(names(sp$outputs), unlist(lapply(.pgs_patterns(sp), .pgs_pattern_reports, sp = sp))))
    } else length(.pgs_pattern_reports(sp, now))
    shiny::span(class = "small text-muted mb-3", sprintf(t("Reports using it: %d"), n))
  })
  shiny::observeEvent(input$pat_pick, {
    if (input$pat_pick %in% c("Standard", pat_names())) pat_now(input$pat_pick)
  }, ignoreInit = TRUE)
  output$pat_form <- shiny::renderUI({
    page_sheets()
    now <- pat_now()
    id <- if (identical(now, "Standard")) NA_character_ else paste0("@", now)
    .page_form_ui(shiny::isolate(rv$p), id, t)
  })
  shiny::observeEvent(input$pat_new, {
    p2 <- guarded(.pgs_add_pattern(rv$p, input$pat_name))
    if (is.null(p2)) return()
    nm <- trimws(sub("^@", "", input$pat_name))
    page_write(p2)
    pat_now(nm)
    shiny::updateTextInput(session, "pat_name", value = "")
  })
  shiny::observeEvent(input$pat_rename, {
    now <- pat_now()
    p2 <- guarded(.pgs_rename_pattern(rv$p, now, input$pat_name))
    if (is.null(p2)) return()
    page_write(p2)
    pat_now(trimws(sub("^@", "", input$pat_name)))
    shiny::updateTextInput(session, "pat_name", value = "")
  })
  shiny::observeEvent(input$pat_remove, {
    p2 <- guarded(.pgs_remove_pattern(rv$p, pat_now()))
    if (is.null(p2)) return()
    page_write(p2)
    pat_now("Standard")
  })
  # only the user's change, of the report it was drawn for (#418)
  shiny::observeEvent(input$page_pattern_by, {
    id <- page_report()
    shiny::req(id)
    v <- .report_pick(input$page_pattern_by, id)
    if (!length(v)) return()
    v <- v[1L]
    now <- .pgs_pattern_of(.pgs_spec(rv$p), id)
    if (identical(v, if (is.na(now)) "Standard" else now)) return()
    p2 <- guarded(.pgs_set_pattern(rv$p, id, v))
    if (is.null(p2)) return()
    rv$p <- p2
    page_ver(page_ver() + 1L)
  }, ignoreInit = TRUE)
  output$page_form <- shiny::renderUI({
    page_sheets()
    id <- page_report()
    if (is.null(id)) return(NULL)
    .page_form_ui(shiny::isolate(rv$p), id, t)
  })
  page_write <- function(p2) {
    if (is.null(p2)) return()
    rv$p <- p2
    page_ver(page_ver() + 1L)
  }
  # the form's target: the report chosen, or -- in the pattern dialog -- a
  # pattern (@name) or Standard
  pg_id <- function(v) {
    if (is.null(v)) return(page_report())
    id <- .pg_target_id(v)
    if (!is.na(id) && !.is_pattern_id(id) && !identical(id, page_report())) return(NULL)
    id
  }
  shiny::observeEvent(input$pg_edit, {
    e <- input$pg_edit
    id <- pg_id(e$target)
    shiny::req(!is.null(id))
    p <- rv$p
    if (identical(e$kind, "arg")) {
      f <- .pgs_field(e$fn, e$arg)
      shiny::req(!is.null(f))
      v <- guarded(list(.pgs_value(e$value, f$kind)))
      if (is.null(v)) return()
      page_write(guarded(.pgs_set_arg(p, id, e$fn, e$arg, v[[1L]])))
    } else if (identical(e$kind, "line")) {
      d <- .pgs_lines(.pgs_spec(p), id, e$band)
      r <- d[d$line %in% e$line & d$from == "own", , drop = FALSE]
      parts <- list(left = NA_character_, center = NA_character_, right = NA_character_)
      if (nrow(r)) for (k in names(parts)) parts[[k]] <- r[[k]][1L]
      v <- e$value %||% ""
      parts[[e$part]] <- if (nzchar(trimws(v))) v else NA_character_
      page_write(guarded(.pgs_set_line(p, id, e$band, e$line, parts$left, parts$center,
                                       parts$right)))
    } else if (identical(e$kind, "token")) {
      if (.pg_token_taken(e$name)) {
        return(notify(.pg_token_taken_msg(e$name, t), "warning"))
      }
      page_write(guarded(.pgs_set_token(p, id, e$name, trimws(e$value %||% ""))))
    } else if (identical(e$kind, "info") && !is.na(id) && !.is_pattern_id(id)) {
      v <- trimws(e$value %||% "")
      page_write(guarded(.set_report_info_spec(p, id, e$name, if (nzchar(v)) v else NA_character_)))
    }
  })
  shiny::observeEvent(input$pg_act, {
    a <- input$pg_act
    # where a report list's token changes: the report list, step 1, the
    # report's section (Data code)
    if (identical(a$act, "goto")) {
      if (a$name %in% c("outputs", "ard", "code")) go(a$name)
      return()
    }
    id <- pg_id(a$target)
    shiny::req(!is.null(id))
    p <- rv$p
    if (a$act %in% c("token_own", "token_add") &&
        .pg_token_taken(toupper(trimws(a$name %||% "")))) {
      return(notify(.pg_token_taken_msg(toupper(trimws(a$name)), t), "warning"))
    }
    page_write(guarded(switch(a$act,
      reset = .pgs_set_arg(p, id, a$fn, a$arg, NULL),
      none = .pgs_set_arg(p, id, a$fn, a$arg, tflspec::tfl_unset()),
      # a figure's size: both fit to the page, or both its own (none)
      fig_fit = .pgs_set_arg(.pgs_set_arg(p, id, "rtf_figures", "width_twips", "fit"),
                             id, "rtf_figures", "height_twips", "fit"),
      fig_native = .pgs_set_arg(.pgs_set_arg(p, id, "rtf_figures", "width_twips", NULL),
                                id, "rtf_figures", "height_twips", NULL),
      # a line of the layers below made the target's own, to change
      own = {
        r <- .pgs_lines(.pgs_spec(p), id, a$band)
        r <- r[r$line %in% a$line, , drop = FALSE]
        .pgs_set_line(p, id, a$band, a$line, r$left, r$center, r$right)
      },
      omit = .pgs_set_line(p, id, a$band, a$line, omit = TRUE),
      drop = .pgs_set_line(p, id, a$band, a$line, drop = TRUE),
      addline = .pgs_set_line(p, id, a$band, .page_new_line(p, a$band, id, sp = .pgs_spec(p))),
      token_own = {
        r <- .pgs_tokens(.pgs_spec(p), id)
        .pgs_set_token(p, id, a$name, r$value[r$name %in% a$name][1L] %||% "")
      },
      token_reset = .pgs_set_token(p, id, a$name, NULL),
      token_omit = .pgs_set_token(p, id, a$name, tflspec::tfl_unset()),
      token_add = {
        nm <- toupper(trimws(a$name %||% ""))
        if (!grepl("^[A-Z][A-Z0-9_]*$", nm)) {
          stop("A token's name is upper case: a letter, then letters, digits or _ (DATA_CUTOFF).",
               call. = FALSE)
        }
        .pgs_set_token(p, id, nm, trimws(a$value %||% ""))
      },
      NULL)))
  })

  output$report_font <- shiny::renderUI({
    rv$ver
    id <- target()
    shiny::req(has_study(), length(id) == 1L, !is.na(id), nzchar(id))
    p <- shiny::isolate(rv$p)
    own_font <- page_value(p, "font", id)
    own_size <- .points(page_value(p, "font_size_half_points", id))
    all_font <- study_page_value(p, "font")
    all_size <- .points(study_page_value(p, "font_size_half_points"))
    if (is.na(all_font)) all_font <- "Courier New"
    if (!nzchar(all_size)) all_size <- "9"
    rf_drawn$id <- id
    rf_drawn$font <- if (is.na(own_font)) "" else own_font
    rf_drawn$size <- own_size
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end mb-2",
      shiny::textInput(updateOn = "blur", "rp_font", with_tip(t("This report's font"),
        t("Blank: every report's font and size (the study tab). A value here is this report's own (its row of the page sheet).")),
        rf_drawn$font, placeholder = all_font),
      shiny::textInput(updateOn = "blur", "rp_font_size", t("Size (pt)"), own_size,
                       placeholder = all_size, width = "8em"),
      shiny::span(class = "small text-muted mb-3",
                  sprintf(t("Every report's: %s, %s pt"), all_font, all_size)))
  })
  shiny::observeEvent(input$rp_font, {
    id <- target()
    v <- trimws(input$rp_font)
    if (!identical(rf_drawn$id, id) || identical(v, rf_drawn$font)) return()
    rf_drawn$font <- v
    rv$p <- set_page_value(rv$p, "font", v, id)
  })
  shiny::observeEvent(input$rp_font_size, {
    id <- target()
    v <- trimws(input$rp_font_size)
    if (!identical(rf_drawn$id, id) || identical(v, rf_drawn$size)) return()
    hp <- if (nzchar(v)) .half_points(v) else NA_character_
    if (nzchar(v) && is.na(hp)) {
      return(notify(t("The size is a number of points (9, 10 ...)."), "warning"))
    }
    rf_drawn$size <- v
    rv$p <- set_page_value(rv$p, "font_size_half_points", hp, id)
  })
  output$program_state <- shiny::renderUI({
    id <- current()
    if (is.null(id)) return(NULL)
    rv$status_ver
    f <- file.path(rv$study$path, study_layout()[["programs_tfl"]],
                   report_info(rv$p, id)$program)
    st <- .program_state(rv$p, id, f)
    msg <- t(switch(st,
      missing = "No file yet. Saving writes the program below.",
      current = "The saved program is the one below.",
      todo = "The saved program is the one below (its data part is a TODO).",
      generated = "The definition has changed: saving rewrites the program as below (a program nobody edited follows the definition).",
      edited = "The saved program was edited by hand. Saving writes it again from the definition (the edited one is kept in programs/.edited/)."))
    shiny::div(class = "mb-2 small",
               shiny::span(class = if (identical(st, "edited")) "text-warning", msg))
  })

  ask_id <- function(title, button, value = "", type = FALSE) {
    # (a new report: its type first, then its ID after the type's prefix,
    # T- / F- / L-: .id_prefix)
    id_ui <- if (!type) shiny::textInput("modal_id", "output_id", value = value) else shiny::div(
      class = "form-group shiny-input-container",
      shiny::tags$label(class = "control-label", `for` = "modal_id", "output_id"),
      shiny::div(class = "input-group",
                 shiny::span(class = "input-group-text", id = "modal_id_prefix", .id_prefix[["table"]]),
                 shiny::tags$input(id = "modal_id", type = "text", class = "shiny-input-text form-control", value = value)),
      shiny::tags$script(shiny::HTML(sprintf(
        "$(document).on('change', 'input[name=modal_type]', function() {
           var p = %s; $('#modal_id_prefix').text(p[this.value] || ''); });",
        jsonlite::toJSON(as.list(.id_prefix), auto_unbox = TRUE)))))
    shiny::showModal(shiny::modalDialog(
      title = title,
      if (type) shiny::radioButtons(
        "modal_type", t("Type"),
        stats::setNames(.tfl_order, t(unname(.type_labels[.tfl_order]))),
        inline = TRUE),
      if (type) lapply(report_types(), function(k) shiny::conditionalPanel(
        sprintf("input.modal_type == '%s'", k),
        shiny::p(class = "small text-muted mt-n2", t(.type_notes[[k]])))),
      id_ui,
      # how it is made: from one of tflspec's templates (its arguments
      # only), or the code SPEC as before (R/template_report.R)
      if (type) shiny::radioButtons(
        "modal_how", t("How to make it"),
        stats::setNames(c("template", "code"), c(t("Template"), t("Code"))),
        selected = "template", inline = TRUE),
      if (type) shiny::conditionalPanel("input.modal_how == 'template'", tpl_modal_ui()),
      if (type) shiny::uiOutput("modal_ok_js"),
      if (type) shiny::textInput("modal_desc", t("Description"),
                                 width = "100%"),
      # the section: the chosen report's, as a new one usually goes beside it
      if (type) shiny::textInput(
        "modal_section", t("Section (heading)"), width = "100%",
        value = {
          s <- rv$p$outputs$section[match(current(), rv$p$outputs$output_id)]
          if (length(s) && !is.na(s)) s else ""
        },
        placeholder = t("blank: from the ID")),
      if (type && nrow(rv$p$ard$populations)) shiny::conditionalPanel(
        "input.modal_how != 'template'", shiny::selectInput(
        "modal_pop", t("This report's population"), pop_choices(), width = "100%",
        selected = {
          v <- report_population(rv$p, current() %||% "")
          if (is.na(v)) "" else v
        })),
      if (type) shiny::conditionalPanel(
        "input.modal_type == 'listing' && input.modal_how != 'template'",
        shiny::checkboxInput(
          "modal_first_l",
          with_tip(t("Start from the data"),
                   t("Start the listing from the data (writes its columns and shows it)")),
          value = TRUE, width = "100%"),
        shiny::conditionalPanel("input.modal_first_l",
                                shiny::uiOutput("modal_first_l_ui"))),
      if (type) shiny::conditionalPanel(
        "input.modal_type == 'table' && input.modal_how != 'template'",
        shiny::checkboxInput(
          "modal_first",
          with_tip(t("Apply a template"),
                   t("A kind of table: a few questions about the data, then its ARD definition and its table are written -- all of it editable afterwards.")),
          value = !nrow(rv$p$ard$analyses), width = "100%"),
        shiny::conditionalPanel("input.modal_first",
                                shiny::uiOutput("modal_first_ui"))),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(button, "OK", class = "btn-primary")),
      easyClose = TRUE))
  }
  after_id_change <- function(p, id) {
    if (is.null(p)) return()
    rv$p <- p
    rv$want <- id
    tgt_sent$v <- id
    tgt_sent$t <- Sys.time()
    shiny::removeModal()
    bump()
  }
  need_current <- function() {
    if (is.null(current())) notify(t("Choose a report"), "warning")
    !is.null(current())
  }
  shiny::observeEvent(input$add, ask_id(t("Add a report"), "add_ok",
                                        type = TRUE))
  .srv_keep(environment())
}
