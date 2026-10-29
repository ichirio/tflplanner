# app_server(): ard figure -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_ard_figure <- function() {
  # -- a figure's ARD (#293): none, its own analyses, or a table's ----------
  fig_ard_ui <- function(id) {
    src <- .fig_ard_source(rv$p, id)
    o <- rv$p$outputs
    tabs <- o$output_id[vapply(o$output_id, function(x) identical(report_info(rv$p, x)$type, "table"), NA)]
    tab_lab <- stats::setNames(tabs, ifelse(is.na(o$description[match(tabs, o$output_id)]), tabs,
                                            paste(tabs, o$description[match(tabs, o$output_id)], sep = "  ")))
    gone <- src$kind == "table" && !.is_table(rv$p, src$id)
    if (gone) tab_lab <- c(stats::setNames(src$id, sprintf(t("%s (not in the study)"), src$id)), tab_lab)
    if (src$kind == "import") {
      return(shiny::div(class = "alert alert-warning py-1 small",
                        sprintf(t("%s uses an ARD taken in (%s): the analyses here are not used for it."),
                                id, src$file)))
    }
    shiny::div(
      class = "card card-body py-2 mb-2",
      shiny::h6(class = "mb-1", with_tip(t("Numbers from an ARD (optional)"),
        t("Only when the figure prints numbers of an analysis -- a KM median, a forest plot's hazard ratios: its own analyses, or a table's ARD. Most figures need none."))),
      shiny::div(class = "d-flex flex-wrap gap-3 align-items-end",
        shiny::radioButtons(
          "fig_ard_kind", with_tip(t("This figure's ARD"),
            t("Where the numbers it prints come from (step 3: \"A number from the ARD\", \"Statistics from the ARD\"). The program puts it in `ard`.")),
          inline = TRUE, selected = src$kind,
          choices = stats::setNames(c("none", "own", "table"),
                                    c(t("None: the figure reads its data"),
                                      t("Its own analyses (below, as a table's)"),
                                      t("A table's ARD")))),
        if (src$kind == "table") shiny::selectInput(
          "fig_ard_table", t("The table"), choices = tab_lab,
          selected = src$id, width = "26rem")),
      if (gone) shiny::div(class = "alert alert-danger py-1 small mt-2",
        sprintf(t("%s is not a table of the study (deleted, or renamed by hand): choose another, or the figure's program stops."), src$id))
      else if (src$kind == "table") fig_ard_table_view(id, src$id))
  }
  # a table's ARD as the figure reads it: its analyses, the statistics its
  # ARD has (made), and which the figure's design uses
  fig_ard_table_view <- function(id, tb) {
    rv$ard_ver
    an <- ard_rows(rv$p, "analyses", tb)
    rows <- tryCatch(study_ard_rows(rv$study, tb), error = function(e) NULL)
    d <- fig_design(rv$p, id)
    used <- unique(c(unlist(lapply(d$data, function(s) if (identical(s$step, "ard_stats")) .ard_ids(s$analysis_id))),
                     unlist(lapply(d$layers, function(l) if (identical(l$layer, "ard_number")) l$analysis_id))))
    tbl <- if (nrow(an)) data.frame(
      analysis = an$analysis_id,
      label = ifelse(is.na(an$label), "", an$label),
      statistics = vapply(an$analysis_id, function(a) {
        if (is.null(rows) || !nrow(rows)) return(t("(not made yet)"))
        # by variable (not cards' own ..name.. rows)
        k <- rows$analysis_id == a & !grepl("^[.][.]", rows$variable)
        v <- unique(rows$variable[k])
        paste(vapply(v, function(x) paste0(x, ": ", paste(unique(rows$stat_name[k & rows$variable == x]),
                                                            collapse = ", ")), ""),
              collapse = "; ")
      }, ""),
      used = ifelse(an$analysis_id %in% used, t("used by this figure"), ""),
      stringsAsFactors = FALSE) else NULL
    shiny::div(
      class = "small mt-1",
      if (is.null(tbl)) shiny::p(class = "text-muted mb-1", sprintf(t("%s has no analyses yet."), tb))
      else shiny::tags$table(
        class = "table table-sm mb-1",
        shiny::tags$thead(shiny::tags$tr(lapply(c(t("Analysis"), t("Label"), t("In its ARD (variable: statistic)"), ""),
                                               shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(tbl)), function(i) shiny::tags$tr(
          shiny::tags$td(tbl$analysis[i]), shiny::tags$td(tbl$label[i]),
          shiny::tags$td(class = "text-break", tbl$statistics[i]),
          shiny::tags$td(class = "text-success text-nowrap", tbl$used[i]))))),
      shiny::div(class = "d-flex gap-2 align-items-center",
        shiny::span(class = "text-muted", sprintf(t("The statistics are %s's: add one to its analysis (its step 2) to print it here."), tb)),
        .btn("fig_ard_open", sprintf(t("Open %s"), tb), class = "btn-sm btn-outline-secondary py-0")))
  }
  shiny::observeEvent(input$fig_ard_kind, {
    id <- current()
    shiny::req(id, identical(report_kind(), "figure"))
    src <- .fig_ard_source(rv$p, id)
    k <- input$fig_ard_kind
    if (identical(k, src$kind)) return()
    if (identical(k, "table")) {
      o <- rv$p$outputs$output_id
      tabs <- o[vapply(o, function(x) identical(report_info(rv$p, x)$type, "table"), NA)]
      if (!length(tabs)) {
        shiny::showNotification(t("The study has no table yet."), type = "warning")
        return()
      }
      # a table whose ARD has a KM fit first, for a KM figure; else the first
      an <- rv$p$ard$analyses
      km <- an$output_id[!is.na(an$code) & grepl("ard_survival_survfit", an$code, fixed = TRUE)]
      tb <- c(intersect(km, tabs), tabs)[1L]
      rv$p <- set_fig_ard_source(rv$p, id, paste0("table:", tb))
    } else {
      rv$p <- set_fig_ard_source(rv$p, id, if (identical(k, "none")) NULL else k)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$fig_ard_table, {
    id <- current()
    shiny::req(id, identical(report_kind(), "figure"))
    src <- .fig_ard_source(rv$p, id)
    if (identical(src$kind, "table") && !identical(src$id, input$fig_ard_table)) {
      rv$p <- set_fig_ard_source(rv$p, id, paste0("table:", input$fig_ard_table))
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$fig_ard_open, {
    src <- .fig_ard_source(rv$p, current())
    if (identical(src$kind, "table")) shiny::updateSelectInput(session, "target", selected = src$id)
  })

  output$report_head <- shiny::renderUI({
    id <- current()
    if (is.null(id)) return(NULL)
    o <- rv$p$outputs
    d <- o$description[match(id, o$output_id)]
    # its review's counts, each a way to the Review tab (that report, that
    # level)
    cn <- review_counts()
    k <- if (!is.null(cn)) match(id, cn$output_id) else NA
    counts <- if (!is.na(k)) {
      parts <- lapply(names(.review_level_words), function(l) {
        n <- cn[[l]][k]
        if (!n) return(NULL)
        shiny::tags$a(
          href = "#", class = paste("small", .review_level_class[[l]]),
          onclick = sprintf("Shiny.setInputValue('review_goto', {id: %s, level: '%s', n: Math.random()}, {priority: 'event'}); return false;",
                            jsonlite::toJSON(id, auto_unbox = TRUE), l),
          sprintf("%d %s", n, t(.review_level_words[[l]])))
      })
      parts <- Filter(Negate(is.null), parts)
      if (length(parts)) shiny::span(
        class = "ms-2", title = t("The review of this report: a click shows its items"),
        lapply(seq_along(parts), function(i)
          if (i > 1L) shiny::tagList(shiny::span(class = "text-muted small", " \u00b7 "),
                                     parts[[i]]) else parts[[i]]))
    }
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-baseline my-2", `data-rp-head` = id,
      shiny::strong(id),
      shiny::span(class = "badge text-bg-light border",
                  t(unname(.type_labels[report_kind()]))),
      # a template report's template, on every step: a click goes to its
      # card (#440)
      tpl_head_link(id),
      if (!is.na(d)) shiny::span(class = "text-muted small", d),
      counts)
  })

  # -- what the ARD says ---------------------------------------------------
  meta_of <- function(id) {
    rv$ard_ver
    # the study it is (a save gives rv$study anew: the same study)
    study_open()
    if (!has_study() || is.null(id)) return(NULL)
    # (a part's: its report's ARD)
    id <- .part_parent(id)
    .meta_with_hierarchy(ard_info(shiny::isolate(rv$study), id), shiny::isolate(rv$p), id)
  }
  meta_now <- shiny::reactive({
    rv$ard_ver
    if (!has_study()) return(NULL)
    id <- current()
    if (!is.null(id)) return(meta_of(id))
    .merge_meta(lapply(rv$p$outputs$output_id, meta_of))
  })

  # -- sheet grids -------------------------------------------------------
  # (the report sheets' grids again when step 3's form writes them: a grid
  # drawn before cannot write over it)
  page_ver <- shiny::reactiveVal(0L)
  for (sheet in setdiff(c(table_sheets(), report_sheets()), "codelists")) local({
    sh <- sheet
    out_id <- paste0("hot_", sh)
    key <- shiny::reactive(paste(sh, input$target, rv$ver, rv$ard_ver,
                                 if (sh %in% report_sheets()) page_ver(),
                                 sep = "|"))
    output[[out_id]] <- rhandsontable::renderRHandsontable({
      shiny::req(has_study())
      tg <- target()
      d <- sheet_rows(shiny::isolate(rv$p), sh, tg)
      if (!identical(tg, "")) d$output_id <- NULL
      ch <- .std_choices(sh)
      gc <- grid_choices(sh, meta_now())
      for (cn in names(gc)) ch[[cn]] <- unique(c(gc[[cn]], ch[[cn]]))
      grids_drawn()
      .grid(d, sh, key(), ch)
    })
    shiny::observeEvent(input[[out_id]], {
      h <- input[[out_id]]
      if (is.null(h$changes$changes) &&
          !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
      if (!identical(h$params$planner_key, key())) return()
      d <- read_grid(h)
      if (is.null(d)) return()
      guarded(rv$p <- set_sheet_rows(rv$p, sh, target(), d))
      # the table's builder shows the sheets: drawn again from them
      if (sh %in% table_sheets() && identical(active_page(), "builder")) {
        rv$bver <- rv$bver + 1L
      }
    })
    inherited <- shiny::reactive({
      id <- current()
      if (is.null(id)) return(NULL)
      d <- inherited_rows(rv$p, sh, id)
      if (!nrow(d)) return(NULL)
      d$output_id <- NULL
      d[, colSums(!is.na(d)) > 0, drop = FALSE]
    })
    output[[paste0("inh_", sh)]] <- shiny::renderUI({
      d <- inherited()
      # the header: the study's (one for every report, its words
      # the report's tokens), or the report's own -- a copy of the study's
      band <- if (identical(sh, "header") && !is.null(current())) {
        rv$ver
        own <- nrow(sheet_rows(rv$p, sh, current())) > 0L
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-center mt-2 small",
          if (own) shiny::tagList(
            shiny::span(t("This report has a header of its own.")),
            .btn(paste0("band_study_", sh), t("Back to the study's"),
                 class = "btn-sm btn-outline-secondary py-0"))
          else shiny::tagList(
            shiny::span(with_tip(t("The study's (its values: this report's tokens)"),
                                 t("Every report has the study's header; what changes from report to report ({OUTPUT_LABEL}, {OUTPUT_TITLE}, {OUTPUT_POPULATION}) is in the tokens tab."))),
            if (!is.null(d)) .btn(paste0("band_own_", sh), t("A header of this report's own"),
                                  class = "btn-sm btn-outline-secondary py-0")))
      }
      if (is.null(d)) return(band)
      shiny::tagList(
        band,
        shiny::tags$details(
          class = "mt-2 small", open = NA,
          shiny::tags$summary(sprintf(
            t("Inherited from the study defaults (%d rows; edit them under Study defaults)"),
            nrow(d))),
          shiny::div(class = "rp-inherited",
                     shiny::tableOutput(paste0("inh_tbl_", sh)))))
    })
    if (identical(sh, "header")) {
      # a copy of the study's lines, to edit for this report
      shiny::observeEvent(input[[paste0("band_own_", sh)]], {
        d <- inherited_rows(rv$p, sh, current())
        if (!nrow(d)) return()
        d$output_id <- NULL
        guarded(rv$p <- set_sheet_rows(rv$p, sh, current(), d))
        bump()
      })
      shiny::observeEvent(input[[paste0("band_study_", sh)]], {
        guarded(rv$p <- set_sheet_rows(rv$p, sh, current(),
                                       sheet_rows(rv$p, sh, current())[0L, , drop = FALSE]))
        bump()
      })
    }
    output[[paste0("inh_tbl_", sh)]] <- shiny::renderTable(
      inherited(), na = "", striped = TRUE, spacing = "xs")
    output[[paste0("help_", sh)]] <- DT::renderDT(
      .help_table(sh), rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  for (sh in c("datasets", "populations")) local({
    sheet <- sh
    output[[paste0("help_ard_", sheet)]] <- DT::renderDT(
      .help_table(sheet), rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  # a report's code lists, in a dialog, where its data is made (1-1's
  # column definitions, a listing's data, a figure's data): the variables
  # it is about (R/codelists.R)
  cl_dialog <- function(prefix, done = function() NULL) {
    .codelist_dialog_server(input, output, session, prefix, rv, report = current,
                            t = t, notify = notify, guarded = guarded, bump = bump,
                            read_grid = read_grid, grids_drawn = grids_drawn,
                            has_study = has_study, done = done)
  }
  cl_21 <- cl_dialog("cl21")
  cl_lf <- cl_dialog("cllf")
  cl_fig <- cl_dialog("clfig")
  # 1-1's column definitions, part 4: the code lists of the analysis data's
  # columns -- what its program makes of their values -- the values its
  # data has that a list does not (the program would stop on them), and
  # the dialogs (R/codelists.R)
  adata_cl_vars <- function() {
    made <- vapply(.drv_read_all(adata_derive_now()), function(x) x$name, "")
    unique(c(adata_cols(adata_root(input$adata_from %||% "")), made,
             input$adata_add, input$adata_keep))
  }
  adata_cl_ids <- .codelist_part_ids("adata_cl", "cl21")
  adata_cl_part <- .codelist_part_server(
    input, output, rv, current, adata_cl_ids, vars = adata_cl_vars,
    # the rows its condition keeps (the analysis set's too)
    data = function() {
      ds <- adata_root(input$adata_from %||% "")
      if (!.is_blank(ds)) .rows_where(an_data(ds), adata_where_now())
    },
    title = function() {
      nm <- trimws(input$adata_id %||% "")
      if (nzchar(nm)) sprintf(t("Code lists: the columns of %s"), nm) else
        t("Code lists: the columns of this analysis data")
    },
    dialog = cl_21, t = t, notify = notify, guarded = guarded, bump = bump,
    has_study = has_study, read_grid = read_grid, grids_drawn = grids_drawn)
  output$adata_cl <- shiny::renderUI({
    shiny::req(has_study(), current(), !is.null(adata_edit()))
    cl <- sheet_rows(rv$p, "codelists", current())
    .codelist_part_ui(
      .codelist_lines(cl, adata_cl_vars()),
      adata_cl_part$missing(), t, adata_cl_ids,
      t("This report's code lists of these columns: the program puts them on the data (set_levels()), each column a factor in the list's order, its values the labels -- so the ARD holds what the table prints. A value a list does not have stops the program."),
      open = adata_cl_part$open(), others = .codelist_others(cl, adata_cl_vars()))
  })
  # step 2's table: its code lists are where its data is made (1-1)
  shiny::observeEvent(input$cl3_open, go("ard"))
  output$type_note <- shiny::renderUI({
    id <- current()
    if (is.null(id)) return(NULL)
    type <- report_info(rv$p, id)$type
    if (identical(type, "table")) return(NULL)
    shiny::div(class = "alert alert-info py-2 small",
               sprintf(t("%s is a %s: the table definition is for Tables (a %s is made in its data code)."),
                       id, .type_labels[[type]], .type_labels[[type]]))
  })

  # -- input assistance ------------------------------------------------------
  # above the sheets: what the ARD says, and reading it again
  output$assist <- shiny::renderUI({
    id <- current()
    if (is.null(id)) {
      return(shiny::span(class = "small text-muted",
                         t("Choose a report on the left to fill its rows from its ARD and from presets.")))
    }
    st <- report_ard_state(id)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::strong("ARD"),
      shiny::span(class = paste("small", if (st$need) "text-muted"), st$text),
      preview_button("fetch2", current(), "btn-sm btn-outline-primary"),
      help_tip(t(.preview_tip)))
  })
  # the preview's ARD of a report: missing, outdated, current
  # (ard_preview_state(): what it was made from against now)
  preview_state <- function(id) {
    rv$ard_ver
    rv$p
    if (is.null(id) || !has_study()) return(NA_character_)
    tryCatch(ard_preview_state(shiny::isolate(current_study()), id),
             error = function(e) NA_character_)
  }
  # Update preview, and when it would make the ARD, why: none yet, or
  # outdated (its mark beside it); current: the button only redraws
  preview_button <- function(input_id, id, class = "btn-sm btn-primary") {
    st <- preview_state(id)
    mark <- switch(st %||% "",
      missing = shiny::span(class = "badge text-bg-secondary rp-pv-mark", t("No ARD yet")),
      outdated = shiny::span(class = "badge text-bg-warning rp-pv-mark", t("ARD outdated")),
      NULL)
    shiny::span(class = "d-inline-flex align-items-center gap-1",
                .btn(input_id, t("Update preview"), class = class), mark)
  }
  # A report's ARD in one word -- not made, outdated (its definition has
  # changed), made (when), error -- and whether a Preview is wanted.
  report_ard_state <- function(id) {
    m <- meta_of(id)
    s <- tryCatch(ard_state(), error = function(e) NULL)
    state <- if (!is.null(s) && id %in% s$output_id)
      s$state[match(id, s$output_id)] else NA_character_
    if (is.null(m) && (is.na(state) || state != "error")) state <- "not built"
    if (is.na(state)) state <- "built"
    text <- switch(state,
      `not built` = t("Not made"),
      outdated = t("Outdated: the ARD definition has changed since it was made"),
      error = t("Error: its program failed (Data > Study ARD)"),
      sprintf(t("Made %s: %d keys, %d variables, statistics %s"),
              m$fetched, length(m$keys), nrow(m$variables),
              paste(m$stats, collapse = ", ")))
    list(state = state, need = !identical(state, "built"),
         text = text)
  }

  # each assisted tab's own help, above its grid
  assist_box <- function(...) {
    if (is.null(current())) return(NULL)
    shiny::div(class = "rp-assist border rounded p-2 mb-2", ...)
  }
  output$assist_tables <- shiny::renderUI(assist_box(
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      .btn("fill_tables", t("Fill table roles")),
      shiny::span(class = "small text-muted",
                  t("From the ARD: the column key, and the hierarchy (rows, label, sort) or group = variable. Blank cells only.")))))
  output$assist_variables <- shiny::renderUI(assist_box(
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      .btn("fill_vars", t("Fill variables and levels")),
      shiny::span(class = "small text-muted",
                  t("From the ARD and the source data: a row per column key and analysis variable, with levels in order, label and order. Blank cells only.")))))
  output$assist_cells <- shiny::renderUI({
    m <- meta_of(current())
    vars <- if (!is.null(m)) m$variables$variable else character()
    assist_box(
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-end",
        shiny::selectInput("cell_preset", t("Cell preset"),
                           names(cell_presets()), width = "360px"),
        shiny::selectInput(
          "cell_var", t("for"),
          c(stats::setNames("", t("every variable of its kind")),
            stats::setNames(vars, vars)),
          width = "240px"),
        .btn("add_cells", t("Add cells"), class = "btn-sm mb-3")),
      shiny::div(class = "small", shiny::tableOutput("preview_cells")))
  })
  output$preview_cells <- shiny::renderTable({
    shiny::req(input$cell_preset %in% names(cell_presets()))
    d <- cell_presets()[[input$cell_preset]]
    if (!is.null(input$cell_var) && nzchar(input$cell_var)) {
      d$variable <- input$cell_var
    }
    m <- meta_of(current())
    if (!is.null(m)) {
      d$ARD <- vapply(d$template, function(z) {
        miss <- .missing_stats(z, m$stats)
        if (length(miss)) paste(t("missing:"), paste(miss, collapse = ", "))
        else "\u2713"
      }, "")
    }
    d
  }, na = "", spacing = "xs")
  output$assist_col_header <- shiny::renderUI(assist_box(
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end",
      {
        # the presets that fit this report (all, for the study defaults)
        id <- current()
        m <- if (!is.null(id)) meta_of(id)
        hp <- if (is.null(m)) header_presets() else NULL
        ch <- if (is.null(m)) names(hp) else .header_preset_choices(
          length(m$hierarchy) > 0L, .key_label(m, m$by[1L]))
        shiny::selectInput("header_preset", t("Column header preset"), ch,
                           width = "300px")
      },
      .btn("add_header", t("Set column header"), class = "btn-sm mb-3"),
      shiny::span(class = "small text-muted mb-3",
                  t("Replaces this report's column header rows."))),
    shiny::div(class = "small", shiny::tableOutput("preview_header"))))
  output$preview_header <- shiny::renderTable({
    shiny::req(input$header_preset %in% names(header_presets()))
    d <- header_presets()[[input$header_preset]]
    d$text <- gsub("\n", " / ", d$text, fixed = TRUE)
    d
  }, na = "", spacing = "xs")
  after_fill <- function(p, what, sheet) {
    if (is.null(p)) return()
    n <- attr(p, "changed")
    attr(p, "changed") <- NULL
    rv$p <- p
    bump()
    notify(if (is.null(n)) sprintf(t("%s: done"), what) else
      sprintf(t("%s: %d rows added or filled"), what, n))
    if (identical(sheet, "codelists")) go("ard") else
      bslib::nav_select("table_sheet", sheet)
  }
  need_meta <- function() {
    m <- meta_of(current())
    if (is.null(m)) {
      notify(t("Update preview first: it makes this table's ARD."), "warning")
    }
    m
  }
  shiny::observeEvent(input$fill_vars, {
    m <- need_meta()
    if (is.null(m)) return()
    after_fill(guarded(fill_variables(rv$p, current(), m)), "variables",
               "variables")
  })
  shiny::observeEvent(input$fill_tables, {
    m <- need_meta()
    if (is.null(m)) return()
    after_fill(guarded(fill_tables(rv$p, current(), m)), "tables", "tables")
  })
  shiny::observeEvent(input$add_cells, {
    if (is.null(current())) return()
    m <- meta_of(current())
    tpl <- cell_presets()[[input$cell_preset]]$template
    miss <- if (!is.null(m)) .missing_stats(tpl, m$stats) else character()
    after_fill(guarded(add_preset(rv$p, current(), input$cell_preset,
                                  variable = input$cell_var)),
               "cells", "cells")
    if (length(miss)) {
      notify(sprintf(t("The ARD has no %s: add it to the ARD code, or edit the template."),
                     paste(miss, collapse = ", ")), "warning")
    }
  })
  shiny::observeEvent(input$add_header, {
    if (is.null(current())) return()
    after_fill(guarded(add_preset(rv$p, current(), input$header_preset)),
               "col_header", "col_header")
  })
  # Update preview (#428): the one action that makes a report's ARD for its
  # preview, the same from the ARD, Tables and Reports tabs -- only when it
  # is needed: none kept yet, or what it is made from has changed since
  # (ard_preview_state()).  It is made in an R of its own, in a folder of
  # its own (fetch_ard()): the study ARD (output/ard/ard.rds) is not
  # written, and the study is not saved for it.  Then the builder's choices
  # and the preview are read again, and the report's ARD is shown on the
  # ARD tab.  Making the study ARD is do_study_ard() (step 1's Make the
  # ARD) and the official run.
  do_preview <- function(id = current(), force = FALSE) {
    if (is.null(id)) return(notify(t("Choose a report"), "warning"))
    has_an <- any(rv$p$ard$analyses$output_id %in% id)
    is_report <- id %in% rv$p$outputs$output_id
    if (!has_an && !is_report) {
      return(notify(sprintf(t("%s has no analyses in the ARD definition."), id),
                    "warning"))
    }
    if (has_an && !is.null(ard_valid())) {
      notify(t("Correct the ARD definition first."), "warning")
      return(invisible(FALSE))
    }
    s <- current_study()
    t0 <- Sys.time()
    # (analyses not in the report list: their ARD made to be seen, alone)
    if (!is_report) {
      r <- NULL
      shiny::withProgress(message = sprintf(t("Preview of %s"), id),
                          r <- guarded(run_ard(s, id)))
      if (is.null(r)) return(invisible(FALSE))
      if (is.null(r$ard)) {
        ard_res(list(scope = id, error = r$error))
        return(invisible(FALSE))
      }
      ard_res(list(ard = r$ard, scope = id, seconds = r$seconds))
      notify(sprintf(t("%s is not in the Report list: its ARD is shown, no table is made from it."), id),
             "warning")
      return(invisible(TRUE))
    }
    state <- ard_preview_state(s, id)
    m <- NULL
    if (identical(state, "current") && !isTRUE(force)) {
      m <- ard_info(s, id)
    } else {
      n_an <- sum(rv$p$ard$analyses$output_id %in% id)
      err <- NULL
      shiny::withProgress(
        message = sprintf(t("Preview of %s"), id),
        detail = if (has_an) sprintf(t("making its ARD (%d analyses, in its own R process)"), n_an) else
          t("running its data code (in its own R process)"),
        m <- tryCatch(fetch_ard(s, id), error = function(e) {
          err <<- e
          NULL
        }))
      if (is.null(m)) {
        msg <- conditionMessage(err)
        ard_res(list(scope = id, error = paste(c(msg, err$detail), collapse = "\n\n")))
        ard_preview_out(list(id = id, text = strsplit(err$detail %||% msg, "\r?\n")[[1L]]))
        notify(paste(t("The preview's ARD could not be made:"), msg), "error")
        rv$ard_ver <- rv$ard_ver + 1L
        return(invisible(FALSE))
      }
      ard_preview_out(list(id = id, text = strsplit(m$log %||% "", "\r?\n")[[1L]]))
    }
    rv$ard_ver <- rv$ard_ver + 1L
    a <- ard_raw(s, id, rows = TRUE) %||% ard_raw(s, id)
    if (!is.null(a)) {
      ard_res(list(ard = a, scope = id,
                   seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
    if (!is.null(m$error)) {
      notify(paste(t("The ARD was read, but normalize and rework failed:"),
                   m$error), "warning")
    } else if (identical(state, "current") && !isTRUE(force)) {
      notify(sprintf(t("Preview of %s: its ARD is up to date (not made again)."), id))
    } else {
      notify(sprintf(t("Preview of %s: %d keys, %d variables"), id,
                     length(m$keys), nrow(m$variables)))
    }
    invisible(TRUE)
  }
  .srv_keep(environment())
}
