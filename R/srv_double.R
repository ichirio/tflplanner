# app_server(): double -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_double <- function() {
  # double programming: the report's own ARD against the one taken in
  imp_compare <- function(id, file) {
    cmp <- guarded(compare_imported_ard(imp_study(), id, file))
    if (is.null(cmp)) return()
    same <- isTRUE(cards::is_ard_equal(cmp))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("%s: its own ARD and %s"), id, file), size = "l", easyClose = TRUE,
      shiny::div(class = if (same) "alert alert-success py-1" else "alert alert-warning py-1",
                 if (same) t("They agree.") else t("They differ:")),
      if (!same) shiny::div(class = "rp-code",
                            shiny::pre(paste(utils::capture.output(print(cmp)), collapse = "\n"))),
      footer = shiny::modalButton(t("Close"))))
  }
  shiny::observeEvent(input$imp_compare, {
    r <- imp_row()
    shiny::req(r)
    o <- trimws(strsplit(r$outputs %||% "", "|", fixed = TRUE)[[1L]])
    id <- if (!is.null(current()) && current() %in% c(o, .imports_used_by(rv$p$sheets$report, r$file)))
      current() else o[nzchar(o)][1L]
    if (is.na(id %||% NA)) return(notify(t("The ARD names no report: choose one in Make a report."), "warning"))
    imp_compare(id, r$file)
  })
  shiny::observeEvent(input$imp_compare_cur, {
    f <- .ard_import_of(rv$p, current())
    shiny::req(f)
    imp_compare(current(), f)
  })
  # a double click on a report's row: its ARD definition, on the left
  shiny::observeEvent(input$ard_state_dbl, {
    id <- input$ard_state_dbl
    if (!id %in% output_ids(rv$p)) return()
    shiny::updateCheckboxInput(session, "ard_all", value = FALSE)
    shiny::updateSelectInput(session, "target", selected = id)
    bslib::nav_select("ard_sheet", "analyses")
  })
  output$ard_state_data <- DT::renderDT(ard_state_view())
  # below the table: what the last preview printed, or -- a row chosen --
  # that output's log in the latest official run of the ARD
  output$ard_log <- shiny::renderText({
    shiny::req(has_study())
    ard_log_ver()
    rv$status_ver
    d <- ard_state()
    i <- input$ard_state_rows_selected
    if (!length(i)) {
      pv <- ard_preview_out()
      if (is.null(pv)) return(t("Choose a row to see its log in the latest official run."))
      return(paste(c(sprintf(t("# Preview of %s (not kept)"), pv$id), "",
                     pv$text), collapse = "\n"))
    }
    id <- d$output_id[i]
    b <- list_batches(current_study())
    b <- b[b$what %in% c("ard", "all"), , drop = FALSE]
    log <- if (nrow(b)) file.path(b$path, "logs", "ard",
                                  sub("[.][Rr]$", ".log", .ard_prog_name(id)))
    log <- log[file.exists(log)]
    if (!length(log)) {
      return(sprintf(t("%s has no official run yet."), id))
    }
    paste(c(paste("#", normalizePath(log[1L], "/", FALSE)), "",
            readLines(log[1L], warn = FALSE, encoding = "UTF-8")),
          collapse = "\n")
  })
  # the catalog's datasets whose file is not there (the grid is the catalog)
  output$catalog_missing <- shiny::renderUI({
    shiny::req(has_study())
    d <- rv$p$ard$datasets
    gone <- d$dataset[!is.na(d$path) & nzchar(d$path) &
                        !file.exists(.data_file(rv$study$path, d$path))]
    if (!length(gone)) return(NULL)
    shiny::div(class = "alert alert-warning py-1 small",
               sprintf(t("No file for: %s"), paste(gone, collapse = ", ")))
  })


  # -- listings and figures ------------------------------------------------
  # The form is drawn per report with fresh input ids (as the builder's),
  # so an input of another report's form is never read as this one's.
  lf_env <- new.env()
  session$userData$lf_env <- lf_env
  lf_env$n <- 0L
  lf_drawn <- shiny::reactiveVal(0L)
  lf_id <- function(x) paste0("lf", lf_env$n, "_", x)
  lf_type <- shiny::reactive({
    id <- current()
    if (is.null(id)) "none" else report_info(rv$p, id)$type
  })
  catalog <- shiny::reactive({
    d <- rv$p$ard$datasets
    d[!is.na(d$dataset), , drop = FALSE]
  })
  dataset_columns <- function(ds) {
    d <- catalog()
    pth <- d$path[match(ds, d$dataset)]
    if (length(pth) != 1L || is.na(pth)) return(character())
    f <- .data_file(rv$study$path, pth)
    if (!file.exists(f)) return(character())
    k <- paste(f, file.mtime(f))
    if (is.null(ard_cols[[k]])) {
      ard_cols[[k]] <- tryCatch(names(read_data_head(f, 1L)),
                                error = function(e) character())
    }
    ard_cols[[k]]
  }
  .designer_server(input, output, session, rv, current, t, notify, guarded,
                   catalog,
                   page = function() page(), codelists_dialog = cl_fig,
                   bump = bump, has_study = has_study, read_grid = read_grid,
                   grids_drawn = grids_drawn)
  output$lf_note <- shiny::renderUI({
    msg <- switch(lf_type(),
      none = t("Choose a Listing report on the left."),
      table = t("This is a Table: it is made in step 2, the table builder's cards."),
      listing = t("A listing: its data (a dataset of the catalog, a condition, an order) and its columns. Its data code (step 2, Data code), if any, changes the data after the condition."),
      figure = t("This is a Figure: it is made in Make the figure."))
    # a listing's: the (i) of its form's heading
    if (identical(lf_type(), "listing")) return(NULL)
    shiny::div(class = "alert alert-info py-2 small", msg)
  })
  # ---- a step of the report's own code (R/step_code.R) -----------------------
  # Each of the data step and the content step is the definition (GUI) or
  # the report's own R: a step whose code is not blank is that code.  The
  # switch at the top of each step shows the code's editor; [GUI] again
  # with code there asks before the code goes.
  code_wanted <- shiny::reactiveValues()
  step_is_code <- function(step) {
    id <- current()
    if (is.null(id) || !has_study()) return(FALSE)
    .is_code_step(rv$p, id, step) || isTRUE(code_wanted[[paste(id, step)]])
  }
  yes <- function(x) if (isTRUE(x)) "yes" else "no"
  output$data_is_code <- shiny::renderText(yes(step_is_code("data")))
  output$content_is_code <- shiny::renderText(yes(step_is_code("content")))
  # what a step of its own code reads (its datasets, its ARD): above the
  # data step's code; a figure with no design reads them for its content
  output$data_inputs <- shiny::renderText({
    id <- current()
    yes(step_is_code("data") ||
          (identical(report_kind(), "figure") && !is.null(id) &&
             is.null(fig_design(rv$p, id)) && step_is_code("content")))
  })
  for (o in c("data_is_code", "content_is_code", "data_inputs")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }
  mode_bar <- function(step) {
    id <- current()
    shiny::req(!is.null(id), has_study())
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2 small",
      shiny::radioButtons(paste0(step, "_mode"), NULL,
                          stats::setNames(c("gui", "code"), c("GUI", t("R program"))),
                          selected = if (step_is_code(step)) "code" else "gui",
                          inline = TRUE),
      help_tip(t("R program: this step is your own R code instead of the definition.")),
      # the rules the code follows, in full
      shiny::actionLink(paste0(step, "_rules"), t("The rules"), class = "small"))
  }
  rules_dialog <- function() shiny::showModal(shiny::modalDialog(
    title = t("A step of your own R: the rules"), .code_rules_ui(t),
    easyClose = TRUE, footer = shiny::modalButton(t("Close"))))
  shiny::observeEvent(input$data_rules, rules_dialog())
  shiny::observeEvent(input$content_rules, rules_dialog())
  output$data_mode_bar <- shiny::renderUI({
    rv$ver
    mode_bar("data")
  })
  output$content_mode_bar <- shiny::renderUI({
    rv$ver
    mode_bar("content")
  })
  mode_pending <- shiny::reactiveVal(NULL)
  for (st in c("data", "content")) local({
    step <- st
    shiny::observeEvent(input[[paste0(step, "_mode")]], {
      id <- current()
      shiny::req(!is.null(id))
      key <- paste(id, step)
      want <- input[[paste0(step, "_mode")]]
      if (identical(want, "code")) {
        code_wanted[[key]] <- TRUE
        return()
      }
      code_wanted[[key]] <- FALSE
      # code there: it goes only when asked
      if (.is_code_step(rv$p, id, step)) {
        mode_pending(list(id = id, step = step))
        shiny::showModal(shiny::modalDialog(
          title = t("Back to the definition?"),
          shiny::p(sprintf(t("%s's %s step is your code: it goes, and the definition makes the step again."),
                           id, t(step))),
          footer = shiny::tagList(.btn("mode_keep", t("Keep the code")),
                                  .btn("mode_drop", t("Remove the code"), class = "btn-danger"))))
      }
    }, ignoreInit = TRUE)
  })
  shiny::observeEvent(input$mode_keep, {
    shiny::removeModal()
    m <- mode_pending()
    if (!is.null(m)) {
      code_wanted[[paste(m$id, m$step)]] <- TRUE
      shiny::updateRadioButtons(session, paste0(m$step, "_mode"), selected = "code")
    }
  })
  shiny::observeEvent(input$mode_drop, {
    shiny::removeModal()
    m <- mode_pending()
    shiny::req(!is.null(m))
    rv$p <- set_step_code(rv$p, m$id, m$step, NA)
    bump()
  })
  # the datasets a step of its own code reads, and its ARD
  output$uc_inputs <- shiny::renderUI({
    rv$ver
    id <- current()
    shiny::req(!is.null(id), has_study())
    p <- shiny::isolate(rv$p)
    cat_ds <- shiny::isolate(catalog())
    have <- report_datasets(p, id)
    k <- report_kind()
    shiny::div(
      class = "rp-b-card mb-2",
      .for_report(shiny::selectizeInput(
        "uc_ds", t("Data it reads (the program reads them first, by their names in lower case)"),
        .ds_choices(cat_ds, have), selected = have, multiple = TRUE, width = "100%"), id),
      # its ARD as `ard`: a table's own or taken in; a figure's as its ARD
      # source says (below)
      shiny::uiOutput("uc_ard_state"))
  })
  output$uc_ard_state <- shiny::renderUI({
    id <- current()
    shiny::req(!is.null(id), has_study())
    rv$ard_ver
    rv$ver
    # (a figure's: a table's data code makes its data itself)
    if (!identical(report_kind(), "figure")) return(NULL)
    s <- rv$study
    s$planner <- rv$p
    st <- .code_ard_state(s, id)
    line <- switch(st$kind,
      import = shiny::div(class = "small text-success",
                          sprintf(t("It reads the ARD taken in (%s)."), st$file)),
      table = shiny::div(class = "small text-success",
                         sprintf(t("It reads %s's ARD."), st$id)),
      own = shiny::div(class = if (st$built) "small text-success" else "small text-warning",
                       sprintf(if (st$built) t("Its ARD definition has %d analyses (built).") else
                         t("Its ARD definition has %d analyses, not built yet: build them in step 1 (ARD)."), st$n)),
      none = shiny::div(class = "small text-muted",
                        t("No ARD: `ard` is not in reach (add analyses, or take an ARD in, to have it).")))
    shiny::tagList(line, if (!identical(st$kind, "none")) shiny::div(
      class = "small text-muted",
      t("`ard` is this report's rows of the ARD (a cards ARD); normalize_ard(ard) makes it a table's shape.")))
  })
  # only the user's change, of the report it was drawn for (#418): a value
  # sent as it is drawn (none, for a report without data) can arrive after
  # the next report is chosen
  shiny::observeEvent(input$uc_ds_by, {
    id <- current()
    shiny::req(!is.null(id), has_study())
    ds <- .report_pick(input$uc_ds_by, id)
    if (is.null(ds)) return()
    if (!identical(sort(toupper(ds)), sort(report_datasets(rv$p, id)))) {
      rv$p <- set_report_datasets(rv$p, id, ds)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  # the rules' check of a step's code (tflspec::tfl_check_code()), under it
  code_check_ui <- function(step) {
    id <- current()
    shiny::req(!is.null(id), has_study())
    r <- tryCatch(.check_step_code(rv$p, id, step), error = function(e) NULL)
    if (!NROW(r)) return(NULL)
    r <- .review_language(r, lang)
    shiny::tagList(lapply(seq_len(nrow(r)), function(i) shiny::div(
      class = paste("small", if (identical(r$level[i], "error")) "text-danger" else "text-warning"),
      r$message[i])))
  }
  output$dc_check <- shiny::renderUI(code_check_ui("data"))
  output$cc_check <- shiny::renderUI(code_check_ui("content"))
  # the code run as its program runs it, up to the report: what it left
  uc_res <- shiny::reactiveVal(NULL)
  shiny::observeEvent(current(), uc_res(NULL))
  uc_run <- function() {
    id <- current()
    shiny::req(!is.null(id))
    s <- rv$study
    s$planner <- rv$p
    uc_res(guarded(preview_code(s, id)))
  }
  shiny::observeEvent(input$dc_run, uc_run())
  shiny::observeEvent(input$cc_run, uc_run())
  uc_result <- function() {
    res <- uc_res()
    if (is.null(res)) return(shiny::p(class = "small text-muted", t("Not run yet.")))
    if (!is.null(res$error)) {
      miss <- regmatches(res$error, regexec("object '([A-Za-z0-9_.]+)' not found", res$error))[[1L]]
      ds <- catalog()$dataset
      hint <- if (length(miss) == 2L && toupper(miss[2L]) %in% toupper(ds))
        sprintf(t("Add %s to the data it reads."), toupper(miss[2L]))
      where <- if (!is.na(res$code_line %||% NA)) sprintf(
        t("Line %d of the %s step's code: "), res$code_line, t(res$code_step))
      return(shiny::tagList(
        shiny::div(class = "alert alert-danger py-1 small",
                   if (!is.null(where)) shiny::strong(where),
                   res$error, if (!is.null(hint)) shiny::div(hint)),
        shiny::tags$details(shiny::tags$summary(class = "small", t("Log")),
                            shiny::div(class = "rp-code", shiny::pre(res$log)))))
    }
    parts <- .code_preview_parts(res$content)
    shiny::tagList(
      shiny::div(class = "alert alert-success py-1 small",
                 sprintf(t("It ran: %d table page(s), %d figure(s)."),
                         length(parts$pages), length(parts$figures))),
      if (length(parts$pages)) preview_html(parts$pages[1L], max_pages = 1L),
      if (length(parts$figures)) shiny::tags$img(
        src = paste0("data:image/png;base64,", jsonlite::base64_enc(parts$figures[[1L]])),
        style = "max-width: 100%; height: auto; display: block;"))
  }
  output$dc_result <- shiny::renderUI(uc_result())
  output$cc_result <- shiny::renderUI(uc_result())
  output$lf_form <- shiny::renderUI({
    rv$ver
    type <- lf_type()
    shiny::req(identical(type, "listing"))
    id <- current()
    p <- shiny::isolate(rv$p)
    lf_env$n <- lf_env$n + 1L
    lf_drawn(lf_env$n)
    lf_env$id <- id
    l <- lf_rows(p, "listings", id)
    lv <- function(k, d = "") if (nrow(l) && !is.na(l[[k]][1L])) l[[k]][1L] else d
    lt <- listing_types()
    # a value the choices do not have (a type of one's own, a dataset not
    # in the catalog) is offered as it is, not replaced by the first
    type_ch <- stats::setNames(lt$type, lt$label)
    if (nzchar(lv("type")) && !lv("type") %in% type_ch) {
      type_ch <- c(type_ch, stats::setNames(lv("type"), lv("type")))
    }
    ds_choices <- .ds_choices(shiny::isolate(catalog()), lv("dataset"))
    mr <- suppressWarnings(as.numeric(lv("max_rows", NA)))
    # the fields as drawn: an edit writes only the fields that differ
    lf_env$last <- list(type = lv("type", .std_setting("listing_type", "multiline")),
                        dataset = lv("dataset"), where = lv("where"),
                        sort = lv("sort"),
                        max_rows = if (is.na(mr)) "" else as.character(mr))
    shiny::div(
      class = "rp-b-card",
      shiny::h6(with_tip(t("Listing"), t("A listing: its data (a dataset of the catalog, a condition, an order) and its columns. Its data code (step 2, Data code), if any, changes the data after the condition."))),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::selectInput(lf_id("type"), t("Type"), type_ch,
                           selected = lf_env$last$type, width = "200px"),
        shiny::selectInput(lf_id("dataset"), t("Dataset"),
                           c(stats::setNames("", "-"), ds_choices),
                           selected = lv("dataset"), width = "200px"),
        shiny::numericInput(lf_id("max_rows"), t("Rows per page"),
                            value = mr, min = 1, width = "140px")),
      shiny::textInput(lf_id("where"), t("Condition (R)"), value = lv("where"),
                       width = "100%",
                       placeholder = paste(t("e.g."), "AESEV == \"SEVERE\"")),
      # as its program has them: the condition, the code lists, the order
      shiny::div(class = "mb-2", shiny::uiOutput("lf_cl"), shiny::uiOutput("lf_cl_detail")),
      shiny::textInput(lf_id("sort"), t("Order (| between variables, - for descending; a column with a code list sorts in the list's order)"),
                       value = lv("sort"), width = "100%",
                       placeholder = paste(t("e.g."), "TRTA | USUBJID | ASTDT")))
  })
  # a listing's code lists: those of its dataset's columns (its program puts
  # them on the data after the condition, before the order)
  lf_cl_ids <- .codelist_part_ids("lf_cl", "cllf")
  lf_cl_ds <- function() {
    l <- lf_rows(rv$p, "listings", current() %||% "")
    if (!nrow(l) || .is_blank(l$dataset[1L])) return(NULL)
    .rows_where(an_data(l$dataset[1L]), l$where[1L])
  }
  lf_cl_vars <- function() {
    cols <- unlist(lapply(lf_rows(rv$p, "listing_cols", current() %||% "")$vars,
                          .split_bar))
    unique(c(names(lf_cl_ds()), cols))
  }
  lf_cl_part <- .codelist_part_server(
    input, output, rv, current, lf_cl_ids, vars = lf_cl_vars, data = lf_cl_ds,
    title = function() sprintf(t("Code lists: the columns of %s's data"), current()),
    dialog = cl_lf, t = t, notify = notify, guarded = guarded, bump = bump,
    has_study = has_study, read_grid = read_grid, grids_drawn = grids_drawn)
  output$lf_cl <- shiny::renderUI({
    shiny::req(has_study(), current(), identical(lf_type(), "listing"))
    cl <- sheet_rows(rv$p, "codelists", current())
    .codelist_part_ui(
      .codelist_lines(cl, lf_cl_vars()),
      lf_cl_part$missing(), t, lf_cl_ids,
      t("This report's code lists of the listing's columns: its program puts them on the data after the condition (set_levels()), each column a factor in the list's order, its values the labels, and the rows sort in that order. A value a list does not have stops the program."),
      open = lf_cl_part$open(), others = .codelist_others(cl, lf_cl_vars()))
  })
  # the form's values back to the listing's row
  lf_values <- shiny::reactive({
    lf_drawn()
    shiny::req(identical(lf_env$id, current()), identical(lf_type(), "listing"))
    get <- function(x) input[[lf_id(x)]]
    shiny::req(!is.null(get("type")))
    mr <- get("max_rows")
    list(sheet = "listings", values = list(
      type = get("type"), dataset = get("dataset") %||% "",
      where = get("where") %||% "", sort = get("sort") %||% "",
      max_rows = if (is.null(mr) || is.na(mr)) "" else as.character(mr)))
  })
  lf_values_d <- shiny::debounce(lf_values, 400)
  # the form's fields into the report's row: only those changed since drawn
  # (or last written); the row's other columns (blank_row, wrap, the
  # figure's ard ...) as they are
  shiny::observeEvent(lf_values_d(), {
    v <- lf_values_d()
    id <- lf_env$id
    row <- lf_rows(rv$p, v$sheet, id)
    row$output_id <- NULL
    if (!nrow(row)) row[1L, ] <- NA
    row <- row[1L, , drop = FALSE]
    last <- lf_env$last
    for (k in names(v$values)) {
      if (!identical(v$values[[k]], last[[k]])) row[[k]] <- v$values[[k]]
    }
    lf_env$last <- v$values
    p2 <- set_lf_rows(rv$p, v$sheet, id, row)
    if (!identical(p2$lf, rv$p$lf)) rv$p <- p2
  })
  output$lf_cols_box <- shiny::renderUI({
    shiny::req(identical(lf_type(), "listing"))
    shiny::div(
      class = "rp-b-card",
      shiny::h6(with_tip(t("Columns, in order"),
                         t("vars: variables stacked in the column (| between them); label: \\n breaks the header; width: characters; collapse_repeats: print a repeated value once."))),
      .lf_more_note(rv$p, current(), t),
      rhandsontable::rHandsontableOutput("hot_lf_cols"))
  })
  lf_cols_key <- shiny::reactive(paste("lf_cols", input$target, rv$ver,
                                       sep = "|"))
  output$hot_lf_cols <- rhandsontable::renderRHandsontable({
    shiny::req(identical(lf_type(), "listing"))
    id <- current()
    p <- shiny::isolate(rv$p)
    d <- lf_rows(p, "listing_cols", id)
    d$output_id <- NULL
    l <- lf_rows(p, "listings", id)
    cols <- if (nrow(l)) dataset_columns(l$dataset[1L]) else character()
    grids_drawn()
    .grid(d, "listing_cols", lf_cols_key(),
          list(vars = cols, collapse_repeats = .bool), read_only = "more")
  })
  shiny::observeEvent(input$hot_lf_cols, {
    h <- input$hot_lf_cols
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, lf_cols_key())) return()
    d <- read_grid(h)
    if (is.null(d)) return()
    guarded(rv$p <- set_lf_rows(rv$p, "listing_cols", current(), d))
  })
  lf_key <- shiny::reactive(paste("lf_listings", input$target, rv$ver, sep = "|"))
  output$hot_lf_listings <- rhandsontable::renderRHandsontable({
    shiny::req(identical(lf_type(), "listing"))
    id <- current()
    p <- shiny::isolate(rv$p)
    d <- lf_rows(p, "listings", id)
    d$output_id <- NULL
    grids_drawn()
    .grid(d, "listings", lf_key(),
          list(type = listing_types()$type, dataset = shiny::isolate(catalog())$dataset),
          read_only = "content_more")
  })
  shiny::observeEvent(input$hot_lf_listings, {
    h <- input$hot_lf_listings
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, lf_key())) return()
    d <- read_grid(h)
    if (is.null(d)) return()
    p2 <- guarded(set_lf_rows(rv$p, "listings", current(), d))
    if (!is.null(p2)) {
      rv$p <- p2
      bump()
    }
  })
  lf_pv <- shiny::reactiveVal(NULL)
  shiny::observeEvent(current(), {
    if (!identical(lf_pv()$id, current())) lf_pv(NULL)
  })
  shiny::observeEvent(input$lf_preview, {
    id <- current()
    if (!identical(lf_type(), "listing")) {
      return(notify(t("Choose a Listing report"), "warning"))
    }
    lf_show_preview(id)
  })
  lf_show_preview <- function(id) {
    if (!nrow(lf_rows(rv$p, "listing_cols", id))) {
      return(lf_pv(list(error = t("The listing has no columns yet: add them to the table of columns (below the form), or add the listing again with 'Start the listing from the data'."))))
    }
    r <- NULL
    shiny::withProgress(message = sprintf(t("Preview of %s"), id), {
      r <- tryCatch(list(pages = preview_listing(current_study(), id)),
                    error = function(e) list(error = conditionMessage(e)))
    })
    r$id <- id
    lf_pv(r)
  }
  output$lf_preview_out <- shiny::renderUI({
    r <- lf_pv()
    if (is.null(r)) {
      return(shiny::p(class = "small text-muted",
                      t("Preview the listing to see its first pages with the data.")))
    }
    if (!is.null(r$error)) {
      return(shiny::div(class = "alert alert-danger py-1 small",
                        shiny::tags$pre(class = "mb-0", r$error)))
    }
    shiny::tagList(
      shiny::p(class = "small text-muted",
               sprintf(t("%d pages"), length(r$pages))),
      preview_html(r$pages, max_pages = 2L, align = "left"))
  })
  .srv_keep(environment())
}
