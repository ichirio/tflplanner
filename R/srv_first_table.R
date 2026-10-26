# app_server(): first table -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_first_table <- function() {
  # a shift table: the analysis set (of ADSL), the group, the parameters
  # (a page each), the baseline and the post-baseline category, which
  # records count, the worst order, a Total column
  mf_shift_ui <- function() {
    d <- first_data()
    shiny::req(d)
    sl <- first_subjects_file()
    if (is.na(sl)) {
      return(shiny::div(class = "alert alert-warning py-1 small",
        t("A shift table counts the subjects of an analysis set: put the subject-level data (adsl) on the Data tab.")))
    }
    if (!"PARAMCD" %in% names(d)) {
      return(shiny::div(class = "alert alert-warning py-1 small",
        t("A shift table reads a data with parameters (PARAMCD): choose one (adlb).")))
    }
    a <- first_subjects()
    shiny::req(a)
    cols <- as.list(d)
    fl <- grep("FL$", names(a), value = TRUE)
    grp <- intersect(.group_choices(cols), names(a))
    ch <- names(cols)[vapply(cols, function(v) is.character(v) || is.factor(v), NA)]
    first_of <- function(usual) c(intersect(usual, ch), setdiff(ch, usual))
    pc <- sort(unique(as.character(d$PARAMCD[!is.na(d$PARAMCD)])))
    shiny::tagList(
      shiny::selectInput("mf_pop", t("Population (its flag = \"Y\")"),
                         .labelled(fl, as.list(a)),
                         selected = c(intersect(c("SAFFL", "ITTFL", "FASFL"), fl), fl)[1L],
                         width = "100%"),
      shiny::selectInput("mf_group", t("Group (the columns)"), grp,
                         selected = c(intersect(c("TRT01A", "TRTA", "TRT01P", "ARM"), grp), grp)[1L],
                         width = "100%"),
      shiny::selectizeInput("mf_params", t("Parameters (a page each)"), pc,
                            selected = pc[seq_len(min(2L, length(pc)))], multiple = TRUE,
                            width = "100%", options = list(plugins = list("remove_button", "drag_drop"))),
      shiny::div(
        class = "d-flex gap-2",
        shiny::selectInput("mf_base", t("Baseline category"), first_of(c("BNRIND", "BTOXGR")),
                           width = "50%"),
        shiny::selectInput("mf_post", t("Category after baseline"), first_of(c("ANRIND", "ATOXGR")),
                           width = "50%")),
      shiny::textInput("mf_where", t("Records counted (R; blank: all)"),
                       value = if ("ONTRTFL" %in% names(cols)) "ONTRTFL == \"Y\"" else "",
                       width = "100%"),
      shiny::uiOutput("mf_worst_ui"),
      shiny::checkboxInput("mf_total", t("Total column"), TRUE),
      shiny::p(class = "small text-muted",
               t("Each subject once a parameter, at the worst category after baseline; percents of each column's subjects.")))
  }
  # the worst order: the post-baseline categories from best to worst
  output$mf_worst_ui <- shiny::renderUI({
    d <- first_data()
    shiny::req(d, input$mf_post %in% names(d))
    v <- as.character(d[[input$mf_post]])
    shown <- .shift_levels(v)
    w <- if (all(c("LOW", "NORMAL", "HIGH") %in% toupper(shown))) {
      c(shown[toupper(shown) == "NORMAL"], setdiff(shown, shown[toupper(shown) == "NORMAL"]))
    } else shown
    shiny::textInput("mf_worst", with_tip(t("Worst: from best to worst"),
                                          t("A subject's record of a parameter after baseline is the last of these it has; | between them.")),
                     value = paste(w, collapse = " | "), width = "100%")
  })
  start_first_table <- function(id, desc) {
    d <- first_data()
    if (is.null(d)) return(notify(t("Choose the data"), "warning"))
    if (identical(input$mf_kind, "shift")) {
      a <- first_subjects()
      if (is.null(a)) return(notify(t("Choose the data"), "warning"))
      worst <- trimws(strsplit(input$mf_worst %||% "", "|", fixed = TRUE)[[1L]])
      p2 <- guarded(first_shift_table(
        rv$p, id, input$mf_data, d, first_subjects_file(), a,
        population = input$mf_pop, group = input$mf_group, params = input$mf_params,
        baseline = input$mf_base, post = input$mf_post,
        worst = if (length(worst[nzchar(worst)])) worst[nzchar(worst)],
        where = input$mf_where %||% NA_character_,
        total = if (isTRUE(input$mf_total)) "Total" else NA_character_,
        description = desc))
      if (is.null(p2)) return()
      after_id_change(p2, id)
      go("tables")
      bslib::nav_select("table_right", "result")
      do_preview(id)
      return()
    }
    if (identical(input$mf_kind, "hierarchy")) {
      a <- first_subjects()
      if (is.null(a)) return(notify(t("Choose the data"), "warning"))
      p2 <- guarded(first_hierarchy_table(
        rv$p, id, input$mf_data, d, first_subjects_file(), a,
        population = input$mf_pop, group = input$mf_group,
        hierarchy = input$mf_hier, where = input$mf_where %||% NA_character_,
        grade = input$mf_grade %||% NA_character_,
        total = if (isTRUE(input$mf_total) && !nzchar(input$mf_grade %||% "")) "Total" else
          NA_character_,
        description = desc))
      if (is.null(p2)) return()
      after_id_change(p2, id)
      go("tables")
      bslib::nav_select("table_right", "result")
      do_preview(id)
      return()
    }
    p2 <- guarded(first_table(rv$p, id, input$mf_data, d,
                              population = input$mf_pop,
                              group = input$mf_group,
                              variables = input$mf_vars,
                              description = desc,
                              stack = !isFALSE(input$mf_stack)))
    if (is.null(p2)) return()
    attr(p2, "group_missing") <- NULL
    after_id_change(p2, id)
    go("tables")
    bslib::nav_select("table_right", "result")
    do_preview(id)
  }
  shiny::observeEvent(input$copy, {
    if (need_current()) ask_id(sprintf(t("Copy %s"), current()), "copy_ok",
                               paste0(current(), "_2"))
  })
  shiny::observeEvent(input$copy_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(copy_output(rv$p, current(), id)), id)
  })
  shiny::observeEvent(input$rename, {
    if (need_current()) ask_id(sprintf(t("Rename %s"), current()),
                               "rename_ok", current())
  })
  shiny::observeEvent(input$rename_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(rename_output(rv$p, current(), id)), id)
  })
  shiny::observeEvent(input$remove, {
    if (!need_current()) return()
    readers <- .figs_reading_table(rv$p, current())
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Delete %s"), current()),
      t("The report's rows are deleted from every sheet (on save; its program and outputs stay on disk)."),
      if (length(readers)) shiny::p(class = "text-danger mt-2 mb-0",
        sprintf(t("%s print numbers from its ARD: they will have none until their step 1 names another table."),
                paste(readers, collapse = ", "))),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        .btn("remove_ok", t("Delete"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$remove_ok, {
    rv$p <- remove_output(rv$p, current())
    shiny::removeModal()
    bump()
  })
  move <- function(by) {
    id <- current()
    if (is.null(id)) return()
    o <- rv$p$outputs
    i <- which(o$output_id == id)
    j <- i + by
    if (j < 1L || j > nrow(o)) return()
    o[c(i, j), ] <- o[c(j, i), ]
    rownames(o) <- NULL
    rv$p$outputs <- o
  }
  shiny::observeEvent(input$up, move(-1L))
  shiny::observeEvent(input$down, move(1L))
  # the whole list in its ids' order, asked first (the order is the order
  # the reports are made in)
  shiny::observeEvent(input$sort_ids, {
    o <- rv$p$outputs
    to <- sort_outputs(rv$p)$outputs$output_id
    moved <- sum(o$output_id != to)
    if (!moved) {
      notify(t("The reports are in their ids' order already."))
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = t("Sort the reports by ID?"),
      sprintf(t("%d of %d reports move: the list goes in its ids' order, by the numbers in the id (T-14-1-2 before T-14-1-10, T-14-0-1 before F-14-2-1), then its letters. The list's order is the order the reports are made in; the up and down arrows still move one."),
              moved, nrow(o)),
      footer = shiny::tagList(
        shiny::modalButton(t("Cancel")),
        .btn("sort_ids_ok", t("Sort"), class = "btn-primary"))))
  })
  shiny::observeEvent(input$sort_ids_ok, {
    shiny::removeModal()
    rv$p <- sort_outputs(rv$p)
  })

  # -- data ----------------------------------------------------------------
  data_ver <- shiny::reactiveVal(0L)
  data_files <- shiny::reactive({
    data_ver()
    input$data_refresh
    shiny::req(has_study())
    study_files(rv$study, "data")
  })
  # the folder open (the pill): where Add files puts them
  data_pill <- shiny::reactive(input$data_pill %||% "adam")
  # the catalog's rows of the folder open: by their file's folder, a row
  # with no file by its level
  ds_sub <- shiny::reactive({
    shiny::req(has_study())
    d <- ard_rows(rv$p, "datasets", "")
    d[.dataset_pill(d$path, d$level) == data_pill(), , drop = FALSE]
  })
  # the grid is the catalog's rows of the folder, with the file's size beside
  # (display only: it is not written back)
  ds_key <- shiny::reactive(paste("datasets", data_pill(), rv$ver, data_ver(), sep = "|"))
  output$hot_ard_datasets <- rhandsontable::renderRHandsontable({
    shiny::req(has_study())
    grids_drawn()
    key <- ds_key()
    d <- shiny::isolate(ds_sub())
    lay <- study_layout()
    f <- shiny::isolate(data_files())
    mine <- f[f$folder == lay[[data_pill()]], , drop = FALSE]
    p <- .data_file(rv$study$path, d$path)
    d$size_kb <- ifelse(is.na(d$path), NA_character_,
                        ifelse(file.exists(p), sprintf("%.1f", file.size(p) / 1024),
                               t("no file")))
    # a file outside the study folder (ADaM kept elsewhere), said (its
    # folder under the grid)
    out <- .data_outside(rv$study$path, d$path)
    if (any(out)) d$where <- ifelse(out, t("outside the study folder"), "")
    h <- .grid(d, "datasets", key,
               list(path = file.path(mine$folder, mine$file)),
               closed = character(), select = TRUE)
    h <- suppressWarnings(rhandsontable::hot_col(h, "size_kb", readOnly = TRUE))
    if (any(out)) h <- suppressWarnings(rhandsontable::hot_col(h, "where", readOnly = TRUE))
    h
  })
  shiny::observeEvent(input$hot_ard_datasets, {
    h <- input$hot_ard_datasets
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, ds_key())) return()
    d <- read_grid(h)
    if (is.null(d)) return()
    d$size_kb <- d$where <- NULL
    # a new row of ADaM / SDTM is of that level (it stays under its pill)
    lv <- c(adam = "ADaM", sdtm = "SDTM")[data_pill()]
    if (!is.na(lv)) {
      blank <- (is.na(d$path) | !nzchar(d$path)) & (is.na(d$level) | !nzchar(d$level))
      d$level[blank] <- lv
    }
    full <- ard_rows(rv$p, "datasets", "")
    guarded(rv$p <- set_ard_rows(rv$p, "datasets", "",
                                 .datasets_put_back(full, d, data_pill())))
  })
  # the files of the folder that have no row, and the names that have
  # several files (the row uses one)
  output$catalog_files_note <- shiny::renderUI({
    shiny::req(has_study())
    f <- data_files()
    lay <- study_layout()
    f <- f[f$folder == lay[[data_pill()]] &
             tolower(tools::file_ext(f$file)) %in% .data_exts, , drop = FALSE]
    rel <- file.path(f$folder, f$file)
    d <- rv$p$ard$datasets
    bare <- rel[!rel %in% d$path & !.dataset_name(f$file) %in% d$dataset]
    cl <- catalog_name_clashes(rv$p, data_files())
    cl <- cl[cl$dataset %in% .dataset_name(f$file), , drop = FALSE]
    # the folders outside the study folder the rows of this folder read
    mine <- ds_sub()$path
    away <- unique(dirname(.data_file(rv$study$path, mine[.data_outside(rv$study$path, mine)])))
    shiny::tagList(
      if (length(away)) shiny::p(
        class = "small text-muted mt-1 mb-0",
        sprintf(t("Outside the study folder: %s"), paste0(away, "/", collapse = ", "))),
      if (length(bare)) shiny::p(
        class = "small text-muted mt-1 mb-0",
        sprintf(t("Files with no row yet: %s (Refresh makes the rows)."),
                paste(bare, collapse = ", "))),
      if (nrow(cl)) shiny::p(
        class = "small text-warning mt-1 mb-0",
        sprintf(t("More than one file of a name: %s. The row uses the file in its path; choose another there."),
                paste(unique(cl$dataset), collapse = ", "))),
      if (anyDuplicated(d$dataset[!is.na(d$dataset)])) shiny::p(
        class = "small text-warning mt-1 mb-0",
        sprintf(t("A dataset name is in the catalog twice: %s. Only the first row is read."),
                paste(unique(d$dataset[duplicated(d$dataset)]), collapse = ", "))))
  })
  output$data_folder_where <- shiny::renderUI(shiny::code(paste0(study_layout()[[data_pill()]], "/")))
  shiny::observeEvent(input$data_upload, {
    f <- input$data_upload
    dest <- file.path(rv$study$path, study_layout()[[data_pill()]])
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(f$datapath, file.path(dest, f$name), overwrite = TRUE)
    data_ver(data_ver() + 1L)
    notify(sprintf(t("%d files added to %s"), sum(ok),
                   study_layout()[[data_pill()]]))
    to_catalog()
  })
  shiny::observeEvent(input$data_refresh, to_catalog(), ignoreInit = TRUE)
  .srv_keep(environment())
}
