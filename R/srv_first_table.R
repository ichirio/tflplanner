# app_server(): first table -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_first_table <- function() {
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
