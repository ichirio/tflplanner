# app_server(): ard make -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_ard_make <- function() {
  # Make the ARD (step 1): this report's rows of the study ARD, by its
  # saved ARD program (the study saved first) -- what a table program
  # and a figure printing its numbers read; then read for the preview
  do_study_ard <- function(id) {
    if (is.null(id)) return(notify(t("Choose a report"), "warning"))
    if (!any(rv$p$ard$analyses$output_id %in% id)) {
      return(notify(sprintf(t("%s has no analyses in the ARD definition."), id), "warning"))
    }
    t0 <- Sys.time()
    ok <- FALSE
    n_an <- sum(rv$p$ard$analyses$output_id %in% id)
    shiny::withProgress(message = sprintf(t("The study ARD of %s"), id), {
      shiny::setProgress(0.05, detail = t("1/2 saving the study"))
      if (ard_ready()) {
        shiny::setProgress(0.2, detail = sprintf(
          t("2/2 making its ARD (%d analyses, in its own R process)"), n_an))
        r <- guarded(update_study_ard(current_study(), id))
        ard_state_ver(ard_state_ver() + 1L)
        rv$status_ver <- rv$status_ver + 1L
        if (!is.null(r)) {
          ard_preview_out(list(id = id, text = r$output))
          if (!r$ok) {
            ard_res(list(scope = id, error = r$error))
            notify(t("The ARD program failed: see what it printed in Data > Study ARD."),
                   "error")
          } else ok <- TRUE
        }
      }
    })
    if (!ok) return(invisible(FALSE))
    ard_res(list(ard = study_ard_rows(current_study(), id), scope = id,
                 seconds = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    notify(sprintf(t("%s: its rows of the study ARD are made."), id))
    # (the preview reads them, when it is a report's)
    if (id %in% rv$p$outputs$output_id &&
        !identical(ard_preview_state(current_study(), id), "current")) {
      if (!is.null(tryCatch(fetch_ard(current_study(), id), error = function(e) NULL))) {
        rv$ard_ver <- rv$ard_ver + 1L
      }
    }
    invisible(TRUE)
  }
  shiny::observeEvent(input$ard_make_study, do_study_ard(ard_target()))
  shiny::observeEvent(input$fetch, do_preview())
  shiny::observeEvent(input$page_fetch, do_preview())
  shiny::observeEvent(input$fetch2, do_preview())
  shiny::observeEvent(input$fetch3, do_preview())
  output$ard_summary <- shiny::renderUI({
    m <- meta_of(current())
    if (is.null(m)) return(NULL)
    keys <- vapply(names(m$keys), function(k) {
      l <- m$keys[[k]]
      sprintf("%s (%d): %s%s", k, length(l),
              paste(utils::head(l, 6L), collapse = " | "),
              if (length(l) > 6L) " | ..." else "")
    }, "")
    v <- m$variables
    shiny::div(
      class = "small border rounded p-2 mb-2",
      shiny::div(sprintf(t("Read %s from %s"), m$fetched,
                         if (identical(m$source, "data"))
                           t("the normalized data") else t("the raw ARD"))),
      if (!is.null(m$error)) shiny::div(class = "text-danger", m$error),
      shiny::div(shiny::strong(t("Keys")), ": ",
                 if (length(keys)) paste(keys, collapse = "; ") else "-"),
      if (length(m$hierarchy)) shiny::div(
        shiny::strong(t("Hierarchy")), ": ",
        paste(m$hierarchy, collapse = " > ")),
      shiny::div(shiny::strong(t("Variables")), ": ",
                 if (nrow(v)) paste(sprintf("%s [%s%s]", v$variable, v$kind,
                                            ifelse(v$n_levels > 0,
                                                   paste0(", ", v$n_levels),
                                                   "")),
                                    collapse = ", ") else "-"),
      shiny::div(shiny::strong(t("Statistics")), ": ",
                 paste(m$stats, collapse = ", ")))
  })



  # -- the ARD definition ------------------------------------------------
  # Its grids work like the others: `analyses` shows the rows of the report
  # chosen in the sidebar (all of them for Study defaults / ALL); the other
  # sheets are the study's.  The code is tfl_ard_code() of the definition
  # as it stands; Run executes it with run_ard() and shows the ARD.
  ard_res <- shiny::reactiveVal(NULL)
  ard_cols <- new.env()
  data_columns <- function() {
    if (!has_study()) return(character())
    ds <- rv$p$ard$datasets
    unique(unlist(lapply(ds$path[!is.na(ds$path)], function(pth) {
      f <- .data_file(rv$study$path, pth)
      if (!file.exists(f)) return(NULL)
      k <- paste(f, file.mtime(f))
      if (is.null(ard_cols[[k]])) {
        ard_cols[[k]] <- tryCatch(names(read_data_head(f, 1L)),
                                  error = function(e) character())
      }
      ard_cols[[k]]
    })))
  }
  ard_functions <- function() {
    fs <- character()
    for (pkg in c("cards", "cardx")) {
      if (requireNamespace(pkg, quietly = TRUE)) {
        e <- getNamespaceExports(pkg)
        fs <- c(fs, paste0(pkg, "::", sort(e[startsWith(e, "ard_")])))
      }
    }
    fs
  }
  ard_choices <- function(sheet) {
    p <- rv$p
    cols <- data_columns()
    switch(sheet,
      analyses = list(
        output_id = output_ids(p),
        method = c(.std_ard_methods()$method, ard_functions()),
        data = p$ard$analysis_data$data_id,
        dataset = p$ard$datasets$dataset,
        population_id = p$ard$populations$population_id,
        by = cols, strata = cols, variables = cols,
        # (tflspec >= 0.0.24.9186: yes, the variables the data lack skipped)
        optional = c("yes", "no"),
        denominator = c("population", "row", "column", "cell",
                        p$ard$populations$population_id,
                        p$ard$datasets$dataset,
                        p$ard$analysis_data$data_id)),
      datasets = list(path = if (has_study()) {
        f <- study_files(rv$study, "data")
        file.path(f$folder, f$file)
      }),
      populations = list(dataset = p$ard$datasets$dataset),
      analysis_data = list(
        from = c(p$ard$datasets$dataset, p$ard$analysis_data$data_id),
        population_id = p$ard$populations$population_id,
        subjects = p$ard$analysis_data$data_id),
      study = list(key = c("id", "output", "source")),
      list())
  }
  # (the analysis data are each report's: 1-1's sheet, not the Data tab's)
  for (sheet in setdiff(names(.ard_sheets()), c("analysis_data", "datasets"))) local({
    sh <- sheet
    out_id <- paste0("hot_ard_", sh)
    by_report <- sh == "analyses"
    # the analyses of the report chosen, or of every report (an analysis
    # several reports use, GROUPN, seen at once): then the grid is the whole
    # sheet with its output_id column, edited as such
    tg_of <- function() if (by_report && !isTRUE(input$ard_all)) target() else ""
    key <- shiny::reactive({
      k <- paste("ard", sh, if (by_report) input$target, rv$ver, sep = "|")
      if (by_report && isTRUE(input$ard_all)) paste0(k, "|all") else k
    })
    output[[out_id]] <- rhandsontable::renderRHandsontable({
      shiny::req(has_study())
      tg <- tg_of()
      d <- ard_rows(shiny::isolate(rv$p), sh, tg)
      if (by_report && !identical(tg, "") && !is.na(tg)) d$output_id <- NULL
      grids_drawn()
      .grid(d, sh, key(), shiny::isolate(ard_choices(sh)),
            closed = .ard_closed_columns, select = by_report)
    })
    shiny::observeEvent(input[[out_id]], {
      h <- input[[out_id]]
      if (is.null(h$changes$changes) &&
          !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
      if (!identical(h$params$planner_key, key())) return()
      d <- read_grid(h)
      if (is.null(d)) return()
      tg <- tg_of()
      guarded(rv$p <- set_ard_rows(rv$p, sh, tg, d))
    })
  })
  # 1-1's sheet: the analysis data the report reads, edited in their place
  # in the study's sheet (rows added go after them)
  # the report's analysis data and the one open in 1-1 (not read yet by an
  # analysis, a new one for one)
  adata_grid_ids <- function() .adata_of_report(rv$p, ard_target())
  adata_grid_key <- shiny::reactive(paste("adata_report", input$target, adata_pick() %||% "",
                                          rv$ver, sep = "|"))
  output$hot_adata_report <- rhandsontable::renderRHandsontable({
    shiny::req(has_study())
    tg <- ard_target()
    shiny::req(tg)
    p <- shiny::isolate(rv$p)
    ad <- .adata_rows(p, ard_target())
    d <- ad[ad$data_id %in% shiny::isolate(adata_grid_ids()), , drop = FALSE]
    grids_drawn()
    .grid(d, "analysis_data", adata_grid_key(), shiny::isolate(ard_choices("analysis_data")),
          closed = .ard_closed_columns)
  })
  shiny::observeEvent(input$hot_adata_report, {
    h <- input$hot_adata_report
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, adata_grid_key())) return()
    d <- read_grid(h)
    if (is.null(d)) return()
    p <- rv$p
    guarded(rv$p <- set_ard_rows(p, "analysis_data", ard_target(), d))
  })
  output$ard_methods <- DT::renderDT(
    .std_ard_methods()[c("method", "call", "kind", "defaults", "formats", "note")],
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE, ordering = FALSE))
  output$ard_stat_catalog <- DT::renderDT({
    d <- .std_ard_statistics()
    d$computed <- ifelse(is.na(d$fun), "cards / cardx", "tflplanner")
    DT::datatable(d[c("statistic", "kind", "group", "label", "fmt",
                      "computed", "note")],
                  rownames = FALSE, filter = "top", class = "compact",
                  options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                                 scrollY = "40vh", scrollCollapse = TRUE))
  })

  # -- one analysis as a form ----------------------------------------------
  # A click on a row of the analyses grid shows that analysis here, with the
  # parts of the first-table form: what to compute (by its label), the data,
  # the analysis set, the groups (columns), the variables (rows), a subset,
  # and the statistics with their formats.  Apply writes the row; the grid
  # stays for what the form does not say (args, code).  Fresh input ids per
  # drawing, as the builder's.
  st_env <- new.env()
  st_env$n <- 0L
  session$userData$st_env <- st_env
  st_drawn <- shiny::reactiveVal(0L)
  st_id <- function(x) paste0("st", st_env$n, "_", x)
  an_pick <- shiny::reactiveVal(NULL)
  st_rows <- shiny::reactive({
    tg <- ard_target()
    if (is.null(tg) || !has_study()) return(NULL)
    a <- rv$p$ard$analyses
    a[!is.na(a$output_id) & a$output_id == tg & !is.na(a$analysis_id), ,
      drop = FALSE]
  })
  # the grid's row clicked: the analysis it shows
  shiny::observeEvent(input$hot_ard_analyses_select, {
    r <- input$hot_ard_analyses_select$select$r
    tg <- ard_target()
    if (length(r) != 1L) return()
    if (isTRUE(input$ard_all)) {
      # every report's rows: the row's report becomes the one chosen
      d <- ard_rows(rv$p, "analyses", "")
      if (r < 1L || r > nrow(d) || is.na(d$analysis_id[r])) return()
      an_pick(d$analysis_id[r])
      if (!identical(d$output_id[r], tg) &&
          d$output_id[r] %in% output_ids(rv$p)) {
        shiny::updateSelectInput(session, "target", selected = d$output_id[r])
      }
      return()
    }
    if (is.null(tg)) return()
    d <- ard_rows(rv$p, "analyses", tg)
    if (r >= 1L && r <= nrow(d) && !is.na(d$analysis_id[r])) {
      an_pick(d$analysis_id[r])
    }
  })
  st_row <- shiny::reactive({
    a <- st_rows()
    shiny::req(a, nrow(a))
    id <- an_pick()
    if (is.null(id) || !id %in% a$analysis_id) id <- a$analysis_id[1L]
    a[a$analysis_id == id, , drop = FALSE][1L, ]
  })
  st_kind <- function(r) {
    keys <- .std_ard_methods()
    k <- match(.method_kw(r$method), keys$method)
    if (is.na(k)) "" else keys$kind[k]
  }
  # the format a statistic gets when the analysis says none
  st_default <- function(r, stat) {
    keys <- .std_ard_methods()
    k <- match(.method_kw(r$method), keys$method)
    m <- if (!is.na(k)) .parse_formats(keys$formats[k]) else character()
    if (!is.na(m[stat])) return(unname(m[stat]))
    st <- .std_ard_statistics()
    unname(st$fmt[match(stat, st$statistic)])
  }
  # A form field's label: what it is, then its name in the spec and the
  # value it takes when left blank -- "Percentages of (denominator =
  # population)".
  argl <- function(text, arg, dflt = NULL) {
    d <- if (length(dflt) == 1L && !is.na(dflt) && nzchar(dflt))
      paste0(" = ", dflt) else ""
    paste0(t(text), " (", arg, d, ")")
  }
  # An argument's hint, the catalog's (tfl_ard_args()) for that function, in
  # the app's language; NULL when it has none
  arg_hint <- function(call, arg) {
    a <- if (!is.null(call) && !is.na(call))
      tryCatch(tflspec::tfl_ard_args(call), error = function(e) NULL)
    h <- if (!is.null(a)) a$hint[match(arg, a$arg)] else NA_character_
    if (!length(h) || is.na(h) || !nzchar(h)) return(NULL)
    k <- paste0("arg-hint:", h)
    v <- t(k)
    if (identical(v, k)) h else v
  }
  # A label with its hint as a tooltip: an (i) to point at, or to reach with
  # the keyboard (the form says no more than the label)
  with_hint <- function(label, hint) {
    if (is.null(hint)) return(label)
    shiny::span(label, bslib::tooltip(
      shiny::span(class = "ms-1 text-muted rp-hint", tabindex = "0", role = "img",
                  `aria-label` = hint, "\u24d8"),
      hint))
  }
  # the value a method's own defaults give an argument ("population" for a
  # hierarchical analysis's denominator), or NULL
  st_arg_default <- function(r, arg) {
    keys <- .std_ard_methods()
    d <- keys$defaults[match(.method_kw(r$method), keys$method)]
    if (is.na(d) || !nzchar(d)) return(NULL)
    p <- trimws(strsplit(d, ",")[[1L]])
    hit <- p[startsWith(p, paste0(arg, " ")) | startsWith(p, paste0(arg, "="))]
    if (!length(hit)) return(NULL)
    trimws(sub("^[^=]*=", "", hit[1L]))
  }
  # the data an analysis reads, as one choice: a dataset and an analysis
  # set ("<dataset>|<population_id>"), named as the program names it
  data_choices <- function(r) {
    ch <- .an_data_choices(rv$p$ard$datasets$dataset, rv$p$ard$populations,
                           now = .an_data_value_row(r),
                           words = data_words_list(), counts = shiny::isolate(data_counts()),
                           adata = .adata_rows(rv$p, ard_target()), first = .adata_of_report(rv$p, ard_target()),
                           adata_counts = shiny::isolate(adata_counts()))
    # a dataset x analysis set that a plain analysis data of the report is
    # too: the analysis data is its one line
    ch <- .an_data_drop_same(ch, .an_data_value_row(r), .adata_rows(rv$p, ard_target()),
                             rv$p$ard$populations, data_words_list()$groups[[3L]])
    # on an analysis data: the analysis data only (a dataset x analysis set
    # stays for an analysis that reads one)
    if (is.list(ch) && startsWith(.an_data_value_row(r), "@")) ch[[length(ch)]] <- NULL
    ch
  }
  data_words_list <- function() list(with = t("%s \u00d7 %s (%s)"),
                                     alone = t("%s, no population (%s)"),
                                     none = t("(no data)"), count = t("%s: %s"),
                                     added = t("+ %s"), per = t("one row per %s"),
                                     subj = t("%s's subjects"),
                                     named = t("%s (%s)"),
                                     groups = c(t("This report's analysis data"),
                                                t("The study's other analysis data"),
                                                t("Dataset \u00d7 population (named by the program)")))
  adata_count_words <- list(records = t("%d records, %d subjects"),
                            one_row = t("%d subjects, one row a subject"))
  # the records and subjects of each analysis data, made as the ARD
  # programs make them: again only when the sheets or a data file change
  adata_counts <- shiny::reactive({
    shiny::req(has_study())
    ad <- .adata_rows(rv$p, ard_target())
    if (!nrow(ad)) return(character())
    ds <- rv$p$ard$datasets
    f <- .data_file(rv$study$path, ds$path[!is.na(ds$path)])
    key <- paste(c(rv$study$path, unlist(ad), unlist(ds), unlist(rv$p$ard$populations),
                   as.character(file.mtime(f))), collapse = "|")
    if (!identical(ard_cols[["adata_key"]], key)) {
      w <- adata_count_words
      ard_cols[["adata_counts"]] <- vapply(ad$data_id, function(id)
        .adata_count_words(.adata_make(rv$p, rv$study$path, ard_target(), id), w), "")
      ard_cols[["adata_key"]] <- key
    }
    ard_cols[["adata_counts"]]
  })
  # the subjects each data choice reads: counted once when the study is
  # opened (all its rows), and again only when the data catalog, the
  # analysis sets or a data file changes
  data_counts <- shiny::reactive({
    shiny::req(has_study())
    ds <- rv$p$ard$datasets
    po <- rv$p$ard$populations
    f <- .data_file(rv$study$path, ds$path[!is.na(ds$path)])
    key <- paste(c(rv$study$path, ds$dataset, ds$path, po$population_id, po$dataset, po$where,
                   as.character(file.mtime(f))), collapse = "|")
    if (!identical(ard_cols[["counts_key"]], key)) {
      ard_cols[["counts"]] <- tryCatch(
        .an_data_counts(current_study(),
                        list(subjects = t("%d subjects"), records = t("%d records, %d subjects"))),
        error = function(e) NULL)
      ard_cols[["counts_key"]] <- key
    }
    ard_cols[["counts"]]
  })
  # counted when the study is opened, not when a form is first drawn
  shiny::observe({
    if (has_study()) tryCatch(data_counts(), error = function(e) NULL)
  }, priority = -10)
  # the dataset and the analysis set the form's data choice stands for
  form_data <- function(r) {
    v <- input[[st_id("data")]]
    .an_data_split(v %||% .an_data_value_row(r), .adata_rows(rv$p, ard_target()))
  }
  # an analysis data chosen in the form: its subjects become the
  # percentages' denominator, when the form has one and it says nothing
  shiny::observe({
    st_drawn()
    v <- input[[st_id("data")]]
    if (is.null(v) || !startsWith(v, "@")) return()
    r <- shiny::isolate(st_row())
    if (identical(v, .an_data_value_row(r))) return()
    den <- shiny::isolate(input[[st_id("den")]])
    if (is.null(den) || nzchar(den)) return()
    s <- .adata_subjects_of(.adata_rows(shiny::isolate(rv$p), ard_target()), substring(v, 2L))
    if (!is.na(s)) shiny::updateSelectInput(session, st_id("den"), selected = s)
  })
  # the method chosen in the form (the function list on screen may show
  # another category, or a search, without the chosen one)
  st_method <- shiny::reactiveVal(NA_character_)
  # the arguments a search's word means (PROC LOGISTIC: method = "glm", ...)
  # by the function found, for this drawing of the form (`n`); and those
  # the user chose to start from
  fn_offer <- shiny::reactiveVal(NULL)
  fn_preset <- shiny::reactiveVal(NULL)
  shiny::observe({
    st_drawn()
    v <- input[[st_id("fn_pick")]]
    if (!is.null(v) && nzchar(v)) st_method(v)
  })
  # the row as the form has it now: its method may be changed there
  st_row_now <- function() {
    r <- shiny::isolate(st_row())
    m <- st_method()
    if (!is.na(m) && nzchar(m)) r$method <- m
    r
  }
  # the drawing of the form whose function list was opened ("Change")
  fn_list_open <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$ard_fn_opened, {
    fn_list_open(shiny::isolate(st_drawn()))
  })
  # the functions an analysis can name, the chosen one kept.  Made once a
  # session for each method chosen (tflspec's catalog is read from disk,
  # and a click called this for each analysis in the outline): made again
  # when the study's own functions change
  fn_memo <- new.env(parent = emptyenv())
  fn_entries <- function(current) {
    own <- tryCatch(own_data(), error = function(e) NULL)
    if (!identical(own, fn_memo$own)) {
      rm(list = ls(fn_memo), envir = fn_memo)
      fn_memo$own <- own
    }
    key <- paste0("m:", if (length(current) && !is.na(current[1L])) current[1L] else "")
    if (is.null(fn_memo[[key]])) fn_memo[[key]] <- fn_entries_make(current, own)
    fn_memo[[key]]
  }
  fn_entries_make <- function(current, own) {
    e <- .ard_fn_entries(.company_keywords(current), tflspec::tfl_ard_functions(),
                         current = current, company = "Company standard")
    k <- e$category == "Company standard"
    e$label[k] <- method_label(e$value[k], e$label[k])
    e$description[k] <- method_note(e$value[k], e$description[k])
    e$label[!k] <- vapply(e$value[!k], function(v) {
      x <- t(paste0("fn:", v))
      if (identical(x, paste0("fn:", v))) e$label[e$value == v][1L] else x
    }, "")
    e$description[!k] <- vapply(e$value[!k], function(v) {
      x <- t(paste0("fn-note:", v))
      if (identical(x, paste0("fn-note:", v))) e$description[e$value == v][1L] else x
    }, "")
    # subjects and custom code (no function to name): with the functions
    # like them, after those
    home <- c(subjects = "Subjects and attributes", custom = "Own and code")
    mv <- k & e$value %in% names(home)
    e$category[mv] <- home[e$value[mv]]
    e <- rbind(e[!mv, , drop = FALSE], e[mv, , drop = FALSE])
    # the study's own functions (its title and description from its file),
    # and the company's it does not load yet (offered from Own functions)
    if (!is.null(own) && nrow(own)) {
      add <- data.frame(
        value = own$name,
        label = ifelse(is.na(own$title), own$name, paste0(own$title, " (", own$name, ")")),
        label_en = own$name,
        description = paste0("[", t(unname(own_where_words[own$where])), "] ",
                             ifelse(is.na(own$description), "", own$description)),
        category = "Own and code", call = own$name,
        state = ifelse(own$loaded, "ok", "own_off"), stringsAsFactors = FALSE)
      e <- e[!e$value %in% add$value, , drop = FALSE]
      e <- rbind(e, add[names(e)])
    }
    e
  }
  # the search's dictionary, with the company's keywords and the old names
  # (and the company's own words, the standards' sheet ard_fn_keywords)
  fn_keywords <- function() {
    m <- .std_ard_methods()
    co <- .fn_company_dict(company_standards()$ard_fn_keywords)
    k <- paste(c(m$method, m$call, unlist(co)), collapse = "|")
    if (!identical(ard_cols[["fn_keywords_key"]], k)) {
      ard_cols[["fn_keywords"]] <- .fn_keywords(m, tflspec::tfl_ard_functions(),
                                                dict = rbind(.fn_dict(), co))
      ard_cols[["fn_keywords_key"]] <- k
    }
    ard_cols[["fn_keywords"]]
  }
  # a dataset's data (for the choices), read once per file version
  an_data <- function(ds) {
    d <- rv$p$ard$datasets
    pth <- d$path[match(ds, d$dataset)]
    if (!length(pth) || is.na(pth)) return(NULL)
    f <- .data_file(rv$study$path, pth)
    if (!file.exists(f)) return(NULL)
    k <- paste("form", f, file.mtime(f))
    if (is.null(ard_cols[[k]])) {
      ard_cols[[k]] <- tryCatch(read_data_head(f, 5000L),
                                error = function(e) FALSE)
    }
    if (isFALSE(ard_cols[[k]])) NULL else ard_cols[[k]]
  }
  # the data an analysis reads: its own, else its analysis set's
  an_dataset <- function(ds, pop) {
    if (!is.null(ds) && !is.na(ds) && nzchar(ds)) return(ds)
    po <- rv$p$ard$populations
    po$dataset[match(pop %||% NA_character_, po$population_id)]
  }
  # what a percentage may be of: the method's own default, the analysis
  # set, cards' row / column / cell, another population, a dataset
  den_choices <- function(now = "") {
    p <- rv$p$ard
    ch <- c(stats::setNames("", t("the method's default")),
            stats::setNames("population", t("the population")),
            stats::setNames("row", t("within a row")),
            stats::setNames("column", t("within a column")),
            stats::setNames("cell", t("of the whole table")))
    more <- c(p$populations$population_id, p$datasets$dataset)
    more <- unique(c(more[!is.na(more)], setdiff(now, c(ch, ""))))
    ad <- .adata_rows(rv$p, ard_target())$data_id
    more <- setdiff(more, ad)
    n <- tryCatch(shiny::isolate(data_counts()), error = function(e) NULL)
    an <- tryCatch(shiny::isolate(adata_counts()), error = function(e) NULL)
    lab <- function(v, kind, cnt) {
      k <- if (!is.null(cnt) && !is.na(cnt)) paste0(kind, ": ", cnt) else kind
      stats::setNames(v, sprintf(t("%s (%s)"), v, k))
    }
    pops <- intersect(more, p$populations$population_id)
    dss <- setdiff(more, pops)
    out <- list()
    out[[t("The method's own, or cards'")]] <- ch
    if (length(pops)) out[[t("Analysis sets (their subjects)")]] <- unlist(lapply(pops, function(v)
      lab(v, t("population"), n[.an_data_value(NA, v)])))
    if (length(dss)) out[[t("Datasets")]] <- unlist(lapply(dss, function(v)
      lab(v, t("dataset"), n[.an_data_value(v, NA)])))
    if (length(ad)) out[[t("Analysis data")]] <- unlist(lapply(ad, function(v)
      lab(v, t("analysis data"), an[v])))
    out
  }
  output$ard_stat_ui <- shiny::renderUI({
    a <- st_rows()
    # none chosen: nothing (the list above says a click opens one here)
    if (is.null(a) || !nrow(a) || is.null(an_pick()) || !an_pick() %in% a$analysis_id) {
      return(NULL)
    }
    r <- st_row()
    rv$ver
    st_env$n <- st_env$n + 1L
    st_drawn(st_env$n)
    st_method(r$method)
    e <- fn_entries(r$method)
    # inside a stack: only what can run inside one (no category left empty)
    if (identical(st_role(r), "inside")) e <- e[!e$value %in% .stack_not_inside, , drop = FALSE]
    cats <- unique(e$category)
    blank_na <- function(x) if (is.na(x)) "" else x
    tg <- ard_target()
    # a stack's own form; one inside a stack says so, and has no data of
    # its own
    role <- st_role(r)
    if (role == "parent") return(stack_form_ui(r, tg))
    inside <- role == "inside"
    shiny::tagList(
      if (inside) stack_inside_note(r),
      if (!tg %in% rv$p$outputs$output_id) shiny::div(
        class = "alert alert-info py-1 small",
        sprintf(t("%s is not a report yet: add it to the Report list to make its table."), tg)),
      shiny::h6(class = "mb-1", `data-an` = r$analysis_id, with_tip(
        sprintf(t("Analysis %s"), r$analysis_id),
        t("One analysis is one call: add variables to it. Make another analysis only when the statistics, the condition or the groups differ."))),
      # what to compute, first and as a heading (the analysis's most
      # important line): its name and the function it calls; "Change" opens
      # the list (a category and a search, both narrowing it), a pick
      # closes it again (the script in app_ui())
      shiny::tags$details(
        class = "ard-fn mb-2",
        open = if (is.na(r$method) || !nzchar(r$method)) NA,
        shiny::tags$summary(
          class = "d-flex flex-wrap align-items-baseline column-gap-2 mb-1",
          shiny::strong(class = "small text-muted fw-normal", title = t("What to compute"),
                        argl("The method", "method")),
          shiny::uiOutput("ard_fn_now", inline = TRUE),
          shiny::span(class = "ard-fn-closed small link-primary", t("Change")),
          shiny::span(class = "ard-fn-open small link-secondary", t("Close"))),
        shiny::div(
          class = "d-flex flex-wrap align-items-center column-gap-2",
          shiny::div(style = "width: 14rem",
                     shiny::selectInput(st_id("fn_cat"), NULL, selectize = FALSE,
                                        c(stats::setNames(".all", t("All categories")),
                                          stats::setNames(cats, t(cats))),
                                        selected = ".all", width = "100%")),
          shiny::div(
            class = "d-flex align-items-center gap-1 flex-grow-1",
            style = "min-width: 16rem; max-width: 26rem",
            shiny::textInput(st_id("fn_q"), NULL, "", width = "100%",
                             placeholder = t("e.g. odds ratio, PROC LOGISTIC, paired t test")),
            # clears the search
            shiny::tags$button(
              type = "button", class = "btn btn-sm btn-link text-muted px-1 mb-3",
              title = t("Clear"), `aria-label` = t("Clear"),
              onclick = sprintf(paste0("var i = document.getElementById('%s');",
                                       " i.value = ''; $(i).trigger('change');"),
                                st_id("fn_q")),
              "\u00d7"))),
        shiny::div(class = "ard-fn-list",
                   style = paste("max-height: 24rem; overflow-y: auto;",
                                 "overflow-x: hidden; white-space: normal;",
                                 "overflow-wrap: anywhere;"),
                   shiny::uiOutput("ard_fn_list"))),
      shiny::uiOutput("ard_fn_preset"),
      shiny::uiOutput("ard_method_note"),
      bslib::layout_columns(
        col_widths = c(4, 8),
        shiny::textInput(st_id("id"), with_tip(t("Analysis ID (a set of analyses)"), t("Several variables: in the ARD, one row per variable under the same ID.")), r$analysis_id),
        shiny::textInput(st_id("label"), t("Label"), blank_na(r$label),
                         width = "100%")),
      if (!inside) shiny::selectInput(st_id("data"), with_tip(t("Data"), t("The rows the analysis reads, chosen from the definitions: a dataset (Data tab) kept to an analysis set (Populations tab), or an analysis data of 1-1 (the same, with more done to it). The population is part of the choice. The name is the one the program gives it.")),
                                      data_choices(r),
                                      selected = .an_data_value_row(r),
                                      width = "100%"),
      shiny::uiOutput("ard_an_vars"),
      shiny::uiOutput("ard_an_args"),
      if (identical(r$method, "custom")) shiny::tagList(
        shiny::tags$label(class = "form-label", argl("The code (R: `data` and `population` are the analysis's data and population)", "code")),
        shiny::textAreaInput(st_id("code"), NULL, blank_na(r$code), width = "100%", rows = 8,
                             resize = "vertical")),
      if (!inside) shiny::tags$details(
        class = "mb-2", open = if (!is.na(r$where)) NA,
        shiny::tags$summary(class = "small", with_tip(argl("This analysis's own filter", "where"),
                                                      t("An R condition that filters this analysis's rows alone (e.g. AESER == \"Y\"). The data's own filter is 1-1's."))),
        shiny::textInput(st_id("where"), NULL, blank_na(r$where), width = "100%",
                         placeholder = "AESER == \"Y\""),
        .where_labels_note(sheet_rows(shiny::isolate(rv$p), "codelists",
                                      shiny::isolate(current()) %||% ""), t)),
      shiny::uiOutput("ard_stat_part"),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center",
        .btn("ard_stat_apply", t("Apply to the analysis"),
             class = "btn-sm btn-primary"),
        if (role == "single" && !r$method %in% .stack_not_inside)
          .btn("ard_stack_group", t("Run together with other analyses (one call)..."),
               class = "btn-sm btn-outline-secondary"),
        if (role == "single" && !identical(r$method, "custom"))
          .btn("ard_an_as_code", t("Write this analysis as code..."),
               class = "btn-sm btn-outline-secondary")),
      shiny::tags$details(
        class = "mt-2", open = NA,
        shiny::tags$summary(class = "small", t("This analysis as code")),
        shiny::div(class = "rp-code", shiny::verbatimTextOutput("ard_an_code"))))
  })
  # this analysis alone as code (the data it reads, its analysis set, the
  # call), as the definition has it now -- the whole program is on the right
  output$ard_an_code <- shiny::renderText({
    r <- st_row()
    a <- rv$p$ard
    # a stack is one call with the analyses inside it: one inside shows it
    mine <- a$analyses[!is.na(a$analyses$output_id) & a$analyses$output_id == r$output_id, ,
                       drop = FALSE]
    top <- switch(st_role(r, mine), parent = r$analysis_id, inside = r$parent, NA)
    ids <- if (is.na(top)) r$analysis_id else
      c(top, mine$analysis_id[.stack_kids(mine, top)])
    a$analyses <- mine[mine$analysis_id %in% ids, , drop = FALSE]
    code <- tryCatch(tflspec::tfl_ard_code(structure(a, class = "tfl_ard_spec"),
                                           part = "body"),
                     error = function(e) paste(t("The code cannot be written yet:"),
                                               conditionMessage(e)))
    paste(code, collapse = "\n")
  })
  output$ard_fn_now <- shiny::renderUI({
    st_drawn()
    m <- st_method()
    if (is.na(m) || !nzchar(m)) return(shiny::span(class = "text-muted", t("(not chosen)")))
    e <- fn_entries(m)
    k <- match(m, e$value)
    shiny::span(
      shiny::span(class = "fs-5 fw-semibold", if (is.na(k)) m else e$label[k]),
      fn_code(m, "small text-body-secondary ms-1"))
  })
  output$ard_fn_list <- shiny::renderUI({
    n <- st_drawn()
    now <- shiny::isolate(st_method())
    # folded until "Change": made when it is opened, not with every form
    # (its hundred rows were most of the drawing of a form); a search or a
    # category chosen (both inside it) says it is open too
    if (nzchar(input[[st_id("fn_q")]] %||% "") ||
        !identical(input[[st_id("fn_cat")]] %||% ".all", ".all")) {
      shiny::isolate(fn_list_open(n))
    }
    if (!.is_blank(now) && !identical(fn_list_open(), n)) {
      return(shiny::div(class = "small text-muted", t("Loading ...")))
    }
    e <- fn_entries(now)
    # inside a stack: no subjects count, own code or other stacks
    if (identical(st_role(shiny::isolate(st_row())), "inside")) {
      e <- e[!e$value %in% .stack_not_inside, , drop = FALSE]
    }
    # a search: across the categories, by the dictionary's words too
    # (R/ard_fn_search.R), each row saying why it was found
    q <- input[[st_id("fn_q")]] %||% ""
    # the functions of one's own by their files' tflplanner-keywords lines
    own_kw <- .fn_own_dict(tryCatch(own_data(), error = function(e) NULL))
    s <- .fn_search(q, e, rbind(fn_keywords(), own_kw), lang = lang, current = now)
    if (!is.null(s)) e <- s
    # and a category (all by default)
    cat_now <- input[[st_id("fn_cat")]] %||% ".all"
    if (!identical(cat_now, ".all")) e <- e[e$category == cat_now, , drop = FALSE]
    if (!nrow(e)) return(shiny::div(
      class = "ard-fn-searching small text-muted",
      shiny::p(class = "mb-1", t("No function matches.")),
      shiny::p(t("Make it as an own function (Study > Own functions), or write it as R code (custom)."))))
    searching <- !is.null(s)
    # the settings the words found mean, for ard_fn_preset
    if (searching) {
      a <- s$args[!is.na(s$args)]
      fn_offer(list(n = shiny::isolate(st_drawn()),
                    args = as.list(stats::setNames(a, s$value[!is.na(s$args)]))))
    }
    # each row's category, unless one is chosen
    show_cat <- identical(cat_now, ".all")
    near_spelling <- searching & e$tier >= 4
    ok <- e[e$state %in% c("ok", "old", "out"), , drop = FALSE]
    off <- e[!e$state %in% c("ok", "old", "out"), , drop = FALSE]
    fn_of <- function(v) if (grepl("::", v, fixed = TRUE))
      paste0(" (", sub("^.*::", "", v), ")") else ""
    # why a row was found: the keywords and their note; a near one says so
    why <- function(i, d) {
      if (!searching) return(NULL)
      txt <- if (!is.na(d$near_from[i])) {
        sprintf(t("Near spelling: %s -> %s"), d$near_from[i], d$near_to[i])
      } else if (!is.na(d$hit[i])) {
        sprintf(t("Match: %s"), d$hit[i])
      }
      if (!is.na(d$note[i])) txt <- paste0(txt %||% "", if (!is.null(txt)) " \u2014 ", d$note[i])
      if (identical(d$rank[i], 3L)) txt <- paste0(t("Near: "), txt %||% "")
      shiny::tagList(
        if (!is.null(txt)) shiny::div(class = "small text-info-emphasis", txt),
        if (!is.na(d$args[i] %||% NA)) shiny::div(
          class = "small text-muted", sprintf(t("Its setting: %s"), d$args[i])))
    }
    item <- function(i, d) shiny::tagList(
      shiny::span(class = if (searching && identical(d$rank[i], 3L)) "ard-fn-near",
                  paste0(d$label[i], fn_of(d$call[i]))),
      # where it is, for the next time (from the categories)
      if (show_cat) shiny::span(class = "badge text-bg-light fw-normal ms-1",
                                 t(d$category[i])),
      if (!is.na(d$description[i]) && nzchar(d$description[i]))
        shiny::div(class = "small text-muted", d$description[i]),
      why(i, d),
      if (identical(d$state[i], "old"))
        shiny::div(class = "small text-warning", t("An old name: choose its new one.")),
      if (identical(d$state[i], "out"))
        shiny::div(class = "small text-warning", t("Not offered by the builder: kept as written.")))
    shiny::tagList(
      if (searching) shiny::div(
        class = "ard-fn-searching small text-muted mb-1",
        sprintf(t("%d found"), nrow(e)),
        if (all(near_spelling)) paste0(" \u2014 ", t("Near spellings"))),
      if (nrow(ok)) shiny::radioButtons(
        st_id("fn_pick"), NULL, width = "100%",
        choiceNames = lapply(seq_len(nrow(ok)), item, d = ok),
        choiceValues = ok$value,
        selected = if (now %in% ok$value) now else character(0)),
      lapply(seq_len(nrow(off)), function(i) shiny::div(
        class = "small text-body-tertiary ms-4 mb-1",
        paste0(off$label[i], fn_of(off$call[i])), " -- ",
        switch(off$state[i],
               missing = t("its package is not installed"),
               own_off = t("not loaded by this study: Use in this study, on Own functions"),
               t("in preparation: a function that runs others")))))
  })
  # a function found by a word that means a setting: start from it (the
  # argument fields filled in; Apply writes them)
  output$ard_fn_preset <- shiny::renderUI({
    n <- st_drawn()
    m <- st_method()
    o <- fn_offer()
    a <- if (!is.null(o) && identical(o$n, n) && !is.na(m)) o$args[[m]]
    if (is.null(a)) return(NULL)
    # the analysis's own arguments for this function, which the setting
    # replaces (not added to): said before and after
    row <- shiny::isolate(st_row())
    own <- if (identical(row$method, m) && !.is_blank(row$args)) row$args
    replaces <- if (!is.null(own)) shiny::span(
      class = "text-warning-emphasis",
      sprintf(t("It replaces the analysis's own arguments: %s"), own))
    ps <- fn_preset()
    if (!is.null(ps) && identical(ps$n, n) && identical(ps$method, m)) {
      return(shiny::p(class = "small text-muted mt-n1 mb-2",
                      sprintf(t("Started from the setting: %s (Apply to the analysis writes it)."), a),
                      if (!is.null(replaces)) shiny::tagList(shiny::br(), replaces)))
    }
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mt-n1 mb-2 small",
      shiny::span(sprintf(t("The word searched means: %s"), a)),
      .btn("ard_fn_preset_go", t("Start with this setting"),
           class = "btn-sm btn-outline-primary py-0"),
      replaces)
  })
  shiny::observeEvent(input$ard_fn_preset_go, {
    o <- fn_offer()
    m <- st_method()
    shiny::req(o, !is.na(m), o$args[[m]])
    fn_preset(list(n = st_drawn(), method = m, args = o$args[[m]]))
  })
  output$ard_method_note <- shiny::renderUI({
    st_drawn()
    m <- .std_ard_methods()
    k <- match(st_method() %||% "", m$method)
    if (is.na(k) || !nzchar(m$note[k])) return(NULL)
    shiny::p(class = "small text-muted mt-n2 mb-2",
             method_note(m$method[k], m$note[k]),
             shiny::span(class = "ms-1 text-body-tertiary",
                         paste0("(method: ", m$method[k], ")")))
  })
  # the chosen function's own arguments (those written to `args`): a field
  # each, its default shown faint; what the fields cannot hold stays as R
  output$ard_an_args <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    call <- .ard_method_call(r$method, .std_ard_methods())
    f <- .ard_form_fields(call)
    row <- shiny::isolate(st_row())
    # the row's own args fill the fields when the function is the row's
    # ... or the setting a search's word means, when chosen
    ps <- fn_preset()
    from <- if (!is.null(ps) && identical(ps$n, st_drawn()) && identical(ps$method, r$method)) {
      ps$args
    } else if (identical(r$method, row$method)) row$args else NA
    pa <- .ard_args_parse(from, f)
    other <- pa$other
    # inside a stack: the parent's data
    if (identical(st_role(row), "inside")) {
      pr <- st_parent_of(row)
      r$dataset <- pr$dataset
      r$population_id <- pr$population_id
      r$data <- pr$data %||% NA_character_
    }
    if (is.null(f) || !nrow(f)) {
      return(shiny::tags$details(
        class = "mb-2", open = if (nzchar(other)) NA,
        shiny::tags$summary(class = "small", with_tip(argl("Other arguments (R)", "args"), t("cards' arguments, in R (e.g. conf.level = 0.9)."))),
        shiny::textInput(st_id("args_other"), NULL, other, width = "100%")))
    }
    fd <- if (identical(st_role(row), "inside"))
      .an_data_split(.an_data_value_row(r), .adata_rows(rv$p, ard_target())) else form_data(r)
    ds <- an_dataset(fd$dataset, fd$pop)
    d <- if (!is.na(ds %||% NA)) an_data(ds)
    cols <- if (!is.null(d)) names(d) else character()
    vars <- .split_bar(row$variables)
    field <- function(i) {
      a <- f$arg[i]
      id <- st_id(paste0("a_", a))
      v <- pa$values[[a]]
      dflt <- f$default[i]
      hint <- if (!is.na(f$hint[i]) && nzchar(f$hint[i])) {
        # the catalog's hint in the app's language ("arg-hint:<English>")
        k <- paste0("arg-hint:", f$hint[i])
        h <- t(k)
        if (identical(h, k)) f$hint[i] else h
      } else sprintf(t("See ?%s for this argument."), call)
      lab <- with_hint(paste0(a, if (isTRUE(f$required[i])) " *" else ""), hint)
      ph <- if (!is.na(dflt) && nchar(dflt) <= 40) dflt else ""
      ch <- .split_bar(f$choices[i])
      # blank: what the program writes for it (the study's subject key, the
      # company method's), else the function's own default -- said which
      own <- st_arg_default(r, a)
      dflt_lab <- if (!is.null(own) && identical(own, "<id>")) {
        sprintf(t("(default: %s, the study's subject key)"), .adata_subject_key(rv$p))
      } else if (!is.null(own)) {
        sprintf(t("(default: %s, the company's method)"), own)
      } else {
        .ard_default_label(f$kind[i], dflt, ch,
                           if (is.na(dflt) || identical(dflt, "NULL")) t("(default: none)") else
                             sprintf(t("(default: see ?%s)"), sub("^.*::", "", call)),
                           t("(default: %s)"))
      }
      w <- switch(f$kind[i],
        choice = {
          shiny::selectInput(id, lab, c(stats::setNames("", dflt_lab),
                                        unique(c(ch, v))),
                             selected = v %||% "", width = "100%")
        },
        logical = shiny::radioButtons(
          id, lab, inline = TRUE,
          c(stats::setNames("", t("default")), "TRUE", "FALSE"),
          selected = v %||% ""),
        column = shiny::selectInput(id, lab,
                                    c(stats::setNames("", dflt_lab),
                                      unique(c(v, cols))),
                                    selected = v %||% "", width = "100%"),
        columns = shiny::selectizeInput(id, lab, unique(c(v, cols)),
                                        selected = v, multiple = TRUE,
                                        width = "100%"),
        levels = shiny::selectizeInput(
          id, lab, c(stats::setNames("", dflt_lab),
                     unique(c(v, .level_choices(vars, sheet_rows(rv$p, "codelists", current() %||% NA), d)))),
          selected = v %||% "", multiple = FALSE, width = "100%",
          options = list(create = TRUE)),
        formula = shiny::div(
          shiny::textInput(id, lab, v %||% "", width = "100%",
                           placeholder = if (nzchar(ph)) ph else "AVAL ~ TRTA + BASE"),
          # written from columns: the response, then the terms (the
          # analysis's groups first)
          shiny::tags$details(
            class = "mt-n2 mb-1",
            shiny::tags$summary(class = "small", t("Write it from columns")),
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-end",
              shiny::selectInput(st_id(paste0("fb_resp_", a)), t("Response"),
                                 c(stats::setNames("", t("(choose)")), cols), width = "12em"),
              shiny::selectizeInput(st_id(paste0("fb_terms_", a)), t("Terms"), cols,
                                    selected = intersect(.split_bar(row$by), cols),
                                    multiple = TRUE, width = "18em",
                                    options = list(plugins = list("remove_button", "drag_drop"))),
              shiny::checkboxInput(st_id(paste0("fb_int_", a)), t("Interaction of the first two"), FALSE),
              .btn(st_id(paste0("fb_go_", a)), t("Write the formula"),
                   class = "btn-sm btn-outline-secondary mb-3")))),
        # a regression's fitting function: the usual ones to choose
        if (identical(a, "method") && call %in% .regression_calls) shiny::selectInput(
          id, lab, unique(c(stats::setNames("", t("(default)")), .regression_methods, v)),
          selected = v %||% "", width = "100%")
        else shiny::textInput(id, lab, v %||% "", width = "100%", placeholder = ph))
      shiny::div(w)
    }
    # the ones to fill or filled first; those at their defaults folded
    given <- vapply(f$arg, function(a) length(pa$values[[a]]) > 0L &&
                      any(nzchar(pa$values[[a]])), NA)
    written <- vapply(f$arg, function(a) !is.null(st_arg_default(r, a)), NA)
    top <- c(which(f$required %in% TRUE & !written), which(written),
             which(given & !written & !f$required %in% TRUE))
    top <- unique(top)
    rest <- setdiff(seq_len(nrow(f)), top)
    cols2 <- function(i) do.call(bslib::layout_columns,
                                 c(list(col_widths = if (length(i) > 1L) c(6, 6) else 12),
                                   lapply(unname(i), field)))
    shiny::div(
      class = "mb-2",
      shiny::h6(class = "small fw-bold", with_tip(
        sprintf(t("Only for %s"), sub("^.*::", "", call)), t("This function's own arguments. The data, groups, variables, strata, denominator and condition above are its arguments too: those every method has."))),
      if (length(top)) cols2(top),
      if (length(rest)) shiny::tags$details(
        class = "mb-2", open = if (!length(top)) NA,
        shiny::tags$summary(class = "small", sprintf(t("%d more, at their defaults"), length(rest))),
        cols2(rest)),
      shiny::tags$details(
        class = "mb-2", open = if (nzchar(other)) NA,
        shiny::tags$summary(class = "small", with_tip(argl("Other arguments (R)", "args"), t("cards' arguments, in R (e.g. conf.level = 0.9)."))),
        shiny::textInput(st_id("args_other"), NULL, other, width = "100%",
                         placeholder = "weights = W")))
  })
  .srv_keep(environment())
}
