# app_server(): toc -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_toc <- function() {
  # a row: its report in the sidebar, its analysis on the ARD definition
  shiny::observeEvent(input$ard_conds_rows_selected, {
    d <- ard_conds_shown()
    i <- input$ard_conds_rows_selected
    if (!length(i) || i > nrow(d)) return()
    id <- d$output_id[i]
    if (!is.na(id) && id %in% output_ids(rv$p)) {
      shiny::updateSelectInput(session, "target", selected = id)
    }
    if (!is.na(d$analysis_id[i])) an_pick(d$analysis_id[i])
    bslib::nav_select("ard_sheet", "analyses")
  })

  # ---- a TOC taken in ------------------------------------------------------
  # One dialog for the first time and every time after: the file, which of
  # its columns is what (from the company's toc_map), what would change
  # (lines edited here and changed in the TOC asked about one by one), and
  # taking it in -- saved at once, with the TOC's copy and record in
  # input/toc (the record and the reports must agree, as for an ARD)
  toc_n <- shiny::reactiveVal(0L)
  toc_id <- function(x) paste0("toc", toc_n(), "_", x)
  toc_done <- shiny::reactiveVal(FALSE)
  toc_btn_ver <- shiny::reactiveVal(0L)   # redraws the button after a refusal
  shiny::observeEvent(input$toc_new, {
    shiny::req(has_study())
    toc_n(toc_n() + 1L)
    toc_done(FALSE)
    n <- toc_n()
    shiny::showModal(shiny::modalDialog(
      title = t("Take in a TOC"), size = "xl",
      shiny::uiOutput(toc_id("prev")),
      shiny::fileInput(toc_id("file"), t("TOC files (xlsx, xls, csv): the TFL workbook, and the listing workbook when it is another"),
                       accept = c(".xlsx", ".xls", ".csv"), multiple = TRUE, width = "100%"),
      shiny::uiOutput(toc_id("same_file")),
      shiny::uiOutput(toc_id("where")),
      shiny::uiOutput(toc_id("map")),
      shiny::uiOutput(toc_id("preview")),
      shiny::uiOutput(toc_id("changes")),
      shiny::uiOutput(toc_id("result")),
      footer = shiny::tagList(shiny::modalButton(t("Close")),
                              shiny::uiOutput(toc_id("do_btn"), inline = TRUE))))
    output[[paste0("toc", n, "_prev")]] <- shiny::renderUI(toc_prev_ui())
    output[[paste0("toc", n, "_same_file")]] <- shiny::renderUI(toc_same_file_ui())
    output[[paste0("toc", n, "_where")]] <- shiny::renderUI(toc_where_ui())
    output[[paste0("toc", n, "_map")]] <- shiny::renderUI(toc_map_ui())
    output[[paste0("toc", n, "_preview")]] <- shiny::renderUI(toc_preview_ui())
    output[[paste0("toc", n, "_changes")]] <- shiny::renderUI({
      parts <- list(toc_pop_ui(), toc_data_ui(), toc_changes_ui())
      # taken in: nothing (not an empty list)
      if (all(vapply(parts, is.null, NA))) NULL else shiny::tagList(parts)
    })
    output[[paste0("toc", n, "_result")]] <- shiny::renderUI(NULL)
    output[[paste0("toc", n, "_do_btn")]] <- shiny::renderUI(toc_do_btn())
  })
  # the last TOC taken in, said above the file (the same file again is said
  # when it is chosen)
  toc_log <- function() toc_imports(rv$study)
  toc_prev_ui <- function() {
    log <- toc_log()
    if (!nrow(log)) return(NULL)
    r <- log[nrow(log), ]
    shiny::p(class = "small text-muted",
             sprintf(t("Last taken in: %s (%s, %s)."), r$import_id, r$original,
                     substr(r$imported, 1L, 16L)))
  }
  toc_same_file_ui <- function() {
    p <- toc_path()
    log <- toc_log()
    if (!nrow(log)) return(NULL)
    md5 <- unname(tools::md5sum(p))
    # an import of several files keeps their md5s " | " between them
    i <- which(vapply(strsplit(log$md5, " | ", fixed = TRUE), function(v) md5 %in% v, NA))
    if (!length(i)) return(NULL)
    i <- i[length(i)]
    shiny::div(class = "alert alert-warning py-1 small",
               sprintf(t("This is the same file as %s (%s), taken in before."),
                       log$import_id[i], log$original[i]))
  }
  # Take it in: off while the TOC cannot be read; once pressed it says so
  # and cannot be pressed again (taking in saves and rewrites the programs)
  toc_do_btn <- function() {
    toc_btn_ver()
    if (toc_done()) return(NULL)
    ok <- tryCatch(!inherits(toc_ch(), "error"), error = function(e) FALSE)
    busy <- sprintf(
      "var b = this; setTimeout(function () { b.disabled = true; b.textContent = %s; }, 0);",
      jsonlite::toJSON(t("Taking it in..."), auto_unbox = TRUE))
    args <- list(toc_id("do"), t("Take it in"), class = "btn-primary", onclick = busy)
    if (!ok) args$disabled <- TRUE
    do.call(.btn, args)
  }
  # the files under the names they were chosen by (the record keeps those
  # names): the TFL workbook, and the listing workbook when it is another
  toc_paths <- shiny::reactive({
    f <- input[[toc_id("file")]]
    shiny::req(f)
    d <- tempfile("toc")
    dir.create(d)
    p <- file.path(d, f$name)
    file.copy(f$datapath, p, overwrite = TRUE)
    p
  })
  toc_path <- function() toc_paths()[1L]
  toc_is_csv <- function(p) tolower(tools::file_ext(p)) == "csv"
  # the company's rule sets (#299) and the study's profile (what the last
  # import chose)
  toc_rules_now <- shiny::reactive({
    toc_n()   # read again for each dialog: "remember" may have changed them
    toc_rules()
  })
  toc_profile_now <- shiny::reactive({
    rv$ver
    tryCatch(toc_profile(rv$study), error = function(e) NULL)
  })
  # each file read once while the dialog is open
  toc_srcs <- shiny::reactive({
    ps <- toc_paths()
    hr <- max(vapply(toc_rules_now(), function(s) s$settings$header_rows, 1L))
    lapply(ps, function(p) toc_source(p, hr))
  })
  # what is found with no choice made (what the dialog starts from, and the
  # reasons it shows) and what the choices made of it
  toc_found <- shiny::reactive({
    tryCatch(toc_import_read(toc_srcs(), toc_rules_now(), profile = toc_profile_now()),
             error = function(e) e)
  })
  toc_where_id <- function(what, k) if (k == 1L) toc_id(what) else toc_id(paste0(what, "_", k))
  toc_choices <- shiny::reactive({
    fd <- toc_found()
    if (inherits(fd, "error")) return(NULL)
    det <- fd$detection
    ch <- list()
    for (k in seq_len(nrow(det))) {
      nm <- det$file[k]
      sh <- input[[toc_where_id("sheet", k)]]
      if (!.is_blank(sh) && !identical(sh, det$sheet[k])) ch$sheet[[nm]] <- sh
      sk <- suppressWarnings(as.integer(input[[toc_where_id("skip", k)]] %||% NA))
      if (!is.na(sk) && sk >= 0L && (!identical(sk + 1L, det$header_row[k]) || !is.null(.toc_pick(ch$sheet, nm)))) {
        ch$header_row[[nm]] <- sk + 1L
      }
    }
    rs <- input[[toc_id("rule_set")]]
    if (!.is_blank(rs) && !identical(rs, fd$rule_set$name)) ch$rule_set <- rs
    ir <- input[[toc_id("id_rule")]]
    if (!.is_blank(ir) && !identical(ir, fd$id_rule)) ch$id_rule <- ir
    items <- c(.toc_items, .toc_extra_items)
    m <- lapply(stats::setNames(items, items), function(it) input[[toc_id(paste0("map_", it))]])
    m <- m[!vapply(m, is.null, NA)]
    if (length(m)) ch$map <- lapply(m, function(v) v[!is.na(v) & nzchar(v)])
    ov <- list()
    for (k in c("number_suffix", "title_suffix", "title_line")) {
      v <- input[[toc_id(paste0("ov_", k))]]
      if (!is.null(v) && !identical(v, fd$rule_set$settings[[k]])) ov[[k]] <- v
    }
    if (length(ov)) ch$overrides <- ov
    ch
  })
  toc_result <- shiny::reactive({
    fd <- toc_found()
    ch <- toc_choices()
    if (inherits(fd, "error") || !length(ch)) return(fd)
    tryCatch(toc_import_read(toc_srcs(), toc_rules_now(), profile = toc_profile_now(),
                             choices = ch), error = function(e) e)
  })
  toc_why <- function(how, n_cols) {
    switch(how,
           name = t("found by its name"),
           score = sprintf(t("most column names matched (%d)"), n_cols),
           rows = t("most rows of the sheets that look like the TOC"),
           profile = t("as the last time"),
           chosen = t("chosen here"),
           "")
  }
  # the sources: each file's TOC sheet and header row as found (why), each
  # can be changed; the shells and the Topline sheet said
  toc_where_ui <- function() {
    srcs <- toc_srcs()
    fd <- toc_found()
    if (inherits(fd, "error")) {
      return(shiny::div(class = "alert alert-danger py-1 small",
                        sprintf(t("The file cannot be read: %s"), conditionMessage(fd))))
    }
    det <- fd$detection
    shiny::tagList(
      shiny::h6(t("Sources")),
      lapply(seq_along(srcs), function(k) {
        s <- srcs[[k]]
        d <- det[k, ]
        fs <- fd$sheets[[s$name]]
        n_cols <- if (!is.na(d$sheet)) fs$score[match(d$sheet, fs$sheet)] else 0L
        line <- if (d$kind %in% c("other", "none") || (d$kind == "shells" && is.na(d$sheet))) {
          shiny::span(class = "text-warning", if (d$kind == "shells") {
            t("No TOC sheet: shells only. Their links are made from the TOC's file.")
          } else t("Not a table this app can read by rules: take it in from a TOC workbook, or draft one from this document with an AI chat (Assist)."))
        } else {
          shiny::tagList(
            sprintf(t("TOC: %s (header on row %d), %d rows"), dQuote(d$sheet, FALSE),
                    d$header_row, d$rows),
            " \u2014 ", shiny::span(class = "text-muted", toc_why(d$how, n_cols)))
        }
        extra <- c(if (d$shells) sprintf(t("shells: %d sheets"), d$shells),
                   if (!is.na(d$topline_sheet)) sprintf(t("Topline sheet: %s (a cross-check)"),
                                                        dQuote(d$topline_sheet, FALSE)))
        shiny::div(
          class = "mb-2",
          shiny::div(class = "small", shiny::span(class = "fw-bold", s$name), " ", line,
                     if (length(extra)) shiny::span(class = "text-muted",
                                                    paste0(" \u00b7 ", extra, collapse = ""))),
          if (s$kind != "other") shiny::div(
            class = "d-flex flex-wrap gap-3",
            if (!toc_is_csv(s$path) && length(s$sheets)) {
              shiny::selectInput(toc_where_id("sheet", k), t("Sheet"), s$sheets,
                                 selected = if (!is.na(d$sheet)) d$sheet else s$sheets[1L])
            },
            shiny::numericInput(toc_where_id("skip", k), t("Rows above the header"),
                                if (!is.na(d$header_row)) d$header_row - 1L else 0L,
                                min = 0L, step = 1L, width = "12em")))
      }),
      toc_rules_ui(fd))
  }
  # the rule set (scored) and how the report IDs are made (each way with
  # the IDs it gives)
  toc_id_rule_lab <- function() {
    c(column = t("the ID column as it is"),
      normalised = t("the ID column, normalised"),
      type_number = t("type column + number column"),
      title = t("the number in the first title"))
  }
  toc_note_arg <- function(v) {
    lab <- toc_id_rule_lab()
    ifelse(v %in% names(lab), lab[v], v)
  }
  toc_rules_ui <- function(fd) {
    rs <- fd$rule_sets
    if (!nrow(rs)) return(NULL)
    set_lab <- sprintf(t("%s: %d of %d items"), rs$label, rs$matched, rs$named)
    rule_lab <- toc_id_rule_lab()
    ca <- fd$id_candidates
    # of the rows that are not section headings (a heading has no id by
    # any way: it would look like a row lost)
    n_rows <- sum(!(fd$rows$heading %||% FALSE))
    id_ch <- if (nrow(ca)) stats::setNames(ca$rule, sprintf(
      t("%s: %d of %d rows (%s)"), rule_lab[ca$rule], ca$ok, n_rows, ca$preview))
    shiny::div(
      class = "d-flex flex-wrap gap-3",
      shiny::div(style = "min-width: 18rem",
                 shiny::selectInput(toc_id("rule_set"), with_tip(t("Rule set"), t("The company's rule sets (company standards: toc_map, toc_rules), scored on the TOC's column names: the best match is chosen.")),
                                    stats::setNames(rs$rule_set, set_lab),
                                    selected = fd$rule_set$name, width = "100%")),
      if (length(id_ch)) shiny::div(
        style = "min-width: 24rem",
        shiny::selectInput(toc_id("id_rule"), with_tip(t("Report IDs from"), t("The way that gives unique IDs for the most rows is proposed; the labels (Table 14.1.1) come from the label column or the first title.")),
                           id_ch, selected = fd$id_rule, width = "100%")))
  }
  # which column is what: the rule set's map put on the first TOC's header,
  # each one can be changed; the items the set names that the TOC lacks
  toc_map_ui <- function() {
    fd <- toc_found()
    if (inherits(fd, "error")) return(NULL)
    res <- toc_result()
    if (inherits(res, "error")) res <- fd
    det <- res$detection
    k <- which(!is.na(det$sheet))[1L]
    if (is.na(k)) return(NULL)
    h <- .toc_header_names(res$sources[[k]], det$sheet[k], det$header_row[k])
    m <- res$maps[[k]]
    labs <- c(output_id = t("Report ID"), type = t("Type"), title = t("Title lines"),
              population = t("Population"), footnote = t("Footnote lines"),
              program = t("Program"), file = t("File"), note = t("Remarks"),
              section = t("Section (heading)"), datasets = t("Datasets"),
              label = t("Report ID as printed (Table 14.1.1)"),
              phase = t("Phase"), topline = t("Topline flag"), sap_no = t("SAP number"),
              std_shell = t("Standard shell"), reference = t("Reference"),
              shell = t("Shell sheet"), number = t("Number"))
    items <- c(.toc_items, intersect(.toc_extra_items, res$rule_set$map$item))
    choices <- c(stats::setNames("", t("(none)")), stats::setNames(h, h))
    miss <- attr(res$map, "missing") %||% character()
    miss <- intersect(miss, c("phase", "topline", "sap_no"))
    shiny::tagList(
      shiny::h6(t("Which column is what")),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        lapply(items, function(it) {
          many <- it %in% c("title", "footnote")
          shiny::selectizeInput(toc_id(paste0("map_", it)), labs[[it]],
                                if (many) stats::setNames(h, h) else choices,
                                selected = m[[it]] %||% if (!many) "",
                                multiple = many, width = if (many) "24em" else "12em")
        })),
      if (length(miss)) shiny::p(class = "small text-warning mb-1", sprintf(
        t("Not in this TOC, skipped: %s."), paste(labs[miss], collapse = ", "))),
      # another workbook's columns are mapped on its own header: said, so
      # a "(none)" above (the TFL TOC has no population) is not taken for
      # the listing's
      lapply(setdiff(which(!is.na(det$sheet)), k), function(k2) {
        m2 <- res$maps[[k2]]
        m2 <- m2[intersect(names(labs), names(m2))]
        if (!length(m2)) return(NULL)
        shiny::p(class = "small text-muted mb-1", sprintf(
          t("Columns of %s: %s."), det$file[k2],
          paste(sprintf("%s = %s", labs[names(m2)],
                        vapply(m2, paste, "", collapse = ", ")), collapse = "; ")))
      }),
      shiny::p(class = "small text-muted mb-1",
               if (.study_header_says(rv$p, "OUTPUT_TITLE")) {
                 t("The population goes in the header ({OUTPUT_TITLE}), after the first title.")
               } else t("The population becomes the last title line.")),
      shiny::p(class = "small text-muted mb-1",
               t("A report's section: this column, else the heading row above it (a row with no ID); a section given here is kept.")),
      if (.toc_has_item(res, "phase")) toc_overrides_ui(res),
      shiny::checkboxInput(toc_id("remember"),
                           t("Remember this mapping in the company standards"), FALSE,
                           width = "100%"))
  }
  # a phase's suffixes for this study (the rule set's by default; kept in
  # the study's TOC profile)
  toc_overrides_ui <- function(res) {
    st <- res$rule_set$settings
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end",
      shiny::span(class = "small fw-bold", t("Phase suffixes for this study")),
      shiny::textInput(toc_id("ov_number_suffix"), t("On the ID and label"), st$number_suffix,
                       width = "12em"),
      shiny::textInput(toc_id("ov_title_suffix"), t("On a title line"), st$title_suffix,
                       width = "22em"),
      shiny::textInput(toc_id("ov_title_line"), t("Title line (1, 2, last)"), st$title_line,
                       width = "10em"))
  }
  # the reports as the rules make them: a row a report, its phase, label,
  # titles, shell and batches; the rows not read and the warnings under it
  toc_preview_ui <- function() {
    if (toc_done()) return(NULL)
    res <- toc_result()
    if (inherits(res, "error") || !nrow(res$rows)) return(NULL)
    r <- res$reports
    p <- toc_problems(res, tflplanner_language())
    hand <- p[p$level == "hand", , drop = FALSE]
    chk <- p[p$level == "check", , drop = FALSE]
    nb <- nrow(res$batches)
    show <- seq_len(min(nrow(r), if (isTRUE(input[[toc_id("show_all")]])) nrow(r) else 8L))
    titles <- vapply(r$titles, function(v) paste(v[!is.na(v)], collapse = " / "), "")
    shell <- ifelse(is.na(r$shell_sheet), "\u2014", paste0(r$shell_file, ": ", r$shell_sheet))
    shiny::div(
      class = "mb-2",
      shiny::h6(t("The reports")),
      shiny::p(class = "small mb-1", sprintf(t("%d reports from %d rows."), nrow(r), nrow(res$rows)),
               if (nb) paste0(" ", sprintf(t("%s report set: %d reports."),
                                           res$rule_set$settings$topline_batch, nb))),
      shiny::tags$table(
        class = "table table-sm small",
        shiny::tags$thead(shiny::tags$tr(lapply(
          t(c("output_id", "Phase", "Label", "Titles", "Shell", "Report sets")), shiny::tags$th))),
        shiny::tags$tbody(lapply(show, function(i) shiny::tags$tr(
          shiny::tags$td(r$output_id[i]),
          shiny::tags$td(if (is.na(r$phase[i])) "\u2014" else r$phase[i]),
          shiny::tags$td(.or_na(r$label[i], "")), shiny::tags$td(titles[i]),
          shiny::tags$td(shell[i]), shiny::tags$td(.or_na(r$batches[i], "")))))),
      if (nrow(r) > 8L) shiny::checkboxInput(toc_id("show_all"), sprintf(t("Show all %d reports"), nrow(r)),
                                             isTRUE(input[[toc_id("show_all")]])),
      if (nrow(hand)) shiny::div(
        class = "small", shiny::span(class = "fw-bold", sprintf(t("Rows not read (%d):"), nrow(hand))),
        " ", paste(hand$message, collapse = " \u00b7 "), " ",
        shiny::span(class = "text-muted", t("Set them by hand after taking it in, or fix the TOC."))),
      if (nrow(chk)) shiny::div(
        class = "small text-warning", shiny::span(class = "fw-bold", sprintf(t("Warnings (%d):"), nrow(chk))),
        " ", paste(chk$message, collapse = " \u00b7 ")),
      if (nrow(res$notes)) shiny::div(class = "small text-muted", paste(
        # a way of making the IDs is said by its label
        sprintf(t(res$notes$template), toc_note_arg(res$notes$file), toc_note_arg(res$notes$sheet)),
        collapse = " ")))
  }
  # the TOC as tflspec reads it (the reports the rules made); an error of
  # the rules (no title column, an ID twice ...) stops it
  toc_read <- shiny::reactive({
    res <- toc_result()
    if (inherits(res, "error")) return(res)
    err <- res$problems[res$problems$level == "error", , drop = FALSE]
    if (nrow(err)) {
      msg <- paste(toc_problems(res, tflplanner_language())$message[res$problems$level == "error"],
                   collapse = " ")
      return(structure(class = c("error", "condition"), list(message = msg, call = NULL)))
    }
    if (!nrow(res$reports)) {
      return(structure(class = c("error", "condition"),
                       list(message = t("No report could be read from it."), call = NULL)))
    }
    tryCatch(toc_import_spec(res), error = function(e) e)
  })
  # the TOC as the study takes it: a header that says {OUTPUT_TITLE} has
  # the title and analysis set, not the title lines
  toc_spec <- shiny::reactive({
    sp <- toc_read()
    if (inherits(sp, "error")) sp else .toc_titles_in_header(rv$p, sp)
  })
  toc_offset <- shiny::reactive(toc_title_offset(rv$p))
  # the TOC's analysis sets: each text it uses, the study's set it is
  # (matched; changed here), the reports it names
  toc_pops <- shiny::reactive({
    res <- toc_result()
    if (inherits(res, "error") || !.toc_has_item(res, "population") || !nrow(res$reports)) return(NULL)
    po <- rv$p$ard$populations
    adsl <- tryCatch(an_data(po$dataset[1L] %||% "ADSL"), error = function(e) NULL)
    tryCatch(toc_populations(rv$p, res$reports, data = adsl), error = function(e) NULL)
  })
  toc_pop_ui <- function() {
    if (toc_done()) return(NULL)
    tp <- toc_pops()
    if (is.null(tp) || !nrow(tp)) return(NULL)
    u <- unique(tp$text)
    ch <- pop_choices()
    names(ch)[1L] <- t("(not taken)")
    shiny::div(
      class = "mb-2",
      shiny::h6(with_tip(t("Populations"), t("The TOC's population column, as the study's populations: matched by id, label or a usual word (Safety Population: SAF); change it here. Taken in, it is the report's population (the report list's, step 1's): its data is made, and a report whose set changes has its analyses moved."))),
      lapply(seq_along(u), function(k) {
        ids <- tp$output_id[tp$text == u[k]]
        now <- tp$population_id[match(u[k], tp$text)]
        shiny::div(
          class = "d-flex flex-wrap align-items-baseline gap-2 small",
          shiny::span(class = "fw-bold", u[k]),
          shiny::span(class = "text-muted", sprintf(t("%d reports"), length(ids))),
          shiny::div(style = "min-width: 14rem",
                     shiny::selectInput(toc_id(paste0("pop_", k)), NULL, ch,
                                        selected = if (is.na(now)) "" else now, width = "100%")),
          if (is.na(now)) shiny::span(class = "text-warning", t("not matched: choose the population, or leave it")))
      }))
  }
  # the analysis data of the tables' datasets: made for the new reports
  # (ticked by default), for those taken in before only when asked
  toc_data_ui <- function() {
    if (toc_done()) return(NULL)
    res <- toc_result()
    if (inherits(res, "error") || !.toc_has_item(res, "datasets")) return(NULL)
    ch <- toc_ch()
    if (inherits(ch, "error")) return(NULL)
    before <- any(ch$reports$status %in% c("changed", "same"))
    shiny::div(
      class = "mb-2",
      shiny::checkboxInput(toc_id("make_data"), with_tip(t("Make the tables' analysis data from their datasets"), t("For each dataset of a table (the TOC's datasets column) that is not its population's own: <dataset>_<set>, kept to the subjects of adsl_<set> -- found, or made. Its first analysis starts from it; change or delete it in 1-1.")), TRUE,
                           width = "100%"),
      if (before) shiny::conditionalPanel(
        sprintf("input['%s']", toc_id("make_data")),
        shiny::checkboxInput(toc_id("make_again"), t("Also for the reports taken in before (made again where deleted)"), FALSE, width = "100%")))
  }
  # the reports' analysis sets as chosen in the dialog
  toc_pop_chosen <- function() {
    tp <- toc_pops()
    if (is.null(tp) || !nrow(tp)) return(character())
    u <- unique(tp$text)
    v <- vapply(seq_along(u), function(k) input[[toc_id(paste0("pop_", k))]] %||%
                  (tp$population_id[match(u[k], tp$text)] %||% ""), "")
    v <- stats::setNames(v[match(tp$text, u)], tp$output_id)
    v[!is.na(v) & nzchar(v)]
  }
  toc_last_now <- shiny::reactive({
    rv$ver
    toc_last(rv$study)
  })
  toc_ch <- shiny::reactive({
    sp <- toc_spec()
    if (inherits(sp, "error")) return(sp)
    toc_changes(rv$p, sp, toc_last_now(), toc_offset())
  })
  toc_status_labels <- c(new = "New", changed = "Changed", same = "No change",
                         missing = "Not in the TOC")
  toc_line_text <- function(s) {
    s <- gsub("\u001f", " / ", s %||% "", fixed = TRUE)
    s <- gsub("^( / )+|( / )+$", "", s)
    ifelse(is.na(s) | !nzchar(s), t("(none)"), s)
  }
  # what would change: a row a report, its lines under it; a new report's
  # type can be set (a guessed one is marked), a line edited here and
  # changed in the TOC is asked about (kept as it is unless ticked); the
  # reports with nothing to change folded away
  toc_changes_ui <- function() {
    # taken in: the result says what was done (what is left to change is
    # nothing, until the TOC changes again)
    if (toc_done() || is.null(input[[toc_id("file")]])) return(NULL)
    ch <- toc_ch()
    if (inherits(ch, "error")) {
      return(shiny::div(class = "alert alert-danger py-1 small",
                        sprintf(t("The TOC cannot be read: %s"), conditionMessage(ch))))
    }
    sp <- toc_spec()
    r <- ch$reports
    l <- ch$lines
    kp <- ch$kept
    inf <- ch$info %||% .toc_no_info()
    use_all <- isTRUE(input[[toc_id("use_all")]])
    n <- table(factor(r$status, names(toc_status_labels)))
    sheet_lab <- c(titles = t("Title line"), footnotes = t("Footnote line"))
    act_lab <- c(add = t("will be added"), update = t("will be updated"),
                 remove = t("will be taken out (the TOC dropped it)"),
                 ask = t("edited here, and changed in the TOC"),
                 keep = t("edited here; the TOC's is the same as the last time"),
                 move = t("added here: moves after the TOC's lines"))
    # (the report's title: t("Title") is the study's)
    info_lab <- c(OUTPUT_LABEL = t("Label"), OUTPUT_TITLE = t("Report title"),
                  OUTPUT_POPULATION = t("Analysis set text"), OUTPUT_SECTION = t("Section"))
    # an item edited here: the TOC's taken only when ticked (or all)
    choose <- function(input_id) {
      if (use_all) shiny::span(class = "text-primary", t("(the TOC's: all chosen above)"))
      else shiny::checkboxInput(input_id, t("Use the TOC's text"), FALSE)
    }
    # what was edited here, and what the TOC gives: lines kept, information
    edited_rows <- function(id) {
      ki <- which(kp$output_id == id & !is.na(kp$toc))
      ii <- which(inf$output_id == id)
      shiny::tagList(
        lapply(ki, function(k) shiny::div(
          class = "small", sprintf("%s %s (%s): ", sheet_lab[[kp$sheet[k]]], kp$line[k], act_lab[["keep"]]),
          shiny::span(class = "text-muted", toc_line_text(kp$now[k])), " \u2192 ",
          toc_line_text(kp$toc[k]), choose(toc_id(paste0("kept_", k))))),
        lapply(ii, function(j) {
          a <- inf$action[j]
          now <- if (is.na(inf$now[j])) t("(none)") else inf$now[j]
          shiny::div(
            class = "small", sprintf("%s (%s): ", info_lab[[inf$name[j]]], act_lab[[a]]),
            if (a %in% c("keep", "ask")) shiny::tagList(
              shiny::span(class = "text-muted", now), " \u2192 ", inf$toc[j],
              choose(toc_id(paste0("info_", j))))
            else if (a == "add") inf$toc[j]
            else shiny::tagList(shiny::span(class = "text-muted", now), " \u2192 ", inf$toc[j]))
        }))
    }
    types <- stats::setNames(names(.type_labels), t(unname(.type_labels)))
    show_same <- isTRUE(input[[toc_id("show_same")]])
    in_header <- isTRUE(attr(sp, "titles_in_header"))
    toc_row <- function(i) {
      id <- r$output_id[i]
      st <- r$status[i]
      type_cell <- if (st == "new") {
        shiny::div(
          class = "d-flex align-items-center gap-1",
          shiny::selectInput(toc_id(paste0("type_", i)), NULL, types,
                             selected = if (is.na(r$type_toc[i])) "table" else r$type_toc[i],
                             width = "10em"),
          if (isTRUE(r$guessed[i])) shiny::span(
            class = "small text-warning", title = t("Guessed from the ID or the title: check it"), "*"))
      } else if (st == "missing") {
        "\u2014"
      } else {
        now <- t(.type_labels[[r$type_now[i]]] %||% r$type_now[i])
        # (a user-code report: compared with what it makes)
        if (.toc_type_differs(r$type_now[i], r$type_toc[i], r$guessed[i])) {
          shiny::span(now, shiny::span(class = "small text-warning",
                                       sprintf(t("(the TOC says %s: not changed)"),
                                               t(.type_labels[[r$type_toc[i]]]))))
        } else now
      }
      li <- which(l$output_id == id)
      what <- if (st == "missing") {
        shiny::span(class = "small text-muted", t("Kept: delete it yourself if it is no longer needed."))
      } else if (st == "new") {
        # what it will hold: the TOC's lines; none when the header prints
        # the title and population (said once above the table)
        parts <- lapply(c("titles", "footnotes"), function(sh) {
          v <- .toc_text(.toc_lines(sp, sh, id, toc_offset()))
          lapply(seq_along(v), function(k) shiny::div(
            class = "small", sprintf("%s %s: ", sheet_lab[[sh]], names(v)[k]),
            toc_line_text(v[[k]])))
        })
        if (!length(unlist(parts, recursive = FALSE)) && in_header) {
          shiny::span(class = "small text-muted", t("(in the header)"))
        } else shiny::tagList(parts)
      } else if (length(li) && all(l$action[li] == "add")) {
        # only lines to add: said shortly
        shiny::span(class = "small", sprintf(
          t("%d title lines and %d footnote lines will be added."),
          sum(l$sheet[li] == "titles"), sum(l$sheet[li] == "footnotes")))
      } else shiny::tagList(lapply(li, function(k) {
        a <- l$action[k]
        head <- if (a == "move") {
          sprintf("%s %s \u2192 %s (%s): ", sheet_lab[[l$sheet[k]]], l$line[k], l$to[k], act_lab[[a]])
        } else sprintf("%s %s (%s): ", sheet_lab[[l$sheet[k]]], l$line[k], act_lab[[a]])
        shiny::div(
          class = "small", head,
          if (a == "ask") shiny::tagList(
            shiny::span(class = "text-muted", toc_line_text(l$now[k])), " \u2192 ",
            toc_line_text(l$toc[k]), choose(toc_id(paste0("ask_", k))))
          else if (a == "move") shiny::span(class = "text-muted", toc_line_text(l$now[k]))
          else if (a == "add") toc_line_text(l$toc[k])
          else shiny::tagList(shiny::span(class = "text-muted", toc_line_text(l$now[k])),
                              " \u2192 ", toc_line_text(l$toc[k])))
      }))
      shiny::tags$tr(
        class = if (st == "missing") "table-warning",
        shiny::tags$td(id), shiny::tags$td(t(toc_status_labels[[st]])),
        shiny::tags$td(type_cell),
        shiny::tags$td(if (st == "new") what else shiny::tagList(what, edited_rows(id))))
    }
    # edited here: the reports with lines or information to choose for
    n_edit <- sum(!is.na(kp$toc)) + sum(inf$action %in% c("keep", "ask")) +
      sum(l$action == "ask")
    has_edit <- r$output_id %in% c(kp$output_id[!is.na(kp$toc)],
                                   inf$output_id[inf$action %in% c("keep", "ask")])
    shown <- which(r$status != "same" | show_same | has_edit)
    skipped <- attr(sp, "skipped")
    n_skip <- if (is.null(skipped)) 0L else NROW(skipped)
    shiny::tagList(
      shiny::h6(t("What would change")),
      shiny::p(class = "small mb-1",
               sprintf(t("New %d, changed %d, no change %d, not in the TOC %d."),
                       n[["new"]], n[["changed"]], n[["same"]], n[["missing"]]),
               " ", t("Only what the TOC holds is changed: tables, ARDs, pages and lines added here stay; lines added here go after the TOC's lines.")),
      if (n_skip) shiny::p(class = "small text-muted mb-1",
                           sprintf(t("%d heading rows (no report ID) are skipped."), n_skip)),
      if (in_header) shiny::p(class = "small text-muted mb-1",
                              t("The study's header prints each report's title and analysis set ({OUTPUT_TITLE}): the TOC's first title and population go there, not in the title lines.")),
      # what was edited here: kept unless the TOC's is chosen, line by
      # line or all at once
      if (n_edit) shiny::div(
        class = "alert alert-info py-1 small mb-1",
        sprintf(t("%d lines or items were edited here. Unticked, they stay as edited; tick a line's \"Use the TOC's text\", or all of them here, to take the TOC's."), n_edit),
        shiny::checkboxInput(toc_id("use_all"), t("Use the TOC's text for everything edited here"), use_all)),
      if (n[["same"]]) shiny::checkboxInput(toc_id("show_same"),
                                            t("Show the reports with no change"), show_same),
      shiny::tags$table(
        class = "table table-sm align-middle",
        shiny::tags$thead(shiny::tags$tr(lapply(
          t(c("output_id", "State", "Type", "Titles and footnotes")), shiny::tags$th))),
        shiny::tags$tbody(lapply(shown, toc_row))),
      if (any(r$guessed[shown] & r$status[shown] == "new")) shiny::p(
        class = "small text-muted",
        t("* = the type is guessed from the ID or the title: check it before taking it in.")))
  }
  shiny::observeEvent(input[[toc_id("do")]], {
    shiny::req(!toc_done())
    refuse <- function(msg, level = "warning") {
      toc_btn_ver(toc_btn_ver() + 1L)
      notify(msg, level)
    }
    if (is.null(input[[toc_id("file")]])) return(refuse(t("Choose the TOC file.")))
    sp <- toc_spec()
    ch <- toc_ch()
    if (inherits(ch, "error")) return(refuse(conditionMessage(ch), "error"))
    r <- ch$reports
    l <- ch$lines
    types <- vapply(which(r$status == "new"), function(i)
      input[[toc_id(paste0("type_", i))]] %||% NA_character_, "")
    names(types) <- r$output_id[r$status == "new"]
    # the TOC's text for what was edited here: ticked, or all
    all_toc <- isTRUE(input[[toc_id("use_all")]])
    ticked <- function(prefix, k) all_toc || isTRUE(input[[toc_id(paste0(prefix, k))]])
    ask <- which(l$action == "ask")
    use <- ask[vapply(ask, function(k) ticked("ask_", k), NA)]
    kp <- ch$kept
    kk <- which(!is.na(kp$toc))
    kuse <- kk[vapply(kk, function(k) ticked("kept_", k), NA)]
    inf <- ch$info %||% .toc_no_info()
    ii <- which(inf$action %in% c("keep", "ask"))
    iuse <- ii[vapply(ii, function(j) ticked("info_", j), NA)]
    use_toc <- c(paste(l$output_id[use], l$sheet[use], l$line[use], sep = "|"),
                 paste(kp$output_id[kuse], kp$sheet[kuse], kp$line[kuse], sep = "|"),
                 paste(inf$output_id[iuse], "info", inf$name[iuse], sep = "|"))
    off <- toc_offset()
    last <- toc_last_now()
    p <- guarded(toc_apply(rv$p, sp, ch, use_toc = use_toc,
                           types = types[!is.na(types)],
                           populations = toc_pop_chosen(),
                           make_data = isTRUE(input[[toc_id("make_data")]]),
                           again = isTRUE(input[[toc_id("make_again")]])))
    made <- attr(p, "made")
    if (!is.null(p)) attr(p, "made") <- NULL
    if (is.null(p)) return(toc_btn_ver(toc_btn_ver() + 1L))
    res <- toc_result()
    # the Topline batch into the report list's batches
    p <- guarded(toc_apply_extras(p, res, last))
    if (is.null(p)) return(toc_btn_ver(toc_btn_ver() + 1L))
    paths <- toc_paths()
    rec <- guarded(.toc_record(rv$study, paths, basename(paths), ch,
                               .toc_snapshot_rules(sp, off, last, res), res))
    if (is.null(rec)) return(toc_btn_ver(toc_btn_ver() + 1L))
    was_dirty <- isTRUE(shiny::isolate(dirty()))
    rv$p <- p
    if (was_dirty) {
      notify(t("Saved at once, with the other unsaved changes: the record of TOCs taken in and the reports must agree."))
    }
    do_save()
    if (isTRUE(input[[toc_id("remember")]])) {
      det <- res$detection
      k <- which(!is.na(det$sheet))[1L]
      f <- guarded(remember_toc_map(
        res$map[!vapply(res$map, is.null, NA)], rule_set = res$rule_set$name,
        sheet = if (!is.na(k) && !toc_is_csv(paths[k])) det$sheet[k],
        topline_sheet = if (!is.na(k)) .or_na(det$topline_sheet[k], NULL),
        header_row = if (!is.na(k)) det$header_row[k], id_rule = res$id_rule))
      if (!is.null(f)) notify(sprintf(t("The mapping is remembered in %s."), f))
    }
    toc_done(TRUE)
    bump()
    new_ids <- r$output_id[r$status == "new"]
    n_ask <- length(ask) - length(use)
    output[[toc_id("result")]] <- shiny::renderUI(shiny::div(
      class = "alert alert-success py-1 small",
      sprintf(t("%s is taken in as %s: new %d, changed %d, not in the TOC %d."),
              paste(basename(paths), collapse = ", "), rec$import_id, length(new_ids),
              sum(r$status == "changed"), sum(r$status == "missing")),
      if (nrow(res$batches)) shiny::tagList(shiny::br(), sprintf(
        t("%s report set: %d reports."), res$rule_set$settings$topline_batch, nrow(res$batches))),
      if (any(!is.na(res$reports$shell_sheet))) shiny::tagList(shiny::br(), sprintf(
        t("%d shells linked."), sum(!is.na(res$reports$shell_sheet)))),
      if (any(res$problems$level == "hand")) shiny::tagList(shiny::br(), sprintf(
        t("%d rows to set by hand."), sum(res$problems$level == "hand"))),
      if (n_ask) shiny::tagList(shiny::br(), sprintf(
        t("%d lines edited here were kept instead of the TOC's text."), n_ask)),
      if (length(new_ids)) shiny::tagList(shiny::br(), sprintf(
        t("New reports: %s. Make their content from the list's buttons."),
        paste(new_ids, collapse = ", "))),
      if (length(made)) shiny::tagList(shiny::br(), sprintf(
        t("Analysis data made for the reports' populations: %s."), paste(made, collapse = ", ")))))
  })
  .srv_keep(environment())
}
