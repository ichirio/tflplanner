# The app.  A study is opened first; everything else is that study.  The
# study's planner lives in a reactiveValues and every grid shows a slice of
# one sheet (the rows of the report chosen in the sidebar) and writes the
# slice back when it is edited.  A grid is redrawn only when the slice it
# shows changes from outside (another report chosen, the study opened, a
# report copied, rows filled from the ARD), never because it was edited
# itself.  Nothing reaches the disk until the user saves.
#
# Every text is English here and translated by tr() (R/i18n.R).

.all_rows <- "__all__"
.default_rows <- "__default__"
.study_tabs <- c("outputs", "ard", "builder", "table_spec", "report_spec",
                 "data", "results")

.sheet_labels <- c(
  tables = "tables: roles", variables = "variables", cells = "cells",
  layout = "layout: pages", columns = "columns", style = "style",
  col_header = "col_header: column header",
  report = "report", page = "page", header = "header", footer = "footer",
  titles = "titles", footnotes = "footnotes")

.type_labels <- c(table = "Table", listing = "Listing", figure = "Figure")

# the sheets whose tab offers help of its own above the grid
.assisted <- c("tables", "variables", "cells", "col_header")

.status_labels <- c(
  "no program" = "Not written (save)", unsaved = "Unsaved (save)",
  todo = "TODO (data part)", "not run" = "Not run", error = "Error",
  outdated = "Rerun needed", ok = "OK")

#' Start the tflplanner app
#'
#' Opens the study manager.  Choose a study -- it opens as it was last
#' saved -- or create or register one, then define its reports (Tables from
#' the table definition; Listings and Figures from their programs), their
#' data code, and run them.  The first time, tflplanner's home is set up
#' with the defaults ([setup_tflplanner()]).  The app is in English or
#' Japanese (`setup_tflplanner(language = )`, or its settings).
#'
#' @param study A study to open at start: a registered study's id, or a
#'   study folder.
#' @param ... Passed to [shiny::runApp()] (e.g. `launch.browser`, `port`).
#' @return `planner_app()` returns a [shiny::shinyApp()] object;
#'   `run_app()` runs it.
#' @examples
#' \dontrun{
#' run_app()
#' run_app("ABC-101")
#' }
#' @export
run_app <- function(study = NULL, ...) {
  shiny::runApp(planner_app(study), ...)
}

#' @rdname run_app
#' @export
planner_app <- function(study = NULL) {
  if (!.is_set_up()) setup_tflplanner()
  start <- if (!is.null(study)) open_study(study)
  shiny::shinyApp(function(req) app_ui(tflplanner_language()),
                  function(input, output, session)
                    app_server(input, output, session, start))
}

# ------------------------------------------------------------------- UI

.code_css <- "
.rp-code textarea, .rp-code pre { font-family: Consolas, 'Courier New',
  monospace; font-size: 12.5px; }
.rp-code pre { max-height: 520px; overflow: auto; white-space: pre; }
.rp-help { font-size: 12.5px; }
.handsontable td, .handsontable th { font-size: 12.5px; }
.rp-dirty { color: #b45309; font-weight: 600; }
.rp-study { font-weight: 600; }
.rp-assist { background: var(--bs-tertiary-bg, #f6f7f9); }
.rp-inherited td { color: #6b7280; }
.rhandsontable.html-fill-item { flex: none !important; }
.rp-pv { font-family: Consolas, 'Courier New', monospace; font-size: 12px;
  border-collapse: collapse; width: 100%; margin-bottom: 1rem; }
.rp-pv thead { border-top: 1px solid #333; border-bottom: 1px solid #333; }
.rp-pv tbody { border-bottom: 1px solid #333; }
.rp-pv th { font-weight: normal; vertical-align: bottom; padding: 1px 6px; }
.rp-pv td { padding: 0 6px; white-space: pre; }
.rp-pv .rp-pv-val { text-align: center; }
.rp-pv .rp-pv-indent { padding-left: 2.2em; }
.rp-pv-blank td { height: 1em; }
.rp-pv-page { font-size: 11px; color: #6b7280; }
.rp-b-card { border: 1px solid var(--bs-border-color, #dee2e6);
  border-radius: .5rem; padding: .6rem .8rem; margin-bottom: .6rem; }
.rp-b-card h6 { font-weight: 600; margin-bottom: .4rem; }
.rp-b-card .rank-list-container { margin: 0; }
.rp-b-card .rank-list-item { padding: 2px 8px !important; font-size: 13px; }
.rp-b-var { display: flex; gap: .5rem; align-items: center; }
.rp-b-kind { font-size: 11px; color: #6b7280; }
"

.btn <- function(id, label, class = "btn-sm", ...) {
  shiny::actionButton(id, label, class = class, ...)
}

app_ui <- function(lang = "en") {
  t <- function(x) tr(x, lang)
  two <- bslib::breakpoints(sm = 12, lg = c(5, 7))

  sheet_panel <- function(sheet) {
    bslib::nav_panel(
      t(.sheet_labels[[sheet]]), value = sheet,
      if (sheet %in% .assisted) shiny::uiOutput(paste0("assist_", sheet)),
      rhandsontable::rHandsontableOutput(paste0("hot_", sheet)),
      shiny::uiOutput(paste0("inh_", sheet)),
      shiny::tags$details(
        class = "rp-help mt-2",
        shiny::tags$summary(t("Column help (_README)")),
        DT::DTOutput(paste0("help_", sheet))))
  }
  grid_note <- shiny::p(
    class = "text-muted small mb-1",
    t("Right-click to add or delete rows. Paste from Excel works. Blank = not set. Several values in one cell: separate with |."))

  bslib::page_navbar(
    id = "nav",
    title = "tflplanner",
    # pages scroll: a grid squeezed to fit the window would be 0 px high
    fillable = FALSE,
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    header = shiny::tags$style(.code_css),
    sidebar = bslib::sidebar(
      width = 270,
      shiny::uiOutput("study_side"),
      shiny::radioButtons("target", t("Show rows of"),
                          choices = c("-" = .all_rows)),
      shiny::tags$details(
        class = "small text-muted",
        shiny::tags$summary(t("What do these mean?")),
        shiny::p(shiny::strong(t("A report")), ": ",
                 t("its own rows. Where it has none, it uses the study defaults.")),
        shiny::p(shiny::strong(t("Study defaults")), ": ",
                 t("rows whose output_id is blank. They apply to every report that has no row of its own for the same thing: the page header, the run-information footer, the usual cell template, ...")),
        shiny::p(shiny::strong(t("ALL")), ": ",
                 t("every row of every report and the defaults at once, with output_id shown: for bulk edits and pasting from Excel.")))),

    bslib::nav_panel(
      t("Studies"), value = "study",
      bslib::layout_columns(
        col_widths = two,
        bslib::card(
          bslib::card_header(t("Studies")),
          DT::DTOutput("studies"),
          shiny::div(class = "d-flex flex-wrap gap-2",
                     .btn("open_study", t("Open"),
                          class = "btn-sm btn-primary"),
                     .btn("new_study", t("New study")),
                     .btn("register", t("Register a folder")),
                     .btn("unregister", t("Unregister"),
                          class = "btn-sm btn-outline-danger"),
                     .btn("refresh_studies", t("Refresh"))),
          shiny::tags$details(
            class = "mt-2 small",
            shiny::tags$summary(t("Settings")),
            shiny::uiOutput("settings"))),
        bslib::card(
          bslib::card_header(t("This study")),
          shiny::uiOutput("study_detail")))),

    bslib::nav_panel(
      t("Reports"), value = "outputs",
      bslib::layout_columns(
        col_widths = two,
        bslib::card(
          bslib::card_header(t("Reports (TFL)")),
          DT::DTOutput("outputs"),
          shiny::div(
            class = "d-flex flex-wrap gap-1",
            .btn("add", t("Add")), .btn("copy", t("Copy")),
            .btn("rename", t("Rename")),
            .btn("remove", t("Delete"), class = "btn-sm btn-outline-danger"),
            .btn("up", "\u2191"), .btn("down", "\u2193")),
          shiny::p(class = "text-muted small mt-1",
                   t("The order is the order autoexec_report.R runs them in. Copy copies the report's rows of every sheet."))),
        bslib::navset_card_tab(
          bslib::nav_panel(
            t("Data code"),
            shiny::uiOutput("current_label"),
            shiny::textInput("description", t("Description"), width = "100%"),
            shiny::div(
              class = "rp-code",
              shiny::textAreaInput(
                "data_code",
                t("1. ARD: make `ard` (Listing / Figure: make `content`). Blank = TODO."),
                rows = 10, width = "100%", resize = "vertical"),
              shiny::textAreaInput(
                "process_code",
                t("2. Normalize and rework: make `data` from `ard` (ard_normalize(), then mutate() ...). Blank = data <- ard_normalize(ard)."),
                rows = 5, width = "100%", resize = "vertical",
                placeholder = "data <- ard_normalize(ard)"),
              shiny::div(
                class = "d-flex gap-2 align-items-center mb-2",
                .btn("fetch", t("Run and read the ARD"),
                     class = "btn-sm btn-outline-primary"),
                shiny::span(class = "small text-muted",
                            t("Runs 1 and 2 from the study folder and reads the variables, levels and statistics for input assistance."))),
              shiny::uiOutput("ard_summary"),
              shiny::textAreaInput(
                "setup",
                t("Setup code every report runs first (library(), common data)"),
                rows = 4, width = "100%", resize = "vertical"))),
          bslib::nav_panel(
            t("Program"),
            shiny::uiOutput("program_state"),
            shiny::div(class = "rp-code",
                       shiny::verbatimTextOutput("program")))))),

    bslib::nav_panel(
      "ARD", value = "ard",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
        bslib::card(
          bslib::card_header(t("ARD definition (ard_spec)")),
          shiny::p(class = "small text-muted",
                   t("One row per analysis: the data, the population, the subset, the grouping, the variables and the method (a keyword or any cards / cardx function). The sidebar picks the report whose analyses are shown.")),
          bslib::navset_underline(
            id = "ard_sheet",
            bslib::nav_panel("analyses", value = "analyses",
                             rhandsontable::rHandsontableOutput("hot_ard_analyses")),
            bslib::nav_panel("datasets", value = "datasets",
                             rhandsontable::rHandsontableOutput("hot_ard_datasets")),
            bslib::nav_panel("populations", value = "populations",
                             rhandsontable::rHandsontableOutput("hot_ard_populations")),
            bslib::nav_panel("study", value = "study",
                             rhandsontable::rHandsontableOutput("hot_ard_study"))),
          shiny::uiOutput("ard_check"),
          shiny::tags$details(
            class = "rp-help mt-2",
            shiny::tags$summary(t("Methods")),
            DT::DTOutput("ard_methods"))),
        bslib::navset_card_tab(
          id = "ard_right",
          bslib::nav_panel(
            t("Code (cards / cardx)"), value = "code",
            shiny::radioButtons(
              "ard_scope", NULL,
              stats::setNames(c("report", "study"),
                              t(c("This report", "The whole study"))),
              inline = TRUE),
            shiny::div(class = "rp-code", shiny::verbatimTextOutput("ard_code"))),
          bslib::nav_panel(
            t("Generated ARD"), value = "result",
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-center",
              .btn("ard_run", t("Run this report's analyses"),
                   class = "btn-sm btn-primary"),
              .btn("ard_build", t("Build the study ARD")),
              shiny::span(class = "small text-muted",
                          t("Runs the code on the left in its own R process, from the study folder."))),
            shiny::uiOutput("ard_run_info"),
            DT::DTOutput("ard_table"))))),

    bslib::nav_panel(
      t("Table builder (beta)"), value = "builder",
      shiny::uiOutput("builder_note"),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        shiny::uiOutput("builder_form"),
        bslib::card(
          bslib::card_header(shiny::div(
            class = "d-flex justify-content-between",
            shiny::span(t("Preview: the table as it will print")),
            shiny::uiOutput("builder_pages", inline = TRUE))),
          shiny::uiOutput("builder_preview")))),

    bslib::nav_panel(
      t("Table definition"), value = "table_spec",
      shiny::uiOutput("type_note"),
      shiny::div(class = "rp-assist border rounded p-2 mb-2",
                 shiny::uiOutput("assist")),
      grid_note,
      do.call(bslib::navset_card_underline,
              c(list(id = "table_sheet"), lapply(table_sheets(), sheet_panel)))),

    bslib::nav_panel(
      t("Report layout"), value = "report_spec",
      grid_note,
      do.call(bslib::navset_card_underline,
              lapply(report_sheets(), sheet_panel))),

    bslib::nav_panel(
      t("Data"), value = "data",
      bslib::layout_columns(
        col_widths = two,
        bslib::card(
          bslib::card_header(t("Input data (data/)")),
          shiny::div(
            class = "d-flex gap-2 align-items-end",
            shiny::selectInput("data_folder", t("Into"),
                               c("adam", "sdtm", "other"), width = "110px"),
            shiny::fileInput("data_upload", t("Add files"), multiple = TRUE)),
          DT::DTOutput("data_files"),
          shiny::div(class = "d-flex gap-2",
                     .btn("data_refresh", t("Refresh")),
                     .btn("data_open", t("Open folder")))),
        bslib::card(
          bslib::card_header(t("Contents (first 50 rows)")),
          shiny::uiOutput("data_dim"),
          DT::DTOutput("data_head")))),

    bslib::nav_panel(
      t("Results"), value = "results",
      bslib::card(
        bslib::card_header(t("Reports")),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          .btn("run_selected", t("Run selected"),
               class = "btn-sm btn-primary"),
          .btn("run_all", t("Run all"), class = "btn-sm btn-primary"),
          .btn("status_refresh", t("Refresh")),
          .btn("check", t("Check the definition")),
          .btn("tfl_open", t("Open output/tfl")),
          shiny::downloadButton("rtf_download", t("Download the RTF"),
                                class = "btn-sm")),
        shiny::uiOutput("job"),
        DT::DTOutput("status")),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
        bslib::card(
          bslib::card_header(t("Log")),
          shiny::div(class = "rp-code", shiny::verbatimTextOutput("log"))),
        bslib::card(
          bslib::card_header(t("Definition check")),
          DT::DTOutput("check_result")))),

    bslib::nav_spacer(),
    bslib::nav_item(shiny::uiOutput("save_state")),
    bslib::nav_item(.btn("save", t("Save"), class = "btn-sm btn-primary")))
}

# ---------------------------------------------------------------- server

.target_value <- function(target) {
  if (is.null(target) || identical(target, .all_rows)) return("")
  if (identical(target, .default_rows)) return(NA_character_)
  target
}

.na_blank <- function(d) {
  d[] <- lapply(d, function(v) ifelse(is.na(v), "", v))
  d
}

.grid <- function(d, sheet, key, choices) {
  d <- .na_blank(d)
  if (!nrow(d)) d[1L, ] <- ""
  h <- rhandsontable::rhandsontable(
    d, rowHeaders = TRUE, useTypes = FALSE, stretchH = "all",
    height = 420, minSpareRows = 1L, planner_key = key)
  h <- rhandsontable::hot_context_menu(h, allowRowEdit = TRUE,
                                       allowColEdit = FALSE)
  for (cn in names(choices)) {
    if (cn %in% names(d) && length(choices[[cn]])) {
      h <- suppressWarnings(rhandsontable::hot_col(
        h, cn, type = "dropdown", source = c("", choices[[cn]]),
        strict = FALSE))
    }
  }
  h
}

.help_table <- function(sheet) {
  rd <- .readme()
  if (is.null(rd)) return(NULL)
  rd <- rd[!is.na(rd$sheet) & rd$sheet == sheet, , drop = FALSE]
  rd$sheet <- NULL
  rd
}

.open_folder <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  utils::browseURL(normalizePath(path))
}

.dt <- function(d, ..., selection = "single") {
  DT::datatable(d, rownames = FALSE, selection = selection,
                options = list(dom = "t", paging = FALSE, ordering = FALSE,
                               scrollX = TRUE, ...))
}

# several reports' metadata as one: what any of them offers
.merge_meta <- function(ms) {
  ms <- Filter(Negate(is.null), ms)
  if (!length(ms)) return(NULL)
  if (length(ms) == 1L) return(ms[[1L]])
  keys <- list()
  for (m in ms) for (k in names(m$keys)) keys[[k]] <- unique(c(keys[[k]],
                                                               m$keys[[k]]))
  v <- do.call(rbind, lapply(ms, `[[`, "variables"))
  list(keys = keys, by = unique(unlist(lapply(ms, `[[`, "by"))),
       hierarchy = unique(unlist(lapply(ms, `[[`, "hierarchy"))),
       variables = v[!duplicated(v$variable), , drop = FALSE],
       contexts = unique(unlist(lapply(ms, `[[`, "contexts"))),
       stats = unique(unlist(lapply(ms, `[[`, "stats"))),
       columns = unique(unlist(lapply(ms, `[[`, "columns"))))
}

app_server <- function(input, output, session, start) {
  lang <- tflplanner_language()
  t <- function(x) tr(x, lang)
  # `want`: the report to select once the sidebar knows it
  rv <- shiny::reactiveValues(
    study = NULL, p = NULL, saved = NULL, meta = NULL, saved_meta = NULL,
    ver = 0L, want = NULL, job = NULL, job_what = NULL, status_ver = 0L,
    studies_ver = 0L, ard_ver = 0L)
  bump <- function() rv$ver <- rv$ver + 1L
  notify <- function(msg, type = "message") {
    shiny::showNotification(msg, type = type, duration = 6)
  }
  guarded <- function(expr) {
    tryCatch(expr, error = function(e) {
      notify(conditionMessage(e), "error")
      NULL
    })
  }
  has_study <- shiny::reactive(!is.null(rv$study))
  dirty <- shiny::reactive({
    has_study() && (!identical(rv$p, rv$saved) ||
                       !identical(rv$meta, rv$saved_meta))
  })
  # for shiny::testServer(), which sees only the session
  session$userData$rv <- rv
  session$userData$dirty <- dirty

  # -- the study ---------------------------------------------------------
  set_study <- function(s) {
    s$planner <- .study_spec_keys(s$planner)
    rv$study <- s
    rv$p <- rv$saved <- s$planner
    rv$meta <- rv$saved_meta <- s$meta[.study_fields]
    rv$want <- output_ids(s$planner)[1L]
    bump()
    rv$status_ver <- rv$status_ver + 1L
    rv$ard_ver <- rv$ard_ver + 1L
  }
  if (!is.null(start)) set_study(start)

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
  do_save <- function(regenerate = character()) {
    s <- guarded(save_study(current_study(), regenerate = regenerate))
    if (is.null(s)) return(FALSE)
    rv$study <- s
    rv$p <- rv$saved <- s$planner
    rv$meta <- rv$saved_meta <- s$meta[.study_fields]
    rv$status_ver <- rv$status_ver + 1L
    rv$studies_ver <- rv$studies_ver + 1L
    n <- sum(s$files$status == "written")
    kept <- sum(s$files$status == "kept")
    notify(paste0(sprintf(t("Saved (%d files written)"), n),
                  if (kept) paste0(" ", sprintf(
                    t("%d hand-edited programs kept"), kept))))
    TRUE
  }
  shiny::observeEvent(input$save, {
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    do_save()
  })
  output$save_state <- shiny::renderUI({
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
    last <- shiny::isolate(if (has_study()) rv$study$meta$study_id else
      tflplanner_config()$last_study)
    v <- data.frame(
      a = d$study_id, b = d$title, c = d$compound, d = d$phase,
      e = d$saved,
      f = ifelse(d$folder, d$path, paste(d$path, t("(not found)"))),
      stringsAsFactors = FALSE)
    names(v) <- t(c("Study ID", "Title", "Compound", "Phase", "Last saved",
                    "Folder"))
    sel <- match(last, d$study_id)
    DT::datatable(v, rownames = FALSE,
                  selection = list(mode = "single",
                                   selected = if (!is.na(sel)) sel),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                 scrollX = TRUE, scrollY = "320px"))
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
      shiny::hr(),
      shiny::p(shiny::strong(t("Company standards")), shiny::br(),
               if (file.exists(.standards_file())) {
                 a <- company_standards()$about
                 sprintf("%s %s (%s)", a$value[a$key == "name"],
                         a$value[a$key == "version"], .standards_file())
               } else t("the built-in draft")),
      shiny::p(class = "small text-muted",
               t("Dropdowns, presets, statistics, ARD methods, listing types, and the defaults a new study starts with.")),
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
    guarded(suppressMessages(setup_tflplanner(studies_root = r)))
    rv$studies_ver <- rv$studies_ver + 1L
    notify(sprintf(t("New studies go to %s"), r))
  })
  shiny::observeEvent(input$save_language, {
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    guarded(suppressMessages(setup_tflplanner(language = input$language)))
    session$reload()
  })
  selected_study <- function() {
    i <- input$studies_rows_selected
    if (!length(i)) {
      notify(t("Choose a study"), "warning")
      return(NULL)
    }
    studies()[i, , drop = FALSE]
  }
  shiny::observeEvent(input$register, {
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
  })
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
  shiny::observeEvent(input$unregister, {
    d <- selected_study()
    if (is.null(d)) return()
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Unregister %s"), d$study_id),
      t("This deletes what tflplanner keeps about the study (definition, data code, history). The study folder (data, spec, programs, outputs) stays, and Register a folder brings it back from spec/."),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("unregister_ok", t("Unregister"),
                                   class = "btn-danger"))))
  })
  shiny::observeEvent(input$unregister_ok, {
    d <- studies()[input$studies_rows_selected, , drop = FALSE]
    shiny::removeModal()
    if (has_study() && identical(rv$study$meta$study_id, d$study_id)) {
      rv$study <- rv$p <- rv$saved <- rv$meta <- rv$saved_meta <- NULL
      bump()
    }
    unregister_study(d$study_id)
    rv$studies_ver <- rv$studies_ver + 1L
    notify(sprintf(t("Unregistered %s"), d$study_id))
  })
  shiny::observeEvent(input$open_study, {
    d <- selected_study()
    if (is.null(d)) return()
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    s <- guarded(open_study(d$study_id))
    if (!is.null(s)) {
      set_study(s)
      notify(sprintf(t("Opened %s"), s$meta$study_id))
      bslib::nav_select("nav", "outputs")
    }
  })
  shiny::observeEvent(input$new_study, {
    shiny::showModal(shiny::modalDialog(
      title = t("New study"),
      shiny::textInput("ns_id",
                       t("Study ID (the folder name: letters, digits . _ -)")),
      shiny::textInput("ns_title", t("Title"), width = "100%"),
      shiny::textInput("ns_compound", t("Compound")),
      shiny::textInput("ns_phase", t("Phase")),
      shiny::textAreaInput("ns_description", t("Description"), width = "100%"),
      shiny::radioButtons(
        "ns_from", t("Reports"),
        stats::setNames(c("empty", "study", "sample"),
                        t(c("Start empty",
                            "Copy another study (definition and data code)",
                            "rtfreporter's sample (5 reports)")))),
      shiny::conditionalPanel(
        "input.ns_from == 'study'",
        shiny::selectInput("ns_src", t("Copy from"),
                           stats::setNames(studies()$study_id,
                                           studies()$study_id))),
      shiny::textInput("ns_root", t("Create the study folder in"),
                       value = studies_root(), width = "100%"),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ns_ok", t("Create"),
                                   class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$ns_ok, {
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    blank <- function(v) if (is.null(v) || !nzchar(trimws(v))) NA else v
    p <- switch(input$ns_from,
      study = guarded(open_study(input$ns_src)$planner),
      sample = guarded(read_planner(file.path(
        system.file("extdata", "ard-spec", package = "rtfreporter"),
        c("report.xlsx", "study.xlsx")))),
      new_planner())
    if (is.null(p)) return()
    s <- guarded(create_study(
      trimws(input$ns_id),
      title = blank(input$ns_title), compound = blank(input$ns_compound),
      phase = blank(input$ns_phase),
      description = blank(input$ns_description), planner = p,
      root = trimws(input$ns_root)))
    if (is.null(s)) return()
    shiny::removeModal()
    rv$studies_ver <- rv$studies_ver + 1L
    set_study(s)
    notify(sprintf(t("Created %s"), s$meta$study_id))
    bslib::nav_select("nav", "outputs")
  })

  output$study_detail <- shiny::renderUI({
    if (!has_study()) {
      return(shiny::p(class = "text-muted",
                      t("Open a study from the list, or create one.")))
    }
    rv$ver
    m <- shiny::isolate(rv$meta)
    r <- shiny::isolate(rv$p$study[["rounding"]])
    v <- function(x) if (is.na(x)) "" else x
    lay <- study_layout()
    shiny::tagList(
      shiny::p(shiny::strong(m$study_id), shiny::br(),
               shiny::span(class = "small text-muted", rv$study$path)),
      shiny::textInput("m_title", t("Title"), v(m$title), width = "100%"),
      shiny::div(class = "d-flex gap-2",
                 shiny::textInput("m_compound", t("Compound"), v(m$compound)),
                 shiny::textInput("m_phase", t("Phase"), v(m$phase))),
      shiny::textAreaInput("m_description", t("Description"),
                           v(m$description), width = "100%"),
      shiny::selectInput(
        "rounding", t("Rounding"),
        stats::setNames(c("", "r", "sas"),
                        t(c("As options(rtfreporter.rounding)",
                            "r (half to even)", "sas (half away from zero)"))),
        selected = v(r)),
      shiny::tags$details(
        shiny::tags$summary(t("Folders")),
        shiny::tags$pre(class = "small", paste(
          sprintf("%-15s %s", paste0(lay, "/"), t(c(
            "Input data: ADaM", "Input data: SDTM", "Input data: other",
            "Definition workbooks (written on save)",
            "Report programs, autoexec_report.R",
            "Deliverable data: each Table's ARD (.rds)",
            "Deliverable reports: RTF", "Run logs"))), collapse = "\n"))),
      shiny::fileInput(
        "import",
        t("Import definition workbooks (replaces this study's definition)"),
        multiple = TRUE, accept = ".xlsx", width = "100%"),
      shiny::div(class = "d-flex flex-wrap gap-2 align-items-center",
                 shiny::downloadButton("spec_xlsx",
                                       t("Export the definition (Excel)"),
                                       class = "btn-sm"),
                 shiny::downloadButton("study_zip",
                                       t("Download the study (zip)"),
                                       class = "btn-sm"),
                 shiny::checkboxInput("zip_data", t("with data"))),
      .btn("study_folder", t("Open the study folder")))
  })
  for (k in c("title", "compound", "phase", "description")) local({
    key <- k
    shiny::observeEvent(input[[paste0("m_", key)]], {
      v <- trimws(input[[paste0("m_", key)]])
      v <- if (nzchar(v)) v else NA_character_
      if (!identical(rv$meta[[key]], v)) rv$meta[[key]] <- v
    }, ignoreInit = TRUE)
  })
  shiny::observeEvent(input$rounding, {
    v <- if (nzchar(input$rounding)) input$rounding else NA_character_
    if (!identical(unname(rv$p$study[["rounding"]]), v)) {
      rv$p$study[["rounding"]] <- v
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$study_folder, .open_folder(rv$study$path))
  shiny::observeEvent(input$import, {
    f <- input$import
    tmp <- file.path(tempfile("import"), f$name)
    dir.create(dirname(tmp[1L]))
    file.copy(f$datapath, tmp)
    s <- guarded(import_spec(current_study(), tmp))
    if (is.null(s)) return()
    p <- s$planner
    rv$p <- p
    rv$want <- output_ids(p)[1L]
    bump()
    notify(sprintf(t("Imported %d reports (not saved yet)"),
                   nrow(p$outputs)))
  })
  output$spec_xlsx <- shiny::downloadHandler(
    filename = function() paste0(rv$study$meta$study_id, "_spec.zip"),
    content = function(file) {
      d <- tempfile("spec")
      export_spec(current_study(), d)
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
  shiny::observe({
    rv$ver
    p <- shiny::isolate(rv$p)
    ids <- if (is.null(p)) character() else output_ids(p)
    cur <- shiny::isolate(input$target)
    ch <- if (is.null(p)) c("-" = .all_rows) else
      c(stats::setNames(.default_rows, t("Study defaults")),
        stats::setNames(ids, ids),
        stats::setNames(.all_rows, t("ALL (every row)")))
    want <- shiny::isolate(rv$want)
    rv$want <- NULL
    sel <- if (length(want) && !is.na(want) && want %in% ch) want else
      if (!is.null(cur) && cur %in% ch) cur else
        if (length(ids)) ids[1L] else .default_rows
    shiny::updateRadioButtons(session, "target", choices = ch, selected = sel)
  })
  target <- shiny::reactive(.target_value(input$target))
  current <- shiny::reactive({
    tg <- target()
    if (!is.null(rv$p) && !is.na(tg) && nzchar(tg) &&
        tg %in% rv$p$outputs$output_id) tg else NULL
  })

  # -- what the ARD says ---------------------------------------------------
  meta_of <- function(id) {
    rv$ard_ver
    if (!has_study() || is.null(id)) return(NULL)
    ard_info(rv$study, id)
  }
  meta_now <- shiny::reactive({
    rv$ard_ver
    if (!has_study()) return(NULL)
    id <- current()
    if (!is.null(id)) return(meta_of(id))
    .merge_meta(lapply(rv$p$outputs$output_id, meta_of))
  })

  # -- sheet grids -------------------------------------------------------
  for (sheet in c(table_sheets(), report_sheets())) local({
    sh <- sheet
    out_id <- paste0("hot_", sh)
    key <- shiny::reactive(paste(sh, input$target, rv$ver, rv$ard_ver,
                                 sep = "|"))
    output[[out_id]] <- rhandsontable::renderRHandsontable({
      shiny::req(has_study())
      tg <- target()
      d <- sheet_rows(shiny::isolate(rv$p), sh, tg)
      if (!identical(tg, "")) d$output_id <- NULL
      ch <- .std_choices(sh)
      gc <- grid_choices(sh, meta_now())
      for (cn in names(gc)) ch[[cn]] <- unique(c(gc[[cn]], ch[[cn]]))
      .grid(d, sh, key(), ch)
    })
    shiny::observeEvent(input[[out_id]], {
      h <- input[[out_id]]
      if (is.null(h$changes$changes) &&
          !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
      if (!identical(h$params$planner_key, key())) return()
      d <- rhandsontable::hot_to_r(h)
      rv$p <- set_sheet_rows(rv$p, sh, target(), d)
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
      if (is.null(d)) return(NULL)
      shiny::tags$details(
        class = "mt-2 small", open = NA,
        shiny::tags$summary(sprintf(
          t("Inherited from the study defaults (%d rows; edit them under Study defaults)"),
          nrow(d))),
        shiny::div(class = "rp-inherited",
                   shiny::tableOutput(paste0("inh_tbl_", sh))))
    })
    output[[paste0("inh_tbl_", sh)]] <- shiny::renderTable(
      inherited(), na = "", striped = TRUE, spacing = "xs")
    output[[paste0("help_", sh)]] <- DT::renderDT(
      .help_table(sh), rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
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
                         t("Choose a report in the sidebar to fill its rows from its ARD and from presets.")))
    }
    m <- meta_of(id)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::strong("ARD"),
      if (is.null(m)) {
        shiny::span(class = "small text-muted",
                    t("not read yet: Run and read the ARD."))
      } else {
        shiny::span(class = "small", sprintf(
          t("read %s: %d keys, %d variables, statistics %s"),
          m$fetched, length(m$keys), nrow(m$variables),
          paste(m$stats, collapse = ", ")))
      },
      .btn("fetch2", t("Run and read the ARD"),
           class = "btn-sm btn-outline-primary"))
  })

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
      shiny::selectInput("header_preset", t("Column header preset"),
                         names(header_presets()), width = "300px"),
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
    bslib::nav_select("table_sheet", sheet)
  }
  need_meta <- function() {
    m <- meta_of(current())
    if (is.null(m)) {
      notify(t("Read the ARD first (Run and read the ARD)."), "warning")
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
  do_fetch <- function() {
    id <- current()
    if (is.null(id)) return(notify(t("Choose a report"), "warning"))
    m <- NULL
    shiny::withProgress(message = sprintf(t("Running the data code of %s"),
                                          id), {
      m <- guarded(fetch_ard(current_study(), id))
    })
    if (is.null(m)) return()
    rv$ard_ver <- rv$ard_ver + 1L
    if (!is.null(m$error)) {
      notify(paste(t("The ARD was read, but normalize and rework failed:"),
                   m$error), "warning")
    } else {
      notify(sprintf(t("ARD read: %d keys, %d variables"),
                     length(m$keys), nrow(m$variables)))
    }
  }
  shiny::observeEvent(input$fetch, do_fetch())
  shiny::observeEvent(input$fetch2, do_fetch())
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
  # sheets are the study's.  The code is ard_spec_code() of the definition
  # as it stands; Run executes it with run_ard() and shows the ARD.
  ard_res <- shiny::reactiveVal(NULL)
  ard_cols <- new.env()
  data_columns <- function() {
    if (!has_study()) return(character())
    ds <- rv$p$ard$datasets
    unique(unlist(lapply(ds$path[!is.na(ds$path)], function(pth) {
      f <- file.path(rv$study$path, pth)
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
        method = c(ard_methods()$method, ard_functions()),
        dataset = p$ard$datasets$dataset,
        population_id = p$ard$populations$population_id,
        by = cols, variables = cols),
      datasets = list(path = if (has_study()) {
        f <- study_files(rv$study, "data")
        file.path(f$folder, f$file)
      }),
      populations = list(dataset = p$ard$datasets$dataset),
      study = list(key = c("id", "output")),
      list())
  }
  for (sheet in names(.ard_spec_sheets)) local({
    sh <- sheet
    out_id <- paste0("hot_ard_", sh)
    by_report <- sh == "analyses"
    key <- shiny::reactive(paste("ard", sh, if (by_report) input$target,
                                 rv$ver, sep = "|"))
    output[[out_id]] <- rhandsontable::renderRHandsontable({
      shiny::req(has_study())
      tg <- if (by_report) target() else ""
      d <- ard_rows(shiny::isolate(rv$p), sh, tg)
      if (by_report && !identical(tg, "") && !is.na(tg)) d$output_id <- NULL
      .grid(d, sh, key(), shiny::isolate(ard_choices(sh)))
    })
    shiny::observeEvent(input[[out_id]], {
      h <- input[[out_id]]
      if (is.null(h$changes$changes) &&
          !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
      if (!identical(h$params$planner_key, key())) return()
      d <- rhandsontable::hot_to_r(h)
      tg <- if (by_report) target() else ""
      rv$p <- set_ard_rows(rv$p, sh, tg, d)
    })
  })
  output$ard_methods <- DT::renderDT(
    ard_methods()[c("method", "call", "defaults", "note")], rownames = FALSE,
    options = list(dom = "t", paging = FALSE, ordering = FALSE))
  ard_valid <- shiny::reactive({
    shiny::req(has_study())
    tryCatch({
      ard_spec(rv$p$ard)
      NULL
    }, error = function(e) conditionMessage(e))
  })
  output$ard_check <- shiny::renderUI({
    msg <- ard_valid()
    if (is.null(msg)) {
      n <- nrow(rv$p$ard$analyses)
      return(shiny::p(class = "small text-success mt-2",
                      sprintf(t("%d analyses; the definition reads without errors."), n)))
    }
    shiny::div(class = "alert alert-warning py-1 small mt-2",
               shiny::tags$pre(class = "mb-0", msg))
  })
  # the output the sidebar names: a report, or an output the ARD
  # definition has analyses for and the report list does not have yet
  ard_target <- shiny::reactive({
    tg <- target()
    if (is.na(tg) || !nzchar(tg)) NULL else tg
  })
  ard_scope_id <- shiny::reactive({
    if (identical(input$ard_scope, "study")) NULL else ard_target()
  })
  output$ard_code <- shiny::renderText({
    shiny::req(has_study())
    a <- rv$p$ard
    if (!nrow(a$analyses)) {
      return(t("No analyses yet: add rows to the analyses sheet."))
    }
    id <- ard_scope_id()
    if (identical(input$ard_scope, "report") && is.null(id)) {
      return(t("Choose a report in the sidebar, or show the whole study."))
    }
    if (!is.null(id) && !any(a$analyses$output_id %in% id)) {
      return(sprintf(t("%s has no analyses in the ARD definition."), id))
    }
    tryCatch(paste(ard_spec_code(structure(a, class = "ard_spec"),
                                 output_id = id), collapse = "\n"),
             error = function(e) paste(t("The code cannot be written yet:"),
                                       conditionMessage(e)))
  })
  run_ard_now <- function(output_id) {
    if (!is.null(ard_valid())) {
      return(notify(t("Correct the ARD definition first."), "warning"))
    }
    r <- NULL
    shiny::withProgress(message = t("Running the ARD code"), {
      r <- guarded(run_ard(current_study(), output_id = output_id))
    })
    if (is.null(r)) return()
    r$scope <- output_id %||% t("the whole study")
    ard_res(r)
    bslib::nav_select("ard_right", "result")
    if (!is.null(r$error)) notify(t("The ARD code failed: see the log."), "error")
  }
  shiny::observeEvent(input$ard_run, {
    if (is.null(ard_target())) return(notify(t("Choose a report"), "warning"))
    run_ard_now(ard_target())
  })
  shiny::observeEvent(input$ard_build, {
    if (dirty() && !do_save()) return()
    run_ard_now(NULL)
    rv$status_ver <- rv$status_ver + 1L
  })
  output$ard_run_info <- shiny::renderUI({
    r <- ard_res()
    if (is.null(r)) {
      return(shiny::p(class = "small text-muted mt-2",
                      t("Run the analyses to see the ARD they make.")))
    }
    if (!is.null(r$error)) {
      return(shiny::div(class = "alert alert-danger py-1 small mt-2",
                        shiny::tags$pre(class = "mb-0", r$error)))
    }
    a <- r$ard
    tab <- table(factor(a$analysis_id, levels = unique(a$analysis_id)))
    shiny::div(
      class = "small mt-2",
      shiny::p(sprintf(t("%s: %d rows in %.1f s"), r$scope, nrow(a), r$seconds),
               " ", paste(sprintf("%s %d", names(tab), as.integer(tab)),
                          collapse = ", ")))
  })
  output$ard_table <- DT::renderDT({
    r <- ard_res()
    shiny::req(r, r$ard)
    DT::datatable(ard_view(r$ard), rownames = FALSE, filter = "top",
                  selection = "none",
                  options = list(pageLength = 20, scrollX = TRUE,
                                 dom = "tip"))
  })

  # -- the table builder -------------------------------------------------
  # The form is drawn once per report (and whenever the builder tab is
  # opened), with input ids fresh each time: an input of the form before
  # can never be read as one of this form's.  Its edits write the sheets
  # (builder_write()), and the grids redraw when their tab is opened.
  bform <- new.env()
  bform$n <- 0L
  bform_drawn <- shiny::reactiveVal(0L)
  rv$bver <- 0L
  rv$btouched <- FALSE
  shiny::observeEvent(input$nav, {
    if (identical(input$nav, "builder")) rv$bver <- rv$bver + 1L
    if (input$nav %in% c("table_spec", "report_spec") && rv$btouched) {
      rv$btouched <- FALSE
      bump()
    }
  })
  builder_case <- shiny::reactive({
    id <- current()
    if (is.null(id)) return("none")
    if (!identical(report_info(rv$p, id)$type, "table")) return("type")
    m <- meta_of(id)
    if (is.null(m)) return("meta")
    if (length(m$hierarchy)) return("hierarchy")
    "ok"
  })
  output$builder_note <- shiny::renderUI({
    msg <- switch(builder_case(),
      none = t("Choose a report in the sidebar."),
      type = t("The builder is for Tables; Listings and Figures are made in their data code."),
      meta = t("Read the report's ARD first (Reports > Data code > Run and read the ARD): the builder is built from it."),
      hierarchy = t("This table has a hierarchy (SOC / PT): the builder handles summary tables for now. Use Table definition; the preview works."),
      NULL)
    if (is.null(msg)) return(shiny::p(class = "small text-muted",
      t("Drag to order, type to rename; every change is written to the sheets (Table definition) and shown on the right.")))
    shiny::div(class = "alert alert-info py-2 small", msg)
  })
  bid <- function(x) paste0("b", bform$n, "_", x)
  output$builder_form <- shiny::renderUI({
    rv$bver
    rv$ard_ver
    shiny::req(identical(builder_case(), "ok"))
    id <- current()
    m <- meta_of(id)
    st <- builder_read(shiny::isolate(rv$p), id, m)
    bform$n <- bform$n + 1L
    bform_drawn(bform$n)
    bform$id <- id
    bform$st <- st
    bform$meta <- m
    v <- st$variables
    bs <- builder_stats()
    keys <- unique(c(m$by, names(m$keys)))
    var_items <- stats::setNames(lapply(seq_len(nrow(v)), function(i)
      shiny::span(class = "rp-b-var", shiny::strong(v$variable[i]),
                  shiny::span(class = "rp-b-kind", v$kind[i]))), v$variable)
    shiny::tagList(
      shiny::div(
        class = "rp-b-card",
        shiny::h6(t("Columns")),
        shiny::selectInput(bid("key"), t("Column variable"), keys,
                           selected = st$key, width = "240px"),
        shiny::uiOutput(bid("arms_ui")),
        shiny::radioButtons(
          bid("header"), t("Column header"),
          stats::setNames(c("keep", names(header_presets())),
                          c(t("as it is"), names(header_presets()))),
          selected = "keep", inline = TRUE)),
      shiny::div(
        class = "rp-b-card",
        shiny::h6(t("Rows: variables in order")),
        sortable::rank_list(text = NULL, labels = var_items,
                            input_id = bid("vars")),
        bslib::accordion(
          open = FALSE,
          lapply(seq_len(nrow(v)), function(i) {
            bslib::accordion_panel(
              title = paste0(v$variable[i],
                             if (!is.na(v$label[i])) paste0(": ", v$label[i])),
              value = v$variable[i],
              shiny::textInput(bid(paste0("lab", i)), t("Label"),
                               value = if (is.na(v$label[i])) "" else
                                 v$label[i], width = "100%"),
              if (identical(v$kind[i], "categorical") &&
                  length(st$levels[[v$variable[i]]])) {
                shiny::tagList(
                  shiny::tags$label(class = "form-label small",
                                    t("Levels (drag to order)")),
                  sortable::rank_list(
                    text = NULL, labels = st$levels[[v$variable[i]]],
                    input_id = bid(paste0("lv", i)),
                    orientation = "horizontal"))
              })
          }))),
      shiny::div(
        class = "rp-b-card",
        shiny::h6(t("Statistics")),
        shiny::checkboxGroupInput(
          bid("stats"), t("Continuous variables"),
          stats::setNames(bs$key, bs$row), selected = st$stats),
        shiny::numericInput(
          bid("dec"), t("Decimals the data are collected with"),
          value = st$decimals, min = 0, max = 6, width = "260px"),
        shiny::p(class = "small text-muted",
                 t("Mean, median and quartiles get one more, SD two more, Min / Max the same.")),
        shiny::radioButtons(
          bid("cat"), t("Categorical variables"),
          stats::setNames(.cat_formats()$key, .cat_formats()$label),
          selected = st$cat_format, inline = TRUE),
        shiny::numericInput(bid("pct"), t("Decimals of the percent"),
                            value = st$pct_decimals, min = 0, max = 3,
                            width = "260px"),
        shiny::uiOutput(bid("warn"))))
  })
  # the arms follow the column variable chosen
  shiny::observe({
    bform_drawn()
    k <- input[[bid("key")]]
    shiny::req(identical(builder_case(), "ok"), !is.null(k))
    n <- bform$n
    arms <- if (identical(k, bform$st$key)) bform$st$arms else
      bform$meta$keys[[k]]
    output[[paste0("b", n, "_arms_ui")]] <- shiny::renderUI(shiny::tagList(
      shiny::tags$label(class = "form-label small",
                        t("Order of the columns (drag)")),
      sortable::rank_list(text = NULL, labels = arms,
                          input_id = paste0("b", n, "_arms"),
                          orientation = "horizontal")))
  })
  bstate <- shiny::reactive({
    bform_drawn()
    shiny::req(identical(builder_case(), "ok"),
               identical(bform$id, current()))
    st <- bform$st
    get <- function(x) input[[bid(x)]]
    vars <- get("vars")
    shiny::req(!is.null(vars), !is.null(get("key")), !is.null(get("arms")),
               setequal(vars, st$variables$variable))
    v <- st$variables[match(vars, st$variables$variable), , drop = FALSE]
    idx <- match(vars, st$variables$variable)
    v$label <- vapply(seq_along(vars), function(k) {
      l <- get(paste0("lab", idx[k]))
      if (is.null(l)) v$label[k] else if (nzchar(trimws(l))) l else NA
    }, "")
    lev <- stats::setNames(lapply(seq_along(vars), function(k)
      get(paste0("lv", idx[k])) %||% st$levels[[vars[k]]]), vars)
    num <- function(x, d) {
      x <- suppressWarnings(as.numeric(get(x)))
      if (length(x) != 1L || is.na(x)) d else max(0, round(x))
    }
    stats <- builder_stats()$key
    list(key = get("key"), arms = get("arms"), variables = v, levels = lev,
         stats = stats[stats %in% get("stats")],
         decimals = num("dec", st$decimals),
         cat_format = get("cat") %||% st$cat_format,
         pct_decimals = num("pct", st$pct_decimals),
         header = get("header") %||% "keep",
         auto_levels = st$auto_levels)
  })
  bstate_d <- shiny::debounce(bstate, 400)
  shiny::observeEvent(bstate_d(), {
    st <- bstate_d()
    id <- bform$id
    p2 <- guarded(builder_write(rv$p, id, st))
    if (!is.null(p2) && !identical(p2, rv$p)) {
      rv$p <- p2
      rv$btouched <- TRUE
    }
  })
  shiny::observe({
    st <- bstate()
    m <- bform$meta
    n <- bform$n
    tpl <- c(builder_stats()$template[builder_stats()$key %in% st$stats],
             .cat_template(st$cat_format, st$pct_decimals))
    miss <- if (!is.null(m)) .missing_stats(tpl, m$stats) else character()
    output[[paste0("b", n, "_warn")]] <- shiny::renderUI(
      if (length(miss)) shiny::div(
        class = "alert alert-warning py-1 small",
        sprintf(t("The ARD has no %s: those cells stay empty. Add them to the ARD code."),
                paste(miss, collapse = ", "))))
  })
  preview <- shiny::reactive({
    id <- current()
    shiny::req(!is.null(id), identical(report_info(rv$p, id)$type, "table"))
    rv$ard_ver
    d <- ard_data(rv$study, id)
    if (is.null(d)) return(list(error = t("No data to preview: Run and read the ARD.")))
    tryCatch(list(pages = preview_pages(rv$p, id, d)),
             error = function(e) list(error = conditionMessage(e)))
  })
  preview_d <- shiny::debounce(preview, 300)
  output$builder_preview <- shiny::renderUI({
    pv <- preview_d()
    if (!is.null(pv$error)) {
      return(shiny::div(class = "small text-muted", pv$error))
    }
    preview_html(pv$pages)
  })
  output$builder_pages <- shiny::renderUI({
    pv <- preview_d()
    if (is.null(pv$pages)) return(NULL)
    shiny::span(class = "small text-muted",
                sprintf(t("%d pages"), length(pv$pages)))
  })

  # -- report list -------------------------------------------------------
  outputs_view <- shiny::reactive({
    p <- rv$p
    empty <- data.frame(output_id = character(), type = character(),
                        program = character(), rtf = character(),
                        data = character(), description = character())
    if (is.null(p) || !nrow(p$outputs)) return(empty)
    o <- p$outputs
    info <- lapply(o$output_id, function(id) report_info(p, id))
    data.frame(
      output_id = o$output_id,
      type = unname(.type_labels[vapply(info, `[[`, "", "type")]),
      program = vapply(info, `[[`, "", "program"),
      rtf = vapply(info, `[[`, "", "file"),
      data = ifelse(is.na(o$data_code), "TODO", "\u2713"),
      description = ifelse(is.na(o$description), "", o$description),
      stringsAsFactors = FALSE)
  })
  output$outputs <- DT::renderDT({
    v <- outputs_view()
    sel <- match(shiny::isolate(input$target), v$output_id)
    names(v) <- t(c("output_id", "Type", "Program", "RTF", "Data code",
                    "Description"))
    DT::datatable(v, rownames = FALSE,
                  selection = list(mode = "single",
                                   selected = if (!is.na(sel)) sel),
                  options = list(dom = "t", paging = FALSE,
                                 ordering = FALSE, scrollX = TRUE))
  })
  shiny::observeEvent(input$target, {
    v <- outputs_view()
    sel <- match(input$target, v$output_id)
    if (!identical(input$outputs_rows_selected, if (!is.na(sel)) sel)) {
      DT::selectRows(DT::dataTableProxy("outputs"), if (!is.na(sel)) sel)
    }
  })
  shiny::observeEvent(input$outputs_rows_selected, {
    v <- shiny::isolate(outputs_view())
    id <- v$output_id[input$outputs_rows_selected]
    if (length(id) && !identical(id, input$target)) {
      shiny::updateRadioButtons(session, "target", selected = id)
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

  editors <- c(description = "text", data_code = "area",
               process_code = "area")
  shiny::observeEvent(list(current(), rv$ver), {
    id <- current()
    o <- rv$p$outputs[rv$p$outputs$output_id %in% id, , drop = FALSE]
    val <- function(v) if (length(v) && !is.na(v)) v else ""
    owner <- if (!is.null(id)) list(study = study_key(), id = id)
    for (e in names(editors)) {
      fill(e, val(o[[e]]), owner,
           if (editors[[e]] == "text") shiny::updateTextInput else
             shiny::updateTextAreaInput)
    }
  })
  output$current_label <- shiny::renderUI({
    id <- current()
    if (is.null(id)) {
      shiny::p(class = "text-muted",
               t("Choose a report in the list or the sidebar."))
    } else {
      shiny::h5(id, shiny::span(class = "badge bg-secondary ms-1",
                                .type_labels[[report_info(rv$p, id)$type]]))
    }
  })
  set_field <- function(field, value) {
    owner <- edit_of(field, value)
    if (is.null(owner) || !identical(owner$study, study_key())) return()
    value <- if (is.null(value) || !nzchar(trimws(value))) NA_character_ else
      value
    i <- which(rv$p$outputs$output_id == owner$id)
    if (length(i) && !identical(rv$p$outputs[[field]][i], value)) {
      rv$p$outputs[[field]][i] <- value
    }
  }
  for (e in names(editors)) local({
    field <- e
    shiny::observeEvent(input[[field]], set_field(field, input[[field]]),
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

  output$program <- shiny::renderText({
    id <- current()
    if (is.null(id)) return("")
    paste(program_code(rv$p, id), collapse = "\n")
  })
  output$program_state <- shiny::renderUI({
    id <- current()
    if (is.null(id)) return(NULL)
    rv$status_ver
    f <- file.path(rv$study$path, study_layout()[["programs"]],
                   report_info(rv$p, id)$program)
    st <- .program_state(rv$p, id, f)
    msg <- t(switch(st,
      missing = "No file yet. Saving writes the program below.",
      current = "The saved program is the one below.",
      todo = "The saved program is the one below (its data part is a TODO).",
      generated = "The definition has changed: saving rewrites the program as below (a program nobody edited follows the definition).",
      edited = "The saved program was edited by hand. Saving leaves it alone."))
    shiny::div(
      class = "d-flex gap-2 align-items-center mb-2 small",
      shiny::span(msg),
      if (identical(st, "edited")) {
        .btn("regenerate", t("Regenerate this program"),
             class = "btn-sm btn-outline-warning")
      })
  })
  shiny::observeEvent(input$regenerate, {
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Regenerate %s"), current()),
      t("The program on disk is written again from the definition and the data code. Its hand edits are lost."),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("regenerate_ok", t("Regenerate"),
                                   class = "btn-warning"))))
  })
  shiny::observeEvent(input$regenerate_ok, {
    shiny::removeModal()
    do_save(regenerate = current())
  })

  ask_id <- function(title, button, value = "", type = FALSE) {
    shiny::showModal(shiny::modalDialog(
      title = title,
      shiny::textInput("modal_id", "output_id", value = value),
      if (type) shiny::radioButtons(
        "modal_type", t("Type"),
        stats::setNames(report_types(), .type_labels[report_types()]),
        inline = TRUE),
      if (type) shiny::textInput("modal_desc", t("Description"),
                                 width = "100%"),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(button, "OK", class = "btn-primary")),
      easyClose = TRUE))
  }
  after_id_change <- function(p, id) {
    if (is.null(p)) return()
    rv$p <- p
    rv$want <- id
    shiny::removeModal()
    bump()
  }
  need_current <- function() {
    if (is.null(current())) notify(t("Choose a report"), "warning")
    !is.null(current())
  }
  shiny::observeEvent(input$add, ask_id(t("Add a report"), "add_ok",
                                        type = TRUE))
  shiny::observeEvent(input$add_ok, {
    id <- trimws(input$modal_id)
    d <- trimws(input$modal_desc)
    after_id_change(guarded(add_output(
      rv$p, id, description = if (nzchar(d)) d else NA,
      type = input$modal_type)), id)
  })
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
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Delete %s"), current()),
      t("The report's rows are deleted from every sheet (on save; its program and outputs stay on disk)."),
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

  # -- data ----------------------------------------------------------------
  data_ver <- shiny::reactiveVal(0L)
  data_files <- shiny::reactive({
    data_ver()
    input$data_refresh
    shiny::req(has_study())
    study_files(rv$study, "data")
  })
  output$data_files <- DT::renderDT({
    d <- data_files()[c("folder", "file", "size_kb", "modified")]
    names(d) <- t(c("Folder", "File", "KB", "Modified"))
    .dt(d, scrollY = "360px")
  })
  shiny::observeEvent(input$data_upload, {
    f <- input$data_upload
    dest <- file.path(rv$study$path, study_layout()[[input$data_folder]])
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(f$datapath, file.path(dest, f$name), overwrite = TRUE)
    data_ver(data_ver() + 1L)
    notify(sprintf(t("%d files added to %s"), sum(ok),
                   study_layout()[[input$data_folder]]))
  })
  shiny::observeEvent(input$data_open, .open_folder(
    file.path(rv$study$path, "data")))
  data_head <- shiny::reactive({
    i <- input$data_files_rows_selected
    shiny::req(length(i))
    guarded(read_data_head(data_files()$path[i]))
  })
  output$data_head <- DT::renderDT({
    d <- data_head()
    shiny::req(d)
    DT::datatable(d, rownames = FALSE, selection = "none",
                  options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                                 scrollY = "420px"))
  })
  output$data_dim <- shiny::renderUI({
    d <- data_head()
    shiny::req(d)
    full <- attr(d, "dim_full")
    shiny::p(class = "small text-muted",
             sprintf(t("%s rows x %s columns"),
                     format(full[1L], big.mark = ","), full[2L]))
  })

  # -- results -------------------------------------------------------------
  status <- shiny::reactive({
    rv$status_ver
    input$status_refresh
    shiny::req(has_study())
    study_status(current_study())
  })
  output$status <- DT::renderDT({
    d <- status()
    v <- data.frame(
      a = d$output_id, b = unname(.type_labels[d$type]), c = d$program,
      d = t(unname(c(missing = "none", todo = "TODO",
                     current = "as generated",
                     generated = "as generated (definition changed)",
                     edited = "edited by hand")[d$program_state])),
      e = ifelse(is.na(d$ard), "", d$ard), f = ifelse(is.na(d$rtf), "", d$rtf),
      g = t(unname(.status_labels[d$status])), stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Type", "Program", "Program state", "ARD",
                    "RTF", "Status"))
    DT::formatStyle(
      .dt(v, selection = "multiple"), names(v)[7L],
      color = DT::styleEqual(
        t(unname(.status_labels[c("error", "todo", "outdated", "unsaved",
                                  "ok")])),
        c("#b91c1c", "#b45309", "#b45309", "#b45309", "#15803d")))
  })
  selected_status <- shiny::reactive({
    d <- status()
    d[input$status_rows_selected, , drop = FALSE]
  })
  output$log <- shiny::renderText({
    d <- selected_status()
    rv$status_ver
    if (!nrow(d)) {
      f <- file.path(rv$study$path, study_layout()[["logs"]],
                     "autoexec_report.log")
      if (!file.exists(f)) return(t("Choose a report to see its log."))
    } else {
      f <- d$log[1L]
      if (is.na(f)) return(sprintf(t("%s has not been run yet."),
                                   d$output_id[1L]))
    }
    paste(utils::tail(readLines(f, warn = FALSE, encoding = "UTF-8"), 300L),
          collapse = "\n")
  })
  output$rtf_download <- shiny::downloadHandler(
    filename = function() {
      d <- selected_status()
      if (nrow(d)) basename(d$rtf_path[1L]) else "none.rtf"
    },
    content = function(file) {
      d <- selected_status()
      if (!nrow(d) || !file.exists(d$rtf_path[1L])) stop("No RTF")
      file.copy(d$rtf_path[1L], file)
    })
  shiny::observeEvent(input$tfl_open, .open_folder(
    file.path(rv$study$path, study_layout()[["tfl"]])))
  shiny::observeEvent(input$check, {
    r <- check_planner(rv$p)
    output$check_result <- DT::renderDT(.dt(r, selection = "none"))
    if (all(r$ok)) notify(t("The definition reads without errors")) else
      notify(t("The definition has errors (see Definition check)"), "error")
  })

  start_run <- function(ids, what) {
    if (!is.null(rv$job)) return(notify(t("A run is going on"), "warning"))
    if (dirty() && !do_save()) return()
    px <- guarded(run_study(rv$study, ids, wait = FALSE))
    if (is.null(px)) return()
    rv$job <- px
    rv$job_what <- what
  }
  shiny::observeEvent(input$run_all, start_run(NULL, t("every report")))
  shiny::observeEvent(input$run_selected, {
    d <- selected_status()
    if (!nrow(d)) return(notify(t("Choose a report"), "warning"))
    start_run(d$output_id, paste(d$output_id, collapse = ", "))
  })
  shiny::observe({
    px <- rv$job
    if (is.null(px)) return()
    if (px$is_alive()) {
      shiny::invalidateLater(1000)
      return()
    }
    rc <- px$get_exit_status()
    shiny::isolate({
      notify(sprintf(if (identical(rc, 0L)) t("Finished: %s") else
                       t("Finished with errors: %s"), rv$job_what),
             if (identical(rc, 0L)) "message" else "warning")
      rv$job <- NULL
      rv$status_ver <- rv$status_ver + 1L
    })
  })
  output$job <- shiny::renderUI({
    if (is.null(rv$job)) return(NULL)
    shiny::div(class = "alert alert-info py-2 small my-2",
               shiny::span(class = "spinner-border spinner-border-sm me-2"),
               sprintf(t("Running %s ..."), rv$job_what))
  })
  session$onSessionEnded(function() {
    px <- shiny::isolate(rv$job)
    if (!is.null(px) && px$is_alive()) px$kill()
  })
}
