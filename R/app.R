# The app.  A study is opened first; everything else is that study.  The
# study's planner lives in a reactiveValues and every grid shows a slice of
# one sheet (the rows of the report chosen in the sidebar) and writes the
# slice back when it is edited.  A grid is redrawn only when the slice it
# shows changes from outside (another report chosen, the study opened, a
# report copied), never because it was edited itself.  Nothing reaches the
# disk until the user saves.

.all_rows <- "__all__"
.default_rows <- "__default__"
.study_tabs <- c("outputs", "table_spec", "report_spec", "data", "results")

# Values a column takes, offered as a dropdown (anything else may still be
# typed: rtfreporter checks it when the workbook is read).
.bool <- c("TRUE", "FALSE")
.choices <- list(
  tables = list(stats = c("cells", "rows"), value = c("stat", "stat_fmt")),
  layout = list(pages_split = c("group_safe", "group_force"),
                blank_where = c("between_groups"),
                group_page = .bool, group_show = .bool, blank_first = .bool,
                blank_last = .bool, blank_counted = .bool,
                stub_before = .bool),
  columns = list(row_title = .bool, decimal_split = .bool, hide = .bool),
  style = list(align_count_pct = .bool, auto_width = .bool),
  col_header = list(bold = .bool, align = c("left", "center", "right"),
                    span = c("each")),
  report = list(type = c("table", "listing", "figure"),
                auto_section = .bool, auto_title = .bool,
                page_header = .bool, page_footer = .bool),
  page = list(orientation = c("portrait", "landscape"),
              paper_size = c("letter", "A4")))

.sheet_labels <- c(
  tables = "tables \u8868", variables = "variables \u5909\u6570",
  cells = "cells \u30bb\u30eb", layout = "layout \u30da\u30fc\u30b8\u5272",
  columns = "columns \u5217", style = "style \u66f8\u5f0f",
  col_header = "col_header \u5217\u898b\u51fa\u3057",
  report = "report \u5e33\u7968", page = "page \u7528\u7d19",
  header = "header \u30d8\u30c3\u30c0", footer = "footer \u30d5\u30c3\u30bf",
  titles = "titles \u30bf\u30a4\u30c8\u30eb", footnotes = "footnotes \u811a\u6ce8")

.type_labels <- c(table = "Table", listing = "Listing", figure = "Figure")

.status_labels <- c(
  "no program" = "\u672a\u4f5c\u6210\uff08\u4fdd\u5b58\u3067\u751f\u6210\uff09", unsaved = "\u672a\u4fdd\u5b58\uff08\u4fdd\u5b58\u3067\u66f4\u65b0\uff09",
  todo = "TODO\uff08\u30c7\u30fc\u30bf\u90e8\u672a\u8a18\u5165\uff09",
  "not run" = "\u672a\u5b9f\u884c", error = "\u30a8\u30e9\u30fc", outdated = "\u8981\u518d\u5b9f\u884c", ok = "OK")

#' Start the rtfplanner app
#'
#' Opens the study manager.  Choose a study -- it opens as it was last
#' saved -- or create or register one, then define its reports (Tables from
#' the table definition; Listings and Figures from their programs), their
#' data code, and run them.  The first time, rtfplanner's home is set up
#' with the defaults ([setup_rtfplanner()]).
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
  if (!.is_set_up()) setup_rtfplanner()
  start <- if (!is.null(study)) open_study(study)
  shiny::shinyApp(app_ui(), function(input, output, session)
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
"

.sheet_panel <- function(sheet) {
  bslib::nav_panel(
    .sheet_labels[[sheet]],
    rhandsontable::rHandsontableOutput(paste0("hot_", sheet)),
    shiny::tags$details(
      class = "rp-help mt-2",
      shiny::tags$summary("\u5217\u306e\u8aac\u660e (_README)"),
      DT::DTOutput(paste0("help_", sheet))))
}

.grid_note <- shiny::p(
  class = "text-muted small mb-1",
  "\u884c\u306e\u8ffd\u52a0\u30fb\u524a\u9664\u306f\u53f3\u30af\u30ea\u30c3\u30af\u3002Excel \u304b\u3089\u306e\u8cbc\u308a\u4ed8\u3051\u53ef\u3002",
  "\u7a7a\u6b04 = \u672a\u6307\u5b9a\u30021 \u30bb\u30eb\u306b\u8907\u6570\u306f | \u533a\u5207\u308a\u3002")

.btn <- function(id, label, class = "btn-sm", ...) {
  shiny::actionButton(id, label, class = class, ...)
}

app_ui <- function() {
  two <- bslib::breakpoints(sm = 12, lg = c(5, 7))
  bslib::page_navbar(
    id = "nav",
    title = "rtfplanner",
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    header = shiny::tags$style(.code_css),
    sidebar = bslib::sidebar(
      width = 250,
      shiny::uiOutput("study_side"),
      shiny::radioButtons("target", "\u5bfe\u8c61\u306e\u5e33\u7968",
                          choices = c("(\u5168\u884c)" = .all_rows)),
      shiny::p(class = "text-muted small",
               "\u5404\u30b7\u30fc\u30c8\u306f\u9078\u3093\u3060\u5e33\u7968\u306e\u884c\u3060\u3051\u3092\u8868\u793a\u30fb\u7de8\u96c6\u3057\u307e\u3059\u3002",
               "\u300c\u65e2\u5b9a\u300d\u306f output_id \u304c\u7a7a\u6b04\u306e\u884c\uff08\u8a66\u9a13\u5168\u4f53\u306e\u30c7\u30d5\u30a9\u30eb\u30c8\uff09\u3002")),

    bslib::nav_panel(
      "\u8a66\u9a13", value = "study",
      bslib::layout_columns(
        col_widths = two,
        bslib::card(
          bslib::card_header("\u8a66\u9a13\u4e00\u89a7"),
          DT::DTOutput("studies"),
          shiny::div(class = "d-flex flex-wrap gap-2",
                     .btn("open_study", "\u958b\u304f", class = "btn-sm btn-primary"),
                     .btn("new_study", "\u65b0\u898f\u8a66\u9a13"),
                     .btn("register", "\u65e2\u5b58\u30d5\u30a9\u30eb\u30c0\u3092\u767b\u9332"),
                     .btn("unregister", "\u767b\u9332\u89e3\u9664",
                          class = "btn-sm btn-outline-danger"),
                     .btn("refresh_studies", "\u66f4\u65b0")),
          shiny::tags$details(
            class = "mt-2 small",
            shiny::tags$summary("\u8a2d\u5b9a"),
            shiny::uiOutput("settings"))),
        bslib::card(
          bslib::card_header("\u3053\u306e\u8a66\u9a13"),
          shiny::uiOutput("study_detail")))),

    bslib::nav_panel(
      "\u5e33\u7968\u4e00\u89a7", value = "outputs",
      bslib::layout_columns(
        col_widths = two,
        bslib::card(
          bslib::card_header("\u5e33\u7968 (TFL)"),
          DT::DTOutput("outputs"),
          shiny::div(
            class = "d-flex flex-wrap gap-1",
            .btn("add", "\u8ffd\u52a0"), .btn("copy", "\u8907\u88fd"),
            .btn("rename", "\u540d\u524d\u5909\u66f4"),
            .btn("remove", "\u524a\u9664", class = "btn-sm btn-outline-danger"),
            .btn("up", "\u2191"), .btn("down", "\u2193")),
          shiny::p(class = "text-muted small mt-1",
                   "\u9806\u756a = autoexec_report.R \u306e\u5b9f\u884c\u9806\u3002",
                   "\u8907\u88fd\u306f\u5168\u30b7\u30fc\u30c8\u306e\u884c\u3054\u3068\u30b3\u30d4\u30fc\u3057\u307e\u3059\u3002")),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "\u30c7\u30fc\u30bf\u6e96\u5099\u30b3\u30fc\u30c9",
            shiny::uiOutput("current_label"),
            shiny::textInput("description", "\u8aac\u660e", width = "100%"),
            shiny::div(
              class = "rp-code",
              shiny::textAreaInput(
                "data_code",
                "\u3053\u306e\u5e33\u7968\u306e\u30c7\u30fc\u30bf\u6e96\u5099\uff08Table \u306f `data` \u3092\u3001Listing / Figure \u306f `content` \u3092\u4f5c\u308b\u3002\u7a7a\u6b04 = TODO\uff09",
                rows = 12, width = "100%", resize = "vertical"),
              shiny::textAreaInput(
                "setup",
                "\u5168\u5e33\u7968\u5171\u901a\u306e\u524d\u51e6\u7406\uff08library()\u3001\u5171\u901a\u30c7\u30fc\u30bf\u306e\u8aad\u307f\u8fbc\u307f\u306a\u3069\uff09",
                rows = 5, width = "100%", resize = "vertical"))),
          bslib::nav_panel(
            "\u751f\u6210\u3055\u308c\u308b\u30d7\u30ed\u30b0\u30e9\u30e0",
            shiny::uiOutput("program_state"),
            shiny::div(class = "rp-code",
                       shiny::verbatimTextOutput("program")))))),

    bslib::nav_panel(
      "\u8868\u306e\u5b9a\u7fa9 (table_spec)", value = "table_spec",
      shiny::uiOutput("type_note"),
      .grid_note,
      do.call(bslib::navset_card_underline,
              lapply(table_sheets(), .sheet_panel))),

    bslib::nav_panel(
      "\u5e33\u7968\u306e\u4f53\u88c1 (report_spec)", value = "report_spec",
      .grid_note,
      do.call(bslib::navset_card_underline,
              lapply(report_sheets(), .sheet_panel))),

    bslib::nav_panel(
      "\u30c7\u30fc\u30bf", value = "data",
      bslib::layout_columns(
        col_widths = two,
        bslib::card(
          bslib::card_header("\u5165\u529b\u30c7\u30fc\u30bf (data/)"),
          shiny::div(
            class = "d-flex gap-2 align-items-end",
            shiny::selectInput("data_folder", "\u53d6\u308a\u8fbc\u307f\u5148",
                               c("adam", "sdtm", "other"), width = "110px"),
            shiny::fileInput("data_upload", "\u30d5\u30a1\u30a4\u30eb\u3092\u53d6\u308a\u8fbc\u3080",
                             multiple = TRUE)),
          DT::DTOutput("data_files"),
          shiny::div(class = "d-flex gap-2",
                     .btn("data_refresh", "\u66f4\u65b0"),
                     .btn("data_open", "\u30d5\u30a9\u30eb\u30c0\u3092\u958b\u304f"))),
        bslib::card(
          bslib::card_header("\u4e2d\u8eab\uff08\u5148\u982d 50 \u884c\uff09"),
          shiny::uiOutput("data_dim"),
          DT::DTOutput("data_head")))),

    bslib::nav_panel(
      "\u6210\u679c\u7269", value = "results",
      bslib::card(
        bslib::card_header("\u5e33\u7968\u306e\u72b6\u614b"),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          .btn("run_selected", "\u9078\u629e\u3057\u305f\u5e33\u7968\u3092\u5b9f\u884c",
               class = "btn-sm btn-primary"),
          .btn("run_all", "\u5168\u3066\u5b9f\u884c", class = "btn-sm btn-primary"),
          .btn("status_refresh", "\u72b6\u614b\u3092\u66f4\u65b0"),
          .btn("check", "\u5b9a\u7fa9\u30c1\u30a7\u30c3\u30af"),
          .btn("tfl_open", "output/tfl \u3092\u958b\u304f"),
          shiny::downloadButton("rtf_download", "RTF \u3092\u30c0\u30a6\u30f3\u30ed\u30fc\u30c9",
                                class = "btn-sm")),
        shiny::uiOutput("job"),
        DT::DTOutput("status")),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
        bslib::card(
          bslib::card_header("\u30ed\u30b0"),
          shiny::div(class = "rp-code", shiny::verbatimTextOutput("log"))),
        bslib::card(
          bslib::card_header("\u5b9a\u7fa9\u30c1\u30a7\u30c3\u30af\u306e\u7d50\u679c"),
          DT::DTOutput("check_result")))),

    bslib::nav_spacer(),
    bslib::nav_item(shiny::uiOutput("save_state")),
    bslib::nav_item(.btn("save", "\u4fdd\u5b58", class = "btn-sm btn-primary")))
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

.grid <- function(d, sheet, key) {
  d <- .na_blank(d)
  if (!nrow(d)) d[1L, ] <- ""
  h <- rhandsontable::rhandsontable(
    d, rowHeaders = TRUE, useTypes = FALSE, stretchH = "all",
    height = 460, minSpareRows = 1L, planner_key = key)
  h <- rhandsontable::hot_context_menu(h, allowRowEdit = TRUE,
                                       allowColEdit = FALSE)
  for (cn in names(.choices[[sheet]])) {
    if (cn %in% names(d)) {
      h <- rhandsontable::hot_col(h, cn, type = "dropdown",
                                  source = c("", .choices[[sheet]][[cn]]),
                                  strict = FALSE)
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

app_server <- function(input, output, session, start) {
  # `want`: the report to select once the sidebar knows it
  rv <- shiny::reactiveValues(
    study = NULL, p = NULL, saved = NULL, meta = NULL, saved_meta = NULL,
    ver = 0L, want = NULL, job = NULL, job_what = NULL, status_ver = 0L,
    studies_ver = 0L)
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
  }
  if (!is.null(start)) set_study(start)

  shiny::observe({
    for (t in .study_tabs) {
      if (has_study()) bslib::nav_show("nav", t) else bslib::nav_hide("nav", t)
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
    notify(sprintf("\u4fdd\u5b58\u3057\u307e\u3057\u305f\uff08%d \u30d5\u30a1\u30a4\u30eb\u3092\u66f4\u65b0%s\uff09", n,
                   if (kept) sprintf("\u3001\u624b\u4fee\u6b63\u306e\u3042\u308b %d \u30d7\u30ed\u30b0\u30e9\u30e0\u306f\u305d\u306e\u307e\u307e", kept)
                   else ""))
    TRUE
  }
  shiny::observeEvent(input$save, {
    if (!has_study()) return(notify("\u8a66\u9a13\u3092\u958b\u3044\u3066\u304f\u3060\u3055\u3044", "warning"))
    do_save()
  })
  output$save_state <- shiny::renderUI({
    if (!has_study()) return(NULL)
    if (dirty()) shiny::span(class = "rp-dirty me-2", "\u25cf \u672a\u4fdd\u5b58\u306e\u5909\u66f4") else
      shiny::span(class = "text-muted me-2", "\u4fdd\u5b58\u6e08\u307f")
  })
  output$study_side <- shiny::renderUI({
    if (!has_study()) {
      return(shiny::p(class = "text-muted", "\u8a66\u9a13\u3092\u958b\u3044\u3066\u304f\u3060\u3055\u3044\u3002"))
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
      rtfplanner_config()$last_study)
    v <- data.frame(
      "\u8a66\u9a13 ID" = d$study_id, "\u8a66\u9a13\u540d" = d$title, "\u5316\u5408\u7269" = d$compound,
      "\u76f8" = d$phase, "\u6700\u7d42\u4fdd\u5b58" = d$saved,
      "\u30d5\u30a9\u30eb\u30c0" = ifelse(d$folder, d$path, paste(d$path, "\uff08\u898b\u3064\u304b\u308a\u307e\u305b\u3093\uff09")),
      check.names = FALSE, stringsAsFactors = FALSE)
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
      shiny::p("rtfplanner \u306e\u4fdd\u5b58\u30d5\u30a9\u30eb\u30c0\uff08\u8a66\u9a13\u60c5\u5831\u306e\u6b63\u672c\uff09:", shiny::br(),
               shiny::code(rtfplanner_home())),
      shiny::div(
        class = "d-flex gap-2 align-items-end",
        shiny::textInput("studies_root", "\u65b0\u898f\u8a66\u9a13\u306e\u30d5\u30a9\u30eb\u30c0\u3092\u4f5c\u308b\u5834\u6240",
                         value = studies_root(), width = "100%"),
        .btn("save_settings", "\u5909\u66f4", class = "btn-sm mb-3")))
  })
  shiny::observeEvent(input$save_settings, {
    r <- trimws(input$studies_root)
    if (!nzchar(r)) return()
    guarded(suppressMessages(setup_rtfplanner(studies_root = r)))
    rv$studies_ver <- rv$studies_ver + 1L
    notify(paste("\u65b0\u898f\u8a66\u9a13\u306f", r, "\u306b\u4f5c\u308a\u307e\u3059"))
  })
  selected_study <- function() {
    i <- input$studies_rows_selected
    if (!length(i)) {
      notify("\u8a66\u9a13\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044", "warning")
      return(NULL)
    }
    studies()[i, , drop = FALSE]
  }
  shiny::observeEvent(input$register, {
    shiny::showModal(shiny::modalDialog(
      title = "\u65e2\u5b58\u306e\u8a66\u9a13\u30d5\u30a9\u30eb\u30c0\u3092\u767b\u9332",
      shiny::textInput("reg_path", "\u8a66\u9a13\u30d5\u30a9\u30eb\u30c0\uff08study.yml \u306e\u3042\u308b\u30d5\u30a9\u30eb\u30c0\uff09",
                       width = "100%"),
      shiny::p(class = "small text-muted",
               "spec/ \u306b\u5b9a\u7fa9\u30d6\u30c3\u30af\u304c\u3042\u308c\u3070\u3001\u305d\u306e\u5185\u5bb9\u3092\u8a66\u9a13\u60c5\u5831\u3068\u3057\u3066\u53d6\u308a\u8fbc\u307f\u307e\u3059\u3002"),
      footer = shiny::tagList(shiny::modalButton("\u53d6\u6d88"),
                              .btn("reg_ok", "\u767b\u9332", class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$reg_ok, {
    if (dirty()) {
      return(notify("\u672a\u4fdd\u5b58\u306e\u5909\u66f4\u304c\u3042\u308a\u307e\u3059\u3002\u4fdd\u5b58\u3057\u3066\u304b\u3089\u767b\u9332\u3057\u3066\u304f\u3060\u3055\u3044",
                    "warning"))
    }
    s <- guarded(register_study(trimws(input$reg_path)))
    if (is.null(s)) return()
    shiny::removeModal()
    rv$studies_ver <- rv$studies_ver + 1L
    set_study(s)
    notify(paste(s$meta$study_id, "\u3092\u767b\u9332\u3057\u307e\u3057\u305f"))
    bslib::nav_select("nav", "outputs")
  })
  shiny::observeEvent(input$unregister, {
    d <- selected_study()
    if (is.null(d)) return()
    shiny::showModal(shiny::modalDialog(
      title = paste(d$study_id, "\u306e\u767b\u9332\u3092\u89e3\u9664"),
      "rtfplanner \u306b\u4fdd\u5b58\u3057\u305f\u8a66\u9a13\u60c5\u5831\uff08\u5b9a\u7fa9\u30fb\u30c7\u30fc\u30bf\u6e96\u5099\u30b3\u30fc\u30c9\u30fb\u5c65\u6b74\uff09\u3092\u524a\u9664\u3057\u307e\u3059\u3002",
      "\u8a66\u9a13\u30d5\u30a9\u30eb\u30c0\uff08\u30c7\u30fc\u30bf\u30fbspec\u30fb\u30d7\u30ed\u30b0\u30e9\u30e0\u30fb\u6210\u679c\u7269\uff09\u306f\u305d\u306e\u307e\u307e\u6b8b\u308a\u3001",
      "\u300c\u65e2\u5b58\u30d5\u30a9\u30eb\u30c0\u3092\u767b\u9332\u300d\u3067 spec/ \u304b\u3089\u623b\u305b\u307e\u3059\u3002",
      footer = shiny::tagList(shiny::modalButton("\u53d6\u6d88"),
                              .btn("unregister_ok", "\u767b\u9332\u89e3\u9664",
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
    notify(paste(d$study_id, "\u306e\u767b\u9332\u3092\u89e3\u9664\u3057\u307e\u3057\u305f"))
  })
  shiny::observeEvent(input$open_study, {
    d <- selected_study()
    if (is.null(d)) return()
    if (dirty()) {
      return(notify("\u672a\u4fdd\u5b58\u306e\u5909\u66f4\u304c\u3042\u308a\u307e\u3059\u3002\u4fdd\u5b58\u3057\u3066\u304b\u3089\u958b\u3044\u3066\u304f\u3060\u3055\u3044",
                    "warning"))
    }
    s <- guarded(open_study(d$study_id))
    if (!is.null(s)) {
      set_study(s)
      notify(paste(s$meta$study_id, "\u3092\u958b\u304d\u307e\u3057\u305f"))
      bslib::nav_select("nav", "outputs")
    }
  })
  shiny::observeEvent(input$new_study, {
    shiny::showModal(shiny::modalDialog(
      title = "\u65b0\u898f\u8a66\u9a13",
      shiny::textInput("ns_id", "\u8a66\u9a13 ID\uff08\u30d5\u30a9\u30eb\u30c0\u540d\u3002\u82f1\u6570\u5b57 . _ -\uff09"),
      shiny::textInput("ns_title", "\u8a66\u9a13\u540d", width = "100%"),
      shiny::textInput("ns_compound", "\u5316\u5408\u7269"),
      shiny::textInput("ns_phase", "\u76f8"),
      shiny::textAreaInput("ns_description", "\u8aac\u660e", width = "100%"),
      shiny::radioButtons(
        "ns_from", "\u5e33\u7968\u306e\u5b9a\u7fa9",
        c("\u7a7a\u3067\u59cb\u3081\u308b" = "empty",
          "\u65e2\u5b58\u306e\u8a66\u9a13\u304b\u3089\u30b3\u30d4\u30fc\uff08\u5b9a\u7fa9\u3068\u30c7\u30fc\u30bf\u6e96\u5099\u30b3\u30fc\u30c9\uff09" = "study",
          "rtfreporter \u306e\u30b5\u30f3\u30d7\u30eb\uff085 \u5e33\u7968\uff09" = "sample")),
      shiny::conditionalPanel(
        "input.ns_from == 'study'",
        shiny::selectInput("ns_src", "\u30b3\u30d4\u30fc\u5143",
                           stats::setNames(studies()$study_id,
                                           studies()$study_id))),
      shiny::textInput("ns_root", "\u8a66\u9a13\u30d5\u30a9\u30eb\u30c0\u3092\u4f5c\u308b\u5834\u6240",
                       value = studies_root(), width = "100%"),
      footer = shiny::tagList(shiny::modalButton("\u53d6\u6d88"),
                              .btn("ns_ok", "\u4f5c\u6210", class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$ns_ok, {
    if (dirty()) {
      return(notify("\u672a\u4fdd\u5b58\u306e\u5909\u66f4\u304c\u3042\u308a\u307e\u3059\u3002\u4fdd\u5b58\u3057\u3066\u304b\u3089\u4f5c\u6210\u3057\u3066\u304f\u3060\u3055\u3044",
                    "warning"))
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
    notify(paste(s$meta$study_id, "\u3092\u4f5c\u6210\u3057\u307e\u3057\u305f"))
    bslib::nav_select("nav", "outputs")
  })

  output$study_detail <- shiny::renderUI({
    if (!has_study()) {
      return(shiny::p(class = "text-muted",
                      "\u5de6\u306e\u4e00\u89a7\u304b\u3089\u8a66\u9a13\u3092\u958b\u304f\u304b\u3001\u65b0\u898f\u8a66\u9a13\u3092\u4f5c\u6210\u3057\u3066\u304f\u3060\u3055\u3044\u3002"))
    }
    rv$ver
    m <- shiny::isolate(rv$meta)
    r <- shiny::isolate(rv$p$study[["rounding"]])
    v <- function(x) if (is.na(x)) "" else x
    lay <- study_layout()
    shiny::tagList(
      shiny::p(shiny::strong(m$study_id), shiny::br(),
               shiny::span(class = "small text-muted", rv$study$path)),
      shiny::textInput("m_title", "\u8a66\u9a13\u540d", v(m$title), width = "100%"),
      shiny::div(class = "d-flex gap-2",
                 shiny::textInput("m_compound", "\u5316\u5408\u7269", v(m$compound)),
                 shiny::textInput("m_phase", "\u76f8", v(m$phase))),
      shiny::textAreaInput("m_description", "\u8aac\u660e", v(m$description),
                           width = "100%"),
      shiny::selectInput(
        "rounding", "\u4e38\u3081\u65b9 (rounding)",
        c("options(rtfreporter.rounding) \u306b\u5f93\u3046" = "",
          "r\uff08\u5076\u6570\u4e38\u3081\uff09" = "r", "sas\uff08\u56db\u6368\u4e94\u5165\uff09" = "sas"),
        selected = v(r)),
      shiny::tags$details(
        shiny::tags$summary("\u30d5\u30a9\u30eb\u30c0\u69cb\u6210"),
        shiny::tags$pre(class = "small", paste(
          sprintf("%-15s %s", paste0(lay, "/"), c(
            "\u5165\u529b\u30c7\u30fc\u30bf: ADaM", "\u5165\u529b\u30c7\u30fc\u30bf: SDTM", "\u5165\u529b\u30c7\u30fc\u30bf: \u305d\u306e\u4ed6",
            "\u5b9a\u7fa9\u30d6\u30c3\u30af table_spec.xlsx / report_spec.xlsx",
            "\u5e33\u7968\u30d7\u30ed\u30b0\u30e9\u30e0, autoexec_report.R",
            "\u6210\u679c\u7269\u30c7\u30fc\u30bf: \u5404 Table \u306e ARD (.rds)",
            "\u6210\u679c\u7269\u5e33\u7968: RTF", "\u5b9f\u884c\u30ed\u30b0")), collapse = "\n"))),
      shiny::fileInput(
        "import", "Excel \u306e\u5b9a\u7fa9\u30d6\u30c3\u30af\u3092\u53d6\u308a\u8fbc\u3080\uff08\u3053\u306e\u8a66\u9a13\u306e\u5b9a\u7fa9\u3092\u7f6e\u304d\u63db\u3048\uff09",
        multiple = TRUE, accept = ".xlsx", width = "100%"),
      shiny::div(class = "d-flex flex-wrap gap-2 align-items-center",
                 shiny::downloadButton("spec_xlsx",
                                       "\u5b9a\u7fa9\u30d6\u30c3\u30af\u3092\u66f8\u304d\u51fa\u3059 (Excel)",
                                       class = "btn-sm"),
                 shiny::downloadButton("study_zip", "\u8a66\u9a13\u3092 zip \u3067\u30c0\u30a6\u30f3\u30ed\u30fc\u30c9",
                                       class = "btn-sm"),
                 shiny::checkboxInput("zip_data", "\u30c7\u30fc\u30bf\u3082\u542b\u3081\u308b")),
      .btn("study_folder", "\u8a66\u9a13\u30d5\u30a9\u30eb\u30c0\u3092\u958b\u304f"))
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
    notify(paste0(nrow(p$outputs), " \u5e33\u7968\u3092\u53d6\u308a\u8fbc\u307f\u307e\u3057\u305f\uff08\u672a\u4fdd\u5b58\uff09"))
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
    ch <- c(stats::setNames(.all_rows, "(\u5168\u884c)"),
            if (!is.null(p)) stats::setNames(.default_rows, "(\u65e2\u5b9a = \u7a7a\u6b04)"),
            stats::setNames(ids, ids))
    want <- shiny::isolate(rv$want)
    rv$want <- NULL
    sel <- if (length(want) && !is.na(want) && want %in% ch) want else
      if (!is.null(cur) && cur %in% ch) cur else
        if (length(ids)) ids[1L] else .all_rows
    shiny::updateRadioButtons(session, "target", choices = ch, selected = sel)
  })
  target <- shiny::reactive(.target_value(input$target))
  current <- shiny::reactive({
    t <- target()
    if (!is.null(rv$p) && !is.na(t) && nzchar(t) &&
        t %in% rv$p$outputs$output_id) t else NULL
  })

  # -- sheet grids -------------------------------------------------------
  for (sheet in c(table_sheets(), report_sheets())) local({
    sh <- sheet
    out_id <- paste0("hot_", sh)
    key <- shiny::reactive(paste(sh, input$target, rv$ver, sep = "|"))
    output[[out_id]] <- rhandsontable::renderRHandsontable({
      shiny::req(has_study())
      t <- target()
      d <- sheet_rows(shiny::isolate(rv$p), sh, t)
      if (!identical(t, "")) d$output_id <- NULL
      .grid(d, sh, key())
    })
    shiny::observeEvent(input[[out_id]], {
      h <- input[[out_id]]
      if (is.null(h$changes$changes) &&
          !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
      if (!identical(h$params$planner_key, key())) return()
      d <- rhandsontable::hot_to_r(h)
      rv$p <- set_sheet_rows(rv$p, sh, target(), d)
    })
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
               sprintf("%s \u306f %s \u3067\u3059\u3002\u8868\u306e\u5b9a\u7fa9\u306f Table \u306e\u5e33\u7968\u304c\u4f7f\u3044\u307e\u3059\uff08%s \u306f\u30d7\u30ed\u30b0\u30e9\u30e0\u306e\u30c7\u30fc\u30bf\u90e8\u3067\u4f5c\u308a\u307e\u3059\uff09\u3002",
                       id, .type_labels[[type]], .type_labels[[type]]))
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
  # What the server puts in an editor comes back from the browser as an
  # input event, and by then another report -- or another study -- may be
  # chosen.  So an edit belongs to the report (and study) the editor was
  # filled for, not to whatever is chosen when the event arrives, and an
  # event that only echoes what the server put there is not an edit.
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

  shiny::observeEvent(list(current(), rv$ver), {
    id <- current()
    o <- rv$p$outputs[rv$p$outputs$output_id %in% id, , drop = FALSE]
    val <- function(v) if (length(v) && !is.na(v)) v else ""
    owner <- if (!is.null(id)) list(study = study_key(), id = id)
    fill("description", val(o$description), owner, shiny::updateTextInput)
    fill("data_code", val(o$data_code), owner, shiny::updateTextAreaInput)
  })
  output$current_label <- shiny::renderUI({
    id <- current()
    if (is.null(id)) {
      shiny::p(class = "text-muted",
               "\u5de6\u306e\u4e00\u89a7\u304b\u30b5\u30a4\u30c9\u30d0\u30fc\u3067\u5e33\u7968\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044\u3002")
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
  shiny::observeEvent(input$description,
                      set_field("description", input$description),
                      ignoreInit = TRUE)
  shiny::observeEvent(input$data_code, set_field("data_code", input$data_code),
                      ignoreInit = TRUE)

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
    msg <- switch(st,
      missing = "\u307e\u3060\u30d5\u30a1\u30a4\u30eb\u306f\u3042\u308a\u307e\u305b\u3093\u3002\u4fdd\u5b58\u3059\u308b\u3068\u4e0b\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u304c\u66f8\u304b\u308c\u307e\u3059\u3002",
      current = "\u4fdd\u5b58\u6e08\u307f\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u306f\u4e0b\u306e\u5185\u5bb9\u3068\u540c\u3058\u3067\u3059\u3002",
      todo = "\u4fdd\u5b58\u6e08\u307f\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u3068\u540c\u3058\u3067\u3059\uff08\u30c7\u30fc\u30bf\u90e8\u306f TODO \u306e\u307e\u307e\uff09\u3002",
      generated = "\u5b9a\u7fa9\u304c\u5909\u308f\u3063\u3066\u3044\u307e\u3059\u3002\u4fdd\u5b58\u3059\u308b\u3068\u4e0b\u306e\u5185\u5bb9\u306b\u66f4\u65b0\u3055\u308c\u307e\u3059\uff08\u624b\u4fee\u6b63\u306e\u306a\u3044\u30d7\u30ed\u30b0\u30e9\u30e0\u306f\u5b9a\u7fa9\u306b\u8ffd\u5f93\u3057\u307e\u3059\uff09\u3002",
      edited = "\u4fdd\u5b58\u6e08\u307f\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u306f\u624b\u3067\u4fee\u6b63\u3055\u308c\u3066\u3044\u307e\u3059\u3002\u4fdd\u5b58\u3057\u3066\u3082\u4e0a\u66f8\u304d\u3057\u307e\u305b\u3093\u3002")
    shiny::div(
      class = "d-flex gap-2 align-items-center mb-2 small",
      shiny::span(msg),
      if (identical(st, "edited")) {
        .btn("regenerate", "\u3053\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u3092\u518d\u751f\u6210",
             class = "btn-sm btn-outline-warning")
      })
  })
  shiny::observeEvent(input$regenerate, {
    shiny::showModal(shiny::modalDialog(
      title = paste(current(), "\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u3092\u518d\u751f\u6210"),
      "\u30c7\u30a3\u30b9\u30af\u4e0a\u306e\u30d7\u30ed\u30b0\u30e9\u30e0\u3092\u3001\u4eca\u306e\u5b9a\u7fa9\u3068\u30c7\u30fc\u30bf\u6e96\u5099\u30b3\u30fc\u30c9\u304b\u3089\u66f8\u304d\u76f4\u3057\u307e\u3059\u3002\u624b\u3067\u76f4\u3057\u305f\u90e8\u5206\u306f\u6d88\u3048\u307e\u3059\u3002",
      footer = shiny::tagList(shiny::modalButton("\u53d6\u6d88"),
                              .btn("regenerate_ok", "\u518d\u751f\u6210",
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
        "modal_type", "\u7a2e\u5225",
        stats::setNames(report_types(), .type_labels[report_types()]),
        inline = TRUE),
      if (type) shiny::textInput("modal_desc", "\u8aac\u660e", width = "100%"),
      footer = shiny::tagList(shiny::modalButton("\u53d6\u6d88"),
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
    if (is.null(current())) notify("\u5e33\u7968\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044", "warning")
    !is.null(current())
  }
  shiny::observeEvent(input$add, ask_id("\u5e33\u7968\u3092\u8ffd\u52a0", "add_ok", type = TRUE))
  shiny::observeEvent(input$add_ok, {
    id <- trimws(input$modal_id)
    d <- trimws(input$modal_desc)
    after_id_change(guarded(add_output(
      rv$p, id, description = if (nzchar(d)) d else NA,
      type = input$modal_type)), id)
  })
  shiny::observeEvent(input$copy, {
    if (need_current()) ask_id(paste(current(), "\u3092\u8907\u88fd"), "copy_ok",
                               paste0(current(), "_2"))
  })
  shiny::observeEvent(input$copy_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(copy_output(rv$p, current(), id)), id)
  })
  shiny::observeEvent(input$rename, {
    if (need_current()) ask_id(paste(current(), "\u306e\u540d\u524d\u3092\u5909\u66f4"),
                               "rename_ok", current())
  })
  shiny::observeEvent(input$rename_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(rename_output(rv$p, current(), id)), id)
  })
  shiny::observeEvent(input$remove, {
    if (!need_current()) return()
    shiny::showModal(shiny::modalDialog(
      title = paste(current(), "\u3092\u524a\u9664"),
      "\u5168\u30b7\u30fc\u30c8\u304b\u3089\u3053\u306e\u5e33\u7968\u306e\u884c\u3092\u524a\u9664\u3057\u307e\u3059\uff08\u4fdd\u5b58\u3059\u308b\u307e\u3067\u30c7\u30a3\u30b9\u30af\u306f\u5909\u308f\u308a\u307e\u305b\u3093\u3002\u30d7\u30ed\u30b0\u30e9\u30e0\u3068\u6210\u679c\u7269\u306e\u30d5\u30a1\u30a4\u30eb\u306f\u6b8b\u308a\u307e\u3059\uff09\u3002",
      footer = shiny::tagList(
        shiny::modalButton("\u53d6\u6d88"),
        .btn("remove_ok", "\u524a\u9664", class = "btn-danger"))))
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
  output$data_files <- DT::renderDT(
    .dt(data_files()[c("folder", "file", "size_kb", "modified")],
        scrollY = "360px"))
  shiny::observeEvent(input$data_upload, {
    f <- input$data_upload
    dest <- file.path(rv$study$path, study_layout()[[input$data_folder]])
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(f$datapath, file.path(dest, f$name), overwrite = TRUE)
    data_ver(data_ver() + 1L)
    notify(sprintf("%d \u30d5\u30a1\u30a4\u30eb\u3092 %s \u306b\u53d6\u308a\u8fbc\u307f\u307e\u3057\u305f", sum(ok),
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
             sprintf("%s \u884c \u00d7 %s \u5217", format(full[1L], big.mark = ","),
                     full[2L]))
  })

  # -- results -------------------------------------------------------------
  status <- shiny::reactive({
    rv$status_ver
    input$status_refresh
    shiny::req(has_study())
    s <- rv$study
    study_status(s)
  })
  output$status <- DT::renderDT({
    d <- status()
    v <- data.frame(
      output_id = d$output_id,
      type = unname(.type_labels[d$type]),
      program = d$program,
      "\u30d7\u30ed\u30b0\u30e9\u30e0" = unname(c(missing = "\u306a\u3057", todo = "TODO",
                              current = "\u751f\u6210\u306e\u307e\u307e",
                              generated = "\u751f\u6210\u306e\u307e\u307e\uff08\u672a\u4fdd\u5b58\u306e\u5909\u66f4\uff09",
                              edited = "\u624b\u4fee\u6b63\u3042\u308a")[d$program_state]),
      "ARD" = ifelse(is.na(d$ard), "", d$ard),
      "RTF" = ifelse(is.na(d$rtf), "", d$rtf),
      "\u72b6\u614b" = unname(.status_labels[d$status]),
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::formatStyle(
      .dt(v, selection = "multiple"), "\u72b6\u614b",
      color = DT::styleEqual(
        unname(.status_labels[c("error", "todo", "outdated", "unsaved",
                                "ok")]),
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
      if (!file.exists(f)) return("\u5e33\u7968\u3092\u9078\u3076\u3068\u305d\u306e\u30ed\u30b0\u3092\u8868\u793a\u3057\u307e\u3059\u3002")
    } else {
      f <- d$log[1L]
      if (is.na(f)) return(paste(d$output_id[1L], "\u306f\u307e\u3060\u5b9f\u884c\u3055\u308c\u3066\u3044\u307e\u305b\u3093\u3002"))
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
      if (!nrow(d) || !file.exists(d$rtf_path[1L])) {
        stop("RTF \u304c\u3042\u308a\u307e\u305b\u3093")
      }
      file.copy(d$rtf_path[1L], file)
    })
  shiny::observeEvent(input$tfl_open, .open_folder(
    file.path(rv$study$path, study_layout()[["tfl"]])))
  shiny::observeEvent(input$check, {
    r <- check_planner(rv$p)
    output$check_result <- DT::renderDT(.dt(r, selection = "none"))
    if (all(r$ok)) notify("\u5b9a\u7fa9\u306b\u554f\u984c\u306f\u3042\u308a\u307e\u305b\u3093") else
      notify("\u5b9a\u7fa9\u306b\u30a8\u30e9\u30fc\u304c\u3042\u308a\u307e\u3059\uff08\u5b9a\u7fa9\u30c1\u30a7\u30c3\u30af\u306e\u7d50\u679c\u3092\u53c2\u7167\uff09", "error")
  })

  start_run <- function(ids, what) {
    if (!is.null(rv$job)) return(notify("\u5b9f\u884c\u4e2d\u3067\u3059", "warning"))
    if (dirty() && !do_save()) return()
    px <- guarded(run_study(rv$study, ids, wait = FALSE))
    if (is.null(px)) return()
    rv$job <- px
    rv$job_what <- what
  }
  shiny::observeEvent(input$run_all, start_run(NULL, "\u5168\u5e33\u7968"))
  shiny::observeEvent(input$run_selected, {
    d <- selected_status()
    if (!nrow(d)) return(notify("\u5e33\u7968\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044", "warning"))
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
      notify(sprintf("%s \u306e\u5b9f\u884c\u304c\u7d42\u308f\u308a\u307e\u3057\u305f%s", rv$job_what,
                     if (identical(rc, 0L)) "" else "\uff08\u30a8\u30e9\u30fc\u3042\u308a\uff09"),
             if (identical(rc, 0L)) "message" else "warning")
      rv$job <- NULL
      rv$status_ver <- rv$status_ver + 1L
    })
  })
  output$job <- shiny::renderUI({
    if (is.null(rv$job)) return(NULL)
    shiny::div(class = "alert alert-info py-2 small my-2",
               shiny::span(class = "spinner-border spinner-border-sm me-2"),
               paste(rv$job_what, "\u3092\u5b9f\u884c\u4e2d\u2026"))
  })
  session$onSessionEnded(function() {
    px <- shiny::isolate(rv$job)
    if (!is.null(px) && px$is_alive()) px$kill()
  })
}
