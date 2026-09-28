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
.study_tabs <- c("outputs", "ard", "lf", "builder", "table_spec",
                 "report_spec", "data", "results")

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
.rp-pv.rp-pv-left .rp-pv-val, .rp-pv.rp-pv-left th { text-align: left; }
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
#studies td { white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
  max-width: 22em; }
.rp-stat-fmt { display: grid; grid-template-columns: repeat(auto-fill,
  minmax(9.5em, 1fr)); gap: 0 .5rem; }
.rp-stat-fmt .form-group { margin-bottom: .3rem; }
#study_detail code { word-break: break-all; }
.rp-stat-fmt label { font-size: 12px; margin-bottom: 0; }
.rp-split { display: grid; gap: 1rem; align-items: start;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); }
.rp-split.rp-lay-stack, .rp-split.rp-lay-one {
  grid-template-columns: minmax(0, 1fr); }
.rp-split.rp-lay-one.rp-show-def > :nth-child(2),
.rp-split.rp-lay-one.rp-show-out > :nth-child(1) { display: none !important; }
.rhandsontable.html-widget { overflow: hidden; }
.rp-grip { height: 8px; margin: 2px 0 4px; cursor: ns-resize;
  border-radius: 4px; background: var(--bs-tertiary-bg, #eef0f3);
  background-image: linear-gradient(90deg, transparent 45%, #9ca3af 45%,
    #9ca3af 55%, transparent 55%);
  background-size: 100% 2px; background-repeat: no-repeat;
  background-position: center; }
.rp-grip:hover { background-color: #dbe4f0; }
.rp-resize .dataTables_scrollBody { resize: vertical; }
@media (max-width: 991px) {
  .rp-split { grid-template-columns: minmax(0, 1fr); } }
"

# the ARD tab's panes: side by side, stacked, or one at a time (kept in
# the viewer's browser)
.split_js <- "
(function() {
  var cur = {lay: 'side', pane: 'def'};
  try {
    var v = (localStorage.getItem('tflplanner.ard_layout') || '').split('|');
    if (v[0]) cur.lay = v[0];
    if (v[1]) cur.pane = v[1];
  } catch (e) {}
  function apply() {
    var s = document.getElementById('ard_split');
    if (!s) return;
    s.classList.remove('rp-lay-side', 'rp-lay-stack', 'rp-lay-one',
                       'rp-show-def', 'rp-show-out');
    s.classList.add('rp-lay-' + cur.lay, 'rp-show-' + cur.pane);
    document.querySelectorAll('#ard_layout [data-lay]').forEach(function(b) {
      b.classList.toggle('active', b.dataset.lay === cur.lay);
    });
    document.querySelectorAll('#ard_pane [data-pane]').forEach(function(b) {
      b.classList.toggle('active', b.dataset.pane === cur.pane);
    });
    var g = document.getElementById('ard_pane');
    if (g) g.style.display = cur.lay === 'one' ? '' : 'none';
    try { localStorage.setItem('tflplanner.ard_layout', cur.lay + '|' + cur.pane); }
    catch (e) {}
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
      if (window.jQuery && jQuery.fn.dataTable) {
        jQuery.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
      }
    }, 50);
  }
  document.addEventListener('click', function(e) {
    var b = e.target.closest('#ard_layout [data-lay], #ard_pane [data-pane]');
    if (!b) return;
    if (b.dataset.lay) cur.lay = b.dataset.lay;
    if (b.dataset.pane) cur.pane = b.dataset.pane;
    apply();
  });
  document.addEventListener('DOMContentLoaded', apply);
})();

// a grid is as tall as its rows; the bar under it drags it taller or
// shorter, and the height it is given stays through redraws
(function() {
  var heights = {}, drag = null;
  function hotOf(id) {
    var w = window.HTMLWidgets && HTMLWidgets.find('#' + id);
    return w && w.hot;
  }
  function fit(g, h) {
    var hot = hotOf(g.id);
    if (!hot) return;
    g.style.height = h + 'px';
    hot.updateSettings({height: h});
  }
  function grip(g) {
    var n = g.nextElementSibling;
    if (n && n.classList.contains('rp-grip')) return;
    var b = document.createElement('div');
    b.className = 'rp-grip';
    b.title = 'drag to resize';
    b.dataset.grid = g.id;
    g.parentNode.insertBefore(b, g.nextSibling);
  }
  document.addEventListener('mousedown', function(e) {
    var b = e.target.closest('.rp-grip');
    if (b) {
      var g = document.getElementById(b.dataset.grid);
      drag = {g: g, y: e.clientY, h: g.getBoundingClientRect().height};
      e.preventDefault();
      return;
    }
    var s = e.target.closest('.rp-resize .dataTables_scrollBody');
    if (s) {
      var r = s.getBoundingClientRect();
      if (e.clientX > r.right - 18 && e.clientY > r.bottom - 18) {
        s.style.maxHeight = 'none';
      }
    }
  });
  document.addEventListener('mousemove', function(e) {
    if (!drag) return;
    var h = Math.max(80, Math.round(drag.h + e.clientY - drag.y));
    heights[drag.g.id] = h;
    fit(drag.g, h);
  });
  document.addEventListener('mouseup', function() { drag = null; });
  if (window.jQuery) {
    jQuery(document).on('shiny:value', function(e) {
      setTimeout(function() {
        var g = document.getElementById(e.name);
        if (!g || !g.classList.contains('rhandsontable')) return;
        var hot = hotOf(e.name);
        if (!hot) return;
        grip(g);
        fit(g, heights[e.name] || hot.getSettings().height);
      }, 100);
    });
  }
})();
"

.btn <- function(id, label, class = "btn-sm", ...) {
  shiny::actionButton(id, label, class = class, ...)
}

app_ui <- function(lang = "en") {
  t <- function(x) tr(x, lang)
  two <- bslib::breakpoints(sm = 12, lg = c(5, 7))
  lay_btn <- function(...) {
    shiny::tags$button(type = "button", class = "btn btn-outline-secondary",
                       ...)
  }

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
    header = shiny::tagList(shiny::tags$style(shiny::HTML(.code_css)),
                            shiny::tags$script(shiny::HTML(.split_js))),
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
        col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
        bslib::card(
          bslib::card_header(t("Studies")),
          shiny::uiOutput("studies_root_note"),
          DT::DTOutput("studies"),
          shiny::p(class = "small text-muted mb-1",
                   t("Click a study to see it on the right; double-click to open it.")),
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
          bslib::card_header(shiny::uiOutput("study_detail_title",
                                             inline = TRUE)),
          shiny::uiOutput("study_detail")))),

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
          DT::DTOutput("data_head"))),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
        bslib::card(
          bslib::card_header(t("Data catalog (SDTM / ADaM)")),
          shiny::p(class = "small text-muted",
                   t("The datasets the study's programs read: edit them on the ARD tab (datasets).")),
          DT::DTOutput("catalog")),
        bslib::card(
          bslib::card_header(t("Study ARD")),
          shiny::p(class = "small text-muted",
                   t("One ARD for the study; each output takes its rows by output_id.")),
          DT::DTOutput("ard_state_data")))),

    bslib::nav_panel(
      "ARD", value = "ard",
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center mb-2 small",
        shiny::span(class = "text-muted", t("Layout")),
        shiny::div(
          id = "ard_layout", class = "btn-group btn-group-sm", role = "group",
          lay_btn("data-lay" = "side", t("Side by side")),
          lay_btn("data-lay" = "stack", t("Stacked")),
          lay_btn("data-lay" = "one", t("One pane"))),
        shiny::div(
          id = "ard_pane", class = "btn-group btn-group-sm", role = "group",
          lay_btn("data-pane" = "def", t("ARD definition")),
          lay_btn("data-pane" = "out", t("Code / ARD")))),
      shiny::div(
        id = "ard_split", class = "rp-split rp-lay-side rp-show-def",
        bslib::card(
          bslib::card_header(t("ARD definition")),
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
          shiny::div(
            class = "rp-b-card mt-2",
            shiny::h6(t("Statistics and formats")),
            shiny::uiOutput("ard_stat_ui")),
          shiny::tags$details(
            class = "rp-help mt-2",
            shiny::tags$summary(t("Methods")),
            DT::DTOutput("ard_methods")),
          shiny::tags$details(
            class = "rp-help mt-2",
            shiny::tags$summary(t("Statistics (company standards)")),
            DT::DTOutput("ard_stat_catalog"))),
        bslib::navset_card_tab(
          id = "ard_right",
          bslib::nav_panel(
            t("Code (cards / cardx)"), value = "code",
            shiny::radioButtons(
              "ard_scope", NULL,
              stats::setNames(c("report", "setup", "autoexec"),
                              c(t("This output's program"), "ard_setup.R",
                                "autoexec_ard.R")),
              inline = TRUE),
            shiny::uiOutput("ard_prog_state"),
            shiny::div(class = "rp-code", shiny::verbatimTextOutput("ard_code"))),
          bslib::nav_panel(
            t("Study ARD"), value = "state",
            shiny::p(class = "small text-muted",
                     t("Build the ARD output by output as each is ready: tables can be made from the outputs already in it, while others are still being defined.")),
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-center",
              .btn("ard_update", t("Preview: put this output into the study ARD"),
                   class = "btn-sm btn-primary"),
              .btn("ard_build2", t("Official run: the whole study ARD")),
              shiny::checkboxInput("ard_batch_code",
                                   t("keep the code in the batch folder"),
                                   value = TRUE)),
            shiny::p(class = "small text-muted mt-1 mb-1",
                     t("Both run the saved programs (programs/ard/): unsaved changes are saved first. A preview updates the working ARD and keeps no log; an official run makes a batch folder (runs/) with the logs (logrx), the ARD and the code. Choose a row to see its log in the latest official run.")),
            DT::DTOutput("ard_state"),
            shiny::div(class = "rp-code mt-2",
                       shiny::verbatimTextOutput("ard_log"))),
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
            shiny::div(class = "rp-resize",
                       DT::DTOutput("ard_table", height = "auto", fill = FALSE)))))),

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
                t("1. ARD: make `ard` (Listing: rework `data`; Figure: the plot). Blank for a table = the company template: its rows of the study ARD."),
                rows = 10, width = "100%", resize = "vertical"),
              shiny::textAreaInput(
                "process_code",
                t("2. Normalize and rework: make `data` from `ard` (tfl_ard_normalize(), then mutate() ...). Blank = the company template (normalize)."),
                rows = 5, width = "100%", resize = "vertical",
                placeholder = "data <- tfl_ard_normalize(ard)"),
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
      t("Listing / Figure"), value = "lf",
      shiny::uiOutput("lf_note"),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
        shiny::div(
          shiny::uiOutput("lf_form"),
          shiny::uiOutput("lf_cols_box")),
        bslib::card(
          bslib::card_header(shiny::div(
            class = "d-flex justify-content-between align-items-center",
            shiny::span(t("Preview")),
            .btn("lf_preview", t("Preview the listing"),
                 class = "btn-sm btn-outline-primary"))),
          shiny::uiOutput("lf_preview_out")))),

    .designer_ui(t),

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
      t("Results"), value = "results",
      bslib::card(
        bslib::card_header(t("Reports")),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          .btn("run_selected", t("Preview selected"),
               class = "btn-sm btn-primary"),
          .btn("run_all", t("Preview all"), class = "btn-sm btn-primary"),
          .btn("status_refresh", t("Refresh")),
          .btn("check", t("Check the definition")),
          .btn("tfl_open", t("Open output/tfl")),
          shiny::downloadButton("rtf_download", t("Download the RTF"),
                                class = "btn-sm")),
        shiny::uiOutput("job"),
        DT::DTOutput("status")),
      bslib::card(
        bslib::card_header(t("Official runs (batch folders)")),
        shiny::p(class = "small text-muted mb-1",
                 t("An official run makes a batch folder runs/<date>_<time>_<what>/ with each program's log (logrx), what the run made (the study ARD, the RTFs) and, if kept, the programs and definition workbooks it ran.")),
        shiny::div(
          class = "d-flex flex-wrap gap-3 align-items-center",
          shiny::radioButtons(
            "batch_parts", NULL, inline = TRUE,
            stats::setNames(c("ard", "tfl", "all"),
                            t(c("ARD", "Reports", "ARD, then reports")))),
          shiny::checkboxInput("batch_code",
                               t("keep the code in the batch folder"),
                               value = TRUE),
          .btn("batch_run", t("Start the official run"),
               class = "btn-sm btn-primary"),
          .btn("batch_open", t("Open the batch folder"))),
        DT::DTOutput("batches")),
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

# as tall as its rows (and the spare one), up to a screenful; the viewer
# can drag it taller or shorter (.split_js)
.grid_height <- function(n) {
  as.integer(min(max(28 + 23 * (n + 1L) + 20, 100), 420))
}

.grid <- function(d, sheet, key, choices) {
  d <- .na_blank(d)
  if (!nrow(d)) d[1L, ] <- ""
  h <- rhandsontable::rhandsontable(
    d, rowHeaders = TRUE, useTypes = FALSE, stretchH = "all",
    height = .grid_height(nrow(d)), minSpareRows = 1L, planner_key = key)
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
    # what this session opened (or last saved): the save merges it with
    # whatever others saved since
    base <- rv$study
    base$planner <- rv$saved
    base$meta[.study_fields] <- rv$saved_meta
    mine <- rv$p
    s <- tryCatch(save_study(current_study(), regenerate = regenerate,
                             base = base),
                  tflplanner_conflict = function(e) {
                    shiny::showModal(shiny::modalDialog(
                      title = t("Someone else saved the same part"),
                      conditionMessage(e), easyClose = TRUE))
                    NULL
                  },
                  error = function(e) {
                    notify(conditionMessage(e), "error")
                    NULL
                  })
    if (is.null(s)) return(FALSE)
    rv$study <- s
    rv$p <- rv$saved <- s$planner
    if (!identical(.study_spec_keys(mine), s$planner)) {
      bump()
      notify(t("Changes others saved meanwhile were merged in."))
    }
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
    shiny::validate(shiny::need(
      nrow(d) > 0L,
      t("No studies yet. Create one with New study (the sample study is one of its choices), or register a study folder.")))
    last <- shiny::isolate(if (has_study()) rv$study$meta$study_id else
      tflplanner_config()$last_study)
    open_id <- shiny::isolate(if (has_study()) rv$study$meta$study_id)
    v <- data.frame(
      a = paste0(d$study_id, ifelse(d$folder, "", " \u26a0"),
                 ifelse(d$study_id %in% open_id, " \u25cf", "")),
      b = d$title, c = d$compound, d = d$phase,
      r = d$reports, e = substr(d$saved, 1L, 16L),
      stringsAsFactors = FALSE)
    v[is.na(v)] <- ""
    names(v) <- t(c("Study ID", "Title", "Compound", "Phase", "Reports",
                    "Last saved"))
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
                     ordering = FALSE, scrollX = TRUE, scrollY = "50vh",
                     scrollCollapse = TRUE))
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
  output$study_detail_title <- shiny::renderUI({
    s <- shown_study()
    if (is.null(s)) return(t("Study"))
    shiny::span(s$study_id, " ",
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
      shiny::p(class = "small text-muted mb-1",
               t("The sample study SAMPLE-01: CDISC pilot data (pharmaverseadam), one study ARD, four tables, a listing and a figure.")),
      .btn("add_sample", t("Add the sample study"),
           class = "btn-sm btn-outline-primary mb-2"),
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
    d <- studies()
    i <- input$studies_rows_selected
    i <- i[i >= 1L & i <= nrow(d)]
    if (!length(i)) {
      notify(t("Choose a study"), "warning")
      return(NULL)
    }
    d[i[1L], , drop = FALSE]
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
  to_unregister <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$unregister, {
    d <- selected_study()
    if (is.null(d)) return()
    to_unregister(d$study_id)
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Unregister %s"), d$study_id),
      t("This deletes what tflplanner keeps about the study (definition, data code, history). The study folder (data, spec, programs, outputs) stays, and Register a folder brings it back from spec/."),
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
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
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
    i <- input$studies_dbl
    if (i >= 1L && i <= nrow(d)) open_row(d[i, , drop = FALSE])
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
                        c(t("Start empty"),
                          t("Copy another study (definition and data code)"),
                          t("The sample study: data, ARD definition, tables, listing and figures (made at once)"))),
        selected = "sample"),
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
    s <- if (identical(input$ns_from, "sample")) {
      # the sample study under this ID: its data, ARD definition, tables,
      # listing and figures, made at once by an official run
      nz <- function(v) { v <- blank(v); if (is.na(v)) NULL else v }
      out <- NULL
      shiny::withProgress(
        message = t("Copying the sample study and making its ARD and reports ..."),
        out <- guarded(suppressMessages(create_sample_study(
          root = trimws(input$ns_root), study_id = trimws(input$ns_id),
          title = nz(input$ns_title), compound = nz(input$ns_compound),
          phase = nz(input$ns_phase),
          description = nz(input$ns_description)))))
      out
    } else {
      p <- switch(input$ns_from,
        study = guarded(open_study(input$ns_src)$planner),
        new_planner())
      if (is.null(p)) return()
      guarded(create_study(
        trimws(input$ns_id),
        title = blank(input$ns_title), compound = blank(input$ns_compound),
        phase = blank(input$ns_phase),
        description = blank(input$ns_description), planner = p,
        root = trimws(input$ns_root)))
    }
    if (is.null(s)) return()
    shiny::removeModal()
    rv$studies_ver <- rv$studies_ver + 1L
    set_study(s)
    notify(sprintf(t("Created %s"), s$meta$study_id))
    bslib::nav_select("nav", "outputs")
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
            "ARD programs: one per output, ard_setup.R, autoexec_ard.R",
            "Report programs, autoexec_report.R",
            "Working data: the study ARD (ard.rds)",
            "Working reports: RTF",
            "Official runs: one batch folder each (logs, results, code)",
            "What a report program printed in its last preview"))),
          collapse = "\n"))),
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
  # sheets are the study's.  The code is tfl_ard_code() of the definition
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
        method = c(.std_ard_methods()$method, ard_functions()),
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
  for (sheet in names(.ard_sheets())) local({
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

  # -- statistics and formats of one analysis ------------------------------
  # Picks the analysis's statistics from the company standards' catalog
  # (those its method can give) and each one's format; Apply writes them
  # to the row's `statistics` and `formats`.  Fresh input ids per drawing,
  # as the builder's.
  st_env <- new.env()
  st_env$n <- 0L
  st_drawn <- shiny::reactiveVal(0L)
  st_id <- function(x) paste0("st", st_env$n, "_", x)
  st_rows <- shiny::reactive({
    tg <- ard_target()
    if (is.null(tg) || !has_study()) return(NULL)
    a <- rv$p$ard$analyses
    a[!is.na(a$output_id) & a$output_id == tg & !is.na(a$analysis_id), ,
      drop = FALSE]
  })
  st_row <- shiny::reactive({
    a <- st_rows()
    shiny::req(a, nrow(a), input$ard_stat_row)
    a[a$analysis_id == input$ard_stat_row, , drop = FALSE][1L, ]
  })
  st_kind <- function(r) {
    keys <- .std_ard_methods()
    k <- match(r$method, keys$method)
    if (is.na(k)) "" else keys$kind[k]
  }
  # the format a statistic gets when the analysis says none
  st_default <- function(r, stat) {
    keys <- .std_ard_methods()
    k <- match(r$method, keys$method)
    m <- if (!is.na(k)) .parse_formats(keys$formats[k]) else character()
    if (!is.na(m[stat])) return(unname(m[stat]))
    st <- .std_ard_statistics()
    unname(st$fmt[match(stat, st$statistic)])
  }
  output$ard_stat_ui <- shiny::renderUI({
    a <- st_rows()
    if (is.null(a) || !nrow(a)) {
      return(shiny::p(class = "small text-muted mb-0",
                      t("Choose a report with analyses in the sidebar: its analyses' statistics and formats are picked here.")))
    }
    ids <- a$analysis_id
    sel <- shiny::isolate(input$ard_stat_row)
    shiny::tagList(
      shiny::selectInput("ard_stat_row", t("Analysis"),
                         stats::setNames(ids, paste0(ids, "  (", a$method, ")")),
                         selected = if (isTRUE(sel %in% ids)) sel else ids[1L],
                         width = "100%"),
      shiny::uiOutput("ard_stat_form"))
  })
  output$ard_stat_form <- shiny::renderUI({
    r <- st_row()
    rv$ver
    st_env$n <- st_env$n + 1L
    st_drawn(st_env$n)
    kind <- st_kind(r)
    kinds <- .stat_kinds(kind)
    if (identical(.std_ard_methods()$call[match(r$method, .std_ard_methods()$method)],
                  "(subjects)")) kinds <- "categorical"
    cat <- .std_ard_statistics(kinds)
    cat <- cat[!duplicated(cat$statistic), , drop = FALSE]
    have <- .split_bar(r$statistics)
    extra <- setdiff(have, cat$statistic)
    ch <- lapply(split(cat, factor(cat$group, levels = unique(cat$group))),
                 function(g) stats::setNames(as.list(g$statistic),
                                             paste0(g$statistic, " \u2014 ",
                                                    g$label)))
    if (length(extra)) ch[[t("not in the catalog")]] <-
      stats::setNames(as.list(extra), extra)
    note <- switch(kinds,
      continuous = t("Statistics of the numeric variables, in this order. cards computes its own; tflplanner writes a function for the others (CV, geometric mean, percentiles, CI of the mean ...)."),
      categorical = t("Counts and percents of each level."),
      missing = t("Missing and non-missing counts."),
      t("This method gives a fixed set of results (estimate, confidence limits, p-value ...): the statistics picked here are the ones kept. Blank keeps them all."))
    shiny::tagList(
      shiny::p(class = "small text-muted mb-1", note),
      shiny::selectizeInput(
        st_id("pick"), t("Statistics"), choices = ch, selected = have,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("remove_button"))),
      shiny::uiOutput("ard_stat_fmts"),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center",
        .btn("ard_stat_apply", t("Apply to the analysis"),
             class = "btn-sm btn-primary"),
        shiny::span(class = "small text-muted",
                    t("Format: xx.x = 1 decimal, xx.x% = a proportion as a percent, 2 = 2 decimals, pvalue = <0.001 or 3 decimals. Blank = the default shown."))))
  })
  output$ard_stat_fmts <- shiny::renderUI({
    r <- st_row()
    st_drawn()
    pick <- input[[st_id("pick")]]
    if (!length(pick)) return(NULL)
    f <- .parse_formats(r$formats)
    shiny::div(
      class = "rp-stat-fmt",
      lapply(seq_along(pick), function(i) {
        s <- pick[i]
        shiny::textInput(st_id(paste0("f_", s)), s,
                         value = if (!is.na(f[s])) f[[s]] else "",
                         placeholder = st_default(r, s))
      }))
  })
  shiny::observeEvent(input$ard_stat_apply, {
    r <- st_row()
    pick <- input[[st_id("pick")]] %||% character()
    fm <- vapply(pick, function(s) trimws(input[[st_id(paste0("f_", s))]] %||% ""),
                 "")
    fm <- fm[nzchar(fm)]
    bad <- names(fm)[!.fmt_ok(fm)]
    if (length(bad)) {
      return(notify(sprintf(t("Not a format: %s"), paste(bad, collapse = ", ")),
                    "warning"))
    }
    # the analysis's variable-specific formats (AGE:mean=...) stay
    old <- .parse_formats(r$formats)
    keep <- old[grepl(":", names(old), fixed = TRUE)]
    fm <- c(fm, keep)
    a <- rv$p$ard$analyses
    i <- which(a$output_id == r$output_id & a$analysis_id == r$analysis_id)[1L]
    a$statistics[i] <- if (length(pick)) paste(pick, collapse = " | ") else NA
    a$formats[i] <- if (length(fm))
      paste(paste0(names(fm), "=", fm), collapse = " | ") else NA
    rv$p$ard$analyses <- a
    bump()
    notify(sprintf(t("%s: statistics and formats written"), r$analysis_id))
  })
  ard_valid <- shiny::reactive({
    shiny::req(has_study())
    tryCatch({
      .ard_spec(rv$p$ard)
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
    code <- ard_code_now()
    if (is.character(code) && length(code) == 1L && !is.null(attr(code, "msg"))) {
      return(code)
    }
    paste(code, collapse = "\n")
  })
  # what the Code panel shows, as the save would write it
  ard_code_now <- shiny::reactive({
    a <- structure(rv$p$ard, class = "tfl_ard_spec")
    msg <- function(x) structure(x, msg = TRUE)
    scope <- input$ard_scope %||% "report"
    id <- ard_target()
    if (scope == "report") {
      if (is.null(id)) return(msg(t("Choose a report in the sidebar.")))
      if (!any(a$analyses$output_id %in% id)) {
        return(msg(sprintf(t("%s has no analyses in the ARD definition."), id)))
      }
    }
    tryCatch(switch(scope,
      report = ard_program_code(a, id),
      setup = ard_setup_code(a),
      autoexec = ard_autoexec_code(a)),
      error = function(e) msg(paste(t("The code cannot be written yet:"),
                                    conditionMessage(e))))
  })
  output$ard_prog_state <- shiny::renderUI({
    shiny::req(has_study())
    code <- ard_code_now()
    if (!is.null(attr(code, "msg"))) return(NULL)
    rv$status_ver
    lay <- study_layout()
    name <- switch(input$ard_scope %||% "report",
                   report = .ard_prog_name(ard_target()),
                   setup = .ard_setup_file, autoexec = .ard_autoexec_file)
    f <- file.path(rv$study$path, lay[["programs_ard"]], name)
    st <- .ard_program_state(code, f)
    shiny::p(class = "small text-muted mb-1",
             shiny::code(file.path(lay[["programs_ard"]], name)), " ",
             t(switch(st,
               missing = "No file yet. Saving writes it.",
               current = "The saved program is the one below.",
               generated = "The definition has changed: saving rewrites the program as below.",
               edited = "The saved program was edited by hand. Saving leaves it alone.")))
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
  # making the study ARD: always the saved programs (programs/ard/), so
  # what is built is what anyone rerunning autoexec_ard.R gets
  ard_log_ver <- shiny::reactiveVal(0L)
  ard_preview_out <- shiny::reactiveVal(NULL)
  ard_ready <- function() {
    if (!is.null(ard_valid())) {
      notify(t("Correct the ARD definition first."), "warning")
      return(FALSE)
    }
    !(dirty() && !do_save())
  }
  shiny::observeEvent(input$ard_build, {
    if (ard_ready()) start_batch("ard", isTRUE(input$ard_batch_code))
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
    # a height of its own, so the horizontal scroll bar stays in sight
    DT::datatable(ard_view(r$ard), rownames = FALSE, filter = "top",
                  selection = "none", class = "compact stripe nowrap", fillContainer = FALSE,
                  options = list(pageLength = 50, scrollX = TRUE,
                                 scrollY = "55vh", scrollCollapse = TRUE,
                                 dom = "tip"))
  })


  # -- the study ARD, output by output --------------------------------------
  ard_state_ver <- shiny::reactiveVal(0L)
  ard_state <- shiny::reactive({
    ard_state_ver()
    rv$status_ver
    rv$p
    shiny::req(has_study())
    ard_status(current_study())
  })
  ard_state_view <- function() {
    d <- ard_state()
    v <- data.frame(a = d$output_id, b = d$analyses,
                    c = t(c(built = "built", outdated = "outdated",
                            `not built` = "not built", error = "error")[d$state]),
                    d = ifelse(is.na(d$rows), "", d$rows),
                    e = ifelse(is.na(d$built), "", d$built), f = d$error,
                    stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Analyses", "State", "Rows", "Built",
                    "Error"))
    DT::formatStyle(
      .dt(v, selection = "single"), names(v)[3L],
      color = DT::styleEqual(t(c("built", "outdated", "not built", "error")),
                             c("#15803d", "#b45309", "#6b7280", "#b91c1c")))
  }
  output$ard_state <- DT::renderDT(ard_state_view())
  output$ard_state_data <- DT::renderDT(ard_state_view())
  shiny::observeEvent(input$ard_update, {
    id <- ard_target()
    if (is.null(id)) return(notify(t("Choose a report"), "warning"))
    if (!any(rv$p$ard$analyses$output_id %in% id)) {
      return(notify(sprintf(t("%s has no analyses in the ARD definition."), id),
                    "warning"))
    }
    if (!ard_ready()) return()
    r <- NULL
    shiny::withProgress(message = t("Running the ARD program"), {
      r <- guarded(update_study_ard(current_study(), id))
    })
    ard_state_ver(ard_state_ver() + 1L)
    rv$ard_ver <- rv$ard_ver + 1L
    rv$status_ver <- rv$status_ver + 1L
    if (is.null(r)) return()
    ard_preview_out(list(id = id, text = r$output))
    if (r$ok) {
      notify(sprintf(t("%s is in the study ARD (%d rows)"), id, r$rows))
    } else {
      notify(t("The ARD program failed: see what it printed below."), "error")
    }
  })
  shiny::observeEvent(input$ard_build2, {
    if (ard_ready()) start_batch("ard", isTRUE(input$ard_batch_code))
  })
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
  output$catalog <- DT::renderDT({
    shiny::req(has_study())
    d <- rv$p$ard$datasets
    v <- data.frame(a = d$dataset, b = d$level, c = d$path,
                    d = ifelse(file.exists(file.path(rv$study$path, d$path)),
                               "\u2713", t("missing")),
                    stringsAsFactors = FALSE)
    names(v) <- t(c("Dataset", "Level", "File", "Present"))
    .dt(v, selection = "none")
  })


  # -- listings and figures ------------------------------------------------
  # The form is drawn per report with fresh input ids (as the builder's),
  # so an input of another report's form is never read as this one's.
  lf_env <- new.env()
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
    f <- file.path(rv$study$path, pth)
    if (!file.exists(f)) return(character())
    k <- paste(f, file.mtime(f))
    if (is.null(ard_cols[[k]])) {
      ard_cols[[k]] <- tryCatch(names(read_data_head(f, 1L)),
                                error = function(e) character())
    }
    ard_cols[[k]]
  }
  .designer_server(input, output, session, rv, current, t, notify, guarded,
                   catalog)
  output$lf_note <- shiny::renderUI({
    msg <- switch(lf_type(),
      none = t("Choose a Listing or Figure report in the sidebar."),
      table = t("This is a Table: its data is the study ARD (ARD tab) and its layout the Table definition."),
      listing = t("A listing in rows: the data (a dataset of the catalog, a condition, an order) and its columns. Its data code (Reports tab), if any, reworks `data` after the condition."),
      figure = t("A figure reads its datasets here; the plot is its data code (Reports tab), written with ggplot2: it leaves `plot`."))
    shiny::div(class = "alert alert-info py-2 small", msg)
  })
  output$lf_form <- shiny::renderUI({
    rv$ver
    type <- lf_type()
    shiny::req(type %in% c("listing", "figure"))
    id <- current()
    p <- shiny::isolate(rv$p)
    lf_env$n <- lf_env$n + 1L
    lf_drawn(lf_env$n)
    lf_env$id <- id
    cat_ds <- shiny::isolate(catalog())
    ds_choices <- stats::setNames(cat_ds$dataset,
                                  paste0(cat_ds$dataset, " (", cat_ds$level, ")"))
    if (type == "figure") {
      f <- lf_rows(p, "figures", id)
      have <- if (nrow(f)) .split_bar(f$datasets[1L]) else character()
      return(shiny::div(
        class = "rp-b-card",
        shiny::h6(t("Data the figure reads")),
        shiny::selectizeInput(lf_id("fig_ds"), NULL, ds_choices,
                              selected = have, multiple = TRUE,
                              width = "100%"),
        shiny::h6(t("The program's data part")),
        shiny::div(class = "rp-code", shiny::verbatimTextOutput("lf_fig_code"))))
    }
    l <- lf_rows(p, "listings", id)
    lv <- function(k, d = "") if (nrow(l) && !is.na(l[[k]][1L])) l[[k]][1L] else d
    lt <- listing_types()
    shiny::div(
      class = "rp-b-card",
      shiny::h6(t("Listing")),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::selectInput(lf_id("type"), t("Type"),
                           stats::setNames(lt$type, lt$label),
                           selected = lv("type", .std_setting("listing_type",
                                                             "multiline")),
                           width = "200px"),
        shiny::selectInput(lf_id("dataset"), t("Dataset"),
                           c(stats::setNames("", "-"), ds_choices),
                           selected = lv("dataset"), width = "200px"),
        shiny::numericInput(lf_id("max_rows"), t("Rows per page"),
                            value = suppressWarnings(as.numeric(lv("max_rows", NA))),
                            min = 1, width = "140px")),
      shiny::textInput(lf_id("where"), t("Condition (R)"), value = lv("where"),
                       width = "100%",
                       placeholder = "AESEV == \"SEVERE\""),
      shiny::textInput(lf_id("sort"), t("Order (| between variables, - for descending)"),
                       value = lv("sort"), width = "100%",
                       placeholder = "TRTA | USUBJID | ASTDT"))
  })
  # the form's values back to the listing / figure rows
  lf_values <- shiny::reactive({
    lf_drawn()
    shiny::req(identical(lf_env$id, current()))
    get <- function(x) input[[lf_id(x)]]
    if (identical(lf_type(), "figure")) {
      return(list(sheet = "figures", rows = data.frame(
        datasets = paste(get("fig_ds") %||% character(), collapse = " | "),
        stringsAsFactors = FALSE)))
    }
    shiny::req(!is.null(get("type")))
    mr <- get("max_rows")
    list(sheet = "listings", rows = data.frame(
      type = get("type"), dataset = get("dataset") %||% "",
      where = get("where") %||% "", sort = get("sort") %||% "",
      max_rows = if (is.null(mr) || is.na(mr)) "" else as.character(mr),
      stringsAsFactors = FALSE))
  })
  lf_values_d <- shiny::debounce(lf_values, 400)
  shiny::observeEvent(lf_values_d(), {
    v <- lf_values_d()
    id <- lf_env$id
    p2 <- set_lf_rows(rv$p, v$sheet, id, v$rows)
    if (!identical(p2$lf, rv$p$lf)) rv$p <- p2
  })
  output$lf_fig_code <- shiny::renderText({
    id <- current()
    shiny::req(identical(lf_type(), "figure"))
    paste(data_lines(rv$p, id), collapse = "\n")
  })
  output$lf_cols_box <- shiny::renderUI({
    shiny::req(identical(lf_type(), "listing"))
    shiny::div(
      class = "rp-b-card",
      shiny::h6(t("Columns, in order")),
      shiny::p(class = "small text-muted",
               t("vars: variables stacked in the column (| between them); label: \\n breaks the header; width: characters; collapse_repeats: print a repeated value once.")),
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
    .grid(d, "listing_cols", lf_cols_key(),
          list(vars = cols, collapse_repeats = .bool))
  })
  shiny::observeEvent(input$hot_lf_cols, {
    h <- input$hot_lf_cols
    if (is.null(h$changes$changes) &&
        !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
    if (!identical(h$params$planner_key, lf_cols_key())) return()
    rv$p <- set_lf_rows(rv$p, "listing_cols", current(),
                        rhandsontable::hot_to_r(h))
  })
  lf_pv <- shiny::reactiveVal(NULL)
  shiny::observeEvent(current(), lf_pv(NULL))
  shiny::observeEvent(input$lf_preview, {
    id <- current()
    if (!identical(lf_type(), "listing")) {
      return(notify(t("Choose a Listing report"), "warning"))
    }
    r <- tryCatch(list(pages = preview_listing(current_study(), id)),
                  error = function(e) list(error = conditionMessage(e)))
    lf_pv(r)
  })
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
    st <- tryCatch(ard_status(current_study()), error = function(e) NULL)
    ard_of <- if (!is.null(st)) st$state[match(o$output_id, st$output_id)] else
      rep(NA_character_, nrow(o))
    data.frame(
      output_id = o$output_id,
      type = unname(.type_labels[vapply(info, `[[`, "", "type")]),
      program = vapply(info, `[[`, "", "program"),
      rtf = vapply(info, `[[`, "", "file"),
      data = ifelse(!is.na(ard_of), paste("ARD:", ard_of),
                    ifelse(is.na(o$data_code), "TODO", "\u2713")),
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
    f <- file.path(rv$study$path, study_layout()[["programs_tfl"]],
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
    bi <- input$batches_rows_selected
    if (length(bi)) {
      b <- batches()[bi, , drop = FALSE]
      info <- file.path(b$path, "batch.txt")
      run <- file.path(b$path, "run.csv")
      r <- if (file.exists(run)) utils::read.csv(run, colClasses = "character")
      return(paste(c(
        if (file.exists(info)) readLines(info, warn = FALSE, encoding = "UTF-8"),
        "",
        if (!is.null(r)) utils::capture.output(print(
          r[c("part", "program", "status", "errors", "warnings", "seconds",
              "note")], right = FALSE, row.names = FALSE))),
        collapse = "\n"))
    }
    d <- selected_status()
    rv$status_ver
    if (!nrow(d)) {
      return(t("Choose a report to see what its last preview printed, or a batch folder to see its run."))
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
  start_batch <- function(parts, code) {
    if (!is.null(rv$job)) return(notify(t("A run is going on"), "warning"))
    if (dirty() && !do_save()) return()
    parts <- if (identical(parts, "all")) c("ard", "tfl") else parts
    px <- guarded(run_batch(rv$study, parts, code = code, wait = FALSE))
    if (is.null(px)) return()
    rv$job <- px
    rv$job_what <- paste(t("the official run:"),
                         paste(t(c(ard = "ARD", tfl = "Reports")[parts]),
                               collapse = ", "))
  }
  shiny::observeEvent(input$batch_run, {
    parts <- input$batch_parts %||% "all"
    if (parts %in% c("ard", "all") && !is.null(ard_valid())) {
      return(notify(t("Correct the ARD definition first."), "warning"))
    }
    start_batch(parts, isTRUE(input$batch_code))
  })
  batches <- shiny::reactive({
    rv$status_ver
    shiny::req(has_study())
    list_batches(current_study())
  })
  output$batches <- DT::renderDT({
    d <- batches()
    v <- data.frame(a = d$batch, b = d$started,
                    c = t(c(ard = "ARD", report = "Reports",
                            all = "ARD, then reports")[d$what]),
                    d = d$programs, e = d$errors,
                    f = ifelse(d$code, "\u2713", ""),
                    stringsAsFactors = FALSE)
    names(v) <- t(c("Batch folder", "Started", "Runs", "Programs", "Errors",
                    "Code"))
    DT::formatStyle(.dt(v, selection = "single"), names(v)[5L],
                    color = DT::styleInterval(0, c("#15803d", "#b91c1c")))
  })
  shiny::observeEvent(input$batch_open, {
    d <- batches()
    i <- input$batches_rows_selected
    if (!length(i)) {
      .open_folder(file.path(rv$study$path, study_layout()[["runs"]]))
    } else .open_folder(d$path[i])
  })
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
