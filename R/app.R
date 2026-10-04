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
.study_tabs <- c("data", "outputs", "ard", "results")

.sheet_labels <- c(
  tables = "tables: roles", variables = "variables",
  codelists = "codelists: code list", cells = "cells",
  layout = "layout: pages", columns = "columns", style = "style",
  cell_styles = "cell_styles: cell looks",
  col_header = "col_header: column header",
  report = "report", page = "page", header = "header", footer = "footer",
  titles = "titles", footnotes = "footnotes")

.type_labels <- c(table = "Table", listing = "Listing", figure = "Figure")

# Where each kind of report is made, in the order of the work: the page
# (go()'s name) and the button's words.  A fourth kind adds its row here.
.type_moves <- list(
  table = c(ard = "Go to ARD", tables = "Make the table"),
  figure = c(designer = "Make the figure"),
  listing = c(lf = "Make the listing"))

# The top tabs a kind of report has nothing on: shown faded while such a
# report is chosen (still open to click; the tab then says why).
.type_idle_tabs <- list(table = character(), figure = "ard", listing = "ard")

# Text cut to `n` characters, with an ellipsis.
# What each study folder holds, by its study_layout() name (a folder with no
# note yet shows none, so a new folder never breaks the study tab).
.study_folder_notes <- function(nm, t = identity) {
  notes <- c(
    adam = "Input data: ADaM", sdtm = "Input data: SDTM",
    other = "Input data: other",
    spec = "Definition workbooks (written on save)",
    programs_ard = "ARD programs: one per output, ard_setup.R, autoexec_ard.R",
    programs_tfl = "Report programs, autoexec_report.R",
    ard = "Working data: the study ARD (ard.rds)",
    tfl = "Working reports: RTF",
    ard_import = "ARDs made elsewhere, taken in (imports.csv lists them)",
    runs = "Official runs: one batch folder each (logs, results, code)",
    logs_preview = "What a report program printed in its last preview")
  out <- unname(notes[nm])
  has <- !is.na(out)
  out[has] <- vapply(out[has], function(x) t(x), character(1))
  out[!has] <- ""
  out
}

.ellipsis <- function(x, n) {
  ifelse(nchar(x) > n, paste0(substr(x, 1L, n - 1L), "\u2026"), x)
}

# A table cell as escaped HTML, with `tip` shown on hover (none when blank).
.cell_tip <- function(text, tip = "") {
  esc <- htmltools::htmlEscape
  ifelse(nzchar(tip),
         sprintf('<span title="%s">%s</span>', esc(tip, attribute = TRUE),
                 esc(text)),
         esc(text))
}

# the sheets whose tab offers help of its own above the grid
.assisted <- c("tables", "variables", "cells", "col_header")

# a report's ARD, in the same four words everywhere (ard_status()'s states)
.ard_state_labels <- c(built = "Made", outdated = "Outdated",
                       `not built` = "Not made", error = "Error")

# a report's run status in the four words (the status says why)
.run_state <- c(ok = "built", outdated = "outdated", unsaved = "outdated",
                todo = "not built", "not run" = "not built",
                "no program" = "not built", error = "error")

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
#' @param stop_on_close Stop the app when its last browser tab is closed
#'   (after a few seconds, so that reloading the page does not stop it).
#'   The shortcut and [launch_app()] start it this way.
#' @return `planner_app()` returns a [shiny::shinyApp()] object;
#'   `run_app()` runs it.
#' @seealso [launch_app()] to start it in its own R process, as the
#'   shortcut does ([add_shortcut()]).
#' @examples
#' \dontrun{
#' run_app()
#' run_app("ABC-101")
#' }
#' @export
run_app <- function(study = NULL, ..., stop_on_close = FALSE) {
  shiny::runApp(planner_app(study, stop_on_close = stop_on_close), ...)
}

#' @rdname run_app
#' @export
planner_app <- function(study = NULL, stop_on_close = FALSE) {
  if (!.is_set_up()) setup_tflplanner(home = NULL)
  .refresh_launcher()
  start <- if (!is.null(study)) open_study(study)
  shiny::shinyApp(function(req) app_ui(tflplanner_language()),
                  function(input, output, session) {
                    if (isTRUE(stop_on_close)) .stop_when_closed(session)
                    app_server(input, output, session, start)
                  })
}

# Stop the app once no browser tab is left: a tab that closes starts a
# short wait, and the app stops if no tab has come back by then (a reload
# ends one session and starts the next within it).
.open_tabs <- new.env()
.open_tabs$n <- 0L

.stop_when_closed <- function(session, wait = 5) {
  .open_tabs$n <- .open_tabs$n + 1L
  session$onSessionEnded(function() {
    .open_tabs$n <- .open_tabs$n - 1L
    later::later(function() {
      if (.open_tabs$n <= 0L) shiny::stopApp()
    }, wait)
  })
}

# ------------------------------------------------------------------- UI

.code_css <- "
/* a top tab the chosen report has nothing on */
.nav-link.rp-idle { opacity: .45; }
/* While the server works (opening a study, switching a tab or a report,
   saving) the page takes no clicks: a veil, after 0.4 s so that the
   short updates (the builder's preview, a poll) do not flicker or block. */
body::after { content: ''; position: fixed; inset: 0; z-index: 2000;
  visibility: hidden; background: rgba(255,255,255,0); cursor: progress;
  transition: visibility 0s, background 0s; }
html.shiny-busy body::after { visibility: visible;
  background: rgba(255,255,255,.35);
  transition: visibility 0s .4s, background .2s .4s; }
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
      if (identical(sheet, "codelists")) shiny::fileInput(
        "codelist_file", t("Read the study's code list (xlsx / csv: variable, value, label, order)"),
        accept = c(".xlsx", ".csv"), width = "100%"),
      rhandsontable::rHandsontableOutput(paste0("hot_", sheet)),
      shiny::uiOutput(paste0("inh_", sheet)),
      shiny::tags$details(
        class = "rp-help mt-2",
        shiny::tags$summary(t("Column help")),
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
                            shiny::tags$script(shiny::HTML(.split_js)),
                            shiny::tags$script(shiny::HTML(.unsaved_js)),
                            shiny::tags$script(shiny::HTML(.updating_js)),
                            shiny::uiOutput("update_note")),
    sidebar = bslib::sidebar(
      id = "side", width = 270, open = "closed",
      shiny::uiOutput("study_side"),
      shiny::selectInput("target", t("Report (output_id)"),
                         choices = c("-" = .all_rows), width = "100%"),
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
      t("Study"), value = "study",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
        bslib::card(
          bslib::card_header(t("Studies")),
          shiny::uiOutput("welcome"),
          shiny::uiOutput("studies_root_note"),
          DT::DTOutput("studies"),
          shiny::p(class = "small text-muted mb-1",
                   t("Click a study to see it on the right; double-click to open it.")),
          # New study and Register a folder are there even with no study;
          # what acts on a study needs one
          # (the panel that hides itself carries no display class: a
          # Bootstrap d-* class would win over its display: none)
          shiny::div(
            class = "d-flex flex-wrap gap-2",
            shiny::conditionalPanel(
              "output.n_studies > 0",
              .btn("open_study", t("Open"), class = "btn-sm btn-primary")),
            .btn("new_study", t("New study...")),
            .btn("register", t("Register a folder")),
            shiny::conditionalPanel(
              "output.n_studies > 0",
              shiny::span(class = "d-inline-flex gap-2",
                          .btn("unregister", t("Unregister"),
                               class = "btn-sm btn-outline-danger"),
                          .btn("refresh_studies", t("Refresh"))))),
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
      t("Reports"), value = "outputs",
      # one place for a report: the list, then the report chosen (in the
      # list or the sidebar) -- its content (by its kind), page and code
      bslib::navset_underline(
        id = "rep_nav",
        bslib::nav_panel(
          t("All reports"), value = "list",
          shiny::uiOutput("next_steps"),
        bslib::card(
          bslib::card_header(t("Reports (TFL)")),
          DT::DTOutput("outputs"),
          shiny::uiOutput("report_moves"),
          shiny::div(
            class = "d-flex flex-wrap gap-1",
            .btn("add", t("Add")), .btn("copy", t("Copy")),
            .btn("rename", t("Rename")),
            .btn("remove", t("Delete"), class = "btn-sm btn-outline-danger"),
            .btn("up", "\u2191"), .btn("down", "\u2193")),
          shiny::p(class = "text-muted small mt-1",
                   t("Reports are made in this order (the official run too). Copy makes a new report with all of this one's definition.")))),
        bslib::nav_panel(
          t("Content"), value = "content",
          shiny::uiOutput("report_head"),
          shiny::conditionalPanel(
            "output.report_kind == ''",
            shiny::p(class = "text-muted mt-3",
                     t("Choose a report in the list or the sidebar."))),
    shiny::conditionalPanel(
      "output.report_kind == 'table'",
      # not a card: a card around the sheets' own card makes the inner one a
      # fill item of a box with no height, and every grid in it 0 px high
      bslib::navset_underline(
        id = "table_nav",
        bslib::nav_panel(
          t("Table (builder)"), value = "builder",
      shiny::uiOutput("builder_note"),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        shiny::uiOutput("builder_form"),
        bslib::card(
          bslib::card_header(shiny::div(
            class = "d-flex justify-content-between",
            shiny::span(t("Preview: the table as it will print"),
                        shiny::span(id = "builder_updating",
                                    class = "badge text-bg-warning ms-2 d-none",
                                    t("Updating ..."))),
            shiny::uiOutput("builder_pages", inline = TRUE))),
          shiny::uiOutput("builder_preview")))),
        bslib::nav_panel(
          t("Details (sheets)"), value = "table_spec",
      shiny::uiOutput("type_note"),
      shiny::div(class = "rp-assist border rounded p-2 mb-2",
                 shiny::uiOutput("assist")),
      grid_note,
      do.call(bslib::navset_card_underline,
              c(list(id = "table_sheet"), lapply(table_sheets(), sheet_panel)))
        ))),
    shiny::conditionalPanel(
      "output.report_kind == 'listing'",
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
    .designer_ui(t)),
    bslib::nav_panel(
      t("Page"), value = "page",
      shiny::p(class = "small text-muted",
               t("The page of the report chosen in the sidebar: its titles, footnotes and its own header or footer. Study defaults = every report's.")),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
        shiny::div(
          grid_note,
          do.call(bslib::navset_card_underline,
                  lapply(report_sheets(), sheet_panel))),
        bslib::card(
          bslib::card_header(shiny::div(
            class = "d-flex justify-content-between align-items-center",
            shiny::span(t("First page (sample)")),
            .btn("page_full", t("Full size"),
                 class = "btn-sm btn-outline-secondary py-0"))),
          shiny::uiOutput("page_sample")))),
        bslib::nav_panel(
          t("Code"), value = "code",
        bslib::navset_card_tab(
          bslib::nav_panel(
            t("Data code"),
            shiny::uiOutput("current_label"),
            shiny::textInput("description", t("Description"), width = "100%"),
            shiny::div(
              class = "rp-code",
              shiny::textAreaInput(
                "data_code",
                shiny::span(
                  title = t("1. ARD: make `ard` (Listing: rework `data`; Figure: the plot). Blank for a table = the company template: its rows of the study ARD."),
                  t("1. Data code: usually blank (the company's standard code is used). Write code here to make the data yourself; for a figure, the plot."), " \u24d8"),
                rows = 10, width = "100%", resize = "vertical"),
              shiny::textAreaInput(
                "process_code",
                shiny::span(
                  title = t("2. Normalize and rework: make `data` from `ard` (normalize_ard(), then mutate() ...). Blank = the company template (normalize)."),
                  t("2. Rework: usually blank (the standard one). Write code to change the data before the table is made."), " \u24d8"),
                rows = 5, width = "100%", resize = "vertical",
                placeholder = "data <- normalize_ard(ard)"),
              shiny::div(
                class = "d-flex gap-2 align-items-center mb-2",
                .btn("fetch", t("Preview"),
                     class = "btn-sm btn-outline-primary"),
                shiny::span(class = "small text-muted",
                            t("Makes this report's ARD (as the ARD tab's Preview), runs 1 and 2 from the study folder and reads the variables, levels and statistics for input assistance."))),
              shiny::uiOutput("ard_summary"),
              shiny::textAreaInput(
                "setup",
                t("Setup code every report runs first (library(), common data)"),
                rows = 4, width = "100%", resize = "vertical"))),
          bslib::nav_panel(
            t("Program"),
            shiny::uiOutput("program_state"),
            shiny::div(class = "rp-code",
                       shiny::verbatimTextOutput("program"))))))),

    bslib::nav_panel(
      "ARD", value = "ard",
      shiny::uiOutput("ard_kind_note"),
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
            bslib::nav_panel(paste0(t("The analyses"), " (analyses)"), value = "analyses",
                             shiny::checkboxInput(
                               "ard_all", t("Every report's analyses (with output_id)"),
                               FALSE),
                             rhandsontable::rHandsontableOutput("hot_ard_analyses")),
            bslib::nav_panel(paste0(t("Datasets"), " (datasets)"), value = "datasets",
                             rhandsontable::rHandsontableOutput("hot_ard_datasets")),
            bslib::nav_panel(paste0(t("Analysis sets"), " (populations)"), value = "populations",
                             rhandsontable::rHandsontableOutput("hot_ard_populations")),
            bslib::nav_panel(paste0(t("Study keys"), " (study)"), value = "study",
                             rhandsontable::rHandsontableOutput("hot_ard_study"))),
          shiny::uiOutput("ard_check"),
          shiny::div(
            class = "rp-b-card mt-2",
            shiny::h6(t("Analysis")),
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
            shiny::p(class = "small text-muted mt-1 mb-1",
                     t("Preview (on this report's ARD) puts one report into the working study ARD and keeps no log. The official run of the whole study ARD, with its logs (logrx), is on the Results tab. Choose a row to see its log in the latest official run.")),
            DT::DTOutput("ard_state"),
            shiny::div(class = "rp-code mt-2",
                       shiny::verbatimTextOutput("ard_log"))),
          bslib::nav_panel(
            t("ARD (this report)"), value = "result",
            shiny::div(
              class = "d-flex flex-wrap gap-2 align-items-center",
              .btn("ard_preview", t("Preview"), class = "btn-sm btn-primary"),
              shiny::span(class = "small text-muted",
                          t("Saves, runs this report's ARD program into the study ARD, and reads it for the table builder and the fills."))),
            shiny::uiOutput("ard_run_info"),
            shiny::div(class = "rp-resize",
                       DT::DTOutput("ard_table", height = "auto", fill = FALSE)))))),

    bslib::nav_panel(
      t("Runs"), value = "results",
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
    bslib::nav_item(shiny::uiOutput("save_btn")))
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
# While the study has unsaved changes, leaving the page (reload, closing the
# tab) asks first; the server says when (message "tflplanner-dirty").
# The builder's preview says "Updating" from the moment the builder writes a
# change to the sheets (the server says so) until the new table arrives.
.updating_js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('builder-updating', function(x) {
    $('#builder_updating').removeClass('d-none');
    $('#builder_preview').css('opacity', 0.4);
    clearTimeout(window.rpUpd);
    window.rpUpd = setTimeout(function() {
      $('#builder_updating').addClass('d-none');
      $('#builder_preview').css('opacity', 1);
    }, 30000);
  });
});
$(document).on('shiny:value', function(e) {
  if (e.name === 'builder_preview') {
    $('#builder_updating').addClass('d-none');
    $('#builder_preview').css('opacity', 1);
  }
});
// The top tabs the chosen report's kind has nothing on: faded
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('rp-idle-tabs', function(x) {
    $('#nav a[data-value]').each(function() {
      $(this).toggleClass('rp-idle', x.indexOf($(this).attr('data-value')) >= 0);
    });
  });
});
// The study ARD's list: a double click on a row opens that report's ARD
$(document).on('dblclick', '#ard_state tbody tr', function() {
  var id = $(this).find('td').first().text();
  if (id) Shiny.setInputValue('ard_state_dbl', id, {priority: 'event'});
});
// Save: says \"Saving...\" at once and cannot be pressed again until the
// server has finished (it then draws the button and the state anew).
$(document).on('click', '#save', function() {
  var b = $(this), saving = b.attr('data-saving');
  setTimeout(function() { b.prop('disabled', true).text(saving); }, 0);
  $('#save_state').html('<span class=\"text-muted me-2\">' + saving + '</span>');
});
"

.unsaved_js <- "
(function() {
  var dirty = false;
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('tflplanner-dirty', function(x) { dirty = !!x; });
  });
  window.addEventListener('beforeunload', function(e) {
    if (dirty) { e.preventDefault(); e.returnValue = ''; }
  });
})();
"

.grid_height <- function(n) {
  as.integer(min(max(28 + 23 * (n + 1L) + 20, 100), 420))
}

# A sheet as an editable grid.  `choices` offers values in a dropdown;
# the columns named in `closed` accept nothing else (a typo is refused in
# the cell rather than saved), the others take free text too.
.grid <- function(d, sheet, key, choices, closed = character(),
                  select = FALSE) {
  d <- .na_blank(d)
  if (!nrow(d)) d[1L, ] <- ""
  h <- rhandsontable::rhandsontable(
    d, rowHeaders = TRUE, useTypes = FALSE, stretchH = "all",
    height = .grid_height(nrow(d)), minSpareRows = 1L, planner_key = key,
    selectCallback = select)
  h <- rhandsontable::hot_context_menu(h, allowRowEdit = TRUE,
                                       allowColEdit = FALSE)
  for (cn in names(choices)) {
    if (cn %in% names(d) && length(choices[[cn]])) {
      shut <- cn %in% closed
      h <- suppressWarnings(rhandsontable::hot_col(
        h, cn, type = "dropdown", source = c("", choices[[cn]]),
        strict = shut, allowInvalid = !shut))
    }
  }
  h
}

# The rows a grid sends back, each as long as the grid is wide.  A row
# typed into the spare row starting from a dropdown column arrives with
# fewer cells than columns, and hot_to_r() -- which lays the cells out row
# by row -- then cannot rebuild the table: that is a new row, not an error.
.grid_rows_full <- function(rows, width) {
  if (!length(width) || width < 1L || !is.list(rows)) return(rows)
  lapply(rows, function(r) {
    r <- as.list(r)
    r <- r[seq_len(min(length(r), width))]
    if (length(r) < width) r[(length(r) + 1L):width] <- list(NULL)
    lapply(r, function(x) if (is.null(x)) NA else x)
  })
}

# ARD columns whose values can only be one of their choices (the study's
# datasets and populations).  `method` stays open: besides the keywords it
# takes any pkg::function.
.ard_closed_columns <- c("dataset", "population_id", "denominator")

# tflspec's column help is English; the app shows it in its language
.help_table <- function(sheet) {
  rd <- tflspec::tfl_spec_columns(sheet)
  rd$sheet <- NULL
  rd$description <- tr(rd$description)
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
    studies_ver = 0L, ard_ver = 0L, save_ver = 0L)
  bump <- function() rv$ver <- rv$ver + 1L
  # an ARD method's name and note as the user reads them: the translation
  # kept by the app under "method:<name>" / "method-note:<name>", else
  # tflspec's English (tflspec's catalog stays the English original)
  method_label <- function(method, label) {
    k <- paste0("method:", method)
    v <- t(k)
    fallback <- ifelse(!is.na(label) & nzchar(label), label, method)
    ifelse(v == k, fallback, v)
  }
  method_note <- function(method, note) {
    k <- paste0("method-note:", method)
    v <- t(k)
    ifelse(v == k, note, v)
  }
  notify <- function(msg, type = "message") {
    shiny::showNotification(msg, type = type, duration = 6)
  }
  guarded <- function(expr) {
    # the detail (the calls) goes to the console / log; the screen gets
    # the message only
    tryCatch(
      withCallingHandlers(expr, error = function(e) {
        calls <- vapply(utils::tail(sys.calls(), 12L), function(cl)
          paste(utils::head(deparse(cl, width.cutoff = 80L), 1L), collapse = ""), "")
        message("tflplanner: ", conditionMessage(e), "\n  ",
                paste(calls, collapse = "\n  "))
      }),
      error = function(e) {
        # a problem's full output (a program that failed) is for the log
        if (inherits(e, "tflplanner_problem") && length(e$detail)) {
          message("tflplanner: the output of what failed:\n",
                  paste(e$detail, collapse = "\n"))
        }
        notify(conditionMessage(e), "error")
        NULL
      })
  }

  # Every grid's edits come back through read_grid().  A grid whose change
  # cannot be read (rhandsontable could not rebuild the data frame) must not
  # stop the session: the change is dropped, said so, and every grid is
  # drawn again from what the study holds.
  grids_drawn <- shiny::reactiveVal(0L)
  read_grid <- function(h) {
    h$data <- .grid_rows_full(h$data, length(unlist(h$params$rColHeaders)))
    d <- tryCatch(rhandsontable::hot_to_r(h), error = function(e) NULL)
    if (is.null(d)) {
      notify(t("This change could not be taken in; the grid shows the definition as it was."),
             "warning")
      grids_drawn(grids_drawn() + 1L)
    }
    d
  }
  has_study <- shiny::reactive(!is.null(rv$study))
  dirty <- shiny::reactive({
    has_study() && (!identical(rv$p, rv$saved) ||
                       !identical(rv$meta, rv$saved_meta))
  })
  # for shiny::testServer(), which sees only the session
  session$userData$rv <- rv
  session$userData$grids_drawn <- grids_drawn
  session$userData$dirty <- dirty
  # unsaved changes: the page asks before it is left, and a draft keeps
  # them (written once the edits pause) until they are saved or discarded
  shiny::observe(session$sendCustomMessage("tflplanner-dirty", isTRUE(dirty())))
  draft_due <- shiny::debounce(shiny::reactive(list(dirty(), rv$p, rv$meta)),
                               2000)
  shiny::observe({
    unsaved <- isTRUE(draft_due()[[1L]])
    shiny::isolate({
      if (!has_study() || !is.null(rv$draft)) return()
      if (unsaved) .write_draft(current_study())
      else .drop_draft(rv$study$meta$study_id)
    })
  })

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
    offer_draft(s)
  }
  # a draft left by a session that did not save: take it back, or drop it
  offer_draft <- function(s) {
    d <- .read_draft(s$meta$study_id)
    if (is.null(d)) return(invisible())
    if (identical(d$planner, s$planner) &&
        identical(d$meta[.study_fields], s$meta[.study_fields])) {
      .drop_draft(s$meta$study_id)
      return(invisible())
    }
    rv$draft <- d
    shiny::showModal(shiny::modalDialog(
      title = t("Changes that were not saved"),
      t("The last time this study was open, some changes were not saved. Take them back (they are not saved until you save), or discard them?"),
      footer = shiny::tagList(
        .btn("draft_discard", t("Discard"), class = "btn-outline-secondary"),
        .btn("draft_restore", t("Take them back"), class = "btn-primary")),
      easyClose = FALSE))
  }
  shiny::observeEvent(input$draft_restore, {
    d <- rv$draft
    if (!is.null(d)) {
      rv$p <- .study_spec_keys(d$planner)
      rv$meta <- d$meta[.study_fields]
      bump()
    }
    rv$draft <- NULL
    shiny::removeModal()
  })
  shiny::observeEvent(input$draft_discard, {
    if (has_study()) .drop_draft(rv$study$meta$study_id)
    rv$draft <- NULL
    shiny::removeModal()
  })
  # (outside any reactive context at startup: its bump() reads rv$ver)
  if (is.null(start)) {
    last <- tflplanner_config()$last_study
    if (!is.null(last) && last %in% list_studies()$study_id) {
      start <- tryCatch(open_study(last), error = function(e) NULL)
    }
  }
  if (!is.null(start)) shiny::isolate(set_study(start))

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
    # the button and the state are drawn anew once done, whatever happened
    # (the page made them say "Saving..." and the button unpressable)
    on.exit(rv$save_ver <- shiny::isolate(rv$save_ver) + 1L, add = TRUE)
    if (!has_study()) return(notify(t("Open a study first"), "warning"))
    do_save()
  })
  output$save_state <- shiny::renderUI({
    rv$save_ver
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
    shiny::req(nrow(d) > 0L)
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
  output$n_studies <- shiny::renderText(nrow(studies()))
  shiny::outputOptions(output, "n_studies", suspendWhenHidden = FALSE)
  output$save_btn <- shiny::renderUI({
    rv$save_ver
    if (has_study()) .btn("save", t("Save"), class = "btn-sm btn-primary",
                          `data-saving` = t("Saving..."))
  })
  # no study yet: the ways to start, the sample first
  output$welcome <- shiny::renderUI({
    if (nrow(studies())) return(NULL)
    shiny::div(
      class = "border rounded p-3 mb-3 bg-light",
      shiny::h5(t("Getting started")),
      shiny::p(t("tflplanner makes a study's tables, listings and figures: the data, the analyses (ARD), each report's definition, and the programs that make the RTFs.")),
      shiny::div(
        class = "d-flex flex-wrap gap-2 mb-2",
        .btn("try_sample", t("Try the sample study (about 1 minute)..."),
             class = "btn btn-primary"),
        .btn("start_empty", t("New study..."),
             class = "btn btn-outline-primary"),
        .btn("register2", t("Register a study folder"),
             class = "btn btn-outline-secondary")),
      shiny::p(class = "small text-muted mb-0",
               t("The sample uses the CDISC pilot data: it makes its ARD, 5 tables, a listing and 2 figures, to look at and change.")))
  })
  # both open the New study dialog: the sample chosen, or nothing chosen
  # yet; the study's ID is given there
  shiny::observeEvent(input$try_sample, open_new_study("sample"))
  shiny::observeEvent(input$start_empty, open_new_study("empty"))
  # SAMPLE-01, or the first SAMPLE-nn not taken
  next_sample_id <- function(root = studies_root()) {
    id <- "SAMPLE-01"
    k <- 1L
    while (id %in% studies()$study_id || dir.exists(file.path(root, id))) {
      k <- k + 1L
      id <- sprintf("SAMPLE-%02d", k)
    }
    id
  }
  shiny::observeEvent(input$register2, show_register())
  # A newer tflplanner / tflspec / rtfreporter: said, not installed -- the
  # app cannot replace packages it has loaded.  The check runs once per R
  # process in the background (.start_update_check()).
  .start_update_check()
  update_found <- shiny::reactive({
    r <- .update_check_result()
    if (is.null(r)) shiny::invalidateLater(2000)
    r
  })
  output$update_note <- shiny::renderUI({
    r <- update_found()
    if (!length(r) || isTRUE(input$update_note_hide)) return(NULL)
    shiny::div(
      class = "alert alert-info alert-dismissible small py-2 mb-2",
      shiny::strong(t("A newer version is out:")), " ",
      paste(names(r), r, collapse = ", "), ". ",
      t("To update: close tflplanner and start it from the \"update and launch\" shortcut, or run update_tflplanner() in R."),
      shiny::tags$button(
        type = "button", class = "btn-close",
        onclick = "Shiny.setInputValue('update_note_hide', true);"))
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
      shiny::checkboxInput(
        "check_updates", t("Look for a newer version when tflplanner starts"),
        value = !isFALSE(as.logical(tflplanner_config()$check_updates %||% "true"))),
      shiny::p(class = "small text-muted mb-3",
               sprintf(t("Updates (%s channel) are installed from R, not from the app: update_tflplanner(), or the \"update and launch\" shortcut."),
                       .update_channel())),
      shiny::p(class = "small text-muted mb-1",
               t("The sample study: CDISC pilot data (pharmaverseadam), its ARD, 5 tables, a listing and 2 figures.")),
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
  shiny::observeEvent(input$check_updates, {
    now <- !isFALSE(as.logical(tflplanner_config()$check_updates %||% "true"))
    if (!identical(isTRUE(input$check_updates), now)) {
      guarded(suppressMessages(setup_tflplanner(
        check_updates = isTRUE(input$check_updates))))
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$save_language, {
    if (dirty()) {
      return(notify(t("There are unsaved changes. Save first."), "warning"))
    }
    guarded(suppressMessages(setup_tflplanner(language = input$language)))
    shiny::showNotification(
      shiny::tagList(t("The language is changed: the page reloads to show it."), " ",
                     shiny::tags$a(href = "javascript:location.reload()",
                                   t("Reload now"))),
      duration = NULL, type = "message")
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
  shiny::observeEvent(input$register, show_register())
  show_register <- function() {
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
  }
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
      to_open(d)
      return(shiny::showModal(shiny::modalDialog(
        title = sprintf(t("%s has changes that are not saved"),
                        rv$study$meta$study_id),
        sprintf(t("Save them and open %s, or open it without saving them (they are kept as a draft: opening %s again offers them back)."),
                d$study_id, rv$study$meta$study_id),
        footer = shiny::tagList(
          shiny::modalButton(t("Cancel")),
          .btn("open_discard", t("Open without saving"),
               class = "btn-outline-secondary"),
          .btn("open_save", t("Save and open"), class = "btn-primary")))))
    }
    open_now(d)
  }
  to_open <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$open_save, {
    shiny::removeModal()
    if (isTRUE(do_save())) open_now(to_open())
  })
  shiny::observeEvent(input$open_discard, {
    shiny::removeModal()
    guarded(.write_draft(current_study()))
    open_now(to_open())
  })
  open_now <- function(d) {
    if (is.null(d)) return()
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
  shiny::observeEvent(input$new_study, open_new_study("sample"))
  open_new_study <- function(from = "sample") {
    shiny::showModal(shiny::modalDialog(
      title = t("New study"),
      shiny::radioButtons(
        "ns_from", t("Start from"),
        {
          ch <- stats::setNames(
            c("sample", "empty", "study"),
            c(t("The sample study: data, ARD definition, tables, listing and figures (made at once)"),
              t("Start empty"),
              t("Copy another study (definition and data code)")))
          # copying needs a study to copy from
          if (nrow(studies())) ch else ch[1:2]
        },
        selected = from, width = "100%"),
      shiny::conditionalPanel(
        "input.ns_from == 'study'",
        shiny::selectInput("ns_src", t("Copy from"),
                           stats::setNames(studies()$study_id,
                                           studies()$study_id))),
      shiny::textInput("ns_id",
                       t("Study ID (the folder name: letters, digits . _ -)"),
                       value = if (identical(from, "sample")) next_sample_id() else ""),
      shiny::uiOutput("ns_id_check"),
      shiny::tags$details(
        class = "mb-2",
        shiny::tags$summary(t("Details (optional)")),
        shiny::textInput("ns_title", t("Title"), width = "100%"),
        shiny::textInput("ns_compound", t("Compound")),
        shiny::textInput("ns_phase", t("Phase")),
        shiny::textAreaInput("ns_description", t("Description"), width = "100%")),
      shiny::tags$details(
        shiny::tags$summary(t("Where the study folder goes")),
        shiny::textInput("ns_root", t("Create the study folder in"),
                         value = studies_root(), width = "100%")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ns_ok", t("Create"),
                                   class = "btn-primary")),
      easyClose = TRUE))
  }
  # the ID is checked where it is typed, not in a message after Create
  ns_id_problem <- function(id) {
    id <- trimws(id %||% "")
    if (!nzchar(id)) return(t("Give the study an ID."))
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", id)) {
      return(t("Use letters, digits, '.', '_' or '-' only (e.g. ABC-101)."))
    }
    if (id %in% studies()$study_id) return(t("A study with this ID is already registered."))
    root <- trimws(input$ns_root %||% "")
    if (nzchar(root) && dir.exists(file.path(root, id))) {
      return(sprintf(t("A folder %s is already there: choose another ID, or register that folder (Studies > Register a folder)."),
                     file.path(root, id)))
    }
    NULL
  }
  output$ns_id_check <- shiny::renderUI({
    msg <- ns_id_problem(input$ns_id)
    if (!is.null(msg)) shiny::div(class = "small text-danger mb-2", msg)
  })
  # the sample suggests its own ID; another choice leaves the field to you
  shiny::observeEvent(input$ns_from, {
    id <- trimws(input$ns_id %||% "")
    if (identical(input$ns_from, "sample") && !nzchar(id)) {
      shiny::updateTextInput(session, "ns_id", value = next_sample_id())
    } else if (!identical(input$ns_from, "sample") && grepl("^SAMPLE-[0-9]+$", id)) {
      shiny::updateTextInput(session, "ns_id", value = "")
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$ns_ok, {
    if (!is.null(ns_id_problem(input$ns_id))) return()
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
      # "empty": no reports, but the company's study defaults, analysis
      # sets and data catalog (create_study() with no planner)
      p <- switch(input$ns_from,
        study = guarded(open_study(input$ns_src)$planner),
        NULL)
      if (identical(input$ns_from, "study") && is.null(p)) return()
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
    if (identical(input$ns_from, "sample")) rv$next_steps <- TRUE
    notify(sprintf(t("Created %s"), s$meta$study_id))
    bslib::nav_select("nav", "outputs")
  })
  # -- the next steps, once: after the sample is made, where to look ------
  rv$next_steps <- FALSE
  steps_flag <- function() file.path(tflplanner_home(), "next_steps_seen")
  output$next_steps <- shiny::renderUI({
    if (!isTRUE(rv$next_steps) || file.exists(steps_flag())) return(NULL)
    ids <- output_ids(rv$p)
    ft <- c(intersect("T-14-1-1", ids), ids)[1L]
    step <- function(text, id, label) shiny::tags$li(
      class = "mb-1", text, " ",
      .btn(id, label, class = "btn-sm btn-outline-primary py-0"))
    shiny::div(
      class = "border rounded p-3 mb-3 bg-light",
      shiny::div(
        class = "d-flex justify-content-between align-items-start",
        shiny::h5(t("Next steps")),
        .btn("steps_close", t("Close"), class = "btn-sm btn-link")),
      shiny::tags$ol(
        class = "mb-0",
        if (!is.na(ft)) step(sprintf(t("Open %s: its Content builds the table in words, with the table as it will print beside it."), ft),
                             "steps_table", t("Open it")),
        step(t("ARD: where the numbers come from (cards / cardx). A click on a row shows the analysis as a form."),
             "steps_ard", t("Go to ARD")),
        step(t("Runs: preview every report and open its RTF."),
             "steps_runs", t("Go to Runs"))))
  })
  shiny::observeEvent(input$steps_close, {
    rv$next_steps <- FALSE
    try(writeLines(format(Sys.time()), steps_flag()), silent = TRUE)
  })
  shiny::observeEvent(input$steps_table, {
    ids <- output_ids(rv$p)
    ft <- c(intersect("T-14-1-1", ids), ids)[1L]
    shiny::updateSelectInput(session, "target", selected = ft)
    go("tables")
  })
  shiny::observeEvent(input$steps_ard, go("ard"))
  shiny::observeEvent(input$steps_runs, go("results"))

  shiny::observeEvent(input$std_defaults, {
    if (!has_study()) return()
    p2 <- guarded(add_standard_defaults(rv$p, rv$study$meta$study_id))
    if (is.null(p2)) return()
    added <- attr(p2, "added")
    if (!length(added)) {
      return(notify(t("The study has every company default already.")))
    }
    attr(p2, "added") <- NULL
    rv$p <- p2
    bump()
    notify(sprintf(t("Added the company's defaults: %s (save to keep them)"),
                   paste(added, collapse = ", ")))
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
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center mb-2",
        .btn("std_defaults", t("Add the company's study defaults (only what is missing)"),
             class = "btn-sm btn-outline-primary"),
        shiny::span(class = "small text-muted",
                    t("For a study made without them: the table look (stub, blank rows, column headers, widths), headers and footers, analysis sets and data catalog. Nothing already there is changed."))),
      shiny::tags$details(
        shiny::tags$summary(t("Folders")),
        shiny::tags$pre(class = "small", paste(
          sprintf("%-15s %s", paste0(lay, "/"),
                  .study_folder_notes(names(lay), t)),
          collapse = "\n"))),
      shiny::fileInput(
        "import",
        t("Import definition workbooks (replaces this study's definition)"),
        multiple = TRUE, accept = ".xlsx", width = "100%"),
      shiny::div(class = "d-flex flex-wrap gap-2 align-items-center",
                 shiny::downloadButton("spec_xlsx",
                                       t("Export the definition (Excel)"),
                                       class = "btn-sm"),
                 shiny::downloadButton("ars_zip",
                                       t("Export the analyses as CDISC ARS"),
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
  output$ars_zip <- shiny::downloadHandler(
    filename = function() paste0(rv$study$meta$study_id, "_ars.zip"),
    content = function(file) {
      d <- tempfile("ars")
      export_ars(current_study(), d)
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
      stats::setNames(list(
        stats::setNames(ids, ids),
        stats::setNames(c(.default_rows, .all_rows),
                        c(t("Study defaults"), t("ALL (every row)")))),
        c(t("Reports"), t("Rows for every report (ARD, Page)")))
    vals <- c(ids, .default_rows, .all_rows)
    want <- shiny::isolate(rv$want)
    rv$want <- NULL
    sel <- if (length(want) && !is.na(want) && want %in% vals) want else
      if (!is.null(cur) && cur %in% vals) cur else
        if (length(ids)) ids[1L] else .default_rows
    shiny::updateSelectInput(session, "target", choices = ch, selected = sel)
  })
  # the sidebar is for an open study
  shiny::observe(bslib::toggle_sidebar("side", open = has_study()))
  target <- shiny::reactive(.target_value(input$target))
  current_now <- shiny::reactive({
    tg <- target()
    if (!is.null(rv$p) && !is.na(tg) && nzchar(tg) &&
        tg %in% rv$p$outputs$output_id) tg else NULL
  })
  # The report chosen, passed on only when it changes.  It reads rv$p (is
  # the report still there?), which every edit writes: as a reactive, each
  # edit invalidated everything drawn for the report -- the table builder's
  # form among them, redrawn (closed, the text lost) while being typed in.
  current <- shiny::reactiveVal(NULL)
  shiny::observe(current(current_now()), priority = 100)

  # -- where the user is ---------------------------------------------------
  # A report's screens are the Reports tab's sub-tabs (the list, its
  # content by its kind, its page, its code); the logic knows them by the
  # pages they were: tables / lf / designer (content), report_spec (page).
  report_kind <- shiny::reactive({
    id <- current()
    if (is.null(id)) "" else report_info(rv$p, id)$type
  })
  output$report_kind <- shiny::renderText(report_kind())
  shiny::outputOptions(output, "report_kind", suspendWhenHidden = FALSE)
  page <- shiny::reactive({
    nav <- input$nav %||% ""
    if (!identical(nav, "outputs")) return(nav)
    switch(input$rep_nav %||% "list",
      content = switch(report_kind(), table = "tables", listing = "lf",
                       figure = "designer", "outputs"),
      page = "report_spec", code = "code", "outputs")
  })
  go <- function(where) {
    sub <- switch(where, tables = , lf = , designer = "content",
                  report_spec = "page", code = "code", outputs = "list", NULL)
    if (is.null(sub)) return(bslib::nav_select("nav", where))
    bslib::nav_select("nav", "outputs")
    bslib::nav_select("rep_nav", sub)
  }
  # The report chosen in the list: where its kind is made, as buttons
  output$report_moves <- shiny::renderUI({
    id <- current()
    moves <- .type_moves[[report_kind()]]
    if (is.null(id) || !length(moves)) return(NULL)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center my-2",
      shiny::span(class = "small text-muted",
                  sprintf(t("%s (%s):"), id, t(.type_labels[[report_kind()]]))),
      lapply(names(moves), function(to) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary",
        onclick = sprintf("Shiny.setInputValue('report_move', '%s', {priority: 'event'})", to),
        t(moves[[to]]))))
  })
  shiny::observeEvent(input$report_move, go(input$report_move))
  # the tabs this kind of report has nothing on, faded (still clickable)
  shiny::observe({
    idle <- .type_idle_tabs[[report_kind()]] %||% character()
    session$sendCustomMessage("rp-idle-tabs", as.list(idle))
  })
  output$ard_kind_note <- shiny::renderUI({
    k <- report_kind()
    if (!k %in% names(.type_idle_tabs) || !"ard" %in% .type_idle_tabs[[k]]) {
      return(NULL)
    }
    shiny::div(class = "alert alert-info py-1 small",
               sprintf(t("%s is a %s: it has no ARD (its program reads its data). The ARD here is the tables'."),
                       current(), t(.type_labels[[k]])))
  })
  output$report_head <- shiny::renderUI({
    id <- current()
    if (is.null(id)) return(NULL)
    o <- rv$p$outputs
    d <- o$description[match(id, o$output_id)]
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-baseline my-2",
      shiny::strong(id),
      shiny::span(class = "badge text-bg-light border",
                  t(unname(.type_labels[report_kind()]))),
      if (!is.na(d)) shiny::span(class = "text-muted small", d))
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
    st <- report_ard_state(id)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      shiny::strong("ARD"),
      shiny::span(class = paste("small", if (st$need) "text-muted"), st$text),
      if (st$need) .btn("fetch2", t("Preview this table's ARD"),
                        class = "btn-sm btn-outline-primary"))
  })
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
      error = t("Error: its program failed (ARD tab > Study ARD)"),
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
    bslib::nav_select("table_sheet", sheet)
  }
  need_meta <- function() {
    m <- meta_of(current())
    if (is.null(m)) {
      notify(t("Preview this table's ARD first."), "warning")
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
  # Preview: the one action that makes a report's ARD and reads it, the
  # same from the ARD, Tables and Reports tabs.  It saves; runs the report's
  # ARD program into the study's working ARD (when the ARD definition has
  # analyses for it); then runs its data code (1 and 2) and reads what the
  # builder and the fills use; and shows the report's ARD on the ARD tab.
  do_preview <- function(id = current()) {
    if (is.null(id)) return(notify(t("Choose a report"), "warning"))
    has_an <- any(rv$p$ard$analyses$output_id %in% id)
    is_report <- id %in% rv$p$outputs$output_id
    if (!has_an && !is_report) {
      return(notify(sprintf(t("%s has no analyses in the ARD definition."), id),
                    "warning"))
    }
    t0 <- Sys.time()
    ok <- TRUE
    m <- NULL
    n_an <- sum(rv$p$ard$analyses$output_id %in% id)
    shiny::withProgress(message = sprintf(t("Preview of %s"), id), {
      if (has_an) {
        shiny::setProgress(0.05, detail = t("1/3 saving the study"))
        ok <- ard_ready()
      }
      if (ok && has_an) {
        shiny::setProgress(0.15, detail = sprintf(
          t("2/3 making its ARD (%d analyses, in its own R process)"), n_an))
        r <- guarded(update_study_ard(current_study(), id))
        ard_state_ver(ard_state_ver() + 1L)
        rv$status_ver <- rv$status_ver + 1L
        if (is.null(r)) {
          ok <- FALSE
        } else {
          ard_preview_out(list(id = id, text = r$output))
          if (!r$ok) {
            ok <- FALSE
            ard_res(list(scope = id, error = r$error))
            notify(t("The ARD program failed: see what it printed on the ARD tab (Study ARD)."),
                   "error")
          }
        }
      }
      if (ok && is_report) {
        shiny::setProgress(0.6, detail = t("3/3 reading it for the builder"))
        m <- guarded(fetch_ard(current_study(), id))
        if (is.null(m)) ok <- FALSE
      }
    })
    rv$ard_ver <- rv$ard_ver + 1L
    if (!ok) return(invisible(FALSE))
    if (has_an) {
      ard_res(list(ard = study_ard_rows(current_study(), id), scope = id,
                   seconds = as.numeric(difftime(Sys.time(), t0,
                                                 units = "secs"))))
    }
    if (!is_report) {
      notify(sprintf(t("%s is in the study ARD; add it on the Reports tab to make its table."),
                     id), "warning")
    } else if (!is.null(m$error)) {
      notify(paste(t("The ARD was read, but normalize and rework failed:"),
                   m$error), "warning")
    } else {
      notify(sprintf(t("Preview of %s: %d keys, %d variables"), id,
                     length(m$keys), nrow(m$variables)))
    }
    invisible(TRUE)
  }
  shiny::observeEvent(input$fetch, do_preview())
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
        by = cols, strata = cols, variables = cols,
        denominator = c("population", "row", "column", "cell",
                        p$ard$populations$population_id,
                        p$ard$datasets$dataset)),
      datasets = list(path = if (has_study()) {
        f <- study_files(rv$study, "data")
        file.path(f$folder, f$file)
      }),
      populations = list(dataset = p$ard$datasets$dataset),
      study = list(key = c("id", "output", "source")),
      list())
  }
  for (sheet in names(.ard_sheets())) local({
    sh <- sheet
    out_id <- paste0("hot_ard_", sh)
    by_report <- sh == "analyses"
    # the analyses of the report chosen, or of every report (an analysis
    # several reports use, BIGN, seen at once): then the grid is the whole
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
  # A form field's label: what it is, then its name in the spec and the
  # value it takes when left blank -- "Percentages of (denominator =
  # population)".
  argl <- function(text, arg, dflt = NULL) {
    d <- if (length(dflt) == 1L && !is.na(dflt) && nzchar(dflt))
      paste0(" = ", dflt) else ""
    paste0(t(text), " (", arg, d, ")")
  }
  # the value a method's own defaults give an argument ("population" for a
  # hierarchical analysis's denominator), or NULL
  st_arg_default <- function(r, arg) {
    keys <- .std_ard_methods()
    d <- keys$defaults[match(r$method, keys$method)]
    if (is.na(d) || !nzchar(d)) return(NULL)
    p <- trimws(strsplit(d, ",")[[1L]])
    hit <- p[startsWith(p, paste0(arg, " ")) | startsWith(p, paste0(arg, "="))]
    if (!length(hit)) return(NULL)
    trimws(sub("^[^=]*=", "", hit[1L]))
  }
  # the data an analysis reads, as one choice: a dataset and an analysis
  # set ("<dataset>|<population_id>"), named as the program names it
  data_choices <- function(r) {
    .an_data_choices(rv$p$ard$datasets$dataset, rv$p$ard$populations,
                     now = .an_data_value(r$dataset, r$population_id),
                     words = list(with = t("%s \u00d7 %s (%s)"),
                                  alone = t("%s, no analysis set (%s)"),
                                  none = t("(no data)")))
  }
  # the dataset and the analysis set the form's data choice stands for
  form_data <- function(r) {
    v <- input[[st_id("data")]]
    if (is.null(v)) return(list(dataset = r$dataset, pop = r$population_id))
    .an_data_split(v)
  }
  # the method chosen in the form (the function list on screen may show
  # another category, or a search, without the chosen one)
  st_method <- shiny::reactiveVal(NA_character_)
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
  # the functions an analysis can name, the chosen one kept
  fn_entries <- function(current) {
    e <- .ard_fn_entries(.std_ard_methods(), tflspec::tfl_ard_functions(),
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
    e
  }
  # a dataset's data (for the choices), read once per file version
  an_data <- function(ds) {
    d <- rv$p$ard$datasets
    pth <- d$path[match(ds, d$dataset)]
    if (!length(pth) || is.na(pth)) return(NULL)
    f <- file.path(rv$study$path, pth)
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
            stats::setNames("population", t("the analysis set")),
            stats::setNames("row", t("within a row")),
            stats::setNames("column", t("within a column")),
            stats::setNames("cell", t("of the whole table")))
    more <- c(p$populations$population_id, p$datasets$dataset)
    more <- unique(c(more[!is.na(more)], setdiff(now, c(ch, ""))))
    c(ch, stats::setNames(more, more))
  }
  output$ard_stat_ui <- shiny::renderUI({
    a <- st_rows()
    if (is.null(a) || !nrow(a)) {
      return(shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center",
        shiny::span(class = "small text-muted",
                    t("Choose a report in the sidebar and add a row to its analyses: a click on a row shows it here as a form.")),
        if (!is.null(ard_target()))
          .btn("ard_an_new", t("New analysis"), class = "btn-sm btn-outline-primary")))
    }
    r <- st_row()
    rv$ver
    st_env$n <- st_env$n + 1L
    st_drawn(st_env$n)
    st_method(r$method)
    e <- fn_entries(r$method)
    cats <- unique(e$category)
    cat_now <- e$category[match(r$method, e$value)]
    if (is.na(cat_now)) cat_now <- cats[1L]
    blank_na <- function(x) if (is.na(x)) "" else x
    tg <- ard_target()
    shiny::tagList(
      if (!tg %in% rv$p$outputs$output_id) shiny::div(
        class = "alert alert-info py-1 small",
        sprintf(t("%s is not a report yet: add it on the Reports tab to make its table."), tg)),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center mb-1",
        shiny::span(class = "small text-muted",
                    sprintf(t("Analysis %s of %s. A click on another row of the grid edits that one."),
                            r$analysis_id, tg)),
        .btn("ard_an_new", t("New analysis"), class = "btn-sm btn-outline-primary")),
      shiny::p(class = "small text-muted mb-1",
               t("One analysis is one call: add variables to it. Make another analysis only when the statistics, the condition or the groups differ.")),
      bslib::layout_columns(
        col_widths = c(4, 8),
        shiny::textInput(st_id("id"), t("Analysis ID"), r$analysis_id),
        shiny::textInput(st_id("label"), t("Label"), blank_na(r$label),
                         width = "100%")),
      shiny::div(
        class = "ard-fn mb-2",
        shiny::div(
          class = "d-flex justify-content-between align-items-center gap-2",
          shiny::div(shiny::strong(argl("What to compute", "method")),
                     shiny::uiOutput("ard_fn_now", inline = TRUE)),
          shiny::div(style = "width: 14rem",
                     shiny::textInput(st_id("fn_q"), NULL, "",
                                      placeholder = t("Search the functions")))),
        bslib::layout_columns(
          col_widths = c(4, 8),
          shiny::div(class = "ard-fn-cats small",
                     shiny::radioButtons(st_id("fn_cat"), NULL,
                                         stats::setNames(cats, t(cats)),
                                         selected = cat_now)),
          shiny::div(class = "ard-fn-list",
                     style = paste("max-height: 30rem; overflow-y: auto;",
                                   "overflow-x: hidden; white-space: normal;",
                                   "overflow-wrap: anywhere;"),
                     shiny::uiOutput("ard_fn_list")))),
      shiny::uiOutput("ard_method_note"),
      shiny::selectInput(st_id("data"), t("Data (dataset \u00d7 analysis set)"),
                         data_choices(r),
                         selected = .an_data_value(r$dataset, r$population_id),
                         width = "100%"),
      shiny::p(class = "small text-muted mt-n2 mb-2",
               t("The rows the analysis reads: the dataset's records of the analysis set's subjects. The name is the one the program gives the data.")),
      shiny::uiOutput("ard_an_vars"),
      shiny::uiOutput("ard_an_args"),
      shiny::tags$details(
        class = "mb-2", open = if (!is.na(r$where)) NA,
        shiny::tags$summary(class = "small", argl("Subset (an R condition)", "where")),
        shiny::textInput(st_id("where"), NULL, blank_na(r$where), width = "100%",
                         placeholder = "AESER == \"Y\"")),
      shiny::uiOutput("ard_stat_part"),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center",
        .btn("ard_stat_apply", t("Apply to the analysis"),
             class = "btn-sm btn-primary"),
        shiny::span(class = "small text-muted",
                    t("Format: xx.x = 1 decimal, xx.x% = a proportion as a percent, 2 = 2 decimals, pvalue = <0.001 or 3 decimals. Blank = the default shown."))),
      shiny::tags$details(
        class = "mt-2", open = NA,
        shiny::tags$summary(class = "small", t("This analysis as code (after Apply)")),
        shiny::div(class = "rp-code", shiny::verbatimTextOutput("ard_an_code"))))
  })
  # this analysis alone as code (the data it reads, its analysis set, the
  # call), as the definition has it now -- the whole program is on the right
  output$ard_an_code <- shiny::renderText({
    r <- st_row()
    a <- rv$p$ard
    a$analyses <- a$analyses[!is.na(a$analyses$output_id) &
                               a$analyses$output_id == r$output_id &
                               a$analyses$analysis_id %in% r$analysis_id, ,
                             drop = FALSE]
    code <- tryCatch(tflspec::tfl_ard_code(structure(a, class = "tfl_ard_spec"),
                                           part = "body"),
                     error = function(e) paste(t("The code cannot be written yet:"),
                                               conditionMessage(e)))
    paste(code, collapse = "\n")
  })
  output$ard_fn_now <- shiny::renderUI({
    st_drawn()
    m <- st_method()
    if (is.na(m) || !nzchar(m)) return(NULL)
    e <- fn_entries(m)
    k <- match(m, e$value)
    fn <- if (grepl("::", m, fixed = TRUE)) paste0(" (", sub("^.*::", "", e$call[k]), ")") else ""
    shiny::span(class = "small text-muted ms-2",
                sprintf(t("Chosen: %s"), paste0(e$label[k], fn)))
  })
  output$ard_fn_list <- shiny::renderUI({
    st_drawn()
    now <- shiny::isolate(st_method())
    e <- fn_entries(now)
    q <- trimws(input[[st_id("fn_q")]] %||% "")
    e <- if (nzchar(q)) {
      hit <- grepl(q, e$label, ignore.case = TRUE, fixed = TRUE) |
        grepl(q, e$label_en, ignore.case = TRUE, fixed = TRUE) |
        grepl(q, e$value, ignore.case = TRUE, fixed = TRUE) |
        grepl(q, gsub("_", " ", e$value, fixed = TRUE), ignore.case = TRUE,
              fixed = TRUE) |
        grepl(q, e$description %||% "", ignore.case = TRUE, fixed = TRUE)
      e[hit, , drop = FALSE]
    } else {
      e[e$category == (input[[st_id("fn_cat")]] %||% e$category[1L]), , drop = FALSE]
    }
    if (!nrow(e)) return(shiny::p(class = "small text-muted", t("No function matches.")))
    ok <- e[e$state %in% c("ok", "old", "out"), , drop = FALSE]
    off <- e[!e$state %in% c("ok", "old", "out"), , drop = FALSE]
    fn_of <- function(v) if (grepl("::", v, fixed = TRUE))
      paste0(" (", sub("^.*::", "", v), ")") else ""
    item <- function(i, d) shiny::tagList(
      shiny::span(paste0(d$label[i], fn_of(d$call[i]))),
      if (!is.na(d$description[i]) && nzchar(d$description[i]))
        shiny::div(class = "small text-muted", d$description[i]),
      if (identical(d$state[i], "old"))
        shiny::div(class = "small text-warning", t("An old name: choose its new one.")),
      if (identical(d$state[i], "out"))
        shiny::div(class = "small text-warning", t("Not offered by the builder: kept as written.")))
    shiny::tagList(
      if (nrow(ok)) shiny::radioButtons(
        st_id("fn_pick"), NULL, width = "100%",
        choiceNames = lapply(seq_len(nrow(ok)), item, d = ok),
        choiceValues = ok$value,
        selected = if (now %in% ok$value) now else character(0)),
      lapply(seq_len(nrow(off)), function(i) shiny::div(
        class = "small text-body-tertiary ms-4 mb-1",
        paste0(off$label[i], fn_of(off$call[i])), " -- ",
        if (off$state[i] == "missing") t("its package is not installed") else
          t("in preparation: a function that runs others"))))
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
    pa <- .ard_args_parse(if (identical(r$method, row$method)) row$args else NA, f)
    other <- pa$other
    if (is.null(f) || !nrow(f)) {
      return(shiny::tags$details(
        class = "mb-2", open = if (nzchar(other)) NA,
        shiny::tags$summary(class = "small", argl("Other arguments (R)", "args")),
        shiny::textInput(st_id("args_other"), NULL, other, width = "100%")))
    }
    fd <- form_data(r)
    ds <- an_dataset(fd$dataset, fd$pop)
    d <- if (!is.na(ds %||% NA)) an_data(ds)
    cols <- if (!is.null(d)) names(d) else character()
    vars <- .split_bar(row$variables)
    field <- function(i) {
      a <- f$arg[i]
      id <- st_id(paste0("a_", a))
      v <- pa$values[[a]]
      dflt <- f$default[i]
      lab <- paste0(a, if (isTRUE(f$required[i])) " *" else "")
      ph <- if (!is.na(dflt) && nchar(dflt) <= 40) dflt else ""
      ch <- .split_bar(f$choices[i])
      dflt_lab <- .ard_default_label(f$kind[i], dflt, ch,
                                     t("(default)"), t("(default: %s)"))
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
                     unique(c(v, .level_choices(vars, sheet_rows(rv$p, "codelists", NA), d)))),
          selected = v %||% "", multiple = FALSE, width = "100%",
          options = list(create = TRUE)),
        shiny::textInput(id, lab, v %||% "", width = "100%", placeholder = ph))
      shiny::div(w, shiny::div(
        class = "small text-muted mt-n2 mb-2",
        if (!is.na(f$hint[i]) && nzchar(f$hint[i])) f$hint[i] else
          sprintf(t("See ?%s for this argument."), call)))
    }
    shiny::div(
      class = "mb-2",
      shiny::h6(class = "small fw-bold", sprintf(t("Arguments of %s"), sub("^.*::", "", call))),
      do.call(bslib::layout_columns,
              c(list(col_widths = c(6, 6)), lapply(seq_len(nrow(f)), field))),
      shiny::tags$details(
        class = "mb-2", open = if (nzchar(other)) NA,
        shiny::tags$summary(class = "small", argl("Other arguments (R)", "args")),
        shiny::textInput(st_id("args_other"), NULL, other, width = "100%",
                         placeholder = "weights = W")))
  })
  # the groups and the variables, from the data the analysis reads: groups
  # first those that look like treatments; variables of the method's kind
  output$ard_an_vars <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    fd <- form_data(r)
    ds <- an_dataset(fd$dataset, fd$pop)
    d <- if (!is.na(ds %||% NA)) an_data(ds)
    by_now <- shiny::isolate(input[[st_id("by")]]) %||% .split_bar(r$by)
    var_now <- shiny::isolate(input[[st_id("vars")]]) %||% .split_bar(r$variables)
    strata_now <- shiny::isolate(input[[st_id("strata")]]) %||%
      .split_bar(r$strata %||% NA)
    den_now <- shiny::isolate(input[[st_id("den")]]) %||%
      (if (is.na(r$denominator %||% NA)) "" else r$denominator)
    kind <- st_kind(r)
    if (is.null(d)) {
      bch <- by_now
      vch <- var_now
      sch <- strata_now
    } else {
      cols <- as.list(d)
      rows <- .row_choices(cols, c(continuous = t("numbers"),
                                   categorical = t("counts")))
      kinds <- vapply(unname(rows), function(n) .column_kind(cols[[n]]), "")
      vch <- switch(kind,
        continuous = rows[kinds == "continuous"],
        categorical = rows[kinds == "categorical"],
        rows)
      grp <- .group_choices(cols)
      bch <- c(grp, rows[kinds == "categorical" & !rows %in% grp])
      # what the row has stays offered, even if the data do not say so
      bch <- c(bch, stats::setNames(setdiff(by_now, bch), setdiff(by_now, bch)))
      sch <- c(rows[kinds == "categorical"],
               stats::setNames(setdiff(strata_now, rows), setdiff(strata_now, rows)))
      vch <- c(vch, stats::setNames(setdiff(var_now, vch), setdiff(var_now, vch)))
    }
    shiny::tagList(
      shiny::selectizeInput(
        st_id("by"), argl("Groups (the columns)", "by"), bch, by_now, multiple = TRUE,
        width = "100%", options = list(plugins = list("remove_button"))),
      shiny::selectizeInput(
        st_id("vars"), argl("Variables (the rows)", "variables"), vch, var_now,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"))),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::div(class = "flex-grow-1", shiny::selectizeInput(
          st_id("strata"), argl("Repeated within", "strata"), sch, strata_now,
          multiple = TRUE, width = "100%",
          options = list(plugins = list("remove_button"),
                         placeholder = t("none")))),
        shiny::div(class = "flex-grow-1", shiny::selectInput(
          st_id("den"), argl("Percentages of", "denominator", st_arg_default(r, "denominator")),
          den_choices(den_now), den_now, width = "100%"))),
      if (is.null(d)) shiny::p(
        class = "small text-muted",
        t("The data of this analysis cannot be read (no file in the data catalog): the choices are the row's own.")))
  })
  output$ard_stat_part <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    r0 <- shiny::isolate(st_row())
    kind <- st_kind(r)
    kinds <- .stat_kinds(kind)
    if (identical(.std_ard_methods()$call[match(r$method, .std_ard_methods()$method)],
                  "(subjects)")) kinds <- "categorical"
    cat <- .std_ard_statistics(kinds)
    cat <- cat[!duplicated(cat$statistic), , drop = FALSE]
    have <- if (identical(r$method, r0$method)) .split_bar(r0$statistics) else
      character()
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
    dflt <- .method_default_stats(kinds)
    lab <- function(x) {
      l <- cat$label[match(x, cat$statistic)]
      ifelse(is.na(l), x, paste0(x, " \u2014 ", l))
    }
    shiny::tagList(
      shiny::selectizeInput(
        st_id("pick"), argl("Statistics", "statistics"), choices = ch, selected = have,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("remove_button"),
                       placeholder = t("the method's default (below)"))),
      if (!length(have)) shiny::div(
        class = "small mt-n2 mb-1",
        if (length(dflt)) shiny::tagList(
          shiny::span(class = "text-muted",
                      sprintf(t("Blank = the method's default (%d):"), length(dflt))),
          lapply(dflt, function(x) shiny::span(
            class = "badge text-bg-light border fw-normal ms-1", lab(x))),
          .btn(st_id("use_default"), t("Start from these"),
               class = "btn-sm btn-link py-0"))
        else shiny::span(class = "text-muted",
                         t("Blank = every result the method gives."))),
      shiny::p(class = "small text-muted mb-1", note),
      shiny::uiOutput("ard_stat_fmts"))
  })
  # the defaults as a start: they become the analysis's own, to change
  shiny::observe({
    st_drawn()
    n <- input[[st_id("use_default")]]
    if (is.null(n) || n < 1L || identical(n, st_env[["used"]])) return()
    st_env[["used"]] <- n
    r <- shiny::isolate(st_row_now())
    shiny::updateSelectizeInput(session, st_id("pick"),
                                selected = .method_default_stats(.stat_kinds(st_kind(r))))
  })
  output$ard_stat_fmts <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    pick <- input[[st_id("pick")]]
    if (!length(pick)) return(NULL)
    f <- .parse_formats(shiny::isolate(st_row())$formats)
    shiny::tagList(
      shiny::h6(class = "mt-2 mb-1", argl("Formats", "formats")),
      shiny::div(
      class = "rp-stat-fmt",
      lapply(seq_along(pick), function(i) {
        s <- pick[i]
        shiny::textInput(st_id(paste0("f_", s)), s,
                         value = if (!is.na(f[s])) f[[s]] else "",
                         placeholder = st_default(r, s))
      })))
  })
  shiny::observeEvent(input$ard_stat_apply, {
    r <- st_row()
    g <- function(x) input[[st_id(x)]]
    one <- function(v) {
      v <- trimws(paste(v %||% character(), collapse = " | "))
      if (nzchar(v)) v else NA_character_
    }
    new_id <- trimws(g("id") %||% r$analysis_id)
    if (!nzchar(new_id)) return(notify(t("Give the analysis an ID."), "warning"))
    a <- rv$p$ard$analyses
    mine <- !is.na(a$output_id) & a$output_id == r$output_id
    if (!identical(new_id, r$analysis_id) &&
        any(mine & !is.na(a$analysis_id) & a$analysis_id == new_id)) {
      return(notify(sprintf(t("%s already has an analysis %s."), r$output_id,
                            new_id), "warning"))
    }
    where <- trimws(g("where") %||% "")
    if (nzchar(where) &&
        inherits(try(parse(text = where), silent = TRUE), "try-error")) {
      return(notify(sprintf(t("The subset is not an R condition: %s"), where),
                    "warning"))
    }
    pick <- g("pick") %||% character()
    fm <- vapply(pick, function(s) trimws(g(paste0("f_", s)) %||% ""), "")
    fm <- fm[nzchar(fm)]
    bad <- names(fm)[!.fmt_ok(fm)]
    if (length(bad)) {
      return(notify(sprintf(t("Not a format: %s"), paste(bad, collapse = ", ")),
                    "warning"))
    }
    # the analysis's variable-specific formats (AGE:mean=...) stay
    old <- .parse_formats(r$formats)
    fm <- c(fm, old[grepl(":", names(old), fixed = TRUE)])
    i <- which(mine & a$analysis_id == r$analysis_id)[1L]
    a$analysis_id[i] <- new_id
    a$label[i] <- one(g("label"))
    a$method[i] <- one(st_method())
    call <- .ard_method_call(a$method[i], .std_ard_methods())
    f <- .ard_form_fields(call)
    # a field not on screen (yet) keeps the row's value
    pa <- .ard_args_parse(if (identical(a$method[i], r$method)) r$args else NA, f)
    vals <- if (!is.null(f)) stats::setNames(lapply(f$arg, function(x)
      g(paste0("a_", x)) %||% pa$values[[x]]), f$arg) else list()
    new_args <- .ard_args_build(vals, f, g("args_other") %||% pa$other)
    if (!.ard_args_same(new_args, r$args)) a$args[i] <- new_args
    fd <- form_data(r)
    a$dataset[i] <- fd$dataset
    a$population_id[i] <- fd$pop
    a$where[i] <- one(where)
    a$by[i] <- one(g("by"))
    if (!is.null(g("strata"))) a$strata[i] <- one(g("strata"))
    if (!is.null(g("den"))) a$denominator[i] <- one(g("den"))
    a$variables[i] <- one(g("vars"))
    a$statistics[i] <- one(pick)
    a$formats[i] <- if (length(fm))
      paste(paste0(names(fm), "=", fm), collapse = " | ") else NA
    rv$p$ard$analyses <- a
    an_pick(new_id)
    bump()
    notify(sprintf(t("%s: written"), new_id))
  })
  # a new analysis for the report: the data, analysis set and groups of the
  # one shown, to be filled in on the form
  shiny::observeEvent(input$ard_an_new, {
    tg <- ard_target()
    if (is.null(tg)) return()
    a <- rv$p$ard$analyses
    have <- a$analysis_id[!is.na(a$output_id) & a$output_id == tg]
    k <- 1L
    while (paste0("A", k) %in% have) k <- k + 1L
    id <- paste0("A", k)
    a0 <- shiny::isolate(st_rows())
    r <- if (!is.null(a0) && nrow(a0)) shiny::isolate(st_row()) else
      list(dataset = NA, population_id = NA, by = NA)
    a[nrow(a) + 1L, ] <- NA
    a$output_id[nrow(a)] <- tg
    a$analysis_id[nrow(a)] <- id
    a$method[nrow(a)] <- "categorical"
    a$dataset[nrow(a)] <- r$dataset
    a$population_id[nrow(a)] <- r$population_id
    a$by[nrow(a)] <- r$by
    rv$p$ard$analyses <- a
    an_pick(id)
    bump()
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
    # analyses of an output the report list does not have: they run, but
    # no table reads them until it is added (Reports > Add)
    ids <- unique(stats::na.omit(rv$p$ard$analyses$output_id))
    orphan <- setdiff(ids, rv$p$outputs$output_id)
    note <- if (length(orphan)) shiny::div(
      class = "alert alert-info py-1 small mt-2",
      sprintf(t("%s: analyses of no report yet. Add the report on the Reports tab to make its table."),
              paste(orphan, collapse = ", ")))
    if (is.null(msg)) {
      n <- nrow(rv$p$ard$analyses)
      return(shiny::tagList(
        shiny::p(class = "small text-success mt-2",
                 sprintf(t("%d analyses; the definition reads without errors."), n)),
        note))
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
      report = ard_program_code(a, id, dir = rv$study$path),
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
  shiny::observeEvent(input$ard_preview, {
    if (is.null(ard_target())) return(notify(t("Choose a report"), "warning"))
    if (isTRUE(do_preview(ard_target()))) bslib::nav_select("ard_right", "result")
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
  output$ard_run_info <- shiny::renderUI({
    r <- ard_res()
    if (is.null(r)) {
      return(shiny::p(class = "small text-muted mt-2",
                      t("Preview to see the ARD this report's analyses make.")))
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
                    c = t(unname(.ard_state_labels[d$state])),
                    d = ifelse(is.na(d$rows), "", d$rows),
                    e = ifelse(is.na(d$built), "", d$built), f = d$error,
                    stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Analyses", "State", "Rows", "Built",
                    "Error"))
    DT::formatStyle(
      .dt(v, selection = "single"), names(v)[3L],
      color = DT::styleEqual(t(unname(.ard_state_labels)),
                             c("#15803d", "#b45309", "#6b7280", "#b91c1c")))
  }
  output$ard_state <- DT::renderDT(ard_state_view())
  # the study's code list from a file: its values become the study defaults
  # of the codelists sheet (a report's own rows still win for that report)
  shiny::observeEvent(input$codelist_file, {
    f <- input$codelist_file
    if (is.null(f) || !has_study()) return()
    ext <- tolower(tools::file_ext(f$name))
    path <- f$datapath
    if (!identical(tolower(tools::file_ext(path)), ext)) {
      file.copy(path, p2 <- paste0(path, ".", ext))
      path <- p2
    }
    rows <- guarded(read_codelist(path))
    if (is.null(rows)) return()
    p2 <- guarded(set_codelist(rv$p, rows))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    notify(sprintf(t("%d values of %d variables read into the code list (the study's defaults)."),
                   nrow(rows), length(unique(rows$variable))))
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
                   catalog, fig_is_new = function() fig_is_new(),
                   page = function() page())
  output$lf_note <- shiny::renderUI({
    msg <- switch(lf_type(),
      none = t("Choose a Listing report in the sidebar."),
      table = t("This is a Table: its definition and builder are on the Tables tab."),
      listing = t("A listing: its data (a dataset of the catalog, a condition, an order) and its columns. Code on the Code tab, if any, changes the data after the condition."),
      figure = t("This is a Figure: it is designed on the Figures tab."))
    shiny::div(class = "alert alert-info py-2 small", msg)
  })
  # a figure without a design: its user code (the plot written by hand) --
  # on the Figures tab, apart from the designer, below it; open when the
  # figure has that code
  fig_is_new <- function() {
    id <- current()
    if (is.null(id)) return(FALSE)
    o <- rv$p$outputs
    code <- o$data_code[match(id, o$output_id)]
    is.na(code) || !nzchar(trimws(code))
  }
  output$lf_fig_box <- shiny::renderUI({
    rv$ver
    shiny::req(identical(lf_type(), "figure"),
               is.null(fig_design(shiny::isolate(rv$p), current())))
    shiny::tags$details(
      class = "mt-3", open = if (!fig_is_new()) NA,
      shiny::tags$summary(t("User code (write the ggplot yourself)")),
      fig_hand_box())
  })
  fig_hand_box <- function() {
    id <- current()
    p <- shiny::isolate(rv$p)
    shiny::req(is.null(fig_design(p, id)))
    lf_env$n <- lf_env$n + 1L
    lf_drawn(lf_env$n)
    lf_env$id <- id
    cat_ds <- shiny::isolate(catalog())
    ds_choices <- stats::setNames(cat_ds$dataset,
                                  paste0(cat_ds$dataset, " (", cat_ds$level, ")"))
    f <- lf_rows(p, "figures", id)
    have <- if (nrow(f)) .split_bar(f$datasets[1L]) else character()
    bslib::card(
      bslib::card_header(t("The plot written by hand (ggplot2)")),
      shiny::p(class = "small text-muted",
               t("This figure's plot is its data code (Reports tab), written with ggplot2: it leaves `plot`. Its program reads these datasets first.")),
      shiny::h6(t("Data the figure reads")),
      shiny::selectizeInput(lf_id("fig_ds"), NULL, ds_choices,
                            selected = have, multiple = TRUE,
                            width = "100%"),
      shiny::h6(t("The code that makes the plot (edit it on the Reports tab)")),
      shiny::div(class = "rp-code", shiny::verbatimTextOutput("lf_fig_code")))
  }
  output$lf_form <- shiny::renderUI({
    rv$ver
    type <- lf_type()
    shiny::req(identical(type, "listing"))
    id <- current()
    p <- shiny::isolate(rv$p)
    lf_env$n <- lf_env$n + 1L
    lf_drawn(lf_env$n)
    lf_env$id <- id
    cat_ds <- shiny::isolate(catalog())
    ds_choices <- stats::setNames(cat_ds$dataset,
                                  paste0(cat_ds$dataset, " (", cat_ds$level, ")"))
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
                       placeholder = paste(t("e.g."), "AESEV == \"SEVERE\"")),
      shiny::textInput(lf_id("sort"), t("Order (| between variables, - for descending)"),
                       value = lv("sort"), width = "100%",
                       placeholder = paste(t("e.g."), "TRTA | USUBJID | ASTDT")))
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
    grids_drawn()
    .grid(d, "listing_cols", lf_cols_key(),
          list(vars = cols, collapse_repeats = .bool))
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

  # -- the table builder -------------------------------------------------
  # The form is drawn once per report (and whenever the builder tab is
  # opened), with input ids fresh each time: an input of the form before
  # can never be read as one of this form's.  Its edits write the sheets
  # (builder_write()), and the grids redraw when their tab is opened.
  bform <- new.env()
  bform$n <- 0L
  session$userData$bform <- bform
  bform_drawn <- shiny::reactiveVal(0L)
  rv$bver <- 0L
  rv$btouched <- FALSE
  # the page shown: a Tables sub-tab counts as its own page
  active_page_now <- shiny::reactive({
    nav <- page()
    if (identical(nav, "tables")) input$table_nav %||% "builder" else nav
  })
  # Passed on only when the page changes: page() reads the report's kind
  # from rv$p, which every edit writes -- as a reactive, each edit counted
  # as opening the builder again and redrew its form mid-typing.
  active_page <- shiny::reactiveVal("")
  shiny::observe(active_page(active_page_now()), priority = 100)
  shiny::observeEvent(active_page(), {
    if (identical(active_page(), "builder")) rv$bver <- rv$bver + 1L
    if (active_page() %in% c("table_spec", "report_spec") && rv$btouched) {
      rv$btouched <- FALSE
      bump()
    }
  })
  builder_case_now <- shiny::reactive({
    id <- current()
    if (is.null(id)) return("none")
    if (!identical(report_info(rv$p, id)$type, "table")) return("type")
    m <- meta_of(id)
    if (is.null(m)) return("meta")
    if (length(m$hierarchy)) return("hierarchy")
    "ok"
  })
  # Passed on only when it changes: the case reads rv$p, which every edit of
  # the form writes, and the form is drawn from the case -- a reactive would
  # redraw the form (closing it, losing the text being typed) at each edit.
  builder_case <- shiny::reactiveVal("none")
  shiny::observe(builder_case(builder_case_now()), priority = 100)
  # A table whose rows are in the study ARD (made by an official run, the
  # sample's for one) but not read for the builder yet: read them when its
  # builder is opened -- no new ARD, only the reading (once a report).
  auto_read <- new.env()
  shiny::observe({
    shiny::req(identical(active_page(), "builder"),
               identical(builder_case(), "meta"))
    id <- current()
    if (isTRUE(auto_read[[id]])) return()
    st <- tryCatch(ard_status(shiny::isolate(current_study())), error = function(e) NULL)
    if (is.null(st) || !identical(st$state[match(id, st$output_id)], "built")) return()
    auto_read[[id]] <- TRUE
    m <- NULL
    shiny::withProgress(message = sprintf(t("Reading the ARD of %s for the builder"), id),
                        m <- guarded(fetch_ard(shiny::isolate(current_study()), id)))
    if (!is.null(m)) rv$ard_ver <- shiny::isolate(rv$ard_ver) + 1L
  })
  output$builder_note <- shiny::renderUI({
    msg <- switch(builder_case(),
      none = t("Choose a report in the sidebar."),
      type = t("The builder is for Tables; Listings and Figures are made in their data code."),
      meta = shiny::tagList(
        t("This table has no ARD yet: the builder is built from it."), " ",
        .btn("fetch3", t("Preview this table's ARD"),
             class = "btn-sm btn-primary ms-1")),
      hierarchy = t("This table has a hierarchy (SOC / PT): the builder handles summary tables for now. Edit it in Details (sheets); the preview works."),
      NULL)
    if (is.null(msg)) return(shiny::p(class = "small text-muted",
      t("Drag to order, type to rename; every change is written to the sheets (Details) and shown on the right.")))
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
    # the column header as lines of the form (NULL: the form cannot show it)
    p0 <- shiny::isolate(rv$p)
    own <- sheet_rows(p0, "col_header", id)
    own$output_id <- NULL
    eff <- if (nrow(own)) own else {
      inh <- inherited_rows(p0, "col_header", id)
      inh$output_id <- NULL
      inh
    }
    bform$hdr <- header_read(eff, st$key)
    bform$hdr_n <- {
      tb <- sheet_rows(p0, "tables", id)
      if (nrow(tb) && "header_n" %in% names(tb)) tb$header_n[1L] else NA_character_
    }
    hdr_ver(shiny::isolate(hdr_ver()) + 1L)
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
        # one or more (a group, then a visit ...): outermost first
        shiny::selectizeInput(bid("key"), t("Column variables"), keys,
                              selected = if (!anyNA(st$key)) st$key,
                              multiple = TRUE, width = "100%"),
        shiny::uiOutput(bid("arms_ui")),
        shiny::uiOutput(bid("hdr_ui")),
        if (!anyNA(st$key) &&
            !.has_group_n(shiny::isolate(rv$p), id, st$key[1L]))
          shiny::div(
            class = "small text-warning d-flex flex-wrap gap-2 align-items-center",
            shiny::span(sprintf(t("The ARD has no subjects per %s: a header's (N=n) prints NA."),
                                st$key[1L])),
            .btn("b_add_n", t("Count them (then Preview)"),
                 class = "btn-sm btn-outline-primary py-0"))),
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
        builder_stat_boxes(bs, st$stats, m$stats),
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
  # The continuous statistics as checkboxes: one whose template needs a
  # statistic this report's ARD does not have is greyed and cannot be
  # ticked, and says what to add on the ARD tab.  (One already chosen stays
  # ticked, so the sheets are not changed behind the user's back.)
  builder_stat_boxes <- function(bs, chosen, have) {
    lack <- .builder_stats_lacking(bs$template, have)
    names_ui <- lapply(seq_len(nrow(bs)), function(i) {
      if (!nzchar(lack[i])) return(bs$row[i])
      shiny::span(
        class = "text-muted",
        title = sprintf(t("Not in this report's ARD: add %s to its analysis on the ARD tab."),
                        lack[i]),
        bs$row[i],
        shiny::tags$small(class = "ms-1",
                          sprintf(t("(the ARD has no %s)"), lack[i])))
    })
    off <- bs$key[nzchar(lack) & !bs$key %in% chosen]
    boxes <- shiny::checkboxGroupInput(
      bid("stats"), t("Continuous variables"),
      choiceNames = names_ui, choiceValues = bs$key, selected = chosen)
    boxes <- htmltools::tagQuery(boxes)$find("input")$each(function(x, i) {
      if (x$attribs$value %in% off) x$attribs$disabled <- NA
    })$allTags()
    shiny::tagList(
      boxes,
      if (any(nzchar(lack))) shiny::p(
        class = "small text-muted",
        t("Greyed statistics are not in this report's ARD: add them to its analysis on the ARD tab, then read the ARD again.")))
  }

  # Whatever goes wrong in the builder stays in the builder: it is said once
  # (and logged), and the reactive stops quietly (req) instead of ending the
  # session.  (req's own silent stop passes through.)
  builder_guard <- function(expr) {
    tryCatch(expr, error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      message("tflplanner: builder: ", conditionMessage(e))
      notify(sprintf(t("The builder could not read the form: %s"),
                     conditionMessage(e)), "error")
      shiny::req(FALSE)
    })
  }

  # A table with no definition yet (made without the first-table form, an
  # older study, a workbook read in): what the builder shows is written to
  # the sheets once, so the preview has a table to show.
  shiny::observeEvent(bform_drawn(), {
    id <- bform$id
    tb <- sheet_rows(rv$p, "tables", id)
    inh <- inherited_rows(rv$p, "tables", id)
    has_cols <- (nrow(tb) && !is.na(tb$cols[1L])) ||
      (nrow(inh) && !is.na(inh$cols[1L]))
    if (has_cols || anyNA(bform$st$key)) return()
    p2 <- guarded(builder_write(rv$p, id, bform$st))
    if (!is.null(p2)) {
      rv$p <- p2
      rv$btouched <- TRUE
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$b_add_n, {
    id <- bform$id
    key <- bform$st$key[1L]
    p2 <- guarded(add_group_n(rv$p, id, key))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    do_preview(id)
  })

  # a column variable's levels in order: as the definition has them, else
  # the ARD's
  key_levels <- function(k) {
    bform$st$arms[[k]] %||% bform$meta$keys[[k]] %||% character()
  }
  arms_id <- function(k) bid(paste0("arms_", make.names(k)))
  # the columns follow the column variables chosen: their order (outermost
  # first) when there are several, and each one's levels in order
  shiny::observe(builder_guard({
    bform_drawn()
    ks <- input[[bid("key")]]
    shiny::req(identical(builder_case(), "ok"), length(ks) > 0L)
    n <- bform$n
    output[[paste0("b", n, "_arms_ui")]] <- shiny::renderUI(shiny::tagList(
      if (length(ks) > 1L) shiny::tagList(
        shiny::tags$label(class = "form-label small",
                          t("Order of the column variables, outermost first (drag)")),
        sortable::rank_list(text = NULL, labels = ks, input_id = bid("keyorder"),
                            orientation = "horizontal")),
      lapply(ks, function(k) {
        guess <- k %in% names(bform$st$auto_levels) && k %in% bform$st$key
        shiny::tagList(
          shiny::tags$label(class = "form-label small",
                            sprintf(t("Order of the columns of %s (drag)"), k)),
          if (guess) shiny::div(
            class = "small text-warning",
            t("Check this order: the data do not give one (no factor, no numeric twin such as TRT01AN), so it is the ARD's.")),
          sortable::rank_list(text = NULL, labels = key_levels(k),
                              input_id = arms_id(k),
                              orientation = "horizontal"))
      })))
  }))
  # ---- the column header (Q14): one form a header line, in two parts --
  hdr_ver <- shiny::reactiveVal(0L)
  hid <- function(i, part) bid(sprintf("h%d_%s", i, part))
  # the lines as the inputs have them now
  hdr_now <- function() {
    lines <- bform$hdr
    if (is.null(lines)) return(NULL)
    for (i in seq_along(lines)) {
      l <- lines[[i]]
      st <- l$stub
      for (k in seq_along(st)) {
        v <- input[[hid(i, paste0("stub", k))]]
        if (!is.null(v)) st[[k]] <- v
      }
      l$stub <- st
      for (part in c("mode", "key", "text", "align")) {
        v <- input[[hid(i, part)]]
        if (!is.null(v)) l[[part]] <- v
      }
      b <- input[[hid(i, "bold")]]
      if (!is.null(b)) l$bold <- if (isTRUE(b)) "TRUE" else NA_character_
      u <- input[[hid(i, "ul")]]
      if (!is.null(u)) l$border_bottom <- if (isTRUE(u)) "single" else NA_character_
      lines[[i]] <- l
    }
    lines
  }
  shiny::observe(builder_guard({
    hdr_ver()
    n <- bform$n
    output[[paste0("b", n, "_hdr_ui")]] <- shiny::renderUI({
      lines <- bform$hdr
      keys <- shiny::isolate(input[[bid("key")]]) %||% bform$st$key
      hp <- .header_preset_choices(length(bform$meta$hierarchy) > 0L,
                                   .key_label(bform$meta, keys[1L]))
      head <- shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-end mb-2",
        shiny::h6(class = "me-auto mb-0", t("Column header")),
        shiny::div(style = "width: 14rem",
                   shiny::selectInput(bid("hdr_preset"), NULL,
                                      c(stats::setNames("", t("From a preset...")), hp))),
        .btn(bid("hdr_add"), t("Add a line above"), class = "btn-sm btn-outline-primary"))
      if (is.null(lines)) {
        return(shiny::tagList(head, shiny::div(
          class = "small text-muted",
          t("This report's column header is more than the form shows (positions, KEY = value, styled row-header cells): edit it in Details (sheets), col_header."))))
      }
      act <- function(i, what, label) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-link py-0 px-1", label,
        onclick = sprintf("Shiny.setInputValue('%s', {i: %d, act: '%s', t: Date.now()}, {priority: 'event'})",
                          bid("hdr_act"), i, what))
      one <- function(i) {
        l <- lines[[i]]
        st <- l$stub
        stub_ui <- if (!length(st)) NULL else lapply(seq_along(st), function(k)
          shiny::textInput(hid(i, paste0("stub", k)),
                           if (length(st) > 1L) sprintf(t("Row-header column %s"), names(st)[k]) else
                             t("Row-header columns"),
                           if (is.na(st[[k]])) "" else st[[k]], width = "100%"))
        shiny::div(
          class = "rp-b-hdr border rounded p-2 mb-2",
          shiny::div(class = "d-flex align-items-center",
                     shiny::strong(class = "small me-auto", sprintf(t("Line %d"), i)),
                     act(i, "up", "\u2191"), act(i, "down", "\u2193"), act(i, "del", "\u00d7")),
          stub_ui,
          shiny::radioButtons(hid(i, "mode"), t("Value columns"), inline = TRUE,
                              stats::setNames(c("each", "key", "all", "none"),
                                              c(t("the same on each column"),
                                                t("one cell per value of a key"),
                                                t("one cell over them all"),
                                                t("nothing"))),
                              selected = l$mode),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'key'", hid(i, "mode")),
            shiny::selectInput(hid(i, "key"), t("Key"), keys,
                               selected = if (!is.na(l$key)) l$key)),
          shiny::textAreaInput(hid(i, "text"), NULL,
                               if (is.na(l$text)) "" else l$text, rows = 2,
                               width = "100%"),
          shiny::div(
            class = "d-flex flex-wrap gap-3 align-items-center small",
            shiny::selectInput(hid(i, "align"), NULL, width = "9rem",
                               stats::setNames(c("", "left", "center", "right"),
                                               c(t("(default)"), t("left"), t("center"), t("right"))),
                               selected = if (is.na(l$align)) "" else l$align),
            shiny::checkboxInput(hid(i, "bold"), t("bold"), isTRUE(as.logical(l$bold))),
            shiny::checkboxInput(hid(i, "ul"), t("underline"),
                                 !is.na(l$border_bottom) && l$border_bottom != "none")))
      }
      shiny::tagList(
        head,
        lapply(seq_along(lines), one),
        shiny::uiOutput(bid("hdr_tok")),
        if (header_uses_n(lines)) shiny::selectInput(
          bid("hdr_n"), t("Whose {n}"), width = "18rem",
          stats::setNames(c("", "page", "table", "n = page | N = table"),
                          c(t("the default (page)"), t("each page's (page)"),
                            t("the analysis set (table)"), t("both: {n} page, {N} table"))),
          selected = if (is.na(bform$hdr_n)) "" else bform$hdr_n),
        shiny::tags$script(shiny::HTML(paste0(
          "if (!window.tflHdrInsert) {",
          " document.addEventListener('focusin', function(e) {",
          "  if (e.target.closest && e.target.closest('.rp-b-hdr') &&",
          "      /^(TEXTAREA|INPUT)$/.test(e.target.tagName) && e.target.type !== 'radio' &&",
          "      e.target.type !== 'checkbox') window.tflHdrField = e.target; });",
          " window.tflHdrInsert = function(tk) {",
          "  var f = window.tflHdrField; if (!f) return;",
          "  var a = f.selectionStart || 0, b = f.selectionEnd || 0;",
          "  f.value = f.value.slice(0, a) + tk + f.value.slice(b);",
          "  f.focus(); f.selectionStart = f.selectionEnd = a + tk.length;",
          "  $(f).trigger('change'); f.dispatchEvent(new Event('input', {bubbles: true})); };",
          "}"))))
    })
  }))
  # the insert chips, with what each token holds in the preview (drawn on
  # their own: the lines' fields are not drawn again while one types)
  shiny::observe(builder_guard({
    hdr_ver()
    n <- bform$n
    output[[paste0("b", n, "_hdr_tok")]] <- shiny::renderUI({
      keys <- input[[bid("key")]] %||% bform$st$key
      toks <- header_token_choices(keys, input[[bid("hdr_n")]] %||% bform$hdr_n)
      pv <- tryCatch(preview_d(), error = function(e) NULL)
      toks <- header_token_labels(toks, attr(pv$pages, "header_tokens"))
      chip <- function(k) shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary py-0 me-1 mb-1",
        names(toks)[k], onclick = sprintf("tflHdrInsert('%s')", toks[[k]]))
      shiny::div(class = "small", t("Insert (into the field last clicked):"), " ",
                 lapply(seq_along(toks), chip))
    })
  }))
  # moving, removing, adding a line; a preset into the lines
  shiny::observeEvent(input[[bid("hdr_act")]], {
    a <- input[[bid("hdr_act")]]
    lines <- hdr_now()
    i <- as.integer(a$i)
    shiny::req(!is.null(lines), i >= 1L, i <= length(lines))
    j <- switch(a$act, up = i - 1L, down = i + 1L, NA_integer_)
    if (identical(a$act, "del")) {
      lines <- lines[-i]
    } else if (!is.na(j) && j >= 1L && j <= length(lines)) {
      lines[c(i, j)] <- lines[c(j, i)]
    }
    bform$hdr <- lines
    hdr_ver(hdr_ver() + 1L)
  })
  shiny::observeEvent(input[[bid("hdr_add")]], {
    lines <- hdr_now() %||% list()
    st <- if (length(lines)) lines[[1L]]$stub else c(row_label = NA_character_)
    st[] <- NA_character_
    new <- list(line = 0L, stub = st, merge = FALSE, mode = "key",
                key = (input[[bid("key")]] %||% bform$st$key)[1L],
                text = NA_character_, align = NA_character_,
                bold = NA_character_, border_bottom = "single")
    bform$hdr <- c(list(new), lines)
    hdr_ver(hdr_ver() + 1L)
  })
  shiny::observeEvent(input[[bid("hdr_preset")]], {
    pr <- input[[bid("hdr_preset")]]
    shiny::req(nzchar(pr), pr %in% names(header_presets()))
    bform$hdr <- header_read(header_presets()[[pr]],
                             input[[bid("key")]] %||% bform$st$key)
    hdr_ver(hdr_ver() + 1L)
    notify(sprintf(t("The header is the preset %s now."), pr))
  })
  bstate <- shiny::reactive(builder_guard({
    bform_drawn()
    shiny::req(identical(builder_case(), "ok"),
               identical(bform$id, current()))
    st <- bform$st
    get <- function(x) input[[bid(x)]]
    vars <- get("vars")
    ks <- get("key")
    shiny::req(!is.null(vars), length(ks) > 0L,
               setequal(vars, st$variables$variable))
    # the column variables in the order dragged (outermost first)
    ko <- get("keyorder")
    if (length(ks) > 1L && setequal(ko, ks)) ks <- ko
    arms <- stats::setNames(lapply(ks, function(k)
      input[[arms_id(k)]] %||% key_levels(k)), ks)
    v <- st$variables[match(vars, st$variables$variable), , drop = FALSE]
    idx <- match(vars, st$variables$variable)
    v$label <- vapply(seq_along(vars), function(k) {
      l <- get(paste0("lab", idx[k]))
      if (is.null(l)) as.character(v$label[k]) else if (nzchar(trimws(l))) l else
        NA_character_
    }, "")
    lev <- stats::setNames(lapply(seq_along(vars), function(k)
      get(paste0("lv", idx[k])) %||% st$levels[[vars[k]]]), vars)
    num <- function(x, d) {
      x <- suppressWarnings(as.numeric(get(x)))
      if (length(x) != 1L || is.na(x)) d else max(0, round(x))
    }
    stats <- builder_stats()$key
    list(key = ks, arms = arms, variables = v, levels = lev,
         stats = stats[stats %in% get("stats")],
         decimals = num("dec", st$decimals),
         cat_format = get("cat") %||% st$cat_format,
         pct_decimals = num("pct", st$pct_decimals),
         header = {
           hdr_ver()
           l <- hdr_now()
           if (is.null(l)) "keep" else header_write(l)
         },
         header_n = if (!is.null(get("hdr_n"))) {
           v <- get("hdr_n")
           if (nzchar(v)) v else NA_character_
         },
         auto_levels = st$auto_levels)
  }))
  bstate_d <- shiny::debounce(bstate, 400)
  shiny::observeEvent(bstate_d(), {
    st <- bstate_d()
    id <- bform$id
    p2 <- guarded(builder_write(rv$p, id, st))
    if (!is.null(p2) && !identical(p2, rv$p)) {
      session$sendCustomMessage("builder-updating", TRUE)
      rv$p <- p2
      rv$btouched <- TRUE
    }
  })
  shiny::observe(builder_guard({
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
  }))
  preview <- shiny::reactive({
    id <- current()
    shiny::req(!is.null(id), identical(report_info(rv$p, id)$type, "table"))
    rv$ard_ver
    d <- ard_data(rv$study, id)
    if (is.null(d)) return(list(error = t("No data to show yet: Preview this table's ARD.")))
    tryCatch(list(pages = preview_pages(rv$p, id, d)),
             error = function(e) list(error = conditionMessage(e)))
  })
  preview_d <- shiny::debounce(preview, 300)
  # the first page as the report's rows and the study defaults make it:
  # header, titles, the body's start, footnotes, footer
  shiny::observeEvent(input$page_full, {
    shiny::showModal(shiny::modalDialog(
      title = t("First page (sample)"), size = "xl", easyClose = TRUE,
      shiny::div(class = "rp-page-full", shiny::uiOutput("page_sample_full")),
      footer = shiny::modalButton(t("Close"))))
  })
  output$page_sample_full <- shiny::renderUI(page_sample_ui())
  output$page_sample <- shiny::renderUI(page_sample_ui())
  page_sample_ui <- function() {
    id <- current()
    if (is.null(id)) {
      return(shiny::p(class = "small text-muted",
                      t("Choose a report in the sidebar.")))
    }
    rv$ver
    p <- rv$p
    info <- report_info(p, id)
    body <- if (identical(info$type, "table")) {
      pv <- tryCatch(preview_d(), error = function(e) NULL)
      if (!is.null(pv$pages)) preview_html(pv$pages[1L], max_pages = 1L) else
        shiny::div(class = "text-muted small", pv$error %||% t("(the table)"))
    } else {
      shiny::div(class = "text-muted small text-center py-4",
                 if (identical(info$type, "figure")) t("(the figure: see its Content)") else
                   t("(the listing: see its Content)"))
    }
    .page_sample_html(p, id, rv$meta$study_id %||% "", body,
                      program = info$program)
  }
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
                        data = character(), title = character())
    if (is.null(p) || !nrow(p$outputs)) return(empty)
    o <- p$outputs
    info <- lapply(o$output_id, function(id) report_info(p, id))
    type <- vapply(info, `[[`, "", "type")
    st <- tryCatch(ard_status(current_study()), error = function(e) NULL)
    ard_of <- if (!is.null(st)) st$state[match(o$output_id, st$output_id)] else
      rep(NA_character_, nrow(o))
    # the datasets the report reads (lower case, as the files are named);
    # reworked by the report's own data code, said after them
    data <- vapply(seq_len(nrow(o)), function(i) {
      ds <- tolower(.report_datasets(p, o$output_id[i], type[i]))
      txt <- if (length(ds)) paste(ds, collapse = " / ") else "-"
      if (!is.na(o$data_code[i])) {
        note <- t("(reworked by its own code)")
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
    titles <- ifelse(nzchar(titles) | is.na(o$description), titles,
                     o$description)
    data.frame(
      output_id = o$output_id,
      type = t(unname(.type_labels[type])),
      program = vapply(info, `[[`, "", "program"),
      rtf = vapply(info, `[[`, "", "file"),
      data = .cell_tip(data, data_tip),
      title = .cell_tip(.ellipsis(titles, 70L), titles),
      stringsAsFactors = FALSE)
  })
  output$outputs <- DT::renderDT({
    v <- outputs_view()
    sel <- match(shiny::isolate(input$target), v$output_id)
    v$output_id <- htmltools::htmlEscape(v$output_id)
    v$program <- htmltools::htmlEscape(v$program)
    v$rtf <- htmltools::htmlEscape(v$rtf)
    # "Title" here is the report's title; t("Title") is the study's
    names(v) <- c(t(c("output_id", "Type", "Program", "RTF", "Data")),
                  if (identical(lang, "en")) "Title" else t("Report title"))
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
                                 ordering = FALSE, scrollX = TRUE))
  })
  shiny::observeEvent(input$target, {
    v <- outputs_view()
    sel <- match(input$target, v$output_id)
    if (!identical(input$outputs_rows_selected, if (!is.na(sel)) sel)) {
      DT::selectRows(DT::dataTableProxy("outputs"), if (!is.na(sel)) sel)
    }
  })
  # a double click on a report: open its Content
  shiny::observeEvent(input$outputs_dbl, {
    v <- shiny::isolate(outputs_view())
    id <- v$output_id[input$outputs_dbl]
    if (!length(id) || is.na(id)) return()
    shiny::updateSelectInput(session, "target", selected = id)
    bslib::nav_select("rep_nav", "content")
  })
  shiny::observeEvent(input$outputs_rows_selected, {
    v <- shiny::isolate(outputs_view())
    id <- v$output_id[input$outputs_rows_selected]
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
      if (type) shiny::conditionalPanel(
        "input.modal_type == 'listing'",
        shiny::checkboxInput(
          "modal_first_l",
          t("Start the listing from the data (writes its columns and shows it)"),
          value = TRUE, width = "100%"),
        shiny::conditionalPanel("input.modal_first_l",
                                shiny::uiOutput("modal_first_l_ui"))),
      if (type) shiny::conditionalPanel(
        "input.modal_type == 'table'",
        shiny::checkboxInput(
          "modal_first",
          t("Start the table from the data (writes its ARD definition and opens the builder)"),
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
    desc <- if (nzchar(d)) d else NA
    if (identical(input$modal_type, "table") && isTRUE(input$modal_first)) {
      return(start_first_table(id, desc))
    }
    if (identical(input$modal_type, "listing") && isTRUE(input$modal_first_l)) {
      return(start_first_listing(id, desc))
    }
    p2 <- guarded(add_output(rv$p, id, description = desc,
                             type = input$modal_type))
    after_id_change(p2, id)
    # the new report's own tab
    if (!is.null(p2)) go(switch(input$modal_type,
      table = "tables", listing = "lf", figure = "designer", "outputs"))
  })
  # -- the first listing: the data and its columns --------------------------
  first_data_l <- shiny::reactive({
    shiny::req(input$ml_data %in% first_files())
    guarded(read_data_head(file.path(rv$study$path, input$ml_data), n = 2000L))
  })
  output$modal_first_l_ui <- shiny::renderUI({
    f <- first_files()
    if (!length(f)) {
      return(shiny::div(class = "alert alert-warning py-1 small",
        t("No data files yet: put them on the Data tab (data/adam), then add the listing.")))
    }
    shiny::tagList(
      shiny::selectInput("ml_data", t("Data"), f,
                         selected = c(grep("adae", f, value = TRUE), f)[1L],
                         width = "100%"),
      shiny::uiOutput("ml_cols"))
  })
  output$ml_cols <- shiny::renderUI({
    d <- first_data_l()
    shiny::req(d)
    cols <- as.list(d)
    grp <- .group_choices(cols)
    all <- .labelled(names(cols), cols)
    usual <- c("USUBJID", "AEBODSYS", "AEDECOD", "ASTDT", "AENDT", "AESEV",
               "AESER", "AEREL", "AEOUT", "PARAM", "AVISIT", "AVAL", "AVALC",
               "CMDECOD", "MHDECOD")
    shiny::tagList(
      shiny::selectInput("ml_group", t("Group (ordered by first; optional)"),
                         c(stats::setNames("", t("(none)")), grp),
                         selected = c(intersect(c("TRTA", "TRT01A", "ARM"), grp), "")[1L],
                         width = "100%"),
      shiny::selectizeInput(
        "ml_cols_pick", t("Columns (in order)"), all,
        selected = intersect(usual, names(cols)), multiple = TRUE,
        width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"))),
      shiny::p(class = "small text-muted",
               t("Each column is headed by the data's label; the order is the group, the subject and the start date. Everything can be changed afterwards on the Listings tab.")))
  })
  start_first_listing <- function(id, desc) {
    d <- first_data_l()
    if (is.null(d)) return(notify(t("Choose the data"), "warning"))
    g <- input$ml_group
    p2 <- guarded(first_listing(rv$p, id, input$ml_data, d,
                                columns = input$ml_cols_pick,
                                group = if (length(g) && nzchar(g)) g,
                                description = desc))
    if (is.null(p2)) return()
    after_id_change(p2, id)
    go("lf")
    lf_show_preview(id)
  }
  # -- the first table: a few answers instead of five sheets ---------------
  first_files <- shiny::reactive({
    f <- data_files()
    ok <- !is.na(vapply(f$folder, .folder_level, "", USE.NAMES = FALSE)) &
      tolower(tools::file_ext(f$file)) %in% .data_exts
    file.path(f$folder, f$file)[ok]
  })
  first_data <- shiny::reactive({
    shiny::req(input$mf_data %in% first_files())
    guarded(read_data_head(file.path(rv$study$path, input$mf_data), n = Inf))
  })
  output$modal_first_ui <- shiny::renderUI({
    f <- first_files()
    if (!length(f)) {
      return(shiny::div(class = "alert alert-warning py-1 small",
        t("No data files yet: put them on the Data tab (data/adam), then add the table.")))
    }
    shiny::tagList(
      shiny::selectInput("mf_data", t("Data"), f,
                         selected = c(grep("adsl", f, value = TRUE), f)[1L],
                         width = "100%"),
      shiny::uiOutput("mf_cols"))
  })
  output$mf_cols <- shiny::renderUI({
    d <- first_data()
    shiny::req(d)
    cols <- as.list(d)
    fl <- grep("FL$", names(cols), value = TRUE)
    grp <- .group_choices(cols)
    vars <- .row_choices(cols, c(continuous = t("numbers"),
                                 categorical = t("counts")))
    shiny::tagList(
      shiny::selectInput("mf_pop", t("Analysis set (its flag = \"Y\")"),
                         .labelled(fl, cols),
                         selected = c(intersect(c("SAFFL", "ITTFL", "FASFL"), fl),
                                      fl)[1L], width = "100%"),
      shiny::selectInput("mf_group", t("Group (the columns)"), grp,
                         selected = c(intersect(c("TRT01A", "TRT01P", "ARM"),
                                                grp), grp)[1L],
                         width = "100%"),
      shiny::selectizeInput(
        "mf_vars", t("Variables (the rows, in order)"), vars,
        selected = intersect(c("AGE", "AGEGR1", "SEX", "RACE"), vars),
        multiple = TRUE, width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"))),
      shiny::p(class = "small text-muted",
               t("Numbers get summary statistics; the others, counts and percents. Everything can be changed afterwards (builder, ARD tab).")))
  })
  start_first_table <- function(id, desc) {
    d <- first_data()
    if (is.null(d)) return(notify(t("Choose the data"), "warning"))
    p2 <- guarded(first_table(rv$p, id, input$mf_data, d,
                              population = input$mf_pop,
                              group = input$mf_group,
                              variables = input$mf_vars,
                              description = desc))
    if (is.null(p2)) return()
    after_id_change(p2, id)
    go("tables")
    bslib::nav_select("table_nav", "builder")
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
    to_catalog()
  })
  shiny::observeEvent(input$data_refresh, to_catalog(), ignoreInit = TRUE)
  # the data files the catalog (ARD tab > datasets) does not have yet go in
  # (nobody edited anything: a study that was saved is saved again)
  to_catalog <- function() {
    if (!has_study()) return()
    was_clean <- !isTRUE(dirty())
    p2 <- guarded(catalog_add_files(rv$p, data_files()))
    added <- attr(p2, "added")
    if (!length(added)) return()
    attr(p2, "added") <- NULL
    rv$p <- p2
    bump()
    if (was_clean) do_save()
    notify(sprintf(t("In the data catalog now: %s"), paste(added, collapse = ", ")))
  }
  shiny::observeEvent(input$data_open, .open_folder(
    file.path(rv$study$path, "data")))
  data_head <- shiny::reactive({
    # a row selection can outlive the list it was made in (the list is
    # drawn again when the study or the folder changes): read only a file
    # that is there
    path <- data_files()$path[input$data_files_rows_selected]
    shiny::req(length(path) == 1L, !is.na(path), file.exists(path))
    guarded(read_data_head(path))
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
    word <- .run_state[d$status]
    word[is.na(word)] <- "not built"
    v <- data.frame(
      a = d$output_id, b = t(unname(.type_labels[d$type])),
      c = t(unname(.ard_state_labels[word])),
      d = ifelse(word == "built" & !is.na(d$rtf), d$rtf, ""),
      e = ifelse(word == "built", "", t(unname(.status_labels[d$status]))),
      f = paste0(d$program, ifelse(d$program_state %in% "edited",
                                   paste0(" (", t("edited by hand"), ")"), "")),
      stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Type", "State", "When", "Why", "Program"))
    DT::formatStyle(
      .dt(v, selection = "multiple"), names(v)[3L],
      color = DT::styleEqual(t(unname(.ard_state_labels)),
                             c("#15803d", "#b45309", "#6b7280", "#b91c1c")))
  })
  selected_status <- shiny::reactive({
    d <- status()
    i <- input$status_rows_selected
    # no row chosen: the report chosen in the sidebar
    if (!length(i) && !is.null(current())) i <- match(current(), d$output_id)
    d[stats::na.omit(i), , drop = FALSE]
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
