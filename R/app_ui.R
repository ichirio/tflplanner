# ------------------------------------------------------------------- UI

.code_css <- "
/* controls side by side (a choice and a file input; buttons and a tick):
   one height, one bottom line; a file input's upload bar takes no room */
.rp-upload > .shiny-input-container { margin-bottom: 0; position: relative; }
.rp-upload .shiny-file-input-progress { position: absolute; left: 0; right: 0;
  top: 100%; margin: 2px 0 0; }
.rp-upload select, .rp-upload .input-group .form-control,
.rp-upload .input-group .btn-file { height: 38px; }
.rp-upload .input-group .btn-file { display: inline-flex; align-items: center; }
.rp-upload .checkbox, .rp-upload .form-check { margin: 0; }
.rp-upload select { appearance: auto; }
/* fields that are not used (a data written as R) */
.rp-greyed { opacity: .45; pointer-events: none; }
/* a row of inputs, selects and buttons: the same height (the small size),
   no space under an input, the label beside it */
.rp-inline { display: flex; flex-wrap: wrap; gap: .5rem; align-items: center; }
.rp-inline .shiny-input-container, .rp-inline .form-group { margin-bottom: 0; width: auto; }
.rp-inline .form-control, .rp-inline .form-select { height: calc(1.5em + .5rem + 2px);
  padding: .25rem .5rem; font-size: .875rem; }
.rp-inline .btn { padding-top: .25rem; padding-bottom: .25rem; }
/* a top tab the chosen report has nothing on */
.nav-link.rp-idle { opacity: .45; }
/* the top bar (the pages, Save) stays in view on a tall page (a figure's
   designer, the table builder); its height (--rp-top, .sticky_top_js) is
   where the panes that stay in view start, and what a card brought into
   view is kept clear of */
nav.navbar.navbar-static-top { position: sticky; top: 0; z-index: 1020;
  background-color: var(--bs-body-bg, #fff); }
html { scroll-padding-top: calc(var(--rp-top, 0px) + .5rem); }
/* a step's mark worked out again (a run in the background): not faded */
[id^='step_mark_'].recalculating { opacity: 1 !important; }
/* While the server works on what the user did (opening a study, switching
   a tab or a report, saving) the page takes no clicks: a veil, after 0.4 s
   so that the short updates (the builder's preview) do not flicker or
   block.  Work the user did not start -- a run in the background
   followed by the poll -- never veils the page (rp-user-busy: a busy state
   that began right after a click or a key). */
body::after { content: ''; position: fixed; inset: 0; z-index: 2000;
  visibility: hidden; background: rgba(255,255,255,0); cursor: progress;
  transition: visibility 0s, background 0s; }
html.shiny-busy.rp-user-busy body::after { visibility: visible;
  background: rgba(255,255,255,.35);
  transition: visibility 0s .4s, background .2s .4s; }
.rp-code textarea, .rp-code pre { font-family: Consolas, 'Courier New',
  monospace; font-size: 12.5px; }
/* the code: as tall as the window lets it (the sticky column's), its
   bottom edge dragged to make it shorter or taller */
.rp-code pre { max-height: calc(100vh - 12rem); min-height: 6rem; overflow: auto; white-space: pre;
  resize: vertical; }
/* a step's own code: the header, the editor and its check on a laptop's screen
   (above it 320px, below ~90px: measured at 1366x640 and 1536x730) */
.rp-code-step textarea { height: clamp(12em, calc(100vh - 410px), 36em); }
.rp-help { font-size: 12.5px; }
/* the ARD form's function chooser: one line once chosen (Change opens
   it); a near match of the search faint */
details.ard-fn > summary { list-style: none; cursor: pointer; }
details.ard-fn > summary::-webkit-details-marker { display: none; }
details.ard-fn[open] .ard-fn-closed, details.ard-fn:not([open]) .ard-fn-open { display: none; }
.ard-fn-near { opacity: .7; }
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
/* the builder's card of what the ARD holds: one line, its table when opened */
.rp-ard-card { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .375rem;
  padding: .3rem .6rem; margin-bottom: .5rem; font-size: 12.5px; }
.rp-ard-card > summary { cursor: pointer; color: #374151; }
.rp-ard-card table { font-size: 12px; margin: .4rem 0; }
.rp-ard-card td, .rp-ard-card th { padding: 1px 8px 1px 0; vertical-align: top; }
.rp-ard-card th { color: #6b7280; font-weight: 600; }
/* a numbered part of a form (1-1's column definitions): its number in the
   margin, the field beside it */
.rp-num-part { display: flex; gap: .4rem; align-items: flex-start; }
.rp-num-part > .rp-num { color: var(--bs-primary, #0d6efd); font-weight: 600;
  padding-top: .1rem; min-width: 1.1em; }
.rp-num-part > :not(.rp-num) { flex: 1 1 auto; min-width: 0; }
/* the column header as a grid with the table's columns: the names on
   top, a row of fields a header line, a line's tools under it */
.rp-hgrid-wrap { overflow-x: auto; }
.rp-hgrid { display: grid; gap: .25rem .35rem; align-items: start; }
.rp-hgrid .rp-hcol { font-size: 11px; color: #6b7280; white-space: nowrap;
  overflow: hidden; text-overflow: ellipsis; border-bottom: 1px solid #e5e7eb; }
.rp-hgrid .rp-hdr-tools { grid-column: 1 / -1; display: flex; flex-wrap: wrap;
  gap: .25rem .6rem; align-items: center; font-size: 12px;
  padding: .2rem .4rem; margin-top: .45rem;
  background: var(--bs-tertiary-bg, #f3f4f6); border-top: 1px solid #9ca3af;
  border-radius: .2rem .2rem 0 0; }
.rp-hgrid .rp-hdr-tools > strong { font-size: 13px; }
/* a part of the header form of its own: the insert chips, what {n} counts */
.rp-hdr-box { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .375rem;
  padding: .35rem .55rem; margin-top: .6rem; }
.rp-hdr-box-title { font-size: 12px; font-weight: 600; color: #4b5563; margin-bottom: .25rem; }
.rp-hdr-box .shiny-input-container { margin-bottom: 0; width: auto; }
.rp-hdr-box .radio label, .rp-hdr-box .shiny-options-group { font-size: 13px; }
.rp-hdr-preset-sample { font-family: Consolas, 'Courier New', monospace; font-size: 11px;
  color: #6b7280; margin-left: .4rem; }
.rp-hgrid .rp-hdr-cell { display: flex; flex-direction: column; min-width: 0; }
.rp-hgrid .rp-hdr-cell-tools { display: flex; justify-content: flex-end; line-height: 1; }
.rp-hgrid .rp-hdr-cap { font-size: 11px; color: #6b7280; text-align: center; }
.rp-hgrid .rp-hdr-none { color: #9ca3af; text-align: center; padding-top: .3rem; }
.rp-b-hdr .shiny-input-container { width: auto; margin-bottom: .1rem; }
.rp-b-hdr .form-control { padding: 2px 6px; font-size: 13px; min-height: 0; }
.rp-b-hdr .selectize-input { padding: 2px 6px; min-height: 0; font-size: 13px; }
.rp-b-hdr .rp-hdr-tools .shiny-input-container,
.rp-b-hdr .rp-hdr-tools .checkbox { margin: 0; }
.rp-b-hdr .rp-hdr-tools label { font-size: 12px; margin: 0; }
/* the code-list editors' box of every variable: its label on one line (not
   the 300 px of an input) */
.shiny-input-container:has(> .checkbox input[id$='_all']) { width: auto; margin-bottom: 0; }
/* none chosen: no empty frame (not display: none -- shiny draws nothing
   into a hidden output, so the form would never come) */
.rp-b-card:has(> #ard_stat_ui:empty) { border: 0; padding: 0; margin: 0; }
.rp-b-card .rank-list-container { margin: 0; }
.rp-b-card .rank-list-item { padding: 2px 8px !important; font-size: 13px; }
.rp-b-var { display: flex; gap: .5rem; align-items: center; }
.rp-b-kind { font-size: 11px; color: #6b7280; }
#studies td { white-space: nowrap; }
#studies td:nth-child(2) { white-space: normal; }
.rp-stat-fmt { display: grid; grid-template-columns: repeat(auto-fill,
  minmax(9.5em, 1fr)); gap: 0 .5rem; }
.rp-stat-fmt .form-group { margin-bottom: .3rem; }
#study_detail code { word-break: break-all; }
.rp-stat-fmt label { font-size: 12px; margin-bottom: 0; }
.rp-split { display: grid; gap: 1rem; align-items: start;
  grid-template-columns: minmax(0, 3fr) minmax(0, 2fr); }
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

# A section of the study tab's settings: a heading that folds it.  Open or
# closed as the viewer left it (kept in the browser), else `open`.
.study_section <- function(name, title, ..., open = FALSE) {
  shiny::tags$details(
    class = "rp-sec", `data-sec` = name, open = if (open) NA,
    shiny::tags$summary(title),
    shiny::div(class = "rp-sec-body", ...))
}

.study_section_css <- "
.rp-sec { border-top: 1px solid var(--bs-border-color, #dee2e6); padding: .35rem 0; }
.rp-sec > summary { font-weight: 600; font-size: .95rem; cursor: pointer;
  padding: .25rem 0; }
.rp-sec > .rp-sec-body { padding-top: .4rem; }
"

.study_section_js <- "
(function() {
  function key(d) { return 'tflplanner.sec.' + d.getAttribute('data-sec'); }
  function restore() {
    document.querySelectorAll('details.rp-sec[data-sec]').forEach(function(d) {
      var v = null;
      try { v = localStorage.getItem(key(d)); } catch (e) {}
      if (v === 'open') d.open = true;
      if (v === 'closed') d.open = false;
    });
  }
  document.addEventListener('toggle', function(e) {
    var d = e.target;
    if (!d.matches || !d.matches('details.rp-sec[data-sec]')) return;
    try { localStorage.setItem(key(d), d.open ? 'open' : 'closed'); } catch (e2) {}
    // a grid drawn while folded: drawn again at its size
    if (d.open) setTimeout(function() {
      $(d).find('.rhandsontable').each(function() {
        var w = window.HTMLWidgets && HTMLWidgets.find('#' + this.id);
        if (w && w.hot) w.hot.render();
      });
    }, 0);
  }, true);
  $(document).on('shiny:value', function(e) {
    if (e.name === 'study_detail' || e.name === 'study_actions') setTimeout(restore, 0);
  });
  $(document).on('shiny:connected', function() { setTimeout(restore, 0); });
})();
"

# the ARD tab's panes: side by side, stacked, or one at a time (kept in
# the viewer's browser)
.split_js <- "
(function() {
  // step 1's (ard_) and step 3's (page_, #438) split, each kept on its own
  function group(pre) {
    var cur = {lay: 'side', pane: 'def'};
    try {
      var v = (localStorage.getItem('tflplanner.' + pre + 'layout') || '').split('|');
      if (v[0]) cur.lay = v[0];
      if (v[1]) cur.pane = v[1];
    } catch (e) {}
    function apply() {
      var s = document.getElementById(pre + 'split');
      if (!s) return;
      s.classList.remove('rp-lay-side', 'rp-lay-stack', 'rp-lay-one',
                         'rp-show-def', 'rp-show-out');
      s.classList.add('rp-lay-' + cur.lay, 'rp-show-' + cur.pane);
      document.querySelectorAll('#' + pre + 'layout [data-lay]').forEach(function(b) {
        b.classList.toggle('active', b.dataset.lay === cur.lay);
      });
      document.querySelectorAll('#' + pre + 'pane [data-pane]').forEach(function(b) {
        b.classList.toggle('active', b.dataset.pane === cur.pane);
      });
      var g = document.getElementById(pre + 'pane');
      if (g) g.style.display = cur.lay === 'one' ? '' : 'none';
      try { localStorage.setItem('tflplanner.' + pre + 'layout', cur.lay + '|' + cur.pane); }
      catch (e) {}
      setTimeout(function() {
        window.dispatchEvent(new Event('resize'));
        if (window.jQuery && jQuery.fn.dataTable) {
          jQuery.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
        }
      }, 50);
    }
    document.addEventListener('click', function(e) {
      var b = e.target.closest('#' + pre + 'layout [data-lay], #' + pre + 'pane [data-pane]');
      if (!b) return;
      if (b.dataset.lay) cur.lay = b.dataset.lay;
      if (b.dataset.pane) cur.pane = b.dataset.pane;
      apply();
    });
    document.addEventListener('DOMContentLoaded', apply);
  }
  group('ard_');
  group('page_');
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

  # a sheet's grid, with its help (in a tab of its own, or alone)
  sheet_body <- function(sheet) {
    shiny::tagList(
      if (sheet %in% .assisted) shiny::uiOutput(paste0("assist_", sheet)),
      rhandsontable::rHandsontableOutput(paste0("hot_", sheet)),
      shiny::uiOutput(paste0("inh_", sheet)),
      shiny::tags$details(
        class = "rp-help mt-2",
        shiny::tags$summary(t("Column help")),
        DT::DTOutput(paste0("help_", sheet))))
  }
  sheet_panel <- function(sheet) {
    bslib::nav_panel(t(.sheet_labels[[sheet]]), value = sheet, sheet_body(sheet))
  }
  # a paragraph of help, as an (i) where there is no heading (R/help_tip.R)
  about <- function(x) about_tip(x, lang)
  edit_note <- t("Right-click to add or delete rows. Paste from Excel works. Blank = not set. Several values in one cell: separate with |. Spaces at the start or end of a text are kept only in quotes: \"  Total\".")
  grid_note <- shiny::div(
    class = "text-muted small mb-1",
    with_tip(t("Editing the sheet"), edit_note))
  # a step of making a report: its name (one place: .step_labels) and its
  # state's mark
  step_title <- function(step) {
    shiny::span(shiny::span(class = "rp-step-name", `data-step` = step, t(.step_labels[[step]])),
                shiny::uiOutput(paste0("step_mark_", step), inline = TRUE))
  }

  # the report chosen, and the help on what a report's rows are
  report_side <- bslib::sidebar(
    id = "side", width = 330, open = "desktop",
    shiny::uiOutput("study_side"),
    # the reports to search and choose from (R/report_picker.R); the
    # chooser it sets is kept, out of sight
    report_picker_ui("rp", lang),
    shiny::div(style = "display: none",
               shiny::selectInput("target", t("Report (output_id)"),
                                  choices = c("-" = .all_rows), width = "100%")),
    shiny::tags$details(
      class = "small text-muted",
      shiny::tags$summary(t("What do these mean?")),
      shiny::p(shiny::strong(t("A report")), ": ",
               t("its own rows. Where it has none, it uses the study defaults.")),
      shiny::p(shiny::strong(t("Study defaults")), ": ",
               t("rows whose output_id is blank. They apply to every report that has no row of its own for the same thing: the page header, the run-information footer, the usual cell template, ...")),
      shiny::p(shiny::strong(t("ALL")), ": ",
               t("every row of every report and the defaults at once, with output_id shown: for bulk edits and pasting from Excel.")),
      # the marks after a report's title: its run's state
      shiny::p(shiny::strong(t("The marks")), ": ",
               lapply(names(.report_state_marks), function(k) shiny::span(
                 class = "me-2 text-nowrap",
                 shiny::span(class = .report_state_class[[k]], .report_state_marks[[k]]),
                 " ", t(.report_state_words[[k]]))))))

  # -- the steps of making a report ----------------------------------------
  step_ard <- shiny::div(
    class = "mt-2",
    # the definition or the report's own code (R/step_code.R)
    shiny::uiOutput("data_mode_bar"),
    # what a step of its own code reads: its datasets, its ARD
    shiny::conditionalPanel("output.data_inputs == 'yes'", shiny::uiOutput("uc_inputs")),
    # the report's ARD: a figure's source (one place, either way), a table's
    # taken in
    shiny::uiOutput("ard_kind_note"),
    shiny::conditionalPanel("output.data_is_code == 'yes'", code_step_ui("dc", t, lang)),
    shiny::conditionalPanel(
    "output.data_is_code != 'yes'",
    # a figure's: its data steps first (its design's; none while its
    # content is its own code), the numbers from an ARD below
    shiny::conditionalPanel("output.content_is_code != 'yes'", .designer_data_ui()),
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
        lay_btn("data-pane" = "out", "SPEC | Code | Result"))),
    shiny::div(
      id = "ard_split", class = "rp-split rp-lay-side rp-show-def",
      bslib::card(
        bslib::card_header(with_tip(t("ARD definition"), t("This report's analyses (the ARD): the data in 1-1, the analyses in 1-2."))),
        bslib::navset_underline(
          id = "ard_sheet",
          bslib::nav_panel(paste0(t("The analyses"), " (analyses)"), value = "analyses",
                           # 1-1 the analysis data (its form below it), then 1-2
                           # the analyses
                           shiny::uiOutput("ard_report_pop"),
                           shiny::uiOutput("ard_adata"),
                           shiny::uiOutput("adata_detail"),
                           shiny::uiOutput("ard_2_2_head"),
                           # the outline first: a click shows the analysis
                           # as a form below; the sheet itself folded
                           shiny::uiOutput("ard_outline"))),
        shiny::uiOutput("ard_check"),
        shiny::div(
          class = "rp-b-card mt-2",
          shiny::uiOutput("ard_stat_ui")),
        shiny::tags$details(
          class = "rp-help mt-2",
          shiny::tags$summary(t("Methods")),
          DT::DTOutput("ard_methods")),
        shiny::tags$details(
          class = "rp-help mt-2",
          shiny::tags$summary(t("Statistics (company standards)")),
          DT::DTOutput("ard_stat_catalog"))),
      # SPEC | Code | Result (R/result_tabs.R).  SPEC and Result are
      # filled by the server (ard_spec_pane, ard_result_pane): what is
      # chosen on the left decides what they show (1-2 an analysis; 1-1
      # an analysis data, S1's)
      result_tabs_ui(
        "ard_right", lang = lang, selected = "code",
        spec = shiny::uiOutput("ard_spec_pane"),
        code = shiny::tagList(
          shiny::radioButtons(
            "ard_scope", NULL,
            stats::setNames(c("report", "setup", "autoexec"),
                            c(t("This output's program"), "ard_setup.R",
                              "autoexec_ard.R")),
            inline = TRUE),
          shiny::uiOutput("ard_prog_state"),
          code_view("ard_code", lang)),
        result = shiny::uiOutput("ard_result_pane")))))

  step_content <- bslib::navset_underline(
    id = "content_nav",
    bslib::nav_panel(
      t("Content"), value = "content",
      # (a template report: its content is the template's, no R program)
      shiny::div(class = "rp-code-only", shiny::uiOutput("content_mode_bar")),
      shiny::conditionalPanel("output.content_is_code == 'yes'", code_step_ui("cc", t, lang)),
      shiny::conditionalPanel(
        "output.report_kind == 'table' && output.content_is_code != 'yes'",
        # not a card: a card around the sheets' own card makes the inner one a
        # fill item of a box with no height, and every grid in it 0 px high
        shiny::div(
          shiny::uiOutput("builder_note"),
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
            # a template report's arguments (R/template_report.R) above its
            # overridden cards
            shiny::div(shiny::div(class = "rp-tpl-only", shiny::uiOutput("tpl_card")),
                       shiny::uiOutput("builder_form")),
            # what the report's ARD holds, folded to one line, above the tabs;
            # beside the cards it stays in view (.rp-b-right) as they scroll
            shiny::div(
              class = "rp-b-right", style = "min-width: 0", `data-busy` = t("Updating ..."),
              shiny::uiOutput("builder_ard_card"),
              result_tabs_ui(
              "table_right", lang = lang, selected = "result",
              # (a template report's: the ARD its template makes)
              ard = shiny::uiOutput("tpl_ard_pane"),
              spec = shiny::tagList(
                shiny::div(class = "rp-tpl-only", shiny::uiOutput("tpl_spec_yaml")),
                shiny::div(class = "rp-code-only",
                shiny::uiOutput("type_note"),
                shiny::div(class = "rp-assist border rounded p-2 mb-2",
                           shiny::uiOutput("assist")),
                grid_note,
                do.call(bslib::navset_card_underline,
                        c(list(id = "table_sheet"),
                          lapply(setdiff(table_sheets(), "codelists"), sheet_panel))))),
              code = shiny::tagList(shiny::div(class = "rp-tpl-only", shiny::uiOutput("tpl_code_ard")),
                                    code_view("program_table", lang),
                                    shiny::uiOutput("builder_code_jump"),
                                    shiny::div(class = "rp-tpl-only", shiny::uiOutput("tpl_code_expanded"))),
              result = shiny::tagList(
                shiny::div(
                  class = "d-flex justify-content-between small mb-1",
                  shiny::span(t("The table as it will print"),
                              shiny::span(id = "builder_updating",
                                          class = "badge text-bg-warning ms-2 d-none",
                                          t("Updating ..."))),
                  shiny::uiOutput("builder_pages", inline = TRUE)),
                shiny::uiOutput("builder_preview"))))))),
      shiny::conditionalPanel(
        "output.report_kind == 'listing' && output.content_is_code != 'yes'",
        shiny::uiOutput("lf_note"),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
          shiny::div(
            shiny::div(class = "rp-tpl-only", shiny::uiOutput("tpl_card_lf")),
            shiny::div(class = "rp-code-only",
                       shiny::uiOutput("lf_form"),
                       shiny::uiOutput("lf_cols_box"))),
          result_tabs_ui(
            "lf_right", lang = lang, selected = "result",
            spec = shiny::tagList(
              shiny::p(class = "small text-muted",
                       t("The listing's row of the definition (listings); its columns are the grid on the left.")),
              rhandsontable::rHandsontableOutput("hot_lf_listings")),
            code = code_view("program_lf", lang),
            result = shiny::tagList(
              shiny::div(class = "mb-2",
                         .btn("lf_preview", t("Preview the listing"),
                              class = "btn-sm btn-outline-primary")),
              shiny::uiOutput("lf_preview_out"))))),
      shiny::conditionalPanel("output.report_kind == 'figure'",
                              shiny::div(class = "rp-tpl-only",
                                         shiny::uiOutput("tpl_card_fig"), shiny::uiOutput("tpl_fig_out"))),
      shiny::conditionalPanel("output.content_is_code != 'yes'",
                              shiny::div(class = "rp-code-only", .designer_ui(t)))),
    bslib::nav_panel(
      t("Data code"), value = "code",
      shiny::uiOutput("current_label"),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::div(style = "flex: 3 1 20rem",
                   shiny::textInput("description", t("Description"), width = "100%")),
        shiny::div(style = "flex: 1 1 12rem",
                   shiny::textInput("section", t("Section (heading)"), width = "100%",
                                    placeholder = t("blank: from the ID")))),
      shiny::div(
        class = "rp-code",
        shiny::conditionalPanel(
          "output.report_kind == 'table' || output.report_kind == 'listing'",
          shiny::textAreaInput(
            "process_code",
            with_tip(t("Rework"),
                     paste(t("Rework: usually blank (the standard one). Write code to change the data before the table is made."),
                           t("A table: make `data` from `ard` (normalize_ard(), then mutate() ...); blank = the company template (normalize). A listing: change `data` after its condition, before its order. (A whole step of your own code: R program at the top of step 1 or 2.)"))),
            rows = 5, width = "100%", resize = "vertical",
            placeholder = "data <- normalize_ard(ard)"),
          shiny::div(
            class = "d-flex gap-2 align-items-center mb-2",
            .btn("fetch", t("Update preview"),
                 class = "btn-sm btn-outline-primary"),
            help_tip(t(.preview_tip))),
          shiny::uiOutput("ard_summary")))))

  step_page <- shiny::div(
    class = "mt-2",
    pane_head(t("This report's page"), t("The page of the report chosen on the left: its titles, footnotes, its own header or footer, and tokens of your own ({STUDY} ...). Study defaults = every report's.")),
    shiny::tags$style(shiny::HTML(.page_form_css)),
    shiny::tags$script(shiny::HTML(.page_form_js)),
    # its page pattern (Standard, Compact ...): what it inherits
    shiny::uiOutput("page_pattern_bar"),
    # the layout, as step 1's: side by side, stacked, one pane (#438)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2 small",
      shiny::span(class = "text-muted", t("Layout")),
      shiny::div(
        id = "page_layout", class = "btn-group btn-group-sm", role = "group",
        lay_btn("data-lay" = "side", t("Side by side")),
        lay_btn("data-lay" = "stack", t("Stacked")),
        lay_btn("data-lay" = "one", t("One pane"))),
      shiny::div(
        id = "page_pane", class = "btn-group btn-group-sm", role = "group",
        lay_btn("data-pane" = "def", t("This report's page")),
        lay_btn("data-pane" = "out", "SPEC | Code | Result"))),
    # the input on the left -- the page's form -- and on the right what it
    # makes: the first page, the program, the rows as they are written
    # (SPEC, read only); the right stays in view as the form scrolls
    shiny::div(
      id = "page_split", class = "rp-split rp-lay-side rp-show-def",
      shiny::div(
        # the page as a form: each value with where it comes from
        shiny::uiOutput("page_form"),
        # the sheets themselves, the same rows
        shiny::tags$details(
          class = "rp-help mt-2", id = "page_sheets_box",
          shiny::tags$summary(t("Details: edit the sheets")),
          grid_note,
          do.call(bslib::navset_card_underline,
                  c(list(id = "page_sheet"), lapply(report_sheets(), sheet_panel))))),
      result_tabs_ui(
        "page_right", lang = lang, selected = "result",
        spec = shiny::uiOutput("page_spec_view"),
        code = shiny::tagList(
          shiny::uiOutput("program_state"),
          code_view("program", lang)),
        result = shiny::tagList(
          shiny::div(class = "d-flex justify-content-between align-items-center gap-2 mb-1 small",
                     shiny::span(t("First page (sample)")),
                     shiny::div(
                       class = "d-flex gap-1",
                       # a table's numbers: its ARD made again (step 2's button)
                       .btn("page_fetch", t("Update preview"),
                            class = "btn-sm btn-outline-primary py-0"),
                       # fit to the pane's width by default; actual size
                       # on a click, remembered in this browser
                       shiny::tags$button(
                         type = "button", id = "page_fit",
                         class = "btn btn-sm btn-outline-secondary py-0",
                         onclick = "rpPageFit(!rpPageFitOn());",
                         t("Fit to width")),
                       .btn("page_full", t("Full size"),
                            class = "btn-sm btn-outline-secondary py-0"))),
          shiny::div(class = "rp-page-wrap", shiny::uiOutput("page_sample"))))))

  bslib::page_navbar(
    id = "nav",
    title = "tflplanner",
    # pages scroll: a grid squeezed to fit the window would be 0 px high
    fillable = FALSE,
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    header = shiny::tagList(shiny::tags$style(shiny::HTML(.code_css)),
                            # a function picked on the ARD form: its list closes
                            shiny::tags$script(shiny::HTML(paste(
                              "$(document).on('change', '.ard-fn-list input[type=radio]',",
                              "function() { $(this).closest('details.ard-fn').prop('open', false); });",
                              # the list is made when it is opened
                              "document.addEventListener('toggle', function(e) {",
                              "  if (e.target.matches && e.target.matches('details.ard-fn') && e.target.open)",
                              "    Shiny.setInputValue('ard_fn_opened', Date.now(), {priority: 'event'});",
                              "}, true);"))),
                            shiny::tags$script(shiny::HTML(.split_js)),
                            shiny::tags$style(shiny::HTML(.result_tabs_css)),
                            shiny::tags$style(shiny::HTML(.builder_cards_css)),
                            shiny::tags$script(shiny::HTML(.builder_cards_js)),
                            shiny::tags$style(shiny::HTML(.tpl_css)),
                            shiny::tags$script(shiny::HTML(.tpl_js)),
                            shiny::tags$style(shiny::HTML(.builder_content_css)),
                            shiny::tags$script(shiny::HTML(.builder_content_js)),
                            shiny::tags$style(shiny::HTML(.study_section_css)),
                            shiny::tags$script(shiny::HTML(.study_section_js)),
                            shiny::tags$style(shiny::HTML(.help_tip_css)),
                            shiny::tags$script(shiny::HTML(.help_tip_js)),
                            shiny::tags$script(shiny::HTML(.result_tabs_js)),
                            shiny::tags$script(shiny::HTML(.unsaved_js)),
                            shiny::tags$script(shiny::HTML(.updating_js)),
                            shiny::tags$script(shiny::HTML(.report_pick_js)),
                            shiny::tags$script(shiny::HTML(.dt_adjust_js)),
                            shiny::tags$script(shiny::HTML(.sticky_top_js)),
                            shiny::tags$script(shiny::HTML(.adata_code_js)),
                            shiny::tags$style(shiny::HTML(.review_css)),
                            shiny::tags$script(shiny::HTML(.review_js)),
                            shiny::uiOutput("home_note"),
                            shiny::uiOutput("update_note")),

    bslib::nav_panel(
      t("Study"), value = "study",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        # as tall as its list (not stretched to the settings beside it), as
        # wide as its column
        bslib::card(
          class = "align-self-start w-100", fill = FALSE,
          bslib::card_header(t("Study list")),
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
        # the study chosen: what it is and how it is set, in one card (its
        # keys and setup code only for the study open)
        bslib::card(
          bslib::card_header(shiny::uiOutput("study_detail_title",
                                             inline = TRUE)),
          shiny::uiOutput("spec_card"),
          shiny::uiOutput("study_detail"),
          shiny::conditionalPanel(
            "output.shows_open == 'yes'",
            .study_section("keys", with_tip(paste0(t("Keys and setup code"), " (study)"), t("The subject key (id), where the study ARD goes (output) and the R files of the study's own analysis functions (source). (source is added when a function is made in Own ARD functions.)")),
              rhandsontable::rHandsontableOutput("hot_ard_study"),
              shiny::textAreaInput(updateOn = "blur",
                "setup",
                t("Setup code every report runs first (library(), common data)"),
                rows = 4, width = "100%", resize = "vertical"))),
          shiny::uiOutput("study_actions"))),
      bslib::card(
          bslib::card_header(with_tip(t("Own ARD functions"), t("Your own ARD functions: the company's (every study) and this study's (programs/ard/functions/; it wins over the company's). They are R files, edited outside the app; try one after a change."))),
          shiny::uiOutput("own_counts"),
          shiny::div(class = "mb-2 d-flex gap-2",
                     .btn("own_new", t("New function..."), class = "btn-sm btn-primary"),
                     .btn("own_refresh", t("Read the files again"), class = "btn-sm btn-outline-secondary")),
          DT::DTOutput("own_list", height = "auto", fill = FALSE),
          shiny::uiOutput("own_detail"))),

    # the study's data in one place: on the left what there is (files, ARDs,
    # definitions), on the right the one chosen
    bslib::nav_panel(
      t("Data"), value = "data",
      bslib::navset_pill_list(
        id = "data_nav", widths = c(2, 10), well = FALSE,
        bslib::nav_item(shiny::div(class = "small text-muted fw-bold mt-2 px-2", t("Files and datasets"))),
        bslib::nav_panel(
          t("Datasets"), value = "datasets",
          bslib::layout_columns(
            col_widths = two,
            bslib::card(
              bslib::card_header(with_tip(
                paste0(t("Datasets"), " (datasets)"),
                paste(t("The data files of the folder (data/) and what the study's programs read of them (the data catalog): a name, its level, its file, columns derived. A file put in the folder gets a row; a row with no file is a dataset made on the way. Defined once for the study, used by every report."),
                      t("A row's file may also be outside the study folder, by its full path (C:/adam/adsl.rds)."),
                      edit_note))),
              # the folder you are in: where Add files puts them
              bslib::navset_pill(
                id = "data_pill",
                bslib::nav_panel("ADaM", value = "adam"),
                bslib::nav_panel("SDTM", value = "sdtm"),
                bslib::nav_panel(t("Other"), value = "other")),
              shiny::uiOutput("catalog_missing"),
              rhandsontable::rHandsontableOutput("hot_ard_datasets"),
              shiny::uiOutput("catalog_files_note"),
              shiny::div(
                class = "d-flex gap-2 align-items-end mt-2 rp-upload",
                shiny::fileInput("data_upload", t("Add files"), multiple = TRUE)),
              shiny::div(class = "d-flex gap-2 align-items-center",
                         .btn("data_refresh", t("Refresh")),
                         .btn("data_open", t("Open folder")),
                         shiny::span(class = "small text-muted",
                                     shiny::uiOutput("data_folder_where", inline = TRUE))),
              shiny::tags$details(
                class = "rp-help mt-2",
                shiny::tags$summary(t("Column help")),
                DT::DTOutput("help_ard_datasets")),
              shiny::p(class = "small text-muted mt-2",
                       t("ARDs made elsewhere are in \"ARDs taken in\" (input/ard/)."))),
            bslib::card(
              bslib::card_header(t("Preview")),
              shiny::uiOutput("data_dim"),
              DT::DTOutput("data_head")))),
        bslib::nav_item(shiny::div(class = "small text-muted fw-bold mt-3 px-2", "ARD")),
        bslib::nav_panel(
          shiny::span(t("Study ARD"), shiny::uiOutput("ard_cond_badge", inline = TRUE)),
          value = "state",
          pane_head(t("Study ARD"), paste(t("Build the ARD output by output as each is ready: tables can be made from the outputs already in it, while others are still being defined."), t("Preview (on this report's ARD) puts one report into the working study ARD and keeps no log; the official run, with its logs, is on the Runs tab."))),
          DT::DTOutput("ard_state"),
          shiny::div(class = "rp-code mt-2",
                     shiny::verbatimTextOutput("ard_log")),
          # what went wrong while it was made: an analysis that failed
          # leaves its message in the ARD, the others carry on
          shiny::div(
            class = "d-flex flex-wrap gap-3 align-items-center mt-3",
            shiny::h6(class = "mb-0", with_tip(t("Errors and warnings inside the analyses"), t("From the study ARD as it was last made, and the ARDs taken in. A program that stopped as a whole is in the table above (Error). The messages are cards' own (in English). A click on a row opens that analysis."))),
            shiny::checkboxInput("ard_cond_errors", t("Errors only"), FALSE)),
          DT::DTOutput("ard_conds")),
        bslib::nav_panel(
          t("ARDs taken in"), value = "imports",
          pane_head(t("ARDs taken in"), t("An ARD made elsewhere (a CRO, another program), kept in input/ard/: making the ARD never overwrites it. A report uses it when its report row says so.")),
          shiny::div(class = "mb-2",
                     .btn("imp_new", t("Take in an ARD..."), class = "btn-sm btn-primary")),
          DT::DTOutput("imp_list"),
          shiny::uiOutput("imp_actions")),
        bslib::nav_item(shiny::div(class = "small text-muted fw-bold mt-3 px-2", t("Definitions"))),
        bslib::nav_panel(
          paste0(t("Populations"), " (populations)"), value = "populations",
          pane_head(paste0(t("Populations"), " (populations)"),
                    paste(t("An analysis set: the subjects of a dataset that a condition keeps (SAFFL == \"Y\"). The Data of an analysis, 1-1\'s Population and a percent\'s denominator choose from here."), edit_note)),
          rhandsontable::rHandsontableOutput("hot_ard_populations"),
          shiny::tags$details(
            class = "rp-help mt-2",
            shiny::tags$summary(t("Column help")),
            DT::DTOutput("help_ard_populations"))))),

    # tflspec's AI table requests and their answers (#431): before the Report list, where its reports go
    if (.ai_drafts_on()) bslib::nav_panel(t("AI drafts"), value = "ai", ai_ui("ai", lang)),

    bslib::nav_panel(
      t("Report list"), value = "outputs",
      shiny::uiOutput("next_steps"),
      bslib::card(
        bslib::card_header(with_tip(t("Reports (TFL)"), t("Reports are made in this order (the official run too). Copy makes a new report with all of this one's definition. A double click makes the report (Make a report)."))),
        # the search and the buttons above the list: below 200 reports no
        # one found them
        shiny::div(
          class = "rp-inline",
          shiny::div(style = "flex: 0 1 22rem; min-width: 14rem;",
                     report_search_ui("outputs_q", lang)),
          shiny::div(
            class = "d-flex flex-wrap gap-1 ms-2",
            .btn("add", t("Add")), .btn("copy", t("Copy")),
            .btn("rename", t("Rename")),
            .btn("report_batches", t("Report sets..."),
                  title = t("Report sets are made and chosen on the Runs tab: this opens it")),
            .btn("remove", t("Delete"), class = "btn-sm btn-outline-danger"),
            .btn("up", "\u2191"), .btn("down", "\u2193"),
            .btn("sort_ids", t("Sort by ID"),
                  title = t("Puts the whole list in the order of the numbers in the ids (T-14-1-2 before T-14-1-10)"))),
          .btn("toc_new", t("Take in a TOC..."), class = "btn-sm btn-outline-primary ms-auto")),
        shiny::uiOutput("report_moves"),
        # its own height (a fixed one let a long list cover the buttons)
        DT::DTOutput("outputs", height = "auto", fill = FALSE))),


    # one report at a time: chosen on the left, made in its steps
    bslib::nav_panel(
      t("Make a report"), value = "make",
      bslib::layout_sidebar(
        sidebar = report_side,
        fillable = FALSE,
        report_picker_compact_ui("rp", lang),
        shiny::uiOutput("report_head"),
        shiny::conditionalPanel(
          "output.report_kind == ''",
          shiny::p(class = "text-muted mt-3",
                   t("Choose a report above, or in the list on the left (> opens it)."))),
        # (a report being opened: its steps hidden until drawn -- .tpl_js)
        shiny::div(class = "rp-report-loading text-muted my-3", t("Loading ...")),
        bslib::navset_pill(
          id = "step",
          bslib::nav_panel(step_title("ard"), value = "ard", step_ard),
          bslib::nav_panel(step_title("content"), value = "content", step_content),
          bslib::nav_panel(step_title("page"), value = "page", step_page)))),

    # the review of the whole definition (#288): what cannot be used, what
    # is probably wrong, what to set by hand -- a click goes to it
    bslib::nav_panel(
      t("Review"), value = "review",
      review_ui("review", lang)),

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
          .btn("tfl_open", t("Open output/tfl")),
          shiny::downloadButton("rtf_download", t("Download the RTF"),
                                class = "btn-sm")),
        shiny::uiOutput("job"),
        report_search_ui("status_q", lang),
        # a study of many reports takes seconds to read: said so in the
        # table's place until it is drawn (an empty card looked broken)
        shiny::div(id = "status_loading", class = "small text-muted py-3",
                   shiny::span(class = "spinner-border spinner-border-sm me-2"),
                   shiny::textOutput("status_loading_txt", inline = TRUE)),
        DT::DTOutput("status"),
        shiny::tags$script(shiny::HTML(.status_loading_js))),
      bslib::card(
        bslib::card_header(with_tip(t("Official runs (run folders)"), t("An official run makes a run folder runs/<date>_<time>_<what>/ with each program's log (logrx), what the run made (the study ARD, the RTFs), the R and package versions (session_info.txt) and, if kept, the programs and definition workbooks it ran and the data they read."))),
        shiny::div(
          class = "d-flex flex-wrap gap-3 align-items-center",
          shiny::radioButtons(
            "batch_parts", NULL, inline = TRUE,
            stats::setNames(c("ard", "tfl", "all"),
                            t(c("ARD", "Reports", "ARD, then reports")))),
          shiny::checkboxInput("batch_code",
                               t("keep the code in the run folder"),
                               value = TRUE),
          shiny::checkboxInput("batch_data",
                               with_tip(t("keep the data in the run folder"),
                                        t("The data files the run's programs read (the data catalog's: each table's datasets, a listing's, a figure's, an ARD taken in), copied with their md5 (data_manifest.csv). With the code, the logs and the R and package versions (session_info.txt), the run can be made again.")),
                               value = FALSE),
          .btn("batch_run", t("Start the official run"),
               class = "btn-sm btn-primary"),
          shiny::uiOutput("batch_open_ui", inline = TRUE),
          .btn("batch_compare", t("Compare..."))),
        # the data the run would keep: its size, before the run
        shiny::uiOutput("batch_data_size"),
        # the reports the run takes: all, unless some are left out
        shiny::uiOutput("batch_reports_ui"),
        # as tall as its rows (a fill table left a card of empty space)
        DT::DTOutput("batches", height = "auto", fill = FALSE)),
      # (the definition's check, as the programs read it, is the Review
      # tab's now)
      bslib::card(
        bslib::card_header(t("Log")),
        shiny::div(class = "rp-code", shiny::verbatimTextOutput("log")))),

    bslib::nav_item(shiny::uiOutput("open_study_bar")),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::uiOutput("save_state")),
    bslib::nav_item(shiny::uiOutput("save_btn")),
    bslib::nav_item(shiny::uiOutput("discard_btn")),
    bslib::nav_item(shiny::uiOutput("close_btn")))
}
