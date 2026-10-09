# ============================================================================
#  The Review tab (#288 phase 2)
# ----------------------------------------------------------------------------
#  A module: the toolbar (review again, with the data, as the programs read
#  it), the levels with their counts, the area and a search; on the left the
#  reports (the report picker, each with its three counts), on the right the
#  items.  A click on an item goes to it (the app's `jump`): the report
#  chosen, the page, the grid's row flashed.  The import dialog (phase 3)
#  uses the same module on a draft's review.
# ============================================================================

.review_areas <- c("report", "codelists", "ard", "ard_run", "table", "listing",
                   "figure", "data", "program", "spec")
.review_area_words <- c(report = "Report list", codelists = "Code lists",
                        ard = "ARD definition", ard_run = "ARD run",
                        table = "Table", listing = "Listing", figure = "Figure",
                        data = "Data", program = "Programs", spec = "Definition files")
.review_level_words <- c(error = "errors", check = "to check", hand = "to set by hand")
.review_level_class <- c(error = "text-danger", check = "text-warning",
                         hand = "text-secondary")

#' @noRd
review_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  t <- function(x) tr(x, lang)
  shiny::tagList(
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center mb-2",
      .btn(ns("light"), t("Review"), class = "btn-sm btn-outline-primary"),
      .btn(ns("data"), t("Review with the data"), class = "btn-sm btn-primary"),
      .btn(ns("deep"), t("Check as the programs read it"),
           class = "btn-sm btn-outline-secondary"),
      shiny::uiOutput(ns("facts"), inline = TRUE)),
    shiny::div(
      class = "d-flex flex-wrap gap-3 align-items-center mb-2 rp-review-bar",
      shiny::checkboxGroupInput(ns("levels"), NULL, inline = TRUE,
                                choiceNames = unname(t(.review_level_words)),
                                choiceValues = names(.review_level_words),
                                selected = names(.review_level_words)),
      shiny::selectInput(ns("area"), NULL, width = "190px", selectize = FALSE,
                         choices = c(stats::setNames("all", t("Every area")),
                                     stats::setNames(.review_areas,
                                                     t(unname(.review_area_words[.review_areas]))))),
      shiny::textInput(ns("q"), NULL, width = "220px", placeholder = t("Search the items")),
      shiny::uiOutput(ns("off"), inline = TRUE)),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(3, 9)),
      shiny::div(class = "rp-review-reports", report_picker_ui(ns("pick"), lang)),
      shiny::div(
        shiny::uiOutput(ns("empty")),
        DT::DTOutput(ns("table")))))
}

# The module's server.  `review()` the review (a tfl_review in the app's
# language), `rows()` the report list's rows (the picker's); `jump(row)`,
# `apply_fix(row)`, `run_light()`, `run_data()`, `run_deep()` the app's.
# Gives list(set_filter = function(output_id, level)).
#' @noRd
review_server <- function(id, review, rows, jump, apply_fix, run_light, run_data,
                          run_deep, lang = "en") {
  shiny::moduleServer(id, function(input, output, session) {
    t <- function(x) tr(x, lang)
    ns <- session$ns
    sel <- shiny::reactiveVal(".all")
    counts <- shiny::reactive({
      r <- review()
      if (is.null(r) || !nrow(r)) return(NULL)
      .review_counts(r)
    })
    report_picker_server(
      "pick", rows, now = sel, pick = function(v) sel(v),
      fixed = stats::setNames(list(t("(all)"), t("(the study)")), c(".all", ".study")),
      lang = lang, counts = counts)
    # the levels, each with its count
    shiny::observe({
      r <- review()
      n <- vapply(names(.review_level_words), function(l) sum(r$level == l), 1L)
      shiny::updateCheckboxGroupInput(
        session, "levels",
        choiceNames = lapply(names(.review_level_words), function(l) shiny::span(
          shiny::span(class = .review_level_class[[l]], "\u25cf"), " ",
          sprintf("%d %s", n[[l]], t(.review_level_words[[l]])))),
        choiceValues = names(.review_level_words),
        selected = shiny::isolate(input$levels) %||% names(.review_level_words),
        inline = TRUE)
    })
    shown <- shiny::reactive({
      r <- review()
      if (is.null(r) || !nrow(r)) return(r)
      # (none sent yet: every level)
      k <- r$level %in% (input$levels %||% names(.review_level_words))
      s <- sel()
      if (identical(s, ".study")) k <- k & is.na(r$output_id) else
        if (!identical(s, ".all")) k <- k & r$output_id %in% s
      a <- input$area %||% "all"
      if (!identical(a, "all")) k <- k & r$area == a
      q <- tolower(trimws(input$q %||% ""))
      if (nzchar(q)) {
        hay <- tolower(paste(r$output_id, r$sheet, r$row, r$field, r$message, r$hint, r$rule))
        k <- k & grepl(q, hay, fixed = TRUE)
      }
      r[k, , drop = FALSE]
    })
    output$table <- DT::renderDT({
      r <- shown()
      shiny::req(!is.null(r))
      where <- vapply(seq_len(nrow(r)), function(i) {
        p <- c(r$sheet[i], r$row[i], r$field[i])
        paste(p[nzchar(p)], collapse = " \u00b7 ")
      }, "")
      dot <- sprintf('<span class="%s" title="%s">\u25cf</span>',
                     .review_level_class[r$level], htmltools::htmlEscape(t(.review_level_words[r$level])))
      fix <- vapply(seq_len(nrow(r)), function(i) {
        if (is.null(r$fix[[i]])) return("")
        sprintf('<button type="button" class="btn btn-sm btn-outline-primary py-0" onclick="event.stopPropagation(); Shiny.setInputValue(\'%s\', %d, {priority: \'event\'})">%s</button>',
                ns("apply"), i, htmltools::htmlEscape(t("Apply")))
      }, "")
      d <- data.frame(level = dot,
                      report = ifelse(is.na(r$output_id), t("(the study)"), r$output_id),
                      where = htmltools::htmlEscape(where),
                      message = htmltools::htmlEscape(gsub("\n\\s*", " ", r$message)),
                      hint = sprintf('<span class="text-muted small">%s</span>',
                                     htmltools::htmlEscape(r$hint)),
                      fix = fix, stringsAsFactors = FALSE)
      names(d) <- c("", t("Report"), t("Where"), t("Item"), t("Hint"), "")
      DT::datatable(d, rownames = FALSE, escape = FALSE, selection = "single",
                    options = list(dom = "tip", pageLength = 50, ordering = FALSE,
                                   autoWidth = FALSE,
                                   columnDefs = list(
                                     list(width = "1.2em", targets = 0),
                                     list(width = "7em", targets = 1),
                                     list(width = "16%", targets = 2),
                                     list(width = "38%", targets = 3),
                                     list(width = "28%", targets = 4),
                                     list(width = "4em", targets = 5)),
                                   language = list(emptyTable = t("Nothing to review."))))
    }, server = FALSE)
    output$empty <- shiny::renderUI({
      r <- review()
      if (is.null(r)) return(shiny::p(class = "small text-muted", t("Open a study to review it.")))
      if (!nrow(r)) return(shiny::div(class = "alert alert-success py-2 small",
                                       t("Nothing to review: no error, nothing to check, nothing to set by hand.")))
      NULL
    })
    output$facts <- shiny::renderUI({
      r <- review()
      made <- attr(r, "facts_made")
      of <- attr(r, "facts_of") %||% character()
      if (is.null(made) || all(is.na(made)) || !length(of)) {
        return(shiny::span(class = "small text-muted",
                           t("Not reviewed against the data yet: Review with the data reads it once (it is kept).")))
      }
      shiny::span(class = "small text-muted",
                  sprintf(t("Against the data as read %s (%s)"),
                          format(made, "%Y-%m-%d %H:%M"), paste(of, collapse = ", ")))
    })
    output$off <- shiny::renderUI({
      off <- attr(review(), "off") %||% character()
      if (!length(off)) return(NULL)
      shiny::span(class = "small text-muted",
                  sprintf(t("Rules off in the company standards: %s"), paste(off, collapse = ", ")))
    })
    # a click on an item (every click: the same one again goes there again)
    shiny::observeEvent(input$table_cell_clicked, {
      i <- input$table_cell_clicked$row
      r <- shown()
      shiny::req(length(i) == 1L, !is.null(r), i >= 1L, i <= nrow(r))
      jump(r[i, , drop = FALSE])
    })
    shiny::observeEvent(input$apply, {
      r <- shown()
      i <- input$apply
      shiny::req(i <= nrow(r))
      apply_fix(r[i, , drop = FALSE])
    })
    shiny::observeEvent(input$light, run_light())
    shiny::observeEvent(input$data, run_data())
    shiny::observeEvent(input$deep, run_deep())
    list(set_filter = function(output_id, level = NULL) {
      sel(output_id %||% ".all")
      if (!is.null(level)) shiny::updateCheckboxGroupInput(session, "levels", selected = level)
    })
  })
}

# output_id x level counts (the study's rows under ".study")
.review_counts <- function(r) {
  id <- ifelse(is.na(r$output_id), ".study", r$output_id)
  ids <- unique(id)
  out <- data.frame(output_id = ids, stringsAsFactors = FALSE)
  for (l in names(.review_level_words)) {
    out[[l]] <- vapply(ids, function(i) sum(id == i & r$level == l), 1L, USE.NAMES = FALSE)
  }
  out
}

# Where a review's row is in the app: the page (`go`), the tabs to choose,
# the inputs to set (in order), and the grid to flash with the columns that
# make its key
.review_target <- function(r, p) {
  sheet <- r$sheet
  area <- r$area
  key <- r$row
  id <- r$output_id
  keys <- list(variables = "variable", codelists = c("variable", "value"),
               cells = c("variable", "context", "row"),
               digits = c("variable", "statistic"), columns = "column",
               header = "line", footer = "line", titles = "line",
               footnotes = "line", tokens = "name", datasets = "dataset",
               populations = "population_id")
  out <- list(go = NULL, nav = list(), inputs = list(), grid = NULL,
              keycols = character(), key = key, field = r$field, row_index = NA)
  if (sheet %in% setdiff(table_sheets(), "codelists")) {
    out$go <- "tables"
    out$nav <- list(table_right = "spec", table_sheet = sheet)
    out$grid <- paste0("hot_", sheet)
    out$keycols <- keys[[sheet]] %||% character()
  } else if (sheet %in% report_sheets()) {
    out$go <- "report_spec"
    out$nav <- list(page_right = "spec", page_sheet = sheet)
    out$grid <- paste0("hot_", sheet)
    out$keycols <- keys[[sheet]] %||% character()
  } else if (identical(sheet, "codelists")) {
    out$go <- "ard"
    ad <- .adata_rows(p, id)
    v <- sub(" / .*$", "", key)
    if (nrow(ad)) out$inputs <- list(ard_adata_pick = ad$data_id[1L], adata_cl_pick = v)
    out$grid <- "adata_cl_hot"
    out$keycols <- "value"
    out$key <- if (grepl(" / ", key, fixed = TRUE)) sub("^.*? / ", "", key) else ""
  } else if (sheet %in% c("datasets", "populations") || identical(area, "data")) {
    s <- if (sheet %in% c("datasets", "populations")) sheet else "datasets"
    out$nav <- list(nav = "data", data_nav = s)
    out$grid <- paste0("hot_ard_", s)
    out$keycols <- keys[[s]]
  } else if (identical(sheet, "analysis_data")) {
    out$go <- "ard"
    if (nzchar(key)) out$inputs <- list(ard_adata_pick = key)
  } else if (identical(sheet, "analyses")) {
    out$go <- "ard"
    if (nzchar(key)) out$inputs <- list(ard_ol_pick = key)
  } else if (sheet %in% c("listings", "listing_cols")) {
    out$go <- "lf"
    out$nav <- list(lf_right = "spec")
    out$grid <- if (identical(sheet, "listings")) "hot_lf_listings" else "hot_lf_cols"
    out$row_index <- if (identical(sheet, "listing_cols")) suppressWarnings(as.integer(key)) else 1L
  } else if (identical(sheet, "design")) {
    out$go <- "designer"
    m <- regmatches(key, regexec("^([a-z]+)\\[([0-9]+)\\]", key))[[1L]]
    sec <- if (length(m)) m[2L] else if (nzchar(key)) sub(" .*$", "", key) else "plot"
    i <- if (length(m)) as.integer(m[3L]) else 1L
    out$inputs <- list(pd_act = list(op = "sel", sec = sec, i = i))
  } else if (identical(sheet, "_tflplanner")) {
    out$nav <- list(nav = "outputs")
  } else if (identical(area, "program")) {
    out$go <- "report_spec"
  }
  out
}

# A row's place, flashed: the inputs set one after the other (an analysis
# data chosen, then a code list opened), then the grid found -- retried
# while it is drawn -- its row by the key's cells, the field's cell chosen
# and flashed
.review_js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('review-focus', function(x) {
    var inputs = x.inputs || {};
    var names = Object.keys(inputs);
    var k = 0;
    function focus(tries) {
      if (!x.grid) return;
      var el = document.getElementById(x.grid);
      var w = el && window.HTMLWidgets ? HTMLWidgets.find('#' + x.grid) : null;
      var hot = w && w.hot;
      if (!hot || !el.offsetParent) {
        if (tries < 20) setTimeout(function() { focus(tries + 1); }, 200);
        return;
      }
      var hdr = hot.getColHeader();
      var row = -1;
      if (x.row_index) {
        row = x.row_index - 1;
      } else if (x.keycols && x.keycols.length) {
        var parts = (x.key || '').split(' / ');
        var n = hot.countRows();
        for (var r = 0; r < n && row < 0; r++) {
          var ok = true;
          for (var j = 0; j < x.keycols.length; j++) {
            var want = parts[j] === undefined ? '' : parts[j];
            if (want === '') continue;
            var c = hdr.indexOf(x.keycols[j]);
            if (c < 0) continue;
            var have = hot.getDataAtCell(r, c);
            if (String(have === null || have === undefined ? '' : have) !== want) { ok = false; break; }
          }
          if (ok) row = r;
        }
      } else {
        row = 0;
      }
      el.scrollIntoView({block: 'center'});
      if (row < 0) return;
      var col = Math.max(0, hdr.indexOf(x.field));
      hot.selectCell(row, col);
      hot.scrollViewportTo(row, col);
      var td = hot.getCell(row, col);
      if (td) {
        td.classList.remove('rp-flash');
        void td.offsetWidth;
        td.classList.add('rp-flash');
        setTimeout(function() { td.classList.remove('rp-flash'); }, 1600);
      }
    }
    function next() {
      if (k < names.length) {
        var nm = names[k++];
        var v = inputs[nm];
        if (v && typeof v === 'object') v.n = Math.random();
        Shiny.setInputValue(nm, v, {priority: 'event'});
        setTimeout(next, 450);
      } else {
        focus(0);
      }
    }
    next();
  });
});
"

.review_css <- "
@keyframes rp-flash { from { background-color: #ffe58a; } to { background-color: transparent; } }
.rp-flash { animation: rp-flash 1.5s ease-out; }
.rp-review-bar .shiny-input-container { margin-bottom: 0; }
.rp-review-bar .checkbox-inline { margin-right: .8rem; }
"
