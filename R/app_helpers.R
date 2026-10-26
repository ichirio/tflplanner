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
# a DataTable drawn while its tab was hidden measured its columns 0 px wide
# (headers one letter a line): when a tab, pill or sidebar shows, the tables
# in sight measure again
# the fields of a data greyed while its code (R) is written
.adata_code_js <- "
$(document).on('input change', '#adata_code', function() {
  $('#adata_gui').toggleClass('rp-greyed', $(this).val().trim() !== '');
});
"

# the runs table's "Reading ..." line: shown while the table is (re)made,
# gone once it is drawn
.status_loading_js <- "
$(document).on('shiny:recalculating', function(e) {
  if (e.target && e.target.id === 'status') $('#status_loading').show();
});
$(document).on('shiny:value shiny:error', function(e) {
  if (e.name === 'status') $('#status_loading').hide();
});
"

# A table drawn while hidden has its header columns out of line: fitted
# again when it shows.  Only the tables inside what was shown (a tab, a
# folded part opened, an output drawn), once for all that showed at the same
# time: fitting every table of the app on each of these (a form opens with
# several folded parts, each a toggle) measured thousands of cells and took
# seconds.
# The top bar's height as --rp-top on the page (it wraps on a narrow
# window): the sticky panes start under it
.sticky_top_js <- "
(function() {
  function put() {
    var n = document.querySelector('nav.navbar.navbar-static-top');
    if (!n) return;
    document.documentElement.style.setProperty('--rp-top', n.offsetHeight + 'px');
  }
  $(function() {
    put();
    var n = document.querySelector('nav.navbar.navbar-static-top');
    if (n && window.ResizeObserver) new ResizeObserver(put).observe(n);
    $(window).on('resize', put);
  });
})();
"

# Update preview's (i): what it does, and what it does not (#428)
.preview_tip <- "Makes this report's ARD in an R of its own, only when it is needed. The study's ARD (output/ard/ard.rds) is not changed."

.dt_adjust_js <- "
(function() {
  var roots = [], timer = null;
  function fit() {
    timer = null;
    var rs = roots; roots = [];
    if (!(window.jQuery && jQuery.fn.dataTable)) return;
    var all = rs.indexOf(document) >= 0;
    jQuery.fn.dataTable.tables({visible: true}).forEach(function(tb) {
      if (all || rs.some(function(r) { return r.contains(tb); })) {
        jQuery(tb).DataTable().columns.adjust();
      }
    });
  }
  function adjust(root) {
    if (!root) return;
    roots.push(root);
    if (!timer) timer = setTimeout(fit, 0);
  }
  function pane(e) {
    var sel = e.target && e.target.getAttribute &&
      (e.target.getAttribute('data-bs-target') || e.target.getAttribute('href'));
    var p = sel && sel.charAt(0) === '#' ? document.querySelector(sel) : null;
    return p || document;
  }
  document.addEventListener('shown.bs.tab', function(e) { adjust(pane(e)); });
  document.addEventListener('shown.bs.collapse', function(e) { adjust(e.target); });
  document.addEventListener('toggle', function(e) {
    if (e.target && e.target.open) adjust(e.target);
  }, true);
  if (window.jQuery) jQuery(document).on('shiny:visualchange', function(e) { adjust(e.target); });
})();
"

# The Runs tab's list of reports of a run (or a report set): the filter hides
# the reports whose ID or description does not hold the words typed; "Tick
# the shown" / "Untick the shown" then tick or untick just those, so a set is
# made from the reports a filter picks.
.batch_filter_js <- "
function rpBatchBoxes() {
  return Array.prototype.slice.call(document.querySelectorAll('#batch_reports .checkbox'));
}
function rpBatchFilter() {
  var q = (document.getElementById('batch_q').value || '').toLowerCase().split(/ +/).filter(Boolean);
  rpBatchBoxes().forEach(function(b) {
    var s = (b.textContent || '').toLowerCase();
    b.style.display = q.every(function(w) { return s.indexOf(w) >= 0; }) ? '' : 'none';
  });
}
function rpBatchShown(mode) {
  var ids = rpBatchBoxes().filter(function(b) { return b.style.display !== 'none'; })
    .map(function(b) { return b.querySelector('input').value; });
  Shiny.setInputValue('batch_shown', {mode: mode, ids: ids, n: Math.random()}, {priority: 'event'});
}
"

# A report's own select (its population, its page pattern, the data its
# code reads) is drawn again for each report, and sends a value when it is
# drawn too.  That value can reach the server after another report is
# chosen, and was written to that one (#418).  So the select is drawn in
# .for_report(), and only the user's change is written: it comes as
# <id>_by = {report, value}, the report the select was drawn for, and is
# written only when that report is the one chosen now (.report_pick()).
.report_pick_js <- "
$(document).on('change', '[data-tfl-report] select', function() {
  var w = $(this).closest('[data-tfl-report]');
  var v = $(this).val();
  if (v === null || v === undefined) v = [];
  Shiny.setInputValue(this.id + '_by',
    {report: w.attr('data-tfl-report'), value: v, n: Math.random()},
    {priority: 'event'});
});
"
.for_report <- function(tag, id) shiny::div(`data-tfl-report` = id, tag)
# the value of a change sent for `id`: NULL when it was sent for another
# report (or none); character(0) when nothing is chosen
.report_pick <- function(ev, id) {
  if (!is.list(ev) || is.null(id) || !identical(ev$report, id)) return(NULL)
  as.character(unlist(ev$value))
}

.updating_js <- "
// The veil (the page's busy state, CSS rp-user-busy) only for work the
// user started: a busy state that begins within 1.5 s of a click, a key or
// a changed field.  A run in the background followed by the poll is not.
// Editing a field (text, text area, select) and Save do not veil either: the
// page stays as it is while the server takes the value in and saves (#51).
(function() {
  var last = 0;
  var quiet = 'input:not([type=button]):not([type=submit]):not([type=radio]):not([type=checkbox]), textarea, select, .selectize-control, .form-control, #save';
  ['mousedown', 'keydown', 'change'].forEach(function(ev) {
    document.addEventListener(ev, function(e) {
      if (e.target && e.target.closest && e.target.closest(quiet)) return;
      last = Date.now();
    }, true);
  });
  $(document).on('shiny:busy', function() {
    // (a template report: only its right pane shows it is busy, #440)
    var b = document.body.classList;
    if (b.contains('rp-tpl') && !b.contains('rp-switching')) return;
    if (Date.now() - last < 1500) document.documentElement.classList.add('rp-user-busy');
  });
  $(document).on('shiny:idle', function() {
    document.documentElement.classList.remove('rp-user-busy');
  });
})();
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
// The builder's form is drawn again after an edit that changes its
// shape (a heading or the order in the code list of variable, a row of
// one's own): the page keeps its place instead of jumping to the top.
(function() {
  var y = null;
  $(document).on('shiny:outputinvalidated', function(e) {
    if (e.name === 'builder_form') y = window.scrollY;
  });
  $(document).on('shiny:value', function(e) {
    if (e.name !== 'builder_form' || y === null) return;
    var at = y;
    y = null;
    setTimeout(function() { window.scrollTo(0, at); }, 0);
    setTimeout(function() { window.scrollTo(0, at); }, 150);
  });
})();
// Outputs shown just now (a part drawn, a panel opened): shiny resumes a
// hidden output after the update that shows it and schedules none, so it
// waited for some unrelated timer (seconds).  A word to the server once
// they are bound starts that update.
(function() {
  var kick = null;
  $(document).on('shiny:bound', function(e) {
    if (e.bindingType !== 'output') return;
    clearTimeout(kick);
    kick = setTimeout(function() {
      Shiny.setInputValue('rp_kick', Date.now(), {priority: 'event'});
    }, 30);
  });
})();
// An analysis opened in step 1 (a click, a new one, a copy): its form, below
// the list and often below the window's edge, is brought into view -- when
// another analysis is drawn, not when the same one is drawn again
(function() {
  var last = null;
  $(document).on('shiny:value', function(e) {
    if (e.name !== 'ard_stat_ui') return;
    setTimeout(function() {
      var out = document.getElementById('ard_stat_ui');
      var h = out && out.querySelector('[data-an]');
      var id = h ? h.getAttribute('data-an') : null;
      if (!id || id === last) { last = id; return; }
      last = id;
      var el = out;
      // an output with no box of its own (display: contents): its frame
      while (el && !el.getClientRects().length) el = el.parentElement;
      if (!el || !el.offsetParent) return;
      var top = el.getBoundingClientRect().top;
      if (top < 0 || top > window.innerHeight * 0.55) {
        el.scrollIntoView({behavior: 'smooth', block: 'start'});
      }
    }, 80);
  });
})();
// The top tabs the chosen report's kind has nothing on: faded
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('rp-idle-tabs', function(x) {
    $('#nav a[data-value], #step a[data-value]').each(function() {
      $(this).toggleClass('rp-idle', x.indexOf($(this).attr('data-value')) >= 0);
    });
  });
  // the ARD definition of the chosen report: hidden for a figure that has
  // none of its own (it reads none, or a table's)
  // the steps' names, the kind of report's own (a figure's 1 Data, 2 Figure)
  Shiny.addCustomMessageHandler('rp-step-names', function(x) {
    $('.rp-step-name').each(function() {
      var k = $(this).attr('data-step');
      if (x[k]) $(this).text(x[k]);
    });
  });
  Shiny.addCustomMessageHandler('rp-ard-def', function(x) {
    // (a class, not display: the row is d-flex !important)
    $('#ard_split').toggleClass('d-none', !x);
    $('#ard_layout').parent().toggleClass('d-none', !x);
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
  // RStudio's Shiny window and Viewer show no dialog: they only refuse to
  // close.  There, nothing is held (the Close button asks instead).
  var inRStudio = /RStudio/i.test(navigator.userAgent);
  window.addEventListener('beforeunload', function(e) {
    if (dirty && !inRStudio) { e.preventDefault(); e.returnValue = ''; }
  });
  // the app has stopped (Close): say so, and close the window if allowed
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('tflplanner-closed', function(x) {
      dirty = false;
      var d = document.createElement('div');
      d.style.padding = '3rem';
      d.style.fontSize = '1.1rem';
      d.textContent = x;
      document.body.innerHTML = '';
      document.body.appendChild(d);
      window.close();
    });
  });
})();
"

.grid_height <- function(n) {
  as.integer(min(max(28 + 23 * (n + 1L) + 20, 100), 420))
}

# A sheet as an editable grid.  `choices` offers values in a dropdown;
# the columns named in `closed` accept nothing else (a typo is refused in
# the cell rather than saved), the others take free text too.
# The catalog's datasets as choices ("ADSL (ADaM)"), and any of `have` it
# does not list, as they are: a value the choices lack is not replaced
.ds_choices <- function(cat_ds, have = character()) {
  ch <- if (length(cat_ds$dataset)) stats::setNames(
    cat_ds$dataset, paste0(cat_ds$dataset, " (", cat_ds$level, ")")) else character()
  gone <- setdiff(have[!is.na(have) & nzchar(have)], ch)
  c(ch, stats::setNames(gone, gone))
}

.grid <- function(d, sheet, key, choices, closed = character(),
                  select = FALSE, read_only = character()) {
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
  # (columns kept as they are: shown, not typed into)
  for (cn in intersect(read_only, names(d))) {
    h <- suppressWarnings(rhandsontable::hot_col(h, cn, readOnly = TRUE))
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
# datasets, populations and analysis data).  `method` stays open: besides
# the keywords it takes any pkg::function.
.ard_closed_columns <- c("data", "dataset", "population_id", "denominator",
                         "from", "subjects")

# tflspec's column help is English; the app shows it in its language
.help_table <- function(sheet) {
  rd <- tflspec::tfl_spec_columns(sheet)
  rd$sheet <- NULL
  rd$description <- tr(rd$description)
  rd
}

# A folder opened in the system's file manager (Explorer, Finder,
# xdg-open): the app runs on this computer (the button is not shown on a
# server)
.open_folder <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # (the tests put their own opener here: no window)
  opener <- getOption("tflplanner.open_folder", .external_browser)
  opener(normalizePath(path))
  invisible(TRUE)
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
