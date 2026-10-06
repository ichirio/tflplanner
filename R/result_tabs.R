# The right of each step of making a report: three tabs, SPEC | Code |
# Result -- the step's rows of the definition (edited there, what the form
# on the left cannot say too), the program written from them (to read: a
# program is written, never edited here), what running it makes (to look
# at).  The form on the left, then these: form => SPEC => code => result,
# left to right.  The tabs take what they show as UI: the grids, code
# and previews the app already draws (shiny draws none of a hidden tab,
# so a result is made only while its tab is open).

# not a card: a card around the sheets' own card makes the inner one a fill
# item of a box with no height, and every grid in it 0 px high
#' @noRd
result_tabs_ui <- function(id, spec, code, result, lang = "en", selected = "spec") {
  shiny::div(
    class = "rp-tabs border rounded p-2",
    bslib::navset_underline(
      id = id, selected = selected,
      bslib::nav_panel("SPEC", value = "spec", shiny::div(class = "pt-2", spec)),
      bslib::nav_panel(tr("Code", lang), value = "code", shiny::div(class = "pt-2", code)),
      bslib::nav_panel(tr("Result", lang), value = "result", shiny::div(class = "pt-2", result))))
}

# Code to read: its lines numbered, a button to copy it.  No field to edit:
# a program is written from the definition.
#' @noRd
code_view <- function(output_id, lang = "en") {
  shiny::div(
    class = "rp-code rp-code-view",
    shiny::div(class = "rp-code-tools",
               shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary py-0",
                                  `data-copy` = output_id, tr("Copy", lang))),
    shiny::verbatimTextOutput(output_id))
}

.result_tabs_css <- "
.rp-code-view { position: relative; }
.rp-code-view .rp-code-tools { position: absolute; right: .5rem; top: .35rem; z-index: 2; }
.rp-code-view pre { counter-reset: rp-line; padding-top: 1.8rem; }
.rp-code-view pre .rp-ln { counter-increment: rp-line; display: block; }
.rp-code-view pre .rp-ln::before { content: counter(rp-line); display: inline-block;
  width: 3em; margin-right: .75em; text-align: right; color: #9ca3af;
  -webkit-user-select: none; user-select: none; }
"

# the lines of a code view numbered once its text is in; the copy button
# copies the text without the numbers; a grid drawn while its tab was hidden
# draws again when the tab shows
.result_tabs_js <- "
$(document).on('shiny:value', function(e) {
  var pre = document.getElementById(e.name);
  if (!pre || !$(pre).closest('.rp-code-view').length) return;
  setTimeout(function() {
    var text = pre.textContent;
    pre.dataset.text = text;
    pre.innerHTML = '';
    text.split('\\n').forEach(function(l) {
      var s = document.createElement('span');
      s.className = 'rp-ln';
      s.textContent = l.length ? l : ' ';
      pre.appendChild(s);
    });
  }, 0);
});
$(document).on('shown.bs.tab', '.rp-tabs', function() {
  setTimeout(function() {
    $('.rp-tabs .rhandsontable:visible').each(function() {
      var w = window.HTMLWidgets && HTMLWidgets.find('#' + this.id);
      if (w && w.hot) w.hot.render();
    });
  }, 0);
});
$(document).on('click', '.rp-code-view [data-copy]', function() {
  var pre = document.getElementById($(this).data('copy'));
  if (!pre) return;
  var text = pre.dataset.text || pre.textContent;
  var b = $(this), was = b.text();
  var done = function() { b.text('\\u2713'); setTimeout(function() { b.text(was); }, 1200); };
  if (navigator.clipboard) navigator.clipboard.writeText(text).then(done, function() {});
});
"
