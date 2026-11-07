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
result_tabs_ui <- function(id, spec, code, result, lang = "en", selected = "spec", ard = NULL,
                           ard_code = NULL) {
  # a template report's tabs are named for what they are (Template SPEC,
  # Report code, Report result), shown while body.rp-tpl; a code report's
  # keep SPEC | Code | Result
  lab <- function(plain, tpl) if (is.null(ard)) plain else shiny::tagList(
    shiny::span(class = "rp-lab-plain", plain), shiny::span(class = "rp-lab-tpl", tr(tpl, lang)))
  shiny::div(
    class = paste("rp-tabs border rounded p-2", if (!is.null(ard)) "rp-tabs-ard"),
    bslib::navset_underline(
      id = id, selected = selected,
      bslib::nav_panel(lab("SPEC", "Template SPEC"), value = "spec", shiny::div(class = "pt-2", spec)),
      # (a template report's ARD is made in steps of its own, code => result,
      # after its SPEC and before the report's -- tabs shown only for such a report)
      if (!is.null(ard_code)) bslib::nav_panel(tr("ARD code", lang), value = "ard_code", shiny::div(class = "pt-2", ard_code)),
      if (!is.null(ard)) bslib::nav_panel(tr("ARD result", lang), value = "ard", shiny::div(class = "pt-2", ard)),
      bslib::nav_panel(lab(tr("Code", lang), "Report code"), value = "code", shiny::div(class = "pt-2", code)),
      bslib::nav_panel(lab(tr("Result", lang), "Report result"), value = "result", shiny::div(class = "pt-2", result))))
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
@media (min-width: 992px) {
  /* the page scrolls, not the sidebar layout's main: its overflow (auto)
     would make it the box a sticky pane sticks in, and it never scrolls */
  .bslib-sidebar-layout > .main { overflow-x: clip; overflow-y: visible; }
  /* beside the form, SPEC | Code | Result stays in view as the form scrolls:
     the table as it prints beside the row being changed */
  .rp-tabs { position: sticky; top: calc(var(--rp-top, 0px) + .5rem); align-self: start;
    max-height: calc(100vh - var(--rp-top, 0px) - 1rem); overflow-y: auto; }
  .rp-split.rp-lay-stack > .rp-tabs, .rp-split.rp-lay-one > .rp-tabs {
    position: static; max-height: none; overflow-y: visible; }
}
/* the two ARD tabs belong to a template report (body.rp-tpl): shown for
   it, greyed out and not selectable (.disabled) when it makes no ARD; a
   code report has neither.  Five short tabs in one row */
body:not(.rp-tpl) :is(#table_right, #fig_right, #lf_right) > .nav-item:has(> a[data-value^=\"ard\"]) { display: none; }
body:not(.rp-tpl) :is(#table_right, #fig_right, #lf_right) .rp-lab-tpl,
body.rp-tpl :is(#table_right, #fig_right, #lf_right) .rp-lab-plain { display: none; }
:is(#table_right, #fig_right, #lf_right) { flex-wrap: nowrap; white-space: nowrap; }
:is(#table_right, #fig_right, #lf_right) .nav-link.disabled { opacity: .4; }
.rp-tpl-right { min-width: 0; max-width: 100%; }
/* a template figure's and listing's pane: Update preview above the tabs,
   both in view as the card beside them scrolls */
@media (min-width: 992px) {
  .rp-tpl-right { position: sticky; top: calc(var(--rp-top, 0px) + .5rem); align-self: start; }
  .rp-tpl-right > .rp-tabs { position: static; max-height: calc(100vh - var(--rp-top, 0px) - 4rem); }
}
/* a template listing's five tabs need the table's 7 of 12 */
@media (min-width: 992px) {
  body.rp-tpl .rp-lf-split > :nth-child(1) { grid-column: span 5 !important; }
  body.rp-tpl .rp-lf-split > :nth-child(2) { grid-column: span 7 !important; }
}
/* the ARD result's wide grid must not widen the pane past its column */
.rp-tabs-ard, .rp-tabs-ard .tabbable,
.rp-tabs-ard .tab-content, .rp-tabs-ard .tab-pane { min-width: 0; max-width: 100%; }
:is(#table_right, #fig_right, #lf_right) .nav-link { padding-left: .6rem; padding-right: .6rem; }
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
// the first page (step 3): fit to the pane's width by default; actual
// size (100%) when asked, remembered in this browser
function rpPageFitOn() {
  try { return window.localStorage.getItem('rp-page-fit') !== '0'; } catch (e) { return true; }
}
function rpPageFitApply() {
  var on = rpPageFitOn();
  $('.rp-page-wrap').each(function() {
    var page = $(this).find('.rp-page')[0];
    if (!page) return;
    page.style.zoom = 1;
    if (on && page.scrollWidth > 0) {
      page.style.zoom = Math.min(1, this.clientWidth / page.scrollWidth);
    }
  });
  $('#page_fit').toggleClass('active', on);
}
function rpPageFit(on) {
  try { window.localStorage.setItem('rp-page-fit', on ? '1' : '0'); } catch (e) {}
  rpPageFitApply();
}
$(document).on('shiny:value', function(e) {
  if (e.name === 'page_sample') setTimeout(rpPageFitApply, 0);
});
$(window).on('resize', function() { rpPageFitApply(); });
$(document).on('shown.bs.tab', '.rp-tabs', function() { setTimeout(rpPageFitApply, 0); });
$(document).on('click', '.rp-code-view [data-copy]', function() {
  var pre = document.getElementById($(this).data('copy'));
  if (!pre) return;
  var text = pre.dataset.text || pre.textContent;
  var b = $(this), was = b.text();
  var done = function() { b.text('\\u2713'); setTimeout(function() { b.text(was); }, 1200); };
  if (navigator.clipboard) navigator.clipboard.writeText(text).then(done, function() {});
});
"
