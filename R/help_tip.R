# A heading's help, out of the way: an (i) beside the heading whose text
# shows on hover or keyboard focus (a tooltip), instead of a paragraph on
# the page.  Long explanations made the screens hard to read; what is
# left on the page is the label, a warning, an error.  One component for
# every tab (2-1 and 2-2 use it too).
#
#   help_tip(text)              the (i) alone
#   with_tip(label, text)       a heading (text or tags) and its (i)
#
# The (i) is a focusable span (tabindex 0) named by the help itself
# (aria-label), so a screen reader reads it; bslib's tooltip gives the
# bubble and its aria-describedby.  It shows on hover and focus, and a
# click or a tap pins it open (a tablet has no hover); another click on
# it, a click elsewhere or Escape closes it (.help_tip_js, for 2-x's
# .rp-hint too).
#
#   pane_head(title, text)      a part's small heading and its (i)

#' @noRd
help_tip <- function(text, placement = "auto") {
  if (is.null(text) || !length(text) || !nzchar(paste(text, collapse = ""))) return(NULL)
  bslib::tooltip(
    shiny::tags$span(class = "rp-tip", tabindex = "0", role = "img",
                     `aria-label` = paste(text, collapse = " "), "\u24d8"),
    text, placement = placement)
}

#' @noRd
with_tip <- function(label, text, placement = "auto") {
  shiny::tags$span(class = "rp-with-tip", label, " ", help_tip(text, placement))
}

# a part's small heading with its help, where the page had a paragraph
# (and, for a sheet, how the sheet is edited, in the same (i))
#' @noRd
pane_head <- function(title, text, class = "mb-1") {
  shiny::h6(class = class, with_tip(title, text))
}

.help_tip_js <- "
(function() {
  var SEL = '.rp-tip, .rp-hint';
  function inst(el) { return window.bootstrap && bootstrap.Tooltip.getInstance(el); }
  function unpin(el) {
    el.classList.remove('rp-pinned');
    var t = inst(el);
    if (t) t.hide();
  }
  function toggle(el) {
    var t = inst(el);
    if (!t) return;
    if (el.classList.contains('rp-pinned')) { unpin(el); return; }
    document.querySelectorAll('.rp-pinned').forEach(unpin);
    el.classList.add('rp-pinned');
    t.show();
  }
  // a pinned tip stays when the pointer leaves it
  document.addEventListener('hide.bs.tooltip', function(e) {
    if (e.target.classList && e.target.classList.contains('rp-pinned')) e.preventDefault();
  }, true);
  $(document).on('click', SEL, function(e) {
    // in a label: not a click on its checkbox
    e.preventDefault(); e.stopPropagation();
    toggle(this);
  });
  $(document).on('keydown', SEL, function(e) {
    if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); toggle(this); }
    if (e.key === 'Escape') unpin(this);
  });
  $(document).on('click', function(e) {
    if (!$(e.target).closest('.tooltip').length) document.querySelectorAll('.rp-pinned').forEach(unpin);
  });
  $(document).on('keydown', function(e) {
    if (e.key === 'Escape') document.querySelectorAll('.rp-pinned').forEach(unpin);
  });
})();
"

.help_tip_css <- "
.rp-tip { color: var(--bs-secondary-color, #6b7280); cursor: help; font-size: .85em;
  font-weight: normal; margin-left: .15rem; }
.rp-tip.rp-pinned { color: var(--bs-primary, #0d6efd); }
.rp-tip:hover, .rp-tip:focus { color: var(--bs-primary, #0d6efd); outline: none; }
.tooltip-inner { max-width: 26rem; text-align: left; }
"

# a screen's or a part's help where it has no heading: "About this (i)"
#' @noRd
about_tip <- function(text, lang = "en") {
  shiny::div(class = "small text-muted mb-1", with_tip(tr("About this", lang), text))
}
