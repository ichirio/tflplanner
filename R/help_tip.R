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
# bubble and its aria-describedby.

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

.help_tip_css <- "
.rp-tip { color: var(--bs-secondary-color, #6b7280); cursor: help; font-size: .85em;
  font-weight: normal; margin-left: .15rem; }
.rp-tip:hover, .rp-tip:focus { color: var(--bs-primary, #0d6efd); outline: none; }
.tooltip-inner { max-width: 26rem; text-align: left; }
"

# a screen's or a part's help where it has no heading: "About this (i)"
#' @noRd
about_tip <- function(text, lang = "en") {
  shiny::div(class = "small text-muted mb-1", with_tip(tr("About this", lang), text))
}
