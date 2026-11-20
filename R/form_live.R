# A form whose fields are applied as they are edited.  It has no Save of
# its own: an edit goes into the study in memory at once (the top-right
# indicator then says "Unsaved changes") and only the top-right Save writes
# the files -- as the template fields do.  Under the form, "Undo" puts back
# what the form wrote since it was opened.
#
# .form_live() keeps one form's state:
# - open(p): the form drawn, with `p` the study as it was.  The values the
#   page sends back first are the drawing's, not an edit: ready() is FALSE
#   until `settle` seconds after the drawing was sent
#   (session$userData$form_settle overrides it; 0 in the tests: ready once
#   the drawing is flushed).
# - wait(): the same form drawn again: its values coming back are not an
#   edit either (ready() FALSE again for `settle` seconds).
# - wrote(p): a write of the form's, `p` the study after it.
# - changed(): the form has written since it was opened.
# - rebase(p): the study saved, or its changes discarded: Undo goes back to
#   `p` from now on (what the form wrote before is no longer its to undo).
# - revert(now): `now` with what the form's writes changed put back
#   (.form_restore()).
.form_live <- function(session, settle = 1.5) {
  e <- new.env(parent = emptyenv())
  e$open <- function(p) {
    e$p0 <- p
    e$mine <- NULL
    e$wait()
  }
  e$wait <- function() {
    e$s <- session$userData$form_settle %||% settle
    e$t0 <- Inf
    session$onFlushed(function() e$t0 <- as.numeric(Sys.time()), once = TRUE)
    invisible()
  }
  e$ready <- function() !is.null(e$t0) && as.numeric(Sys.time()) - e$t0 >= e$s
  e$wrote <- function(p) e$mine <- p
  e$changed <- function() !is.null(e$mine)
  e$rebase <- function(p) {
    if (!is.null(e$p0)) {
      e$p0 <- p
      e$mine <- NULL
    }
    invisible()
  }
  e$revert <- function(now) .form_restore(now, e$p0, e$mine)
  e$close <- function() e$p0 <- e$mine <- e$t0 <- NULL
  e
}

# The study `now` with the parts the form changed (from `p0`, as it was
# opened, to `mine`, its last write) put back as they were in `p0`: a part
# is an element of the study (`ard`, `spec` ...) or one of its elements
# (`ard$analyses`), so what else changed meanwhile stays.
.form_restore <- function(now, p0, mine) {
  if (is.null(mine) || is.null(p0)) return(now)
  is_list <- function(x) is.list(x) && !is.data.frame(x)
  for (a in union(names(p0), names(mine))) {
    x0 <- p0[[a]]
    x1 <- mine[[a]]
    if (identical(x0, x1)) next
    if (is_list(x0) && is_list(x1)) {
      for (b in union(names(x0), names(x1))) {
        if (identical(x0[[b]], x1[[b]])) next
        if (is.null(x0[[b]])) now[[a]][[b]] <- NULL else now[[a]][[b]] <- x0[[b]]
      }
    } else if (is.null(x0)) {
      now[[a]] <- NULL
    } else {
      now[[a]] <- x0
    }
  }
  now
}
