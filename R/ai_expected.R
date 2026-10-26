# The person's list of the study's reports, ai/expected_outputs.csv
# (tflspec::tfl_ai_expected_read() / _write()): the AI's list copied the
# first time a row is touched -- not before, so a list nobody changed is
# no file -- then one row changed, dropped (with its reason), added or set
# back at a time, written at once.  run.R compares the reports with it.

.ai_expected_cols <- c("sheet", "toc_id", "output_id", "title", "population", "split",
                       "action", "was", "reason", "note")

# The list as it is, or the AI's list as its first copy (NULL: no study,
# or tflspec cannot say)
.ai_expected_now <- function(dir) {
  e <- tryCatch(tflspec::tfl_ai_expected_read(dir), error = function(e) NULL)
  if (!is.null(e)) return(e)
  d <- .ai_list(dir)
  if (is.null(d)) return(NULL)
  out <- as.data.frame(lapply(stats::setNames(.ai_expected_cols, .ai_expected_cols), function(k)
    if (k %in% names(d)) as.character(d[[k]]) else rep(NA_character_, nrow(d))),
    stringsAsFactors = FALSE)
  out
}

# One change to the list: `what` "change" (fields: title, population,
# split, note -- the values the AI gave said in `was`), "drop" (`reason`,
# required), "add" (a new row: sheet, toc_id, output_id, title,
# population, split, note), "keep" (set back: no action, the note kept).
# Gives the list.
.ai_expected_apply <- function(e, what, id = NA_character_, fields = list()) {
  blank <- function(x) is.null(x) || !length(x) || is.na(x[1L]) || !nzchar(trimws(x[1L]))
  val <- function(k) if (blank(fields[[k]])) NA_character_ else trimws(as.character(fields[[k]])[1L])
  if (identical(what, "add")) {
    row <- as.data.frame(lapply(stats::setNames(.ai_expected_cols, .ai_expected_cols), function(k) val(k)),
                         stringsAsFactors = FALSE)
    row$action <- "add"
    return(rbind(e, row))
  }
  i <- match(id, e$output_id)
  if (is.na(i)) stop(sprintf("No report %s on the list.", id), call. = FALSE)
  if (identical(what, "drop")) {
    if (blank(fields$reason)) stop("Say why the report is not wanted.", call. = FALSE)
    e$action[i] <- "drop"
    e$reason[i] <- val("reason")
  } else if (identical(what, "keep")) {
    e$action[i] <- NA_character_
    e$reason[i] <- NA_character_
    e$was[i] <- NA_character_
  } else if (identical(what, "change")) {
    keys <- intersect(c("title", "population", "split"), names(fields))
    now <- vapply(keys, val, "")
    old <- vapply(keys, function(k) e[[k]][i] %||% NA_character_, "")
    moved <- keys[!mapply(identical, now, old)]
    if (length(moved)) {
      # (what the AI's list said, the first time it is changed)
      if (is.na(e$was[i])) {
        e$was[i] <- paste(sprintf("%s: %s", moved, ifelse(is.na(old[moved]), "", old[moved])), collapse = " | ")
      }
      for (k in moved) e[[k]][i] <- now[[k]]
      if (!e$action[i] %in% "add") e$action[i] <- "change"
    }
    if ("note" %in% names(fields)) e$note[i] <- val("note")
  } else {
    stop("Unknown change: ", what, call. = FALSE)
  }
  e
}

# The change written at once (the list copied first when there is none)
.ai_expected_change <- function(dir, what, id = NA_character_, fields = list()) {
  e <- .ai_expected_now(dir)
  if (is.null(e)) stop("The study has no list of the AI's reports yet.", call. = FALSE)
  e <- .ai_expected_apply(e, what, id, fields)
  tflspec::tfl_ai_expected_write(dir, e, overwrite = TRUE)
  invisible(e)
}
