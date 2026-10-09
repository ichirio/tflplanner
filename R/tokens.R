# The study's tokens: words every report's header, footer, titles and
# footnotes may say ({COMPANY}, {ANALYSIS_TYPE}, {STUDY_ID} ...), kept as the
# tokens sheet's study rows (a blank output_id) and written once for every
# report (programs/tfl/report_setup.R).  A report's own ({OUTPUT_TITLE} ...)
# are its rows of the same sheet.

# the study's words the study tab sets, in the order it shows them
.study_token_names <- c("COMPANY", "ANALYSIS_TYPE", "STUDY_ID")

# A study token's value (NA: none)
study_token <- function(x, name) {
  d <- x$sheets$tokens
  if (is.null(d) || !nrow(d)) return(NA_character_)
  i <- which(is.na(d$output_id) & trimws(d$name) == name)
  if (length(i)) d$value[i[1L]] else NA_character_
}

# The study's font and size: the page sheet's study row (a blank
# output_id), written once for every report as options() in
# programs/tfl/report_setup.R (tflspec::tfl_report_setup_code()).  NA: none
# (rtfreporter's own, Courier 9 pt).
study_page_value <- function(x, col) page_value(x, col)

# Set one of them (NA or "": none); a study row left with nothing goes
set_study_page_value <- function(x, col, value) set_page_value(x, col, value)

# A value of the page sheet: the study's row (`output_id` NA) or a
# report's own (the report's row: step 4's font and size).  NA: none.
page_value <- function(x, col, output_id = NA_character_) {
  d <- x$sheets$page
  if (is.null(d) || !nrow(d) || is.null(d[[col]])) return(NA_character_)
  i <- .page_row(d, output_id)
  if (length(i)) d[[col]][i[1L]] else NA_character_
}

# Set one (NA or "": none): only that cell changes; a row made for it
# (the study's first, a report's last), a row left with nothing goes
set_page_value <- function(x, col, value, output_id = NA_character_) {
  d <- .normalize_sheet(x$sheets$page, "page")
  if (is.na(value) || !nzchar(trimws(value))) value <- NA_character_
  i <- .page_row(d, output_id)
  if (!length(i)) {
    if (is.na(value)) return(x)
    row <- d[0L, , drop = FALSE]
    row[1L, ] <- NA
    row$output_id <- output_id
    study <- is.na(output_id)
    d <- if (study) rbind(row, d) else rbind(d, row)
    i <- if (study) 1L else nrow(d)
  }
  d[[col]][i[1L]] <- value
  rest <- setdiff(names(d), "output_id")
  if (all(is.na(unlist(d[i[1L], rest])))) d <- d[-i[1L], , drop = FALSE]
  rownames(d) <- NULL
  x$sheets$page <- d
  x
}

.page_row <- function(d, output_id) {
  if (is.na(output_id)) which(is.na(d$output_id)) else
    which(!is.na(d$output_id) & d$output_id == output_id)
}

# points (9, 10.5) as half-points ("18", "21"); NA when not a size
.half_points <- function(pt) {
  v <- suppressWarnings(as.numeric(pt))
  if (length(v) != 1L || is.na(v) || v <= 0 || v > 72) return(NA_character_)
  as.character(as.integer(round(v * 2)))
}

# half-points as points ("18" -> "9", "21" -> "10.5"); "" for none
.points <- function(hp) {
  v <- suppressWarnings(as.numeric(hp))
  if (length(v) != 1L || is.na(v)) return("")
  format(v / 2)
}

# Set a study token (NA or "": no row); only that row changes
set_study_token <- function(x, name, value) {
  d <- x$sheets$tokens
  i <- which(is.na(d$output_id) & trimws(d$name) == name)
  if (is.na(value) || !nzchar(trimws(value))) {
    if (length(i)) d <- d[-i, , drop = FALSE]
  } else if (length(i)) {
    d$value[i[1L]] <- value
  } else {
    # after the study's other rows, before the reports'
    at <- sum(is.na(d$output_id))
    row <- d[0L, , drop = FALSE]
    row[1L, ] <- NA
    row$name <- name
    row$value <- value
    d <- rbind(d[seq_len(at), , drop = FALSE], row,
               d[setdiff(seq_len(nrow(d)), seq_len(at)), , drop = FALSE])
  }
  rownames(d) <- NULL
  x$sheets$tokens <- d
  x
}
