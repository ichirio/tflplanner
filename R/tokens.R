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
