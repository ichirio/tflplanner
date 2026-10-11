# Page patterns (the page-patterns design, approved 2026-10-11): a study
# keeps a few named sets of the report half's defaults -- Standard (the
# blank rows) and, say, Compact for the PK tables -- and each report
# chooses one and changes only what differs.  tflspec reads them: rows of
# report, page, header, footer, titles, footnotes and tokens whose
# output_id is "@<name>", Standard's changes; report$pattern chooses one.
# A report's own rows go over its pattern's, its pattern's over
# Standard's.  Here: the patterns of a planner, a report's choice, and what
# step 3's form shows -- each value with where it comes from.

.pattern_sheets <- c("report", "page", "header", "footer", "titles",
                     "footnotes", "tokens")
.pattern_rx <- "^[A-Za-z][A-Za-z0-9_]*$"
.is_pattern_id <- function(id) !is.na(id) & startsWith(id, "@")

#' Page patterns
#'
#' A study's named page patterns: sets of the report half's defaults
#' (report, page, header, footer, titles, footnotes, tokens) a report
#' chooses from.  Standard is the blank rows; another pattern -- the rows
#' whose `output_id` is `@` and its name -- says only what differs from
#' Standard.  `report_pattern()` is a report's choice (`NA`: Standard),
#' `set_report_pattern()` sets it: the report's own rows stay over it.
#'
#' @param x An `tflplanner`.
#' @param output_id A report.
#' @param pattern A pattern's name, or `NA` / `"Standard"` for Standard.
#' @return `page_patterns()`: the names, in alphabetical order (Standard
#'   not among them).  `report_pattern()`: one name, or `NA`.
#'   `set_report_pattern()`: `x`.
#' @export
page_patterns <- function(x) {
  ids <- unlist(lapply(x$sheets[.pattern_sheets], `[[`, "output_id"), use.names = FALSE)
  sort(unique(substring(ids[.is_pattern_id(ids)], 2L)))
}

#' @rdname page_patterns
#' @export
report_pattern <- function(x, output_id) {
  d <- x$sheets$report
  if (is.null(d$pattern)) return(NA_character_)
  v <- d$pattern[!is.na(d$output_id) & d$output_id == output_id]
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (length(v)) sub("^@", "", trimws(v[1L])) else NA_character_
}

#' @rdname page_patterns
#' @export
set_report_pattern <- function(x, output_id, pattern) {
  if (!is.na(pattern) && identical(pattern, "Standard")) pattern <- NA_character_
  if (!is.na(pattern) && !pattern %in% page_patterns(x)) {
    stop("No page pattern '", pattern, "'.", call. = FALSE)
  }
  .set_row_value(x, "report", "pattern", pattern, output_id)
}

# One cell of a one-row sheet (report, page): the row of `output_id` (NA:
# Standard's, "@name": a pattern's).  NA or "": none -- the row is made
# for a value and goes when it says nothing.
.set_row_value <- function(x, sheet, col, value, output_id) {
  d <- .normalize_sheet(x$sheets[[sheet]], sheet)
  if (is.na(value) || !nzchar(trimws(value))) value <- NA_character_
  i <- if (is.na(output_id)) which(is.na(d$output_id)) else
    which(!is.na(d$output_id) & d$output_id == output_id)
  if (!length(i)) {
    if (is.na(value)) return(x)
    row <- d[0L, , drop = FALSE]
    row[1L, ] <- NA
    row$output_id <- output_id
    d <- if (is.na(output_id)) rbind(row, d) else rbind(d, row)
    i <- if (is.na(output_id)) 1L else nrow(d)
  }
  d[[col]][i[1L]] <- value
  rest <- setdiff(names(d), "output_id")
  # (a report's row of the report sheet is its kind's: kept even when empty)
  if (!identical(sheet, "report") && all(is.na(unlist(d[i[1L], rest])))) {
    d <- d[-i[1L], , drop = FALSE]
  }
  rownames(d) <- NULL
  x$sheets[[sheet]] <- d
  x
}

# The rows of one output_id: a report's, a pattern's ("@name"), Standard's (NA)
.is_id <- function(ids, id) if (is.na(id)) is.na(ids) else !is.na(ids) & ids == id

# The levels a report's page is read from, nearest first: its own rows,
# its pattern's, Standard's (a pattern itself: its own, then Standard's;
# Standard: its own only)
.page_levels <- function(x, output_id) {
  if (is.na(output_id)) return(c(own = NA_character_))
  if (.is_pattern_id(output_id)) return(c(own = output_id, standard = NA))
  pat <- report_pattern(x, output_id)
  c(own = output_id, pattern = if (!is.na(pat)) paste0("@", pat), standard = NA)
}

.rows_of <- function(d, id) {
  if (is.null(d)) return(NULL)
  if (is.na(id)) d[is.na(d$output_id), , drop = FALSE] else
    d[!is.na(d$output_id) & d$output_id == id, , drop = FALSE]
}

# A one-row sheet's value for a report, and where it comes from: "own",
# "pattern" or "standard" (NA: none says it)
page_value_from <- function(x, sheet, col, output_id) {
  lv <- .page_levels(x, output_id)
  for (k in names(lv)) {
    r <- .rows_of(x$sheets[[sheet]], lv[[k]])
    v <- if (NROW(r) && !is.null(r[[col]])) r[[col]][1L] else NA
    if (!is.na(v) && nzchar(trimws(v))) return(list(value = v, from = k))
  }
  list(value = NA_character_, from = NA_character_)
}

# What a report inherits for one cell: the value without its own row's,
# and where it comes from (.inherited_from)
.inherited_from <- function(x, sheet, col, output_id) {
  lv <- .page_levels(x, output_id)[-1L]
  for (k in names(lv)) {
    r <- .rows_of(x$sheets[[sheet]], lv[[k]])
    v <- if (NROW(r) && !is.null(r[[col]])) r[[col]][1L] else NA
    if (!is.na(v) && nzchar(trimws(v))) return(list(value = v, from = k))
  }
  list(value = NA_character_, from = NA_character_)
}
.inherited_value <- function(x, sheet, col, output_id) {
  .inherited_from(x, sheet, col, output_id)$value
}

# Set a report's own cell; the value its pattern (or Standard) gives
# already is no difference: the own cell is cleared instead
set_page_cell <- function(x, sheet, col, output_id, value) {
  if (!is.na(value) && nzchar(trimws(value)) &&
      identical(trimws(value), trimws(.inherited_value(x, sheet, col, output_id) %||% ""))) {
    value <- NA_character_
  }
  .set_row_value(x, sheet, col, value, output_id)
}

# A band's lines (header, footer, titles, footnotes) for a report, as its
# page prints them, nearest level first: line, left, center, right, from
# ("own" / "pattern" / "standard"), and `omitted` (a "(none)" line: the
# line of that number a level below is not printed)
page_lines_from <- function(x, sheet, output_id) {
  d <- .normalize_sheet(x$sheets[[sheet]], sheet)
  lv <- .page_levels(x, output_id)
  out <- d[0L, , drop = FALSE]
  out$from <- character()
  for (k in names(lv)) {
    r <- .rows_of(d, lv[[k]])
    r <- r[!r$line %in% out$line, , drop = FALSE]
    if (nrow(r)) {
      r$from <- rep(k, nrow(r))
      out <- rbind(out, r)
    }
  }
  txt <- function(v) ifelse(is.na(v), "", trimws(v))
  out$omitted <- txt(out$left) == "(none)" | txt(out$center) == "(none)" |
    txt(out$right) == "(none)"
  n <- suppressWarnings(as.integer(out$line))
  out <- out[order(is.na(n), n, out$line), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# A report's own line of a band: set (its text), left out ("(none)"), or
# gone (back to its pattern's or Standard's line of that number)
set_page_line <- function(x, sheet, output_id, line, left = NA, center = NA,
                          right = NA) {
  d <- .normalize_sheet(x$sheets[[sheet]], sheet)
  mine <- .is_id(d$output_id, output_id) & d$line %in% line
  row <- d[0L, , drop = FALSE]
  row[1L, ] <- NA
  row$output_id <- output_id
  row$line <- as.character(line)
  row$left <- left
  row$center <- center
  row$right <- right
  if (any(mine)) {
    d[which(mine)[1L], c("left", "center", "right")] <- row[1L, c("left", "center", "right")]
  } else {
    d <- rbind(d, row)
  }
  rownames(d) <- NULL
  x$sheets[[sheet]] <- d
  x
}

omit_page_line <- function(x, sheet, output_id, line) {
  set_page_line(x, sheet, output_id, line, left = "(none)")
}

drop_page_line <- function(x, sheet, output_id, line) {
  d <- x$sheets[[sheet]]
  keep <- !(.is_id(d$output_id, output_id) & d$line %in% line)
  d <- d[keep, , drop = FALSE]
  rownames(d) <- NULL
  x$sheets[[sheet]] <- d
  x
}

# The tokens a report's page has, nearest level first: name, value, from
page_tokens_from <- function(x, output_id) {
  d <- .normalize_sheet(x$sheets$tokens, "tokens")
  lv <- .page_levels(x, output_id)
  out <- d[0L, c("name", "value"), drop = FALSE]
  out$from <- character()
  for (k in names(lv)) {
    r <- .rows_of(d, lv[[k]])
    r <- r[!trimws(r$name) %in% out$name, c("name", "value"), drop = FALSE]
    if (nrow(r)) {
      r$from <- rep(k, nrow(r))
      out <- rbind(out, r)
    }
  }
  rownames(out) <- NULL
  out
}

# A report's own token value (NA: back to its pattern's or Standard's)
set_page_token <- function(x, output_id, name, value) {
  d <- .normalize_sheet(x$sheets$tokens, "tokens")
  mine <- .is_id(d$output_id, output_id) & trimws(d$name) == name
  if (is.na(value)) {
    d <- d[!mine, , drop = FALSE]
  } else if (any(mine)) {
    d$value[which(mine)[1L]] <- value
  } else {
    d <- rbind(d, .normalize_sheet(data.frame(output_id = output_id, name = name,
                                              value = value), "tokens"))
  }
  rownames(d) <- NULL
  x$sheets$tokens <- d
  x
}

#' Make, rename or remove a page pattern
#'
#' A new pattern starts as Standard (no difference: one empty row of the
#' report sheet, `output_id` `@<name>`).  Renaming moves its rows and the
#' reports' choice of it; a pattern a report uses is not removed.
#'
#' @param x An `tflplanner`.
#' @param name,new A pattern's name: a letter, then letters, digits or `_`.
#' @return `x`.
#' @export
add_page_pattern <- function(x, name) {
  name <- .check_pattern_name(x, name)
  d <- .normalize_sheet(x$sheets$report, "report")
  row <- d[0L, , drop = FALSE]
  row[1L, ] <- NA
  row$output_id <- paste0("@", name)
  x$sheets$report <- rbind(d, row)
  x
}

#' @rdname add_page_pattern
#' @export
rename_page_pattern <- function(x, name, new) {
  if (!name %in% page_patterns(x)) stop("No page pattern '", name, "'.", call. = FALSE)
  new <- .check_pattern_name(x, new)
  for (sh in .pattern_sheets) {
    d <- x$sheets[[sh]]
    d$output_id[d$output_id %in% paste0("@", name)] <- paste0("@", new)
    x$sheets[[sh]] <- d
  }
  p <- x$sheets$report$pattern
  if (!is.null(p)) {
    p[!is.na(p) & sub("^@", "", trimws(p)) == name] <- new
    x$sheets$report$pattern <- p
  }
  x
}

#' @rdname add_page_pattern
#' @export
remove_page_pattern <- function(x, name) {
  used <- pattern_reports(x, name)
  if (length(used)) {
    stop("The page pattern '", name, "' is used by ", paste(used, collapse = ", "),
         ": choose another for them first.", call. = FALSE)
  }
  for (sh in .pattern_sheets) {
    d <- x$sheets[[sh]]
    x$sheets[[sh]] <- d[!d$output_id %in% paste0("@", name), , drop = FALSE]
  }
  x
}

# The reports that use a pattern
pattern_reports <- function(x, name) {
  d <- x$sheets$report
  if (is.null(d$pattern)) return(character())
  p <- sub("^@", "", trimws(d$pattern))
  ids <- d$output_id[!is.na(p) & p == name & !.is_pattern_id(d$output_id) & !is.na(d$output_id)]
  unique(ids)
}

.check_pattern_name <- function(x, name) {
  name <- trimws(sub("^@", "", name %||% ""))
  if (!grepl(.pattern_rx, name)) {
    stop("A page pattern's name is a letter, then letters, digits or _ (Compact): not '",
         name, "'.", call. = FALSE)
  }
  if (identical(tolower(name), "standard")) {
    stop("Standard is the blank rows: give the pattern another name.", call. = FALSE)
  }
  if (name %in% page_patterns(x)) stop("There is a page pattern '", name, "' already.", call. = FALSE)
  name
}
