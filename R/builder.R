# The table builder: a summary table (columns = one key, rows = variables)
# described the way one thinks of it -- which arms in which order, which
# variables in which order with which label and levels, which statistics
# with how many decimals -- instead of sheet rows.  It reads that from the
# same sheets the grids show and writes it back to them, so the two views
# never disagree; and it shows the table as it will print.

#' The rows the builder offers for a continuous variable
#'
#' Each is a row label and its template: one statistic a row (`Mean`,
#' `{mean}`) or several in one (`Mean (SD)`, `{mean} ({sd})`).  Their
#' decimals are the statistics' (the `digits` sheet, see [tflspec::tfl_table_spec()]).
#'
#' @return A data frame: `key`, `row`, `template`.
#' @noRd
builder_stats <- function() {
  d <- company_standards()$statistics
  d[c("key", "row", "template")]
}

# the rows a new table shows for a continuous variable
.builder_default_rows <- c("n", "Mean (SD)", "Median", "Min, Max")

# The statistics a template reads: "{mean} ({sd:.2f})" -> mean, sd
.template_stats <- function(tpl) {
  tok <- regmatches(tpl, gregexpr("\\{[^}:]+", tpl))[[1L]]
  unique(substring(tok, 2L))
}

# For each builder statistic, what its template needs that the ARD does not
# have ("" when the ARD has it all).  With no statistics known, nothing is
# said to be missing.
.builder_stats_lacking <- function(templates, have) {
  if (!length(have)) return(rep("", length(templates)))
  vapply(templates, function(tp)
    paste(setdiff(.template_stats(tp), have), collapse = ", "), "",
    USE.NAMES = FALSE)
}

# The decimals the company standards give each statistic (default_digits)
.std_digits <- function() {
  d <- company_standards()$default_digits
  if (is.null(d) || !nrow(d)) return(stats::setNames(integer(), character()))
  d <- d[is.na(d$variable), , drop = FALSE]
  stats::setNames(as.integer(d$digits), trimws(d$statistic))
}

# The study's rule (its default rows of the `digits` sheet), else the
# standards': the decimals a report's statistic has unless it says its own
.study_digits <- function(x) {
  r <- .std_digits()
  d <- x$sheets$digits
  if (!is.null(d) && nrow(d)) {
    d <- d[is.na(d$output_id) & is.na(d$variable), , drop = FALSE]
    r[trimws(d$statistic)] <- as.integer(d$digits)
  }
  r
}

# A report's rule: its own rows (variable blank) over the study's
.report_digits <- function(x, id) {
  r <- .study_digits(x)
  own <- sheet_rows(x, "digits", id)
  own <- own[is.na(own$variable), , drop = FALSE]
  r[trimws(own$statistic)] <- as.integer(own$digits)
  r
}

# What a row's own digits (cells$digits "1,2") give its statistics; none
# there, its template's own ("{mean:.1f}": the spec's way, #417)
.row_digits <- function(tpl, digits) {
  if (is.na(digits) || !nzchar(digits)) return(.inline_digits(tpl))
  st <- .template_stats(tpl)
  dg <- suppressWarnings(as.integer(trimws(strsplit(digits, ",")[[1L]])))
  dg <- dg[!is.na(dg)]
  if (!length(dg) || !length(st)) return(stats::setNames(integer(), character()))
  stats::setNames(dg[pmin(seq_along(st), length(dg))], st)
}

# The decimals a template states itself: "{mean:.1f} ({sd:.2f})" -> mean 1, sd 2
.inline_digits <- function(tpl) {
  none <- stats::setNames(integer(), character())
  if (length(tpl) != 1L || is.na(tpl)) return(none)
  m <- regmatches(tpl, gregexpr("[{][^}:]+:[.][0-9]+f%?[}]", tpl))[[1L]]
  if (!length(m)) return(none)
  stats::setNames(as.integer(sub("^.*:[.]([0-9]+)f.*$", "\\1", m)),
                  sub("^[{]([^}:]+):.*$", "\\1", m))
}

# the categorical formats: key, label, template with <p> for the decimals
.cat_formats <- function() company_standards()$categorical_formats

.cat_template <- function(key, pct, value = "stat", own = NA_character_) {
  f <- .cat_formats()
  tpl <- if (identical(key, "own")) own else
    gsub("<p>", as.character(pct), f$template[match(key, f$key)], fixed = TRUE)
  # the ARD's own text (stat_fmt): the statistics as formatted there
  if (identical(value, "stat_fmt")) tpl <- gsub(":[^}]*}", "}", tpl)
  tpl
}

# which format, with how many decimals, a template is; one the standards do
# not have is the table's own ("own", `own` the template)
.cat_read <- function(tpl) {
  f <- .cat_formats()
  if (is.na(tpl)) return(list(key = f$key[1L], pct = 1))
  for (i in seq_len(nrow(f))) {
    for (p in 0:3) {
      t1 <- gsub("<p>", p, f$template[i], fixed = TRUE)
      if (identical(t1, tpl) || identical(gsub(":[^}]*}", "}", t1), tpl)) {
        return(list(key = f$key[i], pct = p))
      }
    }
  }
  list(key = "own", pct = 1, own = tpl)
}

.first_seen_chr <- function(x) unique(x[!is.na(x)])

# a value, unless it is missing (NULL, NA): then the other
.or_na <- function(a, b) if (length(a) == 1L && !is.na(a)) a else b

.split_list <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(character())
  trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
}

# a report's rows of a sheet, its own before the defaults it inherits
.rows_for <- function(p, sheet, id) {
  rbind(sheet_rows(p, sheet, id), inherited_rows(p, sheet, id))
}
