# Taking a TOC in by the company's rules: the output id, the type and the
# label.  Split from toc_rules.R (no code change).

# ---- the output id, the type, the label (D3) --------------------------------

# A report's type from a word: Table, Tab, T, Figure, Fig, F, Listing, List,
# Lst, L (case does not matter); NA when none
.toc_type_word <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[x %in% c("table", "tables", "tab", "tbl", "t")] <- "table"
  out[x %in% c("figure", "figures", "fig", "f", "graph")] <- "figure"
  out[x %in% c("listing", "listings", "list", "lst", "l")] <- "listing"
  out
}

.toc_type_re <- "^(tables?|tab|tbl|t|figures?|fig|f|listings?|list|lst|l)[ ._-]*(?=[0-9])"

# The type an id's prefix says ("t.14.1", "T-14-1", "Table 14.1"); NA none
.toc_type_prefix <- function(x) {
  x <- trimws(as.character(x))
  m <- regmatches(x, regexpr(.toc_type_re, x, perl = TRUE, ignore.case = TRUE))
  out <- rep(NA_character_, length(x))
  has <- grepl(.toc_type_re, x, perl = TRUE, ignore.case = TRUE)
  out[has] <- .toc_type_word(gsub("[ ._-]+$", "", m))
  out
}

# An id's number: its type prefix taken off, "-" and "_" read as "."
# ("T-14-1-1" -> "14.1.1"); NA when it does not start with a digit
.toc_number_of <- function(x) {
  x <- trimws(as.character(x))
  n <- sub(.toc_type_re, "", x, perl = TRUE, ignore.case = TRUE)
  n <- gsub("[ _-]+", ".", n)
  n <- gsub("\\.+", ".", sub("[.:]+$", "", n))
  n[is.na(x) | !grepl("^[0-9][0-9A-Za-z.]*$", n)] <- NA_character_
  n
}

# A title cell that starts with a label ("Table 14.1.1", "Figure 14.2 Mean
# SBP"): its type, number, the label as printed, the rest of the cell
# (`""` when the cell is the label alone) and a key; NULLs when none
.toc_parse_label <- function(s) {
  if (is.na(s)) return(list())
  for (ch in c("\u2013", "\u2014")) s <- gsub(ch, "-", s, fixed = TRUE)
  s <- gsub("\uff1a", ":", s, fixed = TRUE)
  m <- regmatches(s, regexec(paste0(
    "^\\s*((tables?|figures?|listings?)\\s*([0-9][0-9A-Za-z]*(?:[.-][0-9A-Za-z]+)*))",
    "[.:]?\\s*[-:]?\\s*(.*)$"), s, ignore.case = TRUE, perl = TRUE))[[1L]]
  if (!length(m)) return(list())
  type <- .toc_type_word(sub("s$", "", tolower(m[3L])))
  number <- gsub("-", ".", m[4L], fixed = TRUE)
  list(type = type, number = number, label = trimws(m[2L]), rest = trimws(m[5L]),
       key = .toc_key(paste0(substr(type, 1L, 1L), ".", number)))
}

# An id from a pattern: {type} t / f / l, {TYPE} T / F / L, {Type} Table /
# Figure / Listing, {number} 14.1.1, {number-} 14-1-1
.toc_id_pattern <- function(pattern, type, number) {
  out <- rep(NA_character_, length(number))
  ok <- !is.na(type) & !is.na(number)
  if (!any(ok)) return(out)
  t1 <- substr(type[ok], 1L, 1L)
  v <- rep(pattern, sum(ok))
  v <- mapply(function(p, t, ty, n) {
    p <- gsub("{type}", t, p, fixed = TRUE)
    p <- gsub("{TYPE}", toupper(t), p, fixed = TRUE)
    p <- gsub("{Type}", paste0(toupper(t), substring(ty, 2L)), p, fixed = TRUE)
    p <- gsub("{number-}", gsub(".", "-", n, fixed = TRUE), p, fixed = TRUE)
    gsub("{number}", n, p, fixed = TRUE)
  }, v, t1, type[ok], number[ok], USE.NAMES = FALSE)
  out[ok] <- v
  out
}

# A row's first title cell (the first title column's, else the first given)
.toc_title1 <- function(rows) {
  vapply(rows$titles, function(v) {
    v <- v[!is.na(v)]
    if (length(v)) v[[1L]] else NA_character_
  }, "")
}

# Each row's type by each source, and the one the set says (or the one that
# reads the most rows): list(by = data frame, from, type)
.toc_types <- function(rows, set, type_from = NULL) {
  t1 <- .toc_title1(rows)
  by <- data.frame(
    column = .toc_type_word(rows$type),
    id = .toc_type_prefix(rows$output_id),
    title = vapply(t1, function(s) .toc_parse_label(s)$type %||% NA_character_, "",
                   USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  from <- type_from %||% set$settings$type_from
  if (is.null(from) || is.na(from) || !from %in% .toc_type_sources) {
    n <- vapply(by, function(v) sum(!is.na(v)), 1L)
    from <- if (any(n > 0L)) names(n)[which.max(n)] else "column"
  }
  type <- by[[from]]
  for (k in setdiff(.toc_type_sources, from)) type[is.na(type)] <- by[[k]][is.na(type)]
  list(by = by, from = from, type = type)
}

# The ids a rule makes (NA where it cannot) with each row's type and number
.toc_ids_by <- function(rows, rule, set, types) {
  n <- nrow(rows)
  none <- rep(NA_character_, n)
  pat <- set$settings$id_pattern
  t1 <- .toc_title1(rows)
  switch(rule,
    column = trimws(rows$output_id),
    normalised = {
      ty <- .toc_type_prefix(rows$output_id)
      ty[is.na(ty)] <- types$type[is.na(ty)]
      .toc_id_pattern(pat, ty, .toc_number_of(rows$output_id))
    },
    type_number = {
      num <- if (all(is.na(rows$number))) rows$output_id else rows$number
      .toc_id_pattern(pat, types$by$column, .toc_number_of(num))
    },
    title = {
      p <- lapply(t1, .toc_parse_label)
      .toc_id_pattern(pat, vapply(p, function(x) x$type %||% NA_character_, ""),
                      vapply(p, function(x) x$number %||% NA_character_, ""))
    },
    none)
}

#' The ways a TOC's output id can be made, scored
#'
#' Each rule of `toc_id_rules` (an id column as it is; normalised to the
#' set's `id_pattern`; a type column and a number column; the number in the
#' first title, "Table 14.1.1") is tried on the rows: the one that gives
#' unique, non-empty ids for the most rows is proposed (on a tie, an id
#' column as it is, unless its ids carry no type and the normalised ones
#' do).
#'
#' @param rows What [toc_rows()] returned.
#' @param rule_set A rule set.
#' @param type_from Where the type is read (`"column"`, `"id"`,
#'   `"title"`); by default the set's, else the source that reads the most
#'   rows.
#' @return A data frame, the proposed first: `rule`, `ok` (rows with a
#'   unique id), `filled` (rows with an id), `preview` (the first ids),
#'   `labels` (the labels they print).
#' @noRd
toc_id_candidates <- function(rows, rule_set, type_from = NULL) {
  types <- .toc_types(rows, rule_set, type_from)
  labels <- .toc_labels(rows)$label
  have_col <- any(!is.na(rows$output_id))
  out <- lapply(.toc_id_rules, function(r) {
    if (r %in% c("column", "normalised") && !have_col) return(NULL)
    if (r == "type_number" && all(is.na(types$by$column))) return(NULL)
    ids <- .toc_ids_by(rows, r, rule_set, types)
    ok <- !is.na(ids) & nzchar(ids)
    dup <- ids %in% ids[ok][duplicated(ids[ok])]
    k <- which(ok)[seq_len(min(3L, sum(ok)))]
    data.frame(rule = r, ok = sum(ok & !dup), filled = sum(ok),
               preview = paste(ids[k], collapse = ", "),
               labels = paste(ifelse(is.na(labels[k]), "-", labels[k]), collapse = ", "),
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, out[!vapply(out, is.null, NA)])
  if (is.null(d)) {
    return(data.frame(rule = character(), ok = integer(), filled = integer(),
                      preview = character(), labels = character(),
                      stringsAsFactors = FALSE))
  }
  pref <- .toc_id_rules
  raw_typed <- mean(!is.na(.toc_type_prefix(rows$output_id[!is.na(rows$output_id)])))
  if (have_col && !is.nan(raw_typed) && raw_typed < 0.5) {
    pref <- c("normalised", "column", "type_number", "title")
  }
  d <- d[order(-d$ok, match(d$rule, pref)), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Each row's label as printed and its title cells without it: the label
# column; else the first title cell when it starts with "Table 14.1.1"
# (the label then is not a title line: a cell that is the label alone is
# dropped, one with more keeps the rest)
.toc_labels <- function(rows) {
  lab <- rows$label
  titles <- rows$titles
  for (i in seq_len(nrow(rows))) {
    if (!is.na(lab[i])) next
    tc <- titles[[i]]
    j <- which(!is.na(tc))[1L]
    if (is.na(j)) next
    p <- .toc_parse_label(tc[j])
    if (!length(p)) next
    lab[i] <- p$label
    tc[j] <- if (nzchar(p$rest)) p$rest else NA_character_
    titles[[i]] <- tc
  }
  list(label = lab, titles = titles)
}
