# The column header as the builder's form (Q14): one form a header line, in
# two parts -- the row-header columns, and the value columns (the same text
# on each column, one cell per value of a key, or one cell over them all).
# The form is a view of the `col_header` sheet's rows; a header the form
# cannot show (positions, KEY = value, styled row-header cells) is left to
# the sheet's own grid.

# The rows of a report's col_header as the form's lines, or NULL when the
# form cannot show them.  A line: `line`, `stub` (text per row-header
# column, named; or one text over them all when `merge`), `merge`, `mode`
# ("each", "key", "all", "none"), `key`, `text`, `align`, `bold`,
# `border_bottom` (the value cell's).
header_read <- function(rows, keys = character()) {
  if (is.null(rows) || !nrow(rows)) return(list())
  r <- as.data.frame(rows, stringsAsFactors = FALSE)
  for (k in c("line", "cols", "span", "text", "align", "bold", "border_top",
              "border_bottom")) if (!k %in% names(r)) r[[k]] <- NA_character_
  ln <- suppressWarnings(as.integer(r$line))
  if (anyNA(ln) || anyNA(r$cols)) return(NULL)
  simple <- function(x) grepl("^[A-Za-z.][A-Za-z0-9._]*$", x)
  out <- list()
  for (l in sort(unique(ln))) {
    d <- r[ln == l, , drop = FALSE]
    val <- d[d$cols == ".values", , drop = FALSE]
    stub <- d[d$cols != ".values", , drop = FALSE]
    if (nrow(val) > 1L) return(NULL)
    # row-header cells: plain column names, one cell each or one over all,
    # no span and no styling of their own
    parts <- strsplit(stub$cols, "\\s*\\|\\s*")
    if (!all(vapply(parts, function(p) all(simple(p)), NA))) return(NULL)
    if (any(!is.na(stub$span)) ||
        any(!is.na(unlist(stub[c("align", "bold", "border_top", "border_bottom")]))))
      return(NULL)
    merge <- nrow(stub) == 1L && length(parts[[1L]]) > 1L
    if (!merge && any(lengths(parts) > 1L)) return(NULL)
    stub_text <- if (merge) stats::setNames(stub$text, paste(parts[[1L]], collapse = " | ")) else
      stats::setNames(stub$text, unlist(parts))
    if (nrow(val)) {
      if (!is.na(val$border_top)) return(NULL)
      sp <- val$span
      mode <- if (is.na(sp)) "all" else if (sp == "each") "each" else
        if (sp %in% keys || simple(sp)) "key" else return(NULL)
      key <- if (mode == "key") sp else NA_character_
    } else {
      mode <- "none"
      key <- NA_character_
    }
    out[[length(out) + 1L]] <- list(
      line = l, stub = stub_text, merge = merge, mode = mode, key = key,
      text = if (nrow(val)) val$text else NA_character_,
      align = if (nrow(val)) val$align else NA_character_,
      bold = if (nrow(val)) val$bold else NA_character_,
      border_bottom = if (nrow(val)) val$border_bottom else NA_character_)
  }
  out
}

# The form's lines back into col_header rows (lines renumbered 1, 2, ...)
header_write <- function(lines) {
  rows <- list()
  blank <- function(x) is.null(x) || !length(x) || is.na(x[1L]) || !nzchar(x[1L])
  for (i in seq_along(lines)) {
    l <- lines[[i]]
    st <- l$stub %||% character()
    for (k in seq_along(st)) {
      rows[[length(rows) + 1L]] <- data.frame(
        line = as.character(i), cols = names(st)[k], span = NA_character_,
        text = if (blank(st[[k]])) NA_character_ else st[[k]],
        align = NA_character_, bold = NA_character_,
        border_top = NA_character_, border_bottom = NA_character_,
        stringsAsFactors = FALSE)
    }
    if (!identical(l$mode, "none")) {
      rows[[length(rows) + 1L]] <- data.frame(
        line = as.character(i), cols = ".values",
        span = switch(l$mode, each = "each",
                      key = if (blank(l$key)) NA_character_ else l$key,
                      NA_character_),
        text = if (blank(l$text)) NA_character_ else l$text,
        align = if (blank(l$align)) NA_character_ else l$align,
        bold = if (blank(l$bold)) NA_character_ else l$bold,
        border_top = NA_character_,
        border_bottom = if (blank(l$border_bottom)) NA_character_ else l$border_bottom,
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) {
    return(data.frame(line = character(), cols = character(), span = character(),
                      text = character(), align = character(), bold = character(),
                      border_top = character(), border_bottom = character()))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# The tokens a header cell of this table may carry: the column's keys
# ({col}, and {col1} {col2} ... for several), the population at each depth
# ({n}, {n1} ...), the total over the cell's columns ({n:sum}), and {N} when
# the table's header_n names one
header_token_choices <- function(keys, header_n = NA) {
  k <- length(keys)
  depth <- if (k > 1L) seq_len(k) else integer()
  c("{col}", if (length(depth)) paste0("{col", depth, "}"),
    "{n}", if (length(depth)) paste0("{n", depth, "}"), "{n:sum}",
    if (!is.na(header_n) && grepl("\\bN\\s*=", header_n)) "{N}")
}

# Does any line's text use {n} (then the form asks whose n it is)?
header_uses_n <- function(lines) {
  txt <- unlist(lapply(lines, function(l) c(l$text, unname(l$stub))))
  any(grepl("\\{[nN][0-9]*(:[a-z]+)?\\}", txt[!is.na(txt)]))
}
