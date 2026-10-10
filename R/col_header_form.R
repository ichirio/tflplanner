# The column header as the builder's form: a small table with the table's
# columns -- the row-header columns, then the value columns -- one row a
# header line (rtfreporter's plan_col_header(lines = ): one element a line).
# A line's value columns: the same text on each, one cell per value of a
# key (a spanner), one cell over them all, nothing -- or cell by cell, each
# cell over one or more neighbouring values of the first column variable
# (`TRT01A = Placebo | TRT01A = ...`).  The form is a view of the
# `col_header` sheet's rows; a header it cannot show (positions, a
# KEY = value of another variable, styled row-header cells) is left to the
# sheet's own grid.

# A text cell's spaces (tflspec's rule for its text columns): leading or
# trailing spaces count only inside quotes ("  Total"), so that the stray
# spaces of a pasted cell do not print.  A form shows the text without the
# quotes and writes it back quoted when it starts or ends with a space:
# spaces typed in a form are not dropped.
.text_to_cell <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1L]) || !nzchar(trimws(x[1L]))) return(x)
  if (grepl("^[\"'].*[\"']$", x) || !grepl("^\\s|\\s$", x)) return(x)
  paste0("\"", x, "\"")
}
.text_from_cell <- function(x) {
  if (is.null(x) || !length(x)) return(x)
  q <- !is.na(x) & grepl("^([\"']).*\\1$", x) & nchar(x) >= 2L
  x[q] <- substring(x[q], 2L, nchar(x[q]) - 1L)
  x
}

# The rows of a report's col_header as the form's lines, or NULL when the
# form cannot show them.  A line: `line`, `stub` (text per row-header
# column, named; or one text over them all when `merge`), `merge`, `mode`
# ("each", "key", "all", "none", "cells"), `key`, `text`, `align`, `bold`,
# `border_bottom` (the value cells'); for "cells", `segments`: each a list
# of `levels` (values of `key`, the first column variable) and `text`.
header_read <- function(rows, keys = character()) {
  if (is.null(rows) || !nrow(rows)) return(list())
  r <- as.data.frame(rows, stringsAsFactors = FALSE)
  for (k in c("line", "cols", "span", "text", "align", "bold", "border_top",
              "border_bottom")) if (!k %in% names(r)) r[[k]] <- NA_character_
  ln <- suppressWarnings(as.integer(r$line))
  if (anyNA(ln) || anyNA(r$cols)) return(NULL)
  simple <- function(x) grepl("^[A-Za-z.][A-Za-z0-9._]*$", x)
  # a cell over values of the first column variable: "K = a | K = b"
  key1 <- if (length(keys)) keys[[1L]] else NA_character_
  kv_levels <- function(cols) {
    if (is.na(key1)) return(NULL)
    parts <- trimws(strsplit(cols, "|", fixed = TRUE)[[1L]])
    pat <- paste0("^", gsub("([.])", "\\\\\\1", key1), "\\s*=\\s*")
    if (!length(parts) || !all(grepl(pat, parts))) return(NULL)
    trimws(sub(pat, "", parts))
  }
  out <- list()
  for (l in sort(unique(ln))) {
    d <- r[ln == l, , drop = FALSE]
    kv <- lapply(d$cols, kv_levels)
    is_kv <- !vapply(kv, is.null, NA)
    if (any(is_kv)) {
      # cell by cell: no other value cell, no span, one style for them all
      cells <- d[is_kv, , drop = FALSE]
      if (any(d$cols == ".values") || any(!is.na(cells$span)) ||
          any(!is.na(cells$border_top))) return(NULL)
      sty <- unique(cells[c("align", "bold", "border_bottom")])
      if (nrow(sty) > 1L) return(NULL)
      stub <- d[!is_kv, , drop = FALSE]
      parts <- strsplit(stub$cols, "\\s*\\|\\s*")
      if (!all(vapply(parts, function(p) all(simple(p)), NA))) return(NULL)
      if (any(!is.na(stub$span)) ||
          any(!is.na(unlist(stub[c("align", "bold", "border_top", "border_bottom")]))))
        return(NULL)
      merge <- nrow(stub) == 1L && length(parts[[1L]]) > 1L
      if (!merge && any(lengths(parts) > 1L)) return(NULL)
      stub_text <- if (merge) stats::setNames(.text_from_cell(stub$text), paste(parts[[1L]], collapse = " | ")) else
        stats::setNames(.text_from_cell(stub$text), unlist(parts))
      out[[length(out) + 1L]] <- list(
        line = l, stub = stub_text, merge = merge, mode = "cells", key = key1,
        text = NA_character_,
        segments = lapply(which(is_kv), function(i)
          list(levels = kv[[i]], text = .text_from_cell(d$text[i]))),
        align = sty$align[1L], bold = sty$bold[1L],
        border_bottom = sty$border_bottom[1L])
      next
    }
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
    stub_text <- if (merge) stats::setNames(.text_from_cell(stub$text), paste(parts[[1L]], collapse = " | ")) else
      stats::setNames(.text_from_cell(stub$text), unlist(parts))
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
      text = if (nrow(val)) .text_from_cell(val$text) else NA_character_,
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
        text = if (blank(st[[k]])) NA_character_ else .text_to_cell(st[[k]]),
        align = NA_character_, bold = NA_character_,
        border_top = NA_character_, border_bottom = NA_character_,
        stringsAsFactors = FALSE)
    }
    if (identical(l$mode, "cells")) {
      # one row a cell with something to print, over its values
      for (sg in l$segments %||% list()) {
        if (blank(sg$text) || !length(sg$levels)) next
        rows[[length(rows) + 1L]] <- data.frame(
          line = as.character(i),
          cols = paste(paste(l$key, "=", sg$levels), collapse = " | "),
          span = NA_character_, text = .text_to_cell(sg$text),
          align = if (blank(l$align)) NA_character_ else l$align,
          bold = if (blank(l$bold)) NA_character_ else l$bold,
          border_top = NA_character_,
          border_bottom = if (blank(l$border_bottom)) NA_character_ else l$border_bottom,
          stringsAsFactors = FALSE)
      }
    } else if (!identical(l$mode, "none")) {
      rows[[length(rows) + 1L]] <- data.frame(
        line = as.character(i), cols = ".values",
        span = switch(l$mode, each = "each",
                      key = if (blank(l$key)) NA_character_ else l$key,
                      NA_character_),
        text = if (blank(l$text)) NA_character_ else .text_to_cell(l$text),
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

# What the header's {n} counts, as choices: the populations the ARD states
# (`cand`: rtfreporter::plan_n_candidates()), each with its values; the
# value of each is what tables$header_n holds.  `now`: header_n as it is.
# One population (or two with the same numbers): it alone, chosen, and
# header_n as it is; two that differ: each, both, nothing chosen unless
# header_n says, and a warning then; no preview (the builder needs an
# ARD, so only when it could not be made): the three in words.
header_n_choices <- function(cand, keys, now = NA_character_, tr = function(x) x) {
  keep <- if (is.na(now %||% NA_character_)) "" else now
  by <- paste(keys, collapse = " \u00d7 ")
  both <- "n = page | N = table"
  vals <- function(d) {
    d <- d[!is.na(d$value), , drop = FALSE]
    col <- d[!is.na(d$column), , drop = FALSE]
    # the columns of the first key (a key's own values, not joined ones)
    top <- col[!grepl("____", col$column, fixed = TRUE), , drop = FALSE]
    if (nrow(top)) col <- top
    v <- if (nrow(col)) col$value else d$value[is.na(d$column)]
    if (!length(v)) return("\u2014")
    v <- format(v, trim = TRUE)
    if (length(v) > 4L) v <- c(utils::head(v, 4L), "\u2026")
    paste(v, collapse = " / ")
  }
  if (is.null(cand) || !nrow(cand)) {
    ch <- stats::setNames(c(if (identical(keep, "page")) "page" else "", "table", both),
                          c(tr("Subjects of each page (the default)"),
                            tr("The analysis set, the same on every page"),
                            tr("Both: {n} each page's, {N} the analysis set's")))
    return(list(choices = ch, selected = keep,
                note = tr("The numbers are not known yet: the preview has not been made."), warn = NULL))
  }
  if (all(cand$scope == "all")) {
    ch <- stats::setNames(keep, sprintf(tr("Subjects, by %s: %s"), by, vals(cand)))
    return(list(choices = ch, selected = keep, note = NULL, warn = NULL))
  }
  pages <- unique(cand$page[cand$scope == "page"])
  first <- cand[cand$scope == "page" & cand$page %in% pages[1L], , drop = FALSE]
  pcol <- attr(cand, "page_col") %||% tr("the page")
  page_lab <- paste0(sprintf(tr("Subjects on each page (per %s, by %s): %s"), pcol, by, vals(first)),
                     " ", sprintf(tr("(the first page, %s)"), pages[1L]))
  tbl_lab <- sprintf(tr("Analysis set (by %s, the same on every page): %s"), by,
                     vals(cand[cand$scope == "table", , drop = FALSE]))
  if (!isTRUE(attr(cand, "differ"))) {
    ch <- stats::setNames(keep, paste(page_lab,
                                      tr("(the pages and the analysis set have the same numbers)")))
    return(list(choices = ch, selected = keep, note = NULL, warn = NULL))
  }
  ch <- stats::setNames(c("page", "table", both),
                        c(page_lab, tbl_lab, tr("Both: {n} each page's, {N} the analysis set's")))
  sel <- if (keep %in% ch) keep else NULL
  list(choices = ch, selected = sel, note = NULL,
       warn = if (is.null(sel)) tr("The pages and the analysis set have different numbers: choose which one {n} says (until then, making the report warns)."))
}

# A preset's lines in short, for the list it is chosen from:
# "{col} / (N={n})" (each line's value text, the lines joined)
header_preset_sample <- function(d) {
  if (is.null(d) || !nrow(d)) return("")
  d <- d[order(suppressWarnings(as.numeric(d$line))), , drop = FALSE]
  one <- vapply(split(d$text, factor(d$line, unique(d$line))), function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) gsub("\n", " ", x[length(x)], fixed = TRUE) else ""
  }, "")
  paste(one[nzchar(one)], collapse = "  /  ")
}

# A cell-by-cell line's cells over the values `lv` in their order: each
# value in one cell (those not in any: a cell of their own, blank); a cell's
# values kept together where they are neighbours, else split
header_segments <- function(segments, lv) {
  out <- list()
  done <- character()
  for (v in lv) {
    if (v %in% done) next
    hit <- Filter(function(sg) v %in% sg$levels, segments %||% list())
    if (length(hit)) {
      sg <- hit[[1L]]
      # its values from here on that come one after another in `lv`
      at <- match(v, lv)
      run <- lv[at]
      while (at + length(run) <= length(lv) && lv[at + length(run)] %in% sg$levels &&
             !lv[at + length(run)] %in% done) {
        run <- c(run, lv[at + length(run)])
      }
      out[[length(out) + 1L]] <- list(levels = run, text = sg$text)
      done <- c(done, run)
    } else {
      out[[length(out) + 1L]] <- list(levels = v, text = NA_character_)
      done <- c(done, v)
    }
  }
  out
}

# Does any line's text use {n} (then the form asks whose n it is)?
header_uses_n <- function(lines) {
  txt <- unlist(lapply(lines, function(l)
    c(l$text, unname(l$stub), vapply(l$segments %||% list(), function(sg)
      if (is.null(sg$text)) NA_character_ else sg$text, ""))))
  any(grepl("\\{[nN][0-9]*(:[a-z]+)?\\}", txt[!is.na(txt)]))
}
