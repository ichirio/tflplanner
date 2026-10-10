# ============================================================================
#  A TOC's source files read by the company's rules (#299): each workbook
#  read once -- its sheet names and the first rows of every sheet -- and the
#  TOC sheet and its header row found, not chosen: a sheet named as a rule
#  set names it, else the one whose rows match the most column names.
#  Nothing here knows Shiny; the dialog draws what these return.
# ============================================================================

# A cell or a column name as compared with a rule's candidates: case, blanks
# and a trailing colon do not matter ("Title 1:" is "title1")
.toc_norm <- function(x) {
  x <- tolower(as.character(x))
  # (fixed: a pattern of these characters is not safe in every locale)
  for (ch in c("\u00a0", "\u3000")) x <- gsub(ch, " ", x, fixed = TRUE)
  x <- gsub("\uff1a", ":", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", "", x)
  sub(":+$", "", x)
}

# " | "-separated candidates as a vector (blank: none)
.toc_cands <- function(s) {
  if (is.null(s) || !length(s) || is.na(s[1L])) return(character())
  v <- trimws(strsplit(s[1L], "|", fixed = TRUE)[[1L]])
  v[nzchar(v)]
}

# Does a sheet's name match one of the candidates (`*` a wildcard)?
.toc_name_hit <- function(name, cands) {
  if (!length(cands)) return(FALSE)
  n <- .toc_norm(name)
  any(vapply(.toc_norm(cands), function(cn) {
    if (grepl("*", cn, fixed = TRUE)) grepl(utils::glob2rx(cn), n) else identical(cn, n)
  }, NA))
}

# An output number as a key to compare by: "Table 14.1.1", "T-14-1-1" and
# "t.14.1.1" are one key ("t.14.1.1"); "14.1.1" stays "14.1.1"
.toc_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^(tables?|tab|tbl|t)[ ._-]*(?=[0-9])", "t.", x, perl = TRUE)
  x <- sub("^(figures?|fig|f)[ ._-]*(?=[0-9])", "f.", x, perl = TRUE)
  x <- sub("^(listings?|list|lst|l)[ ._-]*(?=[0-9])", "l.", x, perl = TRUE)
  x <- gsub("[[:space:]_-]+", ".", x)
  gsub("\\.+", ".", x)
}

# Is a sheet named like an output number (a shell)?
.toc_shell_named <- function(name) {
  grepl("^([tfl]\\.)?[0-9]+(\\.[0-9a-z]+)+$", .toc_key(name))
}

#' A TOC's source file, read once
#'
#' `toc_source()` reads a workbook's sheet names and the first rows of every
#' sheet (a `.csv` is one sheet); the rest of a sheet is read when it is
#' needed, once.  `toc_find_header()` finds the header row of a sheet's
#' first rows; `toc_find_sheet()` the TOC sheet of a source; `toc_rows()`
#' reads the TOC's rows under its header through a map.
#'
#' Sheet: a sheet named as the rule set names a TOC sheet (`sheet_names`)
#' is taken; else the sheet whose best row of the first `header_rows`
#' matches the most column names (a TOC has a title column and one item
#' more at least), ties broken by more rows, then by more sheets named like
#' its report numbers (shells).  A second TOC-like sheet -- named as a
#' Topline sheet, or whose reports are some of the TOC's -- is the Topline
#' cross-check, never the source.
#'
#' @param path An `.xlsx`, `.xls` or `.csv` file.
#' @param header_rows How many rows from the top are read at first (the
#'   header is looked for in them).
#' @param name The file's name as the user knows it (a file chosen in a
#'   browser has a temporary path).
#' @return `toc_source()`: a `toc_source`: `path`, `name`, `md5`, `kind`
#'   (`"excel"`, `"csv"` or `"other"`), `sheets`, `head` (each sheet's
#'   first rows as a character matrix).
#' @export
toc_source <- function(path, header_rows = 10L, name = basename(path)) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("No file ", sQuote(path), ".", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  kind <- if (ext %in% c("xlsx", "xlsm", "xls")) "excel" else
    if (ext == "csv") "csv" else "other"
  src <- structure(list(path = path, name = name,
                        md5 = unname(tools::md5sum(path)), kind = kind,
                        sheets = character(), head = list(),
                        header_rows = as.integer(header_rows),
                        cache = new.env(parent = emptyenv())),
                   class = "toc_source")
  if (kind == "other") return(src)
  src$sheets <- if (kind == "csv") tools::file_path_sans_ext(basename(name)) else
    tryCatch(readxl::excel_sheets(path), error = function(e) character())
  src$head <- stats::setNames(lapply(src$sheets, function(s)
    .toc_sheet_cells(src, s, header_rows)), src$sheets)
  src
}

#' @export
print.toc_source <- function(x, ...) {
  cat(sprintf("<toc_source> %s (%s): %d sheet(s)\n", x$name, x$kind,
              length(x$sheets)))
  invisible(x)
}

# A sheet's cells as text, as the sheet numbers its rows (row 1 is the
# sheet's first row, blank or not), blanks NA; `n` rows at most.  A whole
# sheet is read once (kept in the source).
.toc_sheet_cells <- function(src, sheet, n = Inf) {
  full <- src$cache[[sheet]]
  if (!is.null(full)) return(full[seq_len(min(n, nrow(full))), , drop = FALSE])
  if (src$kind == "csv") {
    d <- tryCatch(utils::read.csv(src$path, header = FALSE, colClasses = "character",
                                  check.names = FALSE, fileEncoding = "UTF-8-BOM",
                                  na.strings = character(), blank.lines.skip = FALSE),
                  error = function(e) data.frame())
  } else {
    rows <- c(1L, if (is.finite(n)) as.integer(n) else NA_integer_)
    d <- tryCatch(suppressMessages(readxl::read_excel(
      src$path, sheet = sheet, col_names = FALSE, col_types = "text",
      range = readxl::cell_rows(rows), .name_repair = "minimal")),
      error = function(e) data.frame())
  }
  m <- if (nrow(d) && ncol(d)) as.matrix(as.data.frame(d, stringsAsFactors = FALSE)) else
    matrix(NA_character_, 0L, 0L)
  storage.mode(m) <- "character"
  m[] <- trimws(gsub("\r\n", "\n", m, fixed = TRUE))
  m[!is.na(m) & !nzchar(m)] <- NA_character_
  dimnames(m) <- NULL
  if (!is.finite(n) || src$kind == "csv") assign(sheet, m, envir = src$cache)
  if (is.finite(n) && nrow(m) > n) m <- m[seq_len(n), , drop = FALSE]
  m
}

# Every candidate column name of a rule set's items (or of several sets'),
# normalised: a named list item -> names
.toc_item_cands <- function(sets) {
  out <- list()
  for (s in sets) {
    for (i in seq_len(nrow(s$map))) {
      it <- s$map$item[i]
      out[[it]] <- unique(c(out[[it]], .toc_norm(.toc_cands(s$map$columns[i]))))
    }
  }
  out
}

#' @rdname toc_source
#' @param head A sheet's first rows (a character matrix, as `source$head`
#'   holds them).
#' @param rule_set A rule set ([toc_rule_set()]) or a list of them (the
#'   candidates of all are used).
#' @return `toc_find_header()`: a list: `row` (`NA` when no row matches),
#'   `score` (the cells that match a column name), `items` (the items they
#'   are), `toc` (`TRUE` when the row is a TOC's header: a title column and
#'   one item more).
#' @export
toc_find_header <- function(head, rule_set) {
  sets <- if (inherits(rule_set, "toc_rule_set")) list(rule_set) else rule_set
  cands <- .toc_item_cands(sets)
  n <- min(nrow(head), max(vapply(sets, function(s) s$settings$header_rows, 1L)))
  best <- list(row = NA_integer_, score = 0L, items = character(), toc = FALSE)
  for (i in seq_len(n)) {
    v <- .toc_norm(head[i, ])
    v <- v[!is.na(v)]
    if (!length(v)) next
    items <- names(cands)[vapply(cands, function(cn) any(v %in% cn), NA)]
    score <- sum(v %in% unlist(cands))
    if (score > best$score) {
      best <- list(row = i, score = score, items = items,
                   toc = "title" %in% items && length(items) >= 2L)
    }
  }
  best
}

#' @rdname toc_source
#' @param source A `toc_source`.
#' @param rules What [toc_rules()] returned (every set's candidates are
#'   used), or one rule set.
#' @return `toc_find_sheet()`: a data frame, a row a sheet, the chosen
#'   first: `sheet`, `role` (`"toc"`, `"topline"`, `"other"` -- another
#'   TOC-like sheet --, `"shell"` -- named like a report number -- or
#'   `""`), `name_hit`, `header_row`, `score`, `items`, `rows` (rows under
#'   the header), `shell_like`, `why`; `attr(, "kind")`: `"toc"`,
#'   `"shells"` (no TOC, shell sheets) or `"none"`.
#' @export
toc_find_sheet <- function(source, rules) {
  sets <- if (inherits(rules, "toc_rule_set")) list(rules) else rules
  sheet_names <- unique(unlist(lapply(sets, function(s) .toc_cands(s$settings$sheet_names))))
  top_names <- unique(unlist(lapply(sets, function(s) .toc_cands(s$settings$topline_sheet_names))))
  sh <- source$sheets
  if (!length(sh)) {
    out <- data.frame(sheet = character(), role = character(), name_hit = logical(),
                      header_row = integer(), score = integer(), items = character(),
                      rows = integer(), shell_like = integer(), why = character(),
                      stringsAsFactors = FALSE)
    attr(out, "kind") <- "none"
    attr(out, "two_toc") <- FALSE
    return(out)
  }
  out <- data.frame(sheet = sh, role = rep("", length(sh)),
                    name_hit = vapply(sh, .toc_name_hit, NA, cands = sheet_names),
                    topline_hit = vapply(sh, .toc_name_hit, NA, cands = top_names),
                    header_row = NA_integer_, score = 0L, items = "",
                    toc = FALSE, rows = 0L, shell_like = 0L, why = "",
                    stringsAsFactors = FALSE, row.names = NULL)
  if (source$kind == "csv") out$name_hit <- TRUE
  for (i in seq_along(sh)) {
    h <- toc_find_header(source$head[[sh[i]]], sets)
    out$header_row[i] <- h$row
    out$score[i] <- h$score
    out$items[i] <- paste(h$items, collapse = ", ")
    out$toc[i] <- h$toc
  }
  cands <- .toc_item_cands(sets)
  ids_of <- list()
  for (i in which(out$toc)) {
    m <- .toc_sheet_cells(source, sh[i])
    hr <- out$header_row[i]
    body <- if (nrow(m) > hr) m[(hr + 1L):nrow(m), , drop = FALSE] else m[0L, , drop = FALSE]
    out$rows[i] <- sum(rowSums(!is.na(body)) > 0L)
    # the sheet's report numbers: its id-like columns' values, or its first
    # title column's "Table 14.1.1"
    hv <- .toc_norm(m[hr, ])
    idc <- which(hv %in% c(cands$output_id, cands$shell, cands$number))
    if (!length(idc)) idc <- which(hv %in% cands$title)[1L]
    v <- if (length(idc) && !all(is.na(idc))) stats::na.omit(as.vector(body[, idc])) else character()
    ids_of[[sh[i]]] <- unique(.toc_key(v))
    lab <- .toc_key(vapply(v, function(s) .toc_parse_label(s)$key %||% NA_character_, ""))
    keys <- unique(c(ids_of[[sh[i]]], stats::na.omit(lab)))
    others <- .toc_key(sub("-[0-9]+$", "", sh[-i]))
    out$shell_like[i] <- sum(others %in% keys)
  }
  elig <- which(out$toc)
  kind <- if (length(elig)) "toc" else
    if (any(.toc_shell_named(sh))) "shells" else "none"
  out$role[.toc_shell_named(sh) & !out$toc] <- "shell"
  if (length(elig)) {
    pool <- elig[out$name_hit[elig] & !out$topline_hit[elig]]
    by_name <- length(pool) == 1L
    if (!length(pool)) pool <- elig[!out$topline_hit[elig]]
    if (!length(pool)) pool <- elig
    pool <- pool[order(-out$score[pool], -out$rows[pool], -out$shell_like[pool])]
    k <- pool[1L]
    out$role[k] <- "toc"
    out$why[k] <- if (by_name) "name" else
      if (length(pool) > 1L && out$score[pool[2L]] == out$score[k]) "rows" else "score"
    for (j in setdiff(elig, k)) {
      sub_of <- length(ids_of[[sh[j]]]) && all(ids_of[[sh[j]]] %in% ids_of[[sh[k]]])
      out$role[j] <- if (out$topline_hit[j] || sub_of) "topline" else "other"
    }
    # a second topline sheet is another TOC-like sheet
    tl <- which(out$role == "topline")
    if (length(tl) > 1L) out$role[tl[-1L]] <- "other"
    ord <- c(k, setdiff(order(match(out$role, c("toc", "topline", "other", "shell", ""))), k))
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL
  out$topline_hit <- NULL
  out$toc <- NULL
  attr(out, "kind") <- kind
  attr(out, "two_toc") <- length(elig) > 1L && !identical(out$why[1L], "name")
  out
}

#' @rdname toc_source
#' @param sheet The TOC sheet.
#' @param header_row Its header row (as the sheet numbers its rows).
#' @param map Which column is what: a named list, item -> column name(s) of
#'   the header ([toc_map_for()]).
#' @return `toc_rows()`: a data frame, a row a non-blank row under the
#'   header: `.source`, `.sheet`, `.row` (the sheet's row number), a column
#'   an item (`NA` when not mapped), `titles` and `footnotes` as list
#'   columns (a title column's cell each, `NA` kept, so a title column's
#'   place is known).
#' @export
toc_rows <- function(source, sheet, header_row, map) {
  m <- .toc_sheet_cells(source, sheet)
  header_row <- as.integer(header_row)
  if (is.na(header_row) || header_row < 1L || header_row > nrow(m)) {
    return(.toc_no_rows())
  }
  hv <- .toc_norm(m[header_row, ])
  pos <- function(it) {
    want <- .toc_norm(map[[it]])
    if (!length(want)) return(integer())
    p <- which(hv %in% want)
    if (it %in% c("title", "footnote")) p else p[order(match(hv[p], want))][1L]
  }
  body <- if (nrow(m) > header_row) m[(header_row + 1L):nrow(m), , drop = FALSE] else
    m[0L, , drop = FALSE]
  rown <- header_row + seq_len(nrow(body))
  used <- unique(unlist(lapply(names(map), pos)))
  used <- used[!is.na(used)]
  keep <- if (length(used)) rowSums(!is.na(body[, used, drop = FALSE])) > 0L else
    rep(FALSE, nrow(body))
  body <- body[keep, , drop = FALSE]
  rown <- rown[keep]
  if (!nrow(body)) return(.toc_no_rows())
  d <- data.frame(.source = rep(source$name, nrow(body)),
                  .sheet = rep(sheet, nrow(body)), .row = rown,
                  stringsAsFactors = FALSE)
  for (it in setdiff(c(.toc_items, .toc_extra_items), c("title", "footnote"))) {
    p <- pos(it)
    d[[it]] <- if (length(p) && !is.na(p)) body[, p] else NA_character_
  }
  tp <- pos("title")
  fp <- pos("footnote")
  d$titles <- lapply(seq_len(nrow(body)), function(i) body[i, tp])
  d$footnotes <- lapply(seq_len(nrow(body)), function(i) body[i, fp])
  rownames(d) <- NULL
  d
}

.toc_no_rows <- function() {
  cols <- setdiff(c(.toc_items, .toc_extra_items), c("title", "footnote"))
  d <- data.frame(.source = character(), .sheet = character(), .row = integer(),
                  stringsAsFactors = FALSE)
  for (cn in cols) d[[cn]] <- character()
  d$titles <- list()
  d$footnotes <- list()
  d
}

# A sheet's column names (its header row); blank cells named by their column
.toc_header_names <- function(source, sheet, header_row) {
  if (is.na(header_row)) return(character())
  m <- .toc_sheet_cells(source, sheet, max(header_row, 1L))
  if (nrow(m) < header_row) return(character())
  v <- m[header_row, ]
  v[!is.na(v)]
}
