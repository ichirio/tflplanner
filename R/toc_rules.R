# ============================================================================
#  A TOC taken in by the company's rules (#299): the rule sets (the
#  standards' toc_map rows with a rule_set, and toc_rules, a row a set),
#  the pick of a set, the output id built by a rule chosen per set, one
#  report a phase, the shell sheets linked, the Topline batch, and the
#  problems.  Every step is a pure function of its inputs; the dialog draws
#  them and feeds the user's choices back.
# ============================================================================

# The items a rule set may name besides tflspec's report fields: report-list
# facts of tflplanner, not report-sheet fields (D9)
.toc_extra_items <- c("phase", "topline", "sap_no", "std_shell", "reference",
                      "shell", "number")

# The settings of a rule set (toc_rules' columns) and what a blank means
.toc_rule_defaults <- list(
  label = NA_character_,
  sheet_names = "TOC | Table of Contents | TFL | TLF",
  topline_sheet_names = "Topline | Top-line | TOC_Topline",
  header_rows = "10",
  phase_values = "both=1,2 | ph1=1 | ph2=2",
  phase_blank = "none",
  number_suffix = ".a | .b",
  title_suffix = " - Phase 1 Part |  - Phase 2 Part",
  title_line = "2",
  topline_values = "Y | Yes | X | 1",
  topline_batch = "Topline",
  shell_phase_suffix = "-1 | -2",
  id_rule = NA_character_,
  id_pattern = "{type}.{number}",
  type_from = NA_character_,
  note = NA_character_)

# How an output id is made (D3, the maintainer's): an id column as it is; an
# id column normalised (its type prefix and separators as `id_pattern`
# says); a type column and a number column; the number in the first title
.toc_id_rules <- c("column", "normalised", "type_number", "title")
# Where a report's type is read: a type column, the id's prefix, the first
# title's first word
.toc_type_sources <- c("column", "id", "title")

# " | " lists whose items keep their own blanks (" - Phase 1 Part")
.toc_list_keep <- function(s) {
  if (is.null(s) || !length(s) || is.na(s[1L]) || !nzchar(s[1L])) return(character())
  v <- strsplit(s[1L], " ?\\| ?")[[1L]]
  v[nzchar(trimws(v))]
}

# "both=1,2 | ph1=1 | ph2=2" as list(both = 1:2, ph1 = 1, ph2 = 2), the
# values normalised
.toc_phase_values <- function(s) {
  out <- list()
  for (p in .toc_cands(s)) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1L]]
    if (length(kv) != 2L) next
    ph <- suppressWarnings(as.integer(trimws(strsplit(kv[2L], ",", fixed = TRUE)[[1L]])))
    ph <- ph[!is.na(ph) & ph >= 1L]
    if (length(ph)) out[[.toc_norm(kv[1L])]] <- ph
  }
  out
}

#' The company's rules for taking in a TOC
#'
#' A company keeps one rule set or several for its TOCs, in the company
#' standards workbook ([company_standards()]): the sheet `toc_map` (an
#' item's candidate column names, ` | ` between them, by `rule_set`; blank
#' is `standard`) and the sheet `toc_rules` (a row a set: its sheet names,
#' header rows, phases, suffixes, Topline flag, shell sheet names, how the
#' output id is made; a blank takes the default).  A workbook written
#' before either reads as one set, `standard`, with the defaults.
#'
#' * `toc_rules()`: every rule set, its settings with the defaults filled.
#' * `toc_rule_set()`: one, by name.
#' * `toc_pick_rules()`: the sets scored on a TOC's column names: a set
#'   whose title item has no column is out; the most items matched wins,
#'   then the set's order.
#' * `toc_map_for()`: which of a TOC's columns is what, by a rule set.
#'
#' @param home tflplanner's home.
#' @param standards The company standards (a list of sheets); by default
#'   those in use.
#' @return `toc_rules()`: a named list of `toc_rule_set`s (`name`, `label`,
#'   `settings`, `map`: `item`, `columns`).
#' @examples
#' r <- toc_rules(home = tempfile())
#' names(r)
#' toc_pick_rules(c("Shell Number", "Title1", "Title2", "g-", "Topline"), r)
#' @export
toc_rules <- function(home = tflplanner_home(), standards = NULL) {
  s <- standards %||% company_standards(home)
  tm <- s$toc_map
  if (is.null(tm)) tm <- .builtin_standards()$toc_map
  if (!"rule_set" %in% names(tm)) tm$rule_set <- NA_character_
  tm$rule_set <- ifelse(is.na(tm$rule_set) | !nzchar(trimws(tm$rule_set)), "standard",
                        trimws(tm$rule_set))
  tr_ <- s$toc_rules
  if (is.null(tr_)) tr_ <- .builtin_standards()$toc_rules
  tr_ <- tr_[!is.na(tr_$rule_set) & nzchar(trimws(tr_$rule_set)), , drop = FALSE]
  tr_$rule_set <- trimws(tr_$rule_set)
  sets <- unique(c(tr_$rule_set, tm$rule_set))
  # a set with no columns named is no set
  sets <- sets[sets %in% tm$rule_set]
  out <- lapply(sets, function(nm) {
    row <- tr_[match(nm, tr_$rule_set), , drop = FALSE]
    st <- lapply(stats::setNames(names(.toc_rule_defaults), names(.toc_rule_defaults)),
                 function(k) {
                   v <- if (k %in% names(row)) row[[k]][1L] else NA_character_
                   if (is.na(v) || !nzchar(trimws(v))) .toc_rule_defaults[[k]] else v
                 })
    .toc_rule_set_make(nm, st, tm[tm$rule_set == nm, c("item", "columns"), drop = FALSE])
  })
  stats::setNames(out, sets)
}

.toc_rule_set_make <- function(name, settings, map) {
  st <- settings
  if (is.na(st$label)) st$label <- name
  st$header_rows <- suppressWarnings(as.integer(st$header_rows))
  if (is.na(st$header_rows) || st$header_rows < 1L) st$header_rows <- 10L
  if (!is.na(st$id_rule) && !st$id_rule %in% .toc_id_rules) st$id_rule <- NA_character_
  if (!is.na(st$type_from) && !st$type_from %in% .toc_type_sources) st$type_from <- NA_character_
  map <- map[!is.na(map$item) & !is.na(map$columns), , drop = FALSE]
  rownames(map) <- NULL
  structure(list(name = name, label = st$label, settings = st, map = map),
            class = "toc_rule_set")
}

#' @export
print.toc_rule_set <- function(x, ...) {
  cat(sprintf("<toc_rule_set> %s (%s): %s\n", x$name, x$label,
              paste(x$map$item, collapse = ", ")))
  invisible(x)
}

#' @rdname toc_rules
#' @param rules What `toc_rules()` returned.
#' @param name A rule set's name.
#' @param overrides Settings a study changes (its profile): a named list
#'   (`number_suffix`, `title_suffix`, `title_line`, ...).
#' @return `toc_rule_set()`: a `toc_rule_set`.
#' @export
toc_rule_set <- function(rules, name = NULL, overrides = NULL) {
  if (inherits(rules, "toc_rule_set")) {
    s <- rules
  } else {
    name <- name %||% names(rules)[1L]
    if (!name %in% names(rules)) {
      stop("No TOC rule set ", sQuote(name), "; the sets are ",
           paste(sQuote(names(rules)), collapse = ", "), ".", call. = FALSE)
    }
    s <- rules[[name]]
  }
  ov <- overrides[intersect(names(overrides), names(.toc_rule_defaults))]
  ov <- ov[!vapply(ov, function(v) is.null(v) || !length(v) || is.na(v[1L]), NA)]
  if (length(ov)) {
    st <- s$settings
    st[names(ov)] <- lapply(ov, function(v) as.character(v[1L]))
    s <- .toc_rule_set_make(s$name, st, s$map)
  }
  s
}

# toc_map rows of one set (a data frame item / columns), from what is given:
# a rule set, or the standards' toc_map (a set's rows; by default
# `standard`, else the first set)
.toc_set_map <- function(std, rule_set = NULL) {
  if (inherits(std, "toc_rule_set")) return(std$map)
  if (is.list(std) && !is.data.frame(std) && length(std) &&
      all(vapply(std, inherits, NA, "toc_rule_set"))) {
    return(toc_rule_set(std, rule_set)$map)
  }
  if (!"rule_set" %in% names(std)) return(std)
  rs <- ifelse(is.na(std$rule_set) | !nzchar(trimws(std$rule_set)), "standard",
               trimws(std$rule_set))
  want <- rule_set %||% (if ("standard" %in% rs) "standard" else rs[1L])
  std[rs == want, , drop = FALSE]
}

#' @rdname toc_rules
#' @param headers A TOC's column names.
#' @param std A rule set, what `toc_rules()` returned, or the standards'
#'   `toc_map` sheet.
#' @param rule_set The rule set's name (`std` a list of sets or the
#'   `toc_map` sheet); by default `standard`.
#' @return `toc_map_for()`: a named list, item -> the TOC's column (an item:
#'   the first of its candidates the TOC has; `title` and `footnote`: every
#'   one, in the TOC's order); `attr(, "missing")`: the items the set names
#'   that the TOC has no column for.
#' @export
toc_map_for <- function(headers, std = company_standards()$toc_map, rule_set = NULL) {
  tm <- .toc_set_map(std, rule_set)
  headers <- headers[!is.na(headers)]
  hn <- .toc_norm(headers)
  out <- list()
  missing <- character()
  for (it in c(.toc_items, .toc_extra_items)) {
    cand <- tm$columns[match(it, tm$item)]
    if (is.na(cand)) next
    names_ <- .toc_norm(.toc_cands(cand))
    hit <- headers[hn %in% names_]
    if (!length(hit)) {
      missing <- c(missing, it)
      next
    }
    out[[it]] <- if (it %in% c("title", "footnote")) hit else
      hit[order(match(.toc_norm(hit), names_))][1L]
  }
  attr(out, "missing") <- missing
  out
}

#' @rdname toc_rules
#' @return `toc_pick_rules()`: a data frame, the best set first:
#'   `rule_set`, `label`, `required_ok`, `matched` (items with a column),
#'   `named` (items the set names), `items`.
#' @export
toc_pick_rules <- function(headers, rules) {
  hs <- if (is.list(headers)) headers else list(headers)
  rows <- lapply(seq_along(rules), function(k) {
    s <- rules[[k]]
    got <- unique(unlist(lapply(hs, function(h) names(toc_map_for(h, s)))))
    data.frame(rule_set = s$name, label = s$label,
               required_ok = "title" %in% got && length(got) >= 2L,
               matched = length(got), named = length(unique(s$map$item)),
               items = paste(got, collapse = ", "), order = k,
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, rows)
  d <- d[order(!d$required_ok, -d$matched, d$order), , drop = FALSE]
  d$order <- NULL
  rownames(d) <- NULL
  d
}

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
#' @export
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

# ---- phases -----------------------------------------------------------------

# The title cell that gets a phase's suffix: `title_line` (1, 2, "first",
# "last") among the title columns, else the last cell given
.toc_title_target <- function(cells, title_line) {
  given <- which(!is.na(cells))
  if (!length(given)) return(NA_integer_)
  if (identical(title_line, "last")) return(given[length(given)])
  if (identical(title_line, "first")) return(given[1L])
  k <- suppressWarnings(as.integer(title_line))
  if (is.na(k) || k < 1L) return(given[length(given)])
  if (k <= length(cells) && !is.na(cells[k])) k else given[length(given)]
}

#' One report a phase
#'
#' A TOC row whose phase value means two phases (`both`) gives two reports,
#' one that means one phase gives one, each with the phase's suffix on its
#' output id and label (`number_suffix`) and on one title line
#' (`title_suffix` on `title_line`); the analysis set is not changed.  No
#' phase column: one report a row, no suffix.  A blank phase is
#' `phase_blank` (`none`: one report, no suffix; `all`: every phase; or a
#' value of `phase_values`).  A value the rules do not know leaves the row
#' out (listed in `attr(, "unread")`).
#'
#' @param rows The TOC's rows with their ids: [toc_rows()] plus
#'   `output_id`, `type`, `label`, `raw_id`.
#' @param rule_set A rule set (with a study's overrides).
#' @return The reports: a row a report, `output_id`, `base_id`, `phase`
#'   (1, 2, ... or `NA`), `label`, `titles` and `footnotes` (list
#'   columns), the rest of the row; `attr(, "unread")`: the rows left out.
#' @export
toc_expand_phases <- function(rows, rule_set) {
  st <- rule_set$settings
  nsuf <- .toc_list_keep(st$number_suffix)
  tsuf <- .toc_list_keep(st$title_suffix)
  pv <- .toc_phase_values(st$phase_values)
  n_ph <- max(length(nsuf), unlist(pv), 1L)
  has_phase <- any(!is.na(rows$phase))
  unread <- .toc_no_unread()
  out <- list()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    ph <- NA_integer_
    if (has_phase) {
      v <- r$phase
      if (is.na(v)) {
        b <- st$phase_blank
        ph <- if (identical(b, "all")) seq_len(n_ph) else
          if (identical(b, "none") || is.na(b)) NA_integer_ else pv[[.toc_norm(b)]] %||% NA_integer_
      } else {
        ph <- pv[[.toc_norm(v)]]
        if (is.null(ph)) {
          unread[nrow(unread) + 1L, ] <- list(r$.source, r$.sheet, r$.row, "TOC05", "phase",
                                              v, r$output_id)
          next
        }
      }
    }
    for (p in ph) {
      e <- r
      e$base_id <- r$output_id
      e$phase <- p
      if (!is.na(p)) {
        sfx <- if (p <= length(nsuf)) nsuf[p] else ""
        e$output_id <- paste0(r$output_id, sfx)
        if (!is.na(r$label)) e$label <- paste0(r$label, sfx)
        if (p <= length(tsuf)) {
          tc <- r$titles[[1L]]
          k <- .toc_title_target(tc, st$title_line)
          if (!is.na(k)) tc[k] <- paste0(tc[k], tsuf[p])
          e$titles <- list(tc)
        }
      }
      out[[length(out) + 1L]] <- e
    }
  }
  d <- if (length(out)) do.call(rbind, out) else {
    z <- rows[0L, , drop = FALSE]
    z$base_id <- character()
    z$phase <- integer()
    z
  }
  d$phase <- as.integer(d$phase)
  rownames(d) <- NULL
  attr(d, "unread") <- unread
  d
}

.toc_no_unread <- function() {
  data.frame(.source = character(), .sheet = character(), .row = integer(),
             rule = character(), field = character(), value = character(),
             output_id = character(), stringsAsFactors = FALSE)
}

# ---- shells -------------------------------------------------------------------

#' Each report's shell sheet
#'
#' Across every source, the sheet named as the report's shell (its `shell`
#' column), its id column as it is or its output id -- the phase's
#' `shell_phase_suffix` first (`t.14.1.1-1`), else without -- matched
#' ignoring case; then the same by key (`T-14-1-1` is `t.14.1.1`).  Only the
#' link is kept: nothing of the sheet is read.
#'
#' @param reports What [toc_expand_phases()] returned.
#' @param sources The `toc_source`s.
#' @param rule_set A rule set.
#' @param skip Sheets that are not shells: a data frame `source`, `sheet`
#'   (the TOC sheets, the Topline sheet).
#' @return `reports` with `shell_file`, `shell_sheet` (`NA`: none);
#'   `attr(, "unlinked")`: sheets named like a report number no report
#'   links to (`source`, `sheet`).
#' @export
toc_link_shells <- function(reports, sources, rule_set, skip = NULL) {
  sh <- do.call(rbind, lapply(sources, function(s) {
    if (!length(s$sheets)) return(NULL)
    data.frame(source = s$name, sheet = s$sheets, stringsAsFactors = FALSE)
  }))
  if (is.null(sh)) sh <- data.frame(source = character(), sheet = character())
  if (!is.null(skip) && nrow(skip)) {
    sh <- sh[!paste(sh$source, sh$sheet) %in% paste(skip$source, skip$sheet), , drop = FALSE]
  }
  low <- tolower(trimws(sh$sheet))
  key <- .toc_key(sh$sheet)
  psuf <- .toc_list_keep(rule_set$settings$shell_phase_suffix)
  used <- rep(FALSE, nrow(sh))
  reports$shell_file <- rep(NA_character_, nrow(reports))
  reports$shell_sheet <- rep(NA_character_, nrow(reports))
  for (i in seq_len(nrow(reports))) {
    r <- reports[i, , drop = FALSE]
    bases <- unique(stats::na.omit(c(r$shell, r$raw_id, r$base_id)))
    sfx <- if (!is.na(r$phase) && r$phase <= length(psuf)) psuf[r$phase] else character()
    tries <- list(
      function() match(tolower(paste0(bases, if (length(sfx)) sfx else NA)), low),
      function() match(tolower(bases), low),
      function() if (length(sfx)) match(.toc_key(paste0(bases, sfx)), key) else NA,
      function() match(.toc_key(bases), key))
    for (f in tries) {
      k <- f()
      k <- k[!is.na(k)][1L]
      if (!is.na(k)) {
        reports$shell_file[i] <- sh$source[k]
        reports$shell_sheet[i] <- sh$sheet[k]
        used[k] <- TRUE
        break
      }
    }
  }
  named <- .toc_shell_named(sub("-[0-9]+$", "", sh$sheet))
  attr(reports, "unlinked") <- sh[named & !used, , drop = FALSE]
  reports
}

# ---- batches ------------------------------------------------------------------

#' The Topline batch of a TOC
#'
#' The reports whose Topline flag is one of the set's `topline_values` (a
#' row of two phases gives both reports) form the batch `topline_batch`.
#' No flag column: no batch.  The full run is every report and is not a
#' batch.  A Topline sheet, when there is one, is compared: a report
#' flagged but not on it, or on it but not flagged, is listed (a check,
#' never an error; the flag decides).
#'
#' @param reports What [toc_link_shells()] (or [toc_expand_phases()])
#'   returned.
#' @param rule_set A rule set.
#' @param topline_ids The output ids (before the phases) of a Topline
#'   sheet, or `NULL`.
#' @return A data frame `batch`, `output_id`; `attr(, "differ")`: `id`,
#'   `what` (`"flagged_not_on_sheet"`, `"on_sheet_not_flagged"`).
#' @export
toc_batches <- function(reports, rule_set, topline_ids = NULL) {
  out <- data.frame(batch = character(), output_id = character(),
                    stringsAsFactors = FALSE)
  differ <- data.frame(id = character(), what = character(), stringsAsFactors = FALSE)
  if (!nrow(reports) || all(is.na(reports$topline))) {
    attr(out, "differ") <- differ
    return(out)
  }
  st <- rule_set$settings
  vals <- .toc_norm(.toc_cands(st$topline_values))
  on <- !is.na(reports$topline) & .toc_norm(reports$topline) %in% vals
  if (any(on)) {
    out <- data.frame(batch = st$topline_batch, output_id = reports$output_id[on],
                      stringsAsFactors = FALSE)
  }
  if (!is.null(topline_ids)) {
    flagged <- unique(reports$base_id[on])
    tk <- .toc_key(topline_ids)
    fk <- .toc_key(flagged)
    a <- flagged[!fk %in% tk]
    b <- topline_ids[!tk %in% .toc_key(reports$base_id[on])]
    differ <- data.frame(id = c(a, b),
                         what = c(rep("flagged_not_on_sheet", length(a)),
                                  rep("on_sheet_not_flagged", length(b))),
                         stringsAsFactors = FALSE)
  }
  attr(out, "differ") <- differ
  out
}

# ---- problems -----------------------------------------------------------------

# The import's rules: level and message (the message is the key of
# strings.csv); #288's shape, area "toc"
.toc_rule_catalog <- data.frame(
  rule = c("TOC01", "TOC01b", "TOC02", "TOC03", "TOC04", "TOC04b", "TOC05", "TOC06",
           "TOC07", "TOC08", "TOC09", "TOC10", "TOC10b", "TOC11", "TOC12"),
  level = c("error", "error", "error", "error", "hand", "hand", "hand", "check",
            "check", "check", "check", "check", "check", "check", "check"),
  message = c(
    "No column for %s in %s: no report can be read.",
    "No report ID can be made in %s: no ID column, and the titles do not start with one (Table 14.1.1).",
    "Report ID %s is on more than one row (rows %s).",
    "Report ID %s has a character a file name cannot hold.",
    "Row %s has no report ID: not taken in.",
    "Row %s (%s) has no title: not taken in.",
    "Row %s: phase value %s is not in the rules (%s): not taken in.",
    "%s has no SAP number.",
    "SAP number %s is on more than one report: %s.",
    "No shell sheet for %s.",
    "Shell sheet %s is linked to no report.",
    "%s is flagged for %s but is not on the sheet %s.",
    "%s is on the sheet %s but is not flagged for %s.",
    "Sheets %s look like the TOC in %s: %s was taken (more rows).",
    "The rule set names %s, but the TOC has no column for it: skipped."),
  stringsAsFactors = FALSE)

.toc_problem <- function(rule, output_id = NA_character_, row = "", field = "",
                         args = character(), sheet = "") {
  k <- match(rule, .toc_rule_catalog$rule)
  tpl <- .toc_rule_catalog$message[k]
  args <- as.character(args)
  d <- data.frame(output_id = as.character(output_id),
                  level = .toc_rule_catalog$level[k], area = "toc",
                  sheet = sheet, row = as.character(row), field = field,
                  message = do.call(sprintf, c(list(tpl), as.list(args))),
                  template = tpl, hint = NA_character_, rule = sub("b$", "", rule),
                  draft = TRUE, stringsAsFactors = FALSE)
  d$args <- list(args)
  d
}

.toc_no_problems <- function() {
  d <- .toc_problem("TOC08", "x", args = "x")[0L, , drop = FALSE]
  d
}

#' @rdname toc_import_read
#' @param x What `toc_import_read()` returned.
#' @param lang The language of the messages (`"en"`, `"ja"`).
#' @return `toc_problems()`: the problems as rows of the review's shape
#'   (#288: `output_id`, `level` -- `error`, `check`, `hand` --, `area`
#'   `"toc"`, `sheet`, `row`, `field`, `message`, `hint`, `rule`, `draft`),
#'   errors first.
#' @export
toc_problems <- function(x, lang = "en") {
  p <- x$problems
  if (!nrow(p) || identical(lang, "en")) return(p)
  for (i in seq_len(nrow(p))) {
    loc <- tr(p$template[i], lang)
    m <- tryCatch(do.call(sprintf, c(list(loc), as.list(p$args[[i]]))),
                  error = function(e) NA_character_)
    if (!is.na(m)) p$message[i] <- m
  }
  p
}

# ---- the pipeline -------------------------------------------------------------

#' Take a TOC in by the company's rules
#'
#' `toc_import_read()` reads one TOC or several (a TFL workbook and a
#' listing workbook of the same layout) by the company's rules: each source
#' read once, its TOC sheet and header row found ([toc_find_sheet()]), the
#' rule set picked ([toc_pick_rules()]), the columns mapped
#' ([toc_map_for()]), the output ids made by the set's rule (or the one
#' that gives unique ids for the most rows, [toc_id_candidates()]), one
#' report a phase ([toc_expand_phases()]), the shell sheets linked
#' ([toc_link_shells()]), the Topline batch ([toc_batches()]) and what is
#' wrong ([toc_problems()]).  Nothing is written.
#'
#' `toc_import_spec()` gives the reports as [tflspec::tfl_read_toc()] gives
#' a TOC, for [toc_changes()] and [toc_apply()].
#'
#' What the user chose in the dialog (`choices`), else what the study's
#' profile remembers (`profile`), wins over what is found: `rule_set`,
#' `sheet` and `header_row` (named by the file's name), `map`, `id_rule`,
#' `type_from`, `overrides` (the set's settings changed for the study).
#'
#' @param sources The TOC files (paths) or `toc_source`s.
#' @param rules What [toc_rules()] returned.
#' @param profile The study's TOC profile ([toc_profile()]) or `NULL`.
#' @param choices A list (see above) or `NULL`.
#' @param names The sources' names as the user knows them (paths: their
#'   base names).
#' @return A `toc_import`: `sources`, `detection` (a row a source: `file`,
#'   `sheet`, `header_row`, `how` -- `"name"`, `"score"`, `"rows"`,
#'   `"chosen"`, `"profile"` --, `rows`, `shells`, `topline_sheet`, `kind`,
#'   `note`), `sheets` (each source's [toc_find_sheet()]), `rule_set` (the
#'   set in use), `rule_sets` (the scores), `map` (+ `attr(, "missing")`),
#'   `id_rule`, `id_candidates`, `type_from`, `rows`, `reports`, `unread`,
#'   `batches`, `problems`, `profile`, `notes` (what was not as the profile
#'   said: `message`).
#' @examples
#' f <- tempfile(fileext = ".csv")
#' writeLines(c("Output ID,Title 1,Title 2,Population",
#'              "T-14-1-1,Demographics,,Safety Population",
#'              "T-14-2-1,Adverse events,Overview,Safety Population"), f)
#' r <- toc_import_read(f, toc_rules(home = tempfile()))
#' r$reports[c("output_id", "label")]
#' toc_import_spec(r)$report
#' @export
toc_import_read <- function(sources, rules = toc_rules(), profile = NULL,
                            choices = NULL, names = NULL) {
  hr_max <- max(vapply(rules, function(s) s$settings$header_rows, 1L))
  srcs <- lapply(seq_along(sources), function(k) {
    s <- sources[[k]]
    if (inherits(s, "toc_source")) s else
      toc_source(s, hr_max, name = names[k] %||% basename(s))
  })
  pick_set <- choices$rule_set %||% profile$rule_set
  if (!is.null(pick_set) && !pick_set %in% names(rules)) pick_set <- NULL
  det_rules <- if (is.null(pick_set)) rules else rules[pick_set]
  prof_src <- function(file, key) {
    for (p in profile$sources %||% list()) {
      if (identical(p$file, file)) return(p[[key]])
    }
    NULL
  }
  sheets <- list()
  det <- list()
  notes <- data.frame(message = character(), template = character(), file = character(),
                      sheet = character(), stringsAsFactors = FALSE)
  for (s in srcs) {
    fs <- toc_find_sheet(s, det_rules)
    sheets[[s$name]] <- fs
    kind <- attr(fs, "kind")
    want <- .toc_pick(choices$sheet, s$name)
    how <- if (nrow(fs)) fs$why[1L] else ""
    if (is.null(want)) {
      want <- prof_src(s$name, "toc_sheet")
      if (!is.null(want) && want %in% s$sheets) how <- "profile"
      if (!is.null(want) && !want %in% s$sheets) {
        tpl <- "%s: the sheet %s is not in the file any more: found again."
        notes <- rbind(notes, data.frame(message = sprintf(tpl, s$name, want), template = tpl,
                                         file = s$name, sheet = want, stringsAsFactors = FALSE))
        want <- NULL
      }
    } else if (want %in% s$sheets) how <- "chosen" else want <- NULL
    sheet <- if (!is.null(want)) want else if (kind == "toc") fs$sheet[1L] else NA_character_
    hrow <- if (!is.na(sheet)) fs$header_row[match(sheet, fs$sheet)] else NA_integer_
    h_want <- .toc_pick(choices$header_row, s$name) %||%
      (if (identical(how, "profile")) prof_src(s$name, "header_row"))
    if (!is.null(h_want) && !is.na(suppressWarnings(as.integer(h_want)))) {
      hrow <- as.integer(h_want)
      if (!identical(how, "profile")) how <- "chosen"
    }
    if (!is.na(sheet) && is.na(hrow)) {
      hrow <- toc_find_header(s$head[[sheet]], det_rules)$row
    }
    if (!is.na(sheet) && sheet != fs$sheet[1L]) kind <- "toc"
    tl <- fs$sheet[fs$role == "topline" & fs$sheet != .or_na(sheet, "")]
    det[[s$name]] <- data.frame(
      file = s$name, sheet = sheet, header_row = hrow, how = how,
      rows = if (!is.na(sheet)) fs$rows[match(sheet, fs$sheet)] else 0L,
      shells = sum(.toc_shell_named(sub("-[0-9]+$", "", s$sheets))),
      topline_sheet = if (length(tl)) tl[1L] else NA_character_,
      kind = if (s$kind == "other") "other" else kind,
      two_toc = isTRUE(attr(fs, "two_toc")) && !how %in% c("chosen", "profile"),
      others = paste(fs$sheet[fs$role %in% c("other", "topline")], collapse = ", "),
      stringsAsFactors = FALSE)
  }
  detection <- do.call(rbind, det)
  rownames(detection) <- NULL
  readable <- which(!is.na(detection$sheet) & !is.na(detection$header_row))
  headers <- lapply(readable, function(k)
    .toc_header_names(srcs[[k]], detection$sheet[k], detection$header_row[k]))
  scores <- toc_pick_rules(if (length(headers)) headers else list(character()), rules)
  set_name <- pick_set %||% scores$rule_set[1L]
  set <- toc_rule_set(rules, set_name, overrides = c(choices$overrides, profile$overrides))
  # the columns: the set's map on each source's header; the user's map
  # where the source has its columns.  The dialog's selects show the first
  # source: a blank there ("none") leaves that source's column out, and
  # says nothing of another source's (a listing workbook's Population when
  # the TFL TOC has none)
  maps <- lapply(seq_along(srcs), function(k) {
    if (!k %in% readable) return(list())
    h <- headers[[match(k, readable)]]
    m <- toc_map_for(h, set)
    miss <- attr(m, "missing")
    for (it in names(choices$map)) {
      v <- choices$map[[it]]
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) {
        if (identical(k, readable[1L])) m[[it]] <- NULL
      } else if (all(.toc_norm(v) %in% .toc_norm(h))) {
        m[[it]] <- v
      }
    }
    attr(m, "missing") <- setdiff(miss, names(m))
    m
  })
  map <- if (length(readable)) maps[[readable[1L]]] else structure(list(), missing = character())
  rows <- do.call(rbind, c(list(.toc_no_rows()), lapply(readable, function(k)
    toc_rows(srcs[[k]], detection$sheet[k], detection$header_row[k], maps[[k]]))))
  # the output id and the type
  type_from <- choices$type_from %||% profile$type_from
  types <- .toc_types(rows, set, type_from)
  cands <- toc_id_candidates(rows, set, types$from)
  # the way chosen here; else the last time's, while it gives unique ids
  # to as many rows as the best way (a TOC of another layout: the best is
  # proposed, and said); else the rule set's; else the best
  id_rule <- choices$id_rule
  last_rule <- profile$id_rule
  if (is.null(id_rule) && !is.null(last_rule) && last_rule %in% cands$rule &&
      cands$ok[match(last_rule, cands$rule)] < cands$ok[1L]) {
    tpl <- "The report IDs as the last time (%s) are unique for fewer rows of this TOC: %s is proposed."
    notes <- rbind(notes, data.frame(message = sprintf(tpl, last_rule, cands$rule[1L]), template = tpl,
                                     file = last_rule, sheet = cands$rule[1L],
                                     stringsAsFactors = FALSE))
    last_rule <- cands$rule[1L]
  }
  id_rule <- id_rule %||% last_rule %||% set$settings$id_rule
  if (is.null(id_rule) || is.na(id_rule) || !id_rule %in% cands$rule) {
    id_rule <- if (nrow(cands)) cands$rule[1L] else "column"
  }
  labs <- .toc_labels(rows)
  # each row's cells as given (a heading row's text is in any column)
  given <- lapply(seq_len(nrow(rows)), function(i) {
    v <- c(unlist(rows[i, intersect(c(.toc_items, .toc_extra_items), names(rows))]),
           rows$titles[[i]], rows$footnotes[[i]])
    unique(v[!is.na(v)])
  })
  rows$raw_id <- rows$output_id
  rows$output_id <- .toc_ids_by(rows, id_rule, set, types)
  rows$output_id[!is.na(rows$output_id) & !nzchar(rows$output_id)] <- NA_character_
  rows$type <- types$type
  rows$type_column <- types$by$column
  rows$type_from <- vapply(seq_len(nrow(rows)), function(i) {
    k <- which(!is.na(unlist(types$by[i, c(types$from, setdiff(.toc_type_sources, types$from))])))
    if (length(k)) c(types$from, setdiff(.toc_type_sources, types$from))[k[1L]] else NA_character_
  }, "")
  rows$label <- labs$label
  rows$titles <- labs$titles
  # sections: the row's own, else the heading row above it (a row with no
  # id and one cell at most), per sheet
  cells_of <- function(i) given[[i]]
  n_cells <- vapply(seq_len(nrow(rows)), function(i) length(cells_of(i)), 1L)
  no_id <- is.na(rows$output_id)
  heading <- no_id & n_cells <= 1L
  cur <- NA_character_
  last_sheet <- ""
  sec <- rep(NA_character_, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    here <- paste(rows$.source[i], rows$.sheet[i])
    if (!identical(here, last_sheet)) cur <- NA_character_
    last_sheet <- here
    if (heading[i]) cur <- cells_of(i)[1L]
    sec[i] <- if (!is.na(rows$section[i])) rows$section[i] else cur
  }
  rows$section <- sec
  detection$rows <- vapply(detection$file, function(f) sum(rows$.source == f), 1L,
                           USE.NAMES = FALSE)
  has_title <- vapply(rows$titles, function(v) any(!is.na(v)), NA)
  # rows not read: no id (but more than a heading), or no title
  unread <- .toc_no_unread()
  for (i in which(no_id & !heading)) {
    unread[nrow(unread) + 1L, ] <- list(rows$.source[i], rows$.sheet[i], rows$.row[i],
                                        "TOC04", "output_id", NA_character_, NA_character_)
  }
  for (i in which(!no_id & !has_title)) {
    unread[nrow(unread) + 1L, ] <- list(rows$.source[i], rows$.sheet[i], rows$.row[i],
                                        "TOC04b", "title", NA_character_, rows$output_id[i])
  }
  take <- !no_id & has_title
  reports <- toc_expand_phases(rows[take, , drop = FALSE], set)
  unread <- rbind(unread, attr(reports, "unread"))
  attr(reports, "unread") <- NULL
  # the shells, across every source (not the TOC or Topline sheets)
  skip <- data.frame(source = detection$file,
                     sheet = detection$sheet, stringsAsFactors = FALSE)
  skip <- rbind(skip, data.frame(source = detection$file, sheet = detection$topline_sheet,
                                 stringsAsFactors = FALSE))
  for (k in seq_along(srcs)) {
    fs <- sheets[[k]]
    o <- fs$sheet[fs$role %in% c("other", "topline")]
    if (length(o)) skip <- rbind(skip, data.frame(source = srcs[[k]]$name, sheet = o,
                                                  stringsAsFactors = FALSE))
  }
  skip <- skip[!is.na(skip$sheet), , drop = FALSE]
  reports <- toc_link_shells(reports, srcs, set, skip)
  unlinked <- attr(reports, "unlinked")
  attr(reports, "unlinked") <- NULL
  # the Topline sheet, a cross-check: read through the same header
  # detection, map and id rule
  topline_ids <- NULL
  tl_where <- NULL
  for (k in seq_along(srcs)) {
    tl <- detection$topline_sheet[k]
    if (is.na(tl) || is.null(map$topline)) next
    hr <- toc_find_header(srcs[[k]]$head[[tl]], set)$row
    if (is.na(hr)) next
    tm <- toc_map_for(.toc_header_names(srcs[[k]], tl, hr), set)
    tr <- toc_rows(srcs[[k]], tl, hr, tm)
    tt <- .toc_types(tr, set, types$from)
    ids <- .toc_ids_by(tr, id_rule, set, tt)
    topline_ids <- unique(c(topline_ids, ids[!is.na(ids)]))
    tl_where <- tl
  }
  batches <- toc_batches(reports, set, topline_ids)
  reports$batches <- ifelse(reports$output_id %in% batches$output_id,
                            set$settings$topline_batch, NA_character_)
  res <- structure(list(
    sources = srcs, detection = detection, sheets = sheets,
    rule_set = set, rule_sets = scores, map = map, maps = maps,
    id_rule = id_rule, id_candidates = cands, type_from = types$from,
    rows = rows, reports = reports, unread = unread, batches = batches,
    unlinked = unlinked, notes = notes), class = "toc_import")
  res$problems <- .toc_find_problems(res, tl_where)
  res$profile <- .toc_profile_of(res, choices, profile)
  res
}

# a named vector's or list's element, NULL when it has none of that name
.toc_pick <- function(x, name) {
  if (is.null(x) || !name %in% names(x)) NULL else x[[name]]
}

#' @export
print.toc_import <- function(x, ...) {
  n <- table(factor(x$problems$level, c("error", "hand", "check")))
  cat(sprintf(paste0("<toc_import> %d source(s), rule set %s, id rule %s: %d reports ",
                     "(%d rows); %d errors, %d not read, %d checks\n"),
              length(x$sources), x$rule_set$name, x$id_rule, nrow(x$reports),
              nrow(x$rows), n[["error"]], n[["hand"]], n[["check"]]))
  invisible(x)
}

# Where a row is: its row number, with its file when there are several
.toc_where <- function(x, source, row) {
  if (length(x$sources) > 1L) paste(source, row) else as.character(row)
}

.toc_find_problems <- function(x, topline_sheet = NULL) {
  out <- list(.toc_no_problems())
  add <- function(...) out[[length(out) + 1L]] <<- .toc_problem(...)
  det <- x$detection
  set <- x$rule_set
  labs <- c(title = "the titles")
  for (k in seq_len(nrow(det))) {
    if (is.na(det$sheet[k])) next
    m <- x$maps[[k]]
    where <- paste0(det$file[k], ": ", det$sheet[k])
    if (is.null(m$title)) {
      add("TOC01", row = det$file[k], field = "title", args = c(labs[["title"]], where))
    } else if (nrow(x$rows) && !any(x$id_candidates$ok > 0L)) {
      add("TOC01b", row = det$file[k], field = "output_id", args = where)
    }
    if (isTRUE(det$two_toc[k]) && nzchar(det$others[k])) {
      add("TOC11", row = det$file[k],
          args = c(paste(c(det$sheet[k], det$others[k]), collapse = ", "), det$file[k],
                   det$sheet[k]))
    }
  }
  # rows not read
  u <- x$unread
  for (i in seq_len(nrow(u))) {
    w <- .toc_where(x, u$.source[i], u$.row[i])
    if (u$rule[i] == "TOC04") add("TOC04", row = w, field = "output_id", args = w)
    else if (u$rule[i] == "TOC04b") add("TOC04b", u$output_id[i], row = w, field = "title",
                                        args = c(w, u$output_id[i]))
    else add("TOC05", u$output_id[i], row = w, field = "phase",
             args = c(w, u$value[i], set$settings$phase_values))
  }
  r <- x$reports
  # ids twice (also across two workbooks), ids a file name cannot hold
  dup <- unique(r$output_id[duplicated(r$output_id)])
  for (id in dup) {
    i <- which(r$output_id == id)
    add("TOC02", id, row = paste(.toc_where(x, r$.source[i], r$.row[i]), collapse = ", "),
        field = "output_id",
        args = c(id, paste(.toc_where(x, r$.source[i], r$.row[i]), collapse = ", ")))
  }
  for (id in unique(r$output_id[grepl("[\\\\/:*?\"<>|]", r$output_id)])) {
    add("TOC03", id, field = "output_id", args = id)
  }
  miss <- attr(x$map, "missing") %||% character()
  # SAP numbers: blank, or on two reports (the phases of one row are one)
  if (!"sap_no" %in% miss && "sap_no" %in% set$map$item && .toc_has_item(x, "sap_no")) {
    # only the rows of a source whose TOC has the column: a listing workbook
    # with no SAP column says nothing of SAP numbers (#299: an optional item
    # whose column is absent is skipped silently)
    has <- vapply(x$maps %||% list(x$map), function(m) !is.null(m$sap_no), NA)
    from <- if (length(x$maps)) x$detection$file[has] else unique(r$.source)
    for (i in which(is.na(r$sap_no) & r$.source %in% from)) {
      add("TOC06", r$output_id[i], field = "sap_no", args = r$output_id[i])
    }
    key <- paste(r$.source, r$.sheet, r$.row)
    s <- r$sap_no
    for (v in unique(stats::na.omit(s))) {
      i <- which(s == v)
      if (length(unique(key[i])) > 1L) {
        add("TOC07", r$output_id[i[1L]], field = "sap_no",
            args = c(v, paste(r$output_id[i], collapse = ", ")))
      }
    }
  }
  # shells: none for a report; a shell sheet no report links to (only when
  # the sources hold shells at all)
  if (any(det$shells > 0L)) {
    for (i in which(is.na(r$shell_sheet))) add("TOC08", r$output_id[i], field = "shell",
                                               args = r$output_id[i])
    ul <- x$unlinked
    for (i in seq_len(nrow(ul))) {
      w <- paste0(ul$source[i], ": ", ul$sheet[i])
      add("TOC09", row = w, field = "shell", args = w)
    }
  }
  # the Topline sheet against the flag column
  d <- attr(x$batches, "differ")
  b <- set$settings$topline_batch
  for (i in seq_len(nrow(d))) {
    if (d$what[i] == "flagged_not_on_sheet") {
      add("TOC10", d$id[i], field = "topline", args = c(d$id[i], b, topline_sheet %||% ""))
    } else {
      add("TOC10b", d$id[i], field = "topline", args = c(d$id[i], topline_sheet %||% "", b))
    }
  }
  # optional items the set names that the TOC lacks: the feature skipped
  for (it in intersect(c("phase", "topline", "sap_no"), miss)) {
    add("TOC12", field = it, args = it)
  }
  p <- do.call(rbind, out)
  p <- p[order(match(p$level, c("error", "hand", "check"))), , drop = FALSE]
  rownames(p) <- NULL
  p
}

# What the study's profile keeps of an import (input/toc/profile.yml)
.toc_profile_of <- function(x, choices, profile) {
  det <- x$detection
  ov <- c(choices$overrides, profile$overrides)
  ov <- ov[!duplicated(names(ov))]
  list(rule_set = x$rule_set$name, id_rule = x$id_rule, type_from = x$type_from,
       overrides = if (length(ov)) ov else NULL,
       sources = lapply(seq_len(nrow(det)), function(k) {
         s <- list(file = det$file[k], toc_sheet = det$sheet[k],
                   header_row = det$header_row[k])
         if (!is.na(det$topline_sheet[k])) s$topline_sheet <- det$topline_sheet[k]
         s
       }))
}

# Whether any source's TOC has the item's column (x$map is the first
# source's: a listing workbook's Population when the TFL TOC has none)
.toc_has_item <- function(x, item) {
  any(vapply(x$maps %||% list(x$map), function(m) !is.null(m[[item]]), NA))
}

# The fixed map of the table toc_import_spec() hands to tfl_read_toc()
.toc_spec_frame <- function(x) {
  r <- x$reports
  n <- nrow(r)
  lines_of <- function(l) lapply(l, function(v) {
    v <- v[!is.na(v)]
    unlist(lapply(v, function(s) {
      p <- trimws(unlist(strsplit(s, "\n| \\| ")))
      p[nzchar(p)]
    }))
  })
  tl <- lines_of(r$titles)
  fl <- lines_of(r$footnotes)
  nt <- max(1L, lengths(tl))
  nf <- max(0L, lengths(fl))
  d <- data.frame(output_id = r$output_id, type = r$type, stringsAsFactors = FALSE)
  for (k in seq_len(nt)) d[[paste0("title_", k)]] <- vapply(tl, function(v)
    if (length(v) >= k) v[k] else NA_character_, "")
  d$population <- r$population
  for (k in seq_len(nf)) d[[paste0("footnote_", k)]] <- vapply(fl, function(v)
    if (length(v) >= k) v[k] else NA_character_, "")
  for (cn in c("program", "file", "note", "section", "datasets", "label")) d[[cn]] <- r[[cn]]
  map <- list(output_id = "output_id", type = "type",
              title = paste0("title_", seq_len(nt)), population = "population",
              program = "program", file = "file", note = "note", section = "section",
              datasets = "datasets", label = "label")
  if (nf) map$footnote <- paste0("footnote_", seq_len(nf))
  list(data = d, map = map)
}

#' @rdname toc_import_read
#' @return `toc_import_spec()`: what [tflspec::tfl_read_toc()] returns for
#'   the reports (a table spec with the report sheets and their
#'   attributes); `attr(, "guessed")` the reports whose type no type
#'   column said (read from the id or the title: to check).
#' @export
toc_import_spec <- function(x) {
  f <- .toc_spec_frame(x)
  d <- f$data
  if (!nrow(d)) {
    d[1L, ] <- NA
    d$output_id <- "(none)"
    d$title_1 <- "(none)"
  }
  sp <- .toc_read_frame(d, f$map)
  if (!nrow(f$data)) {
    sp$report <- sp$report[0L, , drop = FALSE]
    for (s in c("titles", "footnotes")) if (!is.null(sp[[s]])) sp[[s]] <- sp[[s]][0L, , drop = FALSE]
    for (a in c("sections", "datasets", "labels", "first_titles", "populations")) {
      attr(sp, a) <- attr(sp, a)[0L]
    }
  }
  r <- x$reports
  attr(sp, "guessed") <- r$output_id[is.na(r$type_column)]
  sp
}

# tflspec reads a data frame (D10); an older tflspec, the same rows as a
# .csv
.toc_read_frame <- function(d, map) {
  tryCatch(tflspec::tfl_read_toc(d, map = map), error = function(e) {
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    utils::write.csv(d, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    tflspec::tfl_read_toc(f, map = map)
  })
}
