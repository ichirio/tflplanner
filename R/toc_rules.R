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
#' @noRd
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
#' @noRd
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
#' @noRd
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
