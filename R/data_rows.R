# The four-steps screen, step 1 (data): the report's analysis data as a
# table model -- one row an analysis data, in build order, `adsl_sub` (the
# report's subjects) first.  Pure R, no UI.
#
# A report in tflspec's DP-2 shape has a `common:` block and a flat `data:`:
#
#   common: {population: SAF, treatment: TRT01A}
#   data:
#     - {id: adsl_sub, from: ADSL}          # no subjects: common: population
#     - {id: adae_sub, from: ADAE, subjects: adsl_sub, where: TRTEMFL == "Y"}
#
# A template report (`template: {name, args}`) has no `data:`: its rows are
# what the template builds from its data arguments (tflspec's ids:
# `adsl_sub` the subjects, `<data>_sub` (or `data_id`) the records,
# `<data>_den` a denominator built only when chosen), and each editable cell
# names the template argument it edits (`param`).
#
# A row: list(id, from, population, subjects, where, source, param, optional)
#   population  the population used (common: population when the row has none;
#               a row of subjects: its subjects' row's)
#   subjects    the id of the row whose subjects it keeps; NULL for adsl_sub
#   where       character, "" when none
#   source      "own" (the report's data:), "template", "common" (adsl_sub
#               made from common: alone)
#   param       a template report's: cell -> template argument (named list);
#               otherwise NULL
#   optional    TRUE for the template's denominator data, built only when chosen

# the keys of a data: entry the table edits; any other (derive, keep,
# distinct, code, add, label ...) is kept as it is
.DATA_ROW_KEYS <- c("id", "from", "population", "subjects", "where")

# a value as one line of text: R code (`!r`, tflspec's tfl_r or the YAML
# reader's fs_r) as its code; NULL / unset as ""
.data_row_text <- function(v) {
  if (is.null(v) || inherits(v, c("tfl_unset", "fs_unset"))) return("")
  if (inherits(v, "fs_r")) return(paste(v$code, collapse = "\n"))
  if (inherits(v, "tfl_r")) return(paste(unclass(v), collapse = "\n"))
  if (is.list(v)) v <- unlist(v, use.names = FALSE)
  v <- as.character(v)
  if (!length(v) || all(is.na(v))) return("")
  paste(v[!is.na(v)], collapse = ", ")
}

# the new text of a value in the class of the old one (R code stays R code);
# "" is no value (NULL)
.data_row_value <- function(text, old) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  if (inherits(old, "fs_r")) return(structure(list(code = text), class = class(old)))
  if (inherits(old, "tfl_r")) {
    return(structure(text, class = class(old), tag = attr(old, "tag") %||% "!r"))
  }
  text
}

.data_row <- function(id, from, population, subjects = NULL, where = "",
                      source = "own", param = NULL, optional = FALSE) {
  list(id = id, from = from, population = population, subjects = subjects,
       where = where, source = source, param = param, optional = optional)
}

.data_is_template <- function(report) {
  is.list(report$template) && !is.null(report$template$name)
}

# The report's analysis data as rows, in build order (adsl_sub first).
# `study` (.fs_study()-like) is not read yet: the population comes from the
# report; `template` the template's parameters (tflspec::tfl_template_params(),
# named), NULL: looked up by the report's template name.
.data_rows <- function(report, study = NULL, template = NULL) {
  common_pop <- .data_row_text(report$common$population)
  if (.data_is_template(report)) {
    if (is.null(template)) template <- .tpl_params(as.character(report$template$name))
    return(.data_rows_template(report, template, common_pop))
  }
  .data_rows_code(report, common_pop)
}

# a template report's rows: what the template builds from its data arguments
.data_rows_template <- function(report, template, common_pop) {
  prm <- .data_tpl_names(template)
  args <- report$template$args %||% list()
  val <- function(a) if (a %in% names(args)) args[[a]] else template[[a]]$default
  has <- function(a) a %in% prm
  pop <- if (has("population")) .data_row_text(val("population")) else ""
  if (!nzchar(pop)) pop <- common_pop
  map <- function(...) {
    m <- c(...)
    m <- m[vapply(m, has, NA)]
    as.list(m)
  }
  rows <- list(.data_row(
    "adsl_sub", "ADSL", pop, NULL, if (has("subset")) .data_row_text(val("subset")) else "",
    "template", map(population = "population", where = "subset")))
  ds <- if (has("data")) toupper(.data_row_text(val("data"))) else ""
  if (nzchar(ds) && !identical(ds, "ADSL")) {
    did <- .data_row_text(if (has("data_id")) val("data_id"))
    # (tflspec's ids: data_id itself, else <data>_sub; the denominator <base>_den)
    base <- if (nzchar(did)) did else tolower(ds)
    if (!nzchar(did)) did <- paste0(tolower(ds), "_sub")
    rows[[2L]] <- .data_row(
      did, ds, pop, "adsl_sub", if (has("where")) .data_row_text(val("where")) else "",
      "template", map(id = "data_id", from = "data", where = "where"))
    if (has("denominator")) {
      rows[[3L]] <- .data_row(paste0(base, "_den"), did, pop, "adsl_sub", "", "template",
                              stats::setNames(list(), character()), optional = TRUE)
    }
  }
  rows
}

# the names of a template's parameters (a list named by them, or of
# tfl_param()s with `name`)
.data_tpl_names <- function(template) {
  if (!length(template)) return(character())
  nm <- names(template)
  if (is.null(nm)) nm <- vapply(template, function(p) as.character(p$name %||% ""), "")
  nm
}

# the entries of a code SPEC report's data:
.data_defs <- function(report) {
  d <- report$data
  if (is.list(d) && length(d) && is.null(names(d))) d else list()
}

# a code SPEC report's rows: its data: (adsl_sub moved first), or adsl_sub
# from common: when data: has none
.data_rows_code <- function(report, common_pop) {
  defs <- .data_defs(report)
  ids <- vapply(defs, function(d) .data_row_text(d$id), "")
  first <- match("adsl_sub", ids, nomatch = 0L)
  ord <- c(if (first) first, setdiff(seq_along(defs), first))
  rows <- if (!first) list(.data_row("adsl_sub", "ADSL", common_pop, source = "common"))
  pops <- c(adsl_sub = if (!first) common_pop)
  for (k in ord) {
    d <- defs[[k]]
    subj <- if (is.null(d$subjects)) NULL else .data_row_text(d$subjects)
    pop <- .data_row_text(d$population)
    if (!nzchar(pop)) {
      pop <- if (!is.null(subj) && subj %in% names(pops)) pops[[subj]] else common_pop
    }
    pops[[ids[k]]] <- pop
    rows[[length(rows) + 1L]] <- .data_row(
      ids[k], .data_row_text(d$from), pop, subj, .data_row_text(d$where), "own")
  }
  rows
}

# The report with the rows written back.  A code SPEC report: its data:
# from the rows of source "own", each entry's other keys kept.  A template
# report: its template arguments changed through the rows' `param`
# (.data_rows_args(); `template` as .data_rows() takes it); no data:.
# .data_rows_write(r, .data_rows(r, s)) is r.
.data_rows_write <- function(report, rows, template = NULL) {
  if (.data_is_template(report)) {
    ch <- .data_rows_args(report, rows, template)
    if (!length(ch)) return(report)
    args <- report$template$args %||% list()
    for (a in names(ch)) {
      if (is.null(ch[[a]])) args[a] <- NULL else args[[a]] <- ch[[a]]
    }
    report$template$args <- args
    return(report)
  }
  .data_rows_write_code(report, rows)
}

# A template report: the template arguments the rows change, named; a NULL
# value removes the argument (its default applies).  Each row's `param`
# names the argument a cell edits; a cell whose text is the argument's
# value now changes nothing.
.data_rows_args <- function(report, rows, template = NULL) {
  args <- report$template$args %||% list()
  if (is.null(template)) template <- .tpl_params(as.character(report$template$name))
  cur <- function(a) {
    if (a %in% names(args)) return(args[[a]])
    if (a %in% .data_tpl_names(template)) return(template[[a]]$default)
    NULL
  }
  common_pop <- .data_row_text(report$common$population)
  out <- list()
  for (r in rows) {
    p <- r$param
    if (!length(p)) next
    for (cell in names(p)) {
      a <- as.character(p[[cell]])
      new <- .data_row_text(r[[cell]])
      old <- cur(a)
      now <- .data_row_text(old)
      if (identical(cell, "population") && !nzchar(now)) now <- common_pop
      if (identical(cell, "id")) {
        # the records' id: data_id only when it is not <data>_sub
        now <- if (nzchar(now)) now else paste0(tolower(.data_row_text(r$from)), "_sub")
        if (identical(new, now)) next
        dflt <- paste0(tolower(.data_row_text(r$from)), "_sub")
        out[a] <- list(if (identical(new, dflt)) NULL else new)
        next
      }
      if (identical(new, now)) next
      if (identical(cell, "population") && identical(new, common_pop)) {
        out[a] <- list(NULL)
        next
      }
      out[a] <- list(.data_row_value(new, old))
    }
  }
  # (an argument removed that the report does not give: no change)
  keep <- vapply(names(out), function(a) !is.null(out[[a]]) || a %in% names(args), NA)
  out[keep]
}

# a code SPEC report's data: from the rows of source "own"
.data_rows_write_code <- function(report, rows) {
  defs <- .data_defs(report)
  ids <- vapply(defs, function(d) .data_row_text(d$id), "")
  own <- Filter(function(r) identical(r$source, "own"), rows)
  common_pop <- .data_row_text(report$common$population)
  pops <- c(adsl_sub = common_pop)
  out <- list()
  for (r in own) {
    k <- match(r$id, ids, nomatch = 0L)
    d <- if (k) defs[[k]] else list(id = r$id)
    subj <- if (is.null(r$subjects) || !nzchar(r$subjects)) NULL else r$subjects
    inherited <- if (!is.null(subj) && subj %in% names(pops)) pops[[subj]] else common_pop
    pop <- r$population %||% ""
    pops[[r$id]] <- if (nzchar(pop)) pop else inherited
    d <- .data_set_key(d, "id", r$id)
    d <- .data_set_key(d, "from", r$from)
    # (the population written when the entry has it or it is not the inherited one)
    d <- if (!is.null(d$population) || !identical(pop, inherited)) {
      .data_set_key(d, "population", pop)
    } else d
    d <- .data_set_key(d, "subjects", subj %||% "")
    d <- .data_set_key(d, "where", r$where)
    out[[length(out) + 1L]] <- d
  }
  # data: in its own order when the rows only moved adsl_sub first
  new_ids <- vapply(own, function(r) as.character(r$id), "")
  if (setequal(new_ids, ids) && length(new_ids) == length(ids)) {
    first <- match("adsl_sub", ids, nomatch = 0L)
    if (identical(new_ids, ids[c(if (first) first, setdiff(seq_along(ids), first))])) {
      out <- out[match(ids, new_ids)]
    }
  }
  if (!length(out) && is.null(report$data)) return(report)
  report["data"] <- list(out)
  report
}

# an entry's key set to a text (in the class of its value now: R code stays
# R code); a text the same as now keeps the value as it is; "" removes the key
.data_set_key <- function(d, key, text) {
  text <- .data_row_text(text)
  old <- d[[key]]
  if (identical(text, .data_row_text(old))) return(d)
  if (!nzchar(text)) {
    d[key] <- NULL
    return(d)
  }
  d[[key]] <- .data_row_value(text, old)
  d
}
