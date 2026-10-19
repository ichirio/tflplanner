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
#' @export
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

# ---- the builder's rows from the report's items, and back (#417) ------------
#
# The builder's form reads a table as the `tables` and `variables` rows it
# always has: here they are made from the report's items (its YAML over its
# layers) -- table_plan()'s roles, plan_filter()'s condition, plan_levels(),
# plan_labels() and plan_nest() -- and an edit of them is written back into
# those items, not into the sheets.

# The arguments of a verb as the plan has them (NULL: no such item)
.builder_args <- function(its, fn) {
  for (it in its) {
    if (identical(it$fn, fn) && !isFALSE(it$present)) return(it$args %||% list())
  }
  NULL
}

# An item's argument as R takes it (rows: c(group1 = "AEBODSYS"); a constant
# heading ~"Overall"); NULL when it has none or it is not a constant
.item_arg_r <- function(fn, args, name) {
  if (is.null(args[[name]])) return(NULL)
  txt <- tflspec::tfl_args_to_r(fn, args[name])
  v <- tryCatch(eval(str2lang(sprintf("list(%s)", txt)), baseenv()), error = function(e) NULL)
  v[[name]]
}

# ... and an R value as the item holds it
.item_arg_from_r <- function(fn, name, value) {
  if (is.null(value)) return(NULL)
  txt <- paste(name, "=", paste(deparse(value, width.cutoff = 500L), collapse = " "))
  tflspec::tfl_args_from_r(fn, txt)[[name]]
}

# A role (table_plan()'s rows or label, as R) as the tables sheet says it:
# `name = column | ...`, a constant heading quoted; "NA" for none
.role_text <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (!is.list(v) && length(v) == 1L && is.na(v)) return("NA")
  nm <- names(v) %||% rep("", length(v))
  val <- vapply(seq_along(v), function(i) {
    e <- v[[i]]
    if (inherits(e, "formula")) paste0("\"", eval(e[[2L]], baseenv()), "\"") else
      as.character(e)[1L]
  }, "")
  paste(ifelse(nzchar(nm) & nm != val, paste(nm, "=", val), val), collapse = " | ")
}

# ... and read back as R (tflspec's reading of the sheet): NULL for blank,
# NA for "NA"
.role_value <- function(txt) {
  if (.is_blank(txt)) return(NULL)
  if (identical(trimws(txt), "NA")) return(NA)
  parts <- trimws(strsplit(txt, "|", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  one <- function(x) {
    m <- regmatches(x, regexec("^([A-Za-z.][A-Za-z0-9._]*)\\s*=\\s*(.+)$", x))[[1L]]
    nm <- if (length(m)) m[2L] else ""
    v <- trimws(if (length(m)) m[3L] else x)
    q <- regmatches(v, regexec("^([\"'])(.*)\\1$", v))[[1L]]
    list(name = nm, value = if (length(q)) eval(call("~", q[3L]), baseenv()) else v)
  }
  ps <- lapply(parts, one)
  nms <- vapply(ps, `[[`, "", "name")
  vals <- lapply(ps, `[[`, "value")
  if (any(vapply(vals, inherits, NA, "formula"))) {
    own <- !nzchar(nms) & !vapply(vals, inherits, NA, "formula")
    nms[own] <- unlist(vals[own])
    return(stats::setNames(vals, nms))
  }
  v <- unlist(vals)
  if (any(nzchar(nms))) names(v) <- nms
  v
}

# The report's own `tables` row (one; blank cells where it has none), its
# roles (cols, rows, label) and its row filter (keep_where) the items'
.builder_tables_own <- function(x, id, its) {
  tb <- sheet_rows(x, "tables", id)
  tb$output_id <- NULL
  if (!nrow(tb)) tb[1L, ] <- NA
  tb <- tb[1L, , drop = FALSE]
  for (k in c("cols", "rows", "label", "keep_where", "sort", "header_n")) {
    if (!k %in% names(tb)) tb[[k]] <- NA_character_
  }
  tp <- .builder_args(its, "table_plan")
  if (!is.null(tp)) {
    cols <- as.character(unlist(tp$cols))
    tb$cols[1L] <- if (length(cols)) paste(cols, collapse = " | ") else NA_character_
    tb$rows[1L] <- .role_text(.item_arg_r("table_plan", tp, "rows"))
    tb$label[1L] <- if ("label" %in% names(tp)) .role_text(.item_arg_r("table_plan", tp, "label")) else
      NA_character_
  }
  pf <- .builder_args(its, "plan_filter")
  tb$keep_where[1L] <- .filter_where_text(pf$where) %||% NA_character_
  # a hierarchy's order (plan_sort(keys = )); whose {n} the header prints
  # (plan_col_header(values = ))
  ps <- .builder_args(its, "plan_sort")
  keys <- as.character(unlist(ps$keys))
  tb$sort[1L] <- if (length(keys)) paste(keys, collapse = " | ") else NA_character_
  tb$header_n[1L] <- .header_n_text(.builder_args(its, "plan_col_header")$values)
  tb
}

# plan_col_header(values = ) as the tables sheet's header_n: "n" alone, else
# `name = value | ...`; NA none
.header_n_text <- function(v) {
  if (is.null(v) || !length(v)) return(NA_character_)
  v <- lapply(v, function(e) as.character(unlist(e))[1L])
  if (identical(names(v), "n")) return(v$n)
  paste(paste(names(v), "=", unlist(v)), collapse = " | ")
}

# ... and back
.header_n_values <- function(txt) {
  if (.is_blank(txt)) return(NULL)
  parts <- trimws(strsplit(txt, "|", fixed = TRUE)[[1L]])
  if (!all(grepl("=", parts, fixed = TRUE))) return(list(n = trimws(txt)))
  stats::setNames(as.list(trimws(sub("^[^=]*=", "", parts))), trimws(sub("=.*$", "", parts)))
}

# A hierarchy's outermost row key a page each: plan_paginate_group(col =
# "group1")
.builder_row_pages <- function(its) {
  pg <- .builder_args(its, "plan_paginate_group")
  !is.null(pg) && identical(as.character(unlist(pg$col))[1L], "group1")
}

# ... the rows the form reads: the report's own row over its defaults'
.builder_tables_rows <- function(x, id, its) {
  inh <- inherited_rows(x, "tables", id)
  inh$output_id <- NULL
  own <- .builder_tables_own(x, id, its)
  for (k in setdiff(names(inh), names(own))) own[[k]] <- NA_character_
  for (k in setdiff(names(own), names(inh))) inh[[k]] <- rep(NA_character_, nrow(inh))
  # a cell the report leaves blank is its defaults' (the roles are the
  # plan's already)
  if (nrow(inh)) {
    for (k in setdiff(names(own), c("cols", "rows", "label", "keep_where", "sort", "header_n"))) {
      if (is.na(own[[k]][1L])) own[[k]][1L] <- inh[[k]][1L]
    }
  }
  own
}

# The `variables` rows of a report as its items say them: a variable's
# heading (plan_labels()), its values in order and its empty values hidden
# (plan_levels(), `.drop_empty`), the variables' order (plan_levels(variable
# = )), its rows under a level of another (plan_nest(): "RACE: Asian", placed
# after that variable).  Values in the order of the report's code list
# (`cl`, its rows of the codelists sheet) are the code list's, not the
# table's own: no `levels` (tflspec writes them so from the code list).
.builder_vars_rows <- function(its, cl = NULL) {
  lv <- .builder_args(its, "plan_levels") %||% list()
  lb <- .builder_args(its, "plan_labels") %||% list()
  ne <- .builder_args(its, "plan_nest") %||% list()
  heading <- function(v) is.character(v) && length(v) == 1L && is.null(names(v))
  lb <- lb[vapply(lb, heading, NA)]
  ord <- as.character(unlist(lv$variable))
  drop <- as.character(unlist(lv$.drop_empty))
  keys <- setdiff(names(lv), c("variable", ".drop_empty"))
  under <- vapply(ne, function(u) {
    u <- unlist(u)
    if (length(u) == 1L && !is.null(names(u))) paste0(names(u), ": ", u) else NA_character_
  }, "")
  vars <- unique(c(ord, names(lb), keys, names(under), drop))
  # each nested variable after its parent and the parent's children so far
  seq_v <- ord
  for (v in names(under)) {
    if (v %in% seq_v || is.na(under[[v]])) next
    p <- trimws(sub(":.*$", "", under[[v]]))
    at <- match(p, seq_v)
    if (is.na(at)) next
    par_of <- function(w) if (w %in% names(under)) trimws(sub(":.*$", "", under[[w]])) else NA
    while (at < length(seq_v) && identical(par_of(seq_v[at + 1L]), p)) at <- at + 1L
    seq_v <- append(seq_v, v, after = at)
  }
  n <- length(vars)
  data.frame(
    variable = vars,
    label = if (n) vapply(vars, function(v) lb[[v]] %||% NA_character_, "", USE.NAMES = FALSE) else
      character(),
    order = as.character(match(vars, seq_v)),
    levels = if (n) vapply(vars, function(v) {
      l <- as.character(unlist(lv[[v]]))
      if (length(l) && identical(l, .codelist_order(cl, v))) l <- character()
      if (length(l)) paste(l, collapse = " | ") else NA_character_
    }, "", USE.NAMES = FALSE) else character(),
    empty_levels = ifelse(vars %in% drop, "hide", NA_character_),
    under = if (n) unname(under[vars]) else character(),
    stringsAsFactors = FALSE)
}

# The items the builder's rows stand for, as written back: table_plan()'s
# roles from `tb` (the report's own tables row as edited), plan_levels(),
# plan_labels() and plan_nest() from `vr` (the variables rows as edited;
# `vr_was` as they were read) -- each verb's whole arguments, those the rows
# do not say kept as they are; a variable with no levels of its own takes
# its code list's order (`cl`), as tflspec writes it.  A list of list(fn,
# args, drop_empty) for the verbs that change; none: nothing to write.
.builder_items_from_rows <- function(its, tb, vr, vr_was, cl = NULL) {
  out <- list()
  same <- function(fn, a, b) {
    identical(a, b) || identical(tflspec::tfl_args_to_r(fn, a %||% list()),
                                 tflspec::tfl_args_to_r(fn, b %||% list()))
  }
  put <- function(fn, new, old) {
    if (!same(fn, new, old)) out[[length(out) + 1L]] <<- list(fn = fn, args = new,
                                                              drop_empty = TRUE)
  }
  tp <- .builder_args(its, "table_plan") %||% list()
  new <- tp
  cols <- .split_list(tb$cols[1L])
  new$cols <- if (length(cols)) .item_arg_from_r("table_plan", "cols", cols)
  new$rows <- .item_arg_from_r("table_plan", "rows", .role_value(tb$rows[1L]))
  lab <- .role_value(tb$label[1L])
  if (is.null(lab)) new$label <- NULL else new["label"] <- list(.item_arg_from_r("table_plan", "label", lab))
  if (!same("table_plan", new, tp)) {
    out[[length(out) + 1L]] <- list(fn = "table_plan", args = new)
  }
  vars <- unique(c(vr_was$variable, vr$variable))
  row_of <- function(v, k) {
    i <- match(v, vr$variable)
    if (is.na(i) || is.null(vr[[k]])) NA_character_ else vr[[k]][i]
  }
  # plan_levels(): the variables' order first, then the values of each
  lv <- .builder_args(its, "plan_levels") %||% list()
  new <- lv
  for (v in vars) {
    l <- .split_list(row_of(v, "levels"))
    if (!length(l)) l <- .codelist_order(cl, v)
    if (length(l)) new[[v]] <- l else new[[v]] <- NULL
  }
  o <- suppressWarnings(as.numeric(vr$order))
  nested <- !is.na(vr$under %||% rep(NA, nrow(vr))) & nzchar(vr$under %||% rep("", nrow(vr)))
  k <- !is.na(o) & !nested
  vo <- vr$variable[k][order(o[k])]
  if (length(vo)) new$variable <- vo else new$variable <- NULL
  hide <- vr$variable[!is.na(vr$empty_levels) & tolower(vr$empty_levels) == "hide"]
  keep_drop <- setdiff(as.character(unlist(lv$.drop_empty)), vars)
  drop <- unique(c(keep_drop, hide))
  if (length(drop)) new$.drop_empty <- drop else new$.drop_empty <- NULL
  first <- intersect("variable", names(new))
  new <- new[c(first, setdiff(names(new), first))]
  put("plan_levels", new, lv)
  # plan_labels(): a variable's heading (its values' labels kept)
  lb <- .builder_args(its, "plan_labels") %||% list()
  new <- lb
  for (v in vars) {
    cur <- lb[[v]]
    if (!is.null(cur) && !(is.character(cur) && length(cur) == 1L && is.null(names(cur)))) next
    l <- row_of(v, "label")
    if (is.na(l) || !nzchar(l)) new[[v]] <- NULL else new[[v]] <- l
  }
  # (the variables moved: their headings in their new order, as the
  # program lists them)
  if (!identical(vo, as.character(unlist(lv$variable)))) {
    by_o <- vr$variable[order(is.na(o), o)]
    r <- match(names(new), by_o)
    new <- new[order(is.na(r), r)]
  }
  put("plan_labels", new, lb)
  # plan_nest(): rows under a level of another variable
  ne <- .builder_args(its, "plan_nest") %||% list()
  new <- ne
  for (v in vars) {
    u <- row_of(v, "under")
    if (is.na(u) || !nzchar(trimws(u))) new[[v]] <- NULL else
      new[[v]] <- stats::setNames(list(trimws(sub("^[^:]*:", "", u))), trimws(sub(":.*$", "", u)))
  }
  put("plan_nest", new, ne)
  out
}

# Read and write a table the builder way (internal, #417)
#
# `builder_read()` describes a report's table from its items (its YAML over
# its layers; the sheets while they hold edits the spec has not taken yet)
# and what its ARD holds (`meta`: ard_meta(), for what the plan does not
# say yet); `builder_write()` writes such a description (`state`) back into
# the report's items: table_plan(), plan_levels(), plan_labels(),
# plan_nest(), plan_cells(), plan_sort(), plan_paginate_group(),
# plan_col_header() -- each only where it differs from `was` (what
# builder_read() returned before the edit; NULL: the whole description).
# What the builder does not show (a statistic's own template, a condition,
# an order of one's own) stays as the plan has it.
#
# builder_read() returns a list: `key` (the column variables, outermost
# first), `arms` (each column variable's levels in order), `variables` (a
# data frame: variable, kind, label, hint, under), `levels`, `rows` (a
# continuous variable's row labels in order) and their `templates`,
# `value` ("stat" or "stat_fmt"), `digits` (each statistic the rows print:
# its decimals), `exceptions` (a variable's own: variable, statistic,
# digits), `cat_format`, `cat_own`, `pct_decimals`, `header` ("keep" or a
# name of header_presets()), `auto_levels`, `level_from`, `layers`,
# `layer_choices`, `row_label`, `level_ard` (a variable's levels in the
# ARD's order, where the ARD keeps them as a factor), `variable_ard` (the
# analysis variables in the ARD's order), and for a hierarchy (SOC / PT) its
# `hierarchy`, `row_keys`, `row_choices`, `row_pages` (the outermost a
# page each) and `sort` (each row key's order: "alpha" or "freq", or
# "own": the plan's own, left as it is).  builder_write() returns the
# tflplanner.
builder_read <- function(x, output_id, meta = NULL) {
  id <- output_id
  # the roles, the variables' headings, order and levels: the report's
  # items (its YAML over its layers)
  its <- builder_items(x, id, meta)
  tb <- .builder_tables_rows(x, id, its)
  # (from the spec; while the sheets hold edits it has not taken yet, or
  # the planner has none, the sheets' rows: a code list's order is then
  # the code list's, not the table's own)
  in_spec <- !is.null(.spec_items(x, id))
  # the column variables, outermost first (tables$cols "A | B")
  key <- .split_list(tb$cols[1L])
  if (!length(key) && !is.null(meta) && length(meta$by)) key <- meta$by[1L]
  if (!length(key)) key <- NA_character_

  cl <- tryCatch(sheet_rows(x, "codelists", id), error = function(e) NULL)
  vr <- if (in_spec) .builder_vars_rows(its, cl) else {
    v <- .rows_for(x, "variables", id)
    v[!duplicated(v$variable), , drop = FALSE]
  }
  # a variable's levels in order, by .level_order() (this table's own, the
  # report's code list, the ARD's factor, as first seen) with where they
  # come from (`level_from`); those no sheet states are remembered, so
  # that writing them back unchanged writes nothing
  auto <- list()
  from <- list()
  # (the ARD's own order of a variable it keeps as a factor: an order the
  # same is not written in the code, tflspec #355)
  lard <- list()
  # the columns the plan makes (the stack's subgroup, subgroup_level ...;
  # .stat_group): their order is the plan's, not their code list's
  plan_made <- c(.stack_keys(tb$stack[1L], tb$stack_nest[1L]), ".stat_group")
  lev_of <- function(v) {
    own <- .split_list(vr$levels[match(v, vr$variable)])
    ard <- if (is.null(meta)) character() else if (v %in% names(meta$keys)) meta$keys[[v]] else
      .split_list(meta$variables$levels[match(v, meta$variables$variable)])
    fac <- v %in% c(meta$factor_vars, meta$factor_keys)
    o <- .level_order(own, .codelist_order(cl, v), ard, fac, plan_made = v %in% plan_made)
    if (!identical(o$source, "table") && (!is.null(meta) || identical(o$source, "codelist"))) {
      auto[[v]] <<- o$levels
    }
    if (length(o$levels)) from[[v]] <<- o[c("source", "differs")]
    if (fac && length(ard)) lard[[v]] <<- as.character(ard)
    o$levels
  }
  arms <- if (!anyNA(key)) stats::setNames(lapply(key, lev_of), key) else
    list()

  mv <- if (!is.null(meta)) meta$variables else
    data.frame(variable = character(), kind = character(),
               label = character(), stringsAsFactors = FALSE)
  own <- vr[!is.na(vr$variable) & !vr$variable %in% c(key, names(meta$keys)),
            , drop = FALSE]
  vars <- unique(c(mv$variable, own$variable))
  ord <- suppressWarnings(as.numeric(vr$order[match(vars, vr$variable)]))
  vars <- vars[order(is.na(ord), ord, match(vars, mv$variable))]
  label <- vr$label[match(vars, vr$variable)]
  # where the variables sheet says none: the ARD's own label (a hint shown
  # faint: none now -- the code lists say what the data's values become,
  # not the variables' headings)
  hint <- rep(NA_character_, length(vars))
  mlab <- mv$label[match(vars, mv$variable)]
  use_m <- is.na(label)
  label[use_m] <- mlab[use_m]
  kind <- mv$kind[match(vars, mv$variable)]
  kind[is.na(kind)] <- ifelse(
    vapply(vars[is.na(kind)], function(v) length(lev_of(v)) > 0, NA),
    "categorical", "continuous")
  # under: its rows under a level of another variable (RACE: Asian)
  under <- if (!is.null(vr$under)) vr$under[match(vars, vr$variable)] else
    rep(NA_character_, length(vars))
  variables <- data.frame(variable = vars, kind = kind, label = label,
                          hint = hint, under = under, stringsAsFactors = FALSE)
  levels <- stats::setNames(lapply(vars, function(v)
    if (identical(kind[match(v, vars)], "categorical")) lev_of(v) else
      character()), vars)

  ce <- .rows_for(x, "cells", id)
  cont <- ce[!is.na(ce$variable) & ce$variable == "continuous" &
               !is.na(ce$row), , drop = FALSE]
  rows <- .first_seen_chr(cont$row)
  templates <- stats::setNames(cont$template[match(rows, cont$row)], rows)
  if (!length(rows)) {
    bs <- builder_stats()
    rows <- intersect(.builder_default_rows, bs$row)
    templates <- stats::setNames(bs$template[match(rows, bs$row)], rows)
  }

  # the value the table prints: the number, rounded here, or the ARD's text
  tv <- if ("value" %in% names(tb)) tb$value[!is.na(tb$value)] else character()
  value <- if (identical(tv[1L], "stat_fmt")) "stat_fmt" else "stat"

  # the decimals of the statistics the rows print: the report's rule (its
  # own, the study's, the standards'); an older table wrote them on its rows
  used <- unique(unlist(lapply(templates, .template_stats)))
  rule <- .report_digits(x, id)
  rowdig <- unlist(lapply(seq_len(nrow(cont)), function(i)
    .row_digits(cont$template[i], cont$digits[i])))
  digits <- stats::setNames(rule[used], used)
  from_rows <- intersect(names(rowdig), used)
  digits[from_rows] <- rowdig[from_rows][!duplicated(names(rowdig[from_rows]))]
  digits <- digits[!is.na(digits)]

  own_dg <- sheet_rows(x, "digits", id)
  exc <- own_dg[!is.na(own_dg$variable), c("variable", "statistic", "digits"),
                drop = FALSE]
  exc$digits <- as.integer(exc$digits)
  # (the report's YAML: a variable's own rows -- the continuous rows again,
  # other decimals in their templates -- are its own decimals)
  if (!nrow(exc) && nrow(cont)) {
    bare <- function(t) gsub(":[^}]*}", "}", t)
    own_v <- ce[!is.na(ce$variable) & ce$variable %in% vars & !is.na(ce$row), , drop = FALSE]
    for (v in unique(own_v$variable)) {
      r <- own_v[own_v$variable == v, , drop = FALSE]
      same_rows <- setequal(r$row, cont$row) &&
        identical(bare(r$template[match(cont$row, r$row)]), bare(cont$template))
      if (!same_rows) next
      d <- unlist(lapply(r$template, .inline_digits))
      d <- d[!duplicated(names(d))]
      for (k in names(d)) {
        if (!identical(unname(d[[k]]), unname(digits[k]))) {
          exc[nrow(exc) + 1L, ] <- list(v, k, as.integer(d[[k]]))
        }
      }
    }
  }
  rownames(exc) <- NULL

  cat_row <- ce[(!is.na(ce$variable) & ce$variable == "categorical") |
                  (is.na(ce$variable) & is.na(ce$row)), , drop = FALSE]
  cr <- .cat_read(cat_row$template[1L])
  cat_format <- cr$key
  pct_decimals <- cr$pct
  cat_own <- cr$own %||% NA_character_

  # a hierarchy table (SOC / PT): its terms (the ARD's), the table's rows
  # -- the layout is the table's: the ARD's keys, by default its other
  # groups then its hierarchy -- their order, and a page for the outermost
  hier <- if (!is.null(meta)) meta$hierarchy %||% character() else character()
  rk <- if (length(hier)) .row_keys_read(tb$rows[1L], tb$label[1L]) else character()
  if (length(hier) && !length(rk)) rk <- c(setdiff(meta$by %||% character(), key), hier)
  # the rows as layers, outermost first, for both kinds: a hierarchy's
  # row keys; a summary table's keys over its block of variables
  # (".variable": group = variable)
  if (length(hier)) {
    layers <- rk
  } else {
    layers <- .row_layers_read(tb$rows[1L])
  }
  layer_choices <- .layer_choices(names(meta$keys), key, vars, length(hier) > 0L,
                                  tb$stack[1L], tb$stack_nest[1L])
  list(key = key, arms = arms, variables = variables, levels = levels,
       rows = rows, templates = templates, value = value, digits = digits,
       exceptions = exc, cat_format = cat_format, cat_own = cat_own,
       pct_decimals = pct_decimals, header = "keep", auto_levels = auto,
       level_from = from, level_ard = lard,
       variable_ard = if (!is.null(meta)) intersect(mv$variable, vars) else character(),
       hierarchy = hier, row_keys = rk,
       row_choices = if (length(hier)) setdiff(names(meta$keys), key) else character(),
       row_pages = length(hier) > 0L && .builder_row_pages(its),
       sort = if (length(hier)) .hier_sort_read(tb$sort[1L], length(rk)),
       layers = layers, layer_choices = unique(c(layers, layer_choices)),
       row_label = if (length(hier)) utils::tail(rk, 1L) else .row_label_read(tb$label[1L]))
}

# The rows' candidates: the normalized ARD's keys less the columns' (one
# column is in cols or rows, not both); a summary table's less its
# analysis variables (values of variable, not columns), with the keys a
# stack adds -- subgroup, subgroup_level, though the ARD read has not got
# them yet -- and the block of the analysis variables (".variable")
.layer_choices <- function(keys, cols, vars, hier = FALSE, stack = NA, nest = NA) {
  keys <- keys %||% character()
  if (hier) return(setdiff(keys, cols))
  unique(c(setdiff(keys, c(cols, vars)), .stack_keys(stack, nest), ".variable"))
}

# The keys normalize_ard()'s stack adds (tables$stack "subgroup = ALL, SEX
# | ..."): each name and its _level, and its _outer when the stack nests
# (tables$stack_nest TRUE); none for no stack
.stack_keys <- function(stack, nest = NA) {
  if (.is_blank(stack)) return(character())
  part <- trimws(strsplit(stack, "|", fixed = TRUE)[[1L]])
  nm <- ifelse(grepl("=", part, fixed = TRUE), trimws(sub("=.*$", "", part)), "subgroup")
  nm <- unique(nm[nzchar(nm)])
  nested <- !.is_blank(nest) && isTRUE(as.logical(toupper(trimws(nest))))
  unlist(lapply(nm, function(n) c(n, if (nested) paste0(n, "_outer"), paste0(n, "_level"))))
}

# A summary table's row text (tables$label): ".label" (blank: what
# normalize_ard() builds), a key ("label = SEX"), NA (none: "NA"), or
# "own" (a template or a guard the builder does not write: kept)
.row_label_read <- function(label) {
  if (.is_blank(label)) return(".label")
  v <- trimws(sub("^[^=]*=", "", trimws(label)))
  if (toupper(v) %in% c("NA", "FALSE")) return(NA_character_)
  if (identical(v, ".label") || grepl("^[A-Za-z.][A-Za-z0-9._]*$", v)) return(v)
  "own"
}

# ... and written: blank for ".label", "NA" for none, "label = KEY"
.row_label_write <- function(label) {
  if (length(label) != 1L || is.na(label)) return("NA")
  if (identical(label, ".label")) return(NA_character_)
  paste("label =", label)
}

# A variable's levels in order, data first, and where the order comes
# from: this table's own (the variables sheet's levels: "table"), the
# report's code list ("codelist", ARD step 1-1 -- tflspec writes
# plan_levels() from it, so the screen and the code agree), the ARD's
# factor ("ard": `ard_factor`, its levels), else the ARD's values as
# first seen ("seen"); a code list's order is followed by the ARD's values
# it does not name.  The precedence is `steps`' order, here only.
# `differs`: the ARD keeps a factor's order and the code list puts the
# values they share in another.  A column the plan makes (`plan_made`: the
# stack's subgroup, .stat_group) has the plan's order -- its code list
# rows give its labels only (tflspec writes no plan_levels() from them):
# the ARD's as it is ("plan"), nothing said of it.
.level_order <- function(own = character(), codelist = character(), ard = character(),
                         ard_factor = FALSE, plan_made = FALSE) {
  if (isTRUE(plan_made) && !length(own)) {
    return(list(levels = as.character(ard), source = "plan", differs = FALSE))
  }
  steps <- list(table = own, codelist = codelist,
                ard = if (isTRUE(ard_factor)) ard else character(), seen = ard)
  at <- which(lengths(steps) > 0L)
  src <- if (length(at)) names(steps)[at[1L]] else "seen"
  both <- intersect(ard, codelist)
  differs <- isTRUE(ard_factor) && length(both) > 1L &&
    !identical(both, intersect(codelist, ard))
  lv <- as.character(steps[[src]])
  # (the ARD's values a code list does not name: after its own)
  if (identical(src, "codelist")) lv <- c(lv, setdiff(ard, lv))
  list(levels = lv, source = src, differs = differs)
}

# A summary table's row layers from its tables$rows ("group1 = SEX | group
# = variable"), outermost first: the keys, ".variable" for the block of the
# analysis variables where the rows put it, a constant heading as written
# ('"Overall"'); the block alone when the rows say nothing.  Rows without
# the block have none (a table of one variable: ORR by subgroup).
.row_layers_read <- function(rows) {
  if (.is_blank(rows)) return(".variable")
  part <- trimws(strsplit(rows, "|", fixed = TRUE)[[1L]])
  v <- trimws(sub("^[^=]*=", "", part))
  v <- v[nzchar(v)]
  v[v == "variable"] <- ".variable"
  v
}

# ... and back, in the order given: the keys and the constant headings a
# group each (group1, group2 ...), the block as `group = variable` where it
# is; NA for no layers (the rows' default: the block)
.row_layers_write <- function(layers) {
  layers <- layers[!is.na(layers) & nzchar(layers)]
  if (!length(layers)) return(NA_character_)
  k <- 0L
  parts <- vapply(layers, function(l) {
    if (identical(l, ".variable")) return("group = variable")
    k <<- k + 1L
    sprintf("group%d = %s", k, l)
  }, "", USE.NAMES = FALSE)
  paste(parts, collapse = " | ")
}

# a layer that is a constant heading ('"Overall"', ~"Worst grade"), not a key
.is_heading <- function(x) grepl("^[\"'~]", x)

# A table's row keys, outermost first, from its tables$rows and
# tables$label ("group1 = AEBODSYS", "label = AEDECOD"): the columns named
# (not a constant heading, not .label)
.row_keys_read <- function(rows, label) {
  part <- function(x) {
    if (.is_blank(x)) return(character())
    v <- trimws(sub("^[^=]*=", "", trimws(strsplit(x, "|", fixed = TRUE)[[1L]])))
    v[nzchar(v) & !grepl("^[\"'~]", v)]
  }
  lab <- part(label)
  c(part(rows), lab[lab != ".label"])
}

# A hierarchy table's order (tables$sort), written as plan_sort() takes
# it, one order a level, outermost first: "alpha" (by name -- a code
# list's order when the report has one: a factor sorts by its levels) or
# "freq" (most frequent first, ties by name: an outer level by its own
# row's n, -n@group1; the innermost by its n).  `col`: the column whose n
# counts (a level of the outer column key, or the Total column's
# heading: -n[Total]); NA every column's together.  The "any event" row
# first, and each group's own row above the rows under it.
# `hier`: which row keys are the hierarchy's levels -- a group of the ARD
# outside it (a stratum, outermost) comes before the "any event" row and
# has no summary row of its own.
.hier_sort <- function(levels = c("alpha", "freq"), col = NA_character_,
                       hier = rep(TRUE, length(levels))) {
  n <- length(levels)
  at <- if (!is.na(col) && nzchar(col)) sprintf("[%s]", col) else ""
  out <- character()
  overall <- FALSE
  for (i in seq_len(n)) {
    g <- if (i < n) sprintf("group%d", i) else "label"
    if (hier[i] && !overall) {
      out <- c(out, ".overall")
      overall <- TRUE
    }
    count <- if (identical(levels[i], "freq")) paste0(if (i < n) paste0("-n@", g) else "-n", at)
    out <- c(out, count, g, if (hier[i] && i < n) ".depth")
  }
  if (!overall) out <- c(".overall", out)
  paste(out, collapse = " | ")
}

# ... and read back: the order of each level (attribute "col": the column
# that counts, NA all of them), or "own" (an order the builder does not
# write: kept as it is).  n: the terms.
.hier_sort_read <- function(sort, n = 2L) {
  if (length(sort) != 1L || is.na(sort) || !nzchar(trimws(sort))) return("own")
  tk <- trimws(strsplit(sort, "|", fixed = TRUE)[[1L]])
  if (!".overall" %in% tk) return("own")
  tk <- tk[!tk %in% c(".overall", ".depth")]
  lv <- character(n)
  cols <- character()
  for (i in seq_len(n)) {
    name <- if (i < n) sprintf("group%d", i) else "label"
    count <- if (i < n) paste0("^-n@", name) else "^-n"
    m <- if (length(tk)) regmatches(tk[1L], regexec(paste0(count, "(\\[(.+)\\])?$"), tk[1L]))[[1L]]
    if (length(m)) {
      lv[i] <- "freq"
      cols <- c(cols, if (nzchar(m[3L])) m[3L] else NA_character_)
      tk <- tk[-1L]
    } else {
      lv[i] <- "alpha"
    }
    if (!length(tk) || !identical(tk[1L], name)) return("own")
    tk <- tk[-1L]
  }
  # one column counts for every level, or the order is the sheets' own
  if (length(tk) || length(unique(cols)) > 1L) return("own")
  attr(lv, "col") <- if (length(cols)) cols[1L] else NA_character_
  lv
}

builder_write <- function(x, output_id, state, was = NULL) {
  id <- output_id
  st <- state
  # what the form changed from what it read (`was`): only that is written,
  # so what the form cannot show -- a statistic's own template, a
  # condition, an order of one's own -- stays as the sheets have it
  changed <- function(part) is.null(was) || !identical(st[[part]], was[[part]])
  # the report's items (its YAML over its layers): table_plan()'s roles,
  # plan_levels(), plan_labels(), plan_nest() are written into them
  its <- builder_items(x, id)
  # tables: the column key, and one group per variable unless said otherwise
  tb <- .builder_tables_own(x, id, its)
  tb_was <- tb
  # (a key the plan does not state yet, read from the ARD, is written too)
  no_cols <- is.na(tb$cols[1L])
  put_key <- !anyNA(st$key) && (changed("key") || no_cols)
  if (put_key) tb$cols[1L] <- paste(st$key, collapse = " | ")
  # (the value the table prints: plan_cells(value = ), below)
  # a hierarchy table's order (the form's: by frequency or by name)
  # the rows: the keys, outermost first (a hierarchy's every level among
  # them: a level left out would put its rows in another's cells); a new
  # set of rows takes the card's order
  rk <- st$row_keys %||% character()
  new_rows <- length(st$hierarchy) && changed("row_keys") && length(rk) &&
    all(st$hierarchy %in% rk)
  if (new_rows) {
    n <- length(rk)
    tb$rows[1L] <- if (n > 1L) paste(sprintf("group%d = %s", seq_len(n - 1L), rk[-n]),
                                     collapse = " | ") else NA_character_
    tb$label[1L] <- paste("label =", rk[n])
    if (!identical(st$sort, "own")) {
      if (!"sort" %in% names(tb)) tb$sort <- NA_character_
      tb$sort[1L] <- .hier_sort(c(rep("alpha", n - 1L), "freq"), hier = rk %in% st$hierarchy)
    }
  }
  if (length(st$hierarchy) && !new_rows && changed("sort") && !identical(st$sort, "own") &&
      length(st$sort) == length(rk)) {
    if (!"sort" %in% names(tb)) tb$sort <- NA_character_
    tb$sort[1L] <- .hier_sort(st$sort, attr(st$sort, "col") %||% NA_character_,
                              hier = rk %in% st$hierarchy)
  }
  # a summary table's row layers: written when they changed, in their
  # order (a key over the block of variables, or no block at all); the
  # key that is the rows' text is no layer of them
  lab_ch <- !length(st$hierarchy) && !is.null(was) && !is.null(st$row_label) &&
    !identical(st$row_label, "own") && !identical(st$row_label, was$row_label)
  lab_key <- if (!length(st$hierarchy) && length(st$row_label) == 1L && !is.na(st$row_label) &&
                   !st$row_label %in% c(".label", "own")) st$row_label
  if (!length(st$hierarchy) && length(st$layers) && !is.null(was) &&
      (!identical(st$layers, was$layers) || (lab_ch && isTRUE(lab_key %in% st$layers)))) {
    tb$rows[1L] <- .row_layers_write(setdiff(st$layers, lab_key))
  }
  # ... and its rows' text: normalize_ard()'s .label, a key, or none
  if (lab_ch) {
    if (!"label" %in% names(tb)) tb$label <- NA_character_
    tb$label[1L] <- .row_label_write(st$row_label)
  }
  # whose {n} the header prints (the form asks only when a cell uses {n})
  if (!is.null(st$header_n)) tb$header_n[1L] <- st$header_n
  if (is.na(tb$rows[1L]) && (is.null(was) || put_key)) tb$rows[1L] <- "group = variable"


  # variables: the arms' order, each variable's label, order and levels
  cl <- tryCatch(sheet_rows(x, "codelists", id), error = function(e) NULL)
  vr <- .builder_vars_rows(its, cl)
  vr_was <- vr
  put <- function(vr, v, ...) {
    val <- list(...)
    if (!length(val)) return(vr)
    i <- match(v, vr$variable)
    if (is.na(i)) {
      vr[nrow(vr) + 1L, ] <- NA
      i <- nrow(vr)
      vr$variable[i] <- v
    }
    for (k in names(val)) vr[[k]][i] <- val[[k]]
    vr
  }
  j <- function(l) if (length(l)) paste(l, collapse = " | ") else NA_character_
  lv <- function(v, l) {
    if (identical(l, st$auto_levels[[v]])) NA_character_ else j(l)
  }
  if (!anyNA(st$key)) for (k in st$key) {
    l <- st$arms[[k]] %||% st$auto_levels[[k]]
    if (is.null(was) || !identical(l, was$arms[[k]] %||% was$auto_levels[[k]])) {
      vr <- put(vr, k, levels = lv(k, l))
    }
  }
  v <- st$variables
  wv <- was$variables
  # the order is written when the variables were moved (then all of them)
  moved <- is.null(was) || !identical(v$variable, wv$variable)
  for (i in seq_len(nrow(v))) {
    nm <- v$variable[i]
    w <- if (!is.null(wv)) match(nm, wv$variable) else NA_integer_
    val <- list()
    lab <- if (is.na(v$label[i]) || !nzchar(v$label[i])) NA_character_ else v$label[i]
    if (is.na(w) || !identical(v$label[i], wv$label[w])) val$label <- lab
    if (moved) val$order <- as.character(i)
    un <- v$under[i] %||% NA_character_
    if (!is.na(un) && !nzchar(un)) un <- NA_character_
    if (is.na(w) || !identical(un, wv$under[w] %||% NA_character_)) val$under <- un
    cat_v <- identical(v$kind[i], "categorical")
    if (is.na(w) || !identical(st$levels[[nm]], was$levels[[nm]]) ||
        !identical(v$kind[i], wv$kind[w])) {
      val$levels <- if (cat_v) lv(nm, st$levels[[nm]]) else NA_character_
    }
    vr <- do.call(put, c(list(vr, nm), val))
  }
  empty <- rowSums(!is.na(vr[setdiff(names(vr), "variable")])) == 0 &
    vr$variable %in% st$key
  vr <- vr[!empty, , drop = FALSE]
  # the roles and the variables into the report's items, with the rest
  # below: one write
  w <- .builder_items_from_rows(its, tb, vr, vr_was, cl)
  same_cell <- function(a, b) {
    a <- as.character(a %||% NA)[1L]
    b <- as.character(b %||% NA)[1L]
    identical(is.na(a), is.na(b)) && (is.na(a) || identical(a, b))
  }
  # a hierarchy's order: plan_sort(keys = ), its other arguments kept
  if (!same_cell(tb$sort, tb_was$sort)) {
    ps <- .builder_args(its, "plan_sort") %||% list()
    k <- .split_list(tb$sort[1L])
    if (length(k)) ps$keys <- k else ps$keys <- NULL
    w[[length(w) + 1L]] <- list(fn = "plan_sort", args = ps, drop_empty = TRUE)
  }
  # the outermost row key a page each: plan_paginate_group(col = "group1")
  if (length(st$hierarchy) && changed("row_pages") && !is.null(st$row_pages) &&
      !identical(isTRUE(st$row_pages), .builder_row_pages(its))) {
    pg <- .builder_args(its, "plan_paginate_group") %||% list()
    pg$col <- "group1"
    w[[length(w) + 1L]] <- list(fn = "plan_paginate_group",
                                args = if (isTRUE(st$row_pages)) pg else list(),
                                drop_empty = TRUE)
  }
  # the column header and whose {n} it prints: plan_col_header()
  hw <- .builder_header_write(its, st, if (!same_cell(tb$header_n, tb_was$header_n)) tb$header_n[1L])
  if (!is.null(hw)) w[[length(w) + 1L]] <- hw
  fin <- function(x) if (length(w)) .spec_set_items(x, id, w) else x

  # plan_cells(): the continuous rows, the statistics' decimals (in the
  # templates, as the report's YAML holds them), a variable's own
  # decimals, the categorical format and the value printed -- each
  # written only when changed
  cont_ch <- changed("rows")
  # the decimals: those the form shows against those the plan says (with no
  # `was`, what it says now)
  now <- if (is.null(was)) builder_read(x, id)
  dig_ch <- if (is.null(was)) {
    !identical(st$digits[sort(names(st$digits))], now$digits[sort(names(st$digits))]) ||
      !identical(nrow(st$exceptions %||% now$exceptions), nrow(now$exceptions)) ||
      !identical(as.list(st$exceptions), as.list(now$exceptions))
  } else changed("digits") || changed("exceptions")
  # one's own format, left blank, is not a format yet: nothing is written
  cat_ch <- (changed("cat_format") || changed("pct_decimals") || changed("value") ||
    changed("cat_own")) &&
    !(identical(st$cat_format, "own") && is.na(st$cat_own %||% NA_character_))
  val_ch <- changed("value") && !is.null(st$value)
  if (cont_ch || dig_ch || cat_ch || val_ch) {
    pcw <- .builder_cells_write(its, st, was %||% now, cont_ch, dig_ch, cat_ch, val_ch)
    if (!is.null(pcw)) w[[length(w) + 1L]] <- pcw
  }
  fin(x)
}

# A template with the statistics' decimals in it ({mean} -> {mean:.1f}): `dg`
# a statistic's decimals; one with none keeps what it has.  The ARD's text
# (`fmt`, stat_fmt): the template as it is (the decimals are not the
# builder's then).
.with_digits <- function(tpl, dg, fmt = FALSE) {
  if (!is.character(tpl) || length(tpl) != 1L || is.na(tpl) || fmt) return(tpl)
  m <- gregexpr("[{][^}]+[}]", tpl)
  toks <- regmatches(tpl, m)[[1L]]
  if (!length(toks)) return(tpl)
  regmatches(tpl, m) <- list(vapply(toks, function(tk) {
    s <- sub("^[{]([^}:]+).*$", "\\1", tk)
    # (a format of its own -- significant digits {median:.3s}, a width --
    # stays; only decimals, {mean:.1f}, are the builder's)
    spec <- sub("^[{][^}:]+", "", sub("[}]$", "", tk))
    if (nzchar(spec) && !grepl("^:[.][0-9]+f%?$", spec)) return(tk)
    d <- if (s %in% names(dg)) dg[[s]] else NA
    if (length(d) != 1L || is.na(d)) return(tk)
    pct <- if (grepl("%[}]$", tk)) "%" else ""
    sprintf("{%s:.%df%s}", s, as.integer(d), pct)
  }, "", USE.NAMES = FALSE))
  tpl
}

# plan_cells() as the form says it (`st`; `was` as read): its whole
# arguments, those the form does not say kept.  The continuous rows (a row
# kept: its own template, or chain, as written) with the statistics'
# decimals; a variable's own decimals its rows again with them; the
# categorical template (the layers' when the same); the value printed.
# NULL: no change.
.builder_cells_write <- function(its, st, was, cont_ch, dig_ch, cat_ch, val_ch) {
  it <- Filter(function(i) identical(i$fn, "plan_cells") && !isFALSE(i$present), its)
  pc <- if (length(it)) it[[1L]]$args %||% list() else list()
  below <- if (length(it)) it[[1L]]$below$args %||% list() else list()
  new <- pc
  fmt <- identical(st$value, "stat_fmt")
  dg <- st$digits %||% integer()
  rows_tpl <- function(dg) {
    old <- pc$continuous %||% below$continuous %||% list()
    # (rows with conditions are cell_rows(): its rows are its arguments)
    call <- if (inherits(old, "tfl_call")) old
    if (!is.null(call)) old <- call$args %||% list()
    had <- length(old) > 0L
    rows <- if (had || any(st$variables$kind %in% "continuous")) st$rows else character()
    out <- lapply(rows, function(lb) {
      o <- old[[lb]]
      # a row of conditions, or a template of its own, as written
      if (!is.null(o) && !(is.character(o) && length(o) == 1L)) return(o)
      tpl <- o %||% st$templates[[lb]] %||%
        builder_stats()$template[match(lb, builder_stats()$row)]
      .with_digits(tpl, dg, fmt)
    })
    out <- stats::setNames(out, rows)
    if (is.null(call)) return(out)
    call$args <- out
    call
  }
  if (cont_ch || dig_ch || val_ch) {
    ct <- rows_tpl(dg)
    if (length(ct)) new$continuous <- ct else new$continuous <- NULL
    # a variable's own decimals: its rows, those decimals in them
    exc_vars <- unique(c(was$exceptions$variable %||% character(),
                         st$exceptions$variable %||% character()))
    for (v in exc_vars) new[[v]] <- NULL
    exc <- st$exceptions
    if (!fmt && !is.null(exc) && nrow(exc) && length(ct)) {
      for (v in unique(exc$variable)) {
        d2 <- dg
        e <- exc[exc$variable == v & !is.na(exc$digits), , drop = FALSE]
        d2[e$statistic] <- as.integer(e$digits)
        new[[v]] <- rows_tpl(d2)
      }
    }
  }
  if (cat_ch) {
    tpl <- .cat_template(st$cat_format, st$pct_decimals, st$value %||% "stat",
                         own = st$cat_own %||% NA_character_)
    # (the layers' template: what is below the report's own; read from the
    # sheets, with nothing below known, the plan's other than categorical)
    ref <- if (length(it) && !is.null(it[[1L]]$below)) below else pc[setdiff(names(pc), "categorical")]
    inh <- ref$categorical %||% ref$default
    add <- is.null(new$categorical)
    new$categorical <- if (identical(inh, tpl)) NULL else tpl
    # (a new one before the continuous rows, as tflspec writes them)
    if (add && !is.null(new$categorical) && "continuous" %in% names(new)) {
      nm <- setdiff(names(new), "categorical")
      at <- match("continuous", nm)
      new <- new[append(nm, "categorical", after = at - 1L)]
    }
    # (the report's own default template gives way to it, as the sheets did)
    if (!is.null(pc$default) && !is.null(below$default)) new$default <- below$default
  }
  if (val_ch) new$value <- if (fmt) "stat_fmt" else NULL
  if (identical(tflspec::tfl_args_to_r("plan_cells", new), tflspec::tfl_args_to_r("plan_cells", pc))) {
    return(NULL)
  }
  list(fn = "plan_cells", args = new, drop_empty = TRUE)
}

# plan_col_header() as the form says it: its header lines (`st$header`, the
# form's rows, or a preset's name) where they differ from the plan's, whose
# {n} (`header_n`: the tables sheet's form, NULL unchanged) -- its other
# arguments kept.  NULL: no change.
.builder_header_write <- function(its, st, header_n = NULL) {
  ch <- .builder_args(its, "plan_col_header") %||% list()
  new <- ch
  rows <- if (is.data.frame(st$header)) st$header else
    if (!identical(st$header, "keep") && isTRUE(st$header %in% names(header_presets())))
      header_presets()[[st$header]]
  if (!is.null(rows)) {
    rows <- as.data.frame(rows, stringsAsFactors = FALSE)
    rows$output_id <- NULL
    if (!.same_header(.header_df(ch$header), rows)) new$header <- .header_list(rows)
  }
  if (!is.null(header_n)) new$values <- .header_n_values(header_n)
  if (identical(tflspec::tfl_args_to_r("plan_col_header", new),
                tflspec::tfl_args_to_r("plan_col_header", ch))) return(NULL)
  list(fn = "plan_col_header", args = new, drop_empty = TRUE)
}

# Do two sets of col_header rows say the same?
.same_header <- function(a, b) {
  cols <- c("line", "cols", "span", "text", "align", "bold", "border_top",
            "border_bottom")
  norm <- function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    for (k in cols) if (!k %in% names(d)) d[[k]] <- NA_character_
    d <- d[cols]
    d[] <- lapply(d, function(v) {
      v <- as.character(v)
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    })
    rownames(d) <- NULL
    d
  }
  identical(norm(a), norm(b))
}

# What a report's ARD holds, as the builder's card says it (#343): `meta`
# is ard_info()'s (the keys, the variables and their statistics, when it
# was read); `page_by`: the layout's page variables.  `line`: the one-line
# summary; `rows`: role, variable, kind, the levels or statistics, the
# label (NULL with no meta).  `tr`: the app's translation.
ard_card_summary <- function(meta, page_by = character(), tr = function(x) x) {
  if (is.null(meta) || (!length(meta$by) && !NROW(meta$variables))) {
    return(list(line = tr("ARD: not made yet (Update preview makes it)"),
                rows = NULL))
  }
  cut <- function(x, k) if (length(x) > k) c(utils::head(x, k), "\u2026") else x
  levels_of <- function(l) {
    l <- as.character(l)
    if (!length(l)) return("")
    sprintf(tr("%d levels: %s"), length(l), paste(cut(l, 4L), collapse = ", "))
  }
  by <- meta$by
  hier <- setdiff(meta$hierarchy %||% character(), page_by)
  v <- meta$variables
  if (is.null(v)) v <- data.frame(variable = character(), kind = character())
  v <- v[!v$variable %in% c(by, hier, page_by), , drop = FALSE]
  n_cat <- sum(v$kind %in% "categorical")
  n_con <- sum(v$kind %in% "continuous")
  st <- unique(stats::na.omit(as.character(meta$stats %||% character())))
  when <- meta$fetched %||% NULL
  parts <- c(
    if (length(by)) sprintf(tr("columns %s (%s groups)"), paste(by, collapse = " \u00d7 "),
                            paste(lengths(meta$keys[by]), collapse = " \u00d7 ")),
    if (length(page_by)) sprintf(tr("pages %s"), paste(page_by, collapse = ", ")),
    if (length(hier)) sprintf(tr("rows %s"), paste(hier, collapse = " \u203a ")),
    # a hierarchy table with no analysis variable says nothing of them
    if (nrow(v) || !length(hier))
      sprintf(tr("%d variables (%d categorical, %d continuous)"), nrow(v), n_cat, n_con),
    if (length(st)) sprintf(tr("statistics %s"), paste0(
      paste(utils::head(st, 6L), collapse = ", "), if (length(st) > 6L) " \u2026")),
    if (!is.null(when) && !all(is.na(when)))
      sprintf(tr("read %s"), format(as.POSIXct(when), "%m/%d %H:%M")))
  line <- paste0("ARD: ", paste(parts, collapse = " | "))
  row <- function(role, var, kind, detail, label = NA_character_) {
    data.frame(role = role, variable = var, kind = kind, detail = detail,
               label = label, stringsAsFactors = FALSE)
  }
  lab <- function(x) {
    if (!"label" %in% names(v)) return(NA_character_)
    l <- v$label[match(x, v$variable)]
    if (is.na(l) || identical(l, x)) NA_character_ else l
  }
  rows <- c(
    lapply(by, function(k) row(tr("column"), k, tr("group"), levels_of(meta$keys[[k]]))),
    lapply(page_by, function(k) row(tr("page split"), k, tr("group"), levels_of(meta$keys[[k]]))),
    lapply(hier, function(k) row(tr("row: hierarchy"), k, tr("hierarchy"),
                                 levels_of(meta$keys[[k]]))),
    lapply(seq_len(nrow(v)), function(i) {
      cont <- identical(v$kind[i], "continuous")
      det <- if (cont) gsub(" | ", ", ", v$stats[i] %||% "", fixed = TRUE) else
        levels_of(.split_list(v$levels[i] %||% NA_character_))
      row(tr("row: variable"), v$variable[i],
          tr(if (cont) "continuous" else "categorical"), det, lab(v$variable[i]))
    }))
  list(line = line, rows = do.call(rbind, rows))
}

# ------------------------------------------------------------ preview

#' The table as it will print
#'
#' Plans the report's table from the definition as it stands (saved or
#' not) and the report's normalized data, and gives its pages -- what
#' [tflspec::tfl_report()] lays out -- or an HTML rendering of them.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param data Its normalized data ([ard_data()]).
#' @param pages `rtftable` pages.
#' @param max_pages How many pages to render.
#' @param align How value cells align: `"center"` (a table) or `"left"` (a
#'   listing).
#' @return `preview_pages()`: a list of `rtftable`; `preview_html()`: HTML.
#' @export
preview_pages <- function(x, output_id, data) {
  sheets <- lapply(x$sheets[table_sheets()], function(d) {
    d <- d[is.na(d$output_id) | d$output_id == output_id, , drop = FALSE]
    if (all(is.na(d$note))) d$note <- NULL
    d
  })
  st <- x$study["rounding"]
  sheets$study <- if (!is.na(st)) st
  spec <- tflspec::tfl_table_spec(sheets)
  plan <- rtfreporter::plan_cells(tflspec::tfl_table_plan(data, spec, output_id),
                                  notes = FALSE)
  res <- suppressMessages(rtfreporter::plan_apply(plan))
  # what the column header's tokens hold here (the builder's insert chips)
  tokens <- tryCatch(rtfreporter::plan_header_tokens(plan),
                     error = function(e) NULL)
  # a plan with no layout gives its table, not pages: one page of it
  if (is.data.frame(res)) {
    res <- list(structure(list(data = res, col_header = list(names(res)),
                               blank_rows = integer()),
                          class = "rtftable"))
  }
  if (inherits(res, "rtftable")) res <- list(res)
  attr(res, "header_tokens") <- tokens
  # the populations a header's {n} can say, for the builder's choice
  attr(res, "n_candidates") <- tryCatch(rtfreporter::plan_n_candidates(plan),
                                        error = function(e) NULL)
  res
}

# The table's data for the preview from its ARD as read (ard_raw(), #428):
# normalized here with normalize_ard()'s arguments as the report's item
# says them now -- the overall row, the hierarchy, the stack -- so a change
# of them shows without making the ARD again.  The same call the program
# writes (its plan's pipe); NULL when the report has no such item.
.preview_normalized <- function(x, output_id, ard) {
  its <- builder_items(x, output_id)
  it <- Filter(function(i) identical(i$fn, "normalize_ard"), its)
  args <- if (length(it)) it[[1L]]$args %||% list() else list()
  txt <- tflspec::tfl_args_to_r("normalize_ard", args)
  call <- if (length(txt) && nzchar(txt)) paste0("rtfreporter::normalize_ard(.ard, ", txt, ")") else
    "rtfreporter::normalize_ard(.ard)"
  eval(parse(text = call), envir = list2env(list(.ard = ard), parent = baseenv()))
}

# The insert chips' labels: a token and what it holds here
# ("{n} = 86 / 84 / 84"); the token alone when the preview has not said
header_token_labels <- function(choices, tokens = NULL) {
  if (is.null(tokens) || !nrow(tokens)) return(stats::setNames(choices, choices))
  lab <- vapply(choices, function(tk) {
    k <- match(tk, tokens$token)
    if (is.na(k) || !isTRUE(tokens$resolved[k])) return(tk)
    v <- unlist(tokens$values[[k]], use.names = FALSE)
    v <- v[!is.na(v)]
    if (!length(v)) return(tk)
    if (length(v) > 4L) v <- c(utils::head(v, 4L), "\u2026")
    paste(tk, "=", paste(v, collapse = " / "))
  }, "")
  stats::setNames(choices, lab)
}

# The lines of a report's header / titles / footnotes / footer: its own,
# and the study defaults for the lines it has not (by line number).
.page_lines <- function(x, sheet, output_id) {
  # the report's own over its pattern's over Standard's; a "(none)" line
  # prints nothing (the line of that number below is left out)
  d <- page_lines_from(x, sheet, output_id)
  d <- d[!d$omitted, setdiff(names(d), c("from", "omitted")), drop = FALSE]
  rownames(d) <- NULL
  d
}

# A page's sample in HTML: each line in three parts (left, centre, right),
# the {PLACEHOLDERS} filled as a first page would have them.
.page_sample_html <- function(x, output_id, study_id, body, program = "") {
  # the report's tokens, as its program gives them ({OUTPUT_LABEL} ...)
  tok <- tryCatch(tflspec::tfl_report_tokens(
    .spec_object_last(x, c(table_sheets(), report_sheets()),
                      unique(c(.study_keys$table, .study_keys$report))),
    output_id), error = function(e) character())
  # a study with no STUDY_ID token: its id, as before
  if (is.na(tok["STUDY_ID"]) || !nzchar(tok[["STUDY_ID"]])) {
    tok <- tok[names(tok) != "STUDY_ID"]
  }
  rx <- "\\{[A-Z][A-Z0-9_]*\\}"
  # a line whose tokens are all empty, the rest blanks or brackets, is not
  # printed (rtfreporter's drop_empty_rows)
  empty <- function(cells) {
    txt <- paste(cells[!is.na(cells)], collapse = " ")
    m <- regmatches(txt, gregexpr(rx, txt))[[1L]]
    nm <- substr(m, 2L, nchar(m) - 1L)
    length(m) && all(nm %in% names(tok)) && !any(nzchar(trimws(tok[nm]))) &&
      !nzchar(gsub("[][[:space:]<>():;,.|/-]", "", gsub(rx, "", txt)))
  }
  fill <- function(s) {
    if (is.na(s)) return("")
    for (k in names(tok)) s <- gsub(paste0("{", k, "}"), tok[[k]], s, fixed = TRUE)
    s <- gsub("{PAGE}", "1", s, fixed = TRUE)
    s <- gsub("{TOTAL_PAGES}", "N", s, fixed = TRUE)
    s <- gsub("{STUDY_ID}", study_id, s, fixed = TRUE)
    s <- gsub("{PROGRAM}", program %||% "", s, fixed = TRUE)
    s <- gsub("{DATETIME}", format(Sys.time(), "%Y-%m-%d %H:%M"), s, fixed = TRUE)
    s <- gsub("{output_id}", output_id, s, fixed = TRUE)
    s
  }
  block <- function(sheet, cls) {
    d <- .page_lines(x, sheet, output_id)
    for (k in .toc_cells) if (!k %in% names(d)) d[[k]] <- rep(NA_character_, nrow(d))
    d <- d[!vapply(seq_len(nrow(d)), function(i)
      empty(unlist(d[i, .toc_cells])), NA), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    lapply(seq_len(nrow(d)), function(i) htmltools::div(
      class = paste("rp-page-line", cls),
      htmltools::span(class = "l", fill(d$left[i])),
      htmltools::span(class = "c", fill(d$center[i])),
      htmltools::span(class = "r", fill(d$right[i]))))
  }
  pg <- rbind(sheet_rows(x, "page", output_id), inherited_rows(x, "page", output_id))
  land <- any(tolower(pg$orientation) %in% "landscape")
  htmltools::div(
    class = paste("rp-page", if (land) "rp-landscape"),
    htmltools::tags$style(htmltools::HTML("
      .rp-page { border: 1px solid #ccc; padding: .6rem .8rem; font-size: .7rem;
                 font-family: 'Courier New', monospace; background: #fff; }
      .rp-page-wrap { overflow-x: auto; max-width: 100%; }
      .rp-page-wrap .rp-page { width: max-content; min-width: 100%; }
      .rp-page-wrap .rp-page-body { overflow-x: visible; }
      .rp-page-full .rp-page { zoom: 1 !important; }
      .rp-page.rp-landscape { min-width: 60rem; }
      .rp-page-line { display: grid; grid-template-columns: 1fr auto 1fr; gap: .5rem; }
      .rp-page-line .c { text-align: center; } .rp-page-line .r { text-align: right; }
      .rp-page-body { margin: .5rem 0; overflow-x: auto; }
      .rp-page-foot { border-top: 1px solid #ddd; margin-top: .4rem; padding-top: .2rem; }")),
    block("header", "head"),
    block("titles", "title"),
    htmltools::div(class = "rp-page-body", body),
    block("footnotes", "note"),
    htmltools::div(class = "rp-page-foot", block("footer", "foot")))
}

#' @rdname preview_pages
#' @export
preview_html <- function(pages, max_pages = 3L, align = "center") {
  one <- function(pg, i) {
    d <- pg$data
    blank_after <- attr(d, "rtf_blank_rows") %||% pg$blank_rows %||%
      integer()
    stub <- attr(d, "rtf_stub_src")
    val <- function(v) if (is.na(v)) "" else as.character(v)
    text <- function(x) htmltools::HTML(gsub("\n", "<br>", htmltools::htmlEscape(
      val(x)), fixed = TRUE))
    # a header cell's own look, as the RTF prints it: bold, a rule above or
    # below it (the builder's "underline"), its alignment
    look <- function(x) {
      line <- function(b) !is.null(b) && !identical(b$style, "none")
      css <- c(if (isTRUE(x$bold)) "font-weight: bold;",
               if (line(x$border$top)) "border-top: 1px solid #333;",
               if (line(x$border$bottom)) "border-bottom: 1px solid #333;",
               if (!is.null(x$align) && x$align %in% c("left", "center", "right"))
                 paste0("text-align: ", x$align, ";"))
      if (length(css)) paste(css, collapse = " ")
    }
    head <- lapply(pg$col_header, function(line) {
      # a line of spanning cells (list(from, to, label)): each over its
      # columns, the columns no cell covers left empty
      spans <- is.list(line) && length(line) &&
        all(vapply(line, function(x) is.list(x) && !is.null(x$from), NA))
      if (spans) {
        cells <- list()
        at <- 1L
        for (x in line[order(vapply(line, function(x) as.integer(x$from), 1L))]) {
          from <- as.integer(x$from)
          to <- as.integer(x$to %||% x$from)
          while (at < from) {
            cells <- c(cells, list(htmltools::tags$th(
              class = if (at == 1L) "rp-pv-stub" else "rp-pv-val")))
            at <- at + 1L
          }
          cells <- c(cells, list(htmltools::tags$th(
            colspan = to - from + 1L,
            class = if (from == 1L) "rp-pv-stub" else "rp-pv-val rp-pv-span",
            style = look(x),
            text(x$label %||% ""))))
          at <- to + 1L
        }
        return(htmltools::tags$tr(cells))
      }
      htmltools::tags$tr(lapply(seq_along(line), function(k)
        htmltools::tags$th(
          class = if (k == 1L) "rp-pv-stub" else "rp-pv-val",
          text(line[[k]]))))
    })
    blank <- htmltools::tags$tr(class = "rp-pv-blank",
                                htmltools::tags$td(colspan = ncol(d),
                                                   htmltools::HTML("&nbsp;")))
    body <- list()
    if (0L %in% blank_after) body <- c(body, list(blank))
    for (r in seq_len(nrow(d))) {
      group <- !is.null(stub) && is.na(stub[r])
      body <- c(body, list(htmltools::tags$tr(
        class = if (group) "rp-pv-group",
        lapply(seq_len(ncol(d)), function(k) htmltools::tags$td(
          class = if (k == 1L) {
            if (!is.null(stub) && !group) "rp-pv-stub rp-pv-indent" else
              "rp-pv-stub"
          } else "rp-pv-val",
          val(d[r, k]))))))
      if (r %in% blank_after) body <- c(body, list(blank))
    }
    htmltools::tagList(
      if (length(pages) > 1L) htmltools::div(
        class = "rp-pv-page", sprintf("%d / %d", i, length(pages))),
      htmltools::tags$table(
        class = paste(c("rp-pv", if (identical(align, "left")) "rp-pv-left"),
                      collapse = " "),
        htmltools::tags$thead(head), htmltools::tags$tbody(body)))
  }
  n <- min(length(pages), max_pages)
  htmltools::tagList(lapply(seq_len(n), function(i) one(pages[[i]], i)))
}
