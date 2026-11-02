# The table builder, reading: the builder's rows from the report's items, and
# builder_read().  Split from builder.R (no code change).

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
# plan_labels()' variables' headings written as tflspec writes them (#358):
# `variable = c(AGE = "Age (years)", ...)`, as a named list (none: empty)
.labels_variable_map <- function(lb) {
  m <- lb[["variable"]]
  if (is.null(m) || is.null(names(m)) || !all(nzchar(names(m)))) return(list())
  m <- as.list(m)
  m[vapply(m, function(x) is.character(x) && length(x) == 1L, NA)]
}

.builder_vars_rows <- function(its, cl = NULL) {
  lv <- .builder_args(its, "plan_levels") %||% list()
  lb <- .builder_args(its, "plan_labels") %||% list()
  ne <- .builder_args(its, "plan_nest") %||% list()
  heading <- function(v) is.character(v) && length(v) == 1L && is.null(names(v))
  # (the headings as tflspec writes them: under `variable = c(AGE = ...)`)
  vm <- .labels_variable_map(lb)
  lb <- lb[vapply(lb, heading, NA)]
  lb <- c(vm, lb[setdiff(names(lb), names(vm))])
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
# do not say kept as they are; a variable with no levels of its own has
# none written (its code list's order, `cl`, kept where the report has
# it: tflspec writes none, #355).  A list of list(fn,
# args, drop_empty) for the verbs that change; none: nothing to write.
.builder_items_from_rows <- function(its, tb, vr, vr_was, cl = NULL) {
  out <- list()
  same <- function(fn, a, b) {
    identical(a, b) || (!length(a) && !length(b)) ||
      identical(tflspec::tfl_args_to_r(fn, a %||% list()),
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
    # none of its own: its code list's order, which tflspec does not write
    # (#355; the ARD is made with it) -- kept where the report has it
    if (!length(l)) {
      was <- as.character(unlist(lv[[v]]))
      l <- if (length(was) && identical(was, .codelist_order(cl, v))) lv[[v]]
    }
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
  heading <- function(x) is.character(x) && length(x) == 1L && is.null(names(x))
  # the headings in the form the report has them: one argument each, or
  # (as tflspec writes them, #358) under `variable = c(AGE = ...)`
  map <- .labels_variable_map(lb)
  one_by_one <- any(vapply(setdiff(names(lb), "variable"),
                           function(k) heading(lb[[k]]), NA)) ||
    (!is.null(lb[["variable"]]) && !length(map))
  for (v in vars) {
    cur <- lb[[v]]
    if (!is.null(cur) && !heading(cur)) next
    l <- row_of(v, "label")
    l <- if (is.na(l) || !nzchar(l)) NULL else l
    if (one_by_one) new[[v]] <- l else {
      new[[v]] <- NULL
      map[[v]] <- l
    }
  }
  # (the variables moved: their headings in their new order, as the
  # program lists them)
  if (!identical(vo, as.character(unlist(lv$variable)))) {
    by_o <- vr$variable[order(is.na(o), o)]
    re <- function(l) {
      r <- match(names(l), by_o)
      l[order(is.na(r), r)]
    }
    new <- re(new)
    map <- re(map)
  }
  if (!one_by_one) {
    new[["variable"]] <- NULL
    if (length(map)) new <- c(list(variable = map), new)
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
# page each) and `sort` (each row key's order: "alpha", "freq", a key of
# a statistic or "=<key>" -- see .hier_sort() -- or "own": the plan's own,
# left as it is).  builder_write() returns the
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
       sort = if (length(hier)) {
         # (a comparison's statistics: no column of the arms' counts them)
         nocol <- unique(as.character(unlist(meta$comparisons)))
         so <- .hier_sort_read(tb$sort[1L], length(rk), nocol)
         if (!identical(so, "own")) attr(so, "nocol") <- nocol
         so
       },
       layers = layers, layer_choices = unique(c(layers, layer_choices)),
       row_label = if (length(hier)) utils::tail(rk, 1L) else .row_label_read(tb$label[1L]))
}
