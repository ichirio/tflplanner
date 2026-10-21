# plan_cells() by variable (#454): a variable's own template -- "variable x
# format" -- read from and written into plan_cells()' named `...`
# (plan_cells(prob = list(cond ~ "tpl", ..., "tpl"), time = "tpl")).  A
# variable not named there takes the common template (`continuous` rows or
# the `categorical` / `default` one); one named there is its own and does
# not follow the common one afterwards.

# plan_cells()' arguments that are not a variable
.cells_fixed_args <- c("default", "continuous", "categorical", "value", "stats", "na", "notes")

# the comparisons a condition's three fields say
.cells_ops <- c("==", "!=", "<", "<=", ">", ">=")

# the variables plan_cells() names (its own templates)
.cells_var_names <- function(pc) setdiff(names(pc %||% list()), .cells_fixed_args)

# a variable's statistics in the ARD (meta: ard_meta()); none known: none
.cells_var_stats <- function(meta, v) {
  mv <- meta$variables
  if (is.null(mv) || !nrow(mv)) return(character())
  i <- match(v, mv$variable)
  if (is.na(i)) return(character())
  .split_list(mv$stats[i])
}

# continuous or categorical, from the ARD's statistics (p: categorical);
# NA: the ARD has no such variable
.cells_var_kind <- function(meta, v) {
  mv <- meta$variables
  if (is.null(mv) || !v %in% mv$variable) return(NA_character_)
  s <- .cells_var_stats(meta, v)
  if (length(s)) return(if ("p" %in% s) "categorical" else "continuous")
  k <- mv$kind[match(v, mv$variable)]
  if (identical(k, "categorical")) "categorical" else "continuous"
}

# ---- a condition: `<column> <op> <value> ~ "<template>"` ----------------------

# a condition (a tfl_r) as its three fields and template; `ok` FALSE: the
# fields cannot say it (it stays as R, read only)
.cells_cond_parse <- function(x) {
  txt <- as.character(unclass(x))
  out <- list(col = NA_character_, op = NA_character_, value = NA_character_,
              tpl = NA_character_, raw = x, ok = FALSE)
  e <- tryCatch(str2lang(txt), error = function(e) NULL)
  if (!is.call(e) || !identical(e[[1L]], as.name("~")) || length(e) != 3L) return(out)
  rhs <- e[[3L]]
  if (is.character(rhs) && length(rhs) == 1L) out$tpl <- rhs
  lhs <- e[[2L]]
  if (is.na(out$tpl) || !is.call(lhs) || length(lhs) != 3L ||
      !as.character(lhs[[1L]]) %in% .cells_ops || !is.name(lhs[[2L]])) return(out)
  val <- lhs[[3L]]
  num <- NULL
  if (is.numeric(val) && length(val) == 1L) num <- val
  if (is.call(val) && identical(val[[1L]], as.name("-")) && length(val) == 2L &&
      is.numeric(val[[2L]])) num <- -val[[2L]]
  if (!is.null(num)) {
    out$value <- format(num, scientific = FALSE, trim = TRUE)
  } else if (is.character(val) && length(val) == 1L) {
    out$value <- val
  } else {
    return(out)
  }
  out$col <- as.character(lhs[[2L]])
  out$op <- as.character(lhs[[1L]])
  out$ok <- TRUE
  out
}

# a value as R: a number as it is, a text quoted
.cells_value_r <- function(v) {
  v <- trimws(as.character(v %||% ""))
  if (grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$", v)) return(v)
  encodeString(v, quote = "\"")
}

# the condition's fields as the R a tfl_r holds ("estimate < 0.5 ~ \"<0.5\"")
.cells_cond_r <- function(col, op, value, tpl) {
  if (!op %in% .cells_ops) stop("Not an operator: ", op, call. = FALSE)
  if (!grepl("^[.A-Za-z][.A-Za-z0-9_]*$", col)) {
    col <- paste0("`", gsub("`", "", col, fixed = TRUE), "`")
  }
  tflspec::tfl_r(sprintf("%s %s %s ~ %s", col, op, .cells_value_r(value),
                         encodeString(tpl %||% "", quote = "\"")))
}

# ---- a variable's entry --------------------------------------------------------

# A variable's entry as the table edits it: `type` "single" (a template, and
# the conditions above it: `conds`, `fallback`), "rows" (a continuous
# variable's rows: `rows`, a "single" a row label; with conditions they are
# cell_rows(), `call`) or "r" (anything else: R, read only; `raw`)
.cells_entry_parse <- function(x) {
  r <- list(type = "r", raw = x)
  if (is.null(x) || inherits(x, "tfl_r")) return(r)
  if (inherits(x, "tfl_call")) {
    rows <- if (identical(x$fn, "cell_rows")) .cells_rows_parse(x$args)
    return(if (is.null(rows)) r else list(type = "rows", rows = rows, call = x))
  }
  s <- .cells_single_parse(x)
  if (!is.null(s)) return(s)
  rows <- .cells_rows_parse(x)
  if (is.null(rows)) r else list(type = "rows", rows = rows)
}

# a template, or conditions (tfl_r) then a template; NULL: not one
.cells_single_parse <- function(x) {
  one <- function(z) is.character(z) && length(z) == 1L && !inherits(z, "tfl_r")
  if (one(x)) return(list(type = "single", conds = list(), fallback = x))
  if (!is.list(x) || !length(x) || inherits(x, "tfl_call")) return(NULL)
  nm <- names(x)
  if (!is.null(nm) && any(nzchar(nm))) return(NULL)
  n <- length(x)
  last <- x[[n]]
  conds <- if (one(last)) x[-n] else x
  if (!all(vapply(conds, function(z) inherits(z, "tfl_r"), NA))) return(NULL)
  list(type = "single", conds = lapply(conds, .cells_cond_parse),
       fallback = if (one(last)) last else NA_character_)
}

# named rows, each a template or conditions; NULL: not such rows
.cells_rows_parse <- function(x) {
  if (!is.list(x) || !length(x) || inherits(x, "tfl_call")) return(NULL)
  nm <- names(x)
  if (is.null(nm) || !all(nzchar(nm))) return(NULL)
  rows <- lapply(x, .cells_single_parse)
  if (any(vapply(rows, is.null, NA))) return(NULL)
  rows
}

# a condition the fields have not said yet (a column and a value): not
# written until they are
.cells_cond_done <- function(c) {
  !is.null(c$raw) || (nzchar(c$col %||% "") && !is.na(c$col) &&
                        nzchar(trimws(c$value %||% "")) && !is.na(c$value))
}

# an entry with a condition not said yet
.cells_entry_pending <- function(e) {
  one <- function(u) any(!vapply(u$conds %||% list(), .cells_cond_done, NA))
  switch(e$type %||% "r", single = one(e), rows = any(vapply(e$rows, one, NA)), FALSE)
}

# ... and back into what plan_cells() takes (a condition not edited: as
# written; one not said yet left out)
.cells_entry_build <- function(e) {
  switch(
    e$type,
    single = {
      cs <- Filter(.cells_cond_done, e$conds)
      if (!length(cs)) return(e$fallback)
      cs <- lapply(cs, function(c) {
        if (!is.null(c$raw)) return(c$raw)
        .cells_cond_r(c$col, c$op, c$value, c$tpl)
      })
      if (is.na(e$fallback %||% NA_character_)) cs else c(cs, list(e$fallback))
    },
    rows = {
      rows <- lapply(e$rows, .cells_entry_build)
      guarded <- any(vapply(rows, is.list, NA))
      if (!is.null(e$call)) {
        e$call$args <- rows
        return(e$call)
      }
      if (guarded) tflspec::tfl_call("cell_rows", rows) else rows
    },
    e$raw)
}

# a part of an entry by its key ("0" the entry, "r:<row>" a row) and back
.cells_entry_part <- function(e, k) {
  if (identical(e$type, "rows") && startsWith(k %||% "", "r:")) e$rows[[substring(k, 3L)]] else
    if (identical(e$type, "single")) e
}
.cells_entry_part_set <- function(e, k, part) {
  if (identical(e$type, "rows") && startsWith(k %||% "", "r:")) {
    e$rows[[substring(k, 3L)]] <- part
    return(e)
  }
  part
}

# ---- decimals ---------------------------------------------------------------

# A template, its rows, its conditions (their templates after the ~) with
# the statistics' decimals `dg` (those changed): nothing taken away
.cells_redigit <- function(x, dg) {
  if (!length(dg) || is.null(x)) return(x)
  if (inherits(x, "tfl_r")) {
    txt <- as.character(unclass(x))
    at <- regexpr("~", txt, fixed = TRUE)
    if (at < 0L) return(x)
    head <- substr(txt, 1L, at)
    tail <- substring(txt, at + 1L)
    m <- gregexpr("\"([^\"\\\\]|\\\\.)*\"", tail)
    lit <- regmatches(tail, m)[[1L]]
    if (!length(lit)) return(x)
    regmatches(tail, m) <- list(vapply(lit, function(s) {
      inner <- substr(s, 2L, nchar(s) - 1L)
      paste0("\"", .with_digits(inner, dg), "\"")
    }, "", USE.NAMES = FALSE))
    new <- paste0(head, tail)
    if (identical(new, txt)) return(x)
    return(tflspec::tfl_r(new))
  }
  if (inherits(x, "tfl_call")) {
    if (!is.null(x$args)) x$args <- .cells_redigit(x$args, dg)
    return(x)
  }
  if (is.character(x) && length(x) == 1L) return(.with_digits(x, dg))
  if (is.list(x)) {
    y <- lapply(x, .cells_redigit, dg)
    attributes(y) <- attributes(x)
    return(y)
  }
  x
}

# the statistics whose decimals the form changed (`now` against `was`)
.cells_digits_changed <- function(now, was) {
  now <- now %||% integer()
  was <- was %||% integer()
  k <- names(now)[vapply(names(now), function(s)
    !identical(unname(as.integer(now[[s]])), unname(as.integer(was[s]))), NA)]
  now[k]
}

# ---- the common template, the table's rows -----------------------------------

# What a variable of `kind` takes when it is not its own: the categorical
# template, or the continuous rows (with their decimals)
.cells_common_entry <- function(pc, st, kind) {
  if (identical(kind, "categorical")) {
    tp <- pc$categorical %||% pc$default
    if (!is.null(tp)) return(tp)
    return(.cat_template(st$cat_format %||% "npct", st$pct_decimals %||% 1,
                         st$value %||% "stat", own = st$cat_own %||% NA_character_))
  }
  ct <- pc$continuous
  if (!is.null(ct)) return(ct)
  rows <- st$rows %||% character()
  tpl <- st$templates %||% list()
  bs <- builder_stats()
  out <- lapply(rows, function(lb) {
    tp <- tpl[[lb]] %||% bs$template[match(lb, bs$row)]
    .with_digits(tp, st$digits %||% integer(), identical(st$value, "stat_fmt"))
  })
  stats::setNames(out, rows)
}

# the common template as a line ("Mean (SD) = {mean:.1f} ({sd:.2f}); ...")
.cells_entry_text <- function(x) {
  if (is.null(x)) return("")
  if (inherits(x, "tfl_call")) return(paste(deparse(x), collapse = " "))
  if (inherits(x, "tfl_r")) return(as.character(unclass(x)))
  if (is.character(x)) return(paste(x, collapse = " "))
  nm <- names(x)
  parts <- vapply(seq_along(x), function(i) {
    v <- .cells_entry_text(x[[i]])
    if (!is.null(nm) && nzchar(nm[i])) paste(nm[i], "=", v) else v
  }, "")
  paste(parts, collapse = "; ")
}

# The table's rows: the ARD's analysis variables, then the variables the
# plan names that the ARD lacks (kept: their settings would go).  A data
# frame: variable, in_ard, own (named in plan_cells()), kind (the common
# template it takes: from the ARD's statistics; one the ARD lacks
# continuous).
.cells_vars_rows <- function(pc, meta) {
  ard <- if (!is.null(meta$variables)) meta$variables$variable else character()
  own <- .cells_var_names(pc)
  vars <- unique(c(ard, own))
  kind <- vapply(vars, function(v) .cells_var_kind(meta, v), "", USE.NAMES = FALSE)
  kind[is.na(kind)] <- "continuous"
  data.frame(variable = vars, in_ard = vars %in% ard, own = vars %in% own,
             kind = kind, stringsAsFactors = FALSE)
}

# the one-line summary: the variables of their own ("prob (3), time"), or none
.cells_vars_summary <- function(pc, lang = "en") {
  v <- .cells_own_vars(pc)
  sprintf(tr("Per variable: %s", lang),
          if (is.null(v)) tr("none", lang) else paste(v, collapse = ", "))
}

# A chip's text: the statistic with its decimals (the report's rule; a
# percent's from the categorical format), e.g. {mean:.1f}
.cells_chip_text <- function(stat, dg = integer(), pct = NA) {
  if (identical(stat, "p") && length(pct) == 1L && !is.na(pct)) {
    return(sprintf("{p:.%df%%}", as.integer(pct)))
  }
  d <- if (stat %in% names(dg)) dg[[stat]] else NA
  if (length(d) == 1L && !is.na(d)) sprintf("{%s:.%df}", stat, as.integer(d)) else
    sprintf("{%s}", stat)
}

# ---- writing ----------------------------------------------------------------

# plan_cells() of a report with variable `v`'s entry set (NULL: it takes the
# common template again): its whole arguments, as .builder_item_write()
# takes them.  The same as now: the planner as it is (nothing written).
.cells_var_put <- function(x, output_id, v, value) {
  its <- builder_items(x, output_id)
  pc <- .builder_args(its, "plan_cells") %||% list()
  if (identical(pc[[v]], value)) return(x)
  if (is.null(value)) pc[[v]] <- NULL else pc[v] <- list(value)
  .builder_item_write(x, output_id, "plan_cells", pc)
}

# ---- the company standard and the study standard -----------------------------

# plan_cells() as the company standards give it: the categorical (default)
# template of default_cells, and the continuous rows -- default_cells' own,
# else the builder's first rows of the statistics -- with default_digits'
# decimals
.cells_company <- function() {
  cs <- company_standards()
  dc <- cs$default_cells
  out <- list()
  if (!is.null(dc) && nrow(dc)) {
    g <- dc[is.na(dc$row) & (is.na(dc$variable) | dc$variable == "categorical") &
              !is.na(dc$template), , drop = FALSE]
    for (i in seq_len(nrow(g))) {
      k <- if (is.na(g$variable[i])) "default" else "categorical"
      if (is.null(out[[k]])) out[[k]] <- g$template[i]
    }
  }
  cr <- if (!is.null(dc) && nrow(dc)) {
    dc[!is.na(dc$variable) & dc$variable == "continuous" & !is.na(dc$row), , drop = FALSE]
  }
  if (!is.null(cr) && nrow(cr)) {
    rows <- stats::setNames(as.list(cr$template), cr$row)
  } else {
    bs <- builder_stats()
    r <- intersect(.builder_default_rows, bs$row)
    rows <- stats::setNames(as.list(bs$template[match(r, bs$row)]), r)
  }
  dg <- .std_digits()
  out$continuous <- lapply(rows, .with_digits, dg)
  out
}

# the study standard's plan_cells() (the study's defaults.yml)
.cells_study <- function(p) {
  st <- tryCatch(.spec_study(p), error = function(e) NULL)
  cont <- st$defaults$content %||% list()
  if (inherits(cont, "tfl_r")) return(list())
  at <- which(.content_fns(cont) == "plan_cells")
  if (!length(at)) return(list())
  cont[[at[1L]]]$args %||% list()
}

# Where the study standard differs from the company standard, an item a
# row: key ("default", "categorical", "continuous:<row>"), label, company,
# study (NA: the study has none), differs (both have one, not the same)
.cells_company_diff <- function(p, company = .cells_company()) {
  sc <- .cells_study(p)
  out <- data.frame(key = character(), label = character(), company = character(),
                    study = character(), differs = logical(), stringsAsFactors = FALSE)
  add <- function(key, label, a, b) {
    b <- if (is.character(b) && length(b) == 1L) b else
      if (is.null(b)) NA_character_ else .cells_entry_text(b)
    if (identical(a, b)) return()
    out[nrow(out) + 1L, ] <<- list(key, label, a, b, !is.na(b))
  }
  for (k in intersect(c("default", "categorical"), names(company))) {
    add(k, k, company[[k]], sc[[k]])
  }
  sct <- sc$continuous
  if (inherits(sct, "tfl_call")) sct <- sct$args
  # (a study with no rows of its own: the company's rows as one item -- a
  # row alone would be every continuous variable's only row)
  if (is.null(sct)) {
    if (length(company$continuous)) {
      add("continuous", "continuous", .cells_entry_text(company$continuous), NULL)
    }
    return(out)
  }
  for (r in names(company$continuous)) {
    add(paste0("continuous:", r), paste0("continuous: ", r), company$continuous[[r]], sct[[r]])
  }
  out
}

# The study standard's plan_cells() with the chosen items (`keys`, of
# .cells_company_diff()) the company's; the others as they are
.cells_company_apply <- function(sc, keys, company = .cells_company()) {
  for (k in keys) {
    if (k %in% c("default", "categorical", "continuous")) {
      sc[[k]] <- company[[k]]
    } else if (startsWith(k, "continuous:")) {
      r <- substring(k, nchar("continuous:") + 1L)
      ct <- sc$continuous
      call <- if (inherits(ct, "tfl_call")) ct
      rows <- if (!is.null(call)) call$args %||% list() else ct %||% list()
      # (a row the study did not have goes after its own; `keys` in the
      # company's order)
      rows[[r]] <- company$continuous[[r]]
      if (!is.null(call)) {
        call$args <- rows
        sc$continuous <- call
      } else {
        sc$continuous <- rows
      }
    }
  }
  sc
}

# The study standard's plan_cells() set to `args`, written into the study's
# defaults.yml (and the view's study rows of the cells sheet with it)
.spec_set_study_cells <- function(p, args) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  sp <- p$spec
  d <- sp$study$defaults %||% list()
  content <- d$content %||% list()
  item <- if (length(args)) tflspec::tfl_item("plan_cells", args)
  d$content <- .content_put(content, "plan_cells", 1L, item)
  sp$study$defaults <- d
  p$spec <- sp
  p <- .spec_set_study_part(p, "defaults")
  # (the view's study rows of the cells sheet, as the spec now says them)
  lean <- p$spec$study
  lean$patterns <- list()
  lean$styles <- list()
  v <- .planner_from_spec(lean, list())
  old <- sheet_rows(p, "cells", NA_character_)
  new <- sheet_rows(v, "cells", NA_character_)
  no_id <- function(d) { d$output_id <- NULL; rownames(d) <- NULL; d }
  if (!identical(no_id(old), no_id(new))) p <- set_sheet_rows(p, "cells", NA_character_, no_id(new))
  if (!is.null(p$spec$base)) p$spec$base <- .planner_body(p)
  p
}
