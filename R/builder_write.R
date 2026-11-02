# The table builder, writing: builder_write() and the cells and header it
# writes back to the sheets.  Split from builder.R (no code change).

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
                              hier = rk %in% st$hierarchy,
                              nocol = attr(st$sort, "nocol") %||% character())
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
  # (the statistics whose decimals changed: a variable's own entries and
  # the conditions take them too, nothing of theirs dropped -- #454)
  chg <- if (dig_ch && !fmt) .cells_digits_changed(dg, was$digits) else integer()
  rows_tpl <- function(dg) {
    old <- pc$continuous %||% below$continuous %||% list()
    # (rows with conditions are cell_rows(): its rows are its arguments)
    call <- if (inherits(old, "tfl_call")) old
    if (!is.null(call)) old <- call$args %||% list()
    had <- length(old) > 0L
    rows <- if (had || any(st$variables$kind %in% "continuous")) st$rows else character()
    out <- lapply(rows, function(lb) {
      o <- old[[lb]]
      # a row of conditions, or a template of its own, as written (the
      # decimals changed put in its templates too: #454)
      if (!is.null(o) && !(is.character(o) && length(o) == 1L)) return(.cells_redigit(o, chg))
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
    for (v in setdiff(.cells_var_names(new), exc_vars)) {
      new[v] <- list(.cells_redigit(new[[v]], chg))
    }
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
