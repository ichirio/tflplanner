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

# What a row's own digits (cells$digits "1,2") give its statistics
.row_digits <- function(tpl, digits) {
  if (is.na(digits) || !nzchar(digits)) return(stats::setNames(integer(), character()))
  st <- .template_stats(tpl)
  dg <- suppressWarnings(as.integer(trimws(strsplit(digits, ",")[[1L]])))
  dg <- dg[!is.na(dg)]
  if (!length(dg) || !length(st)) return(stats::setNames(integer(), character()))
  stats::setNames(dg[pmin(seq_along(st), length(dg))], st)
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

#' Read and write a table the builder way
#'
#' `builder_read()` describes a report's summary table from its sheets (and
#' what its ARD holds, for what the sheets do not say yet);
#' `builder_write()` writes such a description back to the report's own
#' rows of `tables`, `variables`, `cells` and `col_header`.  Rows it does
#' not manage (a statistic it does not offer, a variable of its own
#' template) are left as they are.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param meta Its [ard_meta()], or `NULL`.
#' @param state What `builder_read()` returns, as edited.
#' @param was What `builder_read()` returned before the edit, or `NULL`.
#'   Given it, only what differs from it is written: what the builder does
#'   not show (a statistic's own template, a condition, an order of one's
#'   own) stays as the sheets have it.  `NULL` writes the whole description.
#' @return `builder_read()`: a list -- `key` (the column variables,
#'   outermost first), `arms` (a named list: each column variable's levels
#'   in order), `variables` (a data frame: `variable`,
#'   `kind`, `label`), `levels` (named list), `rows` (a continuous
#'   variable's row labels in order) and their `templates`, `value`
#'   (`stat`: the numbers, rounded by `digits`; `stat_fmt`: the ARD's text),
#'   `digits` (each statistic the rows print: its decimals), `exceptions`
#'   (a variable's own: `variable`, `statistic`, `digits`), `cat_format` (`npct`,
#'   `nNpct`, `n`), `pct_decimals`, `header` (`keep` or a name of
#'   [header_presets()]), and for a hierarchy table (SOC / PT) its
#'   `hierarchy` (the ARD's terms, outermost first), `row_keys` (the
#'   table's rows, outermost first: by default the ARD's other groups, then
#'   its hierarchy), `row_choices` (the keys it may take), `row_pages` (the
#'   outermost a page each), `sort` (each row key's
#'   order, outermost first -- `"alpha"` by name, or a code list's order;
#'   `"freq"` most frequent first, ties by name -- or `"own"`: an order of
#'   the sheets' own, left as it is) and `overall_label` (the "any event"
#'   row's name, `tables$overall_label`).  `builder_write()`: the
#'   `tflplanner`.
#' @export
builder_read <- function(x, output_id, meta = NULL) {
  id <- output_id
  tb <- .rows_for(x, "tables", id)
  # the column variables, outermost first (tables$cols "A | B")
  key <- .split_list(tb$cols[1L])
  if (!length(key) && !is.null(meta) && length(meta$by)) key <- meta$by[1L]
  if (!length(key)) key <- NA_character_

  vr <- .rows_for(x, "variables", id)
  vr <- vr[!duplicated(vr$variable), , drop = FALSE]
  cl <- tryCatch(sheet_rows(x, "codelists", id), error = function(e) NULL)
  # a variable's levels in order, by .level_order() (this table's own, the
  # report's code list, the ARD's factor, as first seen) with where they
  # come from (`level_from`); those no sheet states are remembered, so
  # that writing them back unchanged writes nothing
  auto <- list()
  from <- list()
  lev_of <- function(v) {
    own <- .split_list(vr$levels[match(v, vr$variable)])
    ard <- if (is.null(meta)) character() else if (v %in% names(meta$keys)) meta$keys[[v]] else
      .split_list(meta$variables$levels[match(v, meta$variables$variable)])
    fac <- v %in% c(meta$factor_vars, meta$factor_keys)
    o <- .level_order(own, .codelist_order(cl, v), ard, fac)
    if (!identical(o$source, "table") && (!is.null(meta) || identical(o$source, "codelist"))) {
      auto[[v]] <<- o$levels
    }
    if (length(o$levels)) from[[v]] <<- o[c("source", "differs")]
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
  ly <- .rows_for(x, "layout", id)
  # the rows as layers, outermost first, for both kinds: a hierarchy's
  # row keys; a summary table's keys over its block of variables
  # (".variable": group = variable)
  if (length(hier)) {
    layers <- rk
    layer_choices <- setdiff(names(meta$keys), key)
  } else {
    layers <- .row_layers_read(tb$rows[1L])
    layer_choices <- c(setdiff(names(meta$keys) %||% character(), c(key, vars)), ".variable")
  }
  list(key = key, arms = arms, variables = variables, levels = levels,
       rows = rows, templates = templates, value = value, digits = digits,
       exceptions = exc, cat_format = cat_format, cat_own = cat_own,
       pct_decimals = pct_decimals, header = "keep", auto_levels = auto,
       level_from = from,
       hierarchy = hier, row_keys = rk,
       row_choices = if (length(hier)) setdiff(names(meta$keys), key) else character(),
       row_pages = length(hier) > 0L && isTRUE(as.logical(ly$group_page[1L])) &&
         identical(ly$group_col[1L] %||% NA_character_, "group1"),
       sort = if (length(hier)) .hier_sort_read(tb$sort[1L], length(rk)),
       overall_label = if (length(hier)) tb$overall_label[1L] %||% NA_character_,
       layers = layers, layer_choices = unique(c(layers, layer_choices)))
}

# A variable's levels in order, data first, and where the order comes
# from: this table's own (the variables sheet's levels: "table"), the
# report's code list ("codelist", ARD step 1-1 -- tflspec writes
# plan_levels() from it, so the screen and the code agree), the ARD's
# factor ("ard": `ard_factor`, its levels), else the ARD's values as
# first seen ("seen"); a code list's order is followed by the ARD's values
# it does not name.  The precedence is `steps`' order, here only.
# `differs`: the ARD keeps a factor's order and the code list puts the
# values they share in another.
.level_order <- function(own = character(), codelist = character(), ard = character(),
                         ard_factor = FALSE) {
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
# = variable"): the keys, and ".variable" for the block of the analysis
# variables; the block alone when the rows say nothing
.row_layers_read <- function(rows) {
  if (.is_blank(rows)) return(".variable")
  part <- trimws(strsplit(rows, "|", fixed = TRUE)[[1L]])
  v <- trimws(sub("^[^=]*=", "", part))
  v <- v[nzchar(v) & !grepl("^[\"'~]", v)]
  v[v == "variable"] <- ".variable"
  if (!".variable" %in% v) v <- c(v, ".variable")
  v
}

# ... and back: the keys a group each (group1, group2 ...), outermost
# first, the block as `group = variable`
.row_layers_write <- function(layers) {
  keys <- setdiff(layers, ".variable")
  paste(c(if (length(keys)) sprintf("group%d = %s", seq_along(keys), keys),
          "group = variable"), collapse = " | ")
}

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

#' @rdname builder_read
#' @export
builder_write <- function(x, output_id, state, was = NULL) {
  id <- output_id
  st <- state
  # what the form changed from what it read (`was`): only that is written,
  # so what the form cannot show -- a statistic's own template, a
  # condition, an order of one's own -- stays as the sheets have it
  changed <- function(part) is.null(was) || !identical(st[[part]], was[[part]])
  # tables: the column key, and one group per variable unless said otherwise
  tb <- sheet_rows(x, "tables", id)
  tb$output_id <- NULL
  if (!nrow(tb)) tb[1L, ] <- NA
  # (a key the sheets do not state yet, read from the ARD, is written too)
  inh_tb <- inherited_rows(x, "tables", id)
  no_cols <- is.na(tb$cols[1L]) && (!nrow(inh_tb) || is.na(inh_tb$cols[1L]))
  put_key <- !anyNA(st$key) && (changed("key") || no_cols)
  if (put_key) tb$cols[1L] <- paste(st$key, collapse = " | ")
  # the value the table prints: the number (rounded here) or the ARD's text
  if (changed("value") && !is.null(st$value)) {
    if (!"value" %in% names(tb)) tb$value <- NA_character_
    tb$value[1L] <- if (identical(st$value, "stat_fmt")) "stat_fmt" else NA_character_
  }
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
  # a hierarchy table's "any event" row's name
  if (length(st$hierarchy) && changed("overall_label")) {
    if (!"overall_label" %in% names(tb)) tb$overall_label <- NA_character_
    ol <- st$overall_label %||% NA_character_
    tb$overall_label[1L] <- if (is.na(ol) || !nzchar(trimws(ol))) NA_character_ else trimws(ol)
  }
  # a summary table's row layers: written when they changed (a key over
  # the block of variables)
  if (!length(st$hierarchy) && length(st$layers) && !is.null(was) &&
      !identical(st$layers, was$layers)) {
    tb$rows[1L] <- .row_layers_write(st$layers)
  }
  # whose {n} the header prints (the form asks only when a cell uses {n})
  if (!is.null(st$header_n) && "header_n" %in% names(tb)) tb$header_n[1L] <- st$header_n
  if (is.na(tb$rows[1L]) && (is.null(was) || put_key)) {
    inh <- inherited_rows(x, "tables", id)
    if (!nrow(inh) || is.na(inh$rows[1L])) tb$rows[1L] <- "group = variable"
  }
  x <- set_sheet_rows(x, "tables", id, tb)

  # a hierarchy table's outermost row key a page each (layout)
  if (length(st$hierarchy) && changed("row_pages") && !is.null(st$row_pages)) {
    ly <- sheet_rows(x, "layout", id)
    ly$output_id <- NULL
    if (!nrow(ly)) ly[1L, ] <- NA
    ly$group_page[1L] <- if (isTRUE(st$row_pages)) "TRUE" else NA_character_
    ly$group_col[1L] <- if (isTRUE(st$row_pages)) "group1" else NA_character_
    x <- set_sheet_rows(x, "layout", id, ly)
  }

  # variables: the arms' order, each variable's label, order and levels
  vr <- sheet_rows(x, "variables", id)
  vr$output_id <- NULL
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
  x <- set_sheet_rows(x, "variables", id, vr[!empty, , drop = FALSE])

  # cells: the continuous rows and the categorical format it manages, and
  # the statistics' decimals (the digits sheet), each written only when
  # changed
  cont_ch <- changed("rows")
  # the decimals: those the form shows against those the sheets say (with no
  # `was`, what the sheets say now)
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
  if (!cont_ch && !cat_ch && !dig_ch) return(.builder_header(x, id, st))
  ce <- sheet_rows(x, "cells", id)
  ce$output_id <- NULL
  inh <- inherited_rows(x, "cells", id)
  inh_cont <- inh[!is.na(inh$variable) & inh$variable == "continuous", ,
                  drop = FALSE]
  is_cont <- !is.na(ce$variable) & ce$variable == "continuous" & !is.na(ce$row)
  is_cat <- !is.na(ce$variable) & ce$variable == "categorical" & is.na(ce$row)
  # the rows' decimals become the statistics' when those are set: a row's
  # own digits would win over them
  clear <- dig_ch && identical(st$value %||% "stat", "stat")
  rewrite <- cont_ch || (clear && (any(!is.na(ce$digits[is_cont])) ||
                                   any(!is.na(inh_cont$digits))))
  mine <- (rewrite & is_cont) | (cat_ch & is_cat)
  keep <- ce[!mine, , drop = FALSE]
  new <- ce[0, , drop = FALSE]
  if (rewrite) {
    # a table of categorical variables only has no statistics to state
    had <- any(is_cont) || nrow(inh_cont) > 0L
    rows <- if (had || any(st$variables$kind == "continuous")) st$rows else
      character()
    old <- if (any(is_cont)) ce[is_cont, , drop = FALSE] else {
      o <- inh_cont
      o$output_id <- NULL
      o
    }
    for (lb in rows) {
      if (lb %in% old$row) {
        # a row kept: its own lines as written (its template, the chain of
        # conditions); its digits give way to the statistics'
        r <- old[old$row == lb, , drop = FALSE]
        if (clear) r$digits <- NA_character_
        new <- rbind(new, r[names(new)])
        next
      }
      new[nrow(new) + 1L, ] <- NA
      new$variable[nrow(new)] <- "continuous"
      new$row[nrow(new)] <- lb
      new$template[nrow(new)] <- st$templates[[lb]] %||%
        builder_stats()$template[match(lb, builder_stats()$row)]
    }
    same <- function(a, b) {
      identical(paste(a$row, a$template, a$digits),
                paste(b$row, b$template, b$digits))
    }
    if (nrow(inh_cont) && same(new, inh_cont)) new <- new[0, , drop = FALSE]
  }
  if (cat_ch) {
    tpl <- .cat_template(st$cat_format, st$pct_decimals, st$value %||% "stat",
                         own = st$cat_own %||% NA_character_)
    inh_cat <- inh[(!is.na(inh$variable) & inh$variable == "categorical") |
                     (is.na(inh$variable) & is.na(inh$row)), , drop = FALSE]
    if (!identical(inh_cat$template[1L], tpl)) {
      new[nrow(new) + 1L, ] <- NA
      new$variable[nrow(new)] <- "categorical"
      new$template[nrow(new)] <- tpl
    }
  }
  # the rows written first, in the place the builder's rows had
  x <- set_sheet_rows(x, "cells", id, rbind(new, keep))
  # the decimals as the form says them; rows alone changed: only those of a
  # statistic nothing states yet (a row just added)
  if (dig_ch) x <- .builder_digits(x, id, st)
  else if (cont_ch) x <- .builder_digits(x, id, st, missing_only = TRUE)
  .builder_header(x, id, st)
}

# The digits sheet as the form says it: a statistic's decimals where they
# differ from the study's (or the study has none), a variable's exceptions.
# A statistic the form does not show keeps its row.
.builder_digits <- function(x, id, st, missing_only = FALSE) {
  if (identical(st$value, "stat_fmt")) return(x)
  own <- sheet_rows(x, "digits", id)
  own$output_id <- NULL
  shown <- names(st$digits)
  if (missing_only) {
    # what states a statistic's decimals already: the study, the report's
    # own rows, a row's own digits
    sd <- x$sheets$digits
    covered <- c(if (!is.null(sd)) trimws(sd$statistic[is.na(sd$output_id) &
                                                        is.na(sd$variable)]),
                 trimws(own$statistic[is.na(own$variable)]))
    ce <- .rows_for(x, "cells", id)
    ce <- ce[ce$variable %in% "continuous", , drop = FALSE]
    covered <- c(covered, unlist(lapply(seq_len(nrow(ce)), function(i)
      names(.row_digits(ce$template[i], ce$digits[i])))))
    add <- setdiff(shown, covered)
    if (!length(add)) return(x)
    new <- own[0, , drop = FALSE]
    for (k in add) {
      if (is.na(st$digits[k])) next
      new[nrow(new) + 1L, ] <- NA
      new$statistic[nrow(new)] <- k
      new$digits[nrow(new)] <- as.character(as.integer(st$digits[[k]]))
    }
    if (!nrow(new)) return(x)
    return(set_sheet_rows(x, "digits", id, rbind(own, new[names(own)])))
  }
  keep <- own[is.na(own$variable) & !trimws(own$statistic) %in% shown, ,
              drop = FALSE]
  study <- .study_digits(x)
  study_rows <- x$sheets$digits
  stated <- if (!is.null(study_rows) && nrow(study_rows))
    trimws(study_rows$statistic[is.na(study_rows$output_id) &
                                  is.na(study_rows$variable)]) else character()
  new <- own[0, , drop = FALSE]
  for (k in shown) {
    v <- st$digits[[k]]
    if (is.na(v)) next
    if (k %in% stated && identical(as.integer(study[[k]]), as.integer(v))) next
    new[nrow(new) + 1L, ] <- NA
    new$statistic[nrow(new)] <- k
    new$digits[nrow(new)] <- as.character(as.integer(v))
  }
  exc <- st$exceptions
  if (!is.null(exc) && nrow(exc)) {
    for (i in seq_len(nrow(exc))) {
      if (is.na(exc$variable[i]) || is.na(exc$statistic[i]) ||
          is.na(exc$digits[i])) next
      new[nrow(new) + 1L, ] <- NA
      new$variable[nrow(new)] <- exc$variable[i]
      new$statistic[nrow(new)] <- exc$statistic[i]
      new$digits[nrow(new)] <- as.character(as.integer(exc$digits[i]))
    }
  }
  rows <- rbind(new, keep[names(new)])
  if (identical(paste(rows$variable, rows$statistic, rows$digits),
                paste(own$variable, own$statistic, own$digits))) return(x)
  set_sheet_rows(x, "digits", id, rows)
}

# the column header: the form's rows (a data frame), written as the
# report's own only when they differ from the header it has now
.builder_header <- function(x, id, st) {
  if (is.data.frame(st$header)) {
    own <- sheet_rows(x, "col_header", id)
    own$output_id <- NULL
    eff <- if (nrow(own)) own else {
      inh <- inherited_rows(x, "col_header", id)
      inh$output_id <- NULL
      inh
    }
    if (!.same_header(eff, st$header)) {
      x <- set_sheet_rows(x, "col_header", id, st$header)
    }
  } else if (!identical(st$header, "keep") &&
             st$header %in% names(header_presets())) {
    x <- add_preset(x, id, st$header)
  }
  x
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
    return(list(line = tr("ARD: not read yet (step 1, \"Preview this table's ARD\")"),
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
