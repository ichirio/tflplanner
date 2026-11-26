# A report's code list that is one of the study's lists (tflspec#977, A7):
# `SEX: {list: SEX}` in the report's file -- all the codes of the study's
# list SEX (spec/defaults.yml `codelists:`), in its order -- or `SEX: {list:
# SEX, codes: [M, F]}`, those codes in that order.  On the codelists sheet
# each of its rows has the reference in its `list` cell (`SEX`, `SEX: M |
# F`), and its rows are the study's codes and labels, which tflspec writes
# and does not read back.  Here those rows are derived: shown read only,
# never edited, and made again from the study's lists whenever these change
# (.spec_codelist_refs()), so the programs written from the rows (the ARD,
# the listings, the ARD's hash) never use a list the study no longer has.

# A `list` cell as the reference it is: list(list = <name>, codes = <the
# codes picked, or NULL: all>) -- tflspec's reading of the cell
# (.cl_ref_from_cell(): the name before the first ":", the codes after it
# split at "|")
.cl_ref_parse <- function(cell) {
  codes <- if (grepl(":", cell, fixed = TRUE)) .split_bar(sub("^[^:]*:", "", cell))
  list(list = trimws(sub(":.*$", "", cell)), codes = codes[nzchar(codes)])
}

# A reference's rows as tflspec's sheets give them (tfl_sheets_from_spec()):
# each code (those picked, in the order picked) with the study's label (a
# list of codes alone: the code), `order` 1, 2, ..., the cell on every row.
# A list or a code the study has not got: one row with the cell and no
# code (the study opens; the check and the programs say it).  `row`: the
# report's first row of the variable (its output_id, variable, columns).
.cl_ref_sheet_rows <- function(row, cell, study_cl) {
  ref <- .cl_ref_parse(cell)
  x <- study_cl[[ref$list]]
  codes <- if (!is.null(x)) as.character(names(x) %||% unname(x))
  want <- if (length(ref$codes)) ref$codes else codes
  if (is.null(x) || !length(want) || length(setdiff(want, codes))) {
    want <- NA_character_
    lab <- NA_character_
    ord <- NA_character_
  } else {
    lab <- stats::setNames(as.character(unname(x)), codes)[want]
    ord <- as.character(seq_along(want))
  }
  out <- row[rep(1L, length(want)), , drop = FALSE]
  for (cn in setdiff(names(out), c("output_id", "variable"))) out[[cn]] <- NA_character_
  out$value <- want
  out$label <- unname(lab)
  out$order <- ord
  out$list <- cell
  rownames(out) <- NULL
  out
}

# The codelists sheet's rows (`d`, with output_id, or a report's without)
# with every reference's rows made from the study's lists `study_cl`
# (study$codelists): a report's variable whose rows all have one `list`
# cell gets that list's rows, where its first row was.  Rows whose cells
# differ (some blank, or two lists) are left as they are: tflspec stops on
# them when they are read, naming them.  The other rows: as they are.
.cl_refs_remake <- function(d, study_cl) {
  if (is.null(d) || !nrow(d) || !"list" %in% names(d)) return(d)
  lc <- trimws(as.character(d$list))
  has <- !is.na(lc) & nzchar(lc)
  if (!any(has)) return(d)
  oid <- if ("output_id" %in% names(d)) as.character(d$output_id) else rep("", nrow(d))
  grp <- paste(oid, d$variable, sep = "\r")
  out <- list()
  for (g in unique(grp)) {
    k <- grp == g
    cells <- unique(ifelse(has[k], lc[k], ""))
    out[[length(out) + 1L]] <- if (length(cells) == 1L && nzchar(cells)) {
      .cl_ref_sheet_rows(d[which(k)[1L], , drop = FALSE], cells, study_cl)
    } else {
      d[k, , drop = FALSE]
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# The variables of a report's rows that are references (a `list` cell)
.cl_ref_vars <- function(rows) {
  if (is.null(rows) || !nrow(rows) || !"list" %in% names(rows)) return(character())
  lc <- as.character(rows$list)
  unique(rows$variable[!is.na(lc) & nzchar(trimws(lc))])
}

# The planner with every reference's rows made again from its spec's study
# lists (p$spec$study$codelists): the view's, and the base's (the view the
# spec was last in step with: its rows are the spec's too, so a change of
# the study's lists alone is no edit of a report).  The one place that does
# it: on open and reload (.spec_attach()), and wherever the study's lists
# change in memory.
.spec_codelist_refs <- function(p) {
  if (is.null(p$spec) || is.null(p$sheets$codelists)) return(p)
  scl <- p$spec$study$codelists
  p$sheets$codelists <- .cl_refs_remake(p$sheets$codelists, scl)
  if (!is.null(p$spec$base$sheets$codelists)) {
    p$spec$base$sheets$codelists <- .cl_refs_remake(p$spec$base$sheets$codelists, scl)
  }
  p
}

# A grid's rows of a report's code lists (`d`) with what the grid cannot
# change put back as the report has it (`old`, the same report's rows the
# grid showed): a reference's rows (its variable's rows, all of them, where
# the grid had the first; at the end when the grid had none) and the `list`
# column of the other rows (blank: a report's own list is made one of the
# study's in its file).  attr "kept": the variables an edit of the grid
# did not change, for the message.
.cl_refs_keep <- function(d, old) {
  refs <- .cl_ref_vars(old)
  if (!"list" %in% names(d)) d$list <- rep(NA_character_, nrow(d))
  own <- !d$variable %in% refs
  typed <- own & !is.na(d$list) & nzchar(trimws(as.character(d$list)))
  kept <- unique(d$variable[typed])
  d$list[own] <- NA_character_
  cols <- intersect(names(d), names(old))
  same <- function(a, b) {
    a <- a[cols]
    b <- b[cols]
    a[] <- lapply(a, function(v) {
      v <- trimws(as.character(v))
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    })
    b[] <- lapply(b, function(v) {
      v <- trimws(as.character(v))
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    })
    rownames(a) <- NULL
    rownames(b) <- NULL
    identical(a, b)
  }
  ref_rows <- lapply(stats::setNames(refs, refs), function(v) {
    mine <- old[old$variable %in% v, , drop = FALSE]
    for (cn in setdiff(names(d), names(mine))) mine[[cn]] <- rep(NA_character_, nrow(mine))
    mine[names(d)]
  })
  for (v in refs) if (!same(d[d$variable %in% v, , drop = FALSE], ref_rows[[v]])) kept <- c(kept, v)
  # the grid's rows in its order, a study's list where the grid had its
  # first row (at the end when the grid had none)
  parts <- list()
  placed <- character()
  for (i in seq_len(nrow(d))) {
    v <- d$variable[i]
    if (own[i]) {
      parts[[length(parts) + 1L]] <- d[i, , drop = FALSE]
    } else if (!v %in% placed) {
      parts[[length(parts) + 1L]] <- ref_rows[[v]]
      placed <- c(placed, v)
    }
  }
  parts <- c(parts, ref_rows[setdiff(refs, placed)])
  out <- do.call(rbind, c(list(d[0L, , drop = FALSE]), unname(parts)))
  rownames(out) <- NULL
  attr(out, "kept") <- unique(kept)
  out
}

# What the grids say when an edit of a study's list was not taken in:
# which variables, and where to change them (the study's file, or the
# report's file -- `output_id`)
.cl_refs_kept_text <- function(vars, t, output_id) {
  sprintf(t("%s not changed: it is the study's list, read only here. Change it in spec/defaults.yml (codelists:) or in the report's file (spec/outputs/%s.yml)."),
          paste(vars, collapse = ", "), output_id)
}

# The name of the study's list a `list` cell refers to ("SEX: M | F": SEX)
.cl_ref_name <- function(cell) .cl_ref_parse(cell)$list

# One line of a reference for the refs line over the grid: the variable
# <- the study's list (and the codes picked, when it takes some)
.cl_ref_line <- function(variable, cell, t) {
  ref <- .cl_ref_parse(cell)
  if (length(ref$codes)) {
    sprintf(t("%s \u2190 study list %s (%s)"), variable, ref$list,
            paste(ref$codes, collapse = ", "))
  } else {
    sprintf(t("%s \u2190 study list %s"), variable, ref$list)
  }
}
