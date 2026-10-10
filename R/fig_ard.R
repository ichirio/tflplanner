# A figure's ARD (#293): where the statistics it prints come from.  The
# report row's `ard_source` says it, as it says `import:<file>` for a
# table whose ARD was made elsewhere:
#
#   blank             none: the figure reads its data only
#   own               its own analyses (step 2, as a table's), its own ARD
#                     program; the figure reads its rows of the study ARD
#   table:<id>        that table's rows of the study ARD (no ARD program
#                     of its own)
#   import:<file>     an ARD taken in (input/ard/)
#
# The program puts it in `ard` at the top of `# ---- data ----`; the
# design's `ard_stats` steps and `ard_number` layers read it with the
# study helpers' ard_stats() / ard_value().

#' Where a figure's ARD comes from
#'
#' Sets the figure's report row `ard_source`: its own analyses (`"own"`), a
#' table's ARD (`"table:<output_id>"`), or none (`NULL`).  An ARD taken in
#' is [use_imported_ard()].  The figure's program then reads that ARD as
#' `ard`, for its design's `ard_stats` steps and `ard_number` layers (see
#' [tflspec::tfl_fig_parts()]).
#'
#' @param x A `tflplanner`.
#' @param output_id The figure.
#' @param source `"own"`, `"table:<output_id>"` (a table of the study), or
#'   `NULL`.
#' @return `x`.
#' @export
set_fig_ard_source <- function(x, output_id, source = NULL) {
  if (!is.null(source)) {
    source <- trimws(source)
    if (startsWith(source, "table:")) {
      tb <- sub("^table:", "", source)
      if (!.is_table(x, tb)) {
        stop(sprintf("%s is not a table of the study.", tb), call. = FALSE)
      }
    } else if (!identical(source, "own")) {
      stop("The source is \"own\", \"table:<output_id>\", or NULL.", call. = FALSE)
    }
  }
  r <- x$sheets$report
  if (!"ard_source" %in% names(r)) r$ard_source <- NA_character_
  i <- which(!is.na(r$output_id) & r$output_id == output_id)
  if (!length(i)) {
    r[nrow(r) + 1L, ] <- NA
    i <- nrow(r)
    r$output_id[i] <- output_id
  }
  r$ard_source[i] <- if (is.null(source)) NA_character_ else source
  x$sheets$report <- r
  x
}

# a table of the study (report_info() of an id the study does not have is
# the default row's: a table)
.is_table <- function(x, id) {
  id %in% output_ids(x) && identical(report_info(x, id)$type, "table")
}

# the analyses a design's ARD pieces name
.fig_ard_analyses <- function(design) {
  unique(c(unlist(lapply(design$data, function(s) if (identical(s$step, "ard_stats")) .ard_ids(s$analysis_id))),
           unlist(lapply(design$layers, function(l) if (identical(l$layer, "ard_number")) .ard_ids(l$analysis_id)))))
}

# The figures printing a table's analyses, for the ARS (#293): output_id
# (the figure), source (the table), analysis_id (each analysis its design's
# ARD pieces name); a figure with its own analyses is an ARS output of its
# own already
.fig_ars_references <- function(x) {
  out <- data.frame(output_id = character(), source = character(),
                    analysis_id = character(), stringsAsFactors = FALSE)
  for (id in names(x$fig_designs %||% list())) {
    src <- .fig_ard_source(x, id)
    if (!identical(src$kind, "table")) next
    an <- .fig_ard_analyses(fig_design(x, id))
    if (length(an)) out <- rbind(out, data.frame(output_id = id, source = src$id,
                                                 analysis_id = an, stringsAsFactors = FALSE))
  }
  out
}

# A figure's ARD source: kind (none, own, table, import) and its report
# (own: the figure; table: the table) or file (import)
.fig_ard_source <- function(x, output_id) {
  src <- if (!is.null(x$sheets$report)) .resolve_row(x, "report", output_id)$ard_source
  src <- if (is.null(src) || is.na(src) || !nzchar(trimws(src))) "" else trimws(src)
  if (identical(src, "own")) return(list(kind = "own", id = output_id))
  if (startsWith(src, "table:")) return(list(kind = "table", id = sub("^table:", "", src)))
  if (startsWith(src, "import:")) return(list(kind = "import", file = sub("^import:", "", src)))
  list(kind = "none")
}

# whether a figure's design reads an ARD (its ard_stats / ard_number pieces)
.fig_reads_ard <- function(design, output_id = "fig") {
  if (is.null(design)) return(FALSE)
  isTRUE(attr(tflspec::tfl_fig_design_code(design, output_id, setup = TRUE, save = FALSE),
              "ard"))
}

# The lines of a figure's program that put its ARD in `ard` (none without a
# source; a design that reads one without a source stops, saying where to
# choose it)
.fig_ard_lines <- function(x, output_id, design = NULL) {
  src <- .fig_ard_source(x, output_id)
  path <- .path_lit(.ard_study_value(x$ard, "output", "output/ard/ard.rds"))
  switch(src$kind,
    none = if (.fig_reads_ard(design, output_id)) c(
      "# the design reads an ARD (`ard`), but the figure has none",
      sprintf("stop(\"%s reads an ARD: choose it in the figure's step 2 (ARD).\")", output_id),
      ""),
    own = , table = {
      prog <- file.path(study_layout()[["programs_ard"]], .ard_prog_name(src$id))
      id <- encodeString(src$id, quote = "\"")
      c(if (src$kind == "own") "# this figure's ARD (its rows of the study ARD)"
        else sprintf("# the ARD of %s (its rows of the study ARD)", src$id),
        paste0("ard <- readRDS(", path, ") |>"),
        sprintf("  filter(output_id == %s)", id),
        sprintf("if (!nrow(ard)) stop(\"The study ARD has no rows for %s: run %s first.\")",
                src$id, prog),
        "# (the definition it was made from: the report records it, #293)",
        sprintf("ard_built <- ard_fingerprint(%s)", id),
        "")
    },
    import = {
      p <- .path_lit(file.path(study_layout()[["ard_import"]], src$file))
      id <- encodeString(output_id, quote = "\"")
      c(sprintf("# this figure's ARD, made elsewhere and taken in (%s)",
                file.path("input", "ard", src$file)),
        sprintf("ard <- tflspec::tfl_read_ard(%s)", p),
        sprintf("if (\"output_id\" %%in%% names(ard)) ard <- ard[ard$output_id == %s, , drop = FALSE]", id),
        "")
    })
}

# The report whose ARD a figure reads from the study ARD (its own id, or
# the table's), when its design reads one; NA otherwise (none, or an ARD
# taken in: no definition to follow)
.fig_ard_need <- function(x, output_id) {
  if (!identical(report_info(x, output_id)$type, "figure")) return(NA_character_)
  src <- .fig_ard_source(x, output_id)
  if (!src$kind %in% c("own", "table")) return(NA_character_)
  if (!.fig_reads_ard(fig_design(x, output_id), output_id)) return(NA_character_)
  src$id
}

# The figures printing an ARD's numbers that are to be made again because
# of that ARD (#293): "ard:<id>" when the ARD is not made from its
# definition now (not made, an error, another definition or study setup),
# or the figure was made from another definition than the one now; "" for
# the others.  Read from ard_status.csv and report_status.csv, the
# fingerprint worked out once a table: for study_status() and the report
# list's lighter state alike.
.fig_ard_why <- function(study, ids) {
  p <- study$planner
  out <- stats::setNames(rep("", length(ids)), ids)
  need <- vapply(ids, function(id) tryCatch(.fig_ard_need(p, id), error = function(e) NA_character_), "")
  if (all(is.na(need))) return(out)
  st <- .read_ard_status(study)
  from <- .report_ard_recorded(study$path, ids)
  for (tb in unique(stats::na.omit(need))) {
    now <- tryCatch(tflspec::tfl_ard_spec_hash(structure(p$ard, class = "tfl_ard_spec"), tb,
                                               dir = study$path, codelists = .study_codelists(p)),
                    error = function(e) NA_character_)
    r <- st[st$output_id == tb, , drop = FALSE]
    made <- nrow(r) > 0L && (is.na(r$error[1L]) || !nzchar(r$error[1L])) &&
      identical(r$definition[1L], now) && !isTRUE(.setup_changed(r$setup[1L], study$path))
    k <- which(need %in% tb)
    stale <- !made | (!is.na(from[k]) & nzchar(from[k]) & !(from[k] %in% now))
    out[k[stale]] <- paste0("ard:", tb)
  }
  out
}

# The rows a figure's ARD has now (its source's; NULL when there is none
# or it is not made yet): for the preview and the checks
.fig_ard_rows <- function(study, output_id, x = study$planner) {
  src <- .fig_ard_source(x, output_id)
  switch(src$kind,
    none = NULL,
    own = , table = tryCatch(study_ard_rows(study, src$id), error = function(e) NULL),
    import = {
      p <- file.path(.import_dir(study), src$file)
      if (!file.exists(p)) return(NULL)
      a <- tflspec::tfl_read_ard(p)
      if ("output_id" %in% names(a)) a[a$output_id %in% output_id, , drop = FALSE] else a
    })
}

# ids in a field: "KM, HR" (or "KM | HR") as c("KM", "HR")
.ard_ids <- function(x) {
  if (is.null(x)) return(character())
  v <- trimws(unlist(strsplit(as.character(x), "[,|]")))
  v[nzchar(v)]
}

# The figures whose ARD is a table's: their ids, by the table's
.figs_reading_table <- function(x, table_id) {
  r <- x$sheets$report
  if (is.null(r) || !"ard_source" %in% names(r)) return(character())
  k <- !is.na(r$output_id) & !is.na(r$ard_source) &
    r$ard_source == paste0("table:", table_id)
  r$output_id[k]
}

# A table renamed: the figures reading its ARD read it under its new id
.rename_fig_ard_refs <- function(x, from, to) {
  r <- x$sheets$report
  if (is.null(r) || !"ard_source" %in% names(r)) return(x)
  k <- !is.na(r$ard_source) & r$ard_source == paste0("table:", from)
  r$ard_source[k] <- paste0("table:", to)
  x$sheets$report <- r
  x
}

# A figure's ARD problems, for the study review and the designer: its
# source (a table that is not one; a design that reads an ARD without a
# source; an ARD not made yet) and its pieces against the rows it has.
# `rule` (the review's, F04-F08), and each problem's sentence (`template`)
# and values (`args`), for a translation
.fig_ard_problems <- function(study, output_id, design = fig_design(study$planner, output_id)) {
  rows <- list()
  add <- function(p, f, rule, tpl, args = character(), sev = "error") {
    rows[[length(rows) + 1L]] <<- list(
      part = p, field = f, problem = do.call(sprintf, c(list(tpl), as.list(args))),
      severity = sev, rule = rule, template = tpl, args = as.character(args))
  }
  done <- function() {
    col <- function(nm) vapply(rows, function(r) as.character(r[[nm]]), "")
    out <- data.frame(part = col("part"), field = col("field"), problem = col("problem"),
                      severity = col("severity"), rule = col("rule"),
                      template = col("template"), stringsAsFactors = FALSE)
    out$args <- lapply(rows, `[[`, "args")
    out
  }
  x <- study$planner
  src <- .fig_ard_source(x, output_id)
  reads <- .fig_reads_ard(design, output_id)
  if (src$kind == "table" && !.is_table(x, src$id)) {
    add("ard", "ard_source", "F04",
        if (src$id %in% output_ids(x)) "%s is not a table of the study"
        else "%s is not a table of the study (deleted, or renamed by hand)", src$id)
    return(done())
  }
  if (src$kind == "none") {
    if (reads) add("ard", "ard_source", "F05",
                   "the design reads an ARD, but the figure has none: choose it in step 2")
    return(done())
  }
  if (!reads) return(done())
  # the analyses the pieces name are the source's definition's (its ARD
  # made before a change still has the old ones)
  if (src$kind %in% c("own", "table")) {
    defined <- ard_rows(x, "analyses", src$id)$analysis_id
    for (a in setdiff(.fig_ard_analyses(design), defined)) {
      add("ard", "analysis_id", "F06",
          "%s has no analysis %s (any more): the figure prints from it", c(src$id, a))
    }
    if (length(rows)) return(done())
  }
  ard <- .fig_ard_rows(study, output_id)
  if (is.null(ard) || !nrow(ard)) {
    add("ard", "ard_source", "F07", "the ARD of %s is not made yet: make it first (its step 2)",
        if (src$kind == "import") src$file else src$id, "warning")
    return(done())
  }
  ck <- tflspec::tfl_check_fig_design(design, ard = ard)
  ck <- ck[grepl("ard_", ck$part, fixed = TRUE), , drop = FALSE]
  # tflspec's words about the ARD's rows (no analysis, no statistic ...)
  for (i in seq_len(nrow(ck))) add(ck$part[i], ck$field[i], "F08", "%s", ck$problem[i])
  done()
}

#' A figure's own analyses, written from its design
#'
#' A design made from a template that brings its analyses -- the forest
#' plot's hazard ratios, `attr(design, "analyses")` from
#' [tflspec::tfl_fig_forest_analyses()] -- writes them to the figure's ARD
#' definition: its `analysis_data` and `analyses` rows (a row of the same
#' id is replaced, the others kept) and the report row's `ard_source =
#' "own"`.  The study's ARD then has to be made again for the figure.
#'
#' @param x A `tflplanner`.
#' @param output_id The figure.
#' @param analyses A list of two data frames, `analysis_data` and
#'   `analyses` (without `output_id`), as a design's attribute `analyses`.
#' @return The `tflplanner`, with attribute `written`: the ids of the
#'   analyses written.
#' @export
set_fig_own_analyses <- function(x, output_id, analyses) {
  if (!is.list(analyses) || !all(c("analysis_data", "analyses") %in% names(analyses))) {
    stop("`analyses` is a list of two data frames, analysis_data and analyses.",
         call. = FALSE)
  }
  put <- function(sheet, key, new) {
    if (is.null(new) || !nrow(new)) return()
    new$output_id <- NULL
    have <- ard_rows(x, sheet, output_id)
    have$output_id <- NULL
    have <- have[!have[[key]] %in% new[[key]], , drop = FALSE]
    new <- .normalize_ard_sheet(new, sheet)
    new$output_id <- NULL
    rows <- rbind(have, new[names(have)])
    x <<- set_ard_rows(x, sheet, output_id, rows)
  }
  put("analysis_data", "data_id", analyses$analysis_data)
  put("analyses", "analysis_id", analyses$analyses)
  x <- set_fig_ard_source(x, output_id, "own")
  attr(x, "written") <- analyses$analyses$analysis_id
  x
}
