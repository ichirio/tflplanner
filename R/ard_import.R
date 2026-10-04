# ============================================================================
#  ARDs made elsewhere, taken into a study
# ----------------------------------------------------------------------------
#  An ARD made by another program, a CRO or another tool is copied into the
#  study's input/ard/ -- a folder nothing else in tflplanner writes to, so
#  making the study ARD again, a preview or a run never overwrites it -- and
#  recorded in input/ard/imports.csv: where it came from, when, by whom, its
#  fingerprint and what tflspec::tfl_check_ard() found.  A report reads it
#  when its report row says so (`ard_source = import:<file>`); the report's
#  own ARD definition is then not used for it.  Nothing is decided by which
#  file is newer: the report row says which ARD a report reads.
# ============================================================================

.import_dir <- function(study) file.path(study$path, study_layout()[["ard_import"]])
.import_log <- function(study) file.path(.import_dir(study), "imports.csv")

.import_cols <- c("import_id", "file", "original", "source", "format",
                  "imported", "user", "md5", "rows", "outputs", "check",
                  "state")

#' ARDs taken into a study
#'
#' The record of every ARD taken in ([import_ard()]): `import_id`, the
#' `file` in the study's `input/ard/` folder, the `original` file, its
#' `source` (who made it), `format`, when it was `imported` and by which
#' `user`, its `md5`, its `rows`, the `outputs` it has rows for (its
#' `output_id` column), the result of the `check`, and its `state`
#' (`"in use"`, or `"removed"` -- a removed one stays on the record).
#'
#' @param study An `rtfstudy` ([open_study()]).
#' @return A data frame, one row per ARD taken in.
#' @export
ard_imports <- function(study) {
  f <- .import_log(study)
  empty <- as.data.frame(stats::setNames(
    replicate(length(.import_cols), character(), simplify = FALSE),
    .import_cols), stringsAsFactors = FALSE)
  if (!file.exists(f)) return(empty)
  d <- utils::read.csv(f, colClasses = "character", na.strings = "")
  for (cn in setdiff(.import_cols, names(d))) d[[cn]] <- NA_character_
  d[.import_cols]
}

.write_imports <- function(study, d) {
  f <- .import_log(study)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(f)) Sys.chmod(f, "0644")
  tmp <- paste0(f, ".tmp")
  utils::write.csv(d, tmp, row.names = FALSE, na = "")
  file.rename(tmp, f)
}

#' Take an ARD made elsewhere into a study
#'
#' Copies `path` into the study's `input/ard/` folder (read-only there),
#' reads it ([tflspec::tfl_read_ard()]: rds, the JSON / YAML of
#' [tflspec::tfl_write_ard()], XPT, CSV), checks it
#' ([tflspec::tfl_check_ard()]) -- against the table definition of each
#' report it is for, when `output_id` is given -- and records it
#' ([ard_imports()]).  It is then used by a report whose report row says
#' `ard_source = import:<file>` (see [use_imported_ard()]).
#'
#' @param study An `rtfstudy`.
#' @param path The ARD file.
#' @param output_id The report(s) it is for; `NULL` takes the ARD's own
#'   `output_id` column, if it has one.
#' @param source Who made it (a CRO, a program), for the record.
#' @param name The file name in `input/ard/`; `NULL`: the original's, made
#'   unique.
#' @return The new row of [ard_imports()], invisibly, with the check's
#'   problems as the attribute `check`.
#' @export
import_ard <- function(study, path, output_id = NULL, source = NA_character_,
                       name = NULL) {
  if (!file.exists(path)) stop("No file ", path, call. = FALSE)
  dir <- .import_dir(study)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  log <- ard_imports(study)
  nm <- name %||% basename(path)
  if (nm %in% log$file || file.exists(file.path(dir, nm))) {
    stem <- tools::file_path_sans_ext(nm)
    ext <- tools::file_ext(nm)
    k <- 2L
    while (paste0(stem, "_", k, ".", ext) %in% c(log$file, list.files(dir))) {
      k <- k + 1L
    }
    nm <- paste0(stem, "_", k, ".", ext)
  }
  ard <- tflspec::tfl_read_ard(path)
  dest <- file.path(dir, nm)
  file.copy(path, dest)
  Sys.chmod(dest, "0444")
  outs <- output_id %||% if ("output_id" %in% names(ard))
    unique(stats::na.omit(as.character(ard$output_id))) else character()
  probs <- .check_imported(study, ard, outs)
  id <- paste0("IMP", formatC(nrow(log) + 1L, width = 3L, flag = "0"))
  row <- data.frame(
    import_id = id, file = nm, original = normalizePath(path, winslash = "/"),
    source = source, format = tolower(tools::file_ext(nm)),
    imported = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user = Sys.info()[["user"]], md5 = unname(tools::md5sum(dest)),
    rows = as.character(nrow(ard)), outputs = paste(outs, collapse = " | "),
    check = .check_summary(probs), state = "in use",
    stringsAsFactors = FALSE)
  .write_imports(study, rbind(log, row))
  invisible(structure(row, check = probs))
}

# the check of each report the ARD is for (its own rows, against its table
# definition), or of the ARD's shape alone
.check_imported <- function(study, ard, outs) {
  if (!length(outs)) return(tflspec::tfl_check_ard(ard))
  sp <- tryCatch(.study_table_spec(study), error = function(e) NULL)
  out <- lapply(outs, function(id) {
    a <- if ("output_id" %in% names(ard)) ard[ard$output_id %in% id, , drop = FALSE] else ard
    has <- !is.null(sp) && id %in% unlist(lapply(sp, function(d)
      if (is.data.frame(d) && "output_id" %in% names(d)) d$output_id))
    p <- .drop_method_note(tflspec::tfl_check_ard(a, if (has) sp, if (has) id))
    if (nrow(p)) p$output_id <- id
    p
  })
  do.call(rbind, out)
}

# cards' structure check asks for the `method` rows its own ard_*() add; an
# ARD a report reads does not need them (the study's own ARD has none, and
# tflspec::tfl_write_ard() does not keep them), so the note says nothing
# about whether the report can be made -- leave it out
.drop_method_note <- function(p) {
  if (is.null(p) || !nrow(p)) return(p)
  p[!(p$level == "note" & grepl("stat_name = 'method'", p$message,
                                fixed = TRUE)), , drop = FALSE]
}

# The check's rows for the screen: the level and the message in the
# session's language.  tflspec::tfl_check_ard() writes its messages from a
# few fixed sentences; each is matched and its parts put into the
# translation.  A message from cards' own structure check stays as cards
# wrote it, introduced as such.
.check_view <- function(p, t = identity) {
  if (is.null(p) || !nrow(p)) return(p)
  pats <- list(
    c("^no column (.+): not an ARD$", "no column %s: not an ARD"),
    c("^(group[0-9]+) has no group[0-9]+_level$", "%s has no level column"),
    c("^the ARD has no rows$", "the ARD has no rows"),
    c("^the column fmt_fn has cards' old name.*$",
      "the column fmt_fn has cards' old name (fmt_fun since cards 0.6.1)"),
    c("^the table's (cols|rows) name (.+), which is neither a group nor a variable of the ARD$",
      "the table's %s name %s, which is neither a group nor a variable of the ARD"),
    c("^the cells are written for (.+), which the ARD does not analyse$",
      "the cells are written for %s, which the ARD does not analyse"),
    c("^a template reads \\{(.+)\\}, a statistic the ARD does not have$",
      "a template reads {%s}, a statistic the ARD does not have"),
    # tflspec::tfl_check_ard_function(): an ARD function of one's own tried
    c("^it does not give (.+)$", "it does not give %s"),
    c("^it does not say which statistics it gives: .*$",
      "it does not say which statistics it gives: cards::as_cards_fn(<the function>, stat_names = c(...)) lets a check see them"))
  msg <- vapply(seq_len(nrow(p)), function(i) {
    m <- p$message[i]
    if (identical(p$check[i], "cards")) {
      return(paste0(t("cards' check of the ARD's shape: "), m))
    }
    for (pt in pats) {
      g <- regmatches(m, regexec(pt[1L], m))[[1L]]
      if (length(g)) return(do.call(sprintf, c(list(t(pt[2L])), as.list(g[-1L]))))
    }
    m
  }, "")
  p$message <- msg
  p
}

.check_summary <- function(p) {
  if (is.null(p) || !nrow(p)) return("ok")
  n <- table(factor(p$level, levels = c("error", "warning", "note")))
  paste(sprintf("%d %s", n[n > 0], names(n)[n > 0]), collapse = ", ")
}

# the study's table definition, as the programs read it
.study_table_spec <- function(study) {
  structure(study$planner$sheets, class = "tfl_table_spec")
}

#' Use an ARD taken in for a report, or stop using it
#'
#' Sets the report row's `ard_source`: `import:<file>` (the file in the
#' study's `input/ard/`, see [ard_imports()]) or blank (the report's own
#' ARD definition).  The report's program then reads that file.
#'
#' @param x A `tflplanner` (`study$planner`).
#' @param output_id The report.
#' @param file A `file` of [ard_imports()]; `NULL` to go back to the
#'   report's own ARD definition.
#' @return The `tflplanner`.
#' @export
use_imported_ard <- function(x, output_id, file = NULL) {
  r <- x$sheets$report
  if (!"ard_source" %in% names(r)) r$ard_source <- NA_character_
  i <- which(!is.na(r$output_id) & r$output_id == output_id)
  if (!length(i)) {
    r[nrow(r) + 1L, ] <- NA
    i <- nrow(r)
    r$output_id[i] <- output_id
  }
  r$ard_source[i] <- if (is.null(file)) NA_character_ else paste0("import:", file)
  x$sheets$report <- r
  x
}

#' Stop using an ARD taken in
#'
#' Marks it `"removed"` in the record ([ard_imports()]): the file stays in
#' `input/ard/` and on the record (what a report was made from is not
#' forgotten), and a report still pointing at it is named in an error when
#' its program runs.
#'
#' @param study An `rtfstudy`.
#' @param import_id The import.
#' @return The record, invisibly.
#' @export
remove_imported_ard <- function(study, import_id) {
  d <- ard_imports(study)
  i <- match(import_id, d$import_id)
  if (is.na(i)) stop("No import ", import_id, call. = FALSE)
  d$state[i] <- "removed"
  .write_imports(study, d)
  invisible(d)
}

# where a report's ARD comes from: NULL (its ARD definition) or the file in
# input/ard/
.ard_import_of <- function(x, output_id) {
  src <- .resolve_row(x, "report", output_id)$ard_source %||% NA
  if (is.na(src) || !startsWith(src, "import:")) return(NULL)
  sub("^import:", "", src)
}

#' One report's ARD, wherever it comes from
#'
#' The rows a report's table reads: from the ARD taken in when the report
#' row says `ard_source = import:<file>`, else from the study ARD the ARD
#' definition makes (`output/ard/ard.rds`).  The id columns are dropped,
#' as [tflspec::tfl_ard_for()] does.
#'
#' @param study An `rtfstudy`.
#' @param output_id The report.
#' @return The ARD, or `NULL` when there is none yet.
#' @export
study_ard <- function(study, output_id) {
  ids <- c("output_id", "analysis_id", "population_id")
  f <- .ard_import_of(study$planner, output_id)
  a <- if (is.null(f)) study_ard_rows(study, output_id) else {
    p <- file.path(.import_dir(study), f)
    if (!file.exists(p)) stop("The ARD taken in for ", output_id, ", ", f,
                              ", is not in input/ard/.", call. = FALSE)
    a <- tflspec::tfl_read_ard(p)
    if ("output_id" %in% names(a)) a[a$output_id %in% output_id, , drop = FALSE] else a
  }
  if (is.null(a)) return(NULL)
  a[setdiff(names(a), ids)]
}

#' Compare a report's own ARD with the one taken in
#'
#' Double programming: the report's rows of the study ARD (from its ARD
#' definition) against the ARD taken in for it, by
#' [cards::compare_ard()].
#'
#' @param study An `rtfstudy`.
#' @param output_id The report.
#' @param file The ARD taken in (a `file` of [ard_imports()]); `NULL`: the
#'   one the report row names.
#' @return The comparison ([cards::compare_ard()]); `cards::is_ard_equal()`
#'   says whether they agree.
#' @export
compare_imported_ard <- function(study, output_id, file = NULL) {
  if (!requireNamespace("cards", quietly = TRUE)) {
    stop("compare_imported_ard() needs the cards package.", call. = FALSE)
  }
  file <- file %||% .ard_import_of(study$planner, output_id)
  if (is.null(file)) stop(output_id, " names no ARD taken in.", call. = FALSE)
  own <- study_ard_rows(study, output_id)
  if (is.null(own) || !nrow(own)) {
    stop("The study ARD has no rows for ", output_id, ": make its ARD first.",
         call. = FALSE)
  }
  imp <- tflspec::tfl_read_ard(file.path(.import_dir(study), file))
  if ("output_id" %in% names(imp)) imp <- imp[imp$output_id %in% output_id, , drop = FALSE]
  ids <- c("output_id", "analysis_id", "population_id")
  cards::compare_ard(cards::as_card(own[setdiff(names(own), ids)], check = FALSE),
                     cards::as_card(imp[setdiff(names(imp), ids)], check = FALSE))
}
