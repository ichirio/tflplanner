# export_ars() and the ARS the study builds and writes.
# Split from R/study.R by section; no code changed.

#' Export the study's analyses as CDISC ARS
#'
#' Writes the study's ARD definition -- with its table and report
#' definitions: levels, titles, footnotes, files -- as a CDISC Analysis
#' Results Standard reporting event ([tflspec::tfl_ars()]): the ARS JSON
#' (the form to exchange), CDISC's Excel template of it (to read), and
#' `ars_check.csv`, what [tflspec::tfl_check_ars()] finds and what the ARS
#' does not say ([tflspec::tfl_ars_unmapped()]).  An analysis needs its
#' `purpose` (a column of the ARD definition's analyses) for the ARS to be
#' complete; a blank one is listed.
#'
#' @param study An `rtfstudy`.
#' @param dir Destination folder.
#' @param profile `"cdisc"`, or `"siera"` for a reporting event siera can
#'   run (see [tflspec::tfl_ars()]).
#' @return The paths written, invisibly.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it writes the ARS files)
#' s <- create_sample_study(run = FALSE)
#' # the ARS files go to a folder in the temporary folder
#' files <- export_ars(s, file.path(tempdir(), "ars"))
#' basename(files)
#' }
#' options(old)
#' @export
export_ars <- function(study, dir, profile = c("cdisc", "siera")) {
  profile <- match.arg(profile)
  b <- .ars_build(study, profile)
  .ars_write(b, dir)
}

# The study's ARS and what is wrong with it or left out of it, one table:
# kind ("check" / "not in ARS"), output_id (the report a row is about, NA
# for a population or the study), where, item, note.  The ARS export
# dialog shows it; export_ars() writes it as ars_check.csv.
.ars_build <- function(study, profile = "cdisc") {
  a <- study$planner$ard
  if (is.null(a) || !nrow(a$analyses)) {
    stop("The study has no analyses in its ARD definition.", call. = FALSE)
  }
  p <- study$planner
  id <- study$meta$study_id
  ars <- tflspec::tfl_ars(
    .ard_spec(a), table_spec = .spec_object(p, table_sheets(),
                                            .study_keys$table),
    report_spec = .spec_object(p, report_sheets(), .study_keys$report),
    profile = profile, study_id = id, dir = study$path,
    # a figure printing a table's analyses is an output that names them
    references = .fig_ars_references(p),
    # siera: what each dataset has, for the groupings on a derived column
    columns = if (profile == "siera") .ars_columns(study, a))
  ck <- suppressMessages(tflspec::tfl_check_ars(ars))
  un <- tflspec::tfl_ars_unmapped(ars)
  ids <- p$outputs$output_id
  # the outputs siera cannot run (fewer than three analyses, an analysis
  # without a grouping, a grouping on a derived column): left out of the
  # siera file, each with why -- a limit of siera, not a fault of the
  # definition, so not an error
  sie <- un$item == "siera"
  problems <- rbind(
    data.frame(kind = rep("check", nrow(ck)),
               output_id = .ars_output_of(ck$part, ids), where = ck$part,
               item = ck$field, note = ck$problem, stringsAsFactors = FALSE),
    data.frame(kind = ifelse(sie, "siera", "not in ARS"),
               output_id = .ars_output_of(un$where, ids), where = un$where,
               item = un$item, note = un$reason, stringsAsFactors = FALSE))
  list(ars = ars, problems = problems, profile = profile, study_id = id)
}

# Each dataset's columns, for tfl_ars(columns = ): the files of the data
# catalog that are there (a file not there: its dataset is not checked)
.ars_columns <- function(study, a) {
  ds <- a$datasets
  out <- list()
  for (i in seq_len(nrow(ds))) {
    f <- .data_file(study$path, ds$path[i])
    if (is.na(f) || !file.exists(f)) next
    ext <- tolower(tools::file_ext(f))
    d <- tryCatch(switch(ext,
      rds = readRDS(f),
      xpt = haven::read_xpt(f, n_max = 0L),
      sas7bdat = haven::read_sas(f, n_max = 0L),
      csv = utils::read.csv(f, nrows = 1L, check.names = FALSE),
      NULL), error = function(e) NULL)
    if (is.data.frame(d)) out[[ds$dataset[i]]] <- names(d)
  }
  out
}

# The report a check or an unmapped row is about: an analysis's id is
# An_<output>_<analysis>, an unmapped place "<output> / <analysis>" or the
# output itself; the longest output id that fits (one id may begin another).
.ars_output_of <- function(x, ids) {
  ids <- ids[order(-nchar(ids))]
  # the ARS's ids have letters, digits and _ only (T-14-1-2 is T_14_1_2)
  safe <- gsub("[^A-Za-z0-9]", "_", ids)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    for (k in seq_along(ids)) {
      o <- ids[k]
      if (identical(s, o) || startsWith(s, paste0(o, " /")) ||
          grepl(paste0("An_", o, "_"), s, fixed = TRUE) ||
          identical(s, paste("Output", safe[k])) ||
          grepl(paste0("An_", safe[k], "_"), s, fixed = TRUE)) return(o)
    }
    NA_character_
  }, character(1L), USE.NAMES = FALSE)
}

# Write a built ARS: the JSON, CDISC's Excel template of it, and the
# problems as ars_check.csv.  `parts` chooses what is written.
.ars_write <- function(b, dir, parts = c("json", "xlsx", "check")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  base <- file.path(dir, paste0(b$study_id, "_ars",
                                if (b$profile == "siera") "_siera"))
  out <- character()
  if ("json" %in% parts) {
    out[["json"]] <- tflspec::tfl_write_ars_json(b$ars, paste0(base, ".json"))
  }
  if ("xlsx" %in% parts) {
    out[["xlsx"]] <- tflspec::tfl_write_ars_xlsx(b$ars, paste0(base, ".xlsx"),
                                                 overwrite = TRUE)
  }
  if ("check" %in% parts) {
    csv <- file.path(dir, "ars_check.csv")
    utils::write.csv(b$problems[c("kind", "where", "item", "note")], csv,
                     row.names = FALSE, na = "", fileEncoding = "UTF-8")
    out[["check"]] <- csv
  }
  invisible(out)
}
