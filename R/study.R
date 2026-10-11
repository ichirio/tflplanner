# A study is a folder.  `study.yml` marks it and says what it is; the rest is
# a fixed layout, so every study looks the same and the programs can name
# their files relative to the study folder -- a study can be moved, copied
# or zipped and still run.
#
#   <STUDY>/
#     study.yml            the study: id, title, compound, phase ...
#     <STUDY>.Rproj        opening it makes the study folder the working
#                          directory, which is where the programs run
#     data/adam/           input data: analysis datasets
#     data/sdtm/                       tabulation datasets
#     data/other/                      anything else (formats, lookups)
#     spec/                table_spec.xlsx, report_spec.xlsx (the report
#                          programs read them; ard_spec.xlsx only if exported)
#     programs/            study_setup.R (what every program runs first),
#                          batch.R, autoexec_all.R (official runs)
#     programs/ard/        one ARD program per output, ard_setup.R,
#                          autoexec_ard.R
#     programs/tfl/        one program per report, autoexec_report.R
#     output/ard/          the study's working ARD (ard.rds)
#     input/ard/           ARDs made elsewhere and taken in (import_ard()),
#                          with their record imports.csv; nothing else
#                          writes here
#     output/tfl/          the working reports: the RTF files
#     runs/                official runs: one batch folder each, with the
#                          logs, the results and the code
#     logs/preview/        what a report program printed when last run on
#                          its own (a preview; not a log of record)
#
# Every program runs with the study folder as its working directory:
# RStudio does that when the .Rproj is opened, autoexec_report.R does it
# for the programs it runs, and the app runs everything through it.

#' The folder layout of a study
#'
#' @return A named character vector of folders, relative to the study
#'   folder.
#' @export
study_layout <- function() {
  c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other",
    spec = "spec", programs_ard = "programs/ard",
    programs_tfl = "programs/tfl", ard = "output/ard", tfl = "output/tfl",
    ard_import = "input/ard", toc_import = "input/toc", runs = "runs", logs_preview = "logs/preview")
}

.study_file <- "study.yml"
.table_file <- "table_spec.xlsx"
.report_file <- "report_spec.xlsx"

.study_fields <- c("study_id", "title", "compound", "phase", "description")

.check_study_id <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", id)) {
    stop("A study id is letters, digits, '.', '_' or '-' (e.g. ABC-101).",
         call. = FALSE)
  }
  id
}

.write_meta <- function(meta, path) {
  meta$updated <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  meta$tflplanner <- as.character(utils::packageVersion("tflplanner"))
  meta <- lapply(meta, function(v) if (length(v) == 1L && is.na(v)) "" else v)
  yaml::write_yaml(meta, file.path(path, .study_file))
  invisible(meta)
}

.read_meta <- function(path) {
  f <- file.path(path, .study_file)
  if (!file.exists(f)) stop("Not a study folder (no study.yml): ", path,
                            call. = FALSE)
  m <- yaml::read_yaml(f)
  for (k in .study_fields) {
    v <- as.character(m[[k]] %||% NA_character_)
    m[[k]] <- if (length(v) != 1L || is.na(v) || !nzchar(v)) NA_character_ else
      v
  }
  m
}

.rproj <- c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No",
            "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
            "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8")

# The keys a study decides for its specs: where the RTF files go and where
# the programs are, both relative to the study folder.
.study_spec_keys <- function(p) {
  p$study[["output_path"]] <- study_layout()[["tfl"]]
  p$study[["program_dir"]] <- study_layout()[["programs_tfl"]]
  p
}

#' Create, open, register and list studies
#'
#' A study is known to tflplanner by its saved state in the home
#' ([setup_tflplanner()]); its folder holds the data, the programs and the
#' deliverables.
#'
#' * `create_study()` makes the study folder -- layout, `study.yml`, an
#'   RStudio project -- and saves the study, which registers it.
#' * `open_study()` returns a study as it was last saved, its definition
#'   files compared with what tflplanner last wrote or read: one changed
#'   outside tflplanner (edited in Excel, copied in) is read and taken in
#'   (`$spec` says what changed; see [reload_from_spec()]).  Given the
#'   folder of a study tflplanner does not know yet, it registers it first.
#' * `register_study()` adds an existing study folder: its `study.yml`, and
#'   its definition workbooks when `spec/` has them.  A folder unregistered
#'   before comes back as it was (its saved state, history and unsaved
#'   changes).
#' * `unregister_study()` takes a study off the list.  Its folder is not
#'   deleted: what tflplanner kept about it goes into the folder
#'   (`.tflplanner/`), for `register_study()` to take back.  tflplanner
#'   never deletes a study folder; to delete one, delete it yourself (in the
#'   file manager it goes to the recycle bin).
#' * `list_studies()` lists the registered studies.
#'
#' @param study_id The study's id, which is also its folder name.
#' @param title,compound,phase,description What the study is.
#' @param planner An `tflplanner` to start from (e.g. an earlier study's
#'   `open_study(...)$planner`); `NULL` starts from the company standards:
#'   their study-default rows, analysis sets and data catalog.
#' @param root The folder the new study folder goes in; defaults to the
#'   one set up with [setup_tflplanner()].
#' @param study A registered study's id, or a study folder.
#' @param path A study folder.
#' @param home tflplanner's home.
#' @return `create_study()`, `open_study()` and `register_study()` return
#'   an `rtfstudy`: `path`, `meta` (the study.yml fields) and `planner`.
#'   `list_studies()` returns a data frame.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it writes the study's spec workbooks)
#' s <- create_study("ABC-101", title = "A phase 2 study")
#' list_studies()
#' s <- open_study("ABC-101")
#' }
#' options(old)
#' @export
create_study <- function(study_id, title = NA, compound = NA, phase = NA,
                         description = NA, planner = NULL,
                         root = studies_root(home),
                         home = tflplanner_home()) {
  study_id <- .check_study_id(study_id)
  if (!is.null(.read_state(study_id, home))) {
    stop("Study '", study_id, "' is already registered.", call. = FALSE)
  }
  path <- file.path(root, study_id)
  if (file.exists(path)) {
    stop("A folder '", study_id, "' is already there: ", path, call. = FALSE)
  }
  for (d in study_layout()) {
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  }
  meta <- list(study_id = study_id, title = as.character(title),
               compound = as.character(compound), phase = as.character(phase),
               description = as.character(description),
               created = format(Sys.Date()))
  .write_meta(meta, path)
  writeLines(.rproj, file.path(path, paste0(study_id, ".Rproj")))
  s <- .new_study(path, .read_meta(path)[.study_fields],
                  planner %||% .standard_planner(study_id))
  save_study(s, home = home)
}

.new_study <- function(path, meta, planner) {
  structure(list(path = normalizePath(path, "/", mustWork = FALSE),
                 meta = meta, planner = .study_spec_keys(planner)),
            class = "rtfstudy")
}

.study_from_state <- function(st) {
  meta <- lapply(stats::setNames(.study_fields, .study_fields), function(k)
    as.character(st$meta[[k]] %||% NA_character_))
  .new_study(st$path, meta, .planner_from_state(st))
}

#' @rdname create_study
#' @export
open_study <- function(study, home = tflplanner_home()) {
  is_dir <- dir.exists(study) &&
    file.exists(file.path(study, .study_file))
  id <- if (is_dir) .read_meta(study)$study_id else study
  st <- .read_state(id, home)
  if (is.null(st)) {
    if (is_dir) return(register_study(study, home = home))
    stop("No study '", study, "': not registered and not a study folder.",
         call. = FALSE)
  }
  # a registered study whose folder has moved, opened from where it is now
  moved <- is_dir && !identical(st$path, normalizePath(study, "/"))
  if (moved) st$path <- normalizePath(study, "/")
  # the definition files, compared with what was last written or read: a
  # file changed outside tflplanner is read and taken in (#274); a state of
  # an earlier version (no record) reads them all once
  s <- .adopt_spec(st, home, full = is.null(st$files))
  if (moved && !identical(s$spec$status, "invalid")) .write_state(s, home)
  .spec_note(s)
  .set_config("last_study", id, home)
  # its programs as its setup has it (its folders' variables, its packages)
  .use_study(s$path)
  # the variables' headings of an earlier form, the table's labels now
  s$planner <- .move_heading_rows(s$planner)
  s
}

# What open_study() and register_study() say at the console when the
# definition files were changed outside tflplanner
.spec_note <- function(s) {
  sp <- s$spec
  if (identical(sp$status, "adopted")) {
    message("The definition files were changed outside tflplanner; the ",
            "changes were loaded: ", paste(sp$files$file, collapse = ", "), ".")
  } else if (identical(sp$status, "invalid")) {
    message("The definition files were changed outside tflplanner but do not ",
            "read; the study is open as last saved and cannot be saved until ",
            "they are fixed (reload_from_spec()) or written back (write_spec()):\n",
            paste0("  ", sp$problems$file, ": ", sp$problems$message,
                   collapse = "\n"))
  }
  invisible(s)
}

# What tflplanner keeps about a study (its home's studies/<id>/: the saved
# state, its history, an unsaved draft) goes into the study folder when the
# study is unregistered, and comes back when the folder is registered again:
# taking a study off the list loses nothing.
.kept_dir <- function(path) file.path(path, ".tflplanner")

.keep_store <- function(id, path, home = tflplanner_home()) {
  from <- .store_dir(id, home)
  if (!dir.exists(from) || !dir.exists(path)) return(invisible(FALSE))
  to <- .kept_dir(path)
  unlink(to, recursive = TRUE)
  dir.create(to, showWarnings = FALSE)
  # the ARD fetched for input assistance (ard/) is made again when needed
  for (f in setdiff(list.files(from), "ard")) {
    file.copy(file.path(from, f), to, recursive = TRUE, copy.date = TRUE)
  }
  invisible(TRUE)
}

# The kept store back in the home: its saved state, history and draft.
# TRUE: the state is back.
.restore_store <- function(id, path, home = tflplanner_home()) {
  from <- .kept_dir(path)
  sf <- file.path(from, "state.json")
  if (!file.exists(sf)) return(FALSE)
  st <- tryCatch(jsonlite::fromJSON(sf, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(st) || !identical(st$meta$study_id, id)) return(FALSE)
  to <- .store_dir(id, home)
  dir.create(file.path(to, "history"), recursive = TRUE, showWarnings = FALSE)
  for (f in list.files(file.path(from, "history"), full.names = TRUE)) {
    file.copy(f, file.path(to, "history"), copy.date = TRUE)
  }
  if (file.exists(file.path(from, "draft.json"))) {
    file.copy(file.path(from, "draft.json"), to, copy.date = TRUE)
  }
  # the kept state comes back as it was; register_study() then compares
  # the definition files with its record (#274): an edit made while the
  # study was off the list is read and taken in, the kept state going to
  # the history
  file.copy(sf, to, overwrite = TRUE, copy.date = TRUE)
  unlink(from, recursive = TRUE)
  TRUE
}

#' @rdname create_study
#' @export
register_study <- function(path, home = tflplanner_home()) {
  meta <- .read_meta(path)
  id <- .check_study_id(meta$study_id)
  st <- .read_state(id, home)
  if (!is.null(st) &&
      !identical(normalizePath(st$path, "/", FALSE),
                 normalizePath(path, "/"))) {
    stop("Study '", id, "' is already registered at ", st$path, ".",
         call. = FALSE)
  }
  # unregistered before: what tflplanner kept comes back as it was, the
  # definition files compared with its record (an edit made while the
  # study was off the list is taken in, #274)
  if (is.null(st)) {
    .restore_store(id, path, home)
    st <- .read_state(id, home)
    if (!is.null(st)) {
      st$path <- normalizePath(path, "/")
      s <- .adopt_spec(st, home, full = is.null(st$files))
      if (!identical(s$spec$status, "invalid")) .write_state(s, home)
      .spec_note(s)
      .set_config("last_study", id, home)
      return(s)
    }
  }
  # a folder new to tflplanner: its definition files are the study
  s <- .new_study(path, meta[.study_fields], new_planner())
  r <- .read_spec(s, path)
  problems <- attr(r, "problems")
  attr(r, "problems") <- NULL
  if (.has_errors(problems)) stop(.spec_invalid_condition(problems))
  # a figure design for no report of the study is not taken in
  fd <- names(r$planner$fig_designs %||% list())
  for (f in setdiff(fd, r$planner$outputs$output_id)) {
    r$planner <- set_fig_design(r$planner, f, NULL)
  }
  .write_state(r, home)
  r$spec <- list(status = "same", files = .spec_diff(NULL, list()))
  .set_config("last_study", id, home)
  r
}

#' @rdname create_study
#' @export
unregister_study <- function(study_id, home = tflplanner_home()) {
  if (length(study_id) != 1L || is.na(study_id) || !nzchar(study_id)) {
    stop("unregister_study() takes one study ID.", call. = FALSE)
  }
  study_id <- .check_study_id(study_id)
  st <- .read_state(study_id, home)
  if (is.null(st)) {
    stop("Study '", study_id, "' is not registered.", call. = FALSE)
  }
  # the folder is not touched but for what tflplanner kept, put in it
  # (.tflplanner/): register_study() takes it back
  .keep_store(study_id, st$path %||% "", home)
  unlink(.store_dir(study_id, home), recursive = TRUE)
  if (identical(tflplanner_config(home)$last_study, study_id)) {
    .set_config("last_study", NULL, home)
  }
  invisible(study_id)
}

#' @rdname create_study
#' @export
list_studies <- function(home = tflplanner_home()) {
  ids <- basename(list.dirs(file.path(home, "studies"), recursive = FALSE))
  rows <- lapply(ids, function(id) {
    st <- tryCatch(.read_state(id, home), error = function(e) NULL)
    if (is.null(st)) return(NULL)
    v <- function(k) as.character(st$meta[[k]] %||% NA_character_)
    n <- function(x) length(x$output_id %||% list())
    data.frame(study_id = id, title = v("title"), compound = v("compound"),
               phase = v("phase"), saved = as.character(st$saved %||% NA),
               path = st$path, folder = dir.exists(st$path),
               description = v("description"),
               reports = n(st$planner$outputs),
               analyses = n(st$planner$ard$analyses),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    study_id = character(), title = character(), compound = character(),
    phase = character(), saved = character(), path = character(),
    folder = logical(), description = character(), reports = integer(),
    analyses = integer())), rows))
  rownames(out) <- NULL
  out
}

#' Definition workbooks in and out
#'
#' The study's definition is its folder's `spec/` (the table and report
#' workbooks the programs read; the ARD definition as
#' `ard_definition.json`), written on every save and read back on open when
#' it changed ([reload_from_spec()]).
#' `export_spec()` writes the workbooks anywhere else -- with
#' `ard_spec.xlsx`, the ARD definition, to edit in Excel or keep;
#' `import_spec()` replaces the study's definition with what a set of
#' workbooks says (an `ard_spec.xlsx` among them replaces the ARD
#' definition; save the study to keep it).
#'
#' @param study An `rtfstudy`.
#' @param dir Destination folder.
#' @param path One or more `.xlsx` workbooks.
#' @return `export_spec()` the paths written; `import_spec()` the study.
#' @export
export_spec <- function(study, dir) {
  out <- write_planner(study$planner, dir)
  a <- study$planner$ard
  if (!is.null(a) && (nrow(a$analyses) || nrow(a$datasets))) {
    out <- c(out, .write_ard_spec(a, file.path(dir, .ard_file)))
  }
  invisible(out)
}

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
#' @export
export_ars <- function(study, dir, profile = c("cdisc", "siera")) {
  profile <- match.arg(profile)
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
    references = .fig_ars_references(p))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  base <- file.path(dir, paste0(id, "_ars", if (profile == "siera") "_siera"))
  json <- tflspec::tfl_write_ars_json(ars, paste0(base, ".json"))
  xlsx <- tflspec::tfl_write_ars_xlsx(ars, paste0(base, ".xlsx"),
                                      overwrite = TRUE)
  ck <- suppressMessages(tflspec::tfl_check_ars(ars))
  un <- tflspec::tfl_ars_unmapped(ars)
  rep <- rbind(
    data.frame(kind = rep("check", nrow(ck)), where = ck$part,
               item = ck$field, note = ck$problem, stringsAsFactors = FALSE),
    data.frame(kind = rep("not in ARS", nrow(un)), where = un$where,
               item = un$item, note = un$reason, stringsAsFactors = FALSE))
  csv <- file.path(dir, "ars_check.csv")
  utils::write.csv(rep, csv, row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  invisible(c(json = json, xlsx = xlsx, check = csv))
}

#' @rdname export_spec
#' @export
import_spec <- function(study, path) {
  # an ARD definition workbook (sheet `analyses`) replaces the ARD
  # definition; the others the table and report definition
  is_ard <- vapply(path, function(f) "analyses" %in% readxl::excel_sheets(f),
                   NA)
  if (any(!is_ard)) {
    ard <- study$planner$ard
    study$planner <- .study_spec_keys(read_planner(path[!is_ard]))
    study$planner$ard <- ard
  }
  if (any(is_ard)) {
    study$planner$ard <- unclass(.read_ard_spec(path[is_ard][1L], check = FALSE))
  }
  study
}

#' @export
print.rtfstudy <- function(x, ...) {
  cat("<rtfstudy> ", x$meta$study_id, sep = "")
  if (!is.na(x$meta$title)) cat(" --", x$meta$title)
  cat("\n  ", x$path, "\n  ", sep = "")
  st <- x$spec$status
  if (!is.null(st) && st %in% c("adopted", "invalid")) {
    cat(if (st == "adopted") "definition files changed outside tflplanner: loaded" else
          "definition files changed outside tflplanner: they do not read (reload_from_spec() / write_spec())",
        "\n  ", sep = "")
  }
  print(x$planner)
  invisible(x)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Save a study
#'
#' Saves the study's state in tflplanner's home (the copy
#' [open_study()] reads; the one before goes to its history), then writes
#' the study folder from it: the definition workbooks in `spec/` (with
#' `output_path` and `program_dir` set to the study's own folders), the
#' report programs, `autoexec_report.R`, `programs/study_setup.R` (made
#' when missing; otherwise its tflplanner part only, see
#' [study_setup_code()]), and `study.yml`.  The programs
#' are the definition's: each is written from it whenever it changes.  One
#' edited by hand since (its banner's checksum no longer matches) is
#' written again too, its edited copy first put in `programs/.edited/`
#' (named after its place and the time) -- what it changed belongs in the
#' definition (the data code, a user-code report, a custom analysis,
#' where / derive).
#'
#' @param study An `rtfstudy`.
#' @param home tflplanner's home.
#' @param base The study as it was opened (or last saved) by whoever saves
#'   now.  Given it, the save merges: a part (the study fields, the report
#'   list, each sheet, each sheet of the ARD definition ...) this person did
#'   not change keeps what is saved now -- someone else may have changed
#'   it -- and a part both changed differently is a conflict that stops the
#'   save (class `tflplanner_conflict`).
#'
#' The definition files are the study's source (#274): a save never writes
#' over one changed outside tflplanner since it last wrote or read them.
#' Such a save stops (class `tflplanner_spec_changed`, its `files` the
#' files changed): open the study again, or [reload_from_spec()], to take
#' the files in; or write them back from the last save ([write_spec()]).
#'
#' @param spec `"check"` (the default) stops when a definition file was
#'   changed outside tflplanner; `"overwrite"` writes over it, after copying
#'   it to `spec/.rejected/` (what [write_spec()] does).
#' @return The study, invisibly, with `files`: what was written (status
#'   `written`, `rewritten` for a program edited by hand, `unchanged`, ...).
#' @export
save_study <- function(study, home = tflplanner_home(), base = NULL,
                       spec = c("check", "overwrite")) {
  spec <- match.arg(spec)
  if (!is.null(base)) study <- .merge_saved(study, base, home)
  p <- .study_spec_keys(study$planner)
  study$planner <- p
  root <- study$path
  # its programs as its setup has it (its folders' variables, its packages)
  .use_study(root)
  lay <- study_layout()
  # the state now on disk (another session may have saved since) and its
  # record of the definition files
  st <- .read_state(study$meta$study_id, home)
  rec <- st$files
  if (!is.null(rec)) {
    diff <- .spec_diff(rec, .spec_fingerprints(root))
    if (any(diff$status != "same")) {
      if (spec == "check") stop(.spec_changed_condition(diff))
      .back_up_rejected(root, diff$file[diff$status %in% c("changed", "added")])
      # a file added outside tflplanner is not the last save's: it goes
      # (its copy is in spec/.rejected/)
      unlink(file.path(root, diff$file[diff$status == "added"]))
      rec <- rec[names(rec) %in% diff$file[diff$status == "same"]]
    }
  }
  # a file is what the last save wrote when its fingerprint is the one
  # recorded
  as_recorded <- function(rel) {
    f <- file.path(root, rel)
    !is.null(rec[[rel]]) && file.exists(f) &&
      identical(unname(tools::md5sum(f)), as.character(rec[[rel]]$md5))
  }
  was <- if (!is.null(st)) .study_spec_keys(.planner_from_state(st))
  progs <- vapply(p$outputs$output_id, function(id)
    report_info(p, id)$program, "")
  dup <- unique(progs[duplicated(progs)])
  if (length(dup)) {
    stop("Two reports share the program ", paste(dup, collapse = ", "),
         "; give each its own `program` on the report sheet.", call. = FALSE)
  }
  spec_dir <- file.path(root, lay[["spec"]])
  paths <- file.path(spec_dir, c(.table_file, .report_file))
  same <- if (!is.null(rec)) {
    # the workbooks are what the last save wrote: compared with its state,
    # not read (#274) -- each with its half of the definition
    c(table = as_recorded(file.path(lay[["spec"]], .table_file)) &&
        identical(.book_half(was, "table"), .book_half(p, "table")),
      report = as_recorded(file.path(lay[["spec"]], .report_file)) &&
        identical(.book_half(was, "report"), .book_half(p, "report")))
  } else {
    on_disk <- if (all(file.exists(paths)))
      tryCatch(read_planner(paths), error = function(e) NULL)
    one <- !is.null(on_disk) &&
      identical(on_disk[c("sheets", "study", "outputs", "setup")],
                unclass(p)[c("sheets", "study", "outputs", "setup")])
    c(table = one, report = one)
  }
  # rewriting an unchanged workbook would make every RTF look outdated (and
  # writing one takes a while): each is written only when its half changed
  if (!all(same)) write_planner(p, spec_dir, books = names(same)[!same])
  files <- data.frame(file = paths,
                      status = ifelse(same, "unchanged", "written"),
                      stringsAsFactors = FALSE)
  for (d in lay) dir.create(file.path(root, d), recursive = TRUE,
                            showWarnings = FALSE)
  .move_old_programs(root, progs)
  for (i in seq_along(progs)) {
    f <- file.path(root, lay[["programs_tfl"]], progs[[i]])
    id <- p$outputs$output_id[i]
    state <- .program_state(p, id, f)
    if (state %in% c("current", "todo")) {
      files[nrow(files) + 1L, ] <- list(f, "unchanged")
      next
    }
    if (state == "edited") .back_up_edited(f, root)
    .write_program(program_code(p, id), f)
    files[nrow(files) + 1L, ] <- list(f, if (state == "edited") "rewritten" else "written")
  }
  lf_rel <- file.path(lay[["spec"]], .lf_file)
  ard_files <- .save_ard(p, root)
  ard_problem <- attr(ard_files, "problem")
  if (!is.null(ard_problem)) {
    message("The ARD definition does not hold, so no ARD program was written ",
            "(the rest is saved):\n", ard_problem)
  }
  files <- rbind(files, ard_files,
                 .save_lf(p, root, was = if (as_recorded(lf_rel)) was$lf),
                 .save_fig_designs(p, root, own = function(f)
                   as_recorded(file.path(lay[["spec"]], .fig_design_dir,
                                         basename(f)))))
  files <- rbind(files, .save_study_setup(study$meta, root),
                 .save_study_helpers(root),
                 .save_batch_programs(p, root))
  meta <- study$meta[.study_fields]
  old_meta <- tryCatch(.read_meta(root), error = function(e) list())
  meta$created <- old_meta$created %||% format(Sys.Date())
  .write_meta(meta, root)
  study$meta <- .read_meta(root)[.study_fields]
  .write_state(study, home)
  .set_config("last_study", study$meta$study_id, home)
  study$files <- files
  # why no ARD program was written, or NULL (the app says it)
  study$ard_problem <- ard_problem
  invisible(study)
}

# A study of an earlier version kept its report programs in programs/ and
# the ARD in one programs/make_ard.R: the report programs move to
# programs/tfl/ (edits and all), the generated autoexec goes.
.move_old_programs <- function(root, progs) {
  lay <- study_layout()
  for (p in progs) {
    old <- file.path(root, "programs", p)
    new <- file.path(root, lay[["programs_tfl"]], p)
    if (file.exists(old) && !file.exists(new)) file.rename(old, new)
  }
  old <- file.path(root, "programs", "autoexec_report.R")
  if (file.exists(old)) file.remove(old)
  invisible()
}

# ------------------------------------------------------------ the state

# A generated program carries a checksum of its own body in its banner.  A
# program whose body still matches it has not been touched since tflplanner
# wrote it; one that does not match was edited by hand.  Either is written
# again from the definition (a program is never edited: design 13-00), the
# edited one first copied to programs/.edited/.
.gen_line <- "^#  (Generated|Checksum)  *:"

# (md5sum() reads a file: one file of the session's, written over each
# time -- a new file made and deleted for each program made a save slow)
.hash_file <- new.env()
.body_hash <- function(lines) {
  f <- .hash_file$path
  if (is.null(f)) f <- .hash_file$path <- tempfile("tflplanner-hash-")
  writeLines(enc2utf8(lines[!grepl(.gen_line, lines)]), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

# A program edited by hand, copied to programs/.edited/ before it is
# written again: named after its place under programs/ and the time
# (tfl_T-14-1-1_20261006-153000.R)
.edited_dir <- file.path("programs", ".edited")
.back_up_edited <- function(f, root) {
  dir <- file.path(root, .edited_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rel <- if (startsWith(f, root)) substring(f, nchar(root) + 2L) else basename(f)
  rel <- sub("^programs/", "", rel)
  dest <- file.path(dir, paste0(sub("[.][Rr]$", "", gsub("/", "_", rel, fixed = TRUE)),
                                "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".R"))
  file.copy(f, dest, overwrite = TRUE)
  invisible(dest)
}

.write_program <- function(code, f) {
  at <- grep("^#  Generated  :", code)[1L]
  code <- append(code, paste("#  Checksum   :", .body_hash(code)), at)
  writeLines(enc2utf8(code), f, useBytes = TRUE)
}

# missing   no file yet
# current   untouched, and what tflplanner would write now
# todo      untouched, still the TODO data part
# generated untouched, but the definition has moved on: saving rewrites it
# edited    changed by hand: saving writes it again (the edited one kept
#           in programs/.edited/)
# A report's program as the definition writes it, kept while the
# definition is the same: the runs table asks for every report's again at
# each refresh (study_status()), and the definition rarely changed between
.program_cache <- new.env()
.program_code_last <- function(p, id) {
  if (!identical(.program_cache$p, p) || !identical(.program_cache$date, Sys.Date()) ||
      !identical(.program_cache$opts, .code_context$opts)) {
    .program_cache$p <- p
    .program_cache$date <- Sys.Date()
    .program_cache$opts <- .code_context$opts
    .program_cache$code <- list()
  }
  if (is.null(.program_cache$code[[id]])) .program_cache$code[[id]] <- program_code(p, id)
  .program_cache$code[[id]]
}

# its checksum, kept with it
.program_hash_last <- function(p, id) {
  code <- .program_code_last(p, id)
  if (is.null(.program_cache$hash)) .program_cache$hash <- list()
  if (is.null(.program_cache$hash[[id]]) ||
      !identical(.program_cache$hash[[id]]$code, code)) {
    .program_cache$hash[[id]] <- list(code = code, hash = .body_hash(code))
  }
  .program_cache$hash[[id]]$hash
}

.program_state <- function(p, id, f) {
  if (!file.exists(f)) return("missing")
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  h <- .body_hash(have)
  if (!length(chk) || !identical(chk[1L], h)) return("edited")
  if (identical(h, .program_hash_last(p, id))) {
    if (any(grepl("tflplanner: the data part of", have, fixed = TRUE))) {
      return("todo")
    }
    return("current")
  }
  "generated"
}

.mtime <- function(f) {
  if (length(f) && file.exists(f)) file.mtime(f) else as.POSIXct(NA)
}

#' What each report of a study has produced
#'
#' One row per report: its program (`missing`; `current`, `todo` or
#' `generated` -- untouched since tflplanner wrote it, `generated` meaning
#' the next save rewrites it; or `edited` by hand), whether its ARD and RTF exist
#' and when they were made, and a `status`:
#'
#' * `no program` -- save the study to write it
#' * `unsaved` -- the definition changed since the program was written;
#'   save the study
#' * `todo` -- the program's data part is still to be written
#' * `not run` -- no RTF yet
#' * `error` -- the last run failed (see its log)
#' * `outdated` -- the report's program, or a file it sources (the figure
#'   setup, say), changed after the RTF was made; or the report was made
#'   with another `programs/study_setup.R` than the one there now (its
#'   program records it in `output/tfl/report_status.csv`); or -- a figure
#'   printing the numbers of an ARD (its own, or a table's) -- that ARD's
#'   definition is not the one it was made from, or that ARD is not made
#'   from its definition now.  The program holds the
#'   report's whole definition, so a change to the definition reaches the
#'   reports it is about and no others.
#' * `ok`
#'
#' `why` says what made a report `outdated`: `program` (its program or a
#' file it sources is newer), `setup` (the study setup changed), or
#' `ard:<id>` (the ARD of `<id>` changed, or is to be made again).
#'
#' @param study An `rtfstudy`.
#' @return A data frame.
#' @export
study_status <- function(study) {
  p <- study$planner
  root <- study$path
  .use_study(root)
  lay <- study_layout()
  setup <- .report_setup_recorded(root, p$outputs$output_id)
  now <- .study_setup_hash(root)
  # the figures printing an ARD's numbers: to be made again because of it
  ard_why <- .fig_ard_why(study, p$outputs$output_id)
  rows <- lapply(p$outputs$output_id, function(id) {
    info <- report_info(p, id)
    prog <- file.path(root, lay[["programs_tfl"]], info$program)
    rtf <- file.path(root, info$file)
    ard <- file.path(root, lay[["ard"]], paste0(id, ".rds"))
    log <- file.path(root, lay[["logs_preview"]], sub("\\.[Rr]$", ".log",
                                             info$program))
    pstate <- .program_state(p, id, prog)
    t_rtf <- .mtime(rtf)
    t_log <- .mtime(log)
    failed <- !is.na(t_log) && (is.na(t_rtf) || t_log > t_rtf) &&
      any(grepl("^Error|Execution halted",
                readLines(log, warn = FALSE, encoding = "UTF-8")))
    k <- match(id, p$outputs$output_id)
    why <- c(
      if (isTRUE(.program_time(prog, root) > t_rtf)) "program",
      if (.setup_changed(setup[k], root, now)) "setup",
      if (nzchar(ard_why[[k]])) ard_why[[k]])
    status <- if (pstate == "missing") "no program" else
      if (pstate == "generated") "unsaved" else
      if (pstate == "todo") "todo" else
        if (failed) "error" else
          if (is.na(t_rtf)) "not run" else
            if (length(why)) "outdated" else "ok"
    if (status != "outdated") why <- character()
    fmt <- function(t) if (is.na(t)) NA_character_ else
      format(t, "%Y-%m-%d %H:%M")
    data.frame(output_id = id, type = info$type, program = info$program,
               program_state = pstate,
               ard = if (file.exists(ard)) fmt(.mtime(ard)) else NA,
               rtf = fmt(t_rtf), status = status,
               why = paste(why, collapse = " "),
               log = if (file.exists(log)) log else NA_character_,
               rtf_path = rtf, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    output_id = character(), type = character(), program = character(),
    program_state = character(), ard = character(), rtf = character(),
    status = character(), why = character(), log = character(),
    rtf_path = character())),
    rows))
  rownames(out) <- NULL
  out
}

# When a report's program last changed: the program itself, or a file of
# the study it sources (programs/tfl/fig_setup.R, a setup of the user's).
# The definition workbooks are not in it: the program is written from them
# and holds all of the report's own definition, so saving a change to one
# report rewrites that report's program only.
.program_time <- function(prog, root) {
  t <- .mtime(prog)
  if (is.na(t)) return(t)
  txt <- readLines(prog, warn = FALSE, encoding = "UTF-8")
  src <- regmatches(txt, regexpr("^\\s*source\\(\"[^\"]+\"", txt))
  src <- unique(sub("^\\s*source\\(\"", "", sub("\"$", "", src)))
  if (!length(src)) return(t)
  ts <- do.call(c, lapply(file.path(root, src), .mtime))
  suppressWarnings(max(c(t, ts), na.rm = TRUE))
}

#' Preview a study's reports
#'
#' Runs report programs on their own (a preview), from the study folder --
#' every report or the ones named -- each in its own `Rscript` process.
#' They write the working RTFs (output/tfl/); what each printed is kept in
#' `logs/preview/` until the next preview, and no log of record is made.
#' The official run is [run_batch()].
#'
#' @param study An `rtfstudy` (saved: the programs run from disk).
#' @param output_id Reports to run; `NULL` runs all.
#' @param wait `FALSE` returns the running [processx::process] at once.
#' @return With `wait = TRUE`, [study_status()] after the run; otherwise
#'   the process.
#' @export
run_study <- function(study, output_id = NULL, wait = TRUE) {
  p <- study$planner
  ids <- output_id %||% p$outputs$output_id
  # a figure printing an ARD's numbers: that ARD first, when it is not made
  # from its definition now (as the official run does, #293)
  need <- unique(stats::na.omit(vapply(ids, function(id)
    tryCatch(.fig_ard_need(p, id), error = function(e) NA_character_), "")))
  if (length(need)) {
    st <- tryCatch(ard_status(study), error = function(e) NULL)
    for (tb in need) {
      if (!identical(st$state[match(tb, st$output_id)], "built")) update_study_ard(study, tb)
    }
  }
  progs <- vapply(ids, function(id) report_info(p, id)$program, "")
  lay <- study_layout()
  # a small runner: each program in its own Rscript, what it prints kept
  # in logs/preview/ (overwritten each time)
  f <- tempfile("preview", fileext = ".R")
  writeLines(c(
    paste0("progs <- c(", paste(encodeString(progs, quote = "\""),
                                collapse = ", "), ")"),
    paste0("dir.create(", encodeString(lay[["logs_preview"]], quote = "\""),
           ", recursive = TRUE, showWarnings = FALSE)"),
    "bad <- 0L",
    "for (p in progs) {",
    paste0("  log <- file.path(", encodeString(lay[["logs_preview"]], quote = "\""),
           ", sub(\"[.][Rr]$\", \".log\", p))"),
    paste0("  rc <- system2(file.path(R.home(\"bin\"), \"Rscript\"), shQuote(file.path(",
           encodeString(lay[["programs_tfl"]], quote = "\""), ", p)),"),
    "                stdout = log, stderr = log)",
    "  cat(if (identical(rc, 0L)) \"OK   \" else \"ERROR\", p, \"\\n\")",
    "  if (!identical(rc, 0L)) bad <- bad + 1L",
    "}",
    "quit(status = if (bad) 1L else 0L)"), f)
  px <- processx::process$new(file.path(R.home("bin"), "Rscript"), f,
                              wd = study$path, stdout = NULL,
                              stderr = NULL, cleanup = TRUE)
  if (!wait) return(px)
  px$wait()
  study_status(study)
}

#' Files in a study folder
#'
#' @param study An `rtfstudy`.
#' @param folder A folder of [study_layout()] (`"adam"`, `"tfl"`, ...), or
#'   `"data"` for all three data folders.
#' @return A data frame: `folder`, `file`, `size_kb`, `modified`, `path`.
#' @export
study_files <- function(study, folder = "data") {
  lay <- study_layout()
  dirs <- if (identical(folder, "data")) lay[c("adam", "sdtm", "other")] else
    lay[folder]
  rows <- lapply(dirs, function(d) {
    f <- list.files(file.path(study$path, d), full.names = TRUE,
                    recursive = TRUE)
    if (!length(f)) return(NULL)
    info <- file.info(f)
    data.frame(folder = d, file = substring(f, nchar(file.path(study$path,
                                                               d)) + 2L),
               size_kb = round(info$size / 1024, 1),
               modified = format(info$mtime, "%Y-%m-%d %H:%M"),
               path = f, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    folder = character(), file = character(), size_kb = numeric(),
    modified = character(), path = character())), rows))
  rownames(out) <- NULL
  out
}

#' The first rows of a data file
#'
#' Reads `.rds`, `.rda` / `.RData` (one dataset a file), `.csv`, `.xpt`,
#' `.sas7bdat` (\pkg{haven}) and `.parquet`
#' (\pkg{arrow}).
#'
#' @param path A file.
#' @param n Rows to return.
#' @return A data frame, with the full dimensions in attribute `dim_full`.
#' @export
read_data_head <- function(path, n = 50L) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("read_data_head(): `path` is one file.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  need <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Reading .", ext, " files needs the ", pkg, " package.",
           call. = FALSE)
    }
  }
  d <- switch(ext,
    rds = readRDS(path),
    rda = , rdata = local({
      e <- new.env()
      load(path, envir = e)
      e[[ls(e)[1L]]]
    }),
    csv = utils::read.csv(path, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8-BOM"),
    xpt = { need("haven"); haven::read_xpt(path) },
    sas7bdat = { need("haven"); haven::read_sas(path) },
    parquet = { need("arrow"); arrow::read_parquet(path) },
    stop("No preview for .", ext, " files.", call. = FALSE))
  if (!is.data.frame(d)) {
    d <- data.frame(class = paste(class(d), collapse = "/"),
                    str = paste(utils::capture.output(utils::str(d,
                      max.level = 1L)), collapse = "\n"))
  }
  full <- dim(d)
  # taking rows drops the columns' labels: keep them (the forms show them)
  lab <- lapply(d, attr, which = "label", exact = TRUE)
  d <- as.data.frame(utils::head(d, n))
  for (nm in names(lab)) if (!is.null(lab[[nm]])) attr(d[[nm]], "label") <- lab[[nm]]
  attr(d, "dim_full") <- full
  d
}

# The parts a study is saved in, each merged on its own.
.study_parts <- function(s) {
  p <- s$planner
  c(stats::setNames(lapply(.study_fields, function(k) s$meta[[k]]),
                    paste0("meta:", .study_fields)),
    list(`study keys` = p$study, setup = p$setup,
         `report list` = p$outputs),
    stats::setNames(p$sheets, paste0("sheet:", names(p$sheets))),
    stats::setNames(p$ard, paste0("ard:", names(p$ard))),
    stats::setNames(p$lf %||% .empty_lf(), paste0("lf:", .lf_sheet_names)),
    # each figure's design (spec/figures/<id>.yml)
    if (length(p$fig_designs))
      stats::setNames(p$fig_designs, paste0("fig:", names(p$fig_designs))))
}

.set_study_part <- function(s, part, value) {
  kind <- sub(":.*$", "", part)
  name <- sub("^[^:]*:", "", part)
  switch(kind,
    meta = s$meta[[name]] <- value,
    sheet = s$planner$sheets[[name]] <- value,
    ard = s$planner$ard[[name]] <- value,
    lf = s$planner$lf[[name]] <- value,
    fig = s$planner <- set_fig_design(s$planner, name, value),
    `study keys` = s$planner$study <- value,
    setup = s$planner$setup <- value,
    `report list` = s$planner$outputs <- value)
  s
}

.merge_saved <- function(study, base, home) {
  st <- .read_state(study$meta$study_id, home)
  if (is.null(st)) return(study)
  disk <- .study_from_state(st)
  base$planner <- .study_spec_keys(base$planner)
  study$planner <- .study_spec_keys(study$planner)
  mine <- .study_parts(study)
  was <- .study_parts(base)
  now <- .study_parts(disk)
  clash <- character()
  # a figure's design is a part when either side has it
  for (k in union(names(mine), union(names(was), names(now)))) {
    changed_here <- !identical(mine[[k]], was[[k]])
    changed_there <- !identical(now[[k]], was[[k]])
    if (!changed_here && changed_there) {
      study <- .set_study_part(study, k, now[[k]])
    } else if (changed_here && changed_there &&
               !identical(mine[[k]], now[[k]])) {
      clash <- c(clash, k)
    }
  }
  if (length(clash)) {
    cond <- structure(class = c("tflplanner_conflict", "error", "condition"),
      list(message = paste0(
        "Someone else saved changes to the same part(s) since you opened the study: ",
        paste(clash, collapse = ", "),
        ".  Open the study again to see them (your unsaved changes to those parts would be lost)."),
        call = NULL, parts = clash))
    stop(cond)
  }
  study
}
