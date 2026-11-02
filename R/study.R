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
#     docs/                the study's documents for reference (SAP, the
#                          TOC / SHELL workbook); nothing writes here
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
#' @examples
#' study_layout()["adam"]
#' names(study_layout())
#' @export
study_layout <- function() {
  c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other",
    spec = "spec", programs_ard = "programs/ard",
    programs_tfl = "programs/tfl", ard = "output/ard", tfl = "output/tfl",
    ard_import = "input/ard", toc_import = "input/toc", runs = "runs", logs_preview = "logs/preview",
    docs = "docs")
}

.study_file <- "study.yml"
.table_file <- "table_spec.xlsx"
.report_file <- "report_spec.xlsx"

.study_fields <- c("study_id", "title", "short_title", "compound", "phase",
                   "description")

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
  f <- file.path(path, .study_file)
  # study.yml holds the YAML spec's study part too (its settings, keys,
  # setup code -- an !r kept as one): the fields set, the rest as it is
  old <- if (file.exists(f)) yaml::read_yaml(f, handlers = list(
    r = function(x) structure(x, tag = "!r"),
    `bool#yes` = function(x) x, `bool#no` = function(x) x))
  out <- old %||% list()
  for (k in names(meta)) out[[k]] <- meta[[k]]
  writeLines(yaml::as.yaml(out, handlers = list(logical = yaml::verbatim_logical)), f,
             useBytes = TRUE)
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
#'   changes).  A folder with no RStudio project (`.Rproj`) gets one.
#' * `unregister_study()` takes a study off the list.  Its folder is not
#'   deleted: what tflplanner kept about it goes into the folder
#'   (`.tflplanner/`), for `register_study()` to take back.  tflplanner
#'   never deletes a study folder; to delete one, delete it yourself (in the
#'   file manager it goes to the recycle bin).
#' * `list_studies()` lists the registered studies.
#'
#' @param study_id The study's id, which is also its folder name.
#' @param title,short_title,compound,phase,description What the study is
#'   (`short_title`: the title for a header or footer, `{STUDY_SHORT_TITLE}`).
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
                         description = NA, short_title = NA, planner = NULL,
                         root = studies_root(home),
                         home = tflplanner_home()) {
  study_id <- .check_study_id(study_id)
  if (!is.null(.read_state(study_id, home))) {
    stop("Study '", study_id, "' is already registered.", call. = FALSE)
  }
  path <- .study_path(root, study_id, home)
  if (file.exists(path)) {
    stop("A folder '", study_id, "' is already there: ", path, call. = FALSE)
  }
  .make_dir(path)
  for (d in study_layout()) {
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  }
  meta <- list(study_id = study_id, title = as.character(title),
               short_title = as.character(short_title),
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
