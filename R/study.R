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
#     programs/            batch.R, autoexec_all.R (official runs)
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
#' * `open_study()` returns a study as it was last saved.  Given the folder
#'   of a study tflplanner does not know yet, it registers it first.
#' * `register_study()` adds an existing study folder: its `study.yml`, and
#'   its definition workbooks when `spec/` has them.
#' * `unregister_study()` forgets a study; its folder is left alone.
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
#' \dontrun{
#' s <- create_study("ABC-101", title = "A phase 2 study")
#' list_studies()
#' s <- open_study("ABC-101")
#' }
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
  s <- .study_from_state(st)
  # a registered study whose folder has moved, opened from where it is now
  if (is_dir && !identical(s$path, normalizePath(study, "/"))) {
    s$path <- normalizePath(study, "/")
    .write_state(s, home)
  }
  .set_config("last_study", id, home)
  s
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
  sp <- file.path(path, study_layout()[["spec"]], c(.table_file, .report_file))
  sp <- sp[file.exists(sp)]
  p <- if (length(sp)) read_planner(sp) else new_planner()
  # the ARD definition: the study folder's copy, else an exported workbook
  aj <- file.path(path, study_layout()[["spec"]], .ard_json)
  af <- file.path(path, study_layout()[["spec"]], .ard_file)
  if (file.exists(aj)) {
    p$ard <- .read_ard_json(aj)
  } else if (file.exists(af)) {
    p$ard <- unclass(.read_ard_spec(af, check = FALSE))
  }
  lf <- file.path(path, study_layout()[["spec"]], .lf_file)
  if (file.exists(lf)) {
    sheets <- readxl::excel_sheets(lf)
    if ("listings" %in% sheets) {
      ls <- tflspec::tfl_read_listing_spec(lf, check = FALSE)
      for (sh in names(ls)) p$lf[[sh]] <- ls[[sh]]
    }
    if ("figures" %in% sheets) {
      p$lf$figures <- .normalize_lf_sheet(.read_sheet_text(lf, "figures"),
                                          "figures")
    }
  }
  # the designed figures (spec/figures/<output_id>.yml, written on save):
  # read back, or the next save would take them away
  fd <- file.path(path, study_layout()[["spec"]], .fig_design_dir)
  for (f in list.files(fd, "\\.yml$", full.names = TRUE)) {
    id <- sub("\\.yml$", "", basename(f))
    d <- tryCatch(tflspec::tfl_read_fig_design(f), error = function(e) NULL)
    if (!is.null(d) && id %in% p$outputs$output_id) p <- set_fig_design(p, id, d)
  }
  s <- .new_study(path, meta[.study_fields], p)
  .write_state(s, home)
  .set_config("last_study", id, home)
  s
}

#' @rdname create_study
#' @export
unregister_study <- function(study_id, home = tflplanner_home()) {
  if (length(study_id) != 1L || is.na(study_id) || !nzchar(study_id)) {
    stop("unregister_study() takes one study ID.", call. = FALSE)
  }
  study_id <- .check_study_id(study_id)
  if (is.null(.read_state(study_id, home))) {
    stop("Study '", study_id, "' is not registered.", call. = FALSE)
  }
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
#' The study's definition lives in tflplanner's home and is written to the
#' study folder's `spec/` on every save (the table and report workbooks the
#' programs read; the ARD definition as `ard_definition.json`).
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
    profile = profile, study_id = id, dir = study$path)
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
#' report programs, `autoexec_report.R`, and `study.yml`.  A program
#' tflplanner wrote and nobody has touched since (its banner's checksum
#' still matches) follows the definition and is rewritten when it changes;
#' one edited by hand is kept unless it is named in `regenerate`.
#'
#' @param study An `rtfstudy`.
#' @param regenerate Report ids whose program is written anew.
#' @param home tflplanner's home.
#' @param base The study as it was opened (or last saved) by whoever saves
#'   now.  Given it, the save merges: a part (the study fields, the report
#'   list, each sheet, each sheet of the ARD definition ...) this person did
#'   not change keeps what is saved now -- someone else may have changed
#'   it -- and a part both changed differently is a conflict that stops the
#'   save (class `tflplanner_conflict`).
#' @return The study, invisibly, with `files`: what was written or kept.
#' @export
save_study <- function(study, regenerate = character(),
                       home = tflplanner_home(), base = NULL) {
  if (!is.null(base)) study <- .merge_saved(study, base, home)
  p <- .study_spec_keys(study$planner)
  study$planner <- p
  root <- study$path
  lay <- study_layout()
  progs <- vapply(p$outputs$output_id, function(id)
    report_info(p, id)$program, "")
  dup <- unique(progs[duplicated(progs)])
  if (length(dup)) {
    stop("Two reports share the program ", paste(dup, collapse = ", "),
         "; give each its own `program` on the report sheet.", call. = FALSE)
  }
  spec_dir <- file.path(root, lay[["spec"]])
  paths <- file.path(spec_dir, c(.table_file, .report_file))
  on_disk <- if (all(file.exists(paths)))
    tryCatch(read_planner(paths), error = function(e) NULL)
  same <- !is.null(on_disk) &&
    identical(on_disk[c("sheets", "study", "outputs", "setup")],
              unclass(p)[c("sheets", "study", "outputs", "setup")])
  # rewriting an unchanged workbook would make every RTF look outdated
  if (!same) write_planner(p, spec_dir)
  files <- data.frame(file = paths,
                      status = if (same) "unchanged" else "written",
                      stringsAsFactors = FALSE)
  for (d in lay) dir.create(file.path(root, d), recursive = TRUE,
                            showWarnings = FALSE)
  .move_old_programs(root, progs)
  for (i in seq_along(progs)) {
    f <- file.path(root, lay[["programs_tfl"]], progs[[i]])
    id <- p$outputs$output_id[i]
    state <- .program_state(p, id, f)
    if (state == "edited" && !id %in% regenerate) {
      files[nrow(files) + 1L, ] <- list(f, "kept")
      next
    }
    if (state %in% c("current", "todo")) {
      files[nrow(files) + 1L, ] <- list(f, "unchanged")
      next
    }
    .write_program(program_code(p, id), f)
    files[nrow(files) + 1L, ] <- list(f, "written")
  }
  files <- rbind(files, .save_ard(p, root), .save_lf(p, root),
                 .save_fig_designs(p, root))
  files <- rbind(files, .save_batch_programs(p, root))
  meta <- study$meta[.study_fields]
  old_meta <- tryCatch(.read_meta(root), error = function(e) list())
  meta$created <- old_meta$created %||% format(Sys.Date())
  .write_meta(meta, root)
  study$meta <- .read_meta(root)[.study_fields]
  .write_state(study, home)
  .set_config("last_study", study$meta$study_id, home)
  study$files <- files
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
# wrote it, so tflplanner may write it again when the definition changes;
# one that does not match was edited by hand and is left alone.
.gen_line <- "^#  (Generated|Checksum)  *:"

.body_hash <- function(lines) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(enc2utf8(lines[!grepl(.gen_line, lines)]), f, useBytes = TRUE)
  unname(tools::md5sum(f))
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
# edited    changed by hand: saving leaves it alone
.program_state <- function(p, id, f) {
  if (!file.exists(f)) return("missing")
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  if (!length(chk) || !identical(chk[1L], .body_hash(have))) return("edited")
  if (identical(.body_hash(have), .body_hash(program_code(p, id)))) {
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
#'   setup, say), changed after the RTF was made.  The program holds the
#'   report's whole definition, so a change to the definition reaches the
#'   reports it is about and no others.
#' * `ok`
#'
#' @param study An `rtfstudy`.
#' @return A data frame.
#' @export
study_status <- function(study) {
  p <- study$planner
  root <- study$path
  lay <- study_layout()
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
    status <- if (pstate == "missing") "no program" else
      if (pstate == "generated") "unsaved" else
      if (pstate == "todo") "todo" else
        if (failed) "error" else
          if (is.na(t_rtf)) "not run" else
            if (isTRUE(.program_time(prog, root) > t_rtf))
              "outdated" else "ok"
    fmt <- function(t) if (is.na(t)) NA_character_ else
      format(t, "%Y-%m-%d %H:%M")
    data.frame(output_id = id, type = info$type, program = info$program,
               program_state = pstate,
               ard = if (file.exists(ard)) fmt(.mtime(ard)) else NA,
               rtf = fmt(t_rtf), status = status,
               log = if (file.exists(log)) log else NA_character_,
               rtf_path = rtf, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    output_id = character(), type = character(), program = character(),
    program_state = character(), ard = character(), rtf = character(),
    status = character(), log = character(), rtf_path = character())),
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
    stats::setNames(p$lf %||% .empty_lf(), paste0("lf:", .lf_sheet_names)))
}

.set_study_part <- function(s, part, value) {
  kind <- sub(":.*$", "", part)
  name <- sub("^[^:]*:", "", part)
  switch(kind,
    meta = s$meta[[name]] <- value,
    sheet = s$planner$sheets[[name]] <- value,
    ard = s$planner$ard[[name]] <- value,
    lf = s$planner$lf[[name]] <- value,
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
  for (k in names(mine)) {
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
