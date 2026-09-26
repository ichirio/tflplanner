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
#     spec/                table_spec.xlsx, report_spec.xlsx
#     programs/            one program per report, autoexec_report.R
#     output/ard/          deliverable data: each table's ARD (.rds)
#     output/tfl/          deliverable reports: the RTF files
#     logs/                one log per program run
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
    spec = "spec", programs = "programs", ard = "output/ard",
    tfl = "output/tfl", logs = "logs")
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
  meta$rtfplanner <- as.character(utils::packageVersion("rtfplanner"))
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
  p$study[["program_dir"]] <- study_layout()[["programs"]]
  p
}

#' Create, open and list studies
#'
#' `create_study()` makes the study folder with its layout, `study.yml`, an
#' RStudio project and empty (or given) definition workbooks.
#' `open_study()` reads one.  `list_studies()` lists the studies in a
#' folder of studies.
#'
#' @param root The folder that holds the studies.
#' @param study_id The study's id, which is also its folder name.
#' @param title,compound,phase,description What the study is.
#' @param planner An `rtfplanner` to start from (e.g. [read_planner()] of an
#'   earlier study's workbooks); `NULL` starts empty.
#' @param path A study folder.
#' @return `create_study()` and `open_study()` return an `rtfstudy`:
#'   `path`, `meta` (the study.yml fields) and `planner`.
#'   `list_studies()` returns a data frame.
#' @examples
#' root <- tempfile()
#' s <- create_study(root, "ABC-101", title = "A phase 2 study")
#' list_studies(root)
#' @export
create_study <- function(root, study_id, title = NA, compound = NA,
                         phase = NA, description = NA, planner = NULL) {
  study_id <- .check_study_id(study_id)
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
  s <- structure(list(path = normalizePath(path, "/"), meta = .read_meta(path),
                      planner = planner %||% new_planner()),
                 class = "rtfstudy")
  save_study(s)
}

#' @rdname create_study
#' @export
open_study <- function(path) {
  meta <- .read_meta(path)
  sp <- file.path(path, study_layout()[["spec"]], c(.table_file, .report_file))
  sp <- sp[file.exists(sp)]
  p <- if (length(sp)) read_planner(sp) else new_planner()
  structure(list(path = normalizePath(path, "/"), meta = meta, planner = p),
            class = "rtfstudy")
}

#' @rdname create_study
#' @export
list_studies <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE)
  dirs <- dirs[file.exists(file.path(dirs, .study_file))]
  rows <- lapply(dirs, function(d) {
    m <- tryCatch(.read_meta(d), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    data.frame(study_id = m$study_id %||% basename(d),
               title = m$title, compound = m$compound, phase = m$phase,
               updated = as.character(m$updated %||% NA),
               path = normalizePath(d, "/"), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    study_id = character(), title = character(), compound = character(),
    phase = character(), updated = character(), path = character())),
    rows))
  rownames(out) <- NULL
  out
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
#' Writes the definition workbooks to `spec/` (with `output_path` and
#' `program_dir` set to the study's own folders), the report programs,
#' `autoexec_report.R`, and `study.yml`.  A program rtfplanner wrote and
#' nobody has touched since (its banner's checksum still matches) follows
#' the definition and is rewritten when it changes; one edited by hand is
#' kept unless it is named in `regenerate`.
#'
#' @param study An `rtfstudy`.
#' @param regenerate Report ids whose program is written anew.
#' @return The study, invisibly, with `files`: what was written or kept.
#' @export
save_study <- function(study, regenerate = character()) {
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
  for (i in seq_along(progs)) {
    f <- file.path(root, lay[["programs"]], progs[[i]])
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
  f <- file.path(root, lay[["programs"]], "autoexec_report.R")
  auto <- enc2utf8(autoexec_code(p))
  old <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8")
  if (!identical(.body_hash(old %||% ""), .body_hash(auto))) {
    writeLines(auto, f, useBytes = TRUE)
    files[nrow(files) + 1L, ] <- list(f, "written")
  } else {
    files[nrow(files) + 1L, ] <- list(f, "unchanged")
  }
  meta <- study$meta
  meta$updated <- NULL
  meta$rtfplanner <- NULL
  .write_meta(meta, root)
  study$meta <- .read_meta(root)
  study$files <- files
  invisible(study)
}

# ------------------------------------------------------------ the state

# A generated program carries a checksum of its own body in its banner.  A
# program whose body still matches it has not been touched since rtfplanner
# wrote it, so rtfplanner may write it again when the definition changes;
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
# current   untouched, and what rtfplanner would write now
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
    if (any(grepl("rtfplanner: the data part of", have, fixed = TRUE))) {
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
#' `generated` -- untouched since rtfplanner wrote it, `generated` meaning
#' the next save rewrites it; or `edited` by hand), whether its ARD and RTF exist
#' and when they were made, and a `status`:
#'
#' * `no program` -- save the study to write it
#' * `unsaved` -- the definition changed since the program was written;
#'   save the study
#' * `todo` -- the program's data part is still to be written
#' * `not run` -- no RTF yet
#' * `error` -- the last run failed (see its log)
#' * `outdated` -- the program or a definition workbook changed after the
#'   RTF was made
#' * `ok`
#'
#' @param study An `rtfstudy`.
#' @return A data frame.
#' @export
study_status <- function(study) {
  p <- study$planner
  root <- study$path
  lay <- study_layout()
  spec_time <- suppressWarnings(max(.mtime(file.path(root, lay[["spec"]],
                                                    .table_file)),
                                    .mtime(file.path(root, lay[["spec"]],
                                                     .report_file)),
                                    na.rm = TRUE))
  rows <- lapply(p$outputs$output_id, function(id) {
    info <- report_info(p, id)
    prog <- file.path(root, lay[["programs"]], info$program)
    rtf <- file.path(root, info$file)
    ard <- file.path(root, lay[["ard"]], paste0(id, ".rds"))
    log <- file.path(root, lay[["logs"]], sub("\\.[Rr]$", ".log",
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
            if (isTRUE(.mtime(prog) > t_rtf) || isTRUE(spec_time > t_rtf))
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

#' Run a study's report programs
#'
#' Runs `programs/autoexec_report.R` from the study folder, for every
#' report or the ones named, each program in its own `Rscript` process
#' with its log in `logs/`.
#'
#' @param study An `rtfstudy` (saved: the programs run from disk).
#' @param output_id Reports to run; `NULL` runs all.
#' @param wait `FALSE` returns the running [processx::process] at once.
#' @return With `wait = TRUE`, [study_status()] after the run; otherwise
#'   the process.
#' @export
run_study <- function(study, output_id = NULL, wait = TRUE) {
  p <- study$planner
  progs <- if (is.null(output_id)) character() else
    vapply(output_id, function(id) report_info(p, id)$program, "")
  lay <- study_layout()
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(lay[["programs"]], "autoexec_report.R"), progs),
    wd = study$path,
    stdout = file.path(study$path, lay[["logs"]], "autoexec_report.log"),
    stderr = "2>&1")
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
#' Reads `.rds`, `.csv`, `.xpt`, `.sas7bdat` (\pkg{haven}) and `.parquet`
#' (\pkg{arrow}).
#'
#' @param path A file.
#' @param n Rows to return.
#' @return A data frame, with the full dimensions in attribute `dim_full`.
#' @export
read_data_head <- function(path, n = 50L) {
  ext <- tolower(tools::file_ext(path))
  need <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Reading .", ext, " files needs the ", pkg, " package.",
           call. = FALSE)
    }
  }
  d <- switch(ext,
    rds = readRDS(path),
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
  d <- as.data.frame(utils::head(d, n))
  attr(d, "dim_full") <- full
  d
}
