# The SPEC is the source (#274): the study folder's definition files --
# spec/ and study.yml -- are what the study is.  The saved state in the home
# is a copy of them with a fingerprint of each file (md5 and size), taken
# when tflplanner last wrote or read them.  Opening a study compares the
# fingerprints with the files: the same, the copy is opened (fast); changed
# outside tflplanner, the changed files are read and, when they read
# without problems, taken in (the copy follows); when they do not, the
# study opens as last saved and cannot be saved until the files are fixed
# (reload_from_spec()) or written back (write_spec()).  A save never writes
# over a file changed outside tflplanner.

# ---------------------------------------------------------- fingerprints

# The definition files tflplanner knows, relative to the study folder (as
# they are now; the record may name others, gone since)
.spec_files <- function(root) {
  sp <- study_layout()[["spec"]]
  f <- c(.study_file, file.path(sp, c(.table_file, .report_file)))
  # the ARD definition: its JSON; an exported workbook only when there is
  # no JSON (then it is the source, as register_study() reads it)
  f <- c(f, if (file.exists(file.path(root, sp, .ard_json)))
    file.path(sp, .ard_json) else file.path(sp, .ard_file))
  f <- c(f, file.path(sp, .lf_file))
  fd <- file.path(root, sp, .fig_design_dir)
  f <- c(f, file.path(sp, .fig_design_dir,
                      sort(list.files(fd, "\\.yml$"))))
  f[file.exists(file.path(root, f))]
}

# Each known file's md5 and size, by its path relative to the study folder
.spec_fingerprints <- function(root) {
  if (is.null(root) || !length(root) || is.na(root) || !dir.exists(root)) {
    return(list())
  }
  f <- .spec_files(root)
  full <- file.path(root, f)
  md5 <- unname(tools::md5sum(full))
  size <- file.size(full)
  stats::setNames(lapply(seq_along(f), function(i)
    list(md5 = md5[[i]], size = size[[i]])), f)
}

# The record against the files now: one row a file, its status same /
# changed / added / removed
.spec_diff <- function(rec, now) {
  rec <- rec %||% list()
  files <- union(names(rec), names(now))
  status <- vapply(files, function(f) {
    if (is.null(rec[[f]])) "added" else if (is.null(now[[f]])) "removed" else
      if (identical(as.character(rec[[f]]$md5), as.character(now[[f]]$md5)))
        "same" else "changed"
  }, "", USE.NAMES = FALSE)
  size <- function(x, f) {
    v <- x[[f]]$size
    if (is.null(v)) NA_real_ else as.numeric(v)
  }
  data.frame(file = files, status = status,
             size_was = vapply(files, function(f) size(rec, f), 0, USE.NAMES = FALSE),
             size_now = vapply(files, function(f) size(now, f), 0, USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# Which part of the study a file holds: books (the two workbooks, read
# together), ard, lf, meta, or fig:<output_id>
.spec_part_of <- function(file) {
  b <- basename(file)
  ifelse(b == .study_file, "meta",
    ifelse(b %in% c(.table_file, .report_file), "books",
      ifelse(b %in% c(.ard_json, .ard_file), "ard",
        ifelse(b == .lf_file, "lf",
          paste0("fig:", sub("\\.yml$", "", b))))))
}

# ---------------------------------------------------------------- reading

# A study's definition read from its files: the parts named (books, ard,
# lf, meta, fig:<id>; "fig" all the designs), over `s` (a study; its
# planner and meta are what a part not read keeps).  Problems -- an error
# or a warning of a reader -- are collected, not thrown: the result has
# `problems` (file, sheet, row, message), and the parts with one are left
# as `s` had them.
.read_spec <- function(s, root, parts = c("books", "ard", "lf", "meta", "fig"),
                       books_needed = FALSE) {
  sp <- file.path(root, study_layout()[["spec"]])
  problems <- .spec_problems()
  p <- s$planner
  meta <- s$meta
  read <- function(file, expr) {
    errs <- character()
    warns <- character()
    out <- withCallingHandlers(
      tryCatch(expr, error = function(e) {
        errs <<- c(errs, conditionMessage(e))
        NULL
      }),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    # files read together (the two workbooks): the one a message names
    which_file <- function(msgs) vapply(msgs, function(m) {
      k <- file[vapply(basename(file), grepl, NA, x = m, fixed = TRUE)]
      if (length(k)) return(k[1L])
      if (length(file) > 1L) {
        # the workbook the sheet a message names is in
        sh <- gsub("`", "", regmatches(m, regexpr("`[^`]+`", m)))
        sh <- if (length(sh)) sh else ""
        if (sh %in% report_sheets()) return(file[basename(file) == .report_file][1L])
        if (sh %in% table_sheets()) return(file[basename(file) == .table_file][1L])
      }
      paste(file, collapse = ", ")
    }, "", USE.NAMES = FALSE)
    if (length(warns)) problems <<- rbind(
      problems, .spec_problems(which_file(warns), warns, "warning"))
    if (length(errs)) {
      problems <<- rbind(problems, .spec_problems(which_file(errs), errs))
      return(NULL)
    }
    out
  }
  if ("books" %in% parts) {
    paths <- file.path(sp, c(.table_file, .report_file))
    rel <- file.path(study_layout()[["spec"]], c(.table_file, .report_file))
    if (!any(file.exists(paths)) && !books_needed) {
      # a study folder with no workbooks yet: its tables and reports are
      # empty (a new folder), what `s` has
    } else if (!all(file.exists(paths))) {
      # the two workbooks are the study's tables and reports: one missing
      # (or both, once written) is never meant (a deleted figure design is)
      for (f in rel[!file.exists(paths)]) {
        problems <- rbind(problems, .spec_problems(f, "The file is missing."))
      }
    } else {
      b <- read(rel, .study_spec_keys(read_planner(paths)))
      if (!is.null(b)) p[c("sheets", "study", "outputs", "setup")] <-
        b[c("sheets", "study", "outputs", "setup")]
    }
  }
  if ("ard" %in% parts) {
    aj <- file.path(sp, .ard_json)
    af <- file.path(sp, .ard_file)
    if (file.exists(aj)) {
      a <- read(file.path(study_layout()[["spec"]], .ard_json), {
        a <- .read_ard_json(aj)
        .ard_spec(a)
        a
      })
      if (!is.null(a)) p$ard <- a
    } else if (file.exists(af)) {
      a <- read(file.path(study_layout()[["spec"]], .ard_file),
                unclass(.read_ard_spec(af, check = TRUE)))
      if (!is.null(a)) p$ard <- a
      # where its cells are, for the checks after
      attr(s, "ard_file") <- .ard_file
    } else {
      p$ard <- .empty_ard_spec()
    }
  }
  if ("lf" %in% parts) {
    lf <- file.path(sp, .lf_file)
    if (file.exists(lf)) {
      x <- read(file.path(study_layout()[["spec"]], .lf_file), {
        out <- .empty_lf()
        sheets <- readxl::excel_sheets(lf)
        if ("listings" %in% sheets) {
          ls <- tflspec::tfl_read_listing_spec(lf, check = TRUE)
          for (sh in names(ls)) out[[sh]] <- .normalize_lf_sheet(ls[[sh]], sh)
        }
        if ("figures" %in% sheets) {
          out$figures <- .normalize_lf_sheet(.read_sheet_text(lf, "figures"),
                                             "figures")
        }
        out
      })
      if (!is.null(x)) p$lf <- x
    } else {
      p$lf <- .empty_lf()
    }
  }
  if ("meta" %in% parts) {
    m <- read(.study_file, .read_meta(root))
    if (!is.null(m)) {
      if (!identical(m$study_id, s$meta$study_id)) {
        problems <- rbind(problems, .spec_problems(
          .study_file, sprintf("The study ID is %s, not %s.", m$study_id,
                               s$meta$study_id)))
      } else {
        meta <- m[.study_fields]
      }
    }
  }
  figs <- if ("fig" %in% parts) {
    union(names(p$fig_designs %||% list()),
          sub("\\.yml$", "", list.files(file.path(sp, .fig_design_dir),
                                        "\\.yml$")))
  } else sub("^fig:", "", grep("^fig:", parts, value = TRUE))
  for (id in figs) {
    f <- file.path(sp, .fig_design_dir, paste0(id, ".yml"))
    rel <- file.path(study_layout()[["spec"]], .fig_design_dir,
                     paste0(id, ".yml"))
    if (!file.exists(f)) {
      p <- set_fig_design(p, id, NULL)
      next
    }
    d <- read(rel, tflspec::tfl_read_fig_design(f))
    if (!is.null(d)) p <- set_fig_design(p, id, d)
  }
  s$planner <- p
  s$meta <- meta
  attr(s, "problems") <- problems
  s
}

# Problems as a table: file, sheet, row (from the message when it names
# them: "`cells` row 12: ...", "Sheet `listings` ..."), column, severity
# (an error stops the files being taken in; a warning does not), message
.spec_problems <- function(file = character(), message = character(),
                           severity = "error") {
  file <- rep_len(as.character(file), length(message))
  # (a message may run over lines: the first match, not a whole-line sub)
  first <- function(re, strip) vapply(message, function(m) {
    hit <- regmatches(m, regexpr(re, m))
    if (length(hit)) gsub(strip, "", hit) else NA_character_
  }, "", USE.NAMES = FALSE)
  sheet <- first("`[^`]+`", "`")
  row <- first("`[^`]+` row [0-9]+", "^.* row ")
  data.frame(file = file, sheet = sheet, row = row,
             column = rep(NA_character_, length(message)),
             severity = rep_len(severity, length(message)),
             message = message, stringsAsFactors = FALSE)
}

.has_errors <- function(problems) {
  !is.null(problems) && any(problems$severity %in% "error")
}

# ----------------------------------------------------------- the decision

# The parts of two studies that differ (the parts .study_parts() names)
.changed_parts <- function(a, b) {
  pa <- .study_parts(a)
  pb <- .study_parts(b)
  k <- union(names(pa), names(pb))
  k[!vapply(k, function(n) identical(pa[[n]], pb[[n]]), NA)]
}

# The reports a change touches: those whose rows of the changed sheets,
# report row or figure design differ
.changed_outputs <- function(a, b, parts) {
  ids <- character()
  rows <- function(d, id) if (is.null(d) || !"output_id" %in% names(d))
    NULL else d[d$output_id %in% id, , drop = FALSE]
  for (k in parts) {
    if (startsWith(k, "fig:")) {
      ids <- c(ids, sub("^fig:", "", k))
      next
    }
    da <- .study_parts(a)[[k]]
    db <- .study_parts(b)[[k]]
    if (!is.data.frame(da) && !is.data.frame(db)) next
    all_ids <- unique(c(da$output_id, db$output_id))
    all_ids <- all_ids[!is.na(all_ids)]
    for (id in all_ids) {
      ra <- rows(da, id)
      rb <- rows(db, id)
      rownames(ra) <- NULL
      rownames(rb) <- NULL
      if (!identical(ra, rb)) ids <- c(ids, id)
    }
  }
  unique(ids)
}

# A study opened from its state `st`, the files compared with the record:
# the result has `spec` (status same / touched / adopted / invalid, files,
# parts, outputs, problems).  `full`: read every part (the first open of a
# state with no record, a reload) whatever the fingerprints say.
.adopt_spec <- function(st, home, full = FALSE) {
  s <- .study_from_state(st)
  now <- .spec_fingerprints(s$path)
  diff <- .spec_diff(st$files, now)
  changed <- diff$file[diff$status != "same"]
  if (!full && !length(changed)) {
    s$spec <- list(status = "same", files = diff)
    return(s)
  }
  parts <- if (full) c("books", "ard", "lf", "meta", "fig") else
    unique(.spec_part_of(changed))
  books <- file.path(study_layout()[["spec"]], c(.table_file, .report_file))
  r <- .read_spec(s, s$path, parts,
                  books_needed = any(names(st$files %||% list()) %in% books))
  problems <- attr(r, "problems")
  attr(r, "problems") <- NULL
  if (.has_errors(problems)) {
    s$spec <- list(status = "invalid", files = diff, problems = problems)
    return(s)
  }
  chg <- .changed_parts(s, r)
  # what changed, checked as an import is: its R cells, its figure
  # designs, the programs of the reports it touches written and parsed
  if (length(chg)) {
    problems <- rbind(problems, .check_spec(r, .changed_outputs(s, r, chg)))
    if (.has_errors(problems)) {
      s$spec <- list(status = "invalid", files = diff, problems = problems)
      return(s)
    }
  }
  if (!length(chg)) {
    # opened and saved again, copied with other line ends: the same
    # definition, a new fingerprint (no history entry, no notice)
    .write_state(s, home)
    # (a state of an earlier version had no record: nothing to say)
    s$spec <- list(status = if (length(changed) && !is.null(st$files))
      "touched" else "same", files = diff)
    return(s)
  }
  out <- .changed_outputs(s, r, chg)
  .write_state(r, home)
  r$spec <- list(status = "adopted", files = diff[diff$status != "same", ,
                                                  drop = FALSE],
                 parts = chg, outputs = out,
                 problems = if (nrow(problems)) problems,
                 # the study before: what a draft or unsaved changes were
                 # made against (merged with .merge_parts())
                 was = list(planner = s$planner, meta = s$meta))
  r
}

# Three-way merge by part (#274 2.6): `mine` (a draft, a session's unsaved
# changes) and `now` (the definition files taken in) were both made from
# `base`.  A part only one side changed takes that side's value; a part
# both changed differently is a conflict, left as `now` has it.  Returns
# list(study = merged (now's path), mine_parts, conflicts).
.merge_parts <- function(base, mine, now) {
  b <- .study_parts(base)
  m <- .study_parts(mine)
  n <- .study_parts(now)
  out <- now
  conflicts <- character()
  for (k in union(names(m), union(names(b), names(n)))) {
    here <- !identical(m[[k]], b[[k]])
    there <- !identical(n[[k]], b[[k]])
    if (here && !there) {
      out <- .set_study_part(out, k, m[[k]])
    } else if (here && there && !identical(m[[k]], n[[k]])) {
      conflicts <- c(conflicts, k)
    }
  }
  list(study = out, conflicts = conflicts)
}

# The parts named for people: "titles (sheet)", "report list", "figure F-1"
.part_labels <- function(parts) {
  kind <- sub(":.*$", "", parts)
  name <- sub("^[^:]*:", "", parts)
  ifelse(kind == "sheet", paste0(name, " (sheet)"),
    ifelse(kind == "ard", paste0(name, " (ARD)"),
      ifelse(kind == "lf", paste0(name, " (listings and figures)"),
        ifelse(kind == "fig", paste0("figure ", name),
          ifelse(kind == "meta", name, parts)))))
}

# ---------------------------------------------------------------- the API

#' The study's definition files: read them, compare them, write them back
#'
#' A study's definition is its folder's files: `spec/` (the table and
#' report workbooks, the ARD definition, the listing and figure workbook,
#' the figure designs) and `study.yml`.  tflplanner keeps a copy of them in
#' its home, with a fingerprint of each file; [open_study()] reads the
#' files that changed since, so an edit made directly in them (in Excel, by
#' a script) is what the study opens with.  A file that does not read (a
#' sheet tflspec does not know, a broken row) stops nothing: the study
#' opens as last saved, and cannot be saved until one of these is done.
#'
#' * `reload_from_spec()` reads every definition file and takes them in
#'   (after fixing them, or to take in workbooks copied into `spec/` by
#'   hand).  It fails with class `tflplanner_spec_invalid` (its `problems`
#'   the file, sheet, row and message of each) when they do not read.
#' * `write_spec()` writes the files again from the study as last saved;
#'   the files it replaces are copied to `spec/.rejected/` first.
#' * `spec_status()` says which files changed since tflplanner last wrote
#'   or read them, without reading them.
#'
#' @param study A registered study's id, or an `rtfstudy`.
#' @param home tflplanner's home.
#' @return `reload_from_spec()` and `write_spec()`: the study (its `spec`
#'   says what was read).  `spec_status()`: a data frame, `file`, `status`
#'   (`same`, `changed`, `added`, `removed`), `size_was`, `size_now`.
#' @examples
#' \dontrun{
#' spec_status("ABC-101")
#' s <- reload_from_spec("ABC-101")
#' }
#' @export
reload_from_spec <- function(study, home = tflplanner_home()) {
  st <- .read_state(.study_id_of(study), home)
  if (is.null(st)) stop("No study '", .study_id_of(study), "' is registered.",
                        call. = FALSE)
  s <- .adopt_spec(st, home, full = TRUE)
  if (identical(s$spec$status, "invalid")) stop(.spec_invalid_condition(s$spec$problems))
  .set_config("last_study", s$meta$study_id, home)
  s
}

#' @rdname reload_from_spec
#' @export
write_spec <- function(study, home = tflplanner_home()) {
  st <- .read_state(.study_id_of(study), home)
  if (is.null(st)) stop("No study '", .study_id_of(study), "' is registered.",
                        call. = FALSE)
  save_study(.study_from_state(st), home = home, spec = "overwrite")
}

#' @rdname reload_from_spec
#' @export
spec_status <- function(study, home = tflplanner_home()) {
  st <- .read_state(.study_id_of(study), home)
  if (is.null(st)) stop("No study '", .study_id_of(study), "' is registered.",
                        call. = FALSE)
  .spec_diff(st$files, .spec_fingerprints(st$path))
}

.study_id_of <- function(study) {
  if (inherits(study, "rtfstudy")) study$meta$study_id else
    .check_study_id(study)
}

# Files changed outside tflplanner since it last wrote or read them:
# copied to spec/.rejected/ before a save writes over them
.back_up_rejected <- function(root, files) {
  dir <- file.path(root, study_layout()[["spec"]], ".rejected")
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  out <- character()
  for (f in files[file.exists(file.path(root, files))]) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    ext <- tools::file_ext(f)
    to <- file.path(dir, paste0(tools::file_path_sans_ext(basename(f)), "_",
                                stamp, if (nzchar(ext)) paste0(".", ext)))
    file.copy(file.path(root, f), to, copy.date = TRUE)
    out <- c(out, to)
  }
  out
}

# -------------------------------------------------------------- conditions

.spec_changed_condition <- function(files) {
  structure(class = c("tflplanner_spec_changed", "error", "condition"),
    list(message = paste0(
      "The definition files were changed outside tflplanner since the study ",
      "was last saved or opened: ",
      paste(files$file[files$status != "same"], collapse = ", "),
      ".  Open the study again (or reload_from_spec()) to take them in, or ",
      "write_spec() to write them back from the last save."),
      call = NULL, files = files))
}

.spec_invalid_condition <- function(problems) {
  structure(class = c("tflplanner_spec_invalid", "error", "condition"),
    list(message = paste0(
      "The definition files do not read:\n",
      paste0("  ", problems$file, ": ", problems$message, collapse = "\n")),
      call = NULL, problems = problems))
}

# ------------------------------------------------------- export / import

#' Copy the definition files out, and bring an edited copy back
#'
#' The usual way to edit a study's definition outside tflplanner (#274):
#' copy the files out with `export_spec_files()`, edit the copy (in Excel,
#' renamed as you like), and bring it back with `import_spec_files()`.
#' (Editing `spec/` directly works too: [open_study()] takes it in.)
#'
#' `preview_spec_import()` says what an import would do, changing nothing:
#' the copy is read in a temporary folder -- every reader's check, every
#' cell that holds R parsed, tflspec's checks of the ARD definition and the
#' figure designs, and the programs of the reports it touches written and
#' parsed -- and compared with the study part by part.
#'
#' `import_spec_files()` then takes in the parts chosen (all that differ,
#' unless `parts` says which).  Before it changes anything it copies the
#' study's definition files as they are to `spec/.backup/<time>/`; after,
#' the study is saved (its files written from it).  A copy with errors
#' changes nothing (class `tflplanner_spec_invalid`, its `problems` the
#' file, sheet, row, column and message of each); warnings do not stop it.
#'
#' @param study A registered study's id, or an `rtfstudy`.
#' @param path `export_spec_files()`: a folder, or a `.zip` file, to write.
#'   `preview_spec_import()` / `import_spec_files()`: what to bring back --
#'   a folder (a study folder, its `spec/`, or the files), a `.zip`, or one
#'   file.  A file is known by what it holds, not its name: a workbook by
#'   its sheets, `.json` as the ARD definition, a `.yml` with a `study_id`
#'   as `study.yml`, another `.yml` as the figure design of the report its
#'   name starts with.
#' @param parts The parts to take in (`preview_spec_import()$parts`
#'   names them); `NULL`: every part that differs.
#' @param home tflplanner's home.
#' @return `export_spec_files()`: the paths written.
#'   `preview_spec_import()`: a list -- `parts` (what differs), `labels`,
#'   `outputs` (the reports touched), `problems`, `ok` (no error).
#'   `import_spec_files()`: the study, with `backup` (the folder of the
#'   copy made before).
#' @export
export_spec_files <- function(study, path, home = tflplanner_home()) {
  st <- .read_state(.study_id_of(study), home)
  if (is.null(st)) stop("No study '", .study_id_of(study), "' is registered.",
                        call. = FALSE)
  root <- st$path
  sp <- study_layout()[["spec"]]
  # the files as they are, but the ARD definition as a workbook to edit in
  # Excel (its JSON is the study's copy; an imported workbook replaces it)
  out <- tempfile("tflplanner-export")
  files <- setdiff(.spec_files(root), file.path(sp, c(.ard_json, .ard_file)))
  for (f in files) {
    dir.create(file.path(out, dirname(f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(root, f), file.path(out, f), copy.date = TRUE)
  }
  a <- .planner_from_state(st)$ard
  if (!is.null(a) && (nrow(a$analyses) || nrow(a$datasets) ||
                      nrow(a$populations) || nrow(a$analysis_data))) {
    dir.create(file.path(out, sp), recursive = TRUE, showWarnings = FALSE)
    .write_ard_spec(a, file.path(out, sp, .ard_file))
    files <- c(files, file.path(sp, .ard_file))
  }
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  if (grepl("[.]zip$", path, ignore.case = TRUE)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    zip::zip(normalizePath(path, "/", mustWork = FALSE), files, root = out)
    return(invisible(path))
  }
  for (f in files) {
    dir.create(file.path(path, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(file.path(out, f), file.path(path, f), overwrite = TRUE)
  }
  invisible(file.path(path, files))
}

# The copy laid out as a study folder's files, in a temporary folder: the
# study's own files first, the copy's over them (an import of one workbook
# reads with the study's other files)
.stage_import <- function(root, path, ids) {
  stage <- tempfile("tflplanner-import")
  sp <- study_layout()[["spec"]]
  dir.create(file.path(stage, sp, .fig_design_dir), recursive = TRUE)
  for (f in .spec_files(root)) {
    dir.create(file.path(stage, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(file.path(root, f), file.path(stage, f))
  }
  src <- path
  if (!dir.exists(path) && grepl("[.]zip$", path, ignore.case = TRUE)) {
    src <- tempfile("tflplanner-unzip")
    utils::unzip(path, exdir = src)
  }
  files <- if (dir.exists(src)) {
    list.files(src, recursive = TRUE, full.names = TRUE)
  } else path
  files <- files[!grepl("(^|/)[.](rejected|backup)/", files) &
                   !grepl("(^|/)~[$]", files)]
  placed <- character()
  for (f in files) {
    to <- .import_place(f, ids)
    if (is.na(to)) next
    file.copy(f, file.path(stage, to), overwrite = TRUE)
    placed <- c(placed, to)
  }
  if (!length(placed)) {
    stop("No definition file in ", path, ".", call. = FALSE)
  }
  # an ARD workbook brought back is the ARD definition (the JSON is read
  # only when there is no workbook beside it: the study's JSON goes)
  if (file.path(sp, .ard_file) %in% placed &&
      !file.path(sp, .ard_json) %in% placed) {
    unlink(file.path(stage, sp, .ard_json))
  }
  structure(stage, placed = unique(placed))
}

# Where a file of a copy goes, from what it holds; NA: not a definition file
.import_place <- function(f, ids) {
  sp <- study_layout()[["spec"]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "xlsx") {
    sh <- tryCatch(readxl::excel_sheets(f), error = function(e) character())
    # a workbook that does not open: in place of the one its name says, to
    # be reported by its reader
    if (!length(sh)) {
      b <- basename(f)
      known <- c(.table_file, .report_file, .lf_file, .ard_file)
      hit <- known[startsWith(b, tools::file_path_sans_ext(known))]
      return(file.path(sp, if (length(hit)) hit[1L] else .report_file))
    }
    if ("analyses" %in% sh) return(file.path(sp, .ard_file))
    if ("listings" %in% sh || "figures" %in% sh) return(file.path(sp, .lf_file))
    if (any(c("report", "titles", "page") %in% sh)) return(file.path(sp, .report_file))
    if (any(c("tables", "cells", "variables") %in% sh)) return(file.path(sp, .table_file))
    return(NA_character_)
  }
  if (ext == "json") return(file.path(sp, .ard_json))
  if (ext %in% c("yml", "yaml")) {
    y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.list(y) && !is.null(y$study_id)) return(.study_file)
    b <- tools::file_path_sans_ext(basename(f))
    id <- ids[startsWith(b, ids)]
    id <- if (length(id)) id[which.max(nchar(id))] else b
    return(file.path(sp, .fig_design_dir, paste0(id, ".yml")))
  }
  NA_character_
}

#' @rdname export_spec_files
#' @export
preview_spec_import <- function(study, path, home = tflplanner_home()) {
  st <- .read_state(.study_id_of(study), home)
  if (is.null(st)) stop("No study '", .study_id_of(study), "' is registered.",
                        call. = FALSE)
  s <- .study_from_state(st)
  stage <- .stage_import(s$path, path, output_ids(s$planner))
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  r <- .read_spec(s, stage, books_needed = TRUE)
  problems <- attr(r, "problems")
  attr(r, "problems") <- NULL
  parts <- character()
  outputs <- character()
  if (!.has_errors(problems)) {
    parts <- .changed_parts(s, r)
    outputs <- .changed_outputs(s, r, parts)
    if (length(parts)) problems <- rbind(problems, .check_spec(r, outputs))
  }
  list(parts = parts, labels = .part_labels(parts), outputs = outputs,
       problems = problems, ok = !.has_errors(problems),
       files = attr(stage, "placed"), incoming = r, current = s)
}

#' @rdname export_spec_files
#' @export
import_spec_files <- function(study, path, parts = NULL,
                              home = tflplanner_home()) {
  pv <- preview_spec_import(study, path, home)
  if (!pv$ok) stop(.spec_invalid_condition(pv$problems))
  take <- if (is.null(parts)) pv$parts else intersect(parts, pv$parts)
  s <- pv$current
  if (!length(take)) return(invisible(s))
  # the files as they are, kept before anything changes
  root <- s$path
  backup <- file.path(root, study_layout()[["spec"]], ".backup",
                      format(Sys.time(), "%Y%m%d-%H%M%S"))
  for (f in .spec_files(root)) {
    dir.create(file.path(backup, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(file.path(root, f), file.path(backup, f), copy.date = TRUE)
  }
  inc <- .study_parts(pv$incoming)
  for (k in take) s <- .set_study_part(s, k, inc[[k]])
  out <- save_study(s, home = home, spec = "overwrite")
  out$backup <- backup
  out$spec <- list(status = "imported", parts = take,
                   outputs = .changed_outputs(pv$current, out, take),
                   problems = pv$problems)
  invisible(out)
}
