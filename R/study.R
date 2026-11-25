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
#     output/ard/          the study's working ARD (ard.rds): what previews
#                          and hand runs of a program leave, for the design
#                          aids to read; not an official result
#     input/ard/           ARDs made elsewhere and taken in (import_ard()),
#                          with their record imports.csv; nothing else
#                          writes here
#     output/tfl/          the working reports: the RTF files of previews and
#                          hand runs
#     runs/                official runs: one batch folder each, a copy of
#                          the study the programs ran in: the logs, the
#                          results (the same output/ layout) and the code.  The screens read a report's state and
#                          RTF from the latest run that made it
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

# ---- study.yml written in place ---------------------------------------------
# study.yml holds the study's fields, the YAML spec's study part (settings,
# keys, setup code as !r, values tagged !series, !study, !unset ...) and
# blocks written by hand or by the AI screens.  A writer that reads the whole
# file and writes it back loses what the YAML reader does not keep: the tags
# (`setup: !r x <- 1` comes back as the text `x <- 1`), the comments, the
# quoting.  So every writer of study.yml changes its own key's lines in the
# text and leaves every other line as it was (.yml_set_key()).

# A file's lines (none when it is not there; a byte order mark left out)
.study_yml_lines <- function(f) {
  if (!file.exists(f)) return(character())
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  if (length(lines)) lines[1L] <- sub("^\ufeff", "", lines[1L])
  lines
}

.study_yml_write <- function(lines, f) {
  writeLines(enc2utf8(as.character(lines)), f, useBytes = TRUE)
  invisible(f)
}

# The lines of the key `key` at `indent` spaces, looked for in lines `from`
# to `to`: its line and the lines of its value (more indented; a sequence's
# `- ` at its own indent; blank and comment lines between them), the blank
# and comment lines at its end left out (they stay with what follows).
# None when the key is not there.
.yml_key_range <- function(lines, key, from = 1L, to = length(lines), indent = 0L) {
  if (to < from) return(integer())
  pad <- strrep(" ", indent)
  ind <- nchar(sub("^( *).*$", "\\1", lines))
  blank <- grepl("^[[:space:]]*$", lines)
  comment <- grepl("^[[:space:]]*#", lines)
  at <- NA_integer_
  for (i in seq.int(from, to)) {
    if (blank[i] || comment[i] || ind[i] != indent) next
    rest <- substring(lines[i], indent + 1L)
    k <- sub("[[:space:]]*:([[:space:]].*)?$", "", rest)
    if (identical(k, rest)) next
    if (identical(gsub("^[\"']|[\"']$", "", k), key)) {
      at <- i
      break
    }
  }
  if (is.na(at)) return(integer())
  end <- at
  i <- at + 1L
  while (i <= to) {
    item <- ind[i] == indent && grepl("^-( |$)", substring(lines[i], indent + 1L))
    if (!(blank[i] || comment[i] || ind[i] > indent || item)) break
    if (!blank[i] && !comment[i]) end <- i
    i <- i + 1L
  }
  at:end
}

# `lines` with the lines `r` replaced by `new`; no `r`: `new` put after line
# `after`
.yml_replace <- function(lines, r, new, after = length(lines)) {
  if (!length(r)) return(append(lines, new, after = after))
  c(lines[seq_len(r[1L] - 1L)], new, lines[-seq_len(r[length(r)])])
}

# A value's lines as the YAML package writes them, under `key`
.yml_value_lines <- function(key, value) {
  x <- stats::setNames(list(value), key)
  out <- yaml::as.yaml(x, handlers = list(logical = yaml::verbatim_logical))
  strsplit(sub("\n$", "", out), "\n", fixed = TRUE)[[1L]]
}

# `lines` (a YAML file's) with the top-level key `key` set to `value`, or,
# with `under`, the key `key` of the top-level block `under`: that key's
# lines replaced, or added at the end of the block (of the file), every other
# line as it was.  `value` NULL: the key's lines taken out.  A block written
# on its key's line (`ai: {run: one}`) is written again from what it says.
.yml_set_key <- function(lines, key, value, under = NULL) {
  lines <- as.character(lines %||% character())
  new <- function(pad) if (is.null(value)) character() else paste0(pad, .yml_value_lines(key, value))
  if (is.null(under)) return(.yml_replace(lines, .yml_key_range(lines, key), new("")))
  b <- .yml_key_range(lines, under)
  if (!length(b)) {
    if (is.null(value)) return(lines)
    return(c(lines, paste0(under, ":"), new("  ")))
  }
  if (!grepl(":[[:space:]]*(#.*)?$", lines[b[1L]])) {
    v <- tryCatch(yaml::yaml.load(paste(lines[b], collapse = "\n"))[[under]],
                  error = function(e) NULL)
    if (!is.list(v)) v <- list()
    v[[key]] <- value
    blk <- if (length(v)) .yml_value_lines(under, v) else paste0(under, ":")
    return(.yml_replace(lines, b, blk))
  }
  last <- b[length(b)]
  inner <- if (last > b[1L]) seq.int(b[1L] + 1L, last) else integer()
  inner <- inner[!grepl("^[[:space:]]*(#.*)?$", lines[inner])]
  indent <- if (length(inner)) nchar(sub("^( *).*$", "\\1", lines[inner[1L]])) else 2L
  r <- .yml_key_range(lines, key, b[1L] + 1L, last, indent)
  .yml_replace(lines, r, new(strrep(" ", indent)), after = last)
}

.write_meta <- function(meta, path) {
  meta$updated <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  meta$tflplanner <- as.character(utils::packageVersion("tflplanner"))
  meta <- lapply(meta, function(v) if (length(v) == 1L && is.na(v)) "" else v)
  f <- file.path(path, .study_file)
  # study.yml holds the YAML spec's study part too (its settings, keys,
  # setup code as !r, tagged values, comments): the fields' lines set, every
  # other line as it is
  lines <- .study_yml_lines(f)
  for (k in names(meta)) lines <- .yml_set_key(lines, k, meta[[k]])
  .study_yml_write(lines, f)
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
#'   their study-default rows, analysis sets, data catalog and code lists
#'   (`spec/defaults.yml` `codelists:`).
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
  # a study made from the company standards takes their code lists too
  # (defaults.yml `codelists:`); one made from a planner has that one's
  if (is.null(planner)) {
    s$planner <- .std_codelists_into_spec(s$planner, c(s$meta, created = meta$created),
                                          home = home)
  }
  s <- save_study(s, home = home)
  # the company's review depth of the AI agent into study.yml (the study's
  # own from now on), and the state told of the file as it is now
  if (file.exists(file.path(path, .study_file))) {
    .ai_review_write(path, .ai_review_company())
    rel <- .study_file
    if (!is.null(s$planner$spec$text[[rel]])) {
      s$planner$spec$text[[rel]] <- readLines(file.path(path, rel), encoding = "UTF-8", warn = FALSE)
    }
    .write_state(s, home)
  }
  invisible(s)
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
    if (is_dir) {
      # its programs as its setup has it, as below (#680)
      s <- register_study(study, home = home)
      .use_study(s$path)
      return(s)
    }
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
  # a report's argument saved as R code text by an old card (rtfreporter#958)
  .warn_deparsed_args(.deparsed_args(s$planner$spec$outputs %||% list()))
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
