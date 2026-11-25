# Official runs and previews.
#
# A program run on its own -- from the app, or opened in RStudio -- is a
# preview or a hand run: it writes the study's working output
# (output/ard/ard.rds, output/tfl/) so the next step can go on, and keeps no
# log.  That is a design aid's output: the builder and the figure designer
# read it.  What the screens show as a report's result (its state, its RTF,
# the report list's marks) is the latest official run's, read from runs/.
#
# An official run starts from tflspec::tfl_run_study() or the app's Run
# button (both run programs/batch.R), which makes a batch folder stamped with the date and time (runs/20260927_143000_all/), copies
# the study into it (programs/, spec/, input/, study.yml, renv.lock; with
# --data the data folders) and runs the programs THERE: each program's log
# (logrx), what it made (the study ARD, the RTFs) and the end record land in
# the folder, never in the study's output/.  The folder is the code: its own
# autoexec_*.R (and run_paths.R, which says where the data are) run it again
# elsewhere (--data-root=), without tflspec or tflplanner.  A run that would read or write outside its folder
# stops before any folder is made.  The study's output/ is the working output
# of previews and hand runs only.
#
#   programs/batch.R               the runner of the study's official runs
#
# A study has no entry file of its own (an earlier tflplanner wrote
# programs/autoexec_all.R, programs/ard/autoexec_ard.R and
# programs/tfl/autoexec_report.R; a save removes them).

.batch_file <- "batch.R"
.fig_setup_file <- "fig_setup.R"
.report_setup_file <- "report_setup.R"

# The entry files an earlier tflplanner wrote (a save removes them), and so
# names no report or ARD program may have: a save would remove the program.
.old_entry_files <- c(file.path("programs", "autoexec_all.R"),
                      file.path("programs", "ard", "autoexec_ard.R"),
                      file.path("programs", "tfl", "autoexec_report.R"))

# Every file name tflplanner writes under programs/ (any folder: programs/,
# programs/ard/, programs/tfl/).  A report's program, or an ARD program's
# name (from its output ID), must not be one of them: the save writes the
# report's program and then this file over it.  A new generated file is added here --
# test-program-names.R fails when a save writes one that is not.
.generated_program_files <- function() {
  c(.ard_setup_file, basename(.old_entry_files), .batch_file, .fig_setup_file,
    .report_setup_file,
    .study_setup_file, .study_helpers_file, .company_setup_file, .study_user_file)
}

#' The official-run programs of a study
#'
#' `batch_code()` is `programs/batch.R`: the study's programs (the ARD
#' programs, the report programs, what each report makes) and the runner
#' that runs them into a batch folder.
#'
#' @param x A `tflplanner`.
#' @param date The date stamped in the banner.
#' @param root The study folder (the definition files it keeps with the
#'   code of a run are listed from it).
#' @return The program, one element per line.
#' @examples
#' x <- add_output(new_planner(), "T-14-1-1", type = "table")
#' head(batch_code(x), 5)
#' @export
batch_code <- function(x, date = Sys.Date(), root = ".") {
  .batch_code(x, date, root, .batch_stamp(root))
}

# The stamp of the spec the programs are made from (tflspec's one function:
# study.yml and the spec folder, runs.yml included); computed after the spec
# is written (save_study()) and before the programs are
.batch_stamp <- function(root) {
  if (!dir.exists(root)) return("none")
  tflspec::tfl_spec_stamp(root, spec = study_layout()[["spec"]])
}

# The run definitions as the manifest holds them: the reports resolved (the
# word all expanded, in the report list's order), the options with their
# defaults, and `problem` ("", or why the definition cannot run: tflspec's
# check; the runner refuses it and prints this text)
.batch_runs_manifest <- function(x) {
  chk <- .runs_problems(x)
  nm <- .run_names(x)
  lapply(seq_along(x$runs), function(i) {
    d <- x$runs[[i]]
    list(name = if (is.na(nm[i])) "" else nm[i],
         reports = as.character(.run_def_ids(x, d)),
         mode = if (is.character(d$mode) && length(d$mode) == 1L) d$mode else "all",
         copy_data = isTRUE(d$copy_data),
         problem = paste(chk$problem[chk$definition == .run_label(nm, i)], collapse = "; "))
  })
}

.batch_code <- function(x, date, root, stamp) {
  lay <- study_layout()
  spec <- x$ard %||% .empty_ard_spec()
  ids <- unique(stats::na.omit(spec$analyses$output_id))
  ard_progs <- file.path(lay[["programs_ard"]],
                         vapply(ids, .ard_prog_name, ""))
  out_ids <- x$outputs$output_id
  infos <- lapply(out_ids, function(id) report_info(x, id))
  tfl_progs <- file.path(lay[["programs_tfl"]],
                         vapply(infos, `[[`, "", "program"))
  made <- lapply(infos, function(info) info$file)
  ard_out <- .study_value(spec, "output", "output/ard/ard.rds")
  # the ARD each report declares it reads (#293, #790): see
  # .batch_report_reads(); a run that selects a report but not the reports it
  # declares does not start, nor one that selects a report whose read cannot
  # be worked out.  (A lookup that fails is that report's reason, not an error.)
  reads <- .batch_report_reads(x)
  need <- .batch_reads_needs(reads)
  names(need) <- tfl_progs[match(names(need), out_ids)]
  unresolved <- .batch_reads_unresolved(reads)
  names(unresolved) <- tfl_progs[match(names(unresolved), out_ids)]
  # a report with no program of its own (none is written for it) is in no
  # run: its rows go (an NA name would make batch.R fail to parse)
  need <- need[!is.na(names(need))]
  unresolved <- unresolved[!is.na(names(unresolved))]
  # (the definition: the YAML spec's files)
  spec_files <- setdiff(.spec_files_yaml(root), .study_file)
  # (the study's own analysis functions, study key `source`: every ARD
  # program sources them, by their path from the study folder; #792)
  own_src <- .split_bar(.study_value(spec, "source", NA))
  support <- list(
    ard = c(file.path(lay[["programs_ard"]], .ard_setup_file),
            own_src, spec_files),
    tfl = c(file.path(lay[["programs_tfl"]], c(.fig_setup_file,
                                               .report_setup_file)),
            spec_files),
    all = c(file.path("programs", c(.batch_file, .study_helpers_file)),
            .study_setup_files(), .study_file))
  # the study's id (study.yml's; a study folder is named by its id): each
  # run records it, and only its own runs count (tflspec::tfl_run_index())
  sf <- file.path(root, .study_file)
  sid <- if (file.exists(sf)) tryCatch(.read_meta(root)$study_id,
                                       error = function(e) NA_character_)
  if (!length(sid) || is.na(sid)) {
    sid <- basename(normalizePath(root, winslash = "/", mustWork = FALSE))
  }
  made <- stats::setNames(lapply(made, as.character), tfl_progs)
  dat <- lapply(.batch_data_files(x), as.character)
  # the runner is tflspec's: the lists of the study's programs (the
  # manifest), then its lines
  manifest <- list(
    root = lay[["runs"]], ard = ard_out, study = sid, spec = lay[["spec"]],
    spec_md5 = stamp,
    programs = list(ard = ard_progs, tfl = tfl_progs),
    ard_outputs = stats::setNames(as.character(ids), basename(ard_progs)),
    report_ids = stats::setNames(as.character(out_ids), tfl_progs),
    needs = lapply(need, as.character),
    unresolved = stats::setNames(as.character(unresolved), names(unresolved)),
    runs = .batch_runs_manifest(x),
    outputs = made,
    support = list(ard = as.character(support$ard),
                   tfl = as.character(support$tfl),
                   all = as.character(support$all)),
    data = dat,
    data_vars = .batch_data_vars(dat),
    data_dirs = lay[c("adam", "sdtm", "other")],
    sources = own_src[!is.na(own_src) & nzchar(own_src)],
    check_code = .batch_check_code(root),
    kind = "run", paths = list())
  c(.banner(
      paste("Program    :", file.path("programs", .batch_file)),
      "The runner of the study's official runs, started by tflspec::tfl_run_study()",
      "and the Run button of tflplanner.",
      "Each run makes a batch folder runs/batch_<date>-<time>_<definition>/ with the",
      "logs, what the run made, and the code it ran; the folder's own",
      "autoexec_*.R run it again without tflspec or tflplanner.",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    tflspec::tfl_runner_code(manifest),
    "")
}

# The data-folder variable (path_adam, path_sdtm, path_other) the generated
# code reads each data catalog path through: tflspec's own rule
# (.path_code(): backslashes are slashes, a leading ./ goes), with the
# study's folders (option tflspec.paths).  "" when no data folder holds the
# path.  A named character vector, path -> variable.
.batch_data_vars <- function(dat) {
  paths <- unique(unlist(dat, use.names = FALSE))
  if (!length(paths)) return(character())
  path_code <- utils::getFromNamespace(".path_code", "tflspec")
  old <- options(tflspec.paths = .study_path_vars())
  on.exit(options(old), add = TRUE)
  vars <- vapply(paths, function(p) {
    code <- path_code(p)
    hit <- regmatches(code, regexec("^file[.]path[(](path_[A-Za-z_]+), ", code))[[1L]]
    v <- if (length(hit)) hit[2L] else code
    if (v %in% c("path_adam", "path_sdtm", "path_other")) v else ""
  }, "")
  stats::setNames(unname(vars), paths)
}

# The study's own code the runner scans for paths outside the batch folder:
# the company's and the user's setup (not the generated study_helpers.R: the
# user cannot fix it) and the study's own ARD functions
.batch_check_code <- function(root) {
  fun <- list.files(file.path(root, .study_fun_dir), pattern = "[.][Rr]$")
  c(.company_setup_path(), .study_user_path(),
    if (length(fun)) file.path(.study_fun_dir, sort(fun)))
}

# programs/tfl/fig_setup.R: the study's setup, the packages the figures'
# code uses (the designed figures': ggsurvfit for a KM ...), the figure
# style of the company standards (tflspec), and report_content()
.fig_setup_code <- function(p = NULL) {
  code <- .with_study_code(tflspec::tfl_fig_setup_code(.std_fig_style()))
  # after its banner (the comment lines it starts with)
  at <- match(FALSE, grepl("^#", code), nomatch = length(code) + 1L) - 1L
  rest <- utils::tail(code, length(code) - at)
  rest <- rest[cumsum(nzchar(rest)) > 0L]
  c(utils::head(code, at),
    if (at) "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .source_study_setup(),
    "",
    "# the packages the figures' code uses",
    paste0("library(", .fig_setup_libs(p), ")"),
    "",
    rest,
    "",
    .report_content_fun)
}

# ggplot2, patchwork, dplyr, and what the study's designed figures use
.fig_setup_libs <- function(p = NULL) {
  base <- c("ggplot2", "patchwork", "dplyr")
  ids <- names(p$fig_designs %||% list())
  libs <- unlist(lapply(ids, function(id) tryCatch(
    attr(tflspec::tfl_fig_design_code(fig_design(p, id), id, setup = TRUE,
                                      save = FALSE), "libs"),
    error = function(e) NULL)))
  c(base, sort(setdiff(unique(libs), base)))
}

# The data files each program reads, by the data catalog: an ARD program
# its report's datasets (an analysis data's: those it is made from); a
# report program its datasets (a listing, a figure, a report of one's own
# code), or the ARD taken in it reads (input/ard/<file>).  A table's
# report program reads the study ARD, which the run keeps anyway.  A named
# list, program -> paths as the catalog gives them (relative to the study
# folder).  A dataset not in the catalog, or a file one's own code reads
# without naming it in the report list, is not known here.
.batch_data_files <- function(x) {
  lay <- study_layout()
  cat_ds <- ard_rows(x, "datasets")
  path_of <- function(ds) {
    p <- cat_ds$path[match(ds, cat_ds$dataset)]
    unique(p[!is.na(p) & nzchar(p)])
  }
  spec <- x$ard %||% .empty_ard_spec()
  a_ids <- unique(stats::na.omit(spec$analyses$output_id))
  ard <- lapply(a_ids, function(id) path_of(.report_datasets(x, id, "table")))
  names(ard) <- file.path(lay[["programs_ard"]], vapply(a_ids, .ard_prog_name, ""))
  ids <- x$outputs$output_id
  infos <- lapply(ids, function(id) report_info(x, id))
  tfl <- lapply(seq_along(ids), function(i) {
    imp <- .ard_import_of(x, ids[i])
    unique(c(if (!identical(infos[[i]]$type, "table"))
      path_of(.report_datasets(x, ids[i], infos[[i]]$type)),
      if (!is.null(imp)) file.path(lay[["ard_import"]], imp)))
  })
  names(tfl) <- file.path(lay[["programs_tfl"]], vapply(infos, `[[`, "", "program"))
  c(ard, tfl)
}

#' The data an official run would keep
#'
#' The data files the programs of a run read (by the data catalog, see
#' [run_batch()]'s `copy_data`), with their size, as the study folder has them
#' now: what a run definition with `copy_data` would copy into the batch folder.
#'
#' @param study An `rtfstudy`.
#' @param parts `"ard"`, `"tfl"` or both.
#' @param output_ids The reports of the run; `NULL` for every report.
#' @return A data frame: `path` (as the catalog gives it), `size` (bytes;
#'   `NA` when the file is not there), `programs` (the run's programs that
#'   read it, one string).
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it saves the sample study)
#' s <- save_study(create_sample_study(run = FALSE))
#' batch_data_plan(s)[c("path", "size")]  # the data files a run would keep
#' }
#' options(old)
#' @export
batch_data_plan <- function(study, parts = c("ard", "tfl"), output_ids = NULL) {
  parts <- match.arg(parts, c("ard", "tfl"), several.ok = TRUE)
  x <- study$planner
  lay <- study_layout()
  dat <- .batch_data_files(x)
  ids <- output_ids %||% x$outputs$output_id
  progs <- c(
    if ("ard" %in% parts) file.path(lay[["programs_ard"]], vapply(ids, .ard_prog_name, "")),
    if ("tfl" %in% parts) file.path(lay[["programs_tfl"]], vapply(ids, function(id)
      report_info(x, id)$program, "")))
  dat <- dat[intersect(names(dat), progs)]
  files <- unique(unlist(dat, use.names = FALSE))
  if (!length(files)) {
    return(data.frame(path = character(), size = numeric(), programs = character(),
                      stringsAsFactors = FALSE))
  }
  full <- .data_file(study$path, files)
  data.frame(
    path = files,
    size = unname(file.size(full)),
    programs = vapply(files, function(f) paste(basename(names(dat)[vapply(dat, function(v)
      f %in% v, NA)]), collapse = ", "), "", USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
}

# the runner, written with the study (and the entry files of an earlier
# tflplanner taken away)
.batch_cache <- new.env()
.save_batch_programs <- function(p, root) {
  lay <- study_layout()
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  put <- function(code, f) {
    f <- file.path(root, f)
    out[nrow(out) + 1L, ] <<- list(f, .put_program(code, f, root))
  }
  # (the same reports, ARD and definition files: the same program -- a
  # save after a table's card edit writes none again, #417)
  # (the run definitions, and the stamp of the spec as written: it is taken
  # after the spec is written and before the programs, and a clean save
  # refreshes batch.R when only the stamp differs)
  stamp <- .batch_stamp(root)
  key <- list(p$ard, p$outputs, p$study, p$sheets$report, p$sheets$codelists, p$fig_designs,
              p$lf, .spec_files_yaml(root), root, Sys.Date(), .code_context$opts,
              .batch_check_code(root), .runs_bare(p$runs), stamp)
  if (!identical(.batch_cache$key, key)) {
    .batch_cache$code <- .batch_code(p, Sys.Date(), root, stamp)
    .batch_cache$key <- key
  }
  put(.batch_cache$code, file.path("programs", .batch_file))
  # the figure style, and the one function a report of one's own code ends
  # with (report_content()), written once here
  put(.fig_setup_code(p), file.path(lay[["programs_tfl"]], .fig_setup_file))
  put(report_setup_code(p), file.path(lay[["programs_tfl"]], .report_setup_file))
  rbind(out, .remove_old_entry_files(root))
}

# The entry files an earlier tflplanner wrote: removed, the way an ARD
# program of an output no longer defined is -- one edited by hand copied to
# programs/.edited/ first.  Only a file tflplanner wrote (it has a checksum
# or its generated banner) is removed.  The save's rows, status "removed".
.remove_old_entry_files <- function(root) {
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  for (rel in .old_entry_files) {
    p <- file.path(root, rel)
    if (!file.exists(p)) next
    have <- readLines(p, warn = FALSE, encoding = "UTF-8")
    chk <- sub("^#  Checksum   : *", "",
               grep("^#  Checksum   :", have, value = TRUE))
    if (!length(chk) && !any(grepl("^#  Generated  : tflplanner", have))) next
    if (length(chk) && !identical(chk[1L], .body_hash(have))) .back_up_edited(p, root)
    file.remove(p)
    out[nrow(out) + 1L, ] <- list(p, "removed")
  }
  out
}

#' Official runs of a study
#'
#' `run_batch()` starts an official run of a saved run definition
#' ([run_definitions()]): the command of [tflspec::tfl_run_command()] (the
#' runner, `programs/batch.R`, with `--run=<name>`).  The run makes a batch folder
#' `runs/batch_<date>-<time>_<name>/` with each program's log, what the run
#' made, the R and package versions (`session_info.txt`, and the study's
#' `renv.lock` when it has one), the study's programs and definition files
#' copied in (the folder is the code) and, when the definition says
#' `copy_data`, the data files its programs read.  `list_batches()` lists a
#' study's batch folders ([tflspec::tfl_list_runs()]).
#'
#' The definition's `mode` decides what is made: `"all"` the ARD, then the
#' reports from it; `"ard"` the ARD only.  The reports are made from the ARD
#' the same run makes.  The run is of the saved study: save it first (the
#' programs hold the definitions as saved, and a run on programs older than
#' the spec is refused).
#'
#' The data kept (`copy_data`) are the files of the data catalog the run's
#' programs read ([batch_data_plan()]): each ARD program its report's
#' datasets, each report program its own (a listing's, a figure's) or the ARD
#' taken in it reads.  They go to the batch folder's `data/` under the path
#' they have in the study folder (`data/data/adam/adsl.rds`; a file outside it
#' under `data/external/`), listed in `data_manifest.csv` with their size, md5
#' and time.  With the code and the logs, that is everything the run read: a
#' run that can be made again.
#'
#' @param study An `rtfstudy`.
#' @param name The run definition's name, as saved.
#' @param wait `FALSE` returns the running [processx::process] at once.
#'   The run goes on by itself: closing the app, or R, does not stop it,
#'   and a study runs one official run at a time.  `TRUE` waits for it.
#' @return `run_batch()`: with `wait = TRUE`, a list: `ok`, `batch` (the
#'   batch folder), `result` (its run.csv), `output` (what the run
#'   printed); otherwise the process.  `list_batches()`: a data frame, one
#'   row a batch folder, the newest first, with its `state` -- `"done"`
#'   (every program without error), `"failed"` (`errors` of `programs`),
#'   or `"incomplete"` (no record of its end yet: still running, or
#'   stopped) -- its `definition` and when it `finished`.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (needs the sample data and rtfreporter; the run is in child R processes)
#' s <- create_sample_study(run = FALSE)
#' b <- run_batch(s, "All", wait = TRUE)  # the sample's definition: every report
#' b$ok
#' list_batches(s)[, c("batch", "definition", "state")]
#' }
#' options(old)
#' @export
run_batch <- function(study, name, wait = FALSE) {
  if (missing(name) || !is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("Give the name of a run definition (`run_definitions()` lists them).", call. = FALSE)
  }
  # one official run at a time in a study, whoever started it
  live <- .live_run(study$meta$study_id)
  if (!is.null(live)) {
    stop("An official run of ", study$meta$study_id, " is going on (started ",
         live$started, "): wait until it has finished.", call. = FALSE)
  }
  lay <- study_layout()
  def <- study$planner$runs[match(name, .run_names(study$planner))][[1L]]
  if (is.null(def)) {
    have <- .run_names(study$planner)
    stop("No run definition named ", sQuote(name), if (length(have)) paste0(": the study's are ",
         paste(sQuote(have), collapse = ", ")) else " (the study has none)", ".", call. = FALSE)
  }
  # an ARD definition that does not hold writes no ARD program (#323): the
  # run does not start (the saved definition, the one the programs are of)
  ap <- .ard_definition_problem(study$planner)
  if (!is.null(ap)) {
    stop("The study cannot start an official run: its ARD definition does not hold, ",
         "and a definition that does not hold writes no ARD program (see the Review tab). ",
         ap, call. = FALSE)
  }
  prob <- tflspec::tfl_programs_problem(study$path)
  if (nzchar(prob)) {
    stop("The study cannot start an official run yet: save it first. ", prob, call. = FALSE)
  }
  # a report whose template or code needs a package not installed: said
  # now, not as an error in the middle of the run (#682)
  if (!identical(def$mode, "ard")) {
    .need_report_pkgs(study$planner, .batch_report_ids(study$planner, name))
  }
  before <- list_batches(study)$batch
  out <- tempfile("batch", fileext = ".txt")
  cmd <- tflspec::tfl_run_command(study$path, name)
  # detached: the run goes on when the app (this R process) ends -- closing
  # the browser, [Close], starting the app again -- and is recorded in the
  # home so an app opened later finds it (live_runs.R)
  px <- processx::process$new(
    cmd[[1L]], cmd[-1L],
    wd = attr(cmd, "wd"),
    # the run's own R finds the packages this R does: its library paths, the
    # rest of the environment as it is ("current")
    env = c("current", R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    stdout = out, stderr = "2>&1", cleanup = FALSE)
  rec <- tryCatch(.record_run(px, study, paste("run", name), before),
                  error = function(e) NULL)
  if (!wait) return(px)
  px$wait()
  if (!is.null(rec)) unlink(rec)
  txt <- if (file.exists(out)) readLines(out, warn = FALSE) else character()
  b <- setdiff(list_batches(study)$batch, before)
  dir <- if (length(b)) file.path(study$path, lay[["runs"]], b[1L])
  csv <- if (!is.null(dir)) file.path(dir, "run.csv")
  list(ok = identical(px$get_exit_status(), 0L), batch = dir,
       result = if (!is.null(csv) && file.exists(csv))
         utils::read.csv(csv, colClasses = "character"),
       output = txt)
}

# The ARD each report reads: tflspec::tfl_ard_reads() (the SPEC's rule, one
# for the GUI and the CLI), given the reports as they are on screen -- every
# report of the planner, unsaved edits and the analyses of its sheets in
# them (.spec_now() takes the edits in; the saved files are not read) -- and
# the study's spec.  A data.frame, one row per read:
#   report  the output id
#   reads   the id whose ARD it reads; NA when that cannot be worked out
#   kind    "own" (its own analyses) or "source" (a figure's ARD source)
#   reason  NA when resolved; else tflspec's: "no source", "source not
#           understood: <value>", "source not in analyses", "source report
#           not found", "source lookup failed", "ref analysis: <ref>",
#           "file not read: <message>" or "lookup failed: <message>"
# A report that reads none has no row (an `import:<file>` source reads no
# report's ARD: no row).  A failure of the lookup as a whole (the spec could
# not be made from the sheets) is a row of every report, not an error.  A
# run must not start on a read not known.
.batch_report_reads <- function(x) {
  out_ids <- x$outputs$output_id
  empty <- data.frame(report = character(), reads = character(),
                      kind = character(), reason = character(),
                      stringsAsFactors = FALSE)
  got <- tryCatch({
    sp <- .spec_now(.spec_ensure(x))
    if (is.null(sp) || is.null(sp$study)) stop("the planner has no spec", call. = FALSE)
    # every report on screen: a report the spec lacks is a lookup that fails,
    # not left out (a source naming it would be "source report not found")
    lack <- setdiff(out_ids, names(sp$outputs))
    if (length(lack)) {
      stop("no spec for the report(s): ", paste(lack, collapse = ", "), call. = FALSE)
    }
    tflspec::tfl_ard_reads(sp$study, sp$outputs)
  }, error = function(e) {
    if (!length(out_ids)) return(empty)
    data.frame(report = out_ids, reads = NA_character_, kind = "source",
               reason = paste0("lookup failed: ", conditionMessage(e)),
               stringsAsFactors = FALSE)
  })
  # (the order of the report list, as before)
  got <- got[order(match(got$report, out_ids), na.last = TRUE), , drop = FALSE]
  got <- got[, names(empty), drop = FALSE]
  rownames(got) <- NULL
  got
}

# the needs of .batch_report_reads(): a named list, output id -> the ids whose
# ARD it reads (a report that reads none is not in it)
.batch_reads_needs <- function(reads) {
  r <- reads[!is.na(reads$reads), , drop = FALSE]
  lapply(split(r$reads, factor(r$report, unique(r$report))), unique)
}

# its unresolved: a named character, output id -> the reasons, joined with "; "
.batch_reads_unresolved <- function(reads) {
  r <- reads[!is.na(reads$reason), , drop = FALSE]
  vapply(split(r$reason, factor(r$report, unique(r$report))),
         function(v) paste(unique(v), collapse = "; "), "")
}

# (the needs only: a named list, output id -> output ids)
.batch_report_needs <- function(x) .batch_reads_needs(.batch_report_reads(x))

# The selected reports that read the ARD of a report that is not selected:
# a run builds the ARD of the selected reports only (#790).  `needs`: report
# -> the reports whose ARD it reads (.batch_report_needs()); `selected`: the
# reports of the run.  A data.frame, one row per pair: report, need.
.batch_missing_needs <- function(needs, selected) {
  report <- character()
  need <- character()
  for (r in intersect(names(needs), selected)) {
    m <- setdiff(needs[[r]], selected)
    report <- c(report, rep(r, length(m)))
    need <- c(need, m)
  }
  data.frame(report = report, need = need, stringsAsFactors = FALSE)
}

# The programs a report left out of a run is: its ARD program and its report
# program (an output id), or the program named
.batch_exclude <- function(x, exclude) {
  ids <- x$outputs$output_id
  progs <- unlist(lapply(exclude, function(e) {
    if (!e %in% ids) return(e)
    c(.ard_prog_name(e), report_info(x, e)$program)
  }))
  unique(progs[!is.na(progs) & nzchar(progs)])
}

#' @rdname run_batch
#' @export
list_batches <- function(study) {
  # tflspec's list of the study's run folders (one reader for the run index
  # too); the columns the Runs table reads
  d <- tflspec::tfl_list_runs(study$path)
  # (a folder starting with "." (.lock ...) is the runner's own)
  d <- d[!startsWith(d$run, "."), , drop = FALSE]
  state <- d$state
  state[state %in% "running or stopped"] <- "incomplete"
  out <- data.frame(
    batch = d$run, started = d$started, what = d$mode, definition = d$definition,
    programs = d$programs, errors = d$errors, state = state, finished = d$finished,
    kind = ifelse(d$rerun %in% TRUE, "rerun", d$kind),
    code = dir.exists(file.path(d$path, "programs")), data = d$data,
    excluded = as.character(ifelse(is.na(d$excluded), "", d$excluded)), path = d$path,
    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# a time taken as people read it: 0:42, 12:05, 1:02:09
.elapsed_text <- function(secs) {
  secs <- max(0, round(as.numeric(secs)))
  h <- secs %/% 3600
  m <- (secs %% 3600) %/% 60
  s <- secs %% 60
  if (h > 0) sprintf("%d:%02d:%02d", h, m, s) else sprintf("%d:%02d", m, s)
}

#' Compare two official runs
#'
#' What changed between two batch folders of a study ([list_batches()]):
#' their inputs -- the data, by the md5 `data_manifest.csv` records, and
#' the code, by the md5 of the files of the folder's `programs/`, where both runs kept them
#' -- and their results (`output/`).  After a fix to the data or the
#' programs, it says whether only what was fixed changed.
#'
#' A result is `"identical"` (the same bytes), `"same but the time"` (a
#' report or a status file the same but the date and time it prints or
#' records), `"same object"` (an `.rds` read the same), `"different"`,
#' `"not made"` (the earlier run made it, this one did not) or `"new"`.
#' An input is `"added"`, `"removed"` or `"changed"` (one the same is not
#' listed); `"not kept"` when either run did not keep that input.
#'
#' @param study An `rtfstudy`.
#' @param batch A batch folder's name (`list_batches()$batch`): the later
#'   run.
#' @param against The earlier one; `NULL`: the run of the same kind (ARD,
#'   reports, both) before `batch`.
#' @return A data frame, `kind` (`"data"`, `"code"`, `"output"`), `file`,
#'   `result`; with `attr(, "batches")` (`against`, `batch`) and
#'   `attr(, "changed")` (a list: the `inputs` and the `outputs` that
#'   changed).
#' @examples
#' \donttest{
#' home <- tempfile()
#' old <- options(tflplanner.home = home)
#' setup_tflplanner(studies_root = file.path(home, "studies"))
#' s <- create_study("CMP", planner = add_output(new_planner(), "T-1"))
#' d <- file.path(s$path, "runs")
#' # two batch folders as two official runs leave them (the second made a
#' # different RTF)
#' for (b in c("20260101_100000_report", "20260102_100000_report")) {
#'   dir.create(file.path(d, b, "output", "tfl"), recursive = TRUE)
#'   writeLines(b, file.path(d, b, "output", "tfl", "T-1.rtf"))
#'   writeLines(c("part,program,status", "tfl,programs/tfl/T-1.R,OK"),
#'              file.path(d, b, "run.csv"))
#'   writeLines("Finished : 2026-01-01 10:00:00", file.path(d, b, "batch.txt"))
#' }
#' compare_batches(s, "20260102_100000_report")
#' options(old)
#' }
#' @export
compare_batches <- function(study, batch, against = NULL) {
  lb <- list_batches(study)
  if (!batch %in% lb$batch) stop("No batch folder ", sQuote(batch), ".", call. = FALSE)
  against <- against %||% .batch_previous(lb, batch)
  if (is.na(against) || !against %in% lb$batch) {
    stop("No earlier official run to compare ", sQuote(batch), " with.", call. = FALSE)
  }
  # tflspec's comparison of two run folders (the same as for a script's)
  tflspec::tfl_compare_runs(lb$path[match(against, lb$batch)], lb$path[match(batch, lb$batch)])
}

# The official run of the same kind (ARD, reports, both) before a batch
# folder that ended (one that stopped has no results to compare); NA when
# there is none
.batch_previous <- function(lb, batch) {
  i <- match(batch, lb$batch)
  # (newest first)
  later <- seq_along(lb$batch) > i & lb$state %in% c("done", "failed")
  same <- later & lb$what %in% lb$what[i]
  k <- c(which(same), which(later))[1L]
  if (is.na(k)) NA_character_ else lb$batch[k]
}

# A batch folder's state, short, for the runs table: running (and for how
# long), done (when; how many without error), failed (how many errors),
# not finished.  `now`: the time it is (a running one's time taken).
.batch_state_text <- function(d, now = Sys.time(), t = identity) {
  started <- as.POSIXct(d$started, format = "%Y-%m-%d %H:%M:%S")
  fin <- as.POSIXct(d$finished, format = "%Y-%m-%d %H:%M:%S")
  vapply(seq_len(nrow(d)), function(i) switch(
    d$state[i],
    running = sprintf(t("Running %s"),
                      .elapsed_text(difftime(now, started[i], units = "secs"))),
    done = sprintf(t("Done %s: %d of %d without error"), format(fin[i], "%H:%M"),
                   d$programs[i], d$programs[i]),
    failed = sprintf(t("%d errors (of %d), %s"), d$errors[i], d$programs[i],
                     format(fin[i], "%H:%M")),
    t("Not finished (stopped)")), "")
}

# a size in MB as people read it: 41 KB, 1.4 MB, 12 MB, 1.3 GB
.mb_text <- function(mb) {
  if (mb >= 1024) sprintf("%.1f GB", mb / 1024) else
    if (mb >= 10) sprintf("%.0f MB", mb) else
      if (mb >= 1) sprintf("%.1f MB", mb) else sprintf("%.0f KB", mb * 1024)
}
