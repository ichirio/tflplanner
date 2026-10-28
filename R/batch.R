# Official runs and previews.
#
# A program run on its own -- from the app, or opened in RStudio -- is a
# preview: it writes the study's working output (output/ard/ard.rds,
# output/tfl/) so the next step can go on, and keeps no log.
#
# An official run goes through an autoexec program, which makes a batch
# folder stamped with the date and time (runs/20260927_143000_all/) and
# keeps in it everything of the run: each program's log (logrx), what it
# made (the study ARD, the RTFs), and -- unless asked not to -- the programs
# and definition workbooks it ran; asked, the data files its programs read
# (by the data catalog), with their md5 -- a run that can be made again.
#
#   programs/batch.R               the runner every autoexec sources
#   programs/ard/autoexec_ard.R    official run of the ARD programs
#   programs/tfl/autoexec_report.R official run of the report programs
#   programs/autoexec_all.R        both, the ARD first, into one batch folder

.batch_file <- "batch.R"
.fig_setup_file <- "fig_setup.R"
.report_setup_file <- "report_setup.R"
.autoexec_all_file <- "autoexec_all.R"

#' The official-run programs of a study
#'
#' `batch_code()` is `programs/batch.R`: the study's programs (the ARD
#' programs, the report programs, what each report makes) and the runner
#' that runs them into a batch folder.  `autoexec_all_code()` is
#' `programs/autoexec_all.R` (the ARD, then the reports); see also
#' [ard_autoexec_code()] and [autoexec_code()].
#'
#' @param x A `tflplanner`.
#' @param date The date stamped in the banner.
#' @param root The study folder (the definitions' fingerprints read the
#'   files the ARD programs name from it).
#' @return The program, one element per line.
#' @examples
#' x <- add_output(new_planner(), "T-14-1-1", type = "table")
#' head(batch_code(x), 5)
#' @export
batch_code <- function(x, date = Sys.Date(), root = ".") {
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
  # the figures printing an ARD's numbers (#293)
  need <- vapply(out_ids, function(id)
    tryCatch(.fig_ard_need(x, id), error = function(e) NA_character_), "")
  need_progs <- tfl_progs[!is.na(need)]
  need_ids <- unname(need[!is.na(need)])
  need_hash <- vapply(unique(need_ids), function(id) tryCatch(
    tflspec::tfl_ard_spec_hash(structure(spec, class = "tfl_ard_spec"), id, dir = root,
                               codelists = .study_codelists(x)),
    error = function(e) ""), "")
  # (the definition: the YAML spec's files)
  spec_files <- setdiff(.spec_files_yaml(root), .study_file)
  support <- list(
    ard = c(file.path(lay[["programs_ard"]], c(.ard_setup_file,
                                               .ard_autoexec_file)),
            spec_files),
    tfl = c(file.path(lay[["programs_tfl"]], c("autoexec_report.R",
                                               .fig_setup_file,
                                               .report_setup_file)),
            spec_files),
    all = c(file.path("programs", c(.batch_file, .autoexec_all_file,
                                    .study_helpers_file)),
            .study_setup_path(), .study_file))
  vec <- function(v, names = NULL, indent = "  ") {
    if (!length(v)) return("character()")
    q <- encodeString(v, quote = "\"")
    if (!is.null(names)) q <- paste(encodeString(names, quote = "`"), "=", q)
    paste0("c(\n", paste0(indent, q, collapse = ",\n"), ")")
  }
  # a list of vectors, its elements indented as the list's
  vlist <- function(...) {
    v <- list(...)
    paste0("list(\n", paste0("  ", names(v), " = ",
                             vapply(v, vec, "", indent = "    "), collapse = ",\n"),
           "\n)")
  }
  runner <- readLines(system.file("batch", "runner.R", package = "tflplanner"),
                      warn = FALSE, encoding = "UTF-8")
  c(.banner(
      paste("Program    :", file.path("programs", .batch_file)),
      "The runner of the study's official runs: programs/ard/autoexec_ard.R,",
      "programs/tfl/autoexec_report.R and programs/autoexec_all.R source it.",
      "Each run makes a batch folder runs/<date>_<time>_<what>/ with the",
      "logs, what the run made, and the code it ran.",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "# ---- the study's programs, in the order they run ---------------------------",
    paste0(".batch_root <- ", encodeString(lay[["runs"]], quote = "\"")),
    paste0(".batch_ard  <- ", encodeString(ard_out, quote = "\"")),
    paste0(".batch_programs <- ", vlist(ard = ard_progs, tfl = tfl_progs)),
    "# the output each ARD program makes",
    paste0(".batch_ard_outputs <- ", vec(ids, basename(ard_progs))),
    "# the output each report program makes",
    paste0(".batch_report_ids <- ", vec(out_ids, tfl_progs)),
    "# a figure printing an ARD's numbers: the report whose ARD it reads, and",
    "# the fingerprint of that ARD's definition when this file was written",
    "# (made again first when the ARD is not made from it)",
    paste0(".batch_needs <- ", vec(need_ids, need_progs)),
    paste0(".batch_needs_hash <- ", vec(need_hash, names(need_hash))),
    "# the named batches (the report list's `batches`): name -> outputs",
    paste0(".batch_sets <- ", if (length(sets <- batch_sets(x)))
      paste0("list(\n", paste0("  ", encodeString(names(sets), quote = "`"), " = ",
                               vapply(sets, function(v) paste0("c(", paste(
                                 encodeString(v, quote = "\""), collapse = ", "), ")"), ""),
                               collapse = ",\n"), ")") else "list()"),
    "# what each report program makes",
    paste0(".batch_outputs <- list(\n",
           paste0("  ", encodeString(tfl_progs, quote = "`"), " = ",
                  vapply(made, function(m) paste0("c(", paste(
                    encodeString(m, quote = "\""), collapse = ", "), ")"), ""),
                  collapse = ",\n"), ")"),
    "# kept with the code of a run",
    paste0(".batch_support <- ", vlist(ard = support$ard, tfl = support$tfl,
                                       all = support$all)),
    "# the data files each program reads (the data catalog's; --data keeps them)",
    paste0(".batch_data <- ", if (length(dat <- .batch_data_files(x))) paste0(
      "list(\n", paste0("  ", encodeString(names(dat), quote = "`"), " = ",
                        vapply(dat, function(v) if (length(v)) paste0("c(", paste(
                          encodeString(v, quote = "\""), collapse = ", "), ")") else
                            "character()", ""),
                        collapse = ",\n"), ")") else "list()"),
    "",
    runner,
    "")
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

.autoexec_banner <- function(file, what, usage, date) {
  .banner(
    paste("Program    :", file),
    what,
    paste("  Rscript", file, "                 every program"),
    paste("  Rscript", file, usage),
    paste("  Rscript", file, "--no-code        the batch folder without the code"),
    paste("  Rscript", file, "--data           with the data files the programs read"),
    "Run it from the study folder.  It makes a batch folder",
    "runs/<date>_<time>_<what>/: each program's log (logrx), what the run",
    "made, the programs and definition workbooks it ran, the R and package",
    "versions (session_info.txt) and, with --data, the data it read.",
    paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
           ", ", format(date, "%Y-%m-%d")))
}

.autoexec_body <- function(parts) {
  c("",
    "if (!file.exists(\"study.yml\")) {",
    "  stop(\"Run it from the study folder (the one with study.yml).\")",
    "}",
    paste0("source(", encodeString(file.path("programs", .batch_file),
                                   quote = "\""), ")"),
    sprintf("run_batch(%s, commandArgs(trailingOnly = TRUE))",
            if (length(parts) == 1L) encodeString(parts, quote = "\"") else
              paste0("c(", paste(encodeString(parts, quote = "\""),
                                 collapse = ", "), ")")),
    "")
}

#' @rdname batch_code
#' @export
autoexec_all_code <- function(date = Sys.Date()) {
  c(.autoexec_banner(file.path("programs", .autoexec_all_file),
                     "Official run: the ARD programs, then the report programs.",
                     "T-14-1-1         only these", date),
    .autoexec_body(c("ard", "tfl")))
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
#' [run_batch()]'s `data`), with their size, as the study folder has them
#' now: what `run_batch(data = TRUE)` would copy into the batch folder.
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

# the runner and the autoexec programs, written with the study
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
  key <- list(p$ard, p$outputs, p$study, p$sheets$report, p$sheets$codelists, p$fig_designs,
              p$lf, .spec_files_yaml(root), root, Sys.Date(), .code_context$opts)
  if (!identical(.batch_cache$key, key)) {
    .batch_cache$code <- batch_code(p, root = root)
    .batch_cache$key <- key
  }
  put(.batch_cache$code, file.path("programs", .batch_file))
  # the figure style, and the one function a report of one's own code ends
  # with (report_content()), written once here
  put(.fig_setup_code(p), file.path(lay[["programs_tfl"]], .fig_setup_file))
  put(report_setup_code(p), file.path(lay[["programs_tfl"]], .report_setup_file))
  put(autoexec_all_code(), file.path("programs", .autoexec_all_file))
  put(autoexec_code(p), file.path(lay[["programs_tfl"]], "autoexec_report.R"))
  out
}

#' Official runs of a study
#'
#' `run_batch()` runs the study's official-run program -- the ARD
#' (`programs/ard/autoexec_ard.R`), the reports
#' (`programs/tfl/autoexec_report.R`) or both (`programs/autoexec_all.R`)
#' -- which makes a batch folder `runs/<date>_<time>_<what>/` with each
#' program's log, what the run made, the R and package versions
#' (`session_info.txt`, and the study's `renv.lock` when it has one), with
#' `code = TRUE` the programs and definition workbooks it ran, and with
#' `data = TRUE` the data files its programs read.  `list_batches()` lists
#' a study's batch folders.
#'
#' The data kept are the files of the data catalog the run's programs read
#' ([batch_data_plan()]): each ARD program its report's datasets, each
#' report program its own (a listing's, a figure's) or the ARD taken in it
#' reads.  They go to the batch folder's `data/` under the path they have
#' in the study folder (`data/data/adam/adsl.rds`; a file outside it under
#' `data/external/`), listed in `data_manifest.csv` with their size, md5
#' and time.  With the code and the logs, that is everything the run read:
#' a run that can be made again.
#'
#' The programs are the saved ones: save the study first.
#'
#' @param study An `rtfstudy`.
#' @param parts `"ard"`, `"tfl"` or both.
#' @param code Keep the code in the batch folder.
#' @param data Keep the data files the programs read in the batch folder.
#' @param only Programs or outputs to run; `NULL` for all.
#' @param exclude Reports to leave out (their ARD program and their report
#'   program), by output id or program name; `NULL` for none.  The batch
#'   folder's `batch.txt` lists the programs left out.
#' @param batch A named batch ([batch_sets()]): only its reports run, and
#'   the batch folder (`runs/<date>_<time>_<what>_<batch>/`) and its
#'   `batch.txt` name it.  `NULL`: every report (or `only` / `exclude`).
#' @param wait `FALSE` returns the running [processx::process] at once.
#'   The run goes on by itself: closing the app, or R, does not stop it,
#'   and a study runs one official run at a time.
#' @return `run_batch()`: with `wait = TRUE`, a list: `ok`, `batch` (the
#'   batch folder), `result` (its run.csv), `output` (what the run
#'   printed); otherwise the process.  `list_batches()`: a data frame, one
#'   row a batch folder, the newest first, with its `state` -- `"done"`
#'   (every program without error), `"failed"` (`errors` of `programs`),
#'   or `"incomplete"` (no record of its end yet: still running, or
#'   stopped) -- and when it `finished`.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (needs the sample data and rtfreporter; the run is in child R processes)
#' s <- create_sample_study(run = FALSE)
#' id <- s$planner$outputs$output_id[1]  # one report
#' b <- run_batch(s, parts = "tfl", only = id)
#' b$ok
#' list_batches(s)[, c("batch", "state")]
#' }
#' options(old)
#' @export
run_batch <- function(study, parts = c("ard", "tfl"), code = TRUE, data = FALSE,
                      only = NULL, exclude = NULL, batch = NULL, wait = TRUE) {
  # one official run at a time in a study, whoever started it
  live <- .live_run(study$meta$study_id)
  if (!is.null(live)) {
    stop("An official run of ", study$meta$study_id, " is going on (started ",
         live$started, "): wait until it has finished.", call. = FALSE)
  }
  lay <- study_layout()
  parts <- match.arg(parts, c("ard", "tfl"), several.ok = TRUE)
  prog <- if (length(parts) == 2L) file.path("programs", .autoexec_all_file) else
    if (parts == "ard") file.path(lay[["programs_ard"]], .ard_autoexec_file) else
      file.path(lay[["programs_tfl"]], "autoexec_report.R")
  if (!file.exists(file.path(study$path, prog))) {
    stop("The study has no ", prog, " yet: save it first (an ARD definition ",
         "that does not hold writes no ARD program; see the Review tab).",
         call. = FALSE)
  }
  if (length(batch) && !batch %in% names(batch_sets(study$planner))) {
    stop("No batch named ", sQuote(batch), ": the study's are ",
         paste(sQuote(names(batch_sets(study$planner))), collapse = ", "),
         ".", call. = FALSE)
  }
  # leaving reports out, a named batch: the runner that knows how (a study
  # saved by an earlier tflplanner has an older programs/batch.R)
  runner <- readLines(file.path(study$path, "programs", .batch_file), warn = FALSE)
  if ((length(exclude) && !any(grepl("--exclude=", runner, fixed = TRUE))) ||
      (length(batch) && !any(grepl("--batch", runner, fixed = TRUE))) ||
      (isTRUE(data) && !any(grepl("--data", runner, fixed = TRUE)))) {
    stop("programs/batch.R was written by an earlier tflplanner and cannot leave ",
         "reports out, run a named batch or keep the data: save the study to ",
         "write it again.", call. = FALSE)
  }
  before <- list_batches(study)$batch
  out <- tempfile("batch", fileext = ".txt")
  # detached: the run goes on when the app (this R process) ends -- closing
  # the browser, [Close], starting the app again -- and is recorded in the
  # home so an app opened later finds it (live_runs.R)
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c(prog, if (!code) "--no-code", if (isTRUE(data)) "--data", only,
      if (length(exclude)) paste0("--exclude=", .batch_exclude(study$planner, exclude)),
      if (length(batch)) c("--batch", batch)),
    wd = study$path,
    stdout = out, stderr = "2>&1", cleanup = FALSE)
  rec <- tryCatch(.record_run(px, study, paste(parts, collapse = "+"), before),
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

# For each report, the reports whose rows of the study ARD it reads: its own
# when it has analyses, a figure's ARD source's table, and any other report
# with analyses its own code (its steps') names in quotes ("T-14-1-1").  A
# named list: output id -> output ids.
.batch_ard_reads <- function(x) {
  a <- x$ard$analyses
  has <- unique(stats::na.omit(a$output_id))
  ids <- x$outputs$output_id
  code <- vapply(ids, function(id) {
    v <- stats::na.omit(c(step_code(x, id, "data"), step_code(x, id, "content")))
    if (length(v)) paste(v, collapse = "\n") else NA_character_
  }, "", USE.NAMES = FALSE)
  stats::setNames(lapply(seq_along(ids), function(i) {
    q <- if (!is.na(code[i])) {
      unlist(regmatches(code[i], gregexpr('"[^"]+"', code[i])))
    }
    q <- gsub('"', "", q)
    unique(c(if (ids[i] %in% has) ids[i], intersect(setdiff(q, ids[i]), has)))
  }), ids)
}

# The reports of a run that read an ARD the run leaves out: data frame
# report (kept) / needs (left out)
.batch_excluded_needs <- function(x, exclude) {
  reads <- .batch_ard_reads(x)
  keep <- setdiff(names(reads), exclude)
  rows <- lapply(keep, function(k) {
    n <- intersect(reads[[k]], exclude)
    if (length(n)) data.frame(report = k, needs = n, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) data.frame(report = character(), needs = character()) else out
}

#' @rdname run_batch
#' @export
list_batches <- function(study) {
  root <- file.path(study$path, study_layout()[["runs"]])
  dirs <- if (dir.exists(root)) sort(list.dirs(root, full.names = FALSE,
                                               recursive = FALSE),
                                     decreasing = TRUE) else character()
  rows <- lapply(dirs, function(d) {
    csv <- file.path(root, d, "run.csv")
    r <- if (file.exists(csv)) tryCatch(
      utils::read.csv(csv, colClasses = "character"),
      error = function(e) NULL)
    # the programs the run left out (batch.txt's "Excluded :")
    bt <- file.path(root, d, "batch.txt")
    ex <- if (file.exists(bt)) sub("^Excluded :\\s*", "", grep("^Excluded :",
      readLines(bt, warn = FALSE), value = TRUE))
    ex <- if (length(ex) && !identical(ex[1L], "none")) ex[1L] else ""
    m <- regmatches(d, regexec("^([0-9]{8})_([0-9]{6})_(ard|report|all)(?:_(.*))?$",
                               d, perl = TRUE))[[1L]]
    # the named batch: batch.txt's "Batch set :" (the folder carries it too)
    set <- if (file.exists(bt <- file.path(root, d, "batch.txt"))) sub(
      "^Batch set :\\s*", "", grep("^Batch set :", readLines(bt, warn = FALSE),
                                   value = TRUE))
    set <- if (length(set) && !identical(set[1L], "none")) set[1L] else ""
    # its end: batch.txt's "Finished :" (written last); none yet, the run
    # goes on or was stopped
    fin <- if (file.exists(bt)) sub("^Finished :\\s*", "", grep(
      "^Finished :", readLines(bt, warn = FALSE), value = TRUE))
    fin <- if (length(fin)) fin[1L] else NA_character_
    errs <- if (is.null(r)) NA_integer_ else sum(r$status != "OK")
    data.frame(
      batch = d,
      started = if (length(m)) paste(
        format(as.Date(m[2L], "%Y%m%d")),
        paste(substring(m[3L], c(1L, 3L, 5L), c(2L, 4L, 6L)), collapse = ":"))
      else NA_character_,
      what = if (length(m)) m[4L] else NA_character_,
      set = set,
      programs = if (is.null(r)) NA_integer_ else nrow(r),
      errors = errs,
      state = if (is.na(fin) || is.null(r)) "incomplete" else
        if (errs > 0L) "failed" else "done",
      finished = fin,
      code = dir.exists(file.path(root, d, "code")),
      data = dir.exists(file.path(root, d, "data")),
      excluded = ex,
      path = file.path(root, d), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    batch = character(), started = character(), what = character(),
    set = character(), programs = integer(), errors = integer(),
    state = character(), finished = character(), code = logical(),
    data = logical(),
    excluded = character(), path = character())), rows))
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
#' the code, by the md5 of the files of `code/`, where both runs kept them
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
  a <- lb$path[match(against, lb$batch)]
  b <- lb$path[match(batch, lb$batch)]
  row <- function(kind, file, result) {
    data.frame(kind = rep(kind, length(file)), file = file, result = result,
               stringsAsFactors = FALSE)
  }
  # an input: by md5, where both runs kept it
  inputs <- function(kind, md5_of) {
    x <- md5_of(a)
    y <- md5_of(b)
    if (is.null(x) || is.null(y)) return(row(kind, "", "not kept"))
    f <- sort(union(names(x), names(y)))
    res <- vapply(f, function(k) {
      if (!k %in% names(x)) "added" else if (!k %in% names(y)) "removed" else
        if (identical(x[[k]], y[[k]])) "same" else "changed"
    }, "")
    row(kind, f, unname(res))[res != "same", , drop = FALSE]
  }
  data_md5 <- function(dir) {
    f <- file.path(dir, "data_manifest.csv")
    if (!file.exists(f)) return(NULL)
    m <- utils::read.csv(f, colClasses = "character")
    stats::setNames(m$md5, m$path)
  }
  code_md5 <- function(dir) {
    d <- file.path(dir, "code")
    if (!dir.exists(d)) return(NULL)
    f <- list.files(d, recursive = TRUE)
    stats::setNames(unname(tools::md5sum(file.path(d, f))), f)
  }
  outs <- sort(union(list.files(file.path(a, "output"), recursive = TRUE),
                     list.files(file.path(b, "output"), recursive = TRUE)))
  out_res <- vapply(outs, function(f) .batch_compare_file(file.path(a, "output", f),
                                                          file.path(b, "output", f)), "")
  out <- rbind(inputs("data", data_md5), inputs("code", code_md5),
               row("output", file.path("output", outs), unname(out_res)))
  rownames(out) <- NULL
  attr(out, "batches") <- c(against = against, batch = batch)
  attr(out, "changed") <- list(
    inputs = out$file[out$kind != "output" & out$result != "not kept"],
    outputs = out$file[out$kind == "output" &
                         out$result %in% c("different", "not made", "new")])
  out
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

# One result of two runs: the same bytes; else the same but the date and
# time a report prints ({DATETIME}) or a status file records; else (an
# .rds) the same R object; else different
.batch_compare_file <- function(x, y) {
  if (!file.exists(y)) return("not made")
  if (!file.exists(x)) return("new")
  if (identical(unname(tools::md5sum(x)), unname(tools::md5sum(y)))) return("identical")
  if (grepl("[.]rds$", x, ignore.case = TRUE)) {
    same <- tryCatch(identical(readRDS(x), readRDS(y)), error = function(e) FALSE)
    return(if (same) "same object" else "different")
  }
  if (grepl("[.](rtf|csv|txt)$", x, ignore.case = TRUE) &&
      identical(.batch_untimed(x), .batch_untimed(y))) return("same but the time")
  "different"
}

# a text file with its dates and times put aside: 2026-10-12 08:49:26,
# 12Oct2026  08:49 (a report's {DATETIME}), an RTF's \creatim ...
.batch_untimed <- function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  for (p in c("[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?",
              "[0-9]{1,2}[A-Za-z]{3}[0-9]{4} +[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?",
              "\\\\(creatim|revtim|printim)[^}]*")) {
    x <- gsub(p, "<time>", x, perl = TRUE, useBytes = TRUE)
  }
  x
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
