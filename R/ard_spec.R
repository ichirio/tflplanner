# The study's ARD definition, as the app keeps it.
#
# The engine -- the definition checked, turned into cards / cardx code, and
# run -- is tflspec's (tfl_ard_spec(), tfl_ard_code(), tfl_build_ard(); the
# workbook ard_spec.xlsx is tfl_read_ard_spec() / tfl_write_ard_spec()).  What is
# here belongs to a study: editing its rows, saving its definition and
# programs in the study folder, running them there, and where each output's
# ARD stands.  The catalogs the engine checks against are the company
# standards' (sheets ard_methods and ard_statistics); every call to the
# engine passes them.

# the sheets of an ARD definition and their columns (tflspec's)
.ard_sheets <- function() lapply(tflspec::tfl_ard_spec_template(), names)

# the company standards' catalogs, as the engine and the app read them
.std_ard_methods <- function() {
  m <- company_standards()$ard_methods
  m[] <- lapply(m, function(v) ifelse(is.na(v), "", v))
  m
}

# The keyword a method stands for: its own name, or the keyword whose
# function it names (`cards::ard_tabulate` -> `categorical`; tflspec makes
# the same analysis of either).  Other methods as they are.
.method_kw <- function(m) {
  keys <- .std_ard_methods()
  j <- match(m, keys$call)
  ifelse(!m %in% keys$method & !is.na(j), keys$method[j], m)
}

# The keywords the ARD tab offers as the company's: those it added or
# changed.  tflspec's own, as built in, are offered as the cards / cardx
# functions they call (the 2026-10-05 decision); a row that names one keeps
# it.  Subjects and custom code (no function to name) are always there.
.company_keywords <- function(current = NA_character_) {
  m <- .std_ard_methods()
  b <- tflspec::tfl_ard_methods()
  cols <- intersect(c("method", "call", "kind", "defaults", "statistics",
                      "formats"), intersect(names(m), names(b)))
  flat <- function(d) do.call(paste, c(lapply(d[cols], function(v)
    ifelse(is.na(v), "", as.character(v))), sep = "\r"))
  keep <- !flat(m) %in% flat(b) | startsWith(m$call, "(") |
    m$method %in% current
  m[keep, , drop = FALSE]
}

.std_ard_statistics <- function(kind = NULL) {
  d <- company_standards()$ard_statistics
  if (!is.null(kind)) d <- d[d$kind %in% kind, , drop = FALSE]
  rownames(d) <- NULL
  d
}

# the engine, with the company standards' catalogs
.ard_spec <- function(x) {
  tflspec::tfl_ard_spec(x, statistics = .std_ard_statistics(),
                    methods = .std_ard_methods())
}

.read_ard_spec <- function(path, check = TRUE) {
  tflspec::tfl_read_ard_spec(path, check = check,
                         statistics = .std_ard_statistics(),
                         methods = .std_ard_methods())
}

.write_ard_spec <- function(spec, path) {
  tflspec::tfl_write_ard_spec(spec, path, statistics = .std_ard_statistics(),
                          methods = .std_ard_methods())
}

.ard_spec_code <- function(spec, ...) {
  tflspec::tfl_ard_code(spec, ..., statistics = .std_ard_statistics(),
                         methods = .std_ard_methods())
}

# The reports' code lists: a report's ARD program makes each listed column
# its analyses read a factor in their order before the analyses, so the ARD
# keeps the order and counts a value no record has (0) (tflspec picks a
# report's rows by output_id).  The codelists sheet's rows (an old workbook's
# rows with no report are left out): a report's own lists only.  The study's
# code lists (defaults.yml codelists:) are study$codelists, which a report
# names where it uses one, and the ADaM levels come from study$adam_meta
# (tflspec#857): no layer gives a report a list.  NULL when there are none.
.study_codelists <- function(p) {
  cl <- if (!is.null(p$sheets$codelists)) sheet_rows(p, "codelists", "")
  cl <- .codelists_of_reports(cl)
  if (is.null(cl) || !nrow(cl)) return(NULL)
  cl <- cl[!is.na(cl$variable) & !is.na(cl$value), , drop = FALSE]
  if (nrow(cl)) cl
}

# The app's own reading of a row, for its forms (the engine reads them the
# same way: tflspec's .split_bar / .parse_formats / .fmt_ok / .stat_kinds).
.split_bar <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
}

# What an analysis computes when it names no statistics (cards' defaults)
.method_default_stats <- function(kinds) {
  switch(kinds %||% "",
         continuous = c("N", "mean", "sd", "median", "p25", "p75", "min", "max"),
         categorical = c("n", "N", "p"),
         missing = c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"),
         character())
}

.stat_kinds <- function(kind) {
  switch(kind %||% "",
         continuous = "continuous", categorical = "categorical",
         missing = "missing", "result")
}

.fmt_ok <- function(f) grepl("^(x+(\\.x+)?%?|[0-9]+|pvalue)$", f)

.parse_formats <- function(x) {
  p <- .split_bar(x)
  if (!length(p)) return(character())
  k <- trimws(sub("=.*$", "", p))
  v <- trimws(sub("^[^=]*=", "", p))
  v[!grepl("=", p, fixed = TRUE)] <- NA
  stats::setNames(v, k)
}

.study_value <- function(x, key, default) {
  v <- x$study$value[match(key, x$study$key)]
  if (length(v) && !is.na(v)) v else default
}

.ard_file <- "ard_spec.xlsx"

.empty_ard_spec <- function() tflspec::tfl_ard_spec_template()

.normalize_ard_sheet <- function(d, sheet) {
  cols <- .ard_sheets()[[sheet]]
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  # (a sheet of open columns -- conditions: a column a param; ards: a call's
  # keys, tflspec#827 -- keeps its own after the fixed ones)
  if (sheet %in% .ard_open_sheets) cols <- c(cols, setdiff(names(d), cols))
  out <- lapply(cols, function(c) {
    v <- if (c %in% names(d)) as.character(d[[c]]) else
      rep(NA_character_, nrow(d))
    v <- trimws(v)
    v[!is.na(v) & !nzchar(v)] <- NA
    v
  })
  out <- as.data.frame(stats::setNames(out, cols), stringsAsFactors = FALSE)
  out <- out[rowSums(!is.na(out)) > 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.ard_study_value <- function(a, key, default) {
  v <- a$study$value[match(key, a$study$key)]
  if (length(v) && !is.na(v)) v else default
}

# Where the study ARD is, relative to the study folder: the ARD
# definition's `output`, else output/ard/ard.rds
.study_ard_rel <- function(a) .ard_study_value(a, "output", "output/ard/ard.rds")

# The one resolver of the study ARD's file: in the study's working output
# (`root` the study folder), or in a batch folder -- an official run made it
# at the same relative path
.study_ard_file <- function(study, root = study$path) {
  file.path(root, .study_ard_rel(study$planner$ard))
}

# The ARD sheets whose columns are not fixed: after output_id, analysis_id
# and row, conditions has a column a param and ards a call's keys
# (tflspec#827)
.ard_open_sheets <- c("conditions", "ards")

# a sheet's rows with the columns given, in their order (NA where it had none)
.ard_with_cols <- function(d, cols) {
  for (k in setdiff(cols, names(d))) d[[k]] <- rep(NA_character_, nrow(d))
  d[, cols, drop = FALSE]
}

# a sheet's own help (tflspec's, English; "" for none)
.ard_sheet_help <- function(sheet) {
  d <- tryCatch(tflspec::tfl_spec_columns(sheet), error = function(e) NULL)
  h <- if (is.data.frame(d)) d$description[is.na(d$column)] else character()
  if (length(h)) h[[1L]] else ""
}

# The ARD sheets whose rows are a report's (output_id)
.ard_report_sheets <- c("analyses", "conditions", "ards")

#' A report's rows of the ARD definition
#'
#' `ard_rows()` gives the rows of an ARD definition sheet the app shows for
#' a report (`analyses`, `conditions`, `ards`: that report's; the other
#' sheets are the study's);
#' `set_ard_rows()` puts them back, edited.
#'
#' @param x A `tflplanner`.
#' @param sheet `analyses`, `conditions`, `ards`, `datasets`, `populations`
#'   or `study`.
#' @param output_id A report, or `""` / `NA` for every row.
#' @param rows The rows, edited.
#' @return `ard_rows()`: a data frame; `set_ard_rows()`: the `tflplanner`.
#' @examples
#' x <- new_planner()
#' ard_rows(x, "datasets")  # the datasets of the ARD definition (none yet)
#' ard_rows(x, "analyses", "T-1")  # a report's analyses
#' @export
ard_rows <- function(x, sheet, output_id = "") {
  d <- x$ard[[sheet]]
  if (!sheet %in% .ard_report_sheets || identical(output_id, "") || is.na(output_id)) {
    return(d)
  }
  d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
}

#' @rdname ard_rows
#' @export
set_ard_rows <- function(x, sheet, output_id = "", rows) {
  rows <- .drop_blank_rows(as.data.frame(rows, stringsAsFactors = FALSE))
  whole <- !sheet %in% c(.ard_report_sheets, "analysis_data") || identical(output_id, "") ||
    is.na(output_id)
  if (!whole) rows$output_id <- rep(output_id, nrow(rows))
  rows <- .normalize_ard_sheet(rows, sheet)
  if (whole) {
    x$ard[[sheet]] <- rows
    return(x)
  }
  d <- x$ard[[sheet]]
  # (a sheet of open columns: the report's and the others' columns together,
  # blank where a row has none)
  if (sheet %in% .ard_open_sheets) {
    cols <- union(names(d), names(rows))
    d <- .ard_with_cols(d, cols)
    rows <- .ard_with_cols(rows, cols)
  }
  mine <- !is.na(d$output_id) & d$output_id == output_id
  at <- if (any(mine)) which(mine)[1L] - 1L else nrow(d)
  rest <- d[!mine, , drop = FALSE]
  before <- sum(!mine[seq_len(at)])
  d <- rbind(rest[seq_len(before), , drop = FALSE], rows,
             rest[setdiff(seq_len(nrow(rest)), seq_len(before)), ,
                  drop = FALSE])
  rownames(d) <- NULL
  x$ard[[sheet]] <- d
  x
}

# The ARD definition lives in the app (the study's state); saving writes the
# ARD programs from it, and a copy of it for the study folder
# (spec/ard_definition.json: a folder registered elsewhere keeps its ARD
# definition -- not meant to be edited).  ard_spec.xlsx is only an export --
# to edit the definition in Excel, or keep it as a file -- and an import
# (tfl_write_ard_spec(), tfl_read_ard_spec()).
.ard_json <- "ard_definition.json"

.save_ard <- function(p, root, json = TRUE) {
  a <- p$ard %||% .empty_ard_spec()
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  a <- stats::setNames(lapply(names(.ard_sheets()), function(s)
    .normalize_ard_sheet(a[[s]], s)), names(.ard_sheets()))
  j <- file.path(root, study_layout()[["spec"]], .ard_json)
  # (a study of the YAML spec keeps its ARD definition there: no JSON)
  if (json && (nrow(a$analyses) || nrow(a$datasets) || nrow(a$populations) ||
      nrow(a$analysis_data) || file.exists(j))) {
    txt <- jsonlite::toJSON(a, dataframe = "columns", na = "null",
                            pretty = TRUE, auto_unbox = FALSE)
    old <- if (file.exists(j)) paste(readLines(j, warn = FALSE,
                                               encoding = "UTF-8"),
                                     collapse = "\n")
    same <- identical(old, as.character(txt))
    if (!same) writeLines(enc2utf8(as.character(txt)), j, useBytes = TRUE)
    out[nrow(out) + 1L, ] <- list(j, if (same) "unchanged" else "written")
  }
  # a definition that does not hold writes no ARD program: said, not
  # passed over (#323) -- the rest of the save is still worth keeping
  problem <- NULL
  spec <- tryCatch(.ard_spec(a), error = function(e) {
    problem <<- conditionMessage(e)
    NULL
  })
  if (!is.null(spec) && nrow(a$analyses)) {
    out <- rbind(out, .save_ard_programs(p, spec, root, .study_codelists(p)))
  }
  if (nrow(a$analyses)) attr(out, "problem") <- problem
  out
}

# the copy in the study folder, read back
.read_ard_json <- function(f) {
  x <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  stats::setNames(lapply(names(.ard_sheets()), function(s) {
    d <- x[[s]]
    d <- if (is.null(d) || !length(d)) data.frame() else
      as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
    .normalize_ard_sheet(d, s)
  }), names(.ard_sheets()))
}

#' Run a report's analyses, or the whole study's
#'
#' Runs [tflspec::tfl_ard_code()] in its own R process from the study folder and
#' reads back the ARD it makes.
#'
#' @param study An `rtfstudy`.
#' @param output_id A report, or `NULL` for the study (saved where the
#'   definition's `output` says).
#' @param timeout Seconds to allow.
#' @return A list: `ard` (or `NULL`), `error`, `log`, `seconds`, `code`.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it runs in its own R process)
#' s <- create_sample_study(run = FALSE)
#' r <- run_ard(s, "T-14-1-2")
#' r$seconds
#' head(ard_view(r$ard), 3)
#' }
#' options(old)
#' @export
run_ard <- function(study, output_id = NULL, timeout = 600) {
  # (a definition tflspec's check refuses -- a function the installed
  # package has not, say: said as the run's error, nothing run)
  spec <- tryCatch(.ard_spec(study$planner$ard), tflspec_spec_error = function(e) e)
  if (inherits(spec, "error")) {
    return(list(ard = NULL, error = conditionMessage(spec), log = "", seconds = 0, code = character()))
  }
  code <- .ard_spec_code(spec, output_id = output_id,
                        save = is.null(output_id), dir = study$path,
                        codelists = .study_codelists(study$planner))
  tmp <- tempfile("ard")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f <- file.path(tmp, "make_ard.R")
  res <- file.path(tmp, "ard.rds")
  writeLines(enc2utf8(c(code, sprintf("saveRDS(ard, %s)", encodeString(
    normalizePath(res, "/", FALSE), quote = "\"")))), f, useBytes = TRUE)
  t0 <- Sys.time()
  px <- processx::run(file.path(R.home("bin"), "Rscript"), f,
                      wd = study$path, error_on_status = FALSE,
                      timeout = timeout, stderr_to_stdout = TRUE)
  ok <- file.exists(res)
  list(ard = if (ok) readRDS(res),
       error = if (!ok) paste(utils::tail(strsplit(px$stdout, "\n")[[1L]],
                                         15L), collapse = "\n"),
       log = px$stdout,
       seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")),
                       1),
       code = code)
}

#' An ARD as a plain table to read
#'
#' @param ard An ARD.
#' @return A data frame of text: the ids, the groups, the variable and its
#'   level, the context, the statistic's name, label and value.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' s <- create_sample_study(run = FALSE)
#' s <- save_study(s)
#' r <- run_ard(s, "T-14-1-2")
#' head(ard_view(r$ard), 3)  # the ARD as a plain table of text
#' }
#' options(old)
#' @export
ard_view <- function(ard) {
  if (is.null(ard) || !nrow(ard)) return(data.frame())
  flat <- function(v) {
    if (!is.list(v)) return(as.character(v))
    vapply(v, function(e) {
      if (is.null(e) || !length(e)) return("")
      if (is.numeric(e)) return(paste(format(e, digits = 6), collapse = ", "))
      paste(format(e), collapse = ", ")
    }, "")
  }
  keep <- c("output_id", "analysis_id",
            grep("^group[0-9]+(_level)?$", names(ard), value = TRUE),
            "variable", "variable_level", "context", "stat_name",
            "stat_label", "stat", "stat_fmt")
  keep <- intersect(keep, names(ard))
  out <- as.data.frame(lapply(ard[keep], flat), stringsAsFactors = FALSE)
  out[is.na(out)] <- ""
  out
}

# PROTOTYPE (tflspec#1032): the outputs' own ARD (NULL when there is
# none), from each report's own file output/ard/report/<id>.rds -- the
# working one (what previews and hand runs left) while it was made from the
# definition as it is now; else, read only, the latest official run's file
# of that output when it was; else (`current = FALSE`) the working rows as
# they are.  `current = TRUE`: only rows made from the definition now.
.report_ard_rows <- function(study, output_id, current = FALSE) {
  got <- lapply(unique(output_id), function(id) {
    now <- .ard_hash_now(study, id)
    work <- .read_report_ard(study$path, id)
    if (!is.null(work) && identical(attr(work, "made")$definition, now)) return(work)
    ok <- .latest_ok(study, id, c("ard", "tfl"))
    run <- if (!is.na(ok$path[1L])) .read_report_ard(ok$path[1L], id, slot = "run")
    if (!is.null(run) && identical(attr(run, "made")$definition, now)) return(run)
    if (!current) work
  })
  got <- lapply(Filter(Negate(is.null), got), function(a) {
    attr(a, "made") <- NULL
    a
  })
  if (length(got)) do.call(rbind, got)
}

# a report's own ARD file under `root` (the study, or a batch folder), read
# once per file (NULL: none)
.read_report_ard <- function(root, id, slot = "ard") {
  f <- file.path(root, "output", "ard", "report",
                 paste0(gsub("[^A-Za-z0-9._-]", "_", id), ".rds"))
  a <- .read_study_ard(f, slot = paste(slot, id))
  if (!is.null(a) && !"output_id" %in% names(a)) a$output_id <- id
  a
}

# The fingerprint of an output's ARD definition as it is now
.ard_hash_now <- function(study, id) {
  tryCatch(tflspec::tfl_ard_spec_hash(structure(study$planner$ard, class = "tfl_ard_spec"), id,
                                      dir = study$path,
                                      codelists = .study_codelists(study$planner)),
           error = function(e) NA_character_)
}

# The study ARD read once per file: every report asks for its rows as a
# study opens (the report list, the review, the figures), and the file is
# the same.  Its path, time and size are the key: a run writes a new file,
# and it is read again.  One file is kept (the study shown), and one more
# for a run's ARD (`slot = "run"`), so reading that does not push the
# working ARD out.
.ard_memo <- new.env(parent = emptyenv())
.read_study_ard <- function(f, slot = "ard") {
  if (!file.exists(f)) return(NULL)
  i <- file.info(f)
  key <- paste(normalizePath(f, "/"), as.numeric(i$mtime), i$size)
  hit <- .ard_memo[[slot]]
  if (!is.null(hit) && identical(hit$key, key)) return(hit$ard)
  a <- readRDS(f)
  .ard_memo[[slot]] <- list(key = key, ard = a)
  a
}

# the status file beside the ARD (in a batch folder when `root` is its)
.ard_status_file <- function(study, root = study$path) {
  file.path(dirname(.study_ard_file(study, root)), "ard_status.csv")
}

.ard_status_empty <- function() {
  data.frame(output_id = character(), definition = character(),
             built = character(), rows = integer(),
             error = character(), setup = character(),
             company = character(), user = character(),
             stringsAsFactors = FALSE)
}

.read_ard_status <- function(study, root = study$path) {
  f <- .ard_status_file(study, root)
  empty <- .ard_status_empty()
  if (!file.exists(f)) return(empty)
  d <- utils::read.csv(f, colClasses = "character")
  for (c in names(empty)) if (!c %in% names(d)) d[[c]] <- NA_character_
  d$rows <- as.integer(d$rows)
  d[names(empty)]
}

# One output's state from its row of ard_status.csv (an NA row: none) and the
# definition's fingerprint now
.ard_row_state <- function(study, r, now) {
  if (is.na(r$output_id)) "not built" else
    if (!is.na(r$error) && nzchar(r$error)) "error" else
      if (!identical(r$definition, now) ||
          .setup_changed(.ard_recorded_setup(r), study$path)) "outdated" else "built"
}

#' Where each output's ARD stands
#'
#' For every output the ARD definition has analyses for: `built` (its rows
#' are in the study ARD, made from the definition as it is now), `outdated`
#' (made from an earlier definition, or with another
#' `programs/study_setup.R` than the one there now), `not built`, or
#' `error` (its last
#' update failed).  Many people may work on one study: the study ARD is
#' updated output by output (a preview, [run_study()]), and a table is made
#' from whatever of it is there.
#'
#' There are two places to look.  `from = "working"` reads the study's
#' working ARD (`output/ard/`: what a preview or a hand run of a program
#' left), the design aids' view.  `from = "run"` reads the official runs
#' (`runs/`): each output's row is the `ard_status.csv` of the latest run
#' that made its ARD without error (`not built` where there is none), and
#' it is `error` when a later run's ARD program for it failed.  The
#' `outdated` / `built` test is the same.
#'
#' @param study An `rtfstudy`.
#' @param from `"working"` (the default) or `"run"`: where the ARD is read
#'   from, see Details.
#' @return A data frame: `output_id`, `analyses`, `state`, `rows`,
#'   `built`, `error`.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' s <- create_sample_study(run = FALSE)
#' head(ard_status(s)[, c("output_id", "analyses", "state")], 3)  # not built yet
#' }
#' options(old)
#' @export
ard_status <- function(study, from = c("working", "run")) {
  from <- match.arg(from)
  a <- study$planner$ard
  ids <- unique(stats::na.omit(a$analyses$output_id))
  spec <- structure(a, class = "tfl_ard_spec")
  cl <- .study_codelists(study$planner)
  if (identical(from, "working")) {
    st <- .read_ard_status(study)
  } else {
    idx <- .run_index(study)
    ok <- .latest_ok(study, ids, c("ard", "tfl"), idx)
    att <- .latest_attempt(study, ids, c("ard", "tfl"), idx)
    seen <- list()
  }
  rows <- lapply(seq_along(ids), function(i) {
    id <- ids[i]
    failed <- FALSE
    if (identical(from, "working")) {
      r <- st[st$output_id == id, , drop = FALSE][1L, ]
    } else {
      pth <- ok$path[i]
      r <- if (is.na(pth)) .ard_status_empty()[1L, ] else {
        if (is.null(seen[[pth]])) seen[[pth]] <<- .read_ard_status(study, root = pth)
        seen[[pth]][seen[[pth]]$output_id == id, , drop = FALSE][1L, ]
      }
      failed <- .attempt_failed(att[i, ], ok[i, ])
    }
    now <- tflspec::tfl_ard_spec_hash(spec, id, dir = study$path, codelists = cl)
    state <- if (failed) "error" else .ard_row_state(study, r, now)
    err <- if (failed) {
      if (!is.na(att$note[i]) && nzchar(att$note[i])) att$note[i] else "failed"
    } else if (is.na(r$error)) "" else r$error
    data.frame(output_id = id, analyses = sum(a$analyses$output_id %in% id),
               state = state, rows = r$rows, built = r$built,
               error = err, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    output_id = character(), analyses = integer(), state = character(),
    rows = integer(), built = character(), error = character())), rows))
  rownames(out) <- NULL
  out
}
