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

#' A report's rows of the ARD definition
#'
#' `ard_rows()` gives the rows of an ARD definition sheet the app shows for
#' a report (`analyses`: that report's; the other sheets are the study's);
#' `set_ard_rows()` puts them back, edited.
#'
#' @param x A `tflplanner`.
#' @param sheet `analyses`, `datasets`, `populations` or `study`.
#' @param output_id A report, or `""` / `NA` for every row.
#' @param rows The rows, edited.
#' @return `ard_rows()`: a data frame; `set_ard_rows()`: the `tflplanner`.
#' @export
ard_rows <- function(x, sheet, output_id = "") {
  d <- x$ard[[sheet]]
  if (sheet != "analyses" || identical(output_id, "") || is.na(output_id)) {
    return(d)
  }
  d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
}

#' @rdname ard_rows
#' @export
set_ard_rows <- function(x, sheet, output_id = "", rows) {
  rows <- .drop_blank_rows(as.data.frame(rows, stringsAsFactors = FALSE))
  whole <- sheet != "analyses" || identical(output_id, "") ||
    is.na(output_id)
  if (!whole) rows$output_id <- rep(output_id, nrow(rows))
  rows <- .normalize_ard_sheet(rows, sheet)
  if (whole) {
    x$ard[[sheet]] <- rows
    return(x)
  }
  d <- x$ard$analyses
  mine <- !is.na(d$output_id) & d$output_id == output_id
  at <- if (any(mine)) which(mine)[1L] - 1L else nrow(d)
  rest <- d[!mine, , drop = FALSE]
  before <- sum(!mine[seq_len(at)])
  d <- rbind(rest[seq_len(before), , drop = FALSE], rows,
             rest[setdiff(seq_len(nrow(rest)), seq_len(before)), ,
                  drop = FALSE])
  rownames(d) <- NULL
  x$ard$analyses <- d
  x
}

# The ARD definition lives in the app (the study's state); saving writes the
# ARD programs from it, and a copy of it for the study folder
# (spec/ard_definition.json: a folder registered elsewhere keeps its ARD
# definition -- not meant to be edited).  ard_spec.xlsx is only an export --
# to edit the definition in Excel, or keep it as a file -- and an import
# (tfl_write_ard_spec(), tfl_read_ard_spec()).
.ard_json <- "ard_definition.json"

.save_ard <- function(p, root) {
  a <- p$ard %||% .empty_ard_spec()
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  a <- stats::setNames(lapply(names(.ard_sheets()), function(s)
    .normalize_ard_sheet(a[[s]], s)), names(.ard_sheets()))
  j <- file.path(root, study_layout()[["spec"]], .ard_json)
  if (nrow(a$analyses) || nrow(a$datasets) || nrow(a$populations) ||
      file.exists(j)) {
    txt <- jsonlite::toJSON(a, dataframe = "columns", na = "null",
                            pretty = TRUE, auto_unbox = FALSE)
    old <- if (file.exists(j)) paste(readLines(j, warn = FALSE,
                                               encoding = "UTF-8"),
                                     collapse = "\n")
    same <- identical(old, as.character(txt))
    if (!same) writeLines(enc2utf8(as.character(txt)), j, useBytes = TRUE)
    out[nrow(out) + 1L, ] <- list(j, if (same) "unchanged" else "written")
  }
  spec <- tryCatch(.ard_spec(a), error = function(e) NULL)
  if (!is.null(spec) && nrow(a$analyses)) {
    out <- rbind(out, .save_ard_programs(spec, root))
  }
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
#' @export
run_ard <- function(study, output_id = NULL, timeout = 600) {
  spec <- .ard_spec(study$planner$ard)
  code <- .ard_spec_code(spec, output_id = output_id,
                        save = is.null(output_id))
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

# One output's rows of the study's working ARD (NULL when there are none).
study_ard_rows <- function(study, output_id) {
  out <- .ard_study_value(study$planner$ard, "output", "output/ard/ard.rds")
  f <- file.path(study$path, out)
  if (!file.exists(f)) return(NULL)
  a <- readRDS(f)
  if (!"output_id" %in% names(a)) return(NULL)
  a[a$output_id %in% output_id, , drop = FALSE]
}

.ard_status_file <- function(study) {
  out <- .ard_study_value(study$planner$ard, "output", "output/ard/ard.rds")
  file.path(study$path, dirname(out), "ard_status.csv")
}

.read_ard_status <- function(study) {
  f <- .ard_status_file(study)
  empty <- data.frame(output_id = character(), definition = character(),
                      built = character(), rows = integer(),
                      error = character(), stringsAsFactors = FALSE)
  if (!file.exists(f)) return(empty)
  d <- utils::read.csv(f, colClasses = "character")
  for (c in names(empty)) if (!c %in% names(d)) d[[c]] <- NA_character_
  d$rows <- as.integer(d$rows)
  d[names(empty)]
}

.write_ard_status <- function(study, d) {
  f <- .ard_status_file(study)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(f, ".tmp")
  utils::write.csv(d, tmp, row.names = FALSE)
  file.rename(tmp, f)
}

#' Where each output's ARD stands
#'
#' For every output the ARD definition has analyses for: `built` (its rows
#' are in the study ARD, made from the definition as it is now), `outdated`
#' (made from an earlier definition), `not built`, or `error` (its last
#' update failed).  Many people may work on one study: the study ARD is
#' updated output by output ([update_study_ard()]), and a table is made
#' from whatever of it is there.
#'
#' @param study An `rtfstudy`.
#' @return A data frame: `output_id`, `analyses`, `state`, `rows`,
#'   `built`, `error`.
#' @export
ard_status <- function(study) {
  a <- study$planner$ard
  ids <- unique(stats::na.omit(a$analyses$output_id))
  st <- .read_ard_status(study)
  spec <- structure(a, class = "tfl_ard_spec")
  rows <- lapply(ids, function(id) {
    r <- st[st$output_id == id, , drop = FALSE][1L, ]
    now <- tflspec::tfl_ard_spec_hash(spec, id)
    state <- if (is.na(r$output_id)) "not built" else
      if (!is.na(r$error) && nzchar(r$error)) "error" else
        if (!identical(r$definition, now)) "outdated" else "built"
    data.frame(output_id = id, analyses = sum(a$analyses$output_id %in% id),
               state = state, rows = r$rows, built = r$built,
               error = if (is.na(r$error)) "" else r$error,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    output_id = character(), analyses = integer(), state = character(),
    rows = integer(), built = character(), error = character())), rows))
  rownames(out) <- NULL
  out
}
