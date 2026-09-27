# EXPERIMENTAL: the study's ARD from a workbook (ard_spec.xlsx).
#
# A study's analyses are rows: which data, which population, which subset,
# grouped by what, which variables, which method and statistics.  Each row
# becomes one cards / cardx call; every result is tagged with the ids that
# trace it -- output_id, analysis_id, population_id -- and all of them are
# bound into ONE ARD for the study (output/ard/ard.rds).
#
# The workbook is turned into R code (ard_spec_code()) and the ARD is made by
# running that code (build_ard()), so the code a programmer can read, keep
# and rerun is exactly what made the ARD.
#
#   study        key / value: id (the subject key, USUBJID), output (where
#                the ARD goes)
#   datasets     dataset, path, derive           the analysis data
#   populations  population_id, dataset, where,  analysis sets (and the
#                derive                          denominators)
#   analyses     output_id, analysis_id, method, dataset, population_id,
#                where, by, variables, statistics, formats, args, code
#
# The concepts are those of CDISC ARS (analysis set, data subset, grouping,
# method), so an ard_spec can later be written as ARS metadata.

.ard_spec_sheets <- list(
  study = c("key", "value"),
  datasets = c("dataset", "level", "path", "derive"),
  populations = c("population_id", "dataset", "where", "derive"),
  analyses = c("output_id", "analysis_id", "label", "method", "dataset",
               "population_id", "where", "by", "variables", "statistics",
               "formats", "args", "code"))

#' The methods an analysis row may name
#'
#' An analysis's `method` is one of these keywords, or the name of any
#' function, `pkg::fun` (every `cards::ard_*` and `cardx::ard_*` among
#' them), which is called as `pkg::fun(data, by = , variables = , ...)` with
#' the analysis data first, `by` and `variables` when the row gives them,
#' and the row's `args` after them.  A function whose first argument is not
#' the data (`cardx::ard_survival_survdiff(formula, data)`) takes it the same
#' way once `args` names the first one (`formula = ...`).
#'
#' In `args` and `code`, `data` is the analysis data and `population` the
#' population's subjects.
#'
#' The keywords are the company standards' (sheet `ard_methods`, see
#' [company_standards()]): each names its function (`(subjects)` and
#' `(code)` are the two built into the engine), its `kind` -- how its
#' `statistics` are passed: `continuous`, `categorical` or `none` -- the
#' arguments it gets unless `args` gives them (`<id>` stands for the
#' subject key) and its default statistics.
#'
#' @return A data frame: `method`, `call`, `kind`, `defaults`, `statistics`,
#'   `note`.
#' @export
ard_methods <- function() {
  m <- company_standards()$ard_methods
  m[] <- lapply(m, function(v) ifelse(is.na(v), "", v))
  m
}

.split_bar <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
}

# The call an analysis row stands for, with `data` and `population` bound.
.analysis_body <- function(r, keys, subj, has) {
  m <- r$method
  k <- match(m, keys$method)
  fn <- if (is.na(k)) m else keys$call[k]
  kind <- if (is.na(k)) "" else keys$kind[k]
  stats <- if (!is.na(r$statistics)) r$statistics else if (!is.na(k) &&
    nzchar(keys$statistics[k])) keys$statistics[k] else NA
  if (identical(fn, "(code)")) return(r$code)
  by <- .vars(r$by)
  vars <- .vars(r$variables)
  if (identical(fn, "(subjects)")) {
    # a subject-level flag: has the subject any record of the data?
    flag <- if (length(.split_bar(r$variables))) .split_bar(r$variables)[1L] else
      make.names(r$analysis_id)
    st <- if (!has("statistic")) .stat_arg("categorical", stats)
    return(paste0(
      sprintf("population$%s <- population$%s %%in%% data$%s\n", flag, subj,
              subj),
      sprintf("cards::ard_dichotomous(population%s, variables = %s, value = list(%s = TRUE)%s%s)",
              if (!is.null(by)) paste0(", by = ", by) else "", flag, flag,
              if (!is.null(st)) paste0(", ", st) else "",
              if (!is.na(r$args)) paste0(", ", r$args) else "")))
  }
  # the keyword's own arguments, each unless the row's args gives it
  dflt <- if (!is.na(k) && nzchar(keys$defaults[k])) {
    d <- trimws(strsplit(gsub("<id>", subj, keys$defaults[k], fixed = TRUE),
                         ",")[[1L]])
    d[!vapply(sub("\\s*=.*$", "", d), has, NA)]
  }
  args <- c(
    if (!is.null(by)) paste("by =", by),
    if (!identical(fn, "cards::ard_total_n") && !is.null(vars))
      paste("variables =", vars),
    if (!has("statistic")) .stat_arg(kind, stats),
    dflt,
    if (!is.na(r$args)) r$args)
  sprintf("%s(%s)", fn, paste(c("data", args), collapse = ",\n    "))
}

.ard_file <- "ard_spec.xlsx"

.empty_ard_spec <- function() {
  out <- lapply(.ard_spec_sheets, function(cols)
    as.data.frame(stats::setNames(replicate(length(cols), character(),
                                            simplify = FALSE), cols),
                  stringsAsFactors = FALSE))
  out$study <- data.frame(key = c("id", "output"),
                          value = c("USUBJID", "output/ard/ard.rds"),
                          stringsAsFactors = FALSE)
  out
}

.normalize_ard_sheet <- function(d, sheet) {
  cols <- .ard_spec_sheets[[sheet]]
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

# the workbook and the code, written with the study when it has analyses
.save_ard <- function(p, root) {
  a <- p$ard %||% .empty_ard_spec()
  f <- file.path(root, study_layout()[["spec"]], .ard_file)
  g <- file.path(root, study_layout()[["programs"]], "make_ard.R")
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  # the data catalog is the study's too (listings and figures read it)
  if (!nrow(a$analyses) && !nrow(a$datasets) && !nrow(a$populations) &&
      !file.exists(f)) return(out)
  norm <- function(z) stats::setNames(lapply(names(.ard_spec_sheets), function(s)
    .normalize_ard_sheet(z[[s]], s)), names(.ard_spec_sheets))
  a <- norm(a)
  old <- if (file.exists(f)) tryCatch(norm(read_ard_spec(f, check = FALSE)),
                                      error = function(e) NULL)
  if (!identical(old, a)) {
    writexl::write_xlsx(c(list(`_README` = .ard_readme()), a,
                          list(`_methods` = ard_methods(),
                               `_statistics` = ard_statistics())), f)
    out[nrow(out) + 1L, ] <- list(f, "written")
  } else {
    out[nrow(out) + 1L, ] <- list(f, "unchanged")
  }
  code <- tryCatch(ard_spec_code(ard_spec(a)), error = function(e) NULL)
  if (!is.null(code) && nrow(a$analyses)) {
    have <- if (file.exists(g)) readLines(g, warn = FALSE, encoding = "UTF-8")
    same <- !is.null(have) &&
      identical(have[!grepl("^# Generated by", have)],
                code[!grepl("^# Generated by", code)])
    if (!same) writeLines(enc2utf8(code), g, useBytes = TRUE)
    out[nrow(out) + 1L, ] <- list(g, if (same) "unchanged" else "written")
  }
  out
}

.ard_readme <- function() {
  data.frame(
    sheet = c("study", "datasets", "datasets", "populations", "populations",
              "analyses", "analyses", "analyses", "analyses", "analyses",
              "analyses"),
    column = c("key / value", "dataset / path", "derive",
               "population_id / dataset / where", "derive",
               "output_id / analysis_id", "method",
               "dataset / population_id / where",
               "by / variables / statistics", "formats", "args / code"),
    description = c(
      "id: the subject key (USUBJID); output: where the study ARD goes",
      "a name for the data, and its file relative to the study folder",
      "new columns, NAME = R expression, | between them",
      "an analysis set: the subjects of `dataset` for which `where` (R) holds",
      "columns added to the population (TRTA = TRT01A ...)",
      "the report the analysis serves, and its id; both are ARD columns",
      "a keyword (sheet _methods) or any pkg::function (cards::, cardx::)",
      "the analysis data: `dataset` restricted to the population and `where`",
      "grouping and analysis variables, statistics (sheet _statistics); | between several",
      "stat_fmt formats: statistic=format, | between them (mean=xx.x | p=xx.x% | AGE:sd=xx.xx); blank = the default of the statistic",
      "more arguments as R; `code` for custom (data, population are bound)"),
    stringsAsFactors = FALSE)
}

#' Run a report's analyses, or the whole study's
#'
#' Runs [ard_spec_code()] in its own R process from the study folder and
#' reads back the ARD it makes.
#'
#' @param study An `rtfstudy`.
#' @param output_id A report, or `NULL` for the study (saved where the
#'   definition's `output` says).
#' @param timeout Seconds to allow.
#' @return A list: `ard` (or `NULL`), `error`, `log`, `seconds`, `code`.
#' @export
run_ard <- function(study, output_id = NULL, timeout = 600) {
  spec <- ard_spec(study$planner$ard)
  code <- ard_spec_code(spec, output_id = output_id,
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

#' Read and check an ARD definition workbook
#'
#' @param path An `ard_spec.xlsx`.
#' @param check `FALSE` reads a definition still being written without
#'   refusing it.
#' @return An `ard_spec`: a list of the four sheets.
#' @export
read_ard_spec <- function(path, check = TRUE) {
  sheets <- readxl::excel_sheets(path)
  out <- lapply(names(.ard_spec_sheets), function(s) {
    cols <- .ard_spec_sheets[[s]]
    d <- if (s %in% sheets) .read_sheet_text(path, s) else
      data.frame(matrix(character(), 0, length(cols),
                        dimnames = list(NULL, cols)))
    bad <- setdiff(names(d), c(cols, "note"))
    if (length(bad)) {
      stop("Sheet `", s, "` has columns it does not read: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
    for (c in cols) if (!c %in% names(d)) d[[c]] <- NA_character_
    d <- d[cols]
    d[] <- lapply(d, function(v) {
      v <- trimws(as.character(v))
      v[!is.na(v) & !nzchar(v)] <- NA
      v
    })
    d[rowSums(!is.na(d)) > 0, , drop = FALSE]
  })
  names(out) <- names(.ard_spec_sheets)
  if (check) ard_spec(out) else structure(out, class = "ard_spec")
}

#' @rdname read_ard_spec
#' @param x A list of the sheets (data frames).
#' @export
ard_spec <- function(x) {
  x <- structure(x, class = "ard_spec")
  a <- x$analyses
  err <- character()
  need <- c("output_id", "analysis_id", "method")
  for (k in need) {
    if (any(is.na(a[[k]]))) err <- c(err, sprintf("`analyses$%s` is blank in row(s) %s", k,
                                                   paste(which(is.na(a[[k]])), collapse = ", ")))
  }
  dup <- duplicated(paste(a$output_id, a$analysis_id))
  if (any(dup)) err <- c(err, sprintf("output_id / analysis_id repeated: %s",
                                      paste(unique(paste(a$output_id, a$analysis_id)[dup]),
                                            collapse = ", ")))
  m <- stats::na.omit(a$method)
  bad <- m[!m %in% ard_methods()$method & !grepl("^[A-Za-z.][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*$", m)]
  if (length(bad)) err <- c(err, sprintf(
    "unknown method(s): %s (a keyword of ard_methods(), or pkg::function)",
    paste(unique(bad), collapse = ", ")))
  miss <- setdiff(stats::na.omit(c(a$dataset, x$populations$dataset)),
                  x$datasets$dataset)
  if (length(miss)) err <- c(err, sprintf("dataset(s) not in `datasets`: %s",
                                          paste(miss, collapse = ", ")))
  miss <- setdiff(stats::na.omit(a$population_id),
                  x$populations$population_id)
  if (length(miss)) err <- c(err, sprintf("population(s) not in `populations`: %s",
                                          paste(miss, collapse = ", ")))
  keys <- ard_methods()
  st <- ard_statistics()
  for (i in seq_len(nrow(a))) {
    k <- match(a$method[i], keys$method)
    s <- .split_bar(a$statistics[i])
    if (!is.na(k) && keys$kind[k] == "continuous") {
      bad <- setdiff(s, st$statistic[st$kind == "continuous"])
      if (length(bad)) err <- c(err, sprintf(
        "%s / %s: no continuous statistic %s (see ard_statistics())",
        a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", ")))
    }
    f <- .parse_formats(a$formats[i])
    bad <- names(f)[is.na(f) | !.fmt_ok(f)]
    if (length(bad)) err <- c(err, sprintf(
      "%s / %s: formats are statistic=format, the format xx.x, xx.x%%, a number of decimals or pvalue (%s)",
      a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", ")))
  }
  cust <- a$method %in% "custom" & is.na(a$code)
  if (any(cust)) err <- c(err, "a `custom` analysis needs its `code`")
  if (length(err)) stop(paste(c("The ARD definition is not valid:", err),
                              collapse = "\n  "), call. = FALSE)
  x
}

.study_value <- function(x, key, default) {
  v <- x$study$value[match(key, x$study$key)]
  if (length(v) && !is.na(v)) v else default
}

.r_name <- function(x) make.names(tolower(x))

# `NAME = expr | NAME = expr` as a transform() call on `obj`
.derive_code <- function(obj, derive) {
  d <- .split_bar(derive)
  if (!length(d)) return(NULL)
  sprintf("%s <- transform(%s, %s)", obj, obj, paste(d, collapse = ", "))
}

.reader <- function(path) {
  ext <- tolower(tools::file_ext(path))
  f <- switch(ext, rds = "readRDS", xpt = "haven::read_xpt",
              sas7bdat = "haven::read_sas", csv = "utils::read.csv",
              parquet = "arrow::read_parquet",
              stop("No reader for .", ext, call. = FALSE))
  sprintf("%s(%s)", f, encodeString(path, quote = "\""))
}

.vars <- function(x) {
  v <- .split_bar(x)
  if (!length(v)) return(NULL)
  if (length(v) == 1L) v else sprintf("c(%s)", paste(v, collapse = ", "))
}

.stat_arg <- function(method, stats) {
  s <- .split_bar(stats)
  if (!length(s)) return(NULL)
  q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
  switch(method,
    continuous = {
      # cards' own statistics and those computed by .tfl_stats, in the
      # order asked
      own <- !s %in% .computed_stats()
      run <- cumsum(c(TRUE, own[-1L] != own[-length(own)]))
      parts <- vapply(split(seq_along(s), run), function(i) {
        if (own[i[1L]]) sprintf("cards::continuous_summary_fns(c(%s))", q(s[i]))
        else sprintf(".tfl_stats[c(%s)]", q(s[i]))
      }, "")
      sprintf("statistic = ~ %s", if (length(parts) == 1L) parts else
        sprintf("c(%s)", paste(parts, collapse = ", ")))
    },
    categorical = ,
    missing = sprintf("statistic = ~ c(%s)", q(s)),
    NULL)
}

#' The statistics an ARD analysis may ask for
#'
#' The company standards' catalog (sheet `ard_statistics`, see
#' [company_standards()]): each statistic's `kind` -- which methods give it:
#' `continuous`, `categorical`, `missing`, or `result` (what a confidence
#' interval, a test or a model gives) --, its label, the format its
#' `stat_fmt` gets unless the method or the analysis gives another, and,
#' for the continuous statistics cards does not compute itself (CV,
#' geometric mean, percentiles ...), the R function tflplanner writes into
#' the ARD program.
#'
#' An analysis's `formats` are `statistic=format` pairs, `|` between them
#' (`mean=xx.xx | sd=xx.xxx`); `VARIABLE:statistic=format` for one variable
#' only.  A format is `xx.x` (as many x after the point as decimals),
#' `xx.x%` (a proportion as a percent), a number of decimals, or `pvalue`
#' (`<0.001`, else 3 decimals).
#'
#' @param kind Only the statistics of these kinds; `NULL` for all.
#' @return A data frame: `statistic`, `kind`, `group`, `label`, `fmt`,
#'   `fun`, `note`.
#' @export
ard_statistics <- function(kind = NULL) {
  d <- company_standards()$ard_statistics
  if (!is.null(kind)) d <- d[d$kind %in% kind, , drop = FALSE]
  rownames(d) <- NULL
  d
}

# the continuous statistics tflplanner computes (not cards)
.computed_stats <- function() {
  d <- ard_statistics("continuous")
  d$statistic[!is.na(d$fun)]
}

# the kinds of statistic a method's analysis may ask for
.stat_kinds <- function(kind) {
  switch(kind %||% "",
         continuous = "continuous", categorical = "categorical",
         missing = "missing", "result")
}

.fmt_ok <- function(f) grepl("^(x+(\\.x+)?%?|[0-9]+|pvalue)$", f)

# `mean=xx.x | AGE:sd=xx.xx` as a named vector
.parse_formats <- function(x) {
  p <- .split_bar(x)
  if (!length(p)) return(character())
  k <- trimws(sub("=.*$", "", p))
  v <- trimws(sub("^[^=]*=", "", p))
  v[!grepl("=", p, fixed = TRUE)] <- NA
  stats::setNames(v, k)
}

.fmt_vector <- function(f) {
  if (!length(f)) return("character()")
  sprintf("c(%s)", paste(sprintf("%s = %s", encodeString(names(f), quote = "`"),
                                 encodeString(f, quote = "\"")),
                         collapse = ", "))
}

# the helpers every ARD program starts with: the computed statistics it
# uses, and stat_fmt
.ard_helpers <- function(used) {
  st <- ard_statistics()
  cst <- st[st$kind == "continuous" & !is.na(st$fun) & st$statistic %in% used, ]
  dflt <- st[!duplicated(st$statistic) & !is.na(st$fmt), ]
  dflt <- stats::setNames(dflt$fmt, dflt$statistic)
  fl <- sprintf("%s = %s", encodeString(names(dflt), quote = "`"),
                encodeString(dflt, quote = "\""))
  fl <- vapply(split(fl, ceiling(seq_along(fl) / 5)), paste, "",
               collapse = ", ")
  c(if (nrow(cst)) c(
      "# statistics cards does not compute itself (company standards)",
      ".tfl_stats <- list(",
      paste0("  ", encodeString(cst$statistic, quote = "`"), " = ", cst$fun,
             c(rep(",", nrow(cst) - 1L), "")),
      ")", ""),
    "# stat_fmt: each statistic formatted -- xx.x = 1 decimal, xx.x% = a",
    "# proportion as a percent, pvalue = <0.001 or 3 decimals",
    ".fmt_default <- c(",
    paste0("  ", fl, c(rep(",", length(fl) - 1L), "")),
    ")",
    ".fmt <- function(ard, fmt = character()) {",
    "  if (!inherits(ard, \"card\")) return(ard)",
    "  f <- .fmt_default",
    "  f[names(fmt)] <- fmt",
    "  f <- f[order(grepl(\":\", names(f), fixed = TRUE))]",
    "  for (k in names(f)) {",
    "    s <- sub(\"^.*:\", \"\", k)",
    "    v <- if (grepl(\":\", k, fixed = TRUE)) sub(\":.*$\", \"\", k)",
    "    rows <- ard$stat_name == s & (is.null(v) | ard$variable %in% v)",
    "    if (!any(rows)) next",
    "    fun <- if (f[[k]] == \"pvalue\") {",
    "      function(x) ifelse(x < 0.001, \"<0.001\", sprintf(\"%.3f\", x))",
    "    } else {",
    "      # the decimals (the x after the point, or the number); % scales by 100",
    "      d <- if (grepl(\"^[0-9]+$\", f[[k]])) as.integer(f[[k]]) else",
    "        nchar(sub(\"^[^.]*[.]?\", \"\", sub(\"%$\", \"\", f[[k]])))",
    "      sc <- if (endsWith(f[[k]], \"%\")) 100 else 1",
    "      local({ d <- d; sc <- sc; cards::label_round(d, scale = sc) })",
    "    }",
    "    ard <- cards::update_ard_fmt_fun(",
    "      ard, variables = dplyr::all_of(unique(ard$variable[rows])),",
    "      stat_names = s, fmt_fun = fun)",
    "  }",
    "  cards::apply_fmt_fun(ard)",
    "}",
    "# only the statistics asked for, of a method that gives more",
    ".keep <- function(ard, keep) ard[ard$stat_name %in% keep, , drop = FALSE]",
    "")
}

#' The R code that makes the study's ARD
#'
#' One cards / cardx call per analysis row, each result tagged with its
#' `output_id`, `analysis_id` and `population_id`, bound into one ARD and
#' saved to the study key `output` (default `output/ard/ard.rds`).  The code
#' runs from the study folder.
#'
#' @param spec An [ard_spec()] (or the path of one).
#' @param output_id Only these outputs' analyses; `NULL` for all.
#' @param save `FALSE` leaves out the final `saveRDS()`.
#' @return The code, one element per line.
#' @export
ard_spec_code <- function(spec, output_id = NULL, save = TRUE) {
  x <- if (is.character(spec)) read_ard_spec(spec) else spec
  a <- x$analyses
  if (!is.null(output_id)) a <- a[a$output_id %in% output_id, , drop = FALSE]
  subj <- .study_value(x, "id", "USUBJID")
  out <- .study_value(x, "output", "output/ard/ard.rds")
  code <- c(
    "# The study's ARD, made from ard_spec.xlsx.  Run from the study folder.",
    paste0("# Generated by tflplanner ", utils::packageVersion("tflplanner"),
           ", ", format(Sys.Date())),
    "",
    "library(cards)",
    "",
    ".tag <- function(ard, output_id, analysis_id, population_id) {",
    "  ard <- as.data.frame(ard)",
    "  cbind(output_id = output_id, analysis_id = analysis_id,",
    "        population_id = population_id, ard, stringsAsFactors = FALSE)",
    "}",
    "",
    .ard_helpers(unlist(lapply(a$statistics, .split_bar))),
    "# ---- data")
  used_ds <- unique(stats::na.omit(c(a$dataset, x$populations$dataset[
    x$populations$population_id %in% a$population_id])))
  for (ds in used_ds) {
    r <- x$datasets[x$datasets$dataset == ds, ]
    obj <- .r_name(ds)
    code <- c(code, sprintf("%s <- %s", obj, .reader(r$path[1L])),
              .derive_code(obj, r$derive[1L]))
  }
  code <- c(code, "", "# ---- populations")
  for (pid in unique(stats::na.omit(a$population_id))) {
    r <- x$populations[x$populations$population_id == pid, ]
    obj <- paste0("pop_", .r_name(pid))
    src <- .r_name(r$dataset[1L])
    code <- c(code, if (is.na(r$where[1L])) sprintf("%s <- %s", obj, src) else
      sprintf("%s <- subset(%s, %s)", obj, src, r$where[1L]),
      .derive_code(obj, r$derive[1L]))
  }
  code <- c(code, "", "# ---- analyses", "ards <- list()")
  keys <- ard_methods()
  for (i in seq_len(nrow(a))) {
    r <- a[i, ]
    pid <- r$population_id
    pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
    ds <- if (!is.na(r$dataset)) .r_name(r$dataset) else pop
    pop_ds <- if (!is.na(pid)) x$populations$dataset[
      x$populations$population_id == pid][1L]
    # the analysis data: the dataset, restricted to the population's
    # subjects (or the population itself when it is that dataset), and
    # to the analysis's own subset
    data <- if (is.null(pop)) ds else if (identical(r$dataset, pop_ds) ||
                                          is.na(r$dataset)) pop else
      sprintf("subset(%s, %s %%in%% %s$%s)", ds, subj, pop, subj)
    if (!is.na(r$where)) data <- sprintf("subset(%s, %s)", data, r$where)
    given <- if (is.na(r$args)) "" else r$args
    has <- function(arg) grepl(paste0("(^|[,(\\s])", arg, "\\s*="), given)
    body <- .analysis_body(r, keys, subj, has)
    k <- match(r$method, keys$method)
    kind <- if (is.na(k)) "" else keys$kind[k]
    keep <- if (!kind %in% c("continuous", "categorical", "missing") &&
                !identical(keys$call[k], "(subjects)"))
      .split_bar(r$statistics)
    fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]),
             .parse_formats(r$formats))
    fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
    code <- c(code,
              sprintf("# %s / %s%s", r$output_id, r$analysis_id,
                      if (!is.na(r$label)) paste(":", r$label) else ""),
              sprintf("ards[[%d]] <- .tag(local({", i),
              paste0("  data <- ", data),
              paste0("  population <- ", if (is.null(pop)) "NULL" else pop),
              "  ard <- local({",
              paste0("    ", strsplit(body, "\n", fixed = TRUE)[[1L]]),
              "  })",
              if (length(keep)) sprintf("  ard <- .keep(ard, c(%s))",
                                        paste(encodeString(keep, quote = "\""),
                                              collapse = ", ")),
              sprintf("  .fmt(ard%s)", if (length(fmt))
                paste0(", ", .fmt_vector(fmt)) else ""),
              sprintf("}), %s, %s, %s)",
                      encodeString(r$output_id, quote = "\""),
                      encodeString(r$analysis_id, quote = "\""),
                      if (is.na(pid)) "NA_character_" else
                        encodeString(pid, quote = "\"")))
  }
  code <- c(code, "", "ard <- do.call(dplyr::bind_rows, ards)")
  if (save) {
    ids <- unique(a$output_id)
    hashes <- vapply(ids, function(id) .ard_output_hash(x, id), "")
    q <- function(v) paste(encodeString(v, quote = "\""), collapse = ", ")
    code <- c(code,
              sprintf("dir.create(dirname(%s), recursive = TRUE, showWarnings = FALSE)",
                      encodeString(out, quote = "\"")),
              sprintf("saveRDS(ard, %s)", encodeString(out, quote = "\"")),
              "# what was built, and from which definition (tflplanner reads it)",
              "status <- data.frame(",
              sprintf("  output_id = c(%s),", q(ids)),
              sprintf("  definition = c(%s),", q(hashes)),
              "  built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
              "  stringsAsFactors = FALSE)",
              "status$rows <- as.integer(table(factor(ard$output_id, levels = status$output_id)))",
              sprintf("utils::write.csv(status, file.path(dirname(%s), \"ard_status.csv\"), row.names = FALSE)",
                      encodeString(out, quote = "\"")))
  }
  c(code, "")
}

# A fingerprint of what makes an output's ARD: its analysis rows and the
# data, populations and study keys they use.  A built ARD whose fingerprint
# differs from the definition's now is outdated.
.ard_output_hash <- function(spec, output_id) {
  a <- spec$analyses[spec$analyses$output_id %in% output_id, , drop = FALSE]
  pops <- spec$populations[spec$populations$population_id %in%
                             a$population_id, , drop = FALSE]
  dss <- spec$datasets[spec$datasets$dataset %in%
                         c(a$dataset, pops$dataset), , drop = FALSE]
  txt <- paste(c(utils::capture.output(print(as.list(a[order(a$analysis_id), ]))),
                 utils::capture.output(print(as.list(pops))),
                 utils::capture.output(print(as.list(dss[setdiff(names(dss), "level")]))),
                 utils::capture.output(print(as.list(spec$study)))),
               collapse = "\n")
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(enc2utf8(txt), f, useBytes = TRUE)
  unname(tools::md5sum(f))
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
  spec <- structure(a, class = "ard_spec")
  rows <- lapply(ids, function(id) {
    r <- st[st$output_id == id, , drop = FALSE][1L, ]
    now <- .ard_output_hash(spec, id)
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

#' Put one output's analyses into the study ARD
#'
#' Runs the output's analyses ([run_ard()]) and replaces its rows in the
#' study ARD, leaving every other output's as they are -- so the study ARD
#' grows output by output while tables are made from what is there.  A
#' failed run leaves the study ARD alone and records the error.
#'
#' @param study An `rtfstudy`.
#' @param output_id The output.
#' @return The run ([run_ard()]), invisibly.
#' @export
update_study_ard <- function(study, output_id) {
  r <- run_ard(study, output_id)
  st <- .read_ard_status(study)
  st <- st[st$output_id != output_id, , drop = FALSE]
  now <- .ard_output_hash(structure(study$planner$ard, class = "ard_spec"),
                          output_id)
  if (is.null(r$ard)) {
    st[nrow(st) + 1L, ] <- list(output_id, now, format(Sys.time(),
      "%Y-%m-%d %H:%M:%S"), NA_integer_, r$error %||% "failed")
    .write_ard_status(study, st)
    return(invisible(r))
  }
  out <- file.path(study$path, .ard_study_value(study$planner$ard, "output",
                                                "output/ard/ard.rds"))
  old <- if (file.exists(out)) readRDS(out)
  new <- if (is.null(old)) r$ard else dplyr::bind_rows(
    old[old$output_id != output_id, , drop = FALSE], r$ard)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(out, ".tmp")
  saveRDS(new, tmp)
  file.rename(tmp, out)
  st[nrow(st) + 1L, ] <- list(output_id, now,
                              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              nrow(r$ard), "")
  .write_ard_status(study, st)
  invisible(r)
}

#' Make the study's ARD from its definition
#'
#' Runs [ard_spec_code()] from the study folder: the ARD is exactly what the
#' code gives.
#'
#' @inheritParams ard_spec_code
#' @param dir The study folder (the code's working directory).
#' @return The ARD (with `output_id`, `analysis_id`, `population_id`),
#'   invisibly; with `save`, also written where the study key `output`
#'   says.
#' @export
build_ard <- function(spec, dir = ".", output_id = NULL, save = TRUE) {
  code <- ard_spec_code(spec, output_id = output_id, save = save)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = globalenv())
  eval(parse(text = code, encoding = "UTF-8"), envir = e)
  invisible(e$ard)
}

#' One output's part of the study ARD
#'
#' @param ard The study ARD ([build_ard()]).
#' @param output_id The output.
#' @return Its rows, without the id columns: what [rtfreporter::ard_normalize()]
#'   takes.
#' @export
ard_for <- function(ard, output_id) {
  d <- ard[ard$output_id == output_id, , drop = FALSE]
  d[setdiff(names(d), c("output_id", "analysis_id", "population_id"))]
}
