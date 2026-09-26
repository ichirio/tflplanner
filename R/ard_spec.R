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
#                where, by, variables, statistics, args, code
#
# The concepts are those of CDISC ARS (analysis set, data subset, grouping,
# method), so an ard_spec can later be written as ARS metadata.

.ard_spec_sheets <- list(
  study = c("key", "value"),
  datasets = c("dataset", "path", "derive"),
  populations = c("population_id", "dataset", "where", "derive"),
  analyses = c("output_id", "analysis_id", "label", "method", "dataset",
               "population_id", "where", "by", "variables", "statistics",
               "args", "code"))

#' The methods an analysis row may name
#'
#' @return A data frame: `method`, `call` (the cards / cardx function),
#'   `statistics` (the default), `note`.
#' @export
ard_methods <- function() {
  data.frame(
    method = c("continuous", "categorical", "dichotomous", "hierarchical",
               "total_n", "proportion_ci", "custom"),
    call = c("cards::ard_continuous", "cards::ard_categorical",
             "cards::ard_dichotomous", "cards::ard_stack_hierarchical",
             "cards::ard_total_n", "cardx::ard_categorical_ci", "(code)"),
    statistics = c("N | mean | sd | median | p25 | p75 | min | max",
                   "n | N | p", "n | N | p", "n | N | p", "N", "", ""),
    note = c(
      "summary statistics of numeric variables",
      "counts and percents of each level",
      "counts of one level (args: value = list(VAR = \"Y\"))",
      "nested counts (variables outermost first); denominator = population",
      "number of subjects in the population",
      "confidence interval of a proportion (args: method = \"wilson\", conf.level = 0.95 ...)",
      "any R expression in `code`, with `data` the analysis data and `population` the population"),
    stringsAsFactors = FALSE)
}

.split_bar <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
}

#' Read and check an ARD definition workbook
#'
#' @param path An `ard_spec.xlsx`.
#' @return An `ard_spec`: a list of the four sheets.
#' @export
read_ard_spec <- function(path) {
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
  ard_spec(out)
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
  bad <- setdiff(stats::na.omit(a$method), ard_methods()$method)
  if (length(bad)) err <- c(err, sprintf("unknown method(s): %s (see ard_methods())",
                                         paste(bad, collapse = ", ")))
  miss <- setdiff(stats::na.omit(c(a$dataset, x$populations$dataset)),
                  x$datasets$dataset)
  if (length(miss)) err <- c(err, sprintf("dataset(s) not in `datasets`: %s",
                                          paste(miss, collapse = ", ")))
  miss <- setdiff(stats::na.omit(a$population_id),
                  x$populations$population_id)
  if (length(miss)) err <- c(err, sprintf("population(s) not in `populations`: %s",
                                          paste(miss, collapse = ", ")))
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
  q <- paste(encodeString(s, quote = "\""), collapse = ", ")
  switch(method,
    continuous = sprintf("statistic = ~ cards::continuous_summary_fns(c(%s))", q),
    categorical = , dichotomous = , hierarchical =
      sprintf("statistic = ~ c(%s)", q),
    NULL)
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
    paste0("# Generated by rtfplanner ", utils::packageVersion("rtfplanner"),
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
    args <- c(
      if (!is.null(.vars(r$by))) paste("by =", .vars(r$by)),
      if (!r$method %in% "total_n" && !is.null(.vars(r$variables)))
        paste("variables =", .vars(r$variables)),
      .stat_arg(r$method, r$statistics),
      if (r$method == "hierarchical") c(
        if (!is.null(pop)) paste("denominator =", pop),
        paste("id =", subj)),
      if (!is.na(r$args)) r$args)
    fn <- ard_methods()$call[ard_methods()$method == r$method]
    call <- if (r$method == "custom") {
      sprintf("local({\n  data <- %s\n  population <- %s\n  %s\n})", data,
              if (is.null(pop)) "NULL" else pop,
              gsub("\n", "\n  ", r$code, fixed = TRUE))
    } else {
      sprintf("%s(%s)", fn, paste(c(data, args), collapse = ",\n    "))
    }
    code <- c(code,
              sprintf("# %s / %s%s", r$output_id, r$analysis_id,
                      if (!is.na(r$label)) paste(":", r$label) else ""),
              sprintf("ards[[%d]] <- .tag(%s,", i, call),
              sprintf("  %s, %s, %s)",
                      encodeString(r$output_id, quote = "\""),
                      encodeString(r$analysis_id, quote = "\""),
                      if (is.na(pid)) "NA_character_" else
                        encodeString(pid, quote = "\"")))
  }
  code <- c(code, "", "ard <- do.call(dplyr::bind_rows, ards)")
  if (save) {
    code <- c(code,
              sprintf("dir.create(dirname(%s), recursive = TRUE, showWarnings = FALSE)",
                      encodeString(out, quote = "\"")),
              sprintf("saveRDS(ard, %s)", encodeString(out, quote = "\"")))
  }
  c(code, "")
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
