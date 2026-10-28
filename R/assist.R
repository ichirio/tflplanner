# Input assistance: what a report's ARD says, turned into rows and choices.
#
# The ARD is the report's own data, so it knows the answers to most of what
# the definition sheets ask -- which variables there are, their levels, the
# column and hierarchy keys, the statistics a template may name.
# fetch_ard() runs the report's data part (from the study folder, in its
# own R process) and keeps what ard_meta() reads from the result -- after
# the report's normalization and rework, since that is what the table is
# built from.  The rest fills sheets from it and offers it as choices.

# the columns normalize_ard() adds or keeps that are not keys
.ard_fixed <- c("variable", "variable_level", "context", "stat_name",
                "stat_label", "stat", "stat_fmt", "fmt_fun", "warning",
                "error")

.flat <- function(x) {
  if (!is.list(x)) return(as.character(x))
  vapply(x, function(e) {
    if (is.null(e) || !length(e)) NA_character_ else as.character(e[[1L]])
  }, "")
}

.levels_of <- function(x) {
  if (is.factor(x)) return(levels(x))
  x <- .flat(x)
  unique(x[!is.na(x) & nzchar(x)])
}

#' What an ARD holds
#'
#' Reads the keys (the column and hierarchy variables) with their levels,
#' the analysis variables with their kind, levels, statistics and labels,
#' and the contexts and statistic names, from a normalized ARD (`data`,
#' what [rtfreporter::table_plan()] is given) or, failing that, from the raw
#' cards ARD.
#'
#' @param ard A cards ARD, or `NULL`.
#' @param data The normalized (and reworked) ARD, or `NULL`.
#' @return A list: `keys` (named list of levels), `by` (the column keys),
#'   `hierarchy` (the hierarchy keys, outermost first), `variables` (a data
#'   frame: `variable`, `kind`, `levels`, `n_levels`, `stats`, `label`),
#'   `contexts`, `stats`, `columns`; from a normalized ARD also `overall`
#'   (rows over all subjects, which a Total column reads), `factor_vars`
#'   (the variables whose levels keep a factor's order), `factor_keys`
#'   (the key columns that are factors: their levels' order) and
#'   `comparisons` (the comparison records -- tests, differences -- by
#'   context: their statistics).
#' @export
ard_meta <- function(ard = NULL, data = NULL) {
  out <- list(keys = list(), by = character(), hierarchy = character(),
              variables = data.frame(variable = character(),
                                     kind = character(), levels = character(),
                                     n_levels = integer(), stats = character(),
                                     label = character(),
                                     stringsAsFactors = FALSE),
              contexts = character(), stats = character(),
              columns = character())
  d <- data
  if (is.null(d) && is.data.frame(ard)) {
    d <- tryCatch(rtfreporter::normalize_ard(ard), error = function(e) NULL)
  }
  labels <- .ard_labels(ard)
  if (is.data.frame(d) && "variable" %in% names(d)) {
    cn <- names(d)
    meta_col <- cn %in% .ard_fixed | startsWith(cn, ".") |
      grepl("^group[0-9]+(_level)?$", cn)
    keys <- cn[!meta_col]
    # the overall ("Any TEAE") row names itself in a key column; it is not
    # a level of that key
    ovr <- if (".overall" %in% cn) d$.overall %in% TRUE else
      rep(FALSE, nrow(d))
    out$keys <- stats::setNames(lapply(keys, function(k)
      .levels_of(d[[k]][!ovr])), keys)
    v <- .flat(d$variable)
    own <- if (".key_own" %in% cn) d$.key_own %in% TRUE else
      rep(FALSE, nrow(d))
    sentinel <- !is.na(v) & startsWith(v, "..")
    by <- unique(v[own & v %in% keys])
    hier <- unique(v[!own & !sentinel & v %in% keys])
    if (".depth" %in% cn && length(hier)) {
      depth <- tapply(suppressWarnings(as.numeric(d$.depth)), v,
                      function(z) min(z, na.rm = TRUE))
      hier <- hier[order(depth[hier])]
    }
    if (!length(by)) by <- setdiff(keys, hier)
    out$by <- by
    out$hierarchy <- hier
    ana <- unique(v[!is.na(v) & !own & !sentinel & !v %in% keys])
    kind <- if (".kind" %in% cn) .flat(d$.kind) else
      ifelse(is.na(.flat(d$variable_level)), "continuous", "categorical")
    lvl <- .flat(d$variable_level)
    sn <- .flat(d$stat_name)
    out$variables <- do.call(rbind, c(list(out$variables), lapply(ana,
      function(a) {
        i <- !is.na(v) & v == a
        k <- kind[i][1L]
        l <- if (identical(k, "categorical")) .levels_of(lvl[i]) else
          character()
        data.frame(variable = a, kind = k,
                   levels = paste(l, collapse = " | "),
                   n_levels = length(l),
                   stats = paste(unique(sn[i]), collapse = " | "),
                   label = labels[a] %||% NA_character_,
                   stringsAsFactors = FALSE)
      })))
    out$contexts <- unique(stats::na.omit(.flat(d$context)))
    out$stats <- unique(stats::na.omit(sn))
    out$columns <- cn
    # the analyses' rows over all subjects (no column key: a Total column
    # reads them), the variables whose levels keep a factor's order and
    # the key columns that are factors (TRT01A in its arms' order)
    out$overall <- .ard_has_overall(d, by)
    out$factor_vars <- if (".label_order" %in% cn) {
      unique(v[!is.na(v) & v %in% ana & !is.na(d$.label_order)])
    } else character()
    out$factor_keys <- keys[vapply(keys, function(k) is.factor(d[[k]]), NA)]
    out$comparisons <- .ard_comparisons(d, by)
  } else if (is.data.frame(ard)) {
    # a raw ARD: keys are the groupN columns, their levels groupN_level
    g <- grep("^group[0-9]+$", names(ard), value = TRUE)
    for (gc in g) {
      nm <- unique(stats::na.omit(.flat(ard[[gc]])))
      for (k in nm) {
        i <- .flat(ard[[gc]]) %in% k
        out$keys[[k]] <- unique(c(out$keys[[k]],
                                  .levels_of(ard[[paste0(gc, "_level")]][i])))
      }
    }
    out$by <- names(out$keys)
    v <- .flat(ard$variable)
    ana <- unique(v[!is.na(v) & !startsWith(v, "..")])
    lvl <- .flat(ard$variable_level)
    sn <- .flat(ard$stat_name)
    out$variables <- do.call(rbind, c(list(out$variables), lapply(ana,
      function(a) {
        i <- v == a
        l <- .levels_of(lvl[i])
        data.frame(variable = a,
                   kind = if (length(l)) "categorical" else "continuous",
                   levels = paste(l, collapse = " | "), n_levels = length(l),
                   stats = paste(unique(sn[i]), collapse = " | "),
                   label = labels[a] %||% NA_character_,
                   stringsAsFactors = FALSE)
      })))
    out$contexts <- unique(stats::na.omit(.flat(ard$context)))
    out$stats <- unique(stats::na.omit(sn))
    out$columns <- names(ard)
  }
  rownames(out$variables) <- NULL
  out
}

# The comparison records of a normalized ARD (the tests, differences
# plan_compare() reads): the analyses whose rows never have a column key
# (`by` all NA in every row of the context; the overall rows share their
# context with the arms' rows), the attributes and the total N left out.
# A named list: context -> its statistics that are numbers, in order.
.ard_comparisons <- function(d, by) {
  if (!is.data.frame(d) || !nrow(d) || !length(by) || !all(by %in% names(d)) ||
      !all(c("context", "stat_name") %in% names(d))) return(list())
  ctx <- .flat(d$context)
  none <- Reduce(`&`, lapply(by, function(k) is.na(.flat(d[[k]]))))
  cmp <- setdiff(unique(ctx[none & !is.na(ctx)]), c(unique(ctx[!none]), "attributes", "total_n"))
  if (!length(cmp)) return(list())
  sn <- .flat(d$stat_name)
  # (a statistic that is a number: the normalized ARD's stat is NA for a
  # text, as a test's method)
  num <- if (!"stat" %in% names(d)) rep(TRUE, nrow(d)) else if (is.list(d$stat)) {
    vapply(d$stat, function(e) is.numeric(e) && length(e) == 1L && !is.na(e), NA)
  } else !is.na(suppressWarnings(as.numeric(d$stat)))
  out <- lapply(cmp, function(k) unique(sn[ctx == k & none & num & !is.na(sn)]))
  stats::setNames(out, cmp)
}

# Has a normalized ARD rows over all subjects -- an analysis's rows with no
# column key (`by` all NA), as ard_stack(.overall = TRUE) makes them?
# plan_total() reads those; without them it stops.
.ard_has_overall <- function(d, by) {
  if (!is.data.frame(d) || !nrow(d) || !length(by) || !all(by %in% names(d))) return(FALSE)
  v <- .flat(d$variable)
  own <- if (".key_own" %in% names(d)) d$.key_own %in% TRUE else rep(FALSE, nrow(d))
  ana <- !is.na(v) & !startsWith(v, "..") & !own & !v %in% by
  none <- Reduce(`&`, lapply(by, function(k) is.na(.flat(d[[k]]))))
  # (an overall row shares its context with the arms' rows; a comparison
  # -- a test, a difference -- has no arm row at all, so it is not one)
  if ("context" %in% names(d)) {
    ctx <- .flat(d$context)
    none <- none & ctx %in% unique(ctx[!none])
  }
  any(ana & none)
}

# variable labels a cards ARD carries (ard_stack(.attributes = TRUE))
.ard_labels <- function(ard) {
  if (!is.data.frame(ard) ||
      !all(c("context", "stat_name", "variable", "stat") %in% names(ard))) {
    return(character())
  }
  i <- .flat(ard$context) %in% "attributes" & .flat(ard$stat_name) %in% "label"
  if (!any(i)) return(character())
  stats::setNames(.flat(ard$stat[i]), .flat(ard$variable[i]))
}

# ------------------------------------------------------------ fetching

.meta_file <- function(study, output_id, home = tflplanner_home()) {
  file.path(.store_dir(study$meta$study_id, home), "ard",
            paste0(output_id, ".rds"))
}

#' Run a report's data part and keep what its ARD holds
#'
#' Runs the study's setup code, the report's ARD code and its
#' normalization and rework ([data_lines()]) in a fresh R process, from the
#' study folder, and reads [ard_meta()] from the result: the normalized
#' `data` when the rework ran, the raw `ard` otherwise.  A report whose ARD
#' its ARD definition makes reads its rows of the study ARD when they are
#' made from the definition as it is now ([ard_status()]: built); else its
#' ARD is made here, in a folder of its own, and the study ARD
#' (`output/ard/ard.rds`) is not written -- making the study ARD is
#' [update_study_ard()]'s, and the official run's.  The data frames the
#' code loaded (`adsl`, ...) add what the ARD lacks: each variable's label,
#' and the order of its values (a factor's levels, or the order of a
#' numeric companion `<name>N`: `TRT01A` by `TRT01AN`).  The result is kept
#' in tflplanner's home, so it is there the next time the study is opened.
#'
#' @param study An `rtfstudy`.
#' @param output_id The report.
#' @param timeout Seconds to allow.
#' @param home tflplanner's home.
#' `ard_info()` returns the metadata kept, `ard_data()` the normalized data
#' kept with it (what the builder's preview is planned from), `ard_raw()`
#' the ARD before its normalization (`rows = TRUE`: the report's rows as the
#' study ARD has them, with their ids), and `ard_preview_state()` whether
#' what is kept is still the report's ARD: `"missing"`, `"outdated"` (its
#' definition, its code, the study's setup or the data have changed since)
#' or `"current"`.
#'
#' @return The metadata ([ard_meta()]) with `fetched` (the time), `source`
#'   (`"data"` or `"ard"`), `error` (the rework's error, if it failed),
#'   `made` (`"here"`: made for the report; `"study"`: read from the study
#'   ARD), `print` (what it was made from, [ard_preview_state()]) and `log`,
#'   invisibly.
#' @export
fetch_ard <- function(study, output_id, timeout = 300,
                      home = tflplanner_home()) {
  p <- study$planner
  o <- p$outputs[p$outputs$output_id == output_id, , drop = FALSE]
  # its data step's own code (its datasets and its ARD read first), or the
  # ARD its definition makes
  ard_code <- if (.is_code_step(p, output_id, "data")) {
    paste(c(.code_inputs(p, output_id), step_code(p, output_id, "data")), collapse = "\n")
  } else .ard_code_of(p, output_id)
  if (!nrow(o) || is.na(ard_code)) {
    stop("Report '", output_id, "' has no ARD code yet.", call. = FALSE)
  }
  tmp <- tempfile("fetch")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f_ard <- file.path(tmp, "ard.R")
  f_proc <- file.path(tmp, "process.R")
  f_out <- file.path(tmp, "out.rds")
  q <- function(x) encodeString(normalizePath(x, "/", FALSE), quote = "\"")
  # its data is its part of the study ARD: those rows while they are the
  # definition's as it is now; else the report's ARD made here (its ARD
  # program's code, the study ARD left alone), and its data code reads that
  made <- "here"
  rows <- NULL
  f_make <- NULL
  if (.ard_from_definition(p, output_id)) {
    st <- tryCatch(ard_status(study), error = function(e) NULL)
    if (identical(st$state[match(output_id, st$output_id)], "built")) {
      rows <- tryCatch(study_ard_rows(study, output_id), error = function(e) NULL)
    }
    if (!is.null(rows) && nrow(rows)) {
      made <- "study"
    } else {
      rows <- NULL
      f_make <- file.path(tmp, "make_ard.R")
      f_made <- file.path(tmp, "ard_made.rds")
      code <- .ard_spec_code(.ard_spec(p$ard), output_id = output_id, save = FALSE,
                             dir = study$path, codelists = .study_codelists(p))
      writeLines(enc2utf8(c(code, sprintf("saveRDS(ard, %s)", q(f_made)))), f_make,
                 useBytes = TRUE)
      ard_code <- .fill_template("table_data", .with_ard_output(p, normalizePath(f_made, "/", FALSE)),
                                 output_id)
    }
  }
  # the report's id first, as its program has it (a template reads it)
  writeLines(enc2utf8(c(.report_id_line(output_id),
                        if (!is.na(p$setup)) .code_block(p$setup),
                        .code_block(ard_code))), f_ard, useBytes = TRUE)
  # (the normalization its program has: a hierarchical analysis's terms
  # and the any-event row, so the builder sees the hierarchy)
  proc <- if (!is.na(o$process_code)) .code_block(o$process_code) else
    if (!.makes_data(ard_code)) .normalize_line(p, output_id) else ""
  writeLines(enc2utf8(proc), f_proc, useBytes = TRUE)
  # the functions the programs call (programs/study_helpers.R's)
  f_helpers <- file.path(tmp, "study_helpers.R")
  writeLines(tflspec::tfl_helpers_code(), f_helpers)
  script <- c(
    # (the report's ARD made first, in an environment of its own)
    if (!is.null(f_make)) paste0("local(eval(parse(", q(f_make),
                                 ", encoding = \"UTF-8\"), envir = new.env(parent = globalenv())))"),
    "suppressPackageStartupMessages(library(rtfreporter))",
    paste0("sys.source(", q(f_helpers), ", envir = globalenv())"),
    ".e <- new.env(parent = globalenv())",
    paste0("eval(parse(", q(f_ard), ", encoding = \"UTF-8\"), envir = .e)"),
    ".res <- list(ard = get0(\"ard\", .e, inherits = FALSE), data = NULL,",
    "             error = NULL)",
    ".res$data <- tryCatch({",
    paste0("  eval(parse(", q(f_proc), ", encoding = \"UTF-8\"), envir = .e)"),
    "  get0(\"data\", .e, inherits = FALSE)",
    "}, error = function(e) { .res$error <<- conditionMessage(e); NULL })",
    # what the source data say about their columns: labels, and the order
    # of a column's values -- a factor's levels, or the values sorted by a
    # numeric companion column <name>N (TRT01A by TRT01AN)
    ".lab <- list(); .ord <- list()",
    "for (.n in ls(.e)) {",
    "  .d <- get(.n, envir = .e)",
    "  if (!is.data.frame(.d) || identical(.n, \"data\")) next",
    "  for (.c in names(.d)) {",
    "    .l <- attr(.d[[.c]], \"label\", exact = TRUE)",
    "    if (is.null(.lab[[.c]]) && is.character(.l) && length(.l) == 1L)",
    "      .lab[[.c]] <- .l",
    "    if (!is.null(.ord[[.c]])) next",
    "    if (is.factor(.d[[.c]])) { .ord[[.c]] <- levels(.d[[.c]]); next }",
    "    .cn <- paste0(.c, \"N\")",
    "    if (is.character(.d[[.c]]) && is.numeric(.d[[.cn]])) {",
    "      .u <- unique(data.frame(v = .d[[.c]], n = .d[[.cn]]))",
    "      .u <- .u[!is.na(.u$v), ]",
    "      .ord[[.c]] <- unique(.u$v[order(.u$n)])",
    "    }",
    "  }",
    "}",
    ".res$labels <- unlist(.lab); .res$orders <- .ord",
    paste0("saveRDS(.res, ", q(f_out), ")"))
  f_script <- file.path(tmp, "fetch.R")
  writeLines(script, f_script)
  px <- processx::run(file.path(R.home("bin"), "Rscript"), f_script,
                      wd = study$path, error_on_status = FALSE,
                      timeout = timeout, stderr_to_stdout = TRUE)
  log <- px$stdout
  if (!file.exists(f_out)) {
    why <- .first_error(log)
    stop(.problem(
      paste0("The ARD code of '", output_id, "' did not run",
             if (!is.na(why)) paste0(": ", why) else "", ".",
             .problem_hint(why)),
      detail = log))
  }
  res <- readRDS(f_out)
  data <- if (is.data.frame(res$data)) res$data
  if (!is.null(f_make) && file.exists(f_made)) rows <- readRDS(f_made)
  m <- .with_source(ard_meta(res$ard, data), res$labels, res$orders)
  m$fetched <- format(Sys.time(), "%Y-%m-%d %H:%M")
  m$source <- if (!is.null(data)) "data" else "ard"
  m$error <- res$error
  m$made <- made
  m$print <- .ard_print(study, output_id)
  m$log <- log
  f <- .meta_file(study, output_id, home)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  # the normalized data, the ARD before it (normalized again in the app as
  # normalize_ard()'s arguments change) and the report's rows with their ids
  put <- function(suffix, x) {
    fx <- sub("[.]rds$", suffix, f)
    if (is.data.frame(x)) saveRDS(x, fx) else unlink(fx)
  }
  put("_data.rds", data)
  put("_ard.rds", res$ard)
  put("_rows.rds", rows)
  invisible(m)
}

# Is a report's ARD its ARD definition's (its part of the study ARD)?  Not
# when its data step is its own code, nor when it takes in an ARD made
# elsewhere.
.ard_from_definition <- function(x, output_id) {
  !.is_code_step(x, output_id, "data") && is.null(.ard_import_of(x, output_id)) &&
    any(x$ard$analyses$output_id %in% output_id)
}

# the planner with its study ARD at `path` (the ARD definition's output):
# the report's data code (code_templates: table_data) then reads that file
.with_ard_output <- function(x, path) {
  st <- x$ard$study
  if (is.null(st) || !is.data.frame(st)) st <- data.frame(key = character(), value = character())
  k <- match("output", st$key)
  if (is.na(k)) {
    st[nrow(st) + 1L, ] <- NA
    k <- nrow(st)
    st$key[k] <- "output"
  }
  st$value[k] <- path
  x$ard$study <- st
  x
}

# What a report's ARD is made from, as one fingerprint: its ARD
# definition's (tflspec's hash: the analyses, their data, populations and
# datasets, the study's settings and own functions, the code lists), its
# data code (the report's own, or the code that reads its ARD), its
# rework, the study's setup, and the data files' sizes and times.  Kept
# with the ARD read (fetch_ard()); another now: the ARD is outdated.
.ard_print <- function(study, output_id) {
  p <- study$planner
  o <- p$outputs[p$outputs$output_id == output_id, , drop = FALSE]
  code <- tryCatch(if (.is_code_step(p, output_id, "data")) {
    paste(c(.code_inputs(p, output_id), step_code(p, output_id, "data")), collapse = "\n")
  } else .ard_code_of(p, output_id), error = function(e) NA_character_)
  defn <- if (.ard_from_definition(p, output_id)) {
    tryCatch(tflspec::tfl_ard_spec_hash(structure(p$ard, class = "tfl_ard_spec"), output_id,
                                        dir = study$path, codelists = .study_codelists(p)),
             error = function(e) NA_character_)
  }
  # (a table's normalization the app makes again itself: not part of it)
  proc <- if (nrow(o) && !.table_norm_builtin(p, output_id)) o$process_code[1L]
  ds <- p$ard$datasets$path
  imp <- .ard_import_of(p, output_id)
  files <- c(character(), if (length(ds)) .data_file(study$path, ds[!is.na(ds) & nzchar(ds)]),
             if (!is.null(imp)) file.path(.import_dir(study), imp))
  info <- file.info(files)
  data <- paste(basename(files), info$size, as.numeric(info$mtime), collapse = ";")
  txt <- c(defn, code, proc %||% "", if (!is.na(p$setup)) p$setup, .study_setup_hash(study$path),
           data)
  f <- tempfile("print")
  on.exit(unlink(f), add = TRUE)
  writeLines(enc2utf8(as.character(txt)), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

# What the source data know and the ARD does not: a variable's label, and
# the order of its values.  The ARD lists values as it met them.
.with_source <- function(m, labels, orders) {
  put_in_order <- function(l, o) {
    if (is.null(o)) return(l)
    c(intersect(o, l), setdiff(l, o))
  }
  for (k in names(m$keys)) m$keys[[k]] <- put_in_order(m$keys[[k]],
                                                       orders[[k]])
  v <- m$variables
  for (i in seq_len(nrow(v))) {
    o <- orders[[v$variable[i]]]
    if (!is.null(o) && v$n_levels[i] > 0) {
      l <- strsplit(v$levels[i], " | ", fixed = TRUE)[[1L]]
      v$levels[i] <- paste(put_in_order(l, o), collapse = " | ")
    }
    if (is.na(v$label[i]) && !is.null(labels) &&
        v$variable[i] %in% names(labels)) {
      v$label[i] <- labels[[v$variable[i]]]
    }
  }
  m$variables <- v
  m$labels <- labels
  m
}

# The hierarchy of a report's hierarchical analysis (its terms, outermost
# first; none: character()) -- the ARD analysis's, step 1's
.analysis_hierarchy <- function(x, output_id) {
  a <- tryCatch(ard_rows(x, "analyses", output_id), error = function(e) NULL)
  if (is.null(a) || !nrow(a)) return(character())
  kw <- .method_kw(a$method)
  at <- which(kw %in% c("hierarchical", "cards::ard_stack_hierarchical"))
  if (!length(at)) return(character())
  v <- .split_bar(a$variables[at[1L]])
  v[!is.na(v) & nzchar(v)]
}

# What the ARD says, with the hierarchy its analysis has when the reading
# did not see one (an ARD read before its normalization knew the terms:
# the innermost term an analysis variable, the outer ones column keys)
.meta_with_hierarchy <- function(m, x, output_id) {
  if (is.null(m) || length(m$hierarchy)) return(m)
  hier <- .analysis_hierarchy(x, output_id)
  if (length(hier) < 2L) return(m)
  v <- m$variables
  for (h in setdiff(hier, names(m$keys))) {
    i <- match(h, v$variable)
    m$keys[[h]] <- if (!is.na(i)) .split_list(v$levels[i]) else character()
  }
  m$by <- setdiff(m$by, hier)
  m$hierarchy <- hier
  if (!is.null(v)) m$variables <- v[!v$variable %in% hier, , drop = FALSE]
  m
}

#' @rdname fetch_ard
#' @noRd
ard_info <- function(study, output_id, home = tflplanner_home()) {
  f <- .meta_file(study, output_id, home)
  if (file.exists(f)) readRDS(f)
}

#' @rdname fetch_ard
#' @noRd
ard_data <- function(study, output_id, home = tflplanner_home()) {
  f <- sub("[.]rds$", "_data.rds", .meta_file(study, output_id, home))
  if (file.exists(f)) readRDS(f)
}

#' @rdname fetch_ard
#' @param rows `TRUE`: the report's rows with their ids, as the study ARD
#'   has them (the ARD definition's reports; `NULL` for the others).
#' @noRd
ard_raw <- function(study, output_id, rows = FALSE, home = tflplanner_home()) {
  f <- sub("[.]rds$", if (isTRUE(rows)) "_rows.rds" else "_ard.rds",
           .meta_file(study, output_id, home))
  if (file.exists(f)) readRDS(f)
}

#' @rdname fetch_ard
#' @param meta What was kept ([ard_info()]); by default, read.
#' @noRd
ard_preview_state <- function(study, output_id, meta = ard_info(study, output_id, home),
                              home = tflplanner_home()) {
  if (is.null(meta)) return("missing")
  if (is.null(meta$print) || !identical(meta$print, .ard_print(study, output_id))) {
    return("outdated")
  }
  "current"
}

# ----------------------------------------------------------- filling

.join <- function(x) paste(x, collapse = " | ")

#' Fill a report's definition from its ARD
#'
#' `fill_variables()` gives every column key and analysis variable of the ARD a
#' `variables` row: its levels in the ARD's order (for a categorical
#' variable, and a key with no more than `max_levels` levels), its label
#' when the ARD carries one, and a running `order` for the analysis
#' variables.  Rows already there are kept; only their blank `levels` and
#' `label` are filled.  `fill_tables()` fills the blank cells of the
#' report's `tables` row: the column key, and the hierarchy (`rows`,
#' `label`, `sort`) or `group = variable`.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param meta Its [ard_meta()].
#' @param max_levels Keys with more levels than this (a preferred term) get
#'   no `levels`.
#' @return The `tflplanner`, with attribute `changed` (rows added or
#'   filled).
#' @export
fill_variables <- function(x, output_id, meta,
                           max_levels = as.integer(.std_setting("max_levels", "30"))) {
  rows <- sheet_rows(x, "variables", output_id)
  rows$output_id <- NULL
  changed <- 0L
  nxt <- suppressWarnings(max(c(0, as.numeric(rows$order)), na.rm = TRUE))
  # the column keys and the analysis variables; a hierarchy key (SOC, PT)
  # is ordered by the table's sort, not by a list of levels
  by <- meta$keys[intersect(meta$by, names(meta$keys))]
  want <- rbind(
    data.frame(variable = names(by),
               levels = vapply(by, function(l)
                 if (length(l) <= max_levels) .join(l) else NA_character_,
                 ""),
               label = NA_character_, analysis = FALSE,
               stringsAsFactors = FALSE),
    if (nrow(meta$variables)) data.frame(
      variable = meta$variables$variable,
      levels = ifelse(meta$variables$n_levels > 0 &
                        meta$variables$n_levels <= max_levels,
                      meta$variables$levels, NA_character_),
      label = meta$variables$label, analysis = TRUE,
      stringsAsFactors = FALSE))
  for (i in seq_len(nrow(want))) {
    w <- want[i, ]
    j <- which(rows$variable %in% w$variable)
    if (!length(j)) {
      new <- rows[0, ][1, ]
      new$variable <- w$variable
      new$levels <- w$levels
      new$label <- w$label
      if (w$analysis) {
        nxt <- nxt + 1
        new$order <- as.character(nxt)
      }
      rows <- rbind(rows, new)
      changed <- changed + 1L
    } else {
      j <- j[1L]
      hit <- FALSE
      if (is.na(rows$levels[j]) && !is.na(w$levels)) {
        rows$levels[j] <- w$levels
        hit <- TRUE
      }
      if (is.na(rows$label[j]) && !is.na(w$label)) {
        rows$label[j] <- w$label
        hit <- TRUE
      }
      changed <- changed + hit
    }
  }
  x <- set_sheet_rows(x, "variables", output_id, rows)
  attr(x, "changed") <- changed
  x
}

#' @rdname fill_variables
#' @noRd
fill_tables <- function(x, output_id, meta) {
  rows <- sheet_rows(x, "tables", output_id)
  rows$output_id <- NULL
  if (!nrow(rows)) rows[1, ] <- NA
  sug <- list(cols = if (length(meta$by)) .join(meta$by))
  h <- meta$hierarchy
  if (length(h)) {
    outer <- h[-length(h)]
    sug$rows <- if (length(outer)) {
      .join(paste0("group", seq_along(outer), " = ", outer))
    }
    sug$label <- paste("label =", h[length(h)])
    sug$sort <- ".overall | group1 | .depth | -n | label"
  } else {
    sug$rows <- "group = variable"
  }
  changed <- 0L
  for (k in names(sug)) {
    if (!is.null(sug[[k]]) && is.na(rows[[k]][1L])) {
      rows[[k]][1L] <- sug[[k]]
      changed <- changed + 1L
    }
  }
  x <- set_sheet_rows(x, "tables", output_id, rows)
  attr(x, "changed") <- changed
  x
}

# ------------------------------------------------------------- presets

#' Presets for cells and column headers
#'
#' Ready-made rows to start from and adjust: the cell templates and the
#' column headers of the company standards ([company_standards()]).
#'
#' @return `cell_presets()` and `header_presets()` return a named list of
#'   data frames in the columns of their sheet.
#' @noRd
cell_presets <- function() {
  d <- company_standards()$cell_presets
  lapply(split(d[c("variable", "row", "template", "digits")],
               factor(d$preset, levels = unique(d$preset))), function(x) {
    rownames(x) <- NULL
    x
  })
}

#' @rdname cell_presets
#' @noRd
header_presets <- function() {
  d <- company_standards()$header_presets
  lapply(split(d[c("line", "cols", "span", "text")],
               factor(d$preset, levels = unique(d$preset))), function(x) {
    rownames(x) <- NULL
    x
  })
}

# The column header presets that fit a table, as choices: a preset for a
# nested (SOC / PT) table only for one with a hierarchy, the others only
# for one without; each named after the table's column variable instead of
# the preset's "Arm" (`key`, e.g. its label "Actual Treatment").
.header_preset_choices <- function(hierarchy = FALSE, key = NULL,
                                   presets = header_presets()) {
  nested <- vapply(presets, function(d)
    any(grepl("System Organ Class|Preferred Term", d$text)), NA) |
    grepl("SOC / PT", names(presets), fixed = TRUE)
  keep <- names(presets)[nested == isTRUE(hierarchy)]
  shown <- if (length(key) == 1L && !is.na(key) && nzchar(key))
    sub("^Arm\\b", key, keep) else keep
  stats::setNames(keep, shown)
}

# A column variable as a person reads it: its label in the source data
# (meta$labels, .with_source()), else its name.
.key_label <- function(m, key) {
  if (length(key) != 1L || is.na(key)) return(NULL)
  l <- m$labels[[key]]
  if (length(l) == 1L && !is.na(l) && nzchar(l)) l else key
}

# tokens a template names that the ARD lacks
.missing_stats <- function(templates, stats) {
  tok <- unlist(regmatches(templates, gregexpr("[{][^{}:]+", templates)))
  tok <- unique(substring(tok, 2L))
  setdiff(tok, c(stats, "col", "col1", "col2", "n:sum"))
}

#' Add a preset's rows to a report
#'
#' @param x An `tflplanner`.
#' @param output_id The report (`NA` for the study defaults).
#' @param preset A name of [cell_presets()] or [header_presets()].
#' @param variable For a cell preset: the variable its rows are for,
#'   instead of the kind (`continuous` / `categorical`) that serves every
#'   variable of that kind.
#' @return The `tflplanner`.  A report's own column header replaces the
#'   default one whole, so a header preset replaces the report's header
#'   rows.
#' @noRd
add_preset <- function(x, output_id, preset, variable = NULL) {
  if (preset %in% names(cell_presets())) {
    sheet <- "cells"
    new <- cell_presets()[[preset]]
    if (!is.null(variable) && nzchar(variable)) new$variable <- variable
    rows <- sheet_rows(x, sheet, output_id)
    rows$output_id <- NULL
    key <- function(d) paste(d$variable, d$context %||% NA, d$row)
    new <- .normalize_sheet(new, sheet)
    new$output_id <- NULL
    rows <- rbind(rows[!key(rows) %in% key(new), , drop = FALSE], new)
  } else if (preset %in% names(header_presets())) {
    sheet <- "col_header"
    rows <- .normalize_sheet(header_presets()[[preset]], sheet)
    rows$output_id <- NULL
  } else {
    stop("No preset '", preset, "'.", call. = FALSE)
  }
  set_sheet_rows(x, sheet, output_id, rows)
}

# -------------------------------------------------------------- choices

# What a sheet's columns may offer as a dropdown, from an ARD's metadata.
# Typing anything else stays allowed.
grid_choices <- function(sheet, meta) {
  keys <- names(meta$keys)
  vars <- meta$variables$variable
  if (is.null(meta)) keys <- vars <- character()
  stats <- meta$stats
  cp <- cell_presets()
  switch(sheet,
    tables = list(
      cols = keys,
      rows = c("group = variable", paste0("group1 = ", keys)),
      label = c(paste0("label = ", keys), "label = .label"),
      sort = c(".overall | group1 | .depth | -n | label", "FALSE"),
      stats = c("cells", "rows"), value = c("stat", "stat_fmt")),
    variables = list(variable = unique(c(keys, vars))),
    cells = list(
      variable = unique(c("continuous", "categorical", vars)),
      context = meta$contexts,
      row = unique(unlist(lapply(cp, `[[`, "row"))),
      template = {
        t <- unique(unlist(lapply(cp, `[[`, "template")))
        if (length(stats)) t[!vapply(t, function(z)
          length(.missing_stats(z, stats)) > 0L, NA)] else t
      },
      digits = NULL),
    columns = list(column = unique(c("row_label", ".values", keys))),
    col_header = list(
      cols = c("row_label", ".values", "row_label | .values", "3:last"),
      span = c("each", keys),
      text = NULL),
    list())
}

# ------------------------------------------------------------ defaults

# A report's rows of a sheet replace the study defaults by key; these are
# the default rows a report still inherits.
.default_keys <- list(tables = NULL, layout = NULL, style = NULL,
                      report = NULL, page = NULL,
                      variables = "variable",
                      cells = c("variable", "context", "row"),
                      columns = "column", cell_styles = NA,
                      col_header = NA,
                      header = "line", footer = "line", titles = "line",
                      footnotes = "line")

#' The study default rows a report inherits
#'
#' @param x An `tflplanner`.
#' @param sheet A sheet.
#' @param output_id The report.
#' @return The default rows (blank `output_id`) that still apply to the
#'   report: those it has no row of its own for.  On a sheet with one row
#'   per report (`tables`, `layout`, ...) that is the default row, whose
#'   cells the report's own non-blank cells override.
#' @noRd
inherited_rows <- function(x, sheet, output_id) {
  d <- x$sheets[[sheet]]
  def <- d[is.na(d$output_id), , drop = FALSE]
  own <- d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
  # a report's page pattern: its rows over Standard's are what it inherits
  pat <- if (sheet %in% .pattern_sheets) report_pattern(x, output_id) else NA
  if (!is.na(pat)) {
    prow <- d[d$output_id %in% paste0("@", pat), , drop = FALSE]
    k0 <- .default_keys[[sheet]]
    if (nrow(prow)) {
      if (is.null(k0)) {
        row <- if (nrow(def)) def[1L, , drop = FALSE] else prow[1L, , drop = FALSE]
        for (cn in setdiff(names(prow), "output_id")) {
          if (!is.na(prow[[cn]][1L])) row[[cn]] <- prow[[cn]][1L]
        }
        row$output_id <- NA_character_
        def <- row
      } else {
        kk <- function(z) do.call(paste, c(lapply(k0, function(c) z[[c]]), sep = "\r"))
        def <- rbind(prow, def[!kk(def) %in% kk(prow), , drop = FALSE])
      }
    }
  }
  k <- .default_keys[[sheet]]
  if (is.null(k) || !nrow(own)) return(def)
  if (identical(k, NA)) return(def[0, , drop = FALSE])
  key <- function(z) do.call(paste, c(lapply(k, function(c) z[[c]]),
                                      sep = "\r"))
  def[!key(def) %in% key(own), , drop = FALSE]
}
