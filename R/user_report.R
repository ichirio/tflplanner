# The fourth kind of report: user code (k-3).  The report's own code
# leaves `content` -- a data frame, rtftable pages, a ggplot (made an
# rtfplot here), rtfplot figures, or a list of them -- and the report spec
# dresses it as any other report: page, running header and footer, titles,
# footnotes.  It reads the datasets the report names (the data catalog),
# and, when the report says so, its ARD (`ard`: the study ARD's rows for
# it, or the ARD taken in for it), as a table does.

# Does a user-code report read its ARD?
.user_reads_ard <- function(x, output_id) {
  f <- lf_rows(x, "figures", output_id)
  nrow(f) > 0L && "ard" %in% names(f) && isTRUE(as.logical(f$ard[1L]))
}

# The small function a report of one's own code makes `content` with,
# written once in the study's programs/tfl/fig_setup.R (which those reports
# source): a ggplot (alone or in a list) becomes a figure; anything else
# than the contract's -- a data frame, rtftable pages, a ggplot, an
# rtfplot, or a list of them -- stops, saying which item it is and what it
# was.
.report_content_fun <- c(
  "# ---- a report of your own code: its `content` as rtfreporter takes it",
  "# (a ggplot becomes a figure; what is not a data frame, rtftable pages, a",
  "# ggplot or an rtfplot, or a list of them, stops, saying which item)",
  "report_content <- function(x) {",
  "  ok <- function(p) is.data.frame(p) ||",
  "    inherits(p, c(\"rtftable\", \"rtfplot\", \"ggplot\"))",
  "  fig <- function(p) if (inherits(p, \"ggplot\")) rtfplot(p) else p",
  "  one <- ok(x)",
  "  items <- if (one) list(x) else if (is.list(x)) x else list(x)",
  "  for (i in seq_along(items)) if (!ok(items[[i]])) {",
  "    stop(sprintf(\"`content`%s is %s: make it a data frame, rtftable pages, a ggplot or an rtfplot (or a list of them).\",",
  "                 if (one || !is.list(x)) \"\" else sprintf(\" (item %d)\", i),",
  "                 class(items[[i]])[1L]), call. = FALSE)",
  "  }",
  "  if (one) fig(x) else lapply(items, fig)",
  "}")

# the part of a user-code report's program between its setup and its report
.user_lines <- function(x, output_id, info) {
  f <- lf_rows(x, "figures", output_id)
  ds <- if (nrow(f)) .split_bar(f$datasets[1L]) else character()
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  code <- if (nrow(o) && !is.na(o$data_code)) o$data_code else NA_character_
  ard <- if (.user_reads_ard(x, output_id)) {
    imp <- .ard_import_of(x, output_id)
    a <- if (is.null(imp)) .fill_template("table_data", x, output_id) else
      .imported_ard_code(imp, output_id)
    if (!is.na(a)) c("# ---- this report's ARD (`ard`)", .code_block(a), "")
  }
  body <- if (is.na(code)) c(
    "# TODO: your code, leaving `content`, e.g.",
    "#   content <- adsl[, c(\"USUBJID\", \"AGE\")]",
    paste0("stop(\"tflplanner: the code of ", info$program,
           " is still to be written.\")")) else .code_block(code)
  c(if (length(ds)) c(paste0("# the data: ", paste(ds, collapse = ", "),
                             " (data catalog)"),
                      unlist(lapply(ds, function(d) .read_dataset_lines(x, d))),
                      ""),
    ard,
    "# the figure style of the company standards (theme_tfl(), tfl_check() ...)",
    sprintf("source(%s)", encodeString(file.path(study_layout()[["programs_tfl"]],
                                                 .fig_setup_file), quote = "\"")),
    "",
    "# ---- the report's own code: leaves `content`",
    body,
    "",
    "content <- report_content(content)")
}

#' Run a user-code report's code and see what it leaves
#'
#' Runs the report's data part ([data_lines()]: the datasets it reads, its
#' ARD when it reads one, its own code) in a fresh R process from the study
#' folder -- the app does not run the code itself -- and gives what it left
#' as `content`, for a preview.
#'
#' @param study An `rtfstudy`.
#' @param output_id The report (type `user`).
#' @param timeout Seconds to allow.
#' @return A list: `content` (or `NULL`), `error` (the code's error, or
#'   `NULL`) and `log`.
#' @export
preview_user <- function(study, output_id, timeout = 300) {
  code <- data_lines(study$planner, output_id, todo = FALSE)
  if (is.null(code)) stop("The report has no code yet.", call. = FALSE)
  tmp <- tempfile("user")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f_code <- file.path(tmp, "code.R")
  f_helper <- file.path(tmp, "helper.R")
  writeLines(.report_content_fun, f_helper)
  # the functions the programs call (programs/study_helpers.R's)
  f_helpers <- file.path(tmp, "study_helpers.R")
  writeLines(tflspec::tfl_helpers_code(), f_helpers)
  f_out <- file.path(tmp, "out.rds")
  writeLines(enc2utf8(code), f_code, useBytes = TRUE)
  q <- function(x) encodeString(normalizePath(x, "/", FALSE), quote = "\"")
  script <- c(
    "suppressPackageStartupMessages(library(rtfreporter))",
    paste0("sys.source(", q(f_helpers), ", envir = globalenv())"),
    ".e <- new.env(parent = globalenv())",
    # report_content() as the study's next fig_setup.R has it (the one on
    # disk may be older: written before it was there)
    paste0("sys.source(", q(f_helper), ", envir = globalenv())"),
    ".res <- list(content = NULL, error = NULL, line = NA_integer_)",
    paste0(".x <- parse(", q(f_code), ", encoding = \"UTF-8\", keep.source = TRUE)"),
    ".res$content <- tryCatch({",
    "  for (.i in seq_along(.x)) {",
    "    .res$line <- utils::getSrcLocation(attr(.x, \"srcref\")[[.i]], \"line\")[1L]",
    "    eval(.x[[.i]], envir = .e)",
    "  }",
    "  .res$line <- NA_integer_",
    "  get0(\"content\", .e, inherits = FALSE)",
    "}, error = function(e) { .res$error <<- conditionMessage(e); NULL })",
    # a figure's image lives in this process's temp folder: keep its bytes
    ".png <- function(it) {",
    "  if (inherits(it, \"ggplot\")) it <- rtfreporter::rtfplot(it)",
    "  if (!inherits(it, \"rtfplot\") || !file.exists(it$path)) return(it)",
    "  structure(list(png = readBin(it$path, \"raw\", file.size(it$path))), class = \"uc_png\")",
    "}",
    ".c <- .res$content",
    "if (inherits(.c, c(\"ggplot\", \"rtfplot\"))) .res$content <- .png(.c) else",
    "  if (is.list(.c) && !is.data.frame(.c) && !inherits(.c, \"rtftable\"))",
    "    .res$content <- lapply(.c, .png)",
    paste0("saveRDS(.res, ", q(f_out), ")"))
  f_script <- file.path(tmp, "run.R")
  writeLines(script, f_script)
  px <- processx::run(file.path(R.home("bin"), "Rscript"), f_script,
                      wd = study$path, error_on_status = FALSE,
                      timeout = timeout, stderr_to_stdout = TRUE)
  res <- if (file.exists(f_out)) readRDS(f_out) else
    list(content = NULL, error = .first_error(px$stdout) %||% "The code did not run.")
  res$log <- px$stdout
  # the line of the report's own code the error is on (the program's line,
  # less the lines before the code)
  start <- match("# ---- the report's own code: leaves `content`", code)
  n_own <- length(strsplit(.user_code_of(study$planner, output_id) %||% "", "\n",
                           fixed = TRUE)[[1L]])
  ln <- res$line %||% NA_integer_
  res$code_line <- if (!is.na(start) && !is.na(ln) && ln > start &&
                       ln <= start + n_own) ln - start else NA_integer_
  res
}

# a user-code report's own code (its data code), or NULL
.user_code_of <- function(x, output_id) {
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  if (nrow(o) && !is.na(o$data_code)) o$data_code
}

# What a user-code report's ARD switch reads: "import" (the ARD taken in,
# `file`), "own" (its analyses, `n` of them, `built` when the study ARD has
# its rows) or "none"
.user_ard_state <- function(study, output_id) {
  x <- study$planner
  f <- .ard_import_of(x, output_id)
  if (!is.null(f)) return(list(kind = "import", file = f))
  n <- nrow(ard_rows(x, "analyses", output_id))
  if (!n) return(list(kind = "none"))
  rows <- tryCatch(study_ard_rows(study, output_id), error = function(e) NULL)
  list(kind = "own", n = n, built = !is.null(rows) && nrow(rows) > 0L)
}

# What a preview shows of a user-code report's content: its table pages
# (rtftable; a data frame laid out as rtfreporter does by default) and its
# figures (each one's PNG bytes, kept by preview_user())
.user_preview_parts <- function(content) {
  items <- if (inherits(content, c("rtftable", "rtfplot", "uc_png", "data.frame")) ||
               !is.list(content)) list(content) else content
  pages <- list()
  figs <- list()
  for (it in items) {
    if (inherits(it, "rtftable")) pages <- c(pages, list(it))
    else if (is.data.frame(it)) pages <- c(pages, list(structure(
      list(data = it, col_header = list(names(it)), blank_rows = integer()),
      class = "rtftable")))
    else if (inherits(it, "uc_png")) figs <- c(figs, list(it$png))
  }
  list(pages = pages, figures = figs)
}

#' Make a figure written by hand a user-code report
#'
#' A figure with no design whose plot is its data code becomes a report of
#' the type `user`: the same datasets, the same code, with
#' `content <- plot` added when the code leaves `plot` (and no `content`).
#' Its program is then written again (it is "to be made again").
#'
#' @param x A `tflplanner`.
#' @param output_id The figure.
#' @return The `tflplanner`.
#' @export
make_user_report <- function(x, output_id) {
  info <- report_info(x, output_id)
  if (!identical(info$type, "figure") || !is.null(fig_design(x, output_id))) {
    stop(output_id, " is not a figure written by hand.", call. = FALSE)
  }
  i <- match(output_id, x$outputs$output_id)
  code <- x$outputs$data_code[i]
  if (!is.na(code)) {
    lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    leaves <- function(nm) any(grepl(sprintf("(^|[^A-Za-z0-9_.])%s[[:space:]]*(<-|=)[^=]", nm), lines))
    if (!leaves("content") && leaves("plot")) {
      # as its figure program did: the figure checks, then the plot
      x$outputs$data_code[i] <- paste(c(sub("\n+$", "", code), "tfl_check(plot)",
                                        "content <- plot"), collapse = "\n")
    }
  }
  .set_report_type(x, output_id, "user")
}

# The figures written by hand that could be user-code reports
.hand_figures <- function(x) {
  o <- x$outputs
  ids <- o$output_id[!is.na(o$data_code)]
  ids[vapply(ids, function(id) identical(report_info(x, id)$type, "figure") &&
                is.null(fig_design(x, id)), NA)]
}
