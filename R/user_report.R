# A report's steps of its own code at run time (R/step_code.R says what
# they are): the content step leaves `content` -- a data frame, rtftable
# pages, a ggplot (made an rtfplot here), rtfplot figures, or a list of
# them -- and the report spec dresses it as any other report: page,
# running header and footer, titles, footnotes.  The data step leaves
# `data` (a figure: `df`).

# The small functions a report's own code is checked with, written once in
# the study's programs/tfl/fig_setup.R (which those reports source):
# report_content() -- a ggplot (alone or in a list) becomes a figure;
# anything else than the contract's -- a data frame, rtftable pages, a
# ggplot, an rtfplot, or a list of them -- stops, saying which item it is
# and what it was; report_data() -- the data step left a data frame.
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
  "}",
  "",
  "# ---- a figure at its size on the page (the report's, step 3): drawn so,",
  "# its text the size its theme says (a ggplot; anything else as it is)",
  "fig_sized <- function(x, width_in, height_in, dpi = 300) {",
  "  one <- function(p) if (inherits(p, \"ggplot\")) rtfplot(p,",
  "    width_twips = round(width_in * 1440), height_twips = round(height_in * 1440),",
  "    render_width = width_in, render_height = height_in, render_dpi = dpi) else p",
  "  if (inherits(x, \"ggplot\") || !is.list(x) || is.data.frame(x)) one(x) else lapply(x, one)",
  "}",
  "",
  "# ---- a data step of your own code: it left a data frame",
  "report_data <- function(x, what = \"data\") {",
  "  if (!is.data.frame(x)) {",
  "    stop(sprintf(\"The data step's code leaves `%s` as %s: make it a data frame.\",",
  "                 what, class(x)[1L]), call. = FALSE)",
  "  }",
  "  x",
  "}")

#' Run a report's own code and see what it leaves
#'
#' Runs the report's program up to its report (`data_lines()`: the datasets
#' it registers and its ARD read, its data step and its content step, each
#' its own code or as its definition makes it) in a fresh R process from
#' the study folder -- the app does not run the code itself -- and gives
#' what it left as `content`, for a preview.
#'
#' @param study An `rtfstudy`.
#' @param output_id The report (a step of it its own code, [step_code()]).
#' @param timeout Seconds to allow.
#' @return A list: `content` (or `NULL`), `error` (the code's error, or
#'   `NULL`), `code_step` and `code_line` (the step and the line of its
#'   code the error is on, when it is in the report's own code) and `log`.
#' @export
preview_code <- function(study, output_id, timeout = 300) {
  code <- data_lines(study$planner, output_id, todo = FALSE)
  if (is.null(code)) stop("The report has no code yet.", call. = FALSE)
  code <- c(.report_id_line(output_id), code)
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
  # the step and the line of the report's own code the error is on (the
  # program's line, less the lines before the code)
  res$code_step <- NA_character_
  res$code_line <- NA_integer_
  ln <- res$line %||% NA_integer_
  x <- study$planner
  for (step in c("data", "content")) {
    own <- step_code(x, output_id, step)
    if (is.na(own)) next
    start <- which(startsWith(code, "# your code: leaves `"))
    start <- start[if (identical(step, "data")) 1L else length(start)]
    n_own <- length(strsplit(own, "\n", fixed = TRUE)[[1L]])
    if (length(start) && !is.na(start) && !is.na(ln) && ln > start && ln <= start + n_own) {
      res$code_step <- step
      res$code_line <- ln - start
    }
  }
  res
}

# What a report's ARD reads, for the switch above its code: "import" (the
# ARD taken in, `file`), "own" (its analyses, `n` of them, `built` when the
# study ARD has its rows), "table" (a figure's: that table's, `id`) or
# "none"
.code_ard_state <- function(study, output_id) {
  x <- study$planner
  f <- .ard_import_of(x, output_id)
  if (!is.null(f)) return(list(kind = "import", file = f))
  src <- .fig_ard_source(x, output_id)
  if (identical(src$kind, "table")) return(list(kind = "table", id = src$id))
  n <- nrow(ard_rows(x, "analyses", output_id))
  if (!n) return(list(kind = "none"))
  rows <- tryCatch(study_ard_rows(study, output_id), error = function(e) NULL)
  list(kind = "own", n = n, built = !is.null(rows) && nrow(rows) > 0L)
}

# What a preview shows of a report's content: its table pages (rtftable; a
# data frame laid out as rtfreporter does by default) and its figures (each
# one's PNG bytes, kept by preview_code())
.code_preview_parts <- function(content) {
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
