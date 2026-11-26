# The report programs: one per report, and autoexec_report.R to run them.
#
# A program does four things and only the first is the study's own: make
# its data, read the report's definition, lay the report out from it, write
# the RTF.  How the report looks lives in the workbooks, so a change of
# layout is a change of workbook, not of program.
#
# The text is tflspec's (tflspec::tfl_report_program(), tflspec#910 G5-1c):
# tflplanner reads each report's values from the planner (.report_values()).

# A one-row sheet (report, page) resolved for one report the way
# rtfreporter resolves it: the report's own cells over the default row,
# cell by cell.
.resolve_row <- function(x, sheet, output_id) {
  d <- x$sheets[[sheet]]
  row <- d[is.na(d$output_id), , drop = FALSE][1L, , drop = FALSE]
  own <- d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
  if (nrow(own)) {
    for (cn in names(own)) if (!is.na(own[[cn]][1L])) row[[cn]] <- own[[cn]][1L]
  }
  as.list(row)
}

.fill_id <- function(x, id) gsub("{output_id}", id, x, fixed = TRUE)

#' Where a report's program and RTF go
#'
#' @param x An `tflplanner`.
#' @param output_id A report id.
#' @return A list: `type`, `program` (file name, with `.R`), `file` (the
#'   RTF, joined to `output_path`).
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' s <- create_sample_study(run = FALSE)
#' report_info(s$planner, "T-14-1-1")  # the program and the RTF of a report
#' }
#' options(old)
#' @export
report_info <- function(x, output_id) {
  .stop_part(output_id)
  r <- .resolve_row(x, "report", output_id)
  type <- if (is.na(r$type)) "table" else r$type
  prog <- .fill_id(if (is.na(r$program)) "{output_id}" else r$program,
                   output_id)
  prog <- basename(prog)
  if (!grepl("\\.[Rr]$", prog)) prog <- paste0(prog, ".R")
  file <- .fill_id(if (is.na(r$file)) "{output_id}.rtf" else r$file,
                   output_id)
  out <- x$study[["output_path"]]
  list(type = type, program = prog,
       file = if (is.na(out)) file else file.path(out, file))
}

# The datasets a report reads, as the report list shows them: a table the
# data of its ARD analyses (each analysis's dataset, and its population's;
# an analysis data's: those it is made from),
# a listing its dataset, a figure the datasets its design reads (or, without
# a design, the ones it registers); a step of its own code, the ones it
# registers (report_datasets()).  In the order first used.
.report_datasets <- function(x, output_id, type = report_info(x, output_id)$type) {
  ds <- switch(type,
    table = {
      a <- x$ard$analyses
      a <- a[!is.na(a$output_id) & a$output_id == output_id, , drop = FALSE]
      po <- x$ard$populations
      pop_ds <- po$dataset[match(a$population_id, po$population_id)]
      # one that reads an analysis data: the datasets it is made from
      ad <- .adata_rows(x, output_id)
      own <- unlist(lapply(seq_len(nrow(a)), function(i) {
        d <- c(a$data[i] %||% NA, a$denominator[i] %||% NA)
        if (any(d %in% ad$data_id)) {
          unlist(lapply(d[d %in% ad$data_id], .adata_datasets, ad = ad, po = po))
        } else c(if (is.na(a$dataset[i])) pop_ds[i] else a$dataset[i], pop_ds[i])
      }))
      own %||% character()
    },
    listing = {
      l <- x$lf$listings
      l$dataset[!is.na(l$output_id) & l$output_id == output_id]
    },
    figure = {
      d <- (x$fig_designs %||% list())[[output_id]]
      steps <- d$data %||% list()
      read <- unlist(lapply(steps, function(s)
        if (isTRUE(s$step %in% c("read", "join"))) s$dataset))
      if (length(read)) read else report_datasets(x, output_id)
    },
    character())
  # a step of its own code: the ones it registers (a data step instead of
  # the definition's: those only)
  own <- c(.is_code_step(x, output_id, "data"), .is_code_step(x, output_id, "content"))
  if (own[1L]) ds <- report_datasets(x, output_id) else
    if (own[2L]) ds <- c(ds, report_datasets(x, output_id))
  ds <- trimws(as.character(ds))
  unique(ds[!is.na(ds) & nzchar(ds)])
}

# A report's title for the report list: its own title lines (not the
# running study lines), one after another.
.report_title <- function(x, output_id) {
  paste(.title_lines(x, output_id), collapse = " / ")
}

# The titles as one line each, for the program's banner.
.title_lines <- function(x, output_id) {
  d <- x$sheets$titles
  d <- d[is.na(d$output_id) | d$output_id == output_id, , drop = FALSE]
  if (!nrow(d)) return(character())
  # a report's own line replaces the default line of the same number
  d$line[is.na(d$line)] <- ""
  own <- !is.na(d$output_id)
  d <- d[own | !d$line %in% d$line[own], , drop = FALSE]
  d <- d[order(suppressWarnings(as.numeric(d$line))), , drop = FALSE]
  txt <- apply(d[c("left", "center", "right")], 1L, function(v)
    paste(stats::na.omit(v), collapse = "  "))
  txt <- txt[nzchar(txt)]
  # the running study lines ({PAGE}...) say nothing about this report
  txt[!grepl("{", txt, fixed = TRUE)]
}

.r_string <- function(x) encodeString(x, quote = "\"")

.code_block <- function(code) {
  code <- gsub("\r\n?", "\n", code)
  sub("\n+$", "", code)
}

.banner <- function(...) {
  bar <- strrep("=", 76)
  body <- c(...)
  c(paste("#", bar), ifelse(nzchar(body), paste0("#  ", body), "#"),
    paste("#", bar))
}

# Does this code assign `data` itself?  (Then it needs no default
# normalization after it.)
.makes_data <- function(code) {
  any(grepl("(^|[^A-Za-z0-9_.])data[[:space:]]*(<-|=)[^=]",
            strsplit(code, "\n", fixed = TRUE)[[1L]]))
}

# A table's ARD code: the company's template (code_templates: table_data)
# that takes its rows of the study ARD, or the ARD taken in.  (A data step
# of the report's own code: step_code().)
.ard_code_of <- function(x, output_id) {
  if (!identical(report_info(x, output_id)$type, "table")) return(NA_character_)
  imp <- .ard_import_of(x, output_id)
  if (is.null(imp)) .fill_template("table_data", x, output_id) else
    .imported_ard_code(imp, output_id)
}

# Is a table's normalization the built-in one, in its plan's pipe?  When
# nothing makes `data` -- no process code, the ARD code (or the data step's
# own code) leaves `ard`, the company's template is blank or left at
# `data <- normalize_ard(ard)` -- and the plan is the definition's (not a
# content step of its own code, which reads `data`).  `code` and `proc`:
# the report's ARD code and process code (by default, the report's).
.table_norm_builtin <- function(x, output_id, code = NULL, proc = NULL) {
  if (!identical(report_info(x, output_id)$type, "table")) return(FALSE)
  if (.is_code_step(x, output_id, "content")) return(FALSE)
  if (is.null(proc)) {
    o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
    proc <- if (nrow(o) && !is.na(o$process_code)) o$process_code else NA
  }
  if (!is.na(proc)) return(FALSE)
  if (is.null(code)) {
    code <- if (.is_code_step(x, output_id, "data")) step_code(x, output_id, "data") else
      .ard_code_of(x, output_id)
  }
  if (!is.na(code) && .makes_data(code)) return(FALSE)
  tp <- .fill_template("table_process", x, output_id)
  is.na(tp) || identical(trimws(tp), "data <- normalize_ard(ard)")
}

# A table's Rework as its program writes it: `data <- ard |>
# normalize_ard(...)` (the table's own arguments), then the code, which
# starts from `data` and leaves `data`.  The code is written as tflspec
# writes SPEC R into a program (.rework_code()): an old one that normalizes
# itself (`data <- normalize_ard(ard)`) overwrites the line before it with
# the same data.  `code` the table's ARD code: one that makes `data` itself
# has no `ard` to normalize.
.rework_lines <- function(x, output_id, code, proc) {
  c(if (is.na(code) || !.makes_data(code)) .rework_normalize_lines(x, output_id),
    .rework_code(proc))
}

# A Rework (a table's, a template listing's) as tflspec writes the data's
# steps into a program, one element a line (tflspec A1): one expression
# that assigns only `data` as it is, any other code in a scope of its own
# that leaves `data`, local({ }), so what else it assigns stays there
.rework_code <- function(proc) {
  .tflspec_fn(".content_steps_code")(.code_block(proc))
}

# The check tflspec makes of a Rework, in its words: it names `data`
# (code that does not parse is left to R, which says so when it runs)
.rework_need_data <- function(proc) {
  ok <- tryCatch("data" %in% all.names(parse(text = proc, keep.source = FALSE)),
                 error = function(e) TRUE)
  if (!ok) stop("the data.steps leave no `data`, the data the plan is made from.", call. = FALSE)
  invisible(TRUE)
}

# The lines before a Rework, as the table's plan would say them: the
# content's normalize_ard() with its arguments -- tflspec writes them (the
# lines before a marker step) -- else, with no spec to ask, the
# definitions' (.rework_normalize_sheets())
.rework_mark <- "data <- data  # rework"
.rework_normalize_lines <- function(x, output_id) {
  y <- .code_output(x, output_id)
  if (!is.null(y)) {
    o <- .code_content(y)
    o$data <- list(source = "ard", object = "ard", steps = list(tflspec::tfl_r(.rework_mark)))
    code <- tryCatch(tflspec::tfl_content_code(o, pipe = "|>", study = .content_study(o, y$study)),
                     error = function(e) NULL)
    i <- match(.rework_mark, code)
    if (!is.na(i) && i > 1L) return(code[seq_len(i - 1L)])
  }
  .rework_normalize_sheets(x, output_id)
}

# The same from the sheets: the steps before table_plan() in the pipe that
# tfl_table_code() writes for the table's plan (the same call as the
# program's, so the same arguments), as `data <- ard |> normalize_ard(...)`
.rework_normalize_sheets <- function(x, output_id) {
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  code <- tryCatch(
    tflspec::tfl_table_code(sp, output_id, pipe = "|>", ard_spec = .ard_spec_last(x$ard)),
    error = function(e) tryCatch(tflspec::tfl_table_code(sp, output_id, pipe = "|>"),
                                 error = function(e) NULL))
  i <- match("plan <- ard |>", code)
  j <- grep("^  table_plan\\(", code)[1L]
  if (is.na(i) || is.na(j) || j <= i + 1L) return("data <- ard |> normalize_ard()")
  steps <- code[(i + 1L):(j - 1L)]
  steps[length(steps)] <- sub(" \\|>$", "", steps[length(steps)])
  one <- paste("data <- ard |>", trimws(steps[1L]))
  if (length(steps) == 1L && nchar(one) <= 80L) one else c("data <- ard |>", steps)
}

# a report whose ARD was made elsewhere and taken in (ard_source =
# import:<file>) reads that file instead of the study ARD
.imported_ard_code <- function(file, output_id) {
  p <- encodeString(file.path("input", "ard", file), quote = "\"")
  id <- encodeString(output_id, quote = "\"")
  paste(
    sprintf("# ---- this output's ARD, made elsewhere and taken in (%s)",
            file.path("input", "ard", file)),
    sprintf("ard <- tflspec::tfl_read_ard(%s)", p),
    sprintf("if (\"output_id\" %%in%% names(ard)) ard <- ard[ard$output_id == %s, , drop = FALSE]", id),
    "ard <- ard[setdiff(names(ard), c(\"output_id\", \"analysis_id\", \"population_id\"))]",
    sprintf("if (!nrow(ard)) stop(\"The ARD taken in has no rows for %s.\")", output_id),
    sep = "\n")
}

#' Code templates
#'
#' The code tflplanner writes where a report says none: the company
#' standards' sheet `code_templates` (see [company_standards()]).
#' `table_data` takes a table's rows of the study ARD, `table_process`
#' normalizes them (and shows where to rework them), `figure_plot` is the
#' plot a figure starts with, `setup` the code every report of a new study
#' runs first.  In a template, `{OUTPUT_ID}`, `{ARD}` (the study ARD),
#' `{ARD_PROGRAM}` (the output's ARD program), `{PROGRAM}` and `{STUDY_ID}`
#' stand for the report's.
#'
#' @param name A template's name; `NULL` for all.
#' @return A named character vector.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' names(code_templates())  # the templates the company standards have
#' code_templates("table_data")
#' options(old)
#' @export
code_templates <- function(name = NULL) {
  d <- company_standards()$code_templates
  out <- stats::setNames(d$code, d$name)
  if (is.null(name)) out else out[name]
}

.fill_template <- function(name, x, output_id = NA, study_id = NA) {
  code <- code_templates(name)
  if (!length(code) || is.na(code)) return(NA_character_)
  lay <- study_layout()
  sub <- c(
    `{OUTPUT_ID}` = if (is.na(output_id)) "" else output_id,
    `{ARD}` = .study_ard_rel(x$ard),
    `{ARD_PROGRAM}` = if (is.na(output_id)) "" else
      file.path(lay[["programs_ard"]], .ard_prog_name(output_id)),
    `{PROGRAM}` = if (is.na(output_id)) "" else
      file.path(lay[["programs_tfl"]], report_info(x, output_id)$program),
    `{STUDY_ID}` = if (is.na(study_id)) "" else study_id)
  # the study ARD as a string of R: through its folder's variable while a
  # program is written
  code <- gsub("\"{ARD}\"", .path_lit(sub[["{ARD}"]]), code, fixed = TRUE)
  for (k in names(sub)) code <- gsub(k, sub[[k]], code, fixed = TRUE)
  # (the report's ARD by its own name, data.object: the content's code
  # starts from it, tflspec >= #482)
  obj <- if (!is.na(output_id) && name %in% c("table_data", "table_process")) .ard_object(x, output_id)
  if (!is.null(obj) && !identical(obj, "ard")) {
    code <- gsub("(?<![[:alnum:]_./])ard(?![[:alnum:]_./])", obj, code, perl = TRUE)
  }
  .drop_attached(unname(code))
}

# The name a report's program gives its ARD: its data.object (the report
# sheet's ard_object), "ard" when it says none
.ard_object <- function(x, output_id) {
  o <- x$spec$outputs[[output_id]]
  v <- if (is.list(o) && is.list(o$data)) o$data$object
  v <- as.character(v %||% "ard")[1L]
  if (is.na(v) || !nzchar(v) || make.names(v) != v) "ard" else v
}

#' The data part of a report's program
#'
#' The code a report runs before the report is laid out: the setup every
#' report runs, then its data and its content as its definition makes them
#' or as its own code does ([step_code()]: the datasets it registers, its
#' ARD as `ard`, the code) -- a table's ARD and its `process_code`
#' (normalization and rework; by default none: the table's plan normalizes
#' `ard`).
#' With nothing to make it, a TODO that stops.  [program_code()] writes it
#' into the program, and [fetch_ard()] runs it to learn what the ARD holds.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param todo `FALSE` returns `NULL` instead of the TODO.
#' @return Code lines.
#' @noRd
data_lines <- function(x, output_id, todo = TRUE) {
  .report_program(x, output_id, part = "data", todo = todo)
}

# A table of records (its YAML's `data: {source: dataset, ...}`): the code
# that makes its `data` -- the dataset, or the report's analysis data, its
# analysis set's subjects, its condition, its order -- as a listing's data
# part is written (tflspec::tfl_listing_code(part = "data")); NULL for a
# table of the ARD
.table_dataset_lines <- function(x, output_id) {
  sp <- .code_spec(x)
  o <- sp$outputs[[output_id]]
  if (is.null(o) || inherits(o$content, "tfl_r")) return(NULL)
  # the objects it reads but its ARD (written from the study's ARD program):
  # its records as `data`, a part's from a dataset by its name
  # (tflspec::tfl_data_code(), the listing's data code); NULL: none
  dc <- .table_objects_code(o, sp$study)
  if (!length(dc)) NULL else unlist(dc, use.names = FALSE)
}

# A listing made from a template (`template: {name: listing, ...}`) that is
# not a code report, or one converted to code from it (content kept): its
# program and its preview are the content (tflspec::tfl_content_code()),
# not the listing sheets'
.is_template_listing <- function(x, output_id) {
  if (!identical(report_info(x, output_id)$type, "listing")) return(FALSE)
  o <- tryCatch(.code_spec(x)$outputs[[output_id]], error = function(e) NULL)
  # (one converted to code keeps the template's content, so its pages are
  # still the content's: a listing of records with a `plan_listing` item and
  # items the listing sheets cannot say (`page_by` ...): the sheets' program
  # would drop them, the content's has them)
  from_template <- is.list(o$template) && !is.null(o$template$name)
  converted <- !is.null(o) && is.null(o$template) && identical(o$data[["source"]], "dataset") &&
    any(vapply(o$content %||% list(), function(it) identical(it$fn, "plan_listing"), NA)) &&
    any(nzchar(trimws(stats::na.omit(lf_rows(x, "listings", output_id)$content_more))))
  !is.null(o) && !inherits(o$content, "tfl_r") && (from_template || converted) &&
    !.is_code_step(x, output_id, "data") && !.is_code_step(x, output_id, "content")
}

# ... by object, its ARD left out: list(data = <lines>, adpc = <lines>)
.table_objects_code <- function(o, study) {
  # (a template report: its records -- a listing's, a table of records
  # such as pk_conc's -- are the template's, once it is resolved)
  if (is.list(o$template)) {
    o <- tflspec::tfl_resolve_output(o, study)
  }
  own <- identical(o$data[["source"]], "dataset")
  parts <- any(vapply(o$content %||% list(), function(it) identical(it$fn, "plan_append"), NA))
  if (!own && !parts) return(list())
  tflspec::tfl_data_code(o, study, ard = FALSE)
}

#' The R program for one report
#'
#' A program runs with the study folder as its working directory (see
#' [study_layout()]) and names every file relative to it.  What it does
#' depends on the report's `type`:
#'
#' * `table` -- the data part leaves `ard`, this report's ARD (or `data`,
#'   when its process code or the company's template normalizes it); the
#'   program plans the table as its file (`spec/outputs/<id>.yml`)
#'   defines it, written out as
#'   `ard |> normalize_ard() |> table_plan() |> plan_*()` from its content's
#'   items ([tflspec::tfl_content_code()]; or `table_plan(data)`).
#' * `listing`, `figure` -- the data part leaves `content`: `rtftable`
#'   pages for a listing, the figures for a figure.
#'
#' The report around it -- page, header, footer, titles, footnotes -- is
#' written out from the report's `page:` (its own, its pattern's, the
#' study's defaults) for every type, as rtfreporter calls
#' ([tflspec::tfl_report_code()]).  The program does not read the spec:
#' generate it again after changing it.  (A planner without a YAML spec --
#' a new study not yet saved -- or with edits not yet saved: from its
#' sheets, the same code.)
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' s <- create_sample_study(run = FALSE)
#' code <- program_code(s$planner, "T-14-1-1")  # one element a line
#' head(code, 6)
#' }
#' options(old)
#' @export
program_code <- function(x, output_id, date = Sys.Date()) {
  # as the study's setup has it: its folders' variables, its packages
  .with_study_code(.program_code(x, output_id, date))
}

.program_code <- function(x, output_id, date = Sys.Date()) {
  .report_program(x, output_id, date = date)
}

# ---- the report programs are made by tflspec (tflspec#910, G5-1c) ----------
#
# tflplanner reads a report's values from the planner -- its sheets, its
# figure design, its own code, its YAML spec when the view is in step with
# it -- and tflspec makes the program's text from them
# (tflspec::tfl_report_program(); the ARD programs: tfl_ard_program()).

# A report's program, or its data part (`part = "data"`), made by tflspec
# in the code context the session has now (the study's, while
# .with_study_code() runs; else none)
.report_program <- function(x, output_id, date = Sys.Date(), part = "program",
                            todo = TRUE) {
  ctx <- .live_context_args()
  tflspec::tfl_report_program(
    .report_values(x, output_id), study = .code_spec(x)$study,
    shared = .report_shared(x, output_id), standards = .report_standards(),
    attached = ctx$attached, study_code = ctx$study_code,
    series_dir = .program_series_dirs(), layout = study_layout(), date = date,
    generator = .setup_generator(), part = part, todo = todo)
}

# The code context in force now, as tflspec's makers take it: the packages
# the study's setup attaches, and whether its programs are written as the
# study's (its folders' variables, its setup's rounding and data)
.live_context_args <- function() {
  list(attached = as.character(getOption("tflspec.attached")),
       study_code = length(getOption("tflspec.paths")) > 0L ||
         isTRUE(getOption("tflspec.study_setup")))
}

# The folders the template series are looked for in before tflspec's own:
# a folder set by option, then the tflplanner home's (where tflspec looks
# itself outside a program's making)
.program_series_dirs <- function() {
  c(getOption("tflspec.series_dir"),
    file.path(tools::R_user_dir("tflplanner", "data"), "standards", "series"))
}

# The company's inputs a report or ARD program reads
.report_standards <- function() {
  s <- company_standards()
  list(code_templates = s$code_templates, ard_statistics = .std_ard_statistics(),
       ard_methods = .std_ard_methods(), settings = s$settings)
}

# The values a report's program is made from (tflspec::tfl_report_program()'s
# `report`).  A figure's size on the page is worked out only when its
# program draws it.
.report_values <- function(x, output_id) {
  info <- report_info(x, output_id)
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  type <- info$type
  out <- list(
    id = output_id, type = type, program = info$program, file = info$file,
    description = if (nrow(o) && !is.na(o$description)) o$description,
    titles = .title_lines(x, output_id),
    steps = list(data = step_code(x, output_id, "data"),
                 content = step_code(x, output_id, "content")),
    datasets = report_datasets(x, output_id),
    process = if (nrow(o) && !is.na(o$process_code)) o$process_code else NA_character_,
    ard_import = .ard_import_of(x, output_id),
    ard_object = .ard_object(x, output_id),
    output = .code_output(x, output_id)$output,
    spec_output = .code_spec(x)$outputs[[output_id]],
    template_listing = .is_template_listing(x, output_id),
    listing = NULL, design = NULL, ard_source = list(kind = "none"))
  if (identical(type, "listing")) {
    # (a listing of the report's analysis data, or of an analysis set's
    # subjects: its program starts with their part of the ARD definition;
    # its set as the report's one reader says it, report_population(): its
    # row's, else the report's -- the set tflspec writes onto its data; S5)
    x$lf <- .lf_with_report_set(x, output_id)
    l <- lf_rows(x, "listings", output_id)
    if (nrow(l)) {
      out$listing <- list(own = any(!is.na(c(l$data[1L] %||% NA, l$population_id[1L] %||% NA))))
    }
  }
  if (identical(type, "figure")) {
    out$design <- fig_design(x, output_id)
    src <- .fig_ard_source(x, output_id)
    src$step <- .step_no("ard")
    out$ard_source <- src
    ad <- .adata_rows(x, output_id)
    out$adata <- list(ids = ad$data_id, reads = unique(unlist(lapply(ad$data_id, function(i)
      .adata_datasets(ad, i, x$ard$populations)))))
    out$fig_codelists <- function() .fig_codelists(x, output_id)
    out$size <- function() .fig_size_of(x, output_id)
    # (written from the sheets -- no spec, or edits it has not taken in --
    # the figure's analysis set is still its data's `population:` as the
    # one reader says it, report_population(): tflspec makes its subjects
    # from it, as the designer's code view does, .fig_rp_context(); A2-2)
    pop <- report_population(x, output_id)
    if (is.null(out$spec_output) && !is.na(pop)) {
      out$spec_output <- list(data = list(population = pop))
    }
  }
  out
}

# The values the reports of a study share (tflspec::tfl_report_program()'s
# `shared`); a spec of the sheets is made only for a report that needs it.
# `output_id` the report the program is for: a listing's set on its row
# (.lf_with_report_set(); S5)
.report_shared <- function(x, output_id = NULL) {
  list(
    setup = x$setup,
    report_setup = function() .uses_report_setup(x),
    ard = structure(x$ard, class = "tfl_ard_spec"),
    ard_spec = function() .ard_spec_last(x$ard),
    study_ard = .study_ard_rel(x$ard),
    codelists = function() .study_codelists(x),
    listings = function() tflspec::tfl_listing_spec(
      if (is.null(output_id)) x$lf else .lf_with_report_set(x, output_id), check = FALSE),
    sheets = function() .spec_object_last(x, c(table_sheets(), report_sheets()),
                                          unique(c(.study_keys$table, .study_keys$report))),
    table_sheets = function() .spec_object_last(x, table_sheets(), .study_keys$table))
}

# A table's normalization for the builder's ARD preview (fetch_ard()) when
# the table says none: the line its definitions say -- the hierarchy of its
# hierarchical analysis, the any-event row (tables$overall_label):
# tflspec::tfl_normalize_code()
.normalize_line <- function(x, output_id) {
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  tryCatch(tflspec::tfl_normalize_code(sp, .ard_spec_last(x$ard), output_id),
           error = function(e) "data <- normalize_ard(ard)")
}

# The whole study's table and report spec, kept for the next report of
# the same definition: writing or checking every report's program (saving
# a study, study_status()) built it once per report -- 0.2 s each, a minute
# for 200 reports -- though it is the same for all of them
.spec_cache <- new.env()
.spec_object_last <- function(x, sheets, keys) {
  key <- list(x, sheets, keys)
  if (!is.null(.spec_cache$key) && identical(.spec_cache$key, key)) return(.spec_cache$sp)
  if ("tokens" %in% sheets) x <- .with_population_tokens(x)
  sp <- .spec_object(x, sheets, keys)
  .spec_cache$key <- key
  .spec_cache$sp <- sp
  sp
}

# The ARD definition of the planner's sheets, kept for the next report of the
# same sheets (#738): writing every report's program read it once per table
# report (0.1 s each).  Cache key: the ARD sheets and the company's
# statistics and methods it is read with.  A definition that does not hold
# is not kept: its error comes again each time.
.ard_spec_cache <- new.env(parent = emptyenv())
.ard_spec_last <- function(ard) {
  key <- list(ard, .std_ard_statistics(), .std_ard_methods())
  k <- .ard_spec_cache
  if (!is.null(k$key) && identical(k$key, key)) return(k$spec)
  spec <- .ard_spec(ard)
  k$key <- key
  k$spec <- spec
  spec
}

# {OUTPUT_POPULATION} and {OUTPUT_SECTION} by default: the label of the
# report's analysis set (step 1) and its section (the report list's), for a
# report no row of the tokens sheet -- its own, its pattern's, Standard's --
# says one for.  A TOC taken in is a row and wins.  Not written to the
# sheets: the programs, the page's sample and step 3 see them.
.with_population_tokens <- function(x) {
  o <- x$outputs
  if (is.null(o) || !nrow(o)) return(x)
  add <- list()
  put <- function(id, name, value) {
    if (.is_blank(value) || name %in% page_tokens_from(x, id)$name) return()
    add[[length(add) + 1L]] <<- data.frame(output_id = id, name = name, value = value,
                                           stringsAsFactors = FALSE)
  }
  for (i in seq_len(nrow(o))) {
    id <- o$output_id[i]
    pop <- if (!is.null(o$population)) o$population[i] else NA_character_
    if (!.is_blank(pop)) put(id, "OUTPUT_POPULATION", .pop_label(x, pop))
    if (!is.null(o$section)) put(id, "OUTPUT_SECTION", o$section[i])
  }
  if (!length(add)) return(x)
  x$sheets$tokens <- rbind(.normalize_sheet(x$sheets$tokens, "tokens"),
                           .normalize_sheet(do.call(rbind, add), "tokens"))
  x
}

# ---- the programs from the YAML spec's items (#411) -------------------------

# The study's spec as the programs are written from it, when the view is
# in step with it (a study opened or saved: what save_study() writes the
# programs from); NULL -- the programs are written from the sheets -- when
# the planner has none (a new study not yet saved, the company standards,
# workbooks) or its view has edits the spec has not taken in yet: taking
# them in runs the bridge over the whole study (.spec_sync(), seconds a
# report's Code tab would wait after each edit) to give the items the
# sheets already say.  Kept for the next report of the same planner
# (writing every report's program, study_status()).
.code_spec_cache <- new.env()
.code_spec <- function(x) {
  # (a study's spec is a tfl_study_spec, also with no report yet; anything
  # else is written from the sheets)
  if (is.null(x$spec) || is.null(x$spec$base) ||
      !inherits(x$spec$study, "tfl_study_spec")) return(NULL)
  if (!is.null(.code_spec_cache$key) && identical(.code_spec_cache$key, x)) {
    return(.code_spec_cache$spec)
  }
  spec <- if (identical(.planner_body(x), x$spec$base)) x$spec
  if (!is.null(spec)) {
    # (the study's page text as its sheets' cells mean it: .page_sheet_text())
    st <- spec$study
    # (the page configurations -- the study's style sets -- and its tokens)
    for (nm in names(st$styles)) {
      st$styles[[nm]]$page <- .page_sheet_text(st$styles[[nm]]$page)
    }
    if (length(st$study$tokens)) {
      st$study$tokens <- .page_sheet_text(list(tokens = st$study$tokens))$tokens
    }
    spec$study <- st
  }
  .code_spec_cache$key <- x
  .code_spec_cache$spec <- spec
  spec
}

# A report as its file says it, with the study's spec: list(output,
# study); NULL when the planner's spec has no such report.  The report's
# analysis set and section are its tokens {OUTPUT_POPULATION} and
# {OUTPUT_SECTION} when no layer of its page says them (as
# .with_population_tokens() gives the sheets): its own, not written.
.code_output <- function(x, output_id) {
  spec <- .code_spec(x)
  o <- spec$outputs[[output_id]]
  if (is.null(o) || is.null(spec$study)) return(NULL)
  said <- union(names(o$page$tokens),
                names(tryCatch(tflspec::tfl_resolve_output(o, spec$study)$page$tokens,
                               error = function(e) NULL)))
  i <- match(output_id, x$outputs$output_id)
  add <- list()
  if (!is.na(i)) {
    # (the report's set as its one reader says it: a listing's row first, S5)
    pop <- report_population(x, output_id)
    if (!.is_blank(pop) && !"OUTPUT_POPULATION" %in% said) {
      add$OUTPUT_POPULATION <- .pop_label(x, pop)
    }
    sec <- if (!is.null(x$outputs$section)) x$outputs$section[i] else NA_character_
    if (!.is_blank(sec) && !"OUTPUT_SECTION" %in% said) add$OUTPUT_SECTION <- sec
  }
  for (k in names(add)) o$page$tokens[[k]] <- add[[k]]
  o$page <- .page_sheet_text(o$page)
  list(output = o, study = spec$study)
}

# The text of a page's lines and tokens as the sheets' cells mean it (the
# bridge from the sheets passes a cell through as it is): a quoted cell is
# its text, spaces kept ("  Indented"), and a written \n a line break -- what
# tflspec reads a sheet's text cell as
.page_sheet_text <- function(pg) {
  if (!is.list(pg) || !length(pg)) return(pg)
  text <- function(v) {
    if (!is.character(v) || length(v) != 1L || is.na(v) || inherits(v, "tfl_r")) return(v)
    q <- regmatches(v, regexec("^([\"'])(.*)\\1$", v))[[1L]]
    if (length(q)) v <- q[3L]
    gsub("\\n", "\n", gsub("\r\n", "\n", v, fixed = TRUE), fixed = TRUE)
  }
  line_text <- function(cells) {
    if (!is.list(cells) || inherits(cells, "tfl_unset")) return(cells)
    for (k in intersect(c("left", "center", "right"), names(cells))) {
      if (!is.null(cells[[k]])) cells[[k]] <- text(cells[[k]])
    }
    cells
  }
  for (band in intersect(c("header", "footer", "titles", "footnotes"), names(pg))) {
    m <- pg[[band]]
    if (!is.list(m) || inherits(m, "tfl_unset")) next
    for (ln in names(m)) {
      # (a configuration's band is its blocks, first and last, of lines; a
      # report's may have them too)
      if (ln %in% c("first", "last") && is.list(m[[ln]]) && !inherits(m[[ln]], "tfl_unset")) {
        for (l2 in names(m[[ln]])) m[[ln]][[l2]] <- line_text(m[[ln]][[l2]])
      } else {
        m[[ln]] <- line_text(m[[ln]])
      }
    }
    pg[[band]] <- m
  }
  if (is.list(pg$tokens) && !inherits(pg$tokens, "tfl_unset")) {
    for (k in names(pg$tokens)) if (!is.null(pg$tokens[[k]])) pg$tokens[[k]] <- text(pg$tokens[[k]])
  }
  pg
}

# A table's content as its program writes it: the report resolved (its
# defaults and pattern applied), each item without the layer it came from
# (the program says no "(study default)")
.code_content <- function(y) {
  r <- tflspec::tfl_resolve_output(y$output, y$study)
  if (is.list(r$content) && !inherits(r$content, "tfl_r")) {
    r$content <- lapply(r$content, function(it) {
      if (is.list(it)) it["from"] <- list(NULL)
      it
    })
  }
  r
}

# The study a content call of tflspec's (tfl_content_code(),
# tfl_content_run()) is given for the report `o`: a figure's, the study the
# program writer has -- every way a figure is made applies its report's set
# (canon 11-15: its subjects made from the study, or a stop saying why);
# NULL for a table's and a listing's, whose data part the program writes
# apart (with the study tflspec would write it into the content's code)
.content_study <- function(o, study) {
  if (identical(o$kind, "figure")) study
}

# `report_id <- "<id>"`: what a report's program sets at its head, and a
# preview of its data part sets first (the data templates read it)
.report_id_line <- function(output_id) paste("report_id <-", .r_string(output_id))

# programs/tfl/report_setup.R is always written, and every report program
# sources it (it sources programs/study_setup.R).  The study's tokens,
# header and footer are written once in it when the study has default rows
# of them (COMPANY, STUDY_ID ...; a header every report has) or a font or
# size for every report -- a new study does, and a report's program then
# says only its own; one that has none writes each report's header in its
# program, as before
.uses_report_setup <- function(x) {
  # a study's spec: its defaults say some of it (tfl_report_setup_code())
  spec <- .code_spec(x)
  if (!is.null(spec$study)) {
    return(length(tflspec::tfl_report_setup_code(spec$study)) > 0L)
  }
  study_rows <- function(sh) {
    d <- x$sheets[[sh]]
    !is.null(d) && any(is.na(d$output_id))
  }
  study_rows("tokens") || study_rows("header") || study_rows("footer") ||
    !is.na(study_page_value(x, "font")) ||
    !is.na(study_page_value(x, "font_lang")) ||
    !is.na(study_page_value(x, "font_size_half_points"))
}

#' The study's setup of its report programs
#'
#' `programs/tfl/report_setup.R`, which every report program sources: the
#' study's setup (`programs/study_setup.R`, see [study_setup_code()]), the
#' reports' font and size (`options(rtfreporter.font = )`), the
#' study's tokens (`options(rtfreporter.tokens = )`: the company, the
#' analysis, the protocol ...), its running header and footer
#' (`header_standard`, `footer_standard`), written once from the study's
#' defaults, `spec/defaults.yml` ([tflspec::tfl_report_setup_code()]).  Each report program then says
#' only its own tokens (`OUTPUT_LABEL`, `OUTPUT_TITLE` ...), and ends with
#' `record_report()`, which records the report as made, with the
#' fingerprint of the study setup, in `output/tfl/report_status.csv`.
#'
#' @param x A `tflplanner`.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' s <- create_sample_study(run = FALSE)
#' head(report_setup_code(s$planner), 6)
#' }
#' options(old)
#' @export
report_setup_code <- function(x, date = Sys.Date()) {
  .with_study_code(.report_setup_code(x, date))
}

.report_setup_code <- function(x, date = Sys.Date()) {
  # (the study's spec: its defaults; else the sheets' default rows)
  spec <- .code_spec(x)
  study <- if (!is.null(spec$study)) spec$study else
    .spec_object_last(x, c(table_sheets(), report_sheets()),
                      unique(c(.study_keys$table, .study_keys$report)))
  ctx <- .setup_context_args()
  tflspec::tfl_setup_program("report_setup", study = study, attached = ctx$attached,
                             study_code = ctx$study_code, layout = study_layout(),
                             date = date, generator = .setup_generator())
}

# What a report program records in output/tfl/report_status.csv (its
# record_report(), written in report_setup.R by tflspec): tflplanner reads
# it -- a report made with another programs/study_setup.R than the one
# there now is outdated; `empty` (a listing says it): whether the report
# has no rows, which the official run notes
.report_status_name <- "report_status.csv"

# the definition of the ARD each figure was made from, as its program
# recorded it (NA: none recorded)
.report_ard_recorded <- function(root, output_id) {
  f <- file.path(root, study_layout()[["tfl"]], .report_status_name)
  st <- if (file.exists(f)) tryCatch(
    utils::read.csv(f, colClasses = "character"), error = function(e) NULL)
  if (is.null(st) || !all(c("output_id", "ard") %in% names(st))) {
    return(rep(NA_character_, length(output_id)))
  }
  st$ard[match(output_id, st$output_id)]
}

# the study setup each report was made with, as its program recorded it
# (NA: not recorded -- made before it was, or by a program of its own)
.report_setup_recorded <- function(root, output_id) {
  f <- file.path(root, study_layout()[["tfl"]], .report_status_name)
  st <- if (file.exists(f)) tryCatch(
    utils::read.csv(f, colClasses = "character"), error = function(e) NULL)
  if (is.null(st) || !all(c("output_id", "setup") %in% names(st))) {
    return(rep(NA_character_, length(output_id)))
  }
  st$setup[match(output_id, st$output_id)]
}

#' The program that runs every report program
#'
#' `programs/tfl/autoexec_report.R` runs from the study folder.  It runs the
#' programs in the order of the report list -- or only the ones named on
#' its command line (`Rscript programs/tfl/autoexec_report.R DM.R AE.R`) --
#' each in its own `Rscript` process with the study folder as its working
#' directory and its log in `logs/`, and ends with a table of what passed
#' (`logs/autoexec_report.csv`).
#'
#' @param x An `tflplanner`.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' s <- create_sample_study(run = FALSE)
#' head(autoexec_code(s$planner), 6)
#' }
#' options(old)
#' @export
autoexec_code <- function(x, date = Sys.Date()) {
  lay <- study_layout()
  c(.autoexec_banner(file.path(lay[["programs_tfl"]], .autoexec_report_file),
                     "Official run of the report programs, in the report list's order.",
                     "T-14-1-1.R       only these", date),
    .autoexec_body("tfl"))
}
