# The report programs: one per report, and autoexec_report.R to run them.
#
# A program does four things and only the first is the study's own: make
# its data, read the report's definition, lay the report out from it, write
# the RTF.  How the report looks lives in the workbooks, so a change of
# layout is a change of workbook, not of program.

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
#' @export
report_info <- function(x, output_id) {
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

# a program's section line, as tflspec writes its own (a designed figure's
# `# ---- data ----` and `# ---- plot ----`): to the 78th column
.section <- function(title) {
  head <- paste("# ----", title, "")
  paste0(head, strrep("-", max(3L, 78L - nchar(head))))
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

# A table's normalization after its ARD: its process code, or (when the
# code before it did not make `data`) the company's template or
# normalize_ard()
.table_process_lines <- function(x, output_id, code, proc) {
  if (!is.na(proc)) {
    c("", "# normalize and rework", .code_block(proc))
  } else if (!.makes_data(code)) {
    tp <- .fill_template("table_process", x, output_id)
    # the built-in normalization, or the company's template left at it:
    # the line the report's definitions say (tflspec::tfl_normalize_code())
    c("", if (is.na(tp) || identical(trimws(tp), "data <- normalize_ard(ard)"))
      .normalize_line(x, output_id) else .code_block(tp))
  }
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
    `{ARD}` = .ard_study_value(x$ard, "output", "output/ard/ard.rds"),
    `{ARD_PROGRAM}` = if (is.na(output_id)) "" else
      file.path(lay[["programs_ard"]], .ard_prog_name(output_id)),
    `{PROGRAM}` = if (is.na(output_id)) "" else
      file.path(lay[["programs_tfl"]], report_info(x, output_id)$program),
    `{STUDY_ID}` = if (is.na(study_id)) "" else study_id)
  # the study ARD as a string of R: through its folder's variable while a
  # program is written
  code <- gsub("\"{ARD}\"", .path_lit(sub[["{ARD}"]]), code, fixed = TRUE)
  for (k in names(sub)) code <- gsub(k, sub[[k]], code, fixed = TRUE)
  .drop_attached(unname(code))
}

#' The data part of a report's program
#'
#' The code a report runs before the report is laid out: the setup every
#' report runs, then its data and its content as its definition makes them
#' or as its own code does ([step_code()]: the datasets it registers, its
#' ARD as `ard`, the code) -- a table's ARD and its `process_code`
#' (normalization and rework, by default `data <- normalize_ard(ard)`).
#' With nothing to make it, a TODO that stops.  [program_code()] writes it
#' into the program, and [fetch_ard()] runs it to learn what the ARD holds.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param todo `FALSE` returns `NULL` instead of the TODO.
#' @return Code lines.
#' @export
data_lines <- function(x, output_id, todo = TRUE) {
  lay <- study_layout()
  info <- report_info(x, output_id)
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  code <- .ard_code_of(x, output_id)
  proc <- if (nrow(o) && !is.na(o$process_code)) o$process_code else NA
  type <- info$type
  setup <- if (!is.na(x$setup)) c(.code_block(x$setup), "")
  # a step of the report's own code: the data and the content, each as its
  # definition or its code makes it
  if (.is_code_step(x, output_id, "data") || .is_code_step(x, output_id, "content")) {
    return(c(setup, .code_report_lines(x, output_id, info, proc)))
  }
  # a listing defined in rows: its process code is a rework of `data`
  if (identical(type, "listing")) {
    l <- .listing_lines(x, output_id, rework = proc)
    if (!is.null(l)) return(c(setup, l))
  }
  # a figure: its design's code, or (none yet) its datasets and a plot to
  # write
  if (identical(type, "figure")) {
    design <- fig_design(x, output_id)
    f <- if (is.null(design)) {
      .figure_lines(x, output_id, info)
    } else {
      .fig_design_lines(x, output_id, design)
    }
    if (!is.null(f)) return(c(setup, f))
  }
  if (is.na(code)) {
    if (!todo) return(NULL)
    stop_todo <- paste0("stop(\"tflplanner: the data part of ",
                        info$program, " is still to be written.\")")
    body <- switch(type,
      figure = c(
        "# TODO: make the figure(s), e.g.",
        "#   content <- list(ggplot2::ggplot(adsl, ggplot2::aes(AGE)) +",
        "#                     ggplot2::geom_histogram())",
        stop_todo),
      listing = c(
        "# TODO: make the listing pages, e.g.",
        paste0("#   adae    <- readRDS(", .path_lit(file.path(lay[["adam"]], "adae.rds")), ")"),
        "#   content <- as_rtftables(adae[, c(\"USUBJID\", \"AEDECOD\")])",
        stop_todo),
      c("# TODO: build the ARD, e.g.",
        paste0("#   adsl <- readRDS(", .path_lit(file.path(lay[["adam"]], "adsl.rds")), ")"),
        "#   ard  <- cards::ard_stack(adsl, .by = TRT01A, ...)",
        stop_todo))
    return(c(if (!is.na(x$setup)) c(.code_block(x$setup), ""), body))
  }
  table <- identical(type, "table")
  proc_lines <- if (table) .table_process_lines(x, output_id, code, proc)
  c(if (!is.na(x$setup)) c(.code_block(x$setup), ""),
    .code_block(code),
    proc_lines)
}

# The part of a program between its setup and its report when a step is
# the report's own code: its data (the datasets it registers and its ARD
# read, its code; or as the definition makes it), then its content (its
# code; or as the definition makes it -- a table's plan is the report
# part's)
.code_report_lines <- function(x, output_id, info, proc = NA) {
  type <- info$type
  dc <- .is_code_step(x, output_id, "data")
  cc <- .is_code_step(x, output_id, "content")
  design <- if (identical(type, "figure")) fig_design(x, output_id)
  todo <- function(what) paste0("stop(\"tflplanner: the ", what, " of ", info$program,
                                " is still to be written.\")")
  data <- if (dc) {
    code <- step_code(x, output_id, "data")
    c(.section("data: your code"),
      .code_inputs(x, output_id),
      if (identical(type, "table")) c(
        "# your code: leaves `data` (or `ard`, normalized below)",
        .code_block(code),
        .table_process_lines(x, output_id, code, proc),
        "", "data <- report_data(data, \"data\")")
      else .code_step_lines(x, output_id, "data"))
  } else switch(type,
    table = {
      a <- .ard_code_of(x, output_id)
      c(.section("data: this report's normalized ARD"),
        if (is.na(a)) todo("ARD") else c(.code_block(a), .table_process_lines(x, output_id, a, proc)))
    },
    listing = c(.section("data: the listing's data"),
                .listing_lines(x, output_id, rework = proc, part = "data") %||%
                  .code_inputs(x, output_id)),
    figure = c(.section("data: the figure's data"),
               if (!is.null(design)) .fig_design_parts(x, output_id, design)$data else
                 .code_inputs(x, output_id)))
  content <- if (cc) {
    c("", .section("content: your code"),
      .code_step_lines(x, output_id, "content"))
  } else switch(type,
    table = NULL,
    listing = c("", .section("content: the listing"),
                .listing_lines(x, output_id, part = "content") %||% todo("listing's columns")),
    figure = c("", .section("content: the figure"),
               if (!is.null(design)) .fig_design_parts(x, output_id, design)$plot else
                 todo("figure")))
  # (one blank line between the two)
  while (length(data) && identical(data[[length(data)]], "")) data <- data[-length(data)]
  # the figure style, report_content() and report_data() first (a designed
  # figure's program sources them at its top)
  c(if (is.null(design)) c(.fig_source_lines(), ""), data, content)
}

#' The R program for one report
#'
#' A program runs with the study folder as its working directory (see
#' [study_layout()]) and names every file relative to it.  What it does
#' depends on the report's `type`:
#'
#' * `table` -- the data part leaves `data`, the normalized ARD; the program
#'   saves it to `output/ard/<output_id>.rds` (the deliverable data) and
#'   plans the table as `table_spec.xlsx` defines it, written out as
#'   `table_plan() |> plan_*()` ([tflspec::tfl_table_code()]).
#' * `listing`, `figure` -- the data part leaves `content`: `rtftable`
#'   pages for a listing, the figures for a figure.
#'
#' The report around it -- page, header, footer, titles, footnotes -- is
#' written out from `report_spec.xlsx` for every type, as rtfreporter calls
#' ([tflspec::tfl_report_code()]).  The program does not read the workbooks:
#' generate it again after changing them.
#'
#' @param x An `tflplanner`.
#' @param output_id The report.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
program_code <- function(x, output_id, date = Sys.Date()) {
  # as the study's setup has it: its folders' variables, its packages
  .with_study_code(.program_code(x, output_id, date))
}

.program_code <- function(x, output_id, date = Sys.Date()) {
  lay <- study_layout()
  info <- report_info(x, output_id)
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  desc <- if (nrow(o) && !is.na(o$description)) o$description else NULL
  titles <- .title_lines(x, output_id)
  type <- info$type
  table <- identical(type, "table")
  obj <- if (table) "data" else "content"
  # a designed figure: its code is the design's (#293)
  design <- if (identical(type, "figure")) fig_design(x, output_id)
  # a step of the report's own code: its data and content parts are
  # .code_report_lines()'s
  own <- .is_code_step(x, output_id, "data") || .is_code_step(x, output_id, "content")

  head <- .banner(
    paste("Program    :", file.path(lay[["programs_tfl"]], info$program)),
    paste0("Output     : ", output_id, " (", type, ") -> ", info$file),
    # wrapped as the ARD banner's Analyses: are
    if (!is.null(desc)) strwrap(desc, width = 74, initial = "Description: ",
                                prefix = strrep(" ", 13)),
    if (length(titles)) paste("Title      :", titles),
    paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
           ", ", format(date, "%Y-%m-%d")),
    if (own) paste0(
      "The data and content parts are your code where its steps are; the report part is written from ",
      lay[["spec"]], "/.") else if (is.null(design)) paste0(
      "The data part makes `", obj, "`; the report part is written from ",
      lay[["spec"]], "/.") else c(
      paste0("The data and plot parts make `plot`; the report part is written from ",
             lay[["spec"]], "/."),
      paste0("Edit the design in the Plot Designer (", lay[["spec"]], "/",
             .fig_design_dir, "/", output_id, ".yml), not this code.")))

  data <- if (is.null(design) || own) data_lines(x, output_id) else c(
    if (!is.na(x$setup)) c(.code_block(x$setup), ""),
    .fig_design_lines(x, output_id, design, source = FALSE))
  data_part <- if (!is.null(design) || own) c(data, if (own && table) c(
      "",
      "# the table's data, kept beside the study ARD",
      paste0("saveRDS(data, file.path(", .path_lit(lay[["ard"]]),
             ", paste0(report_id, \".rds\")))"))) else c(
    .section(paste0("data: ", switch(type,
      figure = "the figure",
      listing = "the listing's pages",
      "this report's normalized ARD"))),
    data,
    if (table) c(
      "",
      "# the table's data, kept beside the study ARD",
      paste0("saveRDS(data, file.path(", .path_lit(lay[["ard"]]),
             ", paste0(report_id, \".rds\")))")))

  # a figure's plot goes to the report as it is (rtf_figures(doc, plot)),
  # unless its code makes `content` itself
  content <- if (.is_code_step(x, output_id, "content")) "content" else
    if (table) "plan" else if (identical(type, "figure") &&
      !any(grepl("^[[:space:]]*content[[:space:]]*(<-|=)[^=]", data))) "plot" else "content"
  report_part <- c(
    .section("report"),
    .report_code_lines(x, output_id, table && identical(content, "plan"), content))

  c(head,
    "",
    .study_guard,
    "# the study's setup, and the header, footer and tokens of every report",
    sprintf("source(%s)", .r_string(file.path(lay[["programs_tfl"]],
                                               .report_setup_file))),
    if (!is.null(design)) .fig_source_lines(),
    "",
    .report_id_line(output_id),
    "",
    data_part,
    "",
    # (no blank line at the end: a file ends on its last line)
    report_part)
}

# The report part of a report program, written out from its definition by
# tflspec: the table's plan (tfl_table_code()), the document around it
# (tfl_report_code()) and where it goes (tfl_report_path()).  A definition
# that does not hold yet (a half-filled sheet) gives a stop() line saying
# why, so the program is still written and says what to fix when run.
# A table's normalization when it says none and the company's template is
# the built-in one: the line its definitions say -- the hierarchy of its
# hierarchical analysis, the any-event row (tables$overall_label):
# tflspec::tfl_normalize_code()
.normalize_line <- function(x, output_id) {
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  tryCatch(tflspec::tfl_normalize_code(sp, .ard_spec(x$ard), output_id),
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

.report_code_lines <- function(x, output_id, table,
                               content = if (table) "plan" else "content") {
  lay <- study_layout()
  sp <- .spec_object_last(x, c(table_sheets(), report_sheets()),
                          unique(c(.study_keys$table, .study_keys$report)))
  tryCatch(c(
    if (table) c(
      paste0("# the table, as ", file.path(lay[["spec"]], .table_file),
             " defines it"),
      tflspec::tfl_table_code(sp, output_id, pipe = "|>"),
      ""),
    paste0("# the report, as ", file.path(lay[["spec"]], .report_file), " defines it"),
    .report_id_code(tflspec::tfl_report_code(sp, output_id, content = content,
                                             setup = .uses_report_setup(x)), output_id),
    sprintf("generate_rtfreport(doc, %s, overwrite = TRUE)",
            .report_path_code(tflspec::tfl_report_path(sp, output_id), output_id)),
    # a figure from an ARD: the definition of that ARD it was made from
    if (!is.na(.fig_ard_need(x, output_id))) "record_report(report_id, ard = ard_built)"
    else "record_report(report_id)"),
    error = function(e) sprintf(
      "stop(%s)", .r_string(paste0("tflplanner: the definition of ",
                                   output_id, " does not hold: ",
                                   conditionMessage(e)))))
}

# `report_id <- "<id>"`: what a report's program sets at its head, and a
# preview of its data part sets first (the data templates read it)
.report_id_line <- function(output_id) paste("report_id <-", .r_string(output_id))

# The first line of every program: it runs from the study folder (its
# setup's source() would fail with a less clear message); under 80 columns
.study_guard <- "stopifnot(\"run it from the study folder\" = file.exists(\"study.yml\"))"

# The report's own id in its program is `report_id` (set at its head): a
# program copied for another report changes it in one place.  The RTF's
# path through its folder's variable; its name, when it is the report's id,
# made from report_id.
.report_id_code <- function(code, output_id) {
  lit <- encodeString(output_id, quote = "\"")
  gsub(sprintf("file.path(path_programs_tfl, %s)", lit),
       "file.path(path_programs_tfl, report_id)", code, fixed = TRUE)
}
.report_path_code <- function(path, output_id) {
  if (!identical(basename(path), paste0(output_id, ".rtf"))) return(.path_lit(path))
  dir <- .path_lit(dirname(path))
  sprintf("file.path(%s, paste0(report_id, \".rtf\"))", dir)
}

# programs/tfl/report_setup.R is always written, and every report program
# sources it (it sources programs/study_setup.R).  The study's tokens,
# header and footer are written once in it when the study has default rows
# of them (COMPANY, STUDY_ID ...; a header every report has) or a font or
# size for every report -- a new study does, and a report's program then
# says only its own; one that has none writes each report's header in its
# program, as before
.uses_report_setup <- function(x) {
  study_rows <- function(sh) {
    d <- x$sheets[[sh]]
    !is.null(d) && any(is.na(d$output_id))
  }
  study_rows("tokens") || study_rows("header") || study_rows("footer") ||
    !is.na(study_page_value(x, "font")) ||
    !is.na(study_page_value(x, "font_size_half_points"))
}

#' The study's setup of its report programs
#'
#' `programs/tfl/report_setup.R`, which every report program sources: the
#' study's setup (`programs/study_setup.R`, see [study_setup_code()]), the
#' reports' font and size (`options(rtfreporter.font = )`), the
#' study's tokens (`options(rtfreporter.tokens = )`: the company, the
#' analysis, the protocol ...), its running header and footer
#' (`study_header`, `study_footer`), written once from `report_spec.xlsx`
#' ([tflspec::tfl_report_setup_code()]).  Each report program then says
#' only its own tokens (`OUTPUT_LABEL`, `OUTPUT_TITLE` ...), and ends with
#' `record_report()`, which records the report as made, with the
#' fingerprint of the study setup, in `output/tfl/report_status.csv`.
#'
#' @param x A `tflplanner`.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
report_setup_code <- function(x, date = Sys.Date()) {
  .with_study_code(.report_setup_code(x, date))
}

.report_setup_code <- function(x, date = Sys.Date()) {
  lay <- study_layout()
  sp <- .spec_object_last(x, c(table_sheets(), report_sheets()),
                          unique(c(.study_keys$table, .study_keys$report)))
  c(.banner(
      paste("Program    :", file.path(lay[["programs_tfl"]], .report_setup_file)),
      "The study's font, header, footer and tokens: every report program sources it.",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d")),
      "",
      "Written from spec/report_spec.xlsx: change them there (the study tab),",
      "then generate the programs again."),
    "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .source_study_setup(),
    "",
    local({
      own <- tflspec::tfl_report_setup_code(sp)
      if (length(own)) c(own, "")
    }),
    .record_report_fun(),
    "")
}

# The small function a report program ends with, written in
# report_setup.R: what was made, and with which study setup, into
# output/tfl/report_status.csv (tflplanner reads it: a report made with
# another programs/study_setup.R than the one there now is outdated).
.report_status_name <- "report_status.csv"
.record_report_fun <- function() {
  sf <- file.path(study_layout()[["tfl"]], .report_status_name)
  c("# what was made, with which study setup, and -- a figure printing an",
    "# ARD's numbers -- from which definition of that ARD (tflplanner reads it)",
    "record_report <- function(output_id, ard = \"\") {",
    paste0("  sf <- ", .r_string(sf)),
    # the study setup's fingerprint on a line of its own
    paste0("  setup <- ", .setup_hash_code()),
    "  row <- data.frame(output_id = output_id,",
    "                    built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
    "                    setup = setup,",
    "                    ard = ard,",
    "                    stringsAsFactors = FALSE)",
    "  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)",
    "  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = \"character\")",
    "  if (!is.null(st) && nrow(st)) {",
    "    for (k in setdiff(names(row), names(st))) st[[k]] <- \"\"",
    "    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)",
    "  }",
    "  utils::write.csv(row, sf, row.names = FALSE)",
    "  invisible(output_id)",
    "}")
}

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
#' @export
autoexec_code <- function(x, date = Sys.Date()) {
  lay <- study_layout()
  c(.autoexec_banner(file.path(lay[["programs_tfl"]], "autoexec_report.R"),
                     "Official run of the report programs, in the report list's order.",
                     "T-14-1-1.R       only these", date),
    .autoexec_body("tfl"))
}
