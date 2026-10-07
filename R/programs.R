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
# a design, the ones its row names).  In the order first used.
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
    user = {
      f <- x$lf$figures
      unlist(strsplit(f$datasets[!is.na(f$output_id) &
                                   f$output_id == output_id], "\\s*\\|\\s*"))
    },
    figure = {
      d <- (x$fig_designs %||% list())[[output_id]]
      steps <- d$data %||% list()
      read <- unlist(lapply(steps, function(s)
        if (isTRUE(s$step %in% c("read", "join"))) s$dataset))
      if (length(read)) read else {
        f <- x$lf$figures
        unlist(strsplit(f$datasets[!is.na(f$output_id) &
                                     f$output_id == output_id], "\\s*\\|\\s*"))
      }
    },
    character())
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

.section <- function(title) {
  paste0("# ---- ", title, " ", strrep("-", max(3L, 70L - nchar(title))))
}

# Does this code assign `data` itself?  (Then it needs no default
# normalization after it.)
.makes_data <- function(code) {
  any(grepl("(^|[^A-Za-z0-9_.])data[[:space:]]*(<-|=)[^=]",
            strsplit(code, "\n", fixed = TRUE)[[1L]]))
}

# A report's ARD code: its own, or -- a table -- the company's template
# (code_templates: table_data) that takes its rows of the study ARD.
.ard_code_of <- function(x, output_id) {
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  code <- if (nrow(o) && !is.na(o$data_code)) o$data_code else NA
  if (is.na(code) && identical(report_info(x, output_id)$type, "table")) {
    imp <- .ard_import_of(x, output_id)
    code <- if (is.null(imp)) .fill_template("table_data", x, output_id) else
      .imported_ard_code(imp, output_id)
  }
  code
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
  for (k in names(sub)) code <- gsub(k, sub[[k]], code, fixed = TRUE)
  unname(code)
}

#' The data part of a report's program
#'
#' The code a report runs before the report is laid out: the setup every
#' report runs, the report's own `data_code` (its ARD) and, for a table,
#' its `process_code` (normalization and rework, by default
#' `data <- normalize_ard(ard)`).  With no `data_code` it is a TODO that
#' stops.  [program_code()] writes it into the program, and
#' [fetch_ard()] runs it to learn what the ARD holds.
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
  # a listing defined in rows: its data code is a rework of `data`
  if (identical(type, "listing")) {
    l <- .listing_lines(x, output_id, rework = code)
    if (!is.null(l)) return(c(setup, l))
  }
  # user code: its datasets, its ARD when it reads one, its own code
  if (identical(type, "user")) {
    return(c(setup, .user_lines(x, output_id, info)))
  }
  # a figure: its datasets read for it, its data code the plot
  if (identical(type, "figure")) {
    design <- fig_design(x, output_id)
    f <- if (is.null(design)) {
      .figure_lines(x, output_id, info, plot_code = code)
    } else {
      .figure_lines(x, output_id, info,
                    plot_code = paste(.fig_design_plot(design, output_id),
                                      collapse = "\n"),
                    datasets = .fig_design_datasets(design, output_id))
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
        paste0("#   adae    <- readRDS(\"", lay[["adam"]], "/adae.rds\")"),
        "#   content <- as_rtftables(adae[, c(\"USUBJID\", \"AEDECOD\")])",
        stop_todo),
      c("# TODO: build the ARD, e.g.",
        paste0("#   adsl <- readRDS(\"", lay[["adam"]], "/adsl.rds\")"),
        "#   ard  <- cards::ard_stack(adsl, .by = TRT01A, ...)",
        stop_todo))
    return(c(if (!is.na(x$setup)) c(.code_block(x$setup), ""), body))
  }
  table <- identical(type, "table")
  proc_lines <- if (!table) NULL else if (!is.na(proc)) {
    c("", "# normalize and rework", .code_block(proc))
  } else if (!.makes_data(code)) {
    tp <- .fill_template("table_process", x, output_id)
    c("", if (is.na(tp)) "data <- normalize_ard(ard)" else .code_block(tp))
  }
  c(if (!is.na(x$setup)) c(.code_block(x$setup), ""),
    .code_block(code),
    proc_lines)
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
  lay <- study_layout()
  info <- report_info(x, output_id)
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  desc <- if (nrow(o) && !is.na(o$description)) o$description else NULL
  titles <- .title_lines(x, output_id)
  type <- info$type
  table <- identical(type, "table")
  obj <- if (table) "data" else "content"

  head <- .banner(
    paste("Program    :", file.path(lay[["programs_tfl"]], info$program)),
    paste0("Output     : ", output_id, " (", type, ") -> ", info$file),
    if (!is.null(desc)) paste("Description:", desc),
    if (length(titles)) paste("Title      :", titles),
    paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
           ", ", format(date, "%Y-%m-%d")),
    "",
    "Runs from the study folder (open the study's .Rproj, or run",
    "programs/tfl/autoexec_report.R).  How the report looks comes from",
    "spec/ and is written out in the Report part: generate the program again",
    paste0("after changing the workbooks.  This program makes `", obj,
           "`; edit the data part freely."))

  data_part <- c(
    .section("Data"),
    paste0("# Leaves `", obj, "`: ", switch(type,
      figure = "the figure(s) of the report.",
      user = "what the report's own code made (tables, figures).",
      listing = "the listing's rtftable pages.",
      "the normalized ARD (normalize_ard()) the table is built from.")),
    paste0("# Input data are in ", lay[["adam"]], "/, ", lay[["sdtm"]],
           "/ and ", lay[["other"]], "/."),
    data_lines(x, output_id))

  report_part <- c(
    .section("Report"),
    if (table) c(
      paste0("saveRDS(data, file.path(\"", lay[["ard"]],
             "\", paste0(output_id, \".rds\")))"),
      ""),
    .report_code_lines(x, output_id, table))

  c(head,
    "",
    "library(rtfreporter)",
    "library(tflspec)",
    "if (!file.exists(\"study.yml\")) {",
    "  stop(\"Run this program from the study folder: open the study's .Rproj\",",
    "       \" or setwd() to the folder that holds study.yml.\")",
    "}",
    if (.uses_report_setup(x)) c(
      "# the study's header, footer and tokens, every report's",
      sprintf("source(%s)", .r_string(file.path(lay[["programs_tfl"]],
                                                 .report_setup_file)))),
    "",
    paste("output_id <-", .r_string(output_id)),
    "",
    data_part,
    "",
    report_part,
    "")
}

# The report part of a report program, written out from its definition by
# tflspec: the table's plan (tfl_table_code()), the document around it
# (tfl_report_code()) and where it goes (tfl_report_path()).  A definition
# that does not hold yet (a half-filled sheet) gives a stop() line saying
# why, so the program is still written and says what to fix when run.
# The whole study's table and report spec, kept for the next report of
# the same definition: writing or checking every report's program (saving
# a study, study_status()) built it once per report -- 0.2 s each, a minute
# for 200 reports -- though it is the same for all of them
.spec_cache <- new.env()
.spec_object_last <- function(x, sheets, keys) {
  key <- list(x, sheets, keys)
  if (!is.null(.spec_cache$key) && identical(.spec_cache$key, key)) return(.spec_cache$sp)
  sp <- .spec_object(x, sheets, keys)
  .spec_cache$key <- key
  .spec_cache$sp <- sp
  sp
}

.report_code_lines <- function(x, output_id, table) {
  lay <- study_layout()
  sp <- .spec_object_last(x, c(table_sheets(), report_sheets()),
                          unique(c(.study_keys$table, .study_keys$report)))
  tryCatch(c(
    if (table) c(
      paste0("# the table, as ", file.path(lay[["spec"]], .table_file),
             " defines it"),
      tflspec::tfl_table_code(sp, output_id, pipe = "|>"),
      ""),
    paste0("# the report -- page, running header and footer, titles and ",
           "footnotes -- as"),
    paste0("# ", file.path(lay[["spec"]], .report_file), " defines it"),
    tflspec::tfl_report_code(sp, output_id,
                             content = if (table) "plan" else "content",
                             setup = .uses_report_setup(x)),
    sprintf("generate_rtfreport(doc, %s, overwrite = TRUE)",
            .r_string(tflspec::tfl_report_path(sp, output_id)))),
    error = function(e) sprintf(
      "stop(%s)", .r_string(paste0("tflplanner: the definition of ",
                                   output_id, " does not hold: ",
                                   conditionMessage(e)))))
}

# The study's tokens, header and footer are written once, in
# programs/tfl/report_setup.R, when the study has tokens of its own (its
# default rows: COMPANY, STUDY_ID ...) or a font or size for every report --
# a new study does; one that has neither writes each report's header in its
# program, as before
.uses_report_setup <- function(x) {
  d <- x$sheets$tokens
  (!is.null(d) && any(is.na(d$output_id))) ||
    !is.na(study_page_value(x, "font")) ||
    !is.na(study_page_value(x, "font_size_half_points"))
}

#' The study's setup of its report programs
#'
#' `programs/tfl/report_setup.R`, which every report program sources: the
#' reports' font and size (`options(rtfreporter.font = )`), the
#' study's tokens (`options(rtfreporter.tokens = )`: the company, the
#' analysis, the protocol ...), its running header and footer
#' (`study_header`, `study_footer`), written once from `report_spec.xlsx`
#' ([tflspec::tfl_report_setup_code()]).  Each report program then says
#' only its own tokens (`OUTPUT_LABEL`, `OUTPUT_TITLE` ...).
#'
#' @param x A `tflplanner`.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
report_setup_code <- function(x, date = Sys.Date()) {
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
    "library(rtfreporter)",
    "",
    tflspec::tfl_report_setup_code(sp),
    "")
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
