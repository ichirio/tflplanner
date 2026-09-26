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
#' @param x An `rtfplanner`.
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

#' The R program for one report
#'
#' A program runs with the study folder as its working directory (see
#' [study_layout()]) and names every file relative to it.  What it does
#' depends on the report's `type`:
#'
#' * `table` -- the data part leaves `data`, the normalized ARD; the program
#'   saves it to `output/ard/<output_id>.rds` (the deliverable data) and
#'   plans the table from `table_spec.xlsx`.
#' * `listing`, `figure` -- the data part leaves `content`: `rtftable`
#'   pages for a listing, the figures for a figure.
#'
#' The report around it -- page, header, footer, titles, footnotes -- comes
#' from `report_spec.xlsx` for every type.
#'
#' @param x An `rtfplanner`.
#' @param output_id The report.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
program_code <- function(x, output_id, date = Sys.Date()) {
  lay <- study_layout()
  info <- report_info(x, output_id)
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  desc <- if (nrow(o) && !is.na(o$description)) o$description else NULL
  code <- if (nrow(o) && !is.na(o$data_code)) o$data_code else NA
  titles <- .title_lines(x, output_id)
  type <- info$type
  table <- identical(type, "table")
  obj <- if (table) "data" else "content"
  spec <- file.path(lay[["spec"]], c(.report_file, .table_file))

  head <- .banner(
    paste("Program    :", file.path(lay[["programs"]], info$program)),
    paste0("Output     : ", output_id, " (", type, ") -> ", info$file),
    if (!is.null(desc)) paste("Description:", desc),
    if (length(titles)) paste("Title      :", titles),
    paste0("Generated  : rtfplanner ", utils::packageVersion("rtfplanner"),
           ", ", format(date, "%Y-%m-%d")),
    "",
    "Runs from the study folder (open the study's .Rproj, or run",
    "programs/autoexec_report.R).  How the report looks is in spec/; this",
    paste0("program makes `", obj, "` and hands it on.  Edit the data part",
           " freely."))

  stop_todo <- paste0("stop(\"rtfplanner: the data part of ", info$program,
                      " is still to be written.\")")
  todo <- switch(type,
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
    c("# TODO: build the ARD and normalize it, e.g.",
      paste0("#   adsl <- readRDS(\"", lay[["adam"]], "/adsl.rds\")"),
      "#   ard  <- cards::ard_stack(adsl, .by = TRT01A, ...)",
      "#   data <- ard_normalize(ard)",
      stop_todo))

  data_part <- c(
    .section("Data"),
    paste0("# Leaves `", obj, "`: ", switch(type,
      figure = "the figure(s) for rtf_report().",
      listing = "the listing's rtftable pages.",
      "the normalized ARD (ard_normalize()) the table is built from.")),
    paste0("# Input data are in ", lay[["adam"]], "/, ", lay[["sdtm"]],
           "/ and ", lay[["other"]], "/."),
    if (!is.na(x$setup)) c(.code_block(x$setup), ""),
    if (is.na(code)) todo else .code_block(code))

  report_part <- c(
    .section("Report"),
    if (table) c(
      paste0("saveRDS(data, file.path(\"", lay[["ard"]],
             "\", paste0(output_id, \".rds\")))"),
      "plan <- rtf_plan(data, spec = spec)",
      "doc  <- rtf_report(spec, plan)") else
      "doc  <- rtf_report(spec, content)",
    "generate_rtfreport(doc, report_path(spec), overwrite = TRUE)")

  c(head,
    "",
    "library(rtfreporter)",
    "if (!file.exists(\"study.yml\")) {",
    "  stop(\"Run this program from the study folder: open the study's .Rproj\",",
    "       \" or setwd() to the folder that holds study.yml.\")",
    "}",
    "",
    paste("output_id <-", .r_string(output_id)),
    "spec <- read_report_spec(",
    paste0("  c(", paste(.r_string(spec), collapse = ", "), "),"),
    "  output_id = output_id)",
    "",
    data_part,
    "",
    report_part,
    "")
}

#' The program that runs every report program
#'
#' `programs/autoexec_report.R` runs from the study folder.  It runs the
#' programs in the order of the report list -- or only the ones named on
#' its command line (`Rscript programs/autoexec_report.R DM.R AE.R`) --
#' each in its own `Rscript` process with the study folder as its working
#' directory and its log in `logs/`, and ends with a table of what passed
#' (`logs/autoexec_report.csv`).
#'
#' @param x An `rtfplanner`.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
autoexec_code <- function(x, date = Sys.Date()) {
  lay <- study_layout()
  progs <- vapply(x$outputs$output_id, function(id)
    report_info(x, id)$program, "")
  c(.banner(
      paste("Program    :", file.path(lay[["programs"]], "autoexec_report.R")),
      "Runs the study's report programs, in the report list's order.",
      "  Rscript programs/autoexec_report.R            every program",
      "  Rscript programs/autoexec_report.R DM.R AE.R  only these",
      "Run it from the study folder.",
      paste0("Generated  : rtfplanner ", utils::packageVersion("rtfplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "if (!file.exists(\"study.yml\")) {",
    "  stop(\"Run autoexec_report.R from the study folder (the one with study.yml).\")",
    "}",
    "",
    "programs <- c(",
    paste0("  ", .r_string(progs),
           c(rep(",", max(0L, length(progs) - 1L)), "")[seq_along(progs)]),
    ")",
    "only <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)",
    "if (length(only)) {",
    "  programs <- programs[programs %in% only |",
    "                        sub(\"\\\\.[Rr]$\", \"\", programs) %in% only]",
    "}",
    "",
    paste0("program_dir <- ", .r_string(lay[["programs"]])),
    paste0("log_dir     <- ", .r_string(lay[["logs"]])),
    "dir.create(log_dir, showWarnings = FALSE)",
    "",
    "run_one <- function(p) {",
    "  log <- file.path(log_dir, sub(\"\\\\.[Rr]$\", \".log\", p))",
    "  t0  <- Sys.time()",
    "  rc  <- system2(file.path(R.home(\"bin\"), \"Rscript\"),",
    "                 shQuote(file.path(program_dir, p)),",
    "                 stdout = log, stderr = log)",
    "  lines  <- if (file.exists(log)) readLines(log, warn = FALSE) else \"\"",
    "  status <- if (identical(rc, 0L)) \"OK\" else \"ERROR\"",
    "  err    <- grep(\"^Error\", lines)[1L]",
    "  note   <- if (status == \"OK\" || is.na(err)) \"\" else",
    "    paste(trimws(stats::na.omit(lines[err + 0:1])), collapse = \" \")",
    "  note   <- sub(\" ?Execution halted$\", \"\", note)",
    "  warns  <- sum(grepl(\"^Warning\", lines))",
    "  secs   <- round(as.numeric(difftime(Sys.time(), t0, units = \"secs\")), 1)",
    "  cat(sprintf(\"%-5s %-30s %6.1fs\\n\", status, p, secs))",
    "  data.frame(program = p, status = status, warnings = warns,",
    "             seconds = secs, note = note, stringsAsFactors = FALSE)",
    "}",
    "",
    "result <- do.call(rbind, lapply(programs, run_one))",
    "utils::write.csv(result, file.path(log_dir, \"autoexec_report.csv\"),",
    "                 row.names = FALSE)",
    "cat(sprintf(\"\\n%d of %d program(s) ran without error.\\n\",",
    "            sum(result$status == \"OK\"), nrow(result)))",
    "if (any(result$status != \"OK\")) {",
    "  print(result[result$status != \"OK\", c(\"program\", \"note\")],",
    "        right = FALSE, row.names = FALSE)",
    "  if (!interactive()) quit(status = 1L)",
    "}",
    "")
}
