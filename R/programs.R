# The report programs: one per report, and autoexec_report.R to run them.
#
# A program does four things and only the first is the study's own: make
# `data` (the normalized ARD), read the report's definition, plan the table
# from it, write the RTF.  Everything about how the table looks lives in the
# workbooks, so a change of layout is a change of workbook, not of program.

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
#' @param x An `rtfplanner`.
#' @param output_id The report.
#' @param spec_dir The folder the program reads the workbooks from.
#' @param table_file,report_file The workbook names.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
program_code <- function(x, output_id, spec_dir = ".",
                         table_file = "table_spec.xlsx",
                         report_file = "report_spec.xlsx",
                         date = Sys.Date()) {
  info <- report_info(x, output_id)
  o <- x$outputs[x$outputs$output_id == output_id, , drop = FALSE]
  desc <- if (nrow(o) && !is.na(o$description)) o$description else NULL
  code <- if (nrow(o) && !is.na(o$data_code)) o$data_code else NA
  titles <- .title_lines(x, output_id)
  figure <- identical(info$type, "figure")
  obj <- if (figure) "content" else "data"

  head <- .banner(
    paste("Program    :", info$program),
    paste("Output     :", output_id, "->", info$file),
    if (!is.null(desc)) paste("Description:", desc),
    if (length(titles)) paste("Title      :", titles),
    paste0("Generated  : rtfplanner ", utils::packageVersion("rtfplanner"),
           ", ", format(date, "%Y-%m-%d")),
    "",
    "How the report looks is in the definition workbooks; this program only",
    paste0("makes `", obj, "` and hands it on.  Edit the data part freely."))

  todo <- if (figure) c(
    "# TODO: make the figure(s) for this report, e.g.",
    "#   content <- list(\"path/to/plot.png\")   # or a ggplot object",
    paste0("stop(\"rtfplanner: the data part of ", info$program,
           " is still to be written.\")")) else c(
    "# TODO: build the ARD and normalize it, e.g.",
    "#   ard  <- cards::ard_stack(adsl, .by = TRT01A, ...)",
    "#   data <- ard_normalize(ard)",
    paste0("stop(\"rtfplanner: the data part of ", info$program,
           " is still to be written.\")"))

  data_part <- c(
    .section("Data"),
    paste0("# Leaves `", obj, "`: ",
           if (figure) "the figure(s) for rtf_report()." else
             "the normalized ARD (ard_normalize()) the table is built from."),
    if (!is.na(x$setup)) c(.code_block(x$setup), ""),
    if (is.na(code)) todo else .code_block(code))

  c(head,
    "",
    "library(rtfreporter)",
    "",
    paste("output_id <-", .r_string(output_id)),
    paste("spec_dir  <-", .r_string(gsub("\\\\", "/", spec_dir))),
    "spec <- read_report_spec(",
    paste0("  file.path(spec_dir, c(", .r_string(report_file), ", ",
           .r_string(table_file), ")),"),
    "  output_id = output_id)",
    "",
    data_part,
    "",
    .section("Report"),
    if (figure) "doc <- rtf_report(spec, content)" else c(
      "plan <- rtf_plan(data, spec = spec)",
      "doc  <- rtf_report(spec, plan)"),
    "generate_rtfreport(doc, report_path(spec), overwrite = TRUE)",
    "")
}

#' The program that runs every report program
#'
#' `autoexec_report.R` runs the programs in the order of the report list,
#' each in its own `Rscript` process (so one report's packages and options
#' cannot leak into the next) with its log in `logs/`, and ends with a
#' table of what passed.  Set `isolate <- FALSE` in it to `source()` them
#' in one session instead.
#'
#' @param x An `rtfplanner`.
#' @param program_dir The folder the programs are in; `NULL` means the
#'   folder autoexec_report.R itself is run from.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
autoexec_code <- function(x, program_dir = NULL, date = Sys.Date()) {
  progs <- vapply(x$outputs$output_id, function(id)
    report_info(x, id)$program, "")
  c(.banner(
      "Program    : autoexec_report.R",
      "Runs every report program of the study, in the report list's order.",
      paste0("Generated  : rtfplanner ", utils::packageVersion("rtfplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "programs <- c(",
    paste0("  ", .r_string(progs),
           c(rep(",", max(0L, length(progs) - 1L)), "")[seq_along(progs)]),
    ")",
    if (is.null(program_dir)) "program_dir <- getwd()" else
      paste("program_dir <-", .r_string(gsub("\\\\", "/", program_dir))),
    "isolate  <- TRUE   # one Rscript process per program, a log for each",
    "",
    "log_dir <- file.path(program_dir, \"logs\")",
    "dir.create(log_dir, showWarnings = FALSE)",
    "",
    "run_one <- function(p) {",
    "  path <- file.path(program_dir, p)",
    "  log  <- file.path(log_dir, sub(\"\\\\.[Rr]$\", \".log\", p))",
    "  t0 <- Sys.time()",
    "  if (isolate) {",
    "    owd <- setwd(program_dir)",
    "    on.exit(setwd(owd))",
    "    rc <- system2(file.path(R.home(\"bin\"), \"Rscript\"), shQuote(path),",
    "                  stdout = log, stderr = log)",
    "    lines  <- if (file.exists(log)) readLines(log, warn = FALSE) else \"\"",
    "    status <- if (identical(rc, 0L)) \"OK\" else \"ERROR\"",
    "    err    <- grep(\"^Error\", lines)[1L]",
    "    note   <- if (status == \"OK\" || is.na(err)) \"\" else",
    "      paste(trimws(stats::na.omit(lines[err + 0:1])), collapse = \" \")",
    "    note   <- sub(\" ?Execution halted$\", \"\", note)",
    "    warns  <- sum(grepl(\"^Warning\", lines))",
    "  } else {",
    "    warns <- 0L",
    "    res <- withCallingHandlers(",
    "      tryCatch({",
    "        source(path, local = new.env(parent = globalenv()), chdir = TRUE)",
    "        \"\"",
    "      }, error = function(e) conditionMessage(e)),",
    "      warning = function(w) {",
    "        warns <<- warns + 1L",
    "        invokeRestart(\"muffleWarning\")",
    "      })",
    "    status <- if (nzchar(res)) \"ERROR\" else \"OK\"",
    "    note   <- res",
    "  }",
    "  secs <- round(as.numeric(difftime(Sys.time(), t0, units = \"secs\")), 1)",
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

#' Write everything: the workbooks, the programs, autoexec_report.R
#'
#' @param x An `rtfplanner`.
#' @param dir Folder for the two workbooks.
#' @param program_dir Folder for the programs and autoexec_report.R;
#'   defaults to `dir`.
#' @param overwrite_programs A report program already there is left alone
#'   unless this is `TRUE` -- it may have been edited by hand since it was
#'   generated.  The workbooks and autoexec_report.R are always rewritten.
#' @param portable `FALSE` writes the folders into the programs as absolute
#'   paths, so a program runs from anywhere.  `TRUE` writes them relative
#'   (the workbooks beside the programs, `spec_dir <- "."`), for a set that
#'   is moved or zipped; run it from its own folder.
#' @return A data frame of the files, each `written` or `kept`, invisibly.
#' @export
export_planner <- function(x, dir, program_dir = dir,
                           overwrite_programs = FALSE, portable = FALSE) {
  if (!nrow(x$outputs)) stop("The report list is empty.", call. = FALSE)
  progs <- vapply(x$outputs$output_id, function(id)
    report_info(x, id)$program, "")
  dup <- unique(progs[duplicated(progs)])
  if (length(dup)) {
    stop("Two reports share the program ", paste(dup, collapse = ", "),
         "; give each its own `program` on the report sheet.", call. = FALSE)
  }
  paths <- write_planner(x, dir)
  dir.create(program_dir, showWarnings = FALSE, recursive = TRUE)
  abs <- function(d) normalizePath(d, "/", FALSE)
  spec_dir <- if (portable) "." else abs(dir)
  files <- data.frame(file = unname(paths), status = "written",
                      stringsAsFactors = FALSE)
  for (i in seq_along(progs)) {
    f <- file.path(program_dir, progs[[i]])
    if (file.exists(f) && !overwrite_programs) {
      files[nrow(files) + 1L, ] <- list(f, "kept")
      next
    }
    code <- program_code(x, x$outputs$output_id[i], spec_dir = spec_dir)
    writeLines(enc2utf8(code), f, useBytes = TRUE)
    files[nrow(files) + 1L, ] <- list(f, "written")
  }
  f <- file.path(program_dir, "autoexec_report.R")
  writeLines(enc2utf8(autoexec_code(x, if (!portable) abs(program_dir))), f,
             useBytes = TRUE)
  files[nrow(files) + 1L, ] <- list(f, "written")
  invisible(files)
}
