# The checks a set of definition files passes before it is taken in
# (#274): on open when the files changed outside tflplanner, and on import
# before anything is changed.  Besides what the readers check (sheets,
# columns, values, ids), every cell that holds R is parsed, tflspec's own
# checks run (the ARD definition, each figure design), and the programs of
# the reports the change touches are written -- in memory -- and parsed.
# Errors stop the files being taken in; warnings are shown, and do not.

# The cells that hold R: the part they are in, the sheet, the column, and
# how the cell is R -- an expression, `NAME = expression` pieces between
# `|`, a function's arguments, or a program
.code_cells <- data.frame(
  part = c(rep("ard", 10), "sheet", "sheet", "lf", "outputs", "outputs"),
  sheet = c("datasets", "populations", "populations", "analysis_data",
            "analysis_data", "analysis_data", "analyses", "analyses",
            "analyses", "analyses", "cells", "cell_styles", "listings",
            "outputs", "outputs"),
  column = c("derive", "where", "derive", "where", "derive", "code",
             "where", "args", "post", "code", "when", "where", "where",
             "data_code", "process_code"),
  kind = c("pieces", "expr", "pieces", "expr", "pieces", "program", "expr",
           "args", "pieces", "program", "expr", "expr", "expr", "program",
           "program"),
  stringsAsFactors = FALSE)

# Does R read it?  NULL, or the parser's message
.parse_problem <- function(txt, kind = "expr") {
  if (is.na(txt) || !nzchar(trimws(txt))) return(NULL)
  try_parse <- function(x) tryCatch({
    parse(text = x, keep.source = FALSE)
    NULL
  }, error = function(e) sub("^<text>:", "", conditionMessage(e)))
  if (identical(kind, "args")) return(try_parse(paste0("f(", txt, "\n)")))
  if (!identical(kind, "pieces")) return(try_parse(txt))
  # pieces between `|`: a `|` inside an expression (a logical or) is part
  # of it, so pieces are joined until what is joined reads
  bits <- strsplit(txt, "|", fixed = TRUE)[[1L]]
  acc <- character()
  last <- NULL
  for (b in bits) {
    acc <- c(acc, b)
    last <- try_parse(paste(acc, collapse = "|"))
    if (is.null(last)) acc <- character()
  }
  if (length(acc)) last else NULL
}

# Which file a sheet's cells are in (the ARD definition: its JSON, or the
# workbook it was read from)
.sheet_file <- function(part, sheet, ard_file = .ard_json) {
  sp <- study_layout()[["spec"]]
  if (part == "ard") return(file.path(sp, ard_file))
  if (part == "lf") return(file.path(sp, .lf_file))
  if (part == "outputs" || sheet %in% report_sheets()) {
    return(file.path(sp, .report_file))
  }
  file.path(sp, .table_file)
}

.problem_rows <- function(file, sheet, row, column, message,
                          severity = "error") {
  n <- length(message)
  data.frame(file = rep_len(as.character(file), n),
             sheet = rep_len(as.character(sheet), n),
             row = rep_len(as.character(row), n),
             column = rep_len(as.character(column), n),
             severity = rep_len(severity, n),
             message = as.character(message), stringsAsFactors = FALSE)
}

# Every R cell of a study's definition, parsed; the rows of `outputs` only
# (NULL: every row)
.check_code_cells <- function(p, outputs = NULL, ard_file = .ard_json) {
  out <- .problem_rows(character(), character(), character(), character(),
                       character())
  for (i in seq_len(nrow(.code_cells))) {
    cc <- .code_cells[i, ]
    d <- switch(cc$part,
                ard = p$ard[[cc$sheet]],
                sheet = p$sheets[[cc$sheet]],
                lf = (p$lf %||% .empty_lf())[[cc$sheet]],
                outputs = p$outputs)
    if (is.null(d) || !nrow(d) || !cc$column %in% names(d)) next
    keep <- if (!is.null(outputs) && "output_id" %in% names(d)) {
      is.na(d$output_id) | d$output_id %in% outputs
    } else rep(TRUE, nrow(d))
    for (r in which(keep)) {
      msg <- .parse_problem(d[[cc$column]][r], cc$kind)
      if (is.null(msg)) next
      sheet <- if (cc$part == "outputs") .planner_sheet else cc$sheet
      out <- rbind(out, .problem_rows(
        .sheet_file(cc$part, cc$sheet, ard_file), sheet,
        # the row as a spreadsheet shows it (the header is row 1); the
        # JSON's row, its place in the sheet
        if (cc$part == "ard" && ard_file == .ard_json) r else r + 1L, cc$column,
        paste("R cannot read it:", trimws(msg))))
    }
  }
  if (!is.na(p$setup %||% NA)) {
    msg <- .parse_problem(p$setup, "program")
    if (!is.null(msg)) out <- rbind(out, .problem_rows(
      .sheet_file("outputs", ""), .planner_sheet, "(setup)", "data_code",
      paste("R cannot read it:", trimws(msg))))
  }
  out
}

# A figure design's R pieces (data_code, layer_code, a derive or filter
# expression, an `!r` value), parsed, and tflspec's check of the design
.check_fig <- function(id, d) {
  f <- file.path(study_layout()[["spec"]], .fig_design_dir, paste0(id, ".yml"))
  out <- .problem_rows(character(), character(), character(), character(),
                       character())
  walk <- function(x, path) {
    if (is.list(x)) {
      nm <- names(x) %||% rep("", length(x))
      for (k in seq_along(x)) {
        key <- if (nzchar(nm[k])) nm[k] else paste0("[", k, "]")
        walk(x[[k]], if (nzchar(path)) paste0(path, if (nzchar(nm[k])) "." else "", key) else key)
      }
    } else if (is.character(x) && length(x) == 1L) {
      leaf <- sub("^.*[.]", "", path)
      code <- inherits(x, "tfl_fig_r") ||
        leaf %in% c("code", "layer_code", "data_code", "expr", "where",
                    "condition", "derive")
      if (code) {
        msg <- .parse_problem(x, if (leaf == "derive") "pieces" else "program")
        if (!is.null(msg)) out <<- rbind(out, .problem_rows(
          f, "", "", path, paste("R cannot read it:", trimws(msg))))
      }
    }
  }
  walk(unclass(d), "")
  ck <- tryCatch(tflspec::tfl_check_fig_design(structure(d, class = "tfl_fig_design")),
                 error = function(e) data.frame(part = "", field = "",
                                                problem = conditionMessage(e)))
  if (nrow(ck)) out <- rbind(out, .problem_rows(
    f, "", "", paste(ck$part, ck$field), ck$problem))
  out
}

# The figures' ARDs (#293): the source each names, and the design's ARD
# pieces against the rows that source has (a study read from its files)
.check_fig_ards <- function(s, ids) {
  p <- s$planner
  out <- .problem_rows(character(), character(), character(), character(), character())
  for (id in ids) {
    if (!identical(report_info(p, id)$type, "figure")) next
    pr <- tryCatch(.fig_ard_problems(s, id), error = function(e) NULL)
    if (is.null(pr) || !nrow(pr)) next
    # the source: the report sheet's; a piece: the design's file
    out <- rbind(out, .problem_rows(
      ifelse(pr$part == "ard", .sheet_file("report", ""),
             file.path(study_layout()[["spec"]], .fig_design_dir, paste0(id, ".yml"))),
      ifelse(pr$part == "ard", "report", ""), "",
      ifelse(pr$part == "ard", "ard_source", paste(pr$part, pr$field)),
      paste0(id, ": ", pr$problem), pr$severity))
  }
  out
}

# The programs of the reports `outputs` written (in memory) and parsed: the
# report program, and the ARD program when the report has analyses
.check_programs <- function(p, outputs, ard_file = .ard_json) {
  out <- .problem_rows(character(), character(), character(), character(),
                       character())
  spec <- NULL
  a <- p$ard
  if (!is.null(a) && nrow(a$analyses)) {
    spec <- tryCatch(.ard_spec(a), error = function(e) {
      out <<- rbind(out, .problem_rows(.sheet_file("ard", "", ard_file), "", "", "",
        paste0("The ARD definition does not hold, so saving writes no ARD ",
               "program: ", conditionMessage(e))))
      NULL
    })
  }
  for (id in intersect(outputs, output_ids(p))) {
    code <- tryCatch(program_code(p, id), error = function(e) e)
    if (inherits(code, "error")) {
      # (the report named in the message: the row and column are a cell's)
      out <- rbind(out, .problem_rows(.sheet_file("outputs", ""), "", "", "",
        paste0(id, ": the report's program cannot be written: ", conditionMessage(code))))
    } else {
      msg <- .parse_problem(paste(code, collapse = "\n"), "program")
      if (!is.null(msg)) out <- rbind(out, .problem_rows(
        .sheet_file("outputs", ""), "", "", "",
        paste0(id, ": the report's program does not read as R: ", trimws(msg))))
    }
    if (!is.null(spec) && id %in% a$analyses$output_id) {
      code <- tryCatch(ard_program_code(spec, id, codelists = .study_codelists(p)),
                       error = function(e) e)
      if (inherits(code, "error")) {
        out <- rbind(out, .problem_rows(.sheet_file("ard", "", ard_file), "analyses", "", "",
          paste0(id, ": the ARD program cannot be written: ", conditionMessage(code))))
      } else {
        msg <- .parse_problem(paste(code, collapse = "\n"), "program")
        if (!is.null(msg)) out <- rbind(out, .problem_rows(
          .sheet_file("ard", "", ard_file), "analyses", "", "",
          paste0(id, ": the ARD program does not read as R: ", trimws(msg))))
      }
    }
  }
  out
}

# All of it, for a study read from its files: R cells, figure designs, the
# programs of `outputs` (NULL: every report)
.check_spec <- function(s, outputs = NULL) {
  p <- s$planner
  af <- attr(s, "ard_file") %||% .ard_json
  ids <- outputs %||% output_ids(p)
  figs <- intersect(names(p$fig_designs %||% list()), ids)
  out <- rbind(.check_code_cells(p, outputs, af),
               do.call(rbind, c(list(.problem_rows(character(), character(),
                                                   character(), character(),
                                                   character())),
                                lapply(figs, function(f) .check_fig(f, p$fig_designs[[f]])))),
               .check_programs(p, ids, af),
               .check_fig_ards(s, ids))
  rownames(out) <- NULL
  unique(out)
}
