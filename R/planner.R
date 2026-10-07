# The planner: one study's definition as the app edits it.
#
#   sheets   every rtfreporter sheet as an all-character data frame, the
#            columns tflspec::tfl_table_spec() gives it plus `note`
#   study    the study sheet's keys, a named character vector
#   outputs  the report list: output_id, description, data_code (makes
#            the ARD), process_code (normalizes and reworks it), section
#            (the TOC's heading it is under; blank: from its ID),
#            population (its analysis set: the TOC's, step 2's),
#            datasets (the TOC's, "ADSL | ADAE") -- the
#            part rtfreporter does not read, kept in the report workbook's
#            `_tflplanner` sheet (a sheet whose name starts with `_` is
#            not read by rtfreporter)
#   setup    code every report program runs before its own data code
#
# The two workbooks split the sheets the way rtfreporter's samples do: the
# table half and `rounding` in table_spec.xlsx, the report half,
# `output_path` and `program_dir` in report_spec.xlsx.

#' Sheets of the two definition workbooks
#'
#' The sheet names each workbook carries, in the order they are written.
#'
#' @return `table_sheets()` and `report_sheets()` return a character vector.
#' @export
table_sheets <- function() {
  c("tables", "variables", "codelists", "cells", "digits", "layout", "columns",
    "style", "cell_styles", "col_header")
}

#' @rdname table_sheets
#' @export
report_sheets <- function() {
  c("report", "page", "header", "footer", "titles", "footnotes", "tokens")
}

#' @rdname table_sheets
#' @export
report_types <- function() c("table", "listing", "figure", "user")

# tflplanner wrote `program = {output_id}.R` on every study's default report
# row (and the company standards' default) up to 0.0.2.9057.  That was its
# default, not a choice: read as blank, so the program's own file name
# names it ({PROGRAM}) and the report's ID is only rtfreporter's last resort
# (tflspec's program_fallback).  A report's own row, or another default,
# is a choice and is kept.
.old_program_default <- function(p) {
  r <- p$sheets$report
  if (is.null(r) || !nrow(r) || !"program" %in% names(r)) return(p)
  old <- is.na(r$output_id) & !is.na(r$program) &
    trimws(r$program) == "{output_id}.R"
  if (any(old)) {
    r$program[old] <- NA_character_
    p$sheets$report <- r
  }
  p
}

#' Read a study's code list into its definition
#'
#' `read_codelist()` reads a code list -- one row a value of a variable:
#' `variable`, `value`, `label` (what it prints as) and `order` (its
#' place) -- from an `.xlsx` (its first sheet) or a `.csv` file.
#' `set_codelist()` puts it into the definition's `codelists` sheet as the
#' study's defaults (every table uses them): a value already there for the
#' same variable is replaced, the others are kept.  A report's own rows
#' replace the defaults for that report (tflspec's table spec).
#'
#' @param path An `.xlsx` or `.csv` file.
#' @param x A `tflplanner`.
#' @param rows What `read_codelist()` returns.
#' @return `read_codelist()`: a data frame; `set_codelist()`: the
#'   `tflplanner`.
#' @export
read_codelist <- function(path) {
  ext <- tolower(tools::file_ext(path))
  d <- switch(ext,
    xlsx = as.data.frame(readxl::read_excel(path, col_types = "text")),
    csv = utils::read.csv(path, colClasses = "character",
                          fileEncoding = "UTF-8-BOM", check.names = FALSE),
    stop("A code list is an .xlsx or a .csv file.", call. = FALSE))
  names(d) <- tolower(trimws(names(d)))
  miss <- setdiff(c("variable", "value"), names(d))
  if (length(miss)) {
    stop("The code list has no column ", paste(sQuote(miss), collapse = ", "),
         ": it needs variable and value (label and order are optional).",
         call. = FALSE)
  }
  for (k in c("label", "order")) if (!k %in% names(d)) d[[k]] <- NA_character_
  d <- d[c("variable", "value", "label", "order")]
  d[] <- lapply(d, function(v) {
    v <- trimws(as.character(v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  d <- d[!is.na(d$variable) & !is.na(d$value), , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' @rdname read_codelist
#' @export
set_codelist <- function(x, rows) {
  old <- sheet_rows(x, "codelists", NA)
  key <- function(d) paste(d$variable, d$value, sep = "\r")
  old <- old[!key(old) %in% key(rows), , drop = FALSE]
  old$output_id <- NULL
  set_sheet_rows(x, "codelists", NA, rbind(old[names(rows)], rows))
}

.study_keys <- list(table = "rounding",
                    report = c("output_path", "program_dir"))

.planner_sheet <- "_tflplanner"

# The columns a sheet has, straight from rtfreporter, plus the free `note`.
sheet_columns <- function(sheet) {
  c(names(tflspec::tfl_table_spec()[[sheet]]), "note")
}

.empty_sheet <- function(sheet) {
  cols <- sheet_columns(sheet)
  as.data.frame(stats::setNames(
    replicate(length(cols), character(), simplify = FALSE), cols),
    stringsAsFactors = FALSE)
}

# Any frame to the sheet's shape: every column text, blanks NA, the
# sheet's columns in its order, wholly blank rows dropped.
.normalize_sheet <- function(d, sheet) {
  cols <- sheet_columns(sheet)
  if (is.null(d)) return(.empty_sheet(sheet))
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  .check_retired_columns(d, sheet)
  out <- lapply(cols, function(cn) {
    v <- if (cn %in% names(d)) as.character(d[[cn]]) else
      rep(NA_character_, nrow(d))
    v <- trimws(v)
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  out <- as.data.frame(stats::setNames(out, cols), stringsAsFactors = FALSE)
  keep <- rowSums(!is.na(out)) > 0L
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Saved code that calls a name the table engine no longer has, rewritten as
# it is read (study workbooks, the app's state, the company standards):
# rtfreporter 0.8.0.9087 took the engine over from tflspec, with no alias
# for the former names.
.renamed_calls <- function(code) {
  code <- gsub("tflspec::tfl_ard_normalize(", "rtfreporter::normalize_ard(",
               code, fixed = TRUE)
  gsub("\\btfl_ard_normalize\\(", "normalize_ard(", code, perl = TRUE)
}

# Sheet columns a study saved before the plan verbs were redesigned
# (tflspec 0.0.23.9001, rtfreporter#498) may still have.  They are not read
# under their old names; the shape above would drop them, and their values
# with them, so a sheet that has one is refused instead, saying what to
# write.
.retired_sheet_columns <- list(
  layout = c(stub_into = "stub_name", group_show = "group_keep",
             colpages_carry = "colpages_keep",
             pages_by = "group_page = TRUE with group_col"))

.check_retired_columns <- function(d, sheet) {
  map <- .retired_sheet_columns[[sheet]]
  old <- intersect(names(map), names(d))
  if (length(old)) {
    stop("The `", sheet, "` sheet has column(s) from before tflplanner ",
         "0.0.1.9001: ", paste0(old, " (now ", map[old], ")",
                                collapse = ", "),
         ".
  Rename them in the study's workbook, or make the study ",
         "again.", call. = FALSE)
  }
  invisible(d)
}

.renamed_outputs <- function(p) {
  # a column the report list gained since (section): blank
  for (cn in setdiff(names(.empty_outputs()), names(p$outputs))) {
    p$outputs[[cn]] <- rep(NA_character_, nrow(p$outputs))
  }
  for (cn in c("data_code", "process_code")) {
    p$outputs[[cn]] <- .renamed_calls(p$outputs[[cn]])
  }
  p$setup <- .renamed_calls(p$setup)
  p
}

.empty_outputs <- function() {
  data.frame(output_id = character(), description = character(),
             data_code = character(), process_code = character(),
             section = character(), population = character(),
             datasets = character(), stringsAsFactors = FALSE)
}

#' A new, empty study definition
#'
#' @param study Named study keys (`rounding`, `output_path`,
#'   `program_dir`).
#' @return An `tflplanner` object.
#' @examples
#' p <- new_planner(c(rounding = "sas"))
#' p <- add_output(p, "DM", description = "Demographics")
#' p
#' @export
new_planner <- function(study = NULL) {
  keys <- unlist(.study_keys, use.names = FALSE)
  st <- stats::setNames(rep(NA_character_, length(keys)), keys)
  if (length(study)) {
    bad <- setdiff(names(study), keys)
    if (length(bad)) {
      stop("Unknown study key(s): ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    st[names(study)] <- as.character(study)
  }
  sh <- c(table_sheets(), report_sheets())
  structure(list(sheets = stats::setNames(lapply(sh, .empty_sheet), sh),
                 study = st, outputs = .empty_outputs(),
                 setup = NA_character_, ard = .empty_ard_spec(),
                 lf = .empty_lf(), fig_designs = list()),
            class = "tflplanner")
}

#' @export
print.tflplanner <- function(x, ...) {
  cat("<tflplanner> ", nrow(x$outputs), " report(s)", sep = "")
  if (nrow(x$outputs)) cat(":", paste(x$outputs$output_id, collapse = ", "))
  cat("\n")
  n <- vapply(x$sheets, nrow, 1L)
  n <- n[n > 0L]
  if (length(n)) {
    cat("  rows:", paste(sprintf("%s %d", names(n), n), collapse = ", "),
        "\n")
  }
  invisible(x)
}

#' Report ids the definition names
#'
#' The report list first, then any id a sheet or the ARD definition names
#' that the list lacks.
#' @param x An `tflplanner`.
#' @return A character vector.
#' @export
output_ids <- function(x) {
  seen <- unlist(lapply(x$sheets, `[[`, "output_id"), use.names = FALSE)
  # an output may be defined in the ARD before it is on the report list
  unique(c(x$outputs$output_id, stats::na.omit(seen),
           stats::na.omit(x$ard$analyses$output_id)))
}

# ---------------------------------------------------------------- reading

# A line break in a cell comes back as "\n" (openxlsx on Windows writes
# it as "\r\n").
.read_sheet_text <- function(path, sheet) {
  d <- readxl::read_excel(path, sheet, col_types = "text", .name_repair =
                            "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_along(d)) {
    if (is.character(d[[j]])) d[[j]] <- gsub("\r\n", "\n", d[[j]],
                                             fixed = TRUE)
  }
  d
}

#' Read the definition workbooks
#'
#' Reads `table_spec.xlsx` and `report_spec.xlsx` (or any rtfreporter
#' definition workbooks, one or several) through
#' [tflspec::tfl_read_report_spec()], so what the app opens is exactly what
#' the report programs will read.  The report list and the data code come
#' from the `_tflplanner` sheet when a workbook has one; otherwise the list
#' is every report a sheet names.
#'
#' @param path One or more `.xlsx` files.
#' @return An `tflplanner` object.
#' @export
read_planner <- function(path) {
  sp <- tflspec::tfl_read_report_spec(path)
  p <- new_planner()
  for (s in names(p$sheets)) p$sheets[[s]] <- .normalize_sheet(sp[[s]], s)
  p <- .old_program_default(p)
  st <- sp$study
  for (i in seq_len(nrow(st))) {
    if (st$key[i] %in% names(p$study) && !is.na(st$value[i])) {
      p$study[[st$key[i]]] <- st$value[i]
    }
  }
  for (f in path) {
    # _rtfplanner: the sheet's name before the package was renamed
    sh <- intersect(c(.planner_sheet, "_rtfplanner"), readxl::excel_sheets(f))
    if (length(sh)) {
      d <- .read_sheet_text(f, sh[1L])
      for (cn in names(.empty_outputs())) {
        if (!cn %in% names(d)) d[[cn]] <- NA_character_
      }
      d$output_id <- trimws(d$output_id)
      d$output_id[!is.na(d$output_id) & !nzchar(d$output_id)] <- NA
      setup <- d$data_code[is.na(d$output_id)]
      if (length(setup)) p$setup <- setup[1L]
      d <- d[!is.na(d$output_id), names(.empty_outputs()), drop = FALSE]
      p$outputs <- d[!duplicated(d$output_id), , drop = FALSE]
    }
  }
  for (id in setdiff(output_ids(p), p$outputs$output_id)) p <- add_output(p, id)
  rownames(p$outputs) <- NULL
  .renamed_outputs(p)
}

# ---------------------------------------------------------------- editing

.check_id <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
      !nzchar(trimws(id))) {
    stop("An output_id is a single non-blank string.", call. = FALSE)
  }
  if (grepl("[\\\\/:*?\"<>|]", id)) {
    stop("output_id '", id, "' has a character a file name cannot hold.",
         call. = FALSE)
  }
  trimws(id)
}

#' Edit the report list
#'
#' `add_output()` puts a report on the list; `copy_output()` gives a new
#' report every row of an existing one (the quickest start for a report
#' like one already defined); `rename_output()` changes an id everywhere;
#' `remove_output()` takes a report and all its rows out.
#'
#' @param x An `tflplanner`.
#' @param output_id,from,to Report ids.
#' @param description A short description shown in the report list.
#' @param data_code R code that makes the report's ARD, `ard` (for a
#'   listing or figure: its `content`); `NA` writes a TODO.
#' @param process_code R code that turns `ard` into `data`, what
#'   [rtfreporter::table_plan()] is given: [rtfreporter::normalize_ard()]
#'   and any rework after it (`mutate()` ...).  `NA` means
#'   `data <- normalize_ard(ard)`, unless `data_code` makes `data` itself.
#' @param type The report's type, one of [report_types()]; anything but
#'   `"table"` is written on the `report` sheet.
#' @param section The section of the TOC the report is under (its heading,
#'   "14.1 Demographics"); `NA`: from its ID's numbers.
#' @param population The report's analysis set, a population_id (see
#'   [set_report_population()], which also makes its analysis data).
#' @return The updated `tflplanner`.
#' @export
add_output <- function(x, output_id, description = NA_character_,
                       data_code = NA_character_, type = "table",
                       process_code = NA_character_, section = NA_character_,
                       population = NA_character_) {
  id <- .check_id(output_id)
  if (id %in% x$outputs$output_id) {
    stop("Report '", id, "' is already on the list.", call. = FALSE)
  }
  type <- match.arg(type, report_types())
  if (!identical(report_info(x, id)$type, type)) x <- .set_report_type(x, id, type)
  x$outputs <- rbind(x$outputs, data.frame(
    output_id = id, description = as.character(description),
    data_code = as.character(data_code),
    process_code = as.character(process_code),
    section = as.character(section), population = as.character(population),
    datasets = NA_character_, stringsAsFactors = FALSE))
  x
}

#' @rdname add_output
#' @export
copy_output <- function(x, from, to) {
  to <- .check_id(to)
  if (!from %in% output_ids(x)) stop("No report '", from, "'.", call. = FALSE)
  if (to %in% output_ids(x)) {
    stop("Report '", to, "' already exists.", call. = FALSE)
  }
  for (s in names(x$sheets)) {
    d <- x$sheets[[s]]
    own <- d[!is.na(d$output_id) & d$output_id == from, , drop = FALSE]
    own$output_id <- rep(to, nrow(own))
    d <- rbind(d, own)
    rownames(d) <- NULL
    x$sheets[[s]] <- d
  }
  an <- x$ard$analyses
  own <- an[!is.na(an$output_id) & an$output_id == from, , drop = FALSE]
  own$output_id <- rep(to, nrow(own))
  x$ard$analyses <- rbind(an, own)
  rownames(x$ard$analyses) <- NULL
  for (sh in names(x$lf)) {
    d <- x$lf[[sh]]
    own <- d[!is.na(d$output_id) & d$output_id == from, , drop = FALSE]
    own$output_id <- rep(to, nrow(own))
    x$lf[[sh]] <- rbind(d, own)
    rownames(x$lf[[sh]]) <- NULL
  }
  x <- set_fig_design(x, to, fig_design(x, from))
  src <- x$outputs[x$outputs$output_id == from, , drop = FALSE]
  # the copy is the same kind of report (its report row was copied above)
  x <- add_output(x, to,
                  description = if (nrow(src)) src$description else NA,
                  data_code = if (nrow(src)) src$data_code else NA,
                  process_code = if (nrow(src)) src$process_code else NA,
                  population = if (nrow(src)) src$population %||% NA else NA,
                  type = report_info(x, from)$type)
  x
}

#' @rdname add_output
#' @export
rename_output <- function(x, from, to) {
  to <- .check_id(to)
  if (identical(from, to)) return(x)
  if (to %in% output_ids(x)) {
    stop("Report '", to, "' already exists.", call. = FALSE)
  }
  for (s in names(x$sheets)) {
    i <- !is.na(x$sheets[[s]]$output_id) & x$sheets[[s]]$output_id == from
    x$sheets[[s]]$output_id[i] <- to
  }
  x$outputs$output_id[x$outputs$output_id == from] <- to
  i <- !is.na(x$ard$analyses$output_id) & x$ard$analyses$output_id == from
  x$ard$analyses$output_id[i] <- to
  for (sh in names(x$lf)) {
    i <- !is.na(x$lf[[sh]]$output_id) & x$lf[[sh]]$output_id == from
    x$lf[[sh]]$output_id[i] <- to
  }
  d <- fig_design(x, from)
  x <- set_fig_design(set_fig_design(x, from, NULL), to, d)
  x
}

#' @rdname add_output
#' @export
remove_output <- function(x, output_id) {
  for (s in names(x$sheets)) {
    d <- x$sheets[[s]]
    d <- d[is.na(d$output_id) | d$output_id != output_id, , drop = FALSE]
    rownames(d) <- NULL
    x$sheets[[s]] <- d
  }
  x$outputs <- x$outputs[x$outputs$output_id != output_id, , drop = FALSE]
  rownames(x$outputs) <- NULL
  an <- x$ard$analyses
  x$ard$analyses <- an[is.na(an$output_id) | an$output_id != output_id, ,
                       drop = FALSE]
  rownames(x$ard$analyses) <- NULL
  for (sh in names(x$lf)) {
    d <- x$lf[[sh]]
    x$lf[[sh]] <- d[is.na(d$output_id) | d$output_id != output_id, ,
                    drop = FALSE]
    rownames(x$lf[[sh]]) <- NULL
  }
  set_fig_design(x, output_id, NULL)
}

# The rows of a sheet one filter shows: "" = every row, NA = the defaults,
# an id = that report's own rows.
sheet_rows <- function(x, sheet, output_id = "") {
  d <- x$sheets[[sheet]]
  if (identical(output_id, "")) return(d)
  if (is.na(output_id)) return(d[is.na(d$output_id), , drop = FALSE])
  d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
}

# A grid's empty rows (its spare row, a row cleared) say nothing -- and
# must be gone before a filtered view stamps its output_id on every row,
# or they would come back as rows of nothing but an output_id.
.drop_blank_rows <- function(rows) {
  cols <- setdiff(names(rows), "output_id")
  if (!length(cols) || !nrow(rows)) return(rows)
  filled <- vapply(cols, function(c) {
    v <- as.character(rows[[c]])
    !is.na(v) & nzchar(trimws(v))
  }, logical(nrow(rows)))
  filled <- matrix(filled, nrow = nrow(rows))
  rows[rowSums(filled) > 0, , drop = FALSE]
}

# Put back what the filter showed, edited: the rows outside the filter
# stay, in place, and the edited ones go where the first shown one was.
set_sheet_rows <- function(x, sheet, output_id = "", rows) {
  d <- x$sheets[[sheet]]
  rows <- .drop_blank_rows(as.data.frame(rows, stringsAsFactors = FALSE))
  if (!identical(output_id, "")) rows$output_id <- rep(output_id, nrow(rows))
  rows <- .normalize_sheet(rows, sheet)
  if (identical(output_id, "")) {
    x$sheets[[sheet]] <- rows
    return(x)
  }
  shown <- if (is.na(output_id)) is.na(d$output_id) else
    !is.na(d$output_id) & d$output_id == output_id
  at <- if (any(shown)) which(shown)[1L] - 1L else nrow(d)
  rest <- d[!shown, , drop = FALSE]
  before <- sum(!shown[seq_len(at)])
  x$sheets[[sheet]] <- rbind(rest[seq_len(before), , drop = FALSE], rows,
                             rest[setdiff(seq_len(nrow(rest)),
                                          seq_len(before)), , drop = FALSE])
  rownames(x$sheets[[sheet]]) <- NULL
  x
}

# ---------------------------------------------------------------- writing

.spec_object <- function(x, sheets, keys) {
  st <- x$study[keys]
  st <- st[!is.na(st)]
  args <- x$sheets[sheets]
  args <- lapply(args, function(d) {
    if (all(is.na(d$note))) d$note <- NULL
    d
  })
  args$study <- if (length(st)) st else NULL
  tflspec::tfl_table_spec(args)
}

# tflspec writes the workbook -- its half's sheets, the study keys, the
# spec_version, each column's help on its header -- and the app adds the
# sheets tflspec does not read.
.write_book <- function(sp, path, writer, extra = list()) {
  writer(sp, path)
  if (!length(extra)) return(invisible(path))
  wb <- openxlsx::loadWorkbook(path)
  for (nm in names(extra)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, extra[[nm]], keepNA = FALSE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Write the two definition workbooks
#'
#' `table_spec.xlsx` gets the table sheets and `rounding`
#' ([tflspec::tfl_write_table_spec()]); `report_spec.xlsx` the report
#' sheets, `output_path`, `program_dir` ([tflspec::tfl_write_report_spec()])
#' and the `_tflplanner` sheet (report list and data code).  Each holds only
#' its own half's sheets; what a column means is a comment on its header
#' cell ([tflspec::tfl_spec_columns()]).  Both are checked by
#' [tflspec::tfl_table_spec()] on the way out.
#'
#' @param x An `tflplanner`.
#' @param dir Destination folder.
#' @param table_file,report_file File names.
#' @return The two paths, invisibly.
#' @export
write_planner <- function(x, dir, table_file = "table_spec.xlsx",
                          report_file = "report_spec.xlsx") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  tp <- file.path(dir, table_file)
  rp <- file.path(dir, report_file)
  .write_book(.spec_object(x, table_sheets(), .study_keys$table), tp,
              tflspec::tfl_write_table_spec)
  meta <- rbind(
    data.frame(output_id = NA_character_, description = "(every report)",
               data_code = x$setup, process_code = NA_character_,
               section = NA_character_, population = NA_character_,
               datasets = NA_character_, stringsAsFactors = FALSE),
    x$outputs[names(.empty_outputs())])
  .write_book(.spec_object(x, report_sheets(), .study_keys$report), rp,
              tflspec::tfl_write_report_spec,
              stats::setNames(list(meta), .planner_sheet))
  invisible(c(table = tp, report = rp))
}

#' Check the definition the way the report programs will read it
#'
#' Writes the workbooks to a temporary folder and reads each report back
#' with [tflspec::tfl_read_report_spec()] narrowed to it, the call every
#' report program makes.  (What needs the data -- a column the ARD lacks --
#' shows only when the program runs.)
#'
#' @param x An `tflplanner`.
#' @return A data frame: `output_id`, `ok`, `message`.
#' @export
check_planner <- function(x) {
  dir <- tempfile("tflplanner")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- tryCatch(write_planner(x, dir), error = function(e) e)
  if (inherits(paths, "error")) {
    return(data.frame(output_id = "(workbook)", ok = FALSE,
                      message = conditionMessage(paths),
                      stringsAsFactors = FALSE))
  }
  ids <- output_ids(x)
  res <- lapply(ids, function(id) {
    msg <- character()
    r <- withCallingHandlers(
      tryCatch({
        tflspec::tfl_read_report_spec(unname(rev(paths)), output_id = id)
        TRUE
      }, error = function(e) {
        msg <<- c(msg, conditionMessage(e))
        FALSE
      }),
      message = function(m) {
        msg <<- c(msg, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        msg <<- c(msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    data.frame(output_id = id, ok = r,
               message = paste(msg, collapse = "\n"),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(output_id = character(),
                                          ok = logical(),
                                          message = character())), res))
  rownames(out) <- NULL
  out
}

# A report's type, on its report row (added when it has none)
.set_report_type <- function(x, output_id, type) {
  type <- match.arg(type, report_types())
  r <- x$sheets$report
  i <- which(!is.na(r$output_id) & r$output_id == output_id)
  if (!length(i)) {
    r[nrow(r) + 1L, ] <- NA_character_
    i <- nrow(r)
    r$output_id[i] <- output_id
  }
  r$type[i] <- type
  x$sheets$report <- r
  x
}
