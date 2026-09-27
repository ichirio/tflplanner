# The planner: one study's definition as the app edits it.
#
#   sheets   every rtfreporter sheet as an all-character data frame, the
#            columns rtfreporter::table_spec() gives it plus `note`
#   study    the study sheet's keys, a named character vector
#   outputs  the report list: output_id, description, data_code (makes
#            the ARD), process_code (normalizes and reworks it) -- the
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
  c("tables", "variables", "cells", "layout", "columns", "style",
    "col_header")
}

#' @rdname table_sheets
#' @export
report_sheets <- function() {
  c("report", "page", "header", "footer", "titles", "footnotes")
}

#' @rdname table_sheets
#' @export
report_types <- function() c("table", "listing", "figure")

.study_keys <- list(table = "rounding",
                    report = c("output_path", "program_dir"))

.planner_sheet <- "_tflplanner"

# The columns a sheet has, straight from rtfreporter, plus the free `note`.
sheet_columns <- function(sheet) {
  c(names(rtfreporter::table_spec()[[sheet]]), "note")
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

.empty_outputs <- function() {
  data.frame(output_id = character(), description = character(),
             data_code = character(), process_code = character(),
             stringsAsFactors = FALSE)
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
                 lf = .empty_lf()),
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

.read_sheet_text <- function(path, sheet) {
  d <- readxl::read_excel(path, sheet, col_types = "text", .name_repair =
                            "minimal")
  as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Read the definition workbooks
#'
#' Reads `table_spec.xlsx` and `report_spec.xlsx` (or any rtfreporter
#' definition workbooks, one or several) through
#' [rtfreporter::read_report_spec()], so what the app opens is exactly what
#' the report programs will read.  The report list and the data code come
#' from the `_tflplanner` sheet when a workbook has one; otherwise the list
#' is every report a sheet names.
#'
#' @param path One or more `.xlsx` files.
#' @return An `tflplanner` object.
#' @export
read_planner <- function(path) {
  sp <- rtfreporter::read_report_spec(path)
  p <- new_planner()
  for (s in names(p$sheets)) p$sheets[[s]] <- .normalize_sheet(sp[[s]], s)
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
  p
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
#'   [rtfreporter::rtf_plan()] is given: [rtfreporter::ard_normalize()]
#'   and any rework after it (`mutate()` ...).  `NA` means
#'   `data <- ard_normalize(ard)`, unless `data_code` makes `data` itself.
#' @param type The report's type, one of [report_types()]; anything but
#'   `"table"` is written on the `report` sheet.
#' @return The updated `tflplanner`.
#' @export
add_output <- function(x, output_id, description = NA_character_,
                       data_code = NA_character_, type = "table",
                       process_code = NA_character_) {
  id <- .check_id(output_id)
  if (id %in% x$outputs$output_id) {
    stop("Report '", id, "' is already on the list.", call. = FALSE)
  }
  type <- match.arg(type, report_types())
  if (!identical(report_info(x, id)$type, type)) {
    r <- x$sheets$report
    i <- which(!is.na(r$output_id) & r$output_id == id)
    if (!length(i)) {
      r[nrow(r) + 1L, ] <- NA_character_
      i <- nrow(r)
      r$output_id[i] <- id
    }
    r$type[i] <- type
    x$sheets$report <- r
  }
  x$outputs <- rbind(x$outputs, data.frame(
    output_id = id, description = as.character(description),
    data_code = as.character(data_code),
    process_code = as.character(process_code), stringsAsFactors = FALSE))
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
  src <- x$outputs[x$outputs$output_id == from, , drop = FALSE]
  x <- add_output(x, to,
                  description = if (nrow(src)) src$description else NA,
                  data_code = if (nrow(src)) src$data_code else NA,
                  process_code = if (nrow(src)) src$process_code else NA)
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
  x
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
  rtfreporter::table_spec(args)
}

.readme <- function() {
  f <- system.file("extdata", "ard-spec", "study.xlsx",
                   package = "rtfreporter")
  if (!nzchar(f) || !"_README" %in% readxl::excel_sheets(f)) return(NULL)
  .read_sheet_text(f, "_README")
}

# rtfreporter writes the workbook -- its sheets, the study keys, the
# spec_version -- and the app adds the sheets rtfreporter does not read.
.write_book <- function(sp, path, extra = list()) {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  rtfreporter::write_table_spec(sp, tmp)
  books <- lapply(readxl::excel_sheets(tmp), function(s)
    .read_sheet_text(tmp, s))
  names(books) <- readxl::excel_sheets(tmp)
  rd <- .readme()
  out <- c(if (!is.null(rd)) list(`_README` = rd), books, extra)
  writexl::write_xlsx(out, path)
  invisible(path)
}

#' Write the two definition workbooks
#'
#' `table_spec.xlsx` gets the table sheets and `rounding`;
#' `report_spec.xlsx` the report sheets, `output_path`, `program_dir`, and
#' the `_tflplanner` sheet (report list and data code).  Both carry every
#' sheet rtfreporter defines -- the other half's sheets empty -- and its
#' `_README`, and both are checked by [rtfreporter::table_spec()] on the
#' way out.
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
  .write_book(.spec_object(x, table_sheets(), .study_keys$table), tp)
  meta <- rbind(
    data.frame(output_id = NA_character_, description = "(every report)",
               data_code = x$setup, process_code = NA_character_,
               stringsAsFactors = FALSE),
    x$outputs)
  .write_book(.spec_object(x, report_sheets(), .study_keys$report), rp,
              stats::setNames(list(meta), .planner_sheet))
  invisible(c(table = tp, report = rp))
}

#' Check the definition the way the report programs will read it
#'
#' Writes the workbooks to a temporary folder and reads each report back
#' with [rtfreporter::read_report_spec()] narrowed to it, the call every
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
        rtfreporter::read_report_spec(unname(rev(paths)), output_id = id)
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
