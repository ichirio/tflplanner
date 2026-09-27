# Listings and figures.
#
# A listing is defined like a table, in rows: which data (a dataset of the
# study's catalog -- SDTM or ADaM -- and a condition), in which order, over
# how many rows a page, and its columns (rtfreporter's listing_col(): the
# variables stacked in a column, the header, the width, whether a repeated
# value is printed once).  Its program reads the data, reworks it with the
# report's data code when it has one, and lays it out with
# rtfreporter::listing_spec() / as_rtftables().  The code is written by
# tflspec::listing_spec_code(); the rows are kept and edited here.
#
# A figure names the datasets it reads; its program reads them and writes
# the RTF, and the plot in between -- ggplot2 -- is the report's data code,
# written by hand (it leaves `plot`, or `content`).

.lf_sheets <- list(
  listings = c("output_id", "type", "dataset", "where", "sort", "max_rows"),
  listing_cols = c("output_id", "vars", "label", "width", "collapse_repeats"),
  figures = c("output_id", "datasets"))

.empty_lf <- function() {
  lapply(.lf_sheets, function(cols)
    as.data.frame(stats::setNames(replicate(length(cols), character(),
                                            simplify = FALSE), cols),
                  stringsAsFactors = FALSE))
}

.normalize_lf_sheet <- function(d, sheet) {
  cols <- .lf_sheets[[sheet]]
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  out <- lapply(cols, function(c) {
    v <- if (c %in% names(d)) as.character(d[[c]]) else
      rep(NA_character_, nrow(d))
    v <- trimws(v)
    v[!is.na(v) & !nzchar(v)] <- NA
    v
  })
  out <- as.data.frame(stats::setNames(out, cols), stringsAsFactors = FALSE)
  out <- out[rowSums(!is.na(out[setdiff(cols, "output_id")])) > 0, ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

#' A listing's or figure's definition
#'
#' `lf_rows()` gives a report's rows of `listings`, `listing_cols` or
#' `figures`; `set_lf_rows()` puts them back, edited.  `listing_types()`
#' are the listing types of the company standards (rtfreporter's).
#'
#' @param x A `tflplanner`.
#' @param sheet `listings`, `listing_cols` or `figures`.
#' @param output_id The report.
#' @param rows The rows, edited.
#' @return `lf_rows()`: a data frame; `set_lf_rows()`: the `tflplanner`;
#'   `listing_types()`: a data frame (`type`, `label`, `note`).
#' @export
lf_rows <- function(x, sheet, output_id) {
  d <- (x$lf %||% .empty_lf())[[sheet]]
  d[!is.na(d$output_id) & d$output_id == output_id, , drop = FALSE]
}

#' @rdname lf_rows
#' @export
set_lf_rows <- function(x, sheet, output_id, rows) {
  if (is.null(x$lf)) x$lf <- .empty_lf()
  rows <- .drop_blank_rows(as.data.frame(rows, stringsAsFactors = FALSE))
  rows$output_id <- rep(output_id, nrow(rows))
  rows <- .normalize_lf_sheet(rows, sheet)
  d <- x$lf[[sheet]]
  mine <- !is.na(d$output_id) & d$output_id == output_id
  at <- if (any(mine)) which(mine)[1L] - 1L else nrow(d)
  rest <- d[!mine, , drop = FALSE]
  before <- sum(!mine[seq_len(at)])
  d <- rbind(rest[seq_len(before), , drop = FALSE], rows,
             rest[setdiff(seq_len(nrow(rest)), seq_len(before)), ,
                  drop = FALSE])
  rownames(d) <- NULL
  x$lf[[sheet]] <- d
  x
}

#' @rdname lf_rows
#' @export
listing_types <- function() company_standards()$listing_types

# the lines that read one dataset of the catalog into an object named
# after it (adsl, adae ...), with its derived columns
.read_dataset_lines <- function(x, dataset) {
  tflspec::read_data_code(x$ard$datasets, dataset)
}

# the part of a listing's program between its setup and its report
.listing_lines <- function(x, output_id, rework = NA) {
  tflspec::listing_spec_code(
    lf_rows(x, "listings", output_id), lf_rows(x, "listing_cols", output_id),
    x$ard$datasets, rework = if (!is.na(rework)) .code_block(rework),
    type = .std_setting("listing_type", "multiline"))
}

# the part of a figure's program between its setup and its report
.figure_lines <- function(x, output_id, info, plot_code = NA) {
  f <- lf_rows(x, "figures", output_id)
  ds <- if (nrow(f)) .split_bar(f$datasets[1L]) else character()
  if (!length(ds) && is.na(plot_code)) return(NULL)
  tpl <- .fill_template("figure_plot", x, output_id)
  plot <- if (is.na(plot_code)) c(
    if (!is.na(tpl)) .code_block(tpl) else c(
      "# TODO: the plot, e.g.",
      "#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram()"),
    paste0("stop(\"tflplanner: the plot of ", info$program,
           " is still to be written.\")")) else .code_block(plot_code)
  makes_content <- !is.na(plot_code) &&
    any(grepl("(^|[^A-Za-z0-9_.])content[[:space:]]*(<-|=)[^=]",
              strsplit(plot_code, "\n", fixed = TRUE)[[1L]]))
  c(if (length(ds)) c(paste0("# the data: ", paste(ds, collapse = ", "),
                             " (data catalog)"),
                      unlist(lapply(ds, function(d) .read_dataset_lines(x, d))),
                      ""),
    "# ---- the plot (ggplot2), written by hand: leaves `plot`",
    plot,
    if (!makes_content) c("", "content <- list(plot)"))
}

#' The rows of a listing, as they will print
#'
#' Runs a listing's data part ([data_lines()]) from the study folder and
#' gives its first pages, for a preview.
#'
#' @param study An `rtfstudy`.
#' @param output_id The listing.
#' @return A list of `rtftable` pages.
#' @export
preview_listing <- function(study, output_id) {
  code <- data_lines(study$planner, output_id, todo = FALSE)
  if (is.null(code)) stop("The listing has no data yet.", call. = FALSE)
  owd <- setwd(study$path)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = asNamespace("rtfreporter"))
  eval(parse(text = code, encoding = "UTF-8"), envir = e)
  content <- e$content
  if (inherits(content, "rtftable")) content <- list(content)
  content
}

.lf_file <- "listing_figure_spec.xlsx"

# the workbook, written with the study when it defines a listing or figure
.save_lf <- function(p, root) {
  lf <- p$lf %||% .empty_lf()
  f <- file.path(root, study_layout()[["spec"]], .lf_file)
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  if (!sum(vapply(lf, nrow, 1L)) && !file.exists(f)) return(out)
  old <- if (file.exists(f)) tryCatch(stats::setNames(lapply(names(.lf_sheets),
    function(sh) .normalize_lf_sheet(.read_sheet_text(f, sh), sh)),
    names(.lf_sheets)), error = function(e) NULL)
  new <- stats::setNames(lapply(names(.lf_sheets), function(sh)
    .normalize_lf_sheet(lf[[sh]], sh)), names(.lf_sheets))
  if (identical(old, new)) {
    out[1L, ] <- list(f, "unchanged")
  } else {
    writexl::write_xlsx(new, f)
    out[1L, ] <- list(f, "written")
  }
  out
}
