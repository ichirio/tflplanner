# Listings and figures.
#
# A listing is defined like a table, in rows: which data (a dataset of the
# study's catalog -- SDTM or ADaM -- and a condition), in which order, over
# how many rows a page, and its columns (rtfreporter's listing_col(): the
# variables stacked in a column, the header, the width, whether a repeated
# value is printed once).  Its program reads the data, reworks it with the
# report's data code when it has one, and lays it out with
# rtfreporter::listing_spec() / as_rtftables().  The code is written by
# tflspec::tfl_listing_code(); the rows are kept and edited here.  The
# listing sheets (`listings`, `listing_cols`) are tflspec's listing
# definition -- their columns, reading and checking are tflspec's
# (tfl_listing_spec(), tfl_read_listing_spec()); only `figures` is ours.
#
# A figure names the datasets it reads; its program reads them and writes
# the RTF, and the plot in between -- ggplot2 -- is the report's data code,
# written by hand (it leaves `plot`, or `content`).

.lf_sheet_names <- c("listings", "listing_cols", "figures")
# figures: the datasets a figure written by hand reads; also a user-code
# report's, and whether that one reads its ARD (`ard`, TRUE / blank)
.lf_figure_cols <- c("output_id", "datasets", "ard")

.empty_lf <- function() {
  c(unclass(tflspec::tfl_listing_spec()),
    list(figures = as.data.frame(stats::setNames(
      replicate(length(.lf_figure_cols), character(), simplify = FALSE),
      .lf_figure_cols), stringsAsFactors = FALSE)))
}

.normalize_lf_sheet <- function(d, sheet) {
  if (sheet != "figures") {
    return(tflspec::tfl_listing_spec(stats::setNames(list(d), sheet),
                                     check = FALSE)[[sheet]])
  }
  cols <- .lf_figure_cols
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

# How many of a listing's settings the listing sheets' columns cannot say:
# its items in content_more and its columns' other arguments (more), kept as
# YAML (tflspec >= 0.0.24.9226) and written back as they are
.lf_more_count <- function(x, output_id) {
  n_yaml <- function(v) {
    v <- v[!is.na(v) & nzchar(trimws(v))]
    sum(vapply(v, function(s) {
      y <- tryCatch(yaml::yaml.load(s), error = function(e) NULL)
      if (is.null(y)) 1L else length(y)
    }, 1L))
  }
  l <- lf_rows(x, "listings", output_id)
  cl <- lf_rows(x, "listing_cols", output_id)
  c(items = if (!is.null(l$content_more)) n_yaml(l$content_more) else 0L,
    columns = if (!is.null(cl$more)) sum(!is.na(cl$more) & nzchar(trimws(cl$more))) else 0L)
}
.lf_more_note <- function(x, output_id, t = identity) {
  n <- .lf_more_count(x, output_id)
  if (!sum(n)) return(NULL)
  shiny::div(class = "small text-muted mb-1",
             sprintf(t("%d settings the sheet cannot say (kept): %d of the listing's (content_more), %d of its columns (more). Edit them in the report's YAML."),
                     sum(n), n[["items"]], n[["columns"]]))
}

#' @rdname lf_rows
#' @export
listing_types <- function() company_standards()$listing_types

# the lines that read one dataset of the catalog into an object named
# after it (adsl, adae ...), with its derived columns
.read_dataset_lines <- function(x, dataset) {
  tflspec::tfl_read_data_code(x$ard$datasets, dataset)
}

# the part of a listing's program between its setup and its report (part
# "data": its data only; "content": laid out from a `data` made before)
.listing_lines <- function(x, output_id, rework = NA, part = "all") {
  if (!nrow(lf_rows(x, "listings", output_id))) return(NULL)
  tflspec::tfl_listing_code(
    tflspec::tfl_listing_spec(x$lf, check = FALSE), output_id,
    x$ard$datasets, rework = if (!is.na(rework)) .code_block(rework),
    type = .std_setting("listing_type", "multiline"),
    codelists = .study_codelists(x), part = part)
}

# the part of a figure's program between its setup and its report: a
# figure with no design and no code of its own yet -- its datasets read,
# the company's plot template or a TODO
.figure_lines <- function(x, output_id, info) {
  ds <- report_datasets(x, output_id)
  if (!length(ds)) return(NULL)
  tpl <- .fill_template("figure_plot", x, output_id)
  plot <- c(
    if (!is.na(tpl)) .code_block(tpl) else c(
      "# TODO: the plot, e.g.",
      "#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram()"),
    paste0("stop(\"tflplanner: the plot of ", info$program,
           " is still to be written.\")"))
  makes_content <- FALSE
  setup <- file.path(study_layout()[["programs_tfl"]], .fig_setup_file)
  c(if (length(ds)) c(paste0("# the data: ", paste(ds, collapse = ", "),
                             " (data catalog)"),
                      unlist(lapply(ds, function(d) .read_dataset_lines(x, d))),
                      ""),
    "# the figure style of the company standards, and the figure checks",
    sprintf("source(%s)", encodeString(setup, quote = "\"")),
    "",
    "# ---- the plot (ggplot2), written by hand: leaves `plot`",
    "#      (theme_tfl(), scale_colour_tfl(), tfl_marker() ... give the standard's look)",
    plot,
    if (!makes_content) c(
      "",
      "# the figure checks: dropped rows, colours against the standard",
      "tfl_check(plot)"))
}

# A designed figure's lines (#293): the study's figure setup sourced (the
# program sources it at its top instead), then the design's code -- its
# `# ---- data ----` and `# ---- plot ----` -- and the figure checks
.fig_design_lines <- function(x, output_id, design, source = TRUE) {
  c(if (source) c(.fig_source_lines(), ""),
    .fig_design_plot(design, output_id, codelists = .fig_codelists(x, output_id), x = x))
}

# A figure's code lists: its own rows, and -- a figure printing a table's
# numbers (ard_source table:<id>) -- that table's for the variables it has
# none of: the figure shows its arms in the table's order and colours
.fig_codelists <- function(x, output_id) {
  cl <- .study_codelists(x)
  src <- .fig_ard_source(x, output_id)
  if (is.null(cl) || !identical(src$kind, "table")) return(cl)
  tb <- cl[cl$output_id %in% src$id & !cl$variable %in%
             cl$variable[cl$output_id %in% output_id], , drop = FALSE]
  if (!nrow(tb)) return(cl)
  tb$output_id <- output_id
  rbind(cl, tb)
}

# A designed figure's code in its two steps: `data` (its ARD, the datasets
# it reads, its `# ---- data ----`) and `plot` (its `# ---- plot ----` and
# the figure checks) -- for a report whose other step is its own code
.fig_design_parts <- function(x, output_id, design) {
  lines <- .fig_design_plot(design, output_id, codelists = .study_codelists(x), x = x)
  at <- match(TRUE, startsWith(lines, "# ---- plot"), nomatch = length(lines) + 1L)
  list(data = utils::head(lines, at - 1L),
       plot = utils::tail(lines, length(lines) - at + 1L))
}

.fig_source_lines <- function() {
  c("# the figure style of the company standards, the figures' packages, and the figure checks",
    sprintf("source(%s)", encodeString(file.path(study_layout()[["programs_tfl"]],
                                                 .fig_setup_file), quote = "\"")))
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
  code <- c(.report_id_line(output_id), code)
  owd <- setwd(study$path)
  on.exit(setwd(owd), add = TRUE)
  e <- new.env(parent = .helpers_env(asNamespace("rtfreporter")))
  eval(parse(text = code, encoding = "UTF-8"), envir = e)
  content <- e$content
  if (inherits(content, "rtftable")) content <- list(content)
  content
}

.lf_file <- "listing_figure_spec.xlsx"

# the workbook, written with the study when it defines a listing or figure
# `was`: the listings and figures the file holds when it is known (the last
# save's, its fingerprint unchanged): compared with, not read (#274)
.save_lf <- function(p, root, was = NULL) {
  lf <- p$lf %||% .empty_lf()
  f <- file.path(root, study_layout()[["spec"]], .lf_file)
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  if (!sum(vapply(lf, nrow, 1L)) && !file.exists(f)) return(out)
  old <- if (!is.null(was)) {
    stats::setNames(lapply(.lf_sheet_names, function(sh)
      .normalize_lf_sheet(was[[sh]], sh)), .lf_sheet_names)
  } else if (file.exists(f)) tryCatch(stats::setNames(lapply(.lf_sheet_names,
    function(sh) .normalize_lf_sheet(.read_sheet_text(f, sh), sh)),
    .lf_sheet_names), error = function(e) NULL)
  new <- stats::setNames(lapply(.lf_sheet_names, function(sh)
    .normalize_lf_sheet(lf[[sh]], sh)), .lf_sheet_names)
  if (identical(old, new)) {
    out[1L, ] <- list(f, "unchanged")
  } else {
    writexl::write_xlsx(new, f)
    out[1L, ] <- list(f, "written")
  }
  out
}
