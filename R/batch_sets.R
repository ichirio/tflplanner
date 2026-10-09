# Named batches: a study's official run of some of its reports, by a name
# ("Topline", "Interim Analysis", "Final").  A report says which it belongs
# to -- the report list's `batches` column, " | " between several -- so a
# batch needs no list of its own to keep up to date, and the TOC import can
# fill the column (a Topline flag).  The full run (every report) is the
# default and is not a named batch.  programs/batch.R carries them as
# `.batch_sets`; `Rscript programs/autoexec_all.R --batch Topline` runs one.

#' Named batches of a study's reports
#'
#' A named batch is a set of reports an official run can take by name
#' ([run_batch()]`(batch = )`).  A report's batches are the report list's
#' `batches` column (`"Topline | Final"`).
#'
#' * `batch_sets()`: every name and its reports, in the report list's
#'   order.
#' * `set_batch()`: the batch `name` is these reports (and no other): a new
#'   name, or one overwritten.
#' * `rename_batch()`, `remove_batch()`: the name changed, or taken off
#'   every report (the reports stay).
#' * `batch_set_problems()`: what an official run would trip on: a name a
#'   batch folder cannot carry, a batch of no report.
#'
#' @param x A `tflplanner`.
#' @param name,from,to A batch's name.
#' @param output_ids The reports of the batch.
#' @return `batch_sets()`: a named list of output ids.  `set_batch()`,
#'   `rename_batch()`, `remove_batch()`: the `tflplanner`.
#'   `batch_set_problems()`: a data frame `batch`, `problem`.
#' @examples
#' p <- add_output(add_output(new_planner(), "DM"), "AE")
#' p <- set_batch(p, "Topline", "DM")
#' p <- set_batch(p, "Final", c("DM", "AE"))
#' batch_sets(p)
#' @export
batch_sets <- function(x) {
  b <- x$outputs$batches %||% rep(NA_character_, nrow(x$outputs))
  of <- lapply(b, function(v) if (is.na(v)) character() else .split_bar(v))
  names <- unique(unlist(of))
  stats::setNames(lapply(names, function(n)
    x$outputs$output_id[vapply(of, function(v) n %in% v, NA)]), names)
}

#' @rdname batch_sets
#' @export
set_batch <- function(x, name, output_ids) {
  name <- .check_batch_name(name)
  bad <- setdiff(output_ids, x$outputs$output_id)
  if (length(bad)) {
    stop("No report ", paste(sQuote(bad), collapse = ", "), " on the list.",
         call. = FALSE)
  }
  .batch_members(x, name, x$outputs$output_id %in% output_ids)
}

#' @rdname batch_sets
#' @export
rename_batch <- function(x, from, to) {
  to <- .check_batch_name(to)
  sets <- batch_sets(x)
  if (!from %in% names(sets)) stop("No batch ", sQuote(from), ".", call. = FALSE)
  if (to %in% names(sets) && !identical(to, from)) {
    stop("A batch ", sQuote(to), " exists already.", call. = FALSE)
  }
  ids <- sets[[from]]
  x <- .batch_members(x, from, rep(FALSE, nrow(x$outputs)))
  .batch_members(x, to, x$outputs$output_id %in% ids)
}

#' @rdname batch_sets
#' @export
remove_batch <- function(x, name) {
  .batch_members(x, name, rep(FALSE, nrow(x$outputs)))
}

#' @rdname batch_sets
#' @export
batch_set_problems <- function(x) {
  sets <- batch_sets(x)
  out <- data.frame(batch = character(), problem = character(),
                    stringsAsFactors = FALSE)
  for (n in names(sets)) {
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9 ._-]*$", n)) {
      out[nrow(out) + 1L, ] <- list(n, "a batch folder cannot carry this name: letters, digits, spaces, . _ - only")
    }
  }
  out
}

# a batch's name: what a batch folder can carry (runs/<stamp>_all_<name>)
.check_batch_name <- function(name) {
  name <- trimws(as.character(name))
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("Give the batch a name.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9 ._-]*$", name)) {
    stop("A batch's name is letters, digits, spaces, . _ - (it names the ",
         "batch folder): ", sQuote(name), ".", call. = FALSE)
  }
  if (grepl("|", name, fixed = TRUE)) stop("No | in a batch's name.", call. = FALSE)
  name
}

# the reports `on` have batch `name`; the others have it not
.batch_members <- function(x, name, on) {
  b <- x$outputs$batches %||% rep(NA_character_, nrow(x$outputs))
  b <- vapply(seq_along(b), function(i) {
    v <- if (is.na(b[i])) character() else .split_bar(b[i])
    v <- setdiff(v, name)
    if (on[i]) v <- c(v, name)
    if (length(v)) paste(v, collapse = " | ") else NA_character_
  }, "")
  x$outputs$batches <- b
  x
}

# a batch's name as a batch folder carries it (runs/<stamp>_all_Topline)
.batch_folder_name <- function(name) gsub("[^A-Za-z0-9._-]", "_", name)
