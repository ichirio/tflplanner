# Run definitions: a study's official runs, each a name, the reports it
# selects and its options ("Topline", "Final").  One saved thing with one
# owner: p$runs, a full part of the planner (state, draft, merge, import),
# written to spec/runs.yml by .spec_sync() through tflspec::tfl_runs_text().
# tflspec parses, checks and writes the file's text; this file is the
# planner's side of it: the list, its edits and the save rule.
#
# p$runs is an unnamed list of definitions, each
#   list(name = , reports = <ids, or the word "all">, mode = , copy_data = )
# (mode and copy_data only when set; a key tflspec does not know is kept).
# The attribute `migrated` (how many) says the list was made from an older
# carrier of a report's membership (toc.yml `sets:`, the report list's
# `batches` column) and is not on disk in the new form yet; `read_problems`
# holds what tflspec found wrong in the file it was read from.

#' Run definitions of a study
#'
#' A run definition is a saved official run: a name, the reports it selects
#' and its options.  `spec/runs.yml` holds them.  A run always starts from a
#' saved definition ([run_batch()]).
#'
#' * `run_definitions()`: each definition, in the order of the file.
#' * `set_run_definition()`: the definition `name` is these `reports` (output
#'   ids, or the word `"all"`: every report of the study) with these options.
#'   A name the study has (spelt the same) is updated, an argument left
#'   `NULL` stays as it was; a new name needs `reports`.  A new name must
#'   keep the name rule -- ASCII letters, digits, `_` and `-`; it starts with
#'   a letter; at most 40 characters -- and differ from every other name
#'   ignoring case.  An older name that breaks the rule can be updated: the
#'   Review tab asks for a rename.
#' * `rename_run_definition()`, `remove_run_definition()`: the name changed
#'   (`Topline` to `topline` is the same definition), or the definition
#'   taken off the study (its reports stay).
#'
#' `mode` is `"all"` (the ARD, then the reports; the default) or `"ard"`
#' (the ARD only).  `copy_data` keeps the data the programs read in the run
#' folder.
#'
#' @param x A `tflplanner`.
#' @param name,from,to A definition's name.
#' @param reports The reports of the definition (output ids, in any order:
#'   they run in the report list's order), or `"all"`.
#' @param mode `"all"` or `"ard"`.
#' @param copy_data `TRUE` or `FALSE`.
#' @return `run_definitions()`: a data frame `name`, `reports` (how many),
#'   `mode`, `copy_data` and `problems` (what the Review tab says, `""` for
#'   none).  The others: the `tflplanner`.
#' @examples
#' p <- add_output(add_output(new_planner(), "DM"), "AE")
#' p <- set_run_definition(p, "Topline", "DM")
#' p <- set_run_definition(p, "Final", "all")
#' run_definitions(p)
#' @export
run_definitions <- function(x) {
  r <- x$runs %||% list()
  nm <- .run_names(x)
  chk <- .runs_problems(x)
  data.frame(
    name = nm,
    reports = vapply(r, function(d) length(.run_def_ids(x, d)), 1L, USE.NAMES = FALSE),
    mode = vapply(r, function(d) as.character(d$mode %||% "all")[1L], "", USE.NAMES = FALSE),
    copy_data = vapply(r, function(d) isTRUE(d$copy_data), NA, USE.NAMES = FALSE),
    problems = vapply(seq_along(r), function(i)
      paste(chk$problem[chk$definition == .run_label(nm, i)], collapse = "; "), "",
      USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
}

#' @rdname run_definitions
#' @export
set_run_definition <- function(x, name, reports = NULL, mode = NULL, copy_data = NULL) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("Give the run definition a name.", call. = FALSE)
  }
  r <- x$runs %||% list()
  i <- match(name, .run_names(x))
  new <- is.na(i)
  if (new) {
    if (is.null(reports)) {
      stop("A new run definition needs its reports (`reports`).", call. = FALSE)
    }
    d <- list(name = name)
  } else {
    d <- r[[i]]
  }
  if (!is.null(reports)) d$reports <- .run_reports_arg(x, reports)
  if (!is.null(mode)) d$mode <- mode
  if (!is.null(copy_data)) d$copy_data <- copy_data
  r[[if (new) length(r) + 1L else i]] <- d
  y <- x
  y$runs <- .runs_keep_attr(r, x$runs)
  .run_def_check(y, name, new)
  y
}

#' @rdname run_definitions
#' @export
rename_run_definition <- function(x, from, to) {
  if (!is.character(to) || length(to) != 1L || is.na(to) || !nzchar(to)) {
    stop("Give the run definition a name.", call. = FALSE)
  }
  nm <- .run_names(x)
  i <- match(from, nm)
  if (is.na(i)) stop("No run definition ", sQuote(from), ".", call. = FALSE)
  if (identical(from, to)) return(x)
  # (the same name but for the case is the same definition: no clash with itself)
  other <- nm[-i]
  if (tolower(to) %in% tolower(other)) {
    stop("A run definition ", sQuote(other[match(tolower(to), tolower(other))]),
         " exists already (names are compared ignoring case).", call. = FALSE)
  }
  p <- .run_name_problem(to)
  if (nzchar(p)) stop(p, call. = FALSE)
  r <- x$runs
  r[[i]]$name <- to
  x$runs <- .runs_keep_attr(r, x$runs)
  x
}

#' @rdname run_definitions
#' @export
remove_run_definition <- function(x, name) {
  i <- match(name, .run_names(x))
  if (is.na(i)) stop("No run definition ", sQuote(name), ".", call. = FALSE)
  r <- x$runs[-i]
  x$runs <- .runs_keep_attr(r, x$runs)
  x
}

# ---- the list ---------------------------------------------------------------

# the names, in the order of the list (NA for an entry with none)
.run_names <- function(x) {
  vapply(x$runs %||% list(), function(d) {
    n <- d$name
    if (is.character(n) && length(n) == 1L) n else NA_character_
  }, "", USE.NAMES = FALSE)
}

# how a definition is named in a problem row (tflspec's labels)
.run_label <- function(nm, i) if (!is.na(nm[i]) && nzchar(nm[i])) nm[i] else paste0("(entry ", i, ")")

# the attributes of the list (migrated, read_problems) kept over an edit
.runs_keep_attr <- function(r, old) {
  for (a in c("migrated", "read_problems")) attr(r, a) <- attr(old, a)
  r
}

# the list without attributes: what the parts and a comparison hold
.runs_bare <- function(r) {
  r <- r %||% list()
  attributes(r) <- NULL
  r
}

# a definition's report ids, in the report list's order ("all": every report)
.run_def_ids <- function(x, d) {
  ids <- x$outputs$output_id
  r <- d$reports
  if (identical(r, "all")) return(ids)
  ids[ids %in% as.character(r)]
}

# the `reports` argument: the word all, or ids the report list has
.run_reports_arg <- function(x, reports) {
  if (identical(reports, "all")) return("all")
  reports <- unique(as.character(reports))
  bad <- setdiff(reports, x$outputs$output_id)
  if (length(bad)) {
    stop("No report ", paste(sQuote(bad), collapse = ", "), " on the list.", call. = FALSE)
  }
  ids <- x$outputs$output_id
  ids[ids %in% reports]
}

# a name's problem under the name rule (tflspec's), "" when it keeps it
.run_name_problem <- function(name) {
  p <- tflspec::tfl_check_runs(list(list(name = name, reports = "all")))
  p <- p[p$field == "name", , drop = FALSE]
  if (nrow(p)) p$problem[[1L]] else ""
}

# What is wrong with the definitions, one row per (definition, field, problem):
# tflspec's checks (the name rule, the same name ignoring case, an empty
# list, an option) and the reports the list does not have
.runs_problems <- function(x) {
  r <- x$runs %||% list()
  out <- tflspec::tfl_check_runs(r)
  nm <- .run_names(x)
  ids <- x$outputs$output_id
  extra <- list()
  for (i in seq_along(r)) {
    v <- r[[i]]$reports
    if (is.character(v) && !identical(v, "all")) {
      for (id in setdiff(unique(v[!is.na(v)]), c(ids, "all", "All"))) {
        extra[[length(extra) + 1L]] <- data.frame(
          definition = .run_label(nm, i), field = "reports",
          problem = paste0("the study has no report ", id), level = "error",
          stringsAsFactors = FALSE)
      }
    }
  }
  if (length(extra)) out <- rbind(out, do.call(rbind, extra))
  rownames(out) <- NULL
  out
}

# A definition just set must keep the rules: its own problems stop the edit;
# the name rule only for a new name (an older name that breaks it passes and
# is a Review row)
.run_def_check <- function(y, name, new) {
  rows <- .runs_problems(y)
  rows <- rows[rows$definition == name, , drop = FALSE]
  if (!new) rows <- rows[rows$field != "name", , drop = FALSE]
  if (nrow(rows)) stop(paste(rows$problem, collapse = "; "), call. = FALSE)
  invisible(TRUE)
}

# The save rule: a name that is new or changed since the files on disk and
# breaks the rule (or repeats another ignoring case) stops the save; a name
# the disk has passes, with its Review row.  `root`: the study folder.
.runs_save_problems <- function(p, root) {
  rows <- .runs_problems(p)
  rows <- rows[rows$field == "name", , drop = FALSE]
  if (!nrow(rows)) return(rows)
  on_disk <- tryCatch({
    d <- tflspec::tfl_read_runs(root)
    d <- unclass(d)
    attributes(d) <- NULL
    vapply(d, function(e) if (is.character(e$name) && length(e$name) == 1L) e$name else NA_character_, "")
  }, error = function(e) character())
  rows[!rows$definition %in% on_disk, , drop = FALSE]
}

# ---- a report's id changed or gone ------------------------------------------

.runs_rename_id <- function(runs, from, to) {
  out <- lapply(runs %||% list(), function(d) {
    if (is.character(d$reports) && !identical(d$reports, "all")) {
      d$reports[d$reports == from] <- to
    }
    d
  })
  .runs_keep_attr(out, runs)
}

.runs_drop_id <- function(runs, id) {
  out <- lapply(runs %||% list(), function(d) {
    if (is.character(d$reports) && !identical(d$reports, "all")) {
      d$reports <- d$reports[d$reports != id]
    }
    d
  })
  .runs_keep_attr(out, runs)
}

# One report in or out of a definition (the TOC import's flag, the AI's
# TOC): the definition is made when it is not there yet
.run_def_member <- function(x, name, output_id, on = TRUE) {
  i <- match(name, .run_names(x))
  if (is.na(i)) {
    if (!on) return(x)
    return(set_run_definition(x, name, output_id))
  }
  have <- .run_def_ids(x, x$runs[[i]])
  ids <- if (on) union(have, output_id) else setdiff(have, output_id)
  if (identical(x$runs[[i]]$reports, "all") && on) return(x)
  if (!length(ids)) {
    # (a definition left with no report stays: a Review problem, not removed)
    r <- x$runs
    r[[i]]$reports <- character()
    x$runs <- .runs_keep_attr(r, x$runs)
    return(x)
  }
  set_run_definition(x, name, ids)
}

# The definitions as tflspec's list gives them, for the planner: no
# attributes of tflspec's, `migrated` (how many) and `read_problems` kept
.runs_from_tfl <- function(x) {
  mig <- attr(x, "migrated")
  rp <- attr(x, "read_problems")
  d <- unclass(x)
  attributes(d) <- NULL
  d <- lapply(d, function(e) e[!vapply(e, is.null, NA)])
  if (length(mig) == 1L && !is.na(mig) && mig > 0) attr(d, "migrated") <- as.integer(mig)
  if (is.data.frame(rp) && nrow(rp)) attr(d, "read_problems") <- rp
  d
}

# The run definitions of a study folder, as tflspec reads them (spec/runs.yml;
# without it, the older carriers').  A file that does not read as a whole
# (not YAML, not a `runs:` list) stops the reading of the definition files,
# as another spec file that does not read does: the study opens as last
# saved and cannot be saved until the file is fixed (its text is never
# replaced).
.runs_read <- function(root) {
  r <- tflspec::tfl_read_runs(root)
  rp <- attr(r, "read_problems")
  bad <- if (is.data.frame(rp) && nrow(rp)) rp$field %in% c("file", "runs") else FALSE
  if (any(bad)) {
    stop(basename(tflspec::tfl_runs_file(root)), " cannot be read: ",
         paste(rp$problem[bad], collapse = "; "), call. = FALSE)
  }
  .runs_from_tfl(r)
}

# ---- the state (JSON) -------------------------------------------------------

# a report list as an array always (one id stays an array; the word all a text)
.runs_to_state <- function(runs) {
  lapply(.runs_bare(runs), function(d) {
    r <- d$reports
    d$reports <- if (identical(r, "all")) "all" else as.list(as.character(r))
    d
  })
}

.runs_from_state <- function(st) {
  lapply(st, function(d) {
    r <- d$reports
    d$reports <- if (is.list(r)) as.character(unlist(r)) else as.character(r)
    d
  })
}

# The definitions of a state: its own; a state of before the run
# definitions (no `runs`) has them in the report list's `batches` column,
# moved here by tflspec (the same function the files use)
.runs_of_state <- function(ps, outputs) {
  if (!is.null(ps$runs)) return(.runs_from_state(ps$runs))
  if (!"batches" %in% names(outputs)) return(list())
  .runs_from_tfl(tflspec::tfl_runs_migrate("", outputs = outputs[c("output_id", "batches")]))
}

# The names of the run definitions each report is in, joined with " | " ("" for
# none): the report list's column
.report_run_names <- function(x, ids) {
  r <- x$runs %||% list()
  nm <- .run_names(x)
  vapply(ids, function(id) {
    in_it <- vapply(r, function(d) id %in% .run_def_ids(x, d), NA)
    paste(nm[in_it & !is.na(nm)], collapse = " | ")
  }, "", USE.NAMES = FALSE)
}

# The Runs tab's ticks after "Tick the shown" (`mode` "add") or "Untick the
# shown" (`"remove"`) on the reports a filter shows
.batch_ticks <- function(now, shown, mode) {
  if (identical(mode, "add")) union(now, shown) else setdiff(now, shown)
}
