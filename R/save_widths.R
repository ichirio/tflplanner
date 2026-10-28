# ============================================================================
#  A report saved from the app says its column widths (10/26, the user): a
#  report new or changed in the save that has none gets the default ones
#  (tflspec::tfl_fill_widths(): the table built, rtfreporter's
#  default_col_widths(); a listing's from its variables), as numbers to
#  change.  A table whose columns changed (its column variable, its levels,
#  its total, the ARD's `by`) has its old widths taken away first: their
#  number no longer fits.  Only save_study(fill_widths = TRUE) does this
#  (the app's save), after the YAML is written and before the programs.
# ============================================================================

# What makes a table's printed columns: its column variables and their
# levels, its total column, the ARD's `by` and `overall`
.cols_signature <- function(o) {
  if (is.null(o) || inherits(o$content, "tfl_r")) return(NULL)
  items <- o$content %||% list()
  arg <- function(fn, name) {
    hit <- Filter(function(it) identical(as.character(it$fn), fn), items)
    if (length(hit)) hit[[1L]]$args[[name]] else NULL
  }
  cols <- unlist(arg("table_plan", "cols"))
  lv <- Filter(function(it) identical(as.character(it$fn), "plan_levels"), items)
  lv <- if (length(lv)) lv[[1L]]$args[intersect(names(lv[[1L]]$args), cols)] else NULL
  total <- Filter(function(it) identical(as.character(it$fn), "plan_total"), items)
  by <- lapply(o$ard$analyses %||% list(), function(a) list(a$by, a$overall))
  list(cols = cols, levels = lv, total = length(total) > 0L, by = by)
}

# A table's widths taken away (plan_columns(widths = ); the item gone when
# it said nothing else)
.drop_widths <- function(o) {
  if (inherits(o$content, "tfl_r")) return(o)
  keep <- list()
  for (it in o$content %||% list()) {
    if (identical(as.character(it$fn), "plan_columns") && !is.null(it$args$widths)) {
      it$args$widths <- NULL
      if (!length(it$args)) next
    }
    keep[[length(keep) + 1L]] <- it
  }
  o$content <- keep
  o
}

.has_widths <- function(o) {
  !inherits(o$content, "tfl_r") && any(vapply(o$content %||% list(), function(it) {
    identical(as.character(it$fn), "plan_columns") && !is.null(it$args$widths)
  }, NA))
}

# The reports' files before the save: id -> its report as read (NULL when
# none)
.widths_before <- function(root) {
  tryCatch(suppressMessages(tflspec::tfl_read_outputs(root)), error = function(e) list())
}

# (the reports whose widths could not be made, by their text: not tried
# again until the report changes)
.widths_failed <- new.env()

# After the YAML is written: the reports written in this save (`files`)
# that need widths given them; the planner with the reports as now written
.save_fill_widths <- function(p, root, files, before) {
  rels <- file.path(study_layout()[["spec"]], "outputs")
  written <- files$file[files$status %in% "written"]
  ids <- names(p$spec$where)[normalizePath(file.path(root, p$spec$where), "/", mustWork = FALSE) %in%
                               normalizePath(written, "/", mustWork = FALSE)]
  ids <- ids[startsWith(unname(p$spec$where[ids]), rels)]
  if (!length(ids)) return(p)
  path_of <- function(id) file.path(root, p$spec$where[[id]])
  # the tables whose columns changed: their old widths gone
  for (id in ids) {
    o <- p$spec$outputs[[id]]
    old <- before[[id]]
    if (!identical(o$kind, "table") || !.has_widths(o) || is.null(old)) next
    if (identical(.cols_signature(o), .cols_signature(old))) next
    o <- .drop_widths(o)
    tflspec::tfl_write_output(o, path_of(id))
  }
  key <- function(id) paste(root, id, unname(tools::md5sum(path_of(id))))
  todo <- ids[!vapply(ids, function(id) exists(key(id), envir = .widths_failed, inherits = FALSE), NA)]
  if (!length(todo)) return(p)
  set <- tryCatch(suppressMessages(tflspec::tfl_fill_widths(root, output_id = todo, assumption = FALSE)),
                  error = function(e) {
                    message("Default column widths not written: ", conditionMessage(e))
                    character()
                  })
  # (a table that could not be built: remembered by its text)
  for (id in setdiff(todo, set)) {
    o <- tryCatch(tflspec::tfl_read_output(path_of(id)), error = function(e) NULL)
    if (inherits(o, "tfl_output") && identical(o$kind, "table") && !.has_widths(o)) {
      assign(key(id), TRUE, envir = .widths_failed)
    }
  }
  # the planner in step with the files: each report changed here read back
  for (id in ids) {
    o <- tryCatch(tflspec::tfl_read_output(path_of(id)), error = function(e) NULL)
    if (!inherits(o, "tfl_output") || identical(o, p$spec$outputs[[id]])) next
    p <- .spec_set_output(p, id, o, content_only = TRUE)
  }
  p
}
