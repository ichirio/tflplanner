# The kept store of a study, register_study(), unregister_study() and
# list_studies().
# Split from R/study.R by section; no code changed.

# What tflplanner keeps about a study (its home's studies/<id>/: the saved
# state, its history, an unsaved draft) goes into the study folder when the
# study is unregistered, and comes back when the folder is registered again:
# taking a study off the list loses nothing.
.kept_dir <- function(path) file.path(path, ".tflplanner")

.keep_store <- function(id, path, home = tflplanner_home()) {
  from <- .store_dir(id, home)
  if (!dir.exists(from) || !dir.exists(path)) return(invisible(FALSE))
  to <- .kept_dir(path)
  unlink(to, recursive = TRUE)
  dir.create(to, showWarnings = FALSE)
  # the ARD fetched for input assistance (ard/) is made again when needed
  for (f in setdiff(list.files(from), "ard")) {
    file.copy(file.path(from, f), to, recursive = TRUE, copy.date = TRUE)
  }
  invisible(TRUE)
}

# The kept store back in the home: its saved state, history and draft.
# TRUE: the state is back.
.restore_store <- function(id, path, home = tflplanner_home()) {
  from <- .kept_dir(path)
  sf <- file.path(from, "state.json")
  if (!file.exists(sf)) return(FALSE)
  st <- tryCatch(jsonlite::fromJSON(sf, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(st) || !identical(st$meta$study_id, id)) return(FALSE)
  to <- .store_dir(id, home)
  dir.create(file.path(to, "history"), recursive = TRUE, showWarnings = FALSE)
  for (f in list.files(file.path(from, "history"), full.names = TRUE)) {
    file.copy(f, file.path(to, "history"), copy.date = TRUE)
  }
  if (file.exists(file.path(from, "draft.json"))) {
    file.copy(file.path(from, "draft.json"), to, copy.date = TRUE)
  }
  # the kept state comes back as it was; register_study() then compares
  # the definition files with its record (#274): an edit made while the
  # study was off the list is read and taken in, the kept state going to
  # the history
  file.copy(sf, to, overwrite = TRUE, copy.date = TRUE)
  unlink(from, recursive = TRUE)
  TRUE
}

#' @rdname create_study
#' @export
register_study <- function(path, home = tflplanner_home()) {
  # never a study folder the home is in (#359)
  if (dir.exists(path) && .under(home, path)) {
    stop(sprintf("The home %s is inside the study folder %s.", home,
                 normalizePath(path, "/", mustWork = FALSE)), call. = FALSE)
  }
  meta <- .read_meta(path)
  id <- .check_study_id(meta$study_id)
  st <- .read_state(id, home)
  if (!is.null(st) &&
      !identical(normalizePath(st$path, "/", FALSE),
                 normalizePath(path, "/"))) {
    stop("Study '", id, "' is already registered at ", st$path, ".",
         call. = FALSE)
  }
  # a folder with no RStudio project gets one, as create_study() makes it
  if (!length(list.files(path, "\\.Rproj$", ignore.case = TRUE))) {
    writeLines(.rproj, file.path(path, paste0(id, ".Rproj")))
  }
  # unregistered before: what tflplanner kept comes back as it was, the
  # definition files compared with its record (an edit made while the
  # study was off the list is taken in, #274)
  if (is.null(st)) {
    .restore_store(id, path, home)
    st <- .read_state(id, home)
    if (!is.null(st)) {
      st$path <- normalizePath(path, "/")
      s <- .adopt_spec(st, home, full = is.null(st$files))
      if (!identical(s$spec$status, "invalid")) .write_state(s, home)
      .spec_note(s)
      .set_config("last_study", id, home)
      return(s)
    }
  }
  # a folder new to tflplanner: its definition files are the study
  s <- .new_study(path, meta[.study_fields], new_planner())
  r <- .read_spec(s, path)
  problems <- attr(r, "problems")
  attr(r, "problems") <- NULL
  if (.has_errors(problems)) stop(.spec_invalid_condition(problems))
  # a figure design for no report of the study is not taken in
  fd <- names(r$planner$fig_designs %||% list())
  for (f in setdiff(fd, r$planner$outputs$output_id)) {
    r$planner <- set_fig_design(r$planner, f, NULL)
  }
  .write_state(r, home)
  r$planner <- .spec_rebase(r$planner, r$meta)
  r$spec <- list(status = "same", files = .spec_diff(NULL, list()))
  .set_config("last_study", id, home)
  r
}

#' @rdname create_study
#' @export
unregister_study <- function(study_id, home = tflplanner_home()) {
  if (length(study_id) != 1L || is.na(study_id) || !nzchar(study_id)) {
    stop("unregister_study() takes one study ID.", call. = FALSE)
  }
  study_id <- .check_study_id(study_id)
  st <- .read_state(study_id, home)
  if (is.null(st)) {
    stop("Study '", study_id, "' is not registered.", call. = FALSE)
  }
  # the folder is not touched but for what tflplanner kept, put in it
  # (.tflplanner/): register_study() takes it back
  .keep_store(study_id, st$path %||% "", home)
  unlink(.store_dir(study_id, home), recursive = TRUE)
  if (identical(tflplanner_config(home)$last_study, study_id)) {
    .set_config("last_study", NULL, home)
  }
  invisible(study_id)
}

#' @rdname create_study
#' @export
list_studies <- function(home = tflplanner_home()) {
  .move_old_stores(home)
  ids <- basename(list.dirs(file.path(home, "state"), recursive = FALSE))
  rows <- lapply(ids, function(id) {
    st <- tryCatch(.read_state(id, home), error = function(e) NULL)
    if (is.null(st)) return(NULL)
    v <- function(k) as.character(st$meta[[k]] %||% NA_character_)
    n <- function(x) length(x$output_id %||% list())
    data.frame(study_id = id, title = v("title"),
               short_title = v("short_title"), compound = v("compound"),
               phase = v("phase"), saved = as.character(st$saved %||% NA),
               path = st$path, folder = dir.exists(st$path),
               description = v("description"),
               reports = n(st$planner$outputs),
               analyses = n(st$planner$ard$analyses),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    study_id = character(), title = character(),
    short_title = character(), compound = character(),
    phase = character(), saved = character(), path = character(),
    folder = logical(), description = character(), reports = integer(),
    analyses = integer())), rows))
  rownames(out) <- NULL
  out
}
