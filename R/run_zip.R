# The ZIPs of a run folder (#790, Q-C6): a run's results are downloaded as a
# ZIP of its folder runs/<date>_<time>_<what>/, not report by report.
#
#   output ZIP     output/, logs/, run.csv and batch.txt: downloaded at once
#   all-files ZIP  the whole folder: downloaded at once at or under the
#                  limit (config.yml's zip_download_limit_mb, 500 MB);
#                  over it, made in the background as runs/<folder>.zip
#
# Each ZIP's entries are under the folder's name (<folder>/run.csv ...), so
# it unpacks as the folder.

# what the output ZIP takes from a run folder (those that are there)
.batch_output_parts <- c("output", "logs", "run.csv", "batch.txt")

# The entries of a run folder's ZIP, relative to the runs folder: `what`
# "output" (.batch_output_parts) or "all" (the whole folder)
.batch_zip_entries <- function(dir, what = c("output", "all")) {
  what <- match.arg(what)
  name <- basename(dir)
  if (identical(what, "all")) return(name)
  have <- .batch_output_parts[file.exists(file.path(dir, .batch_output_parts))]
  file.path(name, have)
}

# The ZIP of a run folder written to `zipfile`
.batch_zip <- function(zipfile, dir, what = c("output", "all")) {
  what <- match.arg(what)
  entries <- .batch_zip_entries(dir, what)
  if (!length(entries)) stop("The run folder ", basename(dir), " has nothing to put in the ZIP.",
                             call. = FALSE)
  zip::zip(zipfile, entries, root = dirname(dir), recurse = TRUE)
  invisible(zipfile)
}

# A folder's size in MB (every file in it)
.dir_size_mb <- function(dir) {
  f <- list.files(dir, recursive = TRUE, all.files = TRUE, full.names = TRUE,
                  no.. = TRUE)
  sum(file.size(f), na.rm = TRUE) / 1024^2
}

# The largest all-files ZIP downloaded at once, in MB: config.yml's
# zip_download_limit_mb, else 500
.zip_limit_mb <- function(home = tflplanner_home()) {
  v <- tryCatch(tflplanner_config(home)$zip_download_limit_mb, error = function(e) NULL)
  v <- suppressWarnings(as.numeric(v))
  if (length(v) == 1L && !is.na(v) && v >= 0) v else 500
}

# The all-files ZIPs being made in the background, by run folder path (one
# per folder at a time, for every session of this R)
.zip_jobs <- new.env(parent = emptyenv())

# The all-files ZIP of a run folder, made in the background by its own
# Rscript as runs/<folder>.zip (written under another name first, then
# renamed: a half-made ZIP never has the name).  The running
# processx::process; an error when one is being made for the folder.
.batch_zip_bg <- function(dir) {
  key <- normalizePath(dir, "/", mustWork = FALSE)
  if (.batch_zip_busy(dir)) {
    stop("A ZIP of ", basename(dir), " is being made.", call. = FALSE)
  }
  out <- paste0(key, ".zip")
  part <- file.path(dirname(key), paste0(".", basename(key), ".zip.part"))
  code <- paste0(
    "args <- commandArgs(TRUE); unlink(args[3L]); ",
    "zip::zip(args[3L], basename(args[1L]), root = dirname(args[1L]), recurse = TRUE); ",
    "if (!file.rename(args[3L], args[2L])) stop('could not write ', args[2L])")
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"), c("-e", code, key, out, part),
    env = c("current", R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    stdout = NULL, stderr = "|", cleanup = TRUE)
  .zip_jobs[[key]] <- px
  px
}

# Is the all-files ZIP of a folder being made?
.batch_zip_busy <- function(dir) {
  px <- .zip_jobs[[normalizePath(dir, "/", mustWork = FALSE)]]
  !is.null(px) && px$is_alive()
}

# The links of each run folder's row: its output ZIP, its all-files ZIP (`t`:
# the app's words)
.batch_zip_links <- function(batch, t = identity) {
  if (!length(batch)) return(character())
  esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  one <- function(what, label, tip) sprintf(paste0(
    '<a href="#" class="text-nowrap" title="%s" ',
    'onmousedown="event.stopPropagation();" ',
    'onclick="event.stopPropagation(); ',
    "Shiny.setInputValue('batch_zip_row', {batch: '%s', what: '%s'}, ",
    '{priority: \'event\'}); return false;">%s</a>'),
    esc(tip), esc(batch), what, htmltools::htmlEscape(label))
  paste(
    one("output", t("Output"), t("output/, logs/, run.csv and batch.txt")),
    one("all", t("All files"),
        t("The whole run folder; over the limit (zip_download_limit_mb), made as runs/<folder>.zip")),
    sep = " \u00b7 ")
}

# The ZIP job of a folder ended: forgotten
.batch_zip_bg_done <- function(dir) {
  key <- normalizePath(dir, "/", mustWork = FALSE)
  if (exists(key, envir = .zip_jobs, inherits = FALSE)) rm(list = key, envir = .zip_jobs)
  invisible()
}
