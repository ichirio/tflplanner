# Data paths and files of a study: study_files(), read_data_head().
# Split from R/study.R by section; no code changed.

# A data path written whole: a drive (C:/adam), a root (/data, \\server),
# or the home (~/adam)
.is_abs_path <- function(p) grepl("^(([A-Za-z]:)?[/\\\\]|~)", p)

# A data catalog's path as a file on this computer: a path written whole
# as it is (ADaM kept outside the study folder), else under the study
# folder; "/" between the folders either way.  No path: NA.
.data_file <- function(study_path, path) {
  path <- as.character(path)
  out <- ifelse(.is_abs_path(path), path.expand(path), file.path(study_path, path))
  out[is.na(path) | !nzchar(path)] <- NA_character_
  gsub("\\", "/", out, fixed = TRUE)
}

# A data catalog's path whose file is not in the study folder: written whole
# and not under the folder, or going up out of it (../adam/adsl.rds)
.data_outside <- function(study_path, path) {
  path <- as.character(path)
  have <- !is.na(path) & nzchar(path)
  if (!length(path)) return(logical())
  np <- function(p) {
    p <- gsub("\\", "/", normalizePath(p, "/", mustWork = FALSE), fixed = TRUE)
    if (identical(.Platform$OS.type, "windows")) tolower(p) else p
  }
  root <- paste0(sub("/+$", "", np(study_path)), "/")
  up <- grepl("^\\.\\.([/\\\\]|$)", path)
  abs <- .is_abs_path(path)
  out <- have & up
  out[have & abs] <- !startsWith(np(.data_file(study_path, path[have & abs])), root)
  out
}

#' Files in a study folder
#'
#' @param study An `rtfstudy`.
#' @param folder A folder of [study_layout()] (`"adam"`, `"tfl"`, ...), or
#'   `"data"` for all three data folders.
#' @return A data frame: `folder`, `file`, `size_kb`, `modified`, `path`.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: the sample study and its data)
#' s <- create_sample_study(run = FALSE)
#' head(study_files(s)[, c("folder", "file", "size_kb")])
#' }
#' options(old)
#' @export
study_files <- function(study, folder = "data") {
  lay <- study_layout()
  dirs <- if (identical(folder, "data")) lay[c("adam", "sdtm", "other")] else
    lay[folder]
  rows <- lapply(dirs, function(d) {
    f <- list.files(file.path(study$path, d), full.names = TRUE,
                    recursive = TRUE)
    if (!length(f)) return(NULL)
    info <- file.info(f)
    data.frame(folder = d, file = substring(f, nchar(file.path(study$path,
                                                               d)) + 2L),
               size_kb = round(info$size / 1024, 1),
               modified = format(info$mtime, "%Y-%m-%d %H:%M"),
               path = f, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    folder = character(), file = character(), size_kb = numeric(),
    modified = character(), path = character())), rows))
  rownames(out) <- NULL
  out
}

#' The first rows of a data file
#'
#' Reads `.rds`, `.rda` / `.RData` (one dataset a file), `.csv`, `.xpt`,
#' `.sas7bdat` (\pkg{haven}) and `.parquet`
#' (\pkg{arrow}).
#'
#' @param path A file.
#' @param n Rows to return.
#' @return A data frame, with the full dimensions in attribute `dim_full`.
#' @noRd
read_data_head <- function(path, n = 50L) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("read_data_head(): `path` is one file.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  need <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Reading .", ext, " files needs the ", pkg, " package.",
           call. = FALSE)
    }
  }
  d <- switch(ext,
    rds = readRDS(path),
    rda = , rdata = local({
      e <- new.env()
      load(path, envir = e)
      e[[ls(e)[1L]]]
    }),
    csv = utils::read.csv(path, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8-BOM"),
    xpt = { need("haven"); haven::read_xpt(path) },
    sas7bdat = { need("haven"); haven::read_sas(path) },
    parquet = { need("arrow"); arrow::read_parquet(path) },
    stop("No preview for .", ext, " files.", call. = FALSE))
  if (!is.data.frame(d)) {
    d <- data.frame(class = paste(class(d), collapse = "/"),
                    str = paste(utils::capture.output(utils::str(d,
                      max.level = 1L)), collapse = "\n"))
  }
  full <- dim(d)
  # taking rows drops the columns' labels: keep them (the forms show them)
  lab <- lapply(d, attr, which = "label", exact = TRUE)
  d <- as.data.frame(utils::head(d, n))
  for (nm in names(lab)) if (!is.null(lab[[nm]])) attr(d[[nm]], "label") <- lab[[nm]]
  attr(d, "dim_full") <- full
  d
}
