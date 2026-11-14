# The code of a study's programs as its setup has it (#268): its folders
# through the variables programs/study_setup.R defines (path_adam,
# path_ard, ...), and no `pkg::` for the packages it attaches.  tflspec
# writes its part so with its options tflspec.paths and tflspec.attached;
# tflplanner's own lines read the same options.  tflspec.study_setup: the
# study's rounding is in programs/study_setup.R's part 2, so the ARD and
# report setups leave it out (tflspec#791).
#
# They are set only while a program is written or compared with its file
# (.with_study_code()): a study's programs run its setup first.  The app's
# previews run code in the app's own session, where no setup ran, and keep
# it as it is.

.code_context <- new.env()

# The study whose programs are written from now on (open_study(),
# save_study(), the app's study): its setup's packages.  NULL: no study
# (closed, or none open yet), the programs as they are (#680)
.use_study <- function(root) {
  .code_context$opts <- if (is.null(root)) NULL else list(
    tflspec.paths = .study_path_vars(),
    tflspec.attached = .study_setup_packages(root),
    tflspec.study_setup = TRUE)
  .code_context$root <- if (is.null(root)) NULL else
    normalizePath(root, "/", mustWork = FALSE)
  invisible(root)
}

# `expr` with the context of the study at `root`.  The open study's is
# set for good (its setup may have changed); another study's (its status
# looked at while one is open) only while `expr` runs: the open study's
# context and its program cache are as they were afterwards (#680)
.with_study_context <- function(root, expr) {
  if (identical(.code_context$root, normalizePath(root, "/", mustWork = FALSE))) {
    .use_study(root)
    return(expr)
  }
  old <- list(opts = .code_context$opts, root = .code_context$root)
  cache <- as.list(.program_cache, all.names = TRUE)
  on.exit({
    .code_context$opts <- old$opts
    .code_context$root <- old$root
    rm(list = ls(.program_cache, all.names = TRUE), envir = .program_cache)
    list2env(cache, envir = .program_cache)
  }, add = TRUE)
  .use_study(root)
  expr
}

# the study's folders as programs/study_setup.R names them
.study_path_vars <- function() {
  lay <- study_layout()
  stats::setNames(unname(lay), paste0("path_", names(lay)))
}

# The packages programs/study_setup.R attaches: its library() and
# require() calls, wherever they are (in its part 1, its part 3, inside
# suppressPackageStartupMessages()).  A study whose file is without
# dplyr (made before it was in the company's setup code) keeps `dplyr::`.
.study_setup_packages <- function(root) {
  f <- file.path(root, .study_setup_path())
  # a study not saved yet: the company's setup code its file starts with
  ex <- tryCatch(if (file.exists(f)) parse(f, keep.source = FALSE, encoding = "UTF-8") else
    parse(text = gsub("{STUDY_ID}", "STUDYID", setup_code(), fixed = TRUE),
          keep.source = FALSE), error = function(e) NULL)
  out <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1L]]
    nm <- if (is.name(fn)) as.character(fn) else
      if (is.call(fn) && identical(fn[[1L]], as.name("::"))) as.character(fn[[3L]]) else ""
    if (nm %in% c("library", "require")) {
      m <- tryCatch(match.call(base::library, e), error = function(err) NULL)
      pk <- m$package
      if (is.name(pk) || (is.character(pk) && length(pk) == 1L)) {
        out <<- c(out, as.character(pk))
      }
    }
    for (a in as.list(e)[-1L]) walk(a)
  }
  for (e in ex) walk(e)
  unique(out)
}

# `expr` as the study's programs have it: with the study's options (no
# study: as it is)
.with_study_code <- function(expr) {
  opts <- .code_context$opts
  if (is.null(opts)) return(expr)
  old <- options(opts)
  on.exit(options(old), add = TRUE)
  expr
}

# A path as tflplanner's own lines write it: through the variable of the
# folder that holds it (while a program is written), else as it is
.path_lit <- function(path) {
  q <- encodeString(path, quote = "\"")
  pv <- getOption("tflspec.paths")
  if (!length(pv)) return(q)
  dirs <- sub("/+$", "", as.character(pv))
  hit <- which(startsWith(path, paste0(dirs, "/")) | path == dirs)
  if (!length(hit)) return(q)
  k <- hit[which.max(nchar(dirs[hit]))]
  rest <- substring(path, nchar(dirs[k]) + 2L)
  if (!nzchar(rest)) return(names(pv)[k])
  sprintf("file.path(%s, %s)", names(pv)[k], encodeString(rest, quote = "\""))
}

# tflplanner's own lines without `pkg::` for the packages the study's setup
# attaches (while a program is written): the `pkg::` of each call in the
# parsed code (a string or a comment keeps its text).  Code that does not
# parse is left as it is.
.drop_attached <- function(code) {
  pk <- getOption("tflspec.attached")
  if (!length(pk) || !length(code)) return(code)
  txt <- paste(code, collapse = "\n")
  pd <- tryCatch(utils::getParseData(parse(text = txt, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(code)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  at <- which(pd$token == "SYMBOL_PACKAGE" & pd$text %in% pk)
  at <- at[at < nrow(pd) & pd$token[at + 1L] == "NS_GET"]
  if (!length(at)) return(code)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  n <- lengths(strsplit(paste0(code, "\n"), "\n", fixed = TRUE))
  length(lines) <- sum(n)
  lines[is.na(lines)] <- ""
  for (i in rev(at[order(pd$line1[at], pd$col1[at])])) {
    s <- lines[pd$line1[i]]
    lines[pd$line1[i]] <- paste0(substr(s, 1L, pd$col1[i] - 1L),
                                 substr(s, pd$col2[i + 1L] + 1L, nchar(s)))
  }
  # the lines as they came (an element may hold several)
  ends <- cumsum(n)
  vapply(seq_along(code), function(i)
    paste(lines[(ends[i] - n[i] + 1L):ends[i]], collapse = "\n"), "")
}

# Where the app runs a program's lines itself (a preview): the functions
# the programs call (programs/study_helpers.R's, tflspec's code), over
# `parent`
.helpers_env <- function(parent = globalenv()) {
  e <- new.env(parent = parent)
  eval(parse(text = tflspec::tfl_helpers_code(), keep.source = FALSE), envir = e)
  e
}

# code without the blank lines it starts with
.drop_leading_blank <- function(code) {
  code[cumsum(nzchar(code)) > 0L]
}
