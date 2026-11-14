# The preview engine of the template screen (part 4): a report's YAML text and
# the study's folder in, the table as HTML out.  Pure R, no screen here.
#
# Reused, not rewritten: tflspec's reader and runners (tfl_read_output(),
# tfl_read_study_spec(), tfl_resolve_output(), tfl_build_ard(),
# tfl_content_run(), tfl_ard_code(), tfl_report_code(), tfl_report()),
# rtfreporter::plan_apply() / generate_rtfreport(), and tflplanner's
# preview_html() (the table as the builder's preview draws it).  The order of
# steps is that of tflspec::tfl_run_output(), stopped before the RTF for
# stage = "table".  It runs in the session, not in a child R: see the
# measurements in the commit message.

.preview_cache_env <- new.env(parent = emptyenv())

# The cache (an environment, kept for the session): key -> a result
.preview_cache <- function() .preview_cache_env

# The files whose change changes the table: the study's data files as
# spec/data.yml names them, and its spec files
.preview_files <- function(study_root) {
  spec <- file.path(study_root, "spec")
  files <- c(file.path(spec, c("data.yml", "defaults.yml", "toc.yml")),
             list.files(file.path(spec, "patterns"), full.names = TRUE))
  d <- file.path(spec, "data.yml")
  if (file.exists(d)) {
    ds <- tryCatch(yaml::read_yaml(d)$datasets, error = function(e) NULL)
    for (x in ds) {
      p <- as.character(x$path %||% "")
      if (!nzchar(p)) next
      if (!grepl("^([A-Za-z]:|/)", p)) p <- file.path(study_root, p)
      files <- c(files, p)
    }
  }
  unique(files)
}

# The cache key: a hash of the YAML text, the files' mtimes and the stage
.preview_key <- function(text, study_root, stage) {
  f <- .preview_files(study_root)
  mt <- vapply(f, function(p) {
    m <- file.mtime(p)
    if (is.na(m)) "none" else format(as.numeric(m), digits = 17)
  }, "")
  x <- paste(c(stage, text, paste(f, mt)), collapse = "\n")
  tf <- tempfile("pvkey")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(x)), tf)
  unname(tools::md5sum(tf))
}

# The study's spec, read once per state of its files: reading it checks every
# report in spec/outputs, which is most of a preview's time, and a preview
# changes none of them (the report being previewed is read from its text)
.preview_study_spec <- function(study_root, cache = .preview_cache()) {
  # (the working directory is the study's: the reader reads from there)
  f <- list.files(file.path(study_root, "spec"), recursive = TRUE, full.names = TRUE)
  mt <- vapply(f, function(p) format(as.numeric(file.mtime(p)), digits = 17), "")
  key <- paste0("study:", study_root, ":", paste(f, mt, collapse = "|"))
  hit <- cache[[key]]
  if (!is.null(hit)) return(hit)
  st <- tflspec::tfl_read_study_spec(".")
  for (k in grep("^study:", ls(cache), value = TRUE)) rm(list = k, envir = cache)
  cache[[key]] <- st
  st
}

# The study's rounding, as the programs' setup sets it (what tfl_run_output()
# does before it runs)
.preview_rounding <- function(st) {
  f <- utils::getFromNamespace(".study_rounding_option", "tflspec")
  f(st)
}

# The preview of a report: list(ok, html, code, rtf, messages, seconds, key).
# `report_yaml_text`: the report's YAML as the screen would save it;
# `study_root`: the study's folder; `stage`: "table" (the table as HTML) or
# "full" (the RTF too: `rtf` is the path of the file written, in the
# session's temporary folder); `cache`: .preview_cache().  Never stops: an
# error is ok = FALSE with the message in `messages`.
.preview_report <- function(report_yaml_text, study_root, stage = c("table", "full"),
                            cache = .preview_cache()) {
  t0 <- Sys.time()
  secs <- function() round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  stage <- tryCatch(match.arg(stage), error = function(e) "table")
  text <- paste(as.character(report_yaml_text), collapse = "\n")
  key <- tryCatch(.preview_key(text, study_root, stage), error = function(e) NA_character_)
  if (!is.na(key) && !is.null(hit <- cache[[key]])) {
    hit$seconds <- secs()
    hit$cached <- TRUE
    return(hit)
  }
  msgs <- character()
  out <- tryCatch(
    withCallingHandlers(.preview_run(text, study_root, stage, cache),
                        message = function(m) {
                          msgs <<- c(msgs, trimws(conditionMessage(m)))
                          invokeRestart("muffleMessage")
                        },
                        warning = function(w) {
                          msgs <<- c(msgs, paste("Warning:", conditionMessage(w)))
                          invokeRestart("muffleWarning")
                        }),
    error = function(e) e)
  if (inherits(out, "error")) {
    return(list(ok = FALSE, html = NULL, code = NULL, rtf = NULL,
                messages = c(msgs, conditionMessage(out)), seconds = secs(),
                key = key, cached = FALSE))
  }
  res <- list(ok = TRUE, html = out$html, code = out$code, rtf = out$rtf,
              messages = msgs, seconds = secs(), key = key, cached = FALSE)
  if (!is.na(key)) cache[[key]] <- res
  res
}

# The run itself: errors are the caller's to catch
.preview_run <- function(text, study_root, stage, cache) {
  if (!is.character(study_root) || length(study_root) != 1L || !dir.exists(study_root)) {
    stop("The study folder is not there.", call. = FALSE)
  }
  tf <- tempfile("pvreport", fileext = ".yml")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), tf)
  o <- tflspec::tfl_read_output(tf)
  if (is.list(o) && !inherits(o, "tfl_output")) o <- o[[1L]]
  owd <- setwd(study_root)
  on.exit(setwd(owd), add = TRUE)
  st <- .preview_study_spec(study_root, cache)
  old <- .preview_rounding(st)
  on.exit(options(old), add = TRUE)
  ro <- tflspec::tfl_resolve_output(o, st)
  if (!is.null(o$contents) || !identical(ro$kind %||% o$kind, "table") ||
      identical(ro$data$source, "dataset")) {
    stop("The preview makes a table from an ARD; this report is not one.", call. = FALSE)
  }
  ard <- tflspec::tfl_build_ard(st, dir = ".", output_id = ro$id, save = FALSE)
  if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% ro$id, , drop = FALSE]
  plan <- tflspec::tfl_content_run(ro, ard)
  pages <- rtfreporter::plan_apply(plan)
  if (is.data.frame(pages)) {
    pages <- list(structure(list(data = pages, col_header = list(names(pages)),
                                 blank_rows = integer()), class = "rtftable"))
  }
  if (inherits(pages, "rtftable")) pages <- list(pages)
  pages <- lapply(pages, function(pg) {
    if (is.null(pg$col_header)) pg$col_header <- list(names(pg$data))
    pg
  })
  html <- as.character(preview_html(pages))
  code <- paste(c(
    tflspec::tfl_ard_code(st, output_id = ro$id, save = FALSE, dir = "."),
    "",
    tflspec::tfl_report_code(o, study = st)), collapse = "\n")
  rtf <- NULL
  if (identical(stage, "full")) {
    rtf <- tempfile("pvreport", fileext = ".rtf")
    rep <- tflspec::tfl_report(o, content = plan, study = st)
    rtfreporter::generate_rtfreport(rep, rtf, overwrite = TRUE)
  }
  list(html = html, code = code, rtf = rtf)
}
