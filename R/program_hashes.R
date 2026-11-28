# The checksums of a study's generated programs, kept between R sessions
# (#749).  A save tells a program file that is current from one the
# definition has moved on from by the checksum of the program the
# definition writes now (.program_state()); in a new session the programs
# were made again for that -- every report's and every ARD program, about
# 5 s of a first save, whatever the edit.  The checksum depends only on
# what the program is made from, so it is kept by the md5 of that: the
# report's program key (.program_key()) or the ARD definition, and what
# else the makers read (.code_hash_env()).  A program is made again only
# when its inputs are new; a file the record does not vouch for is
# compared as before.  The record is a cache: losing it costs time only.
#
# It lives in the tflplanner home (cache/programs/<md5 of the study
# folder>.rds), never in the study folder.

.code_hash_rec <- new.env(parent = emptyenv())
# (the record keeps this many input md5s per program: the last few states
# of its definition, an undo's among them)
.code_hash_keep <- 3L

# The packages that make the programs, as this session loaded them (their
# build time; reinstalled later, a new session's are new).  Taken when
# tflplanner is loaded.  A package without a build time (load_all()): none
# kept, nothing recorded.
.code_hash_builds <- function() {
  vapply(c("tflplanner", "tflspec", "rtfreporter"), function(p) {
    b <- tryCatch(utils::packageDescription(p, fields = "Built"), error = function(e) NA)
    if (length(b) != 1L || is.na(b)) "" else as.character(b)
  }, "")
}
.onLoad <- function(libname, pkgname) {
  .code_hash_rec$builds <- .code_hash_builds()
  invisible()
}

# What a program is made from besides its own inputs: the makers' builds,
# R and the locale, the packages installed, the study's code context and
# layout, the company's inputs, the template series' files and tflspec's
# and rtfreporter's options.  NULL: not recorded (no
# study, or a package without a build time).  The date is not in it: it
# is only in the banner's "Generated" line, which the checksum leaves out.
.code_hash_env <- function() {
  b <- .code_hash_rec$builds
  root <- .code_context$root
  if (is.null(root) || !length(b) || !all(nzchar(b))) return(NULL)
  dirs <- .program_series_dirs()
  dirs <- dirs[dir.exists(dirs)]
  series <- unlist(lapply(dirs, function(d) {
    f <- list.files(d, recursive = TRUE, full.names = TRUE)
    stats::setNames(unname(tools::md5sum(f)), f)
  }))
  o <- options()
  # (the makers ask which packages are installed and what they export: a
  # library folder's time changes when a package is installed, updated or
  # removed in it)
  libs <- .libPaths()
  list(builds = b, r = R.version.string, locale = Sys.getlocale(),
       libs = stats::setNames(as.character(file.mtime(libs)), libs), opts = .code_context$opts,
       layout = study_layout(), generator = .setup_generator(),
       standards = .report_standards(), ard = .ard_standards(),
       series = series, options = o[sort(grep("^(tflspec|rtfreporter)[.]", names(o), value = TRUE))])
}

# The md5 of an R object (its serialized bytes).  Written to a file of
# its own first: md5sum()'s `bytes` argument is R 4.5 and later only, and
# tflplanner runs on R 4.3.  (The hashes are compared only with hashes
# made here, so how the bytes reach md5sum() does not matter.)
.code_hash_md5 <- function(x) {
  f <- tempfile("hash-")
  on.exit(unlink(f), add = TRUE)
  writeBin(serialize(x, NULL, xdr = FALSE), f)
  unname(tools::md5sum(f))
}

# The md5 of one program's inputs: `kind` (report, ard), the output and
# its own inputs; NULL when nothing is recorded.  (The environment's md5
# kept while it is the same.)
.code_hash_key <- function(kind, id, inputs) {
  env <- .code_hash_env()
  if (is.null(env)) return(NULL)
  if (!identical(.code_hash_rec$env, env)) {
    .code_hash_rec$env <- env
    .code_hash_rec$env_md5 <- .code_hash_md5(env)
  }
  .code_hash_md5(list(kind, id, inputs, .code_hash_rec$env_md5))
}

# the record's file for the study whose programs are written now
.code_hash_file <- function() {
  root <- .code_context$root
  if (is.null(root)) return(NULL)
  file.path(tflplanner_home(), "cache", "programs",
            paste0(.code_hash_md5(root), ".rds"))
}

# the record of the study now (read once a session; another study's when
# the context changes)
.code_hash_entries <- function() {
  f <- .code_hash_file()
  if (is.null(f)) return(NULL)
  if (!identical(.code_hash_rec$file, f)) {
    # (the last study's record written first)
    .code_hash_flush()
    .code_hash_rec$file <- f
    .code_hash_rec$dirty <- FALSE
    e <- if (file.exists(f)) tryCatch(readRDS(f), error = function(err) NULL)
    .code_hash_rec$entries <- if (is.list(e)) e else list()
  }
  .code_hash_rec$entries
}

# The checksum recorded for a program's inputs (`key`, .code_hash_key()),
# or NULL
.code_hash_get <- function(prog, key) {
  if (is.null(key)) return(NULL)
  e <- .code_hash_entries()[[prog]]
  h <- e[[key]]
  if (is.character(h) && length(h) == 1L) h else NULL
}

# a program's checksum recorded for its inputs (written to disk by
# .code_hash_flush())
.code_hash_put <- function(prog, key, hash) {
  if (is.null(key) || is.null(.code_hash_entries())) return(invisible())
  e <- .code_hash_rec$entries[[prog]] %||% list()
  if (identical(e[[key]], hash) && identical(names(e)[1L], key)) return(invisible())
  e[[key]] <- NULL
  e <- c(stats::setNames(list(hash), key), e)
  .code_hash_rec$entries[[prog]] <- e[seq_len(min(length(e), .code_hash_keep))]
  .code_hash_rec$dirty <- TRUE
  invisible()
}

# the record written, when it changed (a file of its own, then renamed: a
# reader never sees half of it)
.code_hash_flush <- function() {
  f <- .code_hash_rec$file
  if (!isTRUE(.code_hash_rec$dirty) || is.null(f)) return(invisible())
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("programs-", tmpdir = dirname(f), fileext = ".rds")
  ok <- tryCatch({
    saveRDS(.code_hash_rec$entries, tmp)
    file.rename(tmp, f)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) unlink(tmp)
  .code_hash_rec$dirty <- FALSE
  invisible()
}

# The checksum a generated program file says it has, when its body still
# has it (written by tflplanner, untouched since); else NA
.program_file_hash <- function(f) {
  if (!file.exists(f)) return(NA_character_)
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "", grep("^#  Checksum   :", have, value = TRUE))
  if (!length(chk) || !identical(chk[1L], .body_hash(have))) NA_character_ else chk[1L]
}
