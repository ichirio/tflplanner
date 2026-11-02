# The state of a report program: checksums, edited copies, caches.
# Split from R/study.R by section; no code changed.

# ------------------------------------------------------------ the state

# A generated program carries a checksum of its own body in its banner.  A
# program whose body still matches it has not been touched since tflplanner
# wrote it; one that does not match was edited by hand.  Either is written
# again from the definition (a program is never edited: design 13-00), the
# edited one first copied to programs/.edited/.
.gen_line <- "^#  (Generated|Checksum)  *:"

# (md5sum() reads a file: one file of the session's, written over each
# time -- a new file made and deleted for each program made a save slow)
.hash_file <- new.env()
.body_hash <- function(lines) {
  f <- .hash_file$path
  if (is.null(f)) f <- .hash_file$path <- tempfile("tflplanner-hash-")
  writeLines(enc2utf8(lines[!grepl(.gen_line, lines)]), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

# A program edited by hand, copied to programs/.edited/ before it is
# written again: named after its place under programs/ and the time
# (tfl_T-14-1-1_20261006-153000.R)
.edited_dir <- file.path("programs", ".edited")
.back_up_edited <- function(f, root) {
  dir <- file.path(root, .edited_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rel <- if (startsWith(f, root)) substring(f, nchar(root) + 2L) else basename(f)
  rel <- sub("^programs/", "", rel)
  dest <- file.path(dir, paste0(sub("[.][Rr]$", "", gsub("/", "_", rel, fixed = TRUE)),
                                "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".R"))
  file.copy(f, dest, overwrite = TRUE)
  invisible(dest)
}

.write_program <- function(code, f) {
  at <- grep("^#  Generated  :", code)[1L]
  code <- append(code, paste("#  Checksum   :", .body_hash(code)), at)
  writeLines(enc2utf8(code), f, useBytes = TRUE)
}

# missing   no file yet
# current   untouched, and what tflplanner would write now
# todo      untouched, still the TODO data part
# generated untouched, but the definition has moved on: saving rewrites it
# edited    changed by hand: saving writes it again (the edited one kept
#           in programs/.edited/)
# A report's program as the definition writes it, kept while the
# definition is the same: the runs table asks for every report's again at
# each refresh (study_status()), and the definition rarely changed between
.program_cache <- new.env()
.program_code_last <- function(p, id) {
  if (!identical(.program_cache$date, Sys.Date()) ||
      !identical(.program_cache$opts, .code_context$opts)) {
    .program_cache$date <- Sys.Date()
    .program_cache$opts <- .code_context$opts
    .program_cache$code <- list()
    .program_cache$key <- list()
  }
  # (kept report by report: an edit of one report -- a card of the table
  # builder -- writes the others' programs from the cache, #417)
  key <- .program_key(p, id)
  if (is.null(.program_cache$code[[id]]) || !identical(.program_cache$key[[id]], key)) {
    .program_cache$code[[id]] <- program_code(p, id)
    .program_cache$key[[id]] <- key
  }
  .program_cache$code[[id]]
}

# What a report's program is written from: the planner with the other
# reports' own rows of the sheets, listings, figures and spec left out
# (the study's rows, the patterns', the ARD definition and the report
# list kept whole), and whether its spec is in step with the sheets
.program_key <- function(p, id) {
  # (the planner cut by report once for all its reports)
  if (!identical(.program_cache$cut_of, p)) {
    cut <- function(d) {
      if (!is.data.frame(d) || !"output_id" %in% names(d)) return(list(all = d))
      o <- d$output_id
      shared <- is.na(o) | startsWith(o, "@")
      c(list(shared = d[shared, , drop = FALSE]),
        split(d[!shared, , drop = FALSE], o[!shared]))
    }
    q <- p
    q$sheets <- q$lf <- q$fig_designs <- q$spec <- NULL
    .program_cache$cut <- list(sheets = lapply(p$sheets, cut), lf = lapply(p$lf, cut), rest = q,
                               in_step = !is.null(p$spec) && !is.null(p$spec$base) &&
                                 .same_planner(p$spec$base, p))
    .program_cache$cut_of <- p
  }
  ct <- .program_cache$cut
  mine <- function(parts) lapply(parts, function(x) list(x$all, x$shared, x[[id]]))
  list(ct$rest, sheets = mine(ct$sheets), lf = mine(ct$lf),
       fig = p$fig_designs[intersect(id, names(p$fig_designs))],
       spec_output = p$spec$outputs[[id]], spec_study = p$spec$study, in_step = ct$in_step)
}

# its checksum, kept with it
.program_hash_last <- function(p, id) {
  code <- .program_code_last(p, id)
  if (is.null(.program_cache$hash)) .program_cache$hash <- list()
  if (is.null(.program_cache$hash[[id]]) ||
      !identical(.program_cache$hash[[id]]$code, code)) {
    .program_cache$hash[[id]] <- list(code = code, hash = .body_hash(code))
  }
  .program_cache$hash[[id]]$hash
}

.program_state <- function(p, id, f) {
  if (!file.exists(f)) return("missing")
  # (the file as last read, the definition's program the same: its state
  # again, without reading it)
  md5 <- unname(tools::md5sum(f))
  last <- .program_hash_last(p, id)
  hit <- .state_cache[[f]]
  if (!is.null(hit) && identical(hit$md5, md5) && identical(hit$last, last)) return(hit$state)
  state <- .program_state_read(f, last)
  .state_cache[[f]] <- list(md5 = md5, last = last, state = state)
  state
}
.state_cache <- new.env()
.program_state_read <- function(f, last) {
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  h <- .body_hash(have)
  if (!length(chk) || !identical(chk[1L], h)) return("edited")
  if (identical(h, last)) {
    if (any(grepl("tflplanner: the data part of", have, fixed = TRUE))) {
      return("todo")
    }
    return("current")
  }
  "generated"
}

.mtime <- function(f) {
  if (length(f) && file.exists(f)) file.mtime(f) else as.POSIXct(NA)
}
