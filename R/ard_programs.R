# The ARD programs: one per output, in programs/ard/.
#
#   programs/ard/ard_setup.R      what every ARD program starts with: cards,
#                                 the statistics tflplanner computes, stat_fmt,
#                                 and the step that puts an output's rows into
#                                 the study ARD
#   programs/ard/<output_id>.R    one output's analyses: its data, its analysis
#                                 sets, one cards / cardx call per analysis;
#                                 it replaces that output's rows of
#                                 output/ard/ard.rds and records the build in
#                                 output/ard/ard_status.csv
#   programs/ard/autoexec_ard.R   the official run of them all: a batch
#                                 folder with each program's log (logrx),
#                                 the study ARD and the code (programs/batch.R)
#
# Run on its own -- from the app or in RStudio -- an ARD program is a
# preview: it updates the study's working ARD, so tables can be made from
# it, and keeps no log.  The report programs are in programs/tfl/.
#
# Like the report programs, a generated ARD program carries a checksum, and
# follows the definition on every save: one edited by hand is written again
# too, its edited copy first put in programs/.edited/.
#
# The text of ard_setup.R and of each output's program is tflspec's
# (tflspec::tfl_ard_program(), tflspec#910 G5-1c).

.ard_prog_name <- function(output_id) {
  paste0(gsub("[^A-Za-z0-9._-]", "_", output_id), ".R")
}

.ard_setup_file <- "ard_setup.R"
.ard_autoexec_file <- "autoexec_ard.R"

#' The ARD programs of a study
#'
#' `ard_setup_code()` is `programs/ard/ard_setup.R`, which every ARD program
#' sources: the study's setup (`programs/study_setup.R`, see
#' [study_setup_code()]), cards, the statistics tflplanner computes ([tflspec::tfl_ard_statistics()], the company standards' catalog),
#' the stat_fmt formats, and the option that makes `save_ard()` (the
#' study's `programs/study_helpers.R`, see [study_helpers_code()]) record
#' the study setup each ARD was built with.
#' `ard_program_code()` is one report's ARD program, `programs/ard/<output_id>.R`
#' (PROTOTYPE, tflspec#1032: the data and analysis lines of the report's
#' full program, [tflspec::tfl_onepass_program()]; `NULL` when the report
#' makes no ARD).  `ard_autoexec_code()` is
#' `programs/ard/autoexec_ard.R`, which runs them from the study folder --
#' all, or the ones named (`Rscript programs/ard/autoexec_ard.R T-14-1-1`)
#' -- each in its own R process with its log in `logs/ard/`.
#'
#' @param spec An [tflspec::tfl_ard_spec()] (or the path of one).
#' @param x A `tflplanner`.
#' @param output_id The output.
#' @param date The date stamped in the banner.
#' @return The code, one element per line.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it makes the sample study)
#' s <- create_sample_study(run = FALSE)
#' spec <- tflspec::tfl_ard_spec(s$planner$ard)
#' head(ard_setup_code(spec), 5)
#' }
#' options(old)
#' @export
ard_setup_code <- function(spec, date = Sys.Date()) {
  .with_study_code(.ard_setup_code(spec, date))
}

.ard_setup_code <- function(spec, date = Sys.Date()) {
  x <- if (is.character(spec)) .read_ard_spec(spec) else spec
  ctx <- .live_context_args()
  tflspec::tfl_ard_program(x, standards = .ard_standards(), attached = ctx$attached,
                           study_code = ctx$study_code, layout = study_layout(),
                           date = date, generator = .setup_generator())
}

# The company's ARD catalogs, as tflspec's ARD programs take them
.ard_standards <- function() {
  list(ard_statistics = .std_ard_statistics(), ard_methods = .std_ard_methods())
}

#' @rdname ard_setup_code
#' @export
ard_program_code <- function(x, output_id, date = Sys.Date()) {
  .with_study_code(.report_program(x, output_id, date = date, kind = "ard"))
}

#' @rdname ard_setup_code
#' @export
ard_autoexec_code <- function(spec, date = Sys.Date()) {
  lay <- study_layout()
  c(.autoexec_banner(file.path(lay[["programs_ard"]], .ard_autoexec_file),
                     "Official run of the ARD programs, one per output.",
                     "T-14-1-1         only these", date),
    .autoexec_body("ard"))
}

# missing / current / generated / edited, as a report program's
.ard_program_state <- function(code, f) {
  if (!file.exists(f)) return("missing")
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  # an earlier tflplanner wrote its autoexec without a checksum
  if (!length(chk) && any(grepl("^#  Generated  : tflplanner", have))) {
    return("generated")
  }
  h <- .body_hash(have)
  if (!length(chk) || !identical(chk[1L], h)) return("edited")
  if (identical(h, .body_hash(code))) "current" else "generated"
}

# write a generated program (one edited by hand copied to
# programs/.edited/ first); the file's row of what the save did
.put_program <- function(code, f, root) {
  # (the same code over the file it wrote, untouched since: unchanged --
  # without reading the file again; #417, a save after a card's edit)
  md5 <- if (file.exists(f)) unname(tools::md5sum(f))
  hit <- .put_cache[[f]]
  if (!is.null(md5) && !is.null(hit) && identical(hit$md5, md5) && identical(hit$code, code)) {
    return("unchanged")
  }
  state <- .ard_program_state(code, f)
  if (state != "current") {
    if (state == "edited") .back_up_edited(f, root)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    .write_program(code, f)
  }
  .put_cache[[f]] <- list(md5 = unname(tools::md5sum(f)), code = code)
  switch(state, current = "unchanged", edited = "rewritten", "written")
}
.put_cache <- new.env()

.ard_prog_cache <- new.env()

# the ARD programs, written with the study: the setup, one per report that
# makes an ARD (PROTOTYPE: tfl_onepass_program(kind = "ard")), the
# autoexec; a program of an output no longer defined goes (one edited by
# hand copied to programs/.edited/ first)
.save_ard_programs <- function(p, spec, root, codelists = NULL) {
  lay <- study_layout()
  dir <- file.path(root, lay[["programs_ard"]])
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  put <- function(code, name) {
    f <- file.path(dir, name)
    out[nrow(out) + 1L, ] <<- list(f, .put_program(code, f, root))
  }
  put(ard_setup_code(spec), .ard_setup_file)
  # (a report's ARD program kept while its inputs and the day are the same:
  # a save after an edit of a table writes the same programs, #417)
  key <- list(spec, root, codelists, Sys.Date(), .code_context$opts)
  if (!identical(.ard_prog_cache$key, key)) {
    .ard_prog_cache$key <- key
    .ard_prog_cache$code <- list()
  }
  ids <- character()
  for (id in p$outputs$output_id) {
    k <- .program_key(p, id)
    hit <- .ard_prog_cache$code[[id]]
    f <- file.path(dir, .ard_prog_name(id))
    if (is.null(hit) || !identical(hit$key, k)) {
      # (not made in this session: a file whose checksum is the one
      # recorded for the same inputs in an earlier session is current, and
      # its program is not made again -- R/program_hashes.R, #749; "none":
      # the report makes no ARD)
      rec <- .code_hash_key("ard", id, list(spec, root, codelists, k))
      prog <- paste0("ard:", id)
      h <- .code_hash_get(prog, rec)
      if (identical(h, "none") && !file.exists(f)) next
      if (!is.null(h) && identical(.program_file_hash(f), h)) {
        ids <- c(ids, id)
        out[nrow(out) + 1L, ] <- list(f, "unchanged")
        next
      }
      hit <- list(key = k, code = ard_program_code(p, id))
      .ard_prog_cache$code[[id]] <- hit
      .code_hash_put(prog, rec, if (is.null(hit$code)) "none" else .body_hash(hit$code))
    }
    if (is.null(hit$code)) next
    ids <- c(ids, id)
    put(hit$code, .ard_prog_name(id))
  }
  put(ard_autoexec_code(spec), .ard_autoexec_file)
  keep <- c(.ard_setup_file, .ard_autoexec_file, vapply(ids, .ard_prog_name, ""))
  for (f in setdiff(list.files(dir, "\\.[Rr]$"), keep)) {
    p <- file.path(dir, f)
    have <- readLines(p, warn = FALSE, encoding = "UTF-8")
    chk <- sub("^#  Checksum   : *", "",
               grep("^#  Checksum   :", have, value = TRUE))
    # a program tflplanner wrote (with a checksum); a file of one's own,
    # never written by it, is not its to remove
    if (!length(chk)) next
    if (!identical(chk[1L], .body_hash(have))) .back_up_edited(p, root)
    file.remove(p)
    out[nrow(out) + 1L, ] <- list(p, "removed")
  }
  # the single make_ard.R of earlier versions
  old <- file.path(root, "programs", "make_ard.R")
  if (file.exists(old)) {
    file.remove(old)
    out[nrow(out) + 1L, ] <- list(old, "removed")
  }
  out
}

# The reports that have an ARD program (PROTOTYPE): tflspec's rule for
# tfl_onepass_program(kind = "ard") -- analyses in the definition, or a
# data step of their own code
.ard_program_ids <- function(p) {
  a <- unique(stats::na.omit((p$ard %||% .empty_ard_spec())$analyses$output_id))
  Filter(function(id) id %in% a || .is_code_step(p, id, "data"), p$outputs$output_id)
}
