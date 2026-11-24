# A fake official run: a batch folder as tflspec's runner leaves it, written
# without running anything.  The screens read the latest run of each report
# (R/latest_run.R, G4 slice A''), so a test that wants a report "made" or
# "failed" writes one of these -- not an RTF in output/tfl/, which is what a
# preview leaves.
#
# `study_dir` the study folder (its study.yml says the study id), `stamp`
# the batch folder's name, `part` "tfl" or "ard", `ids` the output ids the
# run made (or failed to make: `status`, one for all or one each).
# `programs`: the programs' paths relative to the study (default
# programs/<part>/<id>.R); their md5 is recorded as the files are now, so the
# report is not "outdated" by its program until it is changed.  `files`: what
# the run made, relative to the batch folder -- a character vector (stub
# RTFs) or a named list (path -> lines); by default a stub RTF at
# output/tfl/<id>.rtf for each report the run made.  `ard_status`, `report_status`:
# data frames written as output/ard/ard_status.csv and output/tfl/report_status.csv (as the run's own
# files: the ARD's record and the reports' record).  `finished`: when the run
# ended (default two seconds from now, after the programs the test just
# wrote); it started a second before.  `note`: run.csv's note for each id.
# Returns the batch folder.
fake_run <- function(study_dir, stamp, part = "tfl", ids = character(),
                     status = "OK", files = NULL, ard_status = NULL,
                     report_status = NULL, finished = Sys.time() + 2,
                     programs = NULL, note = "", study_id = NULL) {
  d <- file.path(study_dir, "runs", stamp)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  if (is.null(study_id)) {
    study_id <- yaml::read_yaml(file.path(study_dir, "study.yml"))$study_id
  }
  n <- length(ids)
  if (is.null(programs)) programs <- file.path("programs", part, paste0(ids, ".R"))
  md5 <- if (n) unname(tools::md5sum(file.path(study_dir, programs))) else character()
  # (a failed program has its log, too)
  log <- if (n) file.path("logs", sub("[.][Rr]$", ".log", basename(programs))) else character()
  rows <- data.frame(
    part = rep(part, n), program = rep(programs, length.out = n),
    output_id = ids, status = rep(status, length.out = n),
    errors = as.integer(rep(status, length.out = n) != "OK"),
    warnings = rep(0L, n), note = rep(note, length.out = n),
    started = rep(format(finished - 1, "%Y-%m-%d %H:%M:%S"), n),
    seconds = rep(1, n), md5 = md5, log = log, stringsAsFactors = FALSE)
  utils::write.csv(rows, file.path(d, "run.csv"), row.names = FALSE)
  for (k in seq_len(n)) {
    if (rows$status[k] != "OK") {
      dir.create(dirname(file.path(d, log[k])), recursive = TRUE, showWarnings = FALSE)
      writeLines(c("Error in eval(expr): a made-up failure", "Execution halted"),
                 file.path(d, log[k]))
    }
  }
  put <- function(rel, lines) {
    f <- file.path(d, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, f)
  }
  if (is.null(files)) {
    files <- if (part == "tfl") file.path("output", "tfl", paste0(ids[rows$status == "OK"], ".rtf")) else character()
  }
  if (is.list(files)) {
    for (rel in names(files)) put(rel, files[[rel]])
  } else for (rel in files) put(rel, "{\\rtf1 }")
  csv <- function(x, rel) {
    f <- file.path(d, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(x, f, row.names = FALSE)
  }
  if (!is.null(ard_status)) csv(ard_status, "output/ard/ard_status.csv")
  if (!is.null(report_status)) csv(report_status, "output/tfl/report_status.csv")
  fmt <- function(t) format(t, "%Y-%m-%d %H:%M:%S")
  writeLines(c(paste("Batch    :", stamp), paste("Study    :", study_id),
               "Spec     : none", paste("Runs     :", part),
               paste("Started  :", fmt(finished - 1)),
               paste("Finished :", fmt(finished))),
             file.path(d, "batch.txt"))
  invisible(d)
}
