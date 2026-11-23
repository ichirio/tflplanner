# A study of many reports, for the tests and the screen reviews of the
# report list (R/report_picker.R): the sample study, its reports copied
# into 10 sections of a TOC until there are `n`, and their states mixed --
# the ARD made, to make again, failed or not made; the report made, failed
# or not -- written as the files that say so, without running anything.
# data-raw/make-big-study.R makes one in a temporary home.

.big_sections <- list(
  "14.1" = c(from = "T-14-1-1", topic = "Demographics"),
  "14.2" = c(from = "T-14-2-1", topic = "Vital signs"),
  "14.3" = c(from = "T-14-3-1", topic = "Adverse events"),
  "14.4" = c(from = "T-14-1-2", topic = "Disposition"),
  "14.5" = c(from = "T-14-2-2", topic = "Time to event"),
  "14.6" = c(from = "T-14-2-1", topic = "Laboratory: chemistry"),
  "14.7" = c(from = "T-14-2-1", topic = "Laboratory: hematology"),
  "15.1" = c(from = "F-14-2-3", topic = "Kaplan-Meier curves"),
  "15.2" = c(from = "F-14-2-1", topic = "Mean over time"),
  "16.2" = c(from = "L-16-2-7", topic = "Listing of adverse events"))

.big_words <- c("by age group", "by sex", "by race", "by region", "Week 4",
                "Week 12", "Week 24", "end of treatment", "serious",
                "leading to discontinuation", "by severity", "by relationship",
                "subjects aged 65 or over", "Japanese subjects", "per protocol set")

# `n` values in these shares, spread out the same way each time
.big_mix <- function(n, shares, shift = 0L) {
  v <- rep(names(shares), round(shares * n))
  v <- c(v, rep(names(shares)[1L], max(0L, n - length(v))))[seq_len(n)]
  v[order((seq_len(n) * 37L + shift) %% 101L, seq_len(n))]
}

#' @noRd
.make_big_study <- function(n = 200L, root, home = tflplanner_home(),
                            study_id = "BIG-200") {
  s <- suppressMessages(create_sample_study(
    root = root, run = FALSE, study_id = study_id, home = home,
    title = sprintf("%d reports (for testing the report list)", n)))
  p <- s$planner
  have <- output_ids(p)
  secs <- names(.big_sections)
  k <- stats::setNames(integer(length(secs)), secs)
  i <- 0L
  while (length(output_ids(p)) < n) {
    sec <- secs[(i %% length(secs)) + 1L]
    i <- i + 1L
    k[[sec]] <- k[[sec]] + 1L
    from <- .big_sections[[sec]][["from"]]
    prefix <- substr(from, 1L, 1L)
    id <- sprintf("%s-%s-%d", prefix, gsub(".", "-", sec, fixed = TRUE), k[[sec]] + 10L)
    if (id %in% output_ids(p)) next
    p <- copy_output(p, from, id)
    word <- .big_words[(k[[sec]] - 1L) %% length(.big_words) + 1L]
    p$outputs$description[p$outputs$output_id == id] <-
      paste(.big_sections[[sec]][["topic"]], word, sep = ", ")
  }
  s$planner <- p
  s <- suppressMessages(save_study(s, home = home))
  # the states, mixed as a study in progress has them (the same each time,
  # without the random numbers' state)
  ids <- output_ids(s$planner)
  ard_ids <- unique(stats::na.omit(s$planner$ard$analyses$output_id))
  pick <- .big_mix(length(ard_ids), c(built = 0.5, outdated = 0.2, error = 0.08, none = 0.22))
  spec <- structure(s$planner$ard, class = "tfl_ard_spec")
  cl <- .study_codelists(s$planner)
  hash <- vapply(ard_ids, function(id)
    tflspec::tfl_ard_spec_hash(spec, id, dir = s$path, codelists = cl), "")
  # the reports: about half made, a few failed (a later run's error)
  run <- .big_mix(length(ids), c(made = 0.5, failed = 0.06, none = 0.44), shift = 13L)
  lay <- study_layout()
  Sys.sleep(1)  # the runs after the programs, by the files' times
  # the states are the results of official runs: two batch folders
  # (the first made what was made, the second -- later -- failed what failed),
  # written as the files that say so
  keep <- pick != "none"
  ok_ard <- pick %in% c("built", "outdated")
  ard_rows <- data.frame(
    output_id = ard_ids[keep],
    definition = ifelse(pick[keep] == "outdated", "an earlier definition", hash[keep]),
    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    rows = ifelse(pick[keep] == "error", NA_integer_, 50L),
    error = ifelse(pick[keep] == "error", "Error in cards::ard_summary(): a made-up failure", ""),
    setup = .study_setup_hash(s$path),
    stringsAsFactors = FALSE)
  progs <- vapply(ids, function(id) report_info(s$planner, id)$program, "")
  made <- run %in% c("made", "failed")
  md5 <- unname(tools::md5sum(file.path(s$path, lay[["programs_tfl"]], progs)))
  rtfs <- vapply(ids, function(id) report_info(s$planner, id)$file, "")
  .write_run_folder(
    s, "20260101_100000_all", started = Sys.time(),
    rows = rbind(
      .run_rows("ard", ard_ids[ok_ard], "OK"),
      .run_rows("tfl", ids[made], "OK", md5 = md5[made],
                program = file.path(lay[["programs_tfl"]], progs[made])),
      .run_rows("ard", ard_ids[pick == "error"], "ERROR",
                note = "Error in cards::ard_summary(): a made-up failure")),
    ard_status = ard_rows, files = rtfs[made])
  if (any(run == "failed")) {
    .write_run_folder(
      s, "20260102_100000_all", started = Sys.time() + 5,
      rows = .run_rows("tfl", ids[run == "failed"], "ERROR",
                       program = file.path(lay[["programs_tfl"]], progs[run == "failed"]),
                       note = "Error in eval(expr): a made-up failure"))
  }
  invisible(open_study(s$path, home = home))
}

# the rows of a fake run's run.csv: one per id (as tflspec's runner writes
# them; `program` the program's path, `md5` its md5)
.run_rows <- function(part, ids, status, program = NULL, md5 = NA_character_,
                      note = "") {
  n <- length(ids)
  if (is.null(program)) program <- file.path("programs", part, paste0(ids, ".R"))
  data.frame(part = rep(part, n), program = rep(program, length.out = n),
             output_id = ids, status = rep(status, n),
             errors = rep(as.integer(status != "OK"), n), warnings = rep(0L, n),
             note = rep(note, n), started = rep("2026-01-01 10:00:00", n),
             seconds = rep(1, n), md5 = rep(md5, length.out = n),
             log = rep(NA_character_, n), stringsAsFactors = FALSE)
}

# A batch folder as the runner leaves it, without running anything: run.csv
# (`rows`), batch.txt (the study's id, when it started and finished: one
# second after `started`), the study's ARD status (`ard_status`, at the
# study ARD's own path) and the files the run made (`files`, relative to the
# folder, each a stub RTF).  The reports and the ARD read the latest run
# (R/latest_run.R), so this is how a study of fake results is made.
.write_run_folder <- function(study, stamp, rows, started = Sys.time(),
                              ard_status = NULL, files = character()) {
  d <- file.path(study$path, study_layout()[["runs"]], stamp)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rows, file.path(d, "run.csv"), row.names = FALSE)
  if (!is.null(ard_status)) {
    f <- file.path(dirname(.study_ard_file(study, d)), "ard_status.csv")
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(ard_status, f, row.names = FALSE)
  }
  for (f in file.path(d, files)) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines("{\\rtf1 }", f)
  }
  fmt <- function(t) format(t, "%Y-%m-%d %H:%M:%S")
  writeLines(c(paste("Batch    :", stamp),
               paste("Study    :", study$meta$study_id),
               paste("Started  :", fmt(started)),
               paste("Finished :", fmt(started + 1))),
             file.path(d, "batch.txt"))
  invisible(d)
}
