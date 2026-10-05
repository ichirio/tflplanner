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
  keep <- pick != "none"
  .write_ard_status(s, data.frame(
    output_id = ard_ids[keep],
    definition = ifelse(pick[keep] == "outdated", "an earlier definition", hash[keep]),
    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    rows = ifelse(pick[keep] == "error", NA_integer_, 50L),
    error = ifelse(pick[keep] == "error", "Error in cards::ard_summary(): a made-up failure", ""),
    stringsAsFactors = FALSE))
  # the reports: about half made, a few failed (a log after the RTF)
  run <- .big_mix(length(ids), c(made = 0.5, failed = 0.06, none = 0.44), shift = 13L)
  lay <- study_layout()
  Sys.sleep(1)  # the RTFs after the programs, by the files' times
  for (j in seq_along(ids)) {
    info <- report_info(s$planner, ids[j])
    if (run[j] %in% c("made", "failed")) {
      f <- file.path(s$path, info$file)
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      writeLines("{\\rtf1 }", f)
    }
    if (run[j] == "failed") {
      Sys.setFileTime(file.path(s$path, info$file), Sys.time() - 60)
      log <- file.path(s$path, lay[["logs_preview"]], sub("\\.[Rr]$", ".log", info$program))
      dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
      writeLines(c("Error in eval(expr): a made-up failure", "Execution halted"), log)
    }
  }
  invisible(open_study(s$path, home = home))
}
