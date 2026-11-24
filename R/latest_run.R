# The screens read the latest official run (G4, slice A'').
#
# D (design aid) readers keep the study's working output/ -- what previews
# and hand runs leave: the builder, the figure designer, the ARD tab.
# R (result) readers read the latest official run, a batch folder of runs/:
# a report's state, the report list's marks, the RTF a report has (its
# `rtf_path`), the ARD state the report list shows.
#
# This file is the one reader of the runs.  tflspec's tfl_run_index() says
# what each finished run made; the three functions here answer the
# screens' questions from it.

.empty_run_index <- function() {
  data.frame(run = character(), started = .POSIXct(numeric()),
             finished = .POSIXct(numeric()), part = character(),
             program = character(), output_id = character(),
             status = character(), md5 = character(), path = character(),
             stringsAsFactors = FALSE)
}

# what changes when a run ends: runs/*/batch.txt (written last, once), with
# study.yml (the study's id says whose runs count)
.run_index_stamp <- function(path) {
  bt <- Sys.glob(file.path(path, "runs", "*", "batch.txt"))
  one <- function(f) {
    f <- f[file.exists(f)]
    if (!length(f)) return("")
    paste(length(f), format(max(file.mtime(f)), "%Y%m%d%H%M%OS3"), sum(file.size(f)))
  }
  paste(one(bt), one(file.path(path, .study_file)), sep = "|")
}

.run_index_memo <- new.env(parent = emptyenv())

# Every finished run of the study, newest first (tflspec::tfl_run_index()),
# read again only when a run ended since; the empty index when it cannot be
# read.
.run_index <- function(study) {
  path <- study$path
  key <- .run_index_stamp(path)
  hit <- .run_index_memo[[path]]
  if (!is.null(hit) && identical(hit$key, key)) return(hit$idx)
  idx <- tryCatch(tflspec::tfl_run_index(path), error = function(e) NULL)
  if (!is.data.frame(idx)) idx <- .empty_run_index()
  .run_index_memo[[path]] <- list(key = key, idx = idx)
  idx
}

# The latest run that made each id without error (the rule of
# tflspec::tfl_latest_run(), from the index read once): `output_id`, `path`
# (the batch folder), `finished`, `md5` (of the program it ran); NA where
# there is none.  `part`: "tfl" (the report) or "ard" (its ARD).
.latest_ok <- function(study, ids, part = "tfl", idx = .run_index(study)) {
  ids <- as.character(ids)
  x <- idx[idx$part %in% part & idx$status %in% "OK" & !is.na(idx$output_id) &
             nzchar(idx$output_id), , drop = FALSE]
  i <- match(ids, x$output_id)
  data.frame(output_id = ids, path = x$path[i], finished = x$finished[i],
             md5 = x$md5[i], stringsAsFactors = FALSE)
}

# The newest row of each id and part, whatever its status: `output_id`,
# `status`, `path`, `finished`, `log` (absolute) and `note` (what run.csv
# says of it).  NA where no run tried it.
.latest_attempt <- function(study, ids, part = "tfl", idx = .run_index(study)) {
  ids <- as.character(ids)
  x <- idx[idx$part %in% part & !is.na(idx$output_id) & nzchar(idx$output_id), ,
           drop = FALSE]
  i <- match(ids, x$output_id)
  out <- data.frame(output_id = ids, status = x$status[i], path = x$path[i],
                    finished = x$finished[i], log = NA_character_,
                    note = NA_character_, stringsAsFactors = FALSE)
  for (pth in unique(stats::na.omit(out$path))) {
    r <- tryCatch(utils::read.csv(file.path(pth, "run.csv"), colClasses = "character"),
                  error = function(e) NULL)
    if (is.null(r) || !all(c("part", "output_id") %in% names(r))) next
    for (j in which(out$path %in% pth)) {
      m <- which(r$part %in% part & r$output_id %in% out$output_id[j])[1L]
      if (is.na(m)) next
      lg <- if ("log" %in% names(r)) r$log[m] else NA_character_
      if (!is.na(lg) && nzchar(lg)) {
        out$log[j] <- if (grepl("^([A-Za-z]:|[/\\\\])", lg)) lg else file.path(pth, lg)
      }
      if ("note" %in% names(r) && !is.na(r$note[m])) out$note[j] <- r$note[m]
    }
  }
  out
}

# A later attempt failed: the latest attempt is not OK, and there is no OK
# run or the attempt is newer than it (`att`, `ok`: one row each)
.attempt_failed <- function(att, ok) {
  if (is.na(att$status[1L]) || identical(att$status[1L], "OK")) return(FALSE)
  is.na(ok$path[1L]) || isTRUE(att$finished[1L] > ok$finished[1L])
}

# When the files a program sources (programs/tfl/fig_setup.R, a setup of
# the user's) last changed, NA when it sources none.  The program itself is
# not in it: its md5 says whether it changed.
.program_sources_time <- function(prog, root) {
  if (!file.exists(prog)) return(as.POSIXct(NA))
  txt <- readLines(prog, warn = FALSE, encoding = "UTF-8")
  src <- regmatches(txt, regexpr("^\\s*source\\(\"[^\"]+\"", txt))
  src <- unique(sub("^\\s*source\\(\"", "", sub("\"$", "", src)))
  if (!length(src)) return(as.POSIXct(NA))
  ts <- do.call(c, lapply(file.path(root, src), .mtime))
  if (all(is.na(ts))) return(as.POSIXct(NA))
  max(ts, na.rm = TRUE)
}

# The program is not the one the run ran: its md5 now is not the run's
# (without a recorded md5: its time is after the run's end), or a file it
# sources changed after the run ended
.program_changed <- function(prog, root, md5, finished) {
  if (!file.exists(prog)) return(FALSE)
  if (!is.na(md5) && nzchar(md5)) {
    if (!identical(unname(tools::md5sum(prog)), md5)) return(TRUE)
  } else if (isTRUE(.mtime(prog) > finished)) return(TRUE)
  isTRUE(.program_sources_time(prog, root) > finished)
}

# A report's last preview failed: its log says so, and is newer than the
# working RTF (a design aid's fact; it does not change the report's state)
.preview_failed <- function(study, ids) {
  p <- study$planner
  lay <- study_layout()
  vapply(ids, function(id) {
    info <- report_info(p, id)
    t_rtf <- .mtime(file.path(study$path, info$file))
    log <- file.path(study$path, lay[["logs_preview"]],
                     sub("\\.[Rr]$", ".log", info$program))
    t_log <- .mtime(log)
    !is.na(t_log) && (is.na(t_rtf) || t_log > t_rtf) && .log_failed(log)
  }, NA, USE.NAMES = FALSE)
}

# Each report's state from the latest official run, for study_status() and
# the report list alike (one reader, so the two cannot drift): a data frame,
# one row per id, with
#   `state`   "error" (the latest attempt failed, and is newer than the last
#             OK run, if any), "not run" (no OK run), "outdated" (see
#             `why`), "ok"
#   `why`     for "outdated": "program" (its md5 is not the run's, or a file
#             it sources is newer than the run), "setup" (the study setup
#             is not the run's), "ard:<id>" (the ARD it prints is not the
#             one it was made from)
#   `finished` the OK run's end; `run_path` its batch folder (NA: none)
#   `log`     the failed attempt's log
.report_run_state <- function(study, ids = study$planner$outputs$output_id) {
  p <- study$planner
  root <- study$path
  lay <- study_layout()
  idx <- .run_index(study)
  ok <- .latest_ok(study, ids, "tfl", idx)
  att <- .latest_attempt(study, ids, "tfl", idx)
  now <- .report_setup_hash(root)
  # (a figure's own run folder: its ARD's status and its record are read there)
  ard_why <- .fig_ard_why(study, ids, roots = stats::setNames(ok$path, ids))
  n <- length(ids)
  state <- rep("", n)
  why <- rep("", n)
  log <- rep(NA_character_, n)
  for (k in seq_len(n)) {
    if (.attempt_failed(att[k, ], ok[k, ])) {
      state[k] <- "error"
      log[k] <- att$log[k]
    } else if (is.na(ok$path[k])) {
      state[k] <- "not run"
    } else {
      info <- report_info(p, ids[k])
      prog <- file.path(root, lay[["programs_tfl"]], info$program)
      w <- c(if (.program_changed(prog, root, ok$md5[k], ok$finished[k])) "program",
             if (.report_setup_changed(.report_setup_recorded(ok$path[k], ids[k]),
                                       root, now)) "setup",
             if (nzchar(ard_why[[k]])) ard_why[[k]])
      state[k] <- if (length(w)) "outdated" else "ok"
      why[k] <- paste(w, collapse = " ")
    }
  }
  data.frame(output_id = ids, state = state, why = why,
             finished = ok$finished, run_path = ok$path, log = log,
             stringsAsFactors = FALSE)
}
