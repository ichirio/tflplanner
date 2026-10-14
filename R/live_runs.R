# Official runs that go on by themselves.  run_batch() starts its process
# detached (processx's cleanup = FALSE): the run keeps going when the app
# closes, the browser is closed or the app is started again.  Each run is
# recorded in the home, live_runs/<study_id>.dcf -- its process (pid and
# when it was created, so a pid used again by another process is not taken
# for it), its study, what it runs, when it started and the batch folders
# there were before it -- so an app opened later shows it running, a
# study runs one official run at a time, and no update installs packages
# a run has loaded.  The launcher reads the same files (inst/launcher).

.live_dir <- function(home = tflplanner_home()) file.path(home, "live_runs")

.live_file <- function(study_id, home = tflplanner_home()) {
  file.path(.live_dir(home), paste0(gsub("[^A-Za-z0-9._-]", "_", study_id), ".dcf"))
}

# The process's create time, as a number (seconds), NA when it is gone
.proc_created <- function(pid) {
  tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle(as.integer(pid)))),
           error = function(e) NA_real_)
}

# Record a run that started: `px` a processx process
.record_run <- function(px, study, what, before = character(),
                        home = tflplanner_home()) {
  f <- .live_file(study$meta$study_id, home)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  pid <- px$get_pid()
  write.dcf(data.frame(
    study_id = study$meta$study_id, path = study$path, pid = pid,
    created = format(.proc_created(pid), digits = 15),
    started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), what = what,
    before = paste(before, collapse = "|"),
    tflplanner = as.character(utils::packageVersion("tflplanner")),
    stringsAsFactors = FALSE), f)
  invisible(f)
}

# Is the run of a record still going on: its pid alive, and the same
# process (the same create time)
.run_alive <- function(rec) {
  now <- .proc_created(rec$pid)
  !is.na(now) && isTRUE(abs(now - as.numeric(rec$created)) < 1)
}

# The official runs going on now, in any study of the home: a data frame,
# one row a run (study_id, path, pid, started, what)
.live_runs <- function(home = tflplanner_home()) {
  cols <- c("study_id", "path", "pid", "started", "what")
  empty <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols))
  recs <- .live_records(home)
  if (!length(recs)) return(empty)
  out <- do.call(rbind, lapply(recs, function(r) as.data.frame(r[cols], stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  out
}

# The records of runs going on (a list of named lists); the others removed
.live_records <- function(home = tflplanner_home()) {
  fs <- list.files(.live_dir(home), "[.]dcf$", full.names = TRUE)
  recs <- lapply(fs, function(f) {
    r <- tryCatch(as.list(read.dcf(f)[1L, ]), error = function(e) NULL)
    if (is.null(r) || is.null(r$pid) || !.run_alive(r)) {
      unlink(f)
      return(NULL)
    }
    r$file <- f
    r
  })
  Filter(Negate(is.null), recs)
}

# The run going on in a study, or NULL
.live_run <- function(study_id, home = tflplanner_home()) {
  f <- .live_file(study_id, home)
  if (!file.exists(f)) return(NULL)
  r <- tryCatch(as.list(read.dcf(f)[1L, ]), error = function(e) NULL)
  if (is.null(r) || !.run_alive(r)) {
    unlink(f)
    return(NULL)
  }
  r$file <- f
  r
}

# A run the app did not start (opened later): what the app asks of a
# processx process -- is_alive(), get_exit_status() (unknown: the batch
# folder says how it went), kill() -- from its record
.attach_run <- function(rec) {
  h <- tryCatch(ps::ps_handle(as.integer(rec$pid)), error = function(e) NULL)
  list(
    is_alive = function() !is.null(h) && .run_alive(rec),
    get_exit_status = function() NA_integer_,
    kill = function() if (!is.null(h)) try(ps::ps_kill(h), silent = TRUE),
    get_pid = function() as.integer(rec$pid))
}

# The message for runs going on, when they keep something from happening
.live_runs_text <- function(runs, t = identity) {
  paste(sprintf(t("The official run of %s is going on (started %s)."),
                runs$study_id, substr(runs$started, 12L, 16L)), collapse = " ")
}
