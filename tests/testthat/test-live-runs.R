# Official runs that go on by themselves (live_runs.R): recorded in the
# home, found again by an app opened later, one at a time in a study, and
# no update while one goes on

# a process standing in for a run: an Rscript that waits.  It is killed, so
# what an R process leaves in its temporary folder stays: a script file
# (not -e, which Rscript writes to a file of its own) and its temporary
# folder inside this session's, which goes with it
sleeper <- function(secs = 60) {
  tmp <- file.path(tempdir(), "sleepers")
  dir.create(tmp, showWarnings = FALSE)
  f <- tempfile("sleep", tmpdir = tmp, fileext = ".R")
  writeLines(sprintf("Sys.sleep(%d)", secs), f)
  processx::process$new(file.path(R.home("bin"), "Rscript"), f,
                        stdout = NULL, stderr = NULL, cleanup = TRUE,
                        env = c("current", TMPDIR = tmp, TMP = tmp, TEMP = tmp))
}
fake_study <- function(id = "S1", path = tempdir()) list(meta = list(study_id = id), path = path)

test_that("a run is recorded in the home and found while its process lives", {
  local_home()
  px <- sleeper()
  on.exit(px$kill(), add = TRUE)
  f <- .record_run(px, fake_study(), "ard+tfl", before = c("20261013_101010_all", "x"))
  expect_true(file.exists(f))
  runs <- .live_runs()
  expect_identical(runs$study_id, "S1")
  expect_identical(runs$what, "ard+tfl")
  rec <- .live_run("S1")
  expect_identical(strsplit(rec$before, "|", fixed = TRUE)[[1L]], c("20261013_101010_all", "x"))
  h <- .attach_run(rec)
  expect_true(h$is_alive())
  expect_true(is.na(h$get_exit_status()))
  expect_match(.live_runs_text(runs), "The official run of S1 is going on (started ", fixed = TRUE)
  # its process gone: no run, and the record is removed
  px$kill()
  px$wait(5000)
  expect_false(h$is_alive())
  expect_identical(nrow(.live_runs()), 0L)
  expect_false(file.exists(f))
  expect_null(.live_run("S1"))
})

test_that("a pid used again by another process is not the run", {
  local_home()
  px <- sleeper()
  on.exit(px$kill(), add = TRUE)
  f <- .record_run(px, fake_study("S2"), "tfl")
  r <- read.dcf(f)
  r[1L, "created"] <- format(as.numeric(r[1L, "created"]) - 3600, digits = 15)
  write.dcf(r, f)
  expect_null(.live_run("S2"))
  expect_false(file.exists(f))
})

test_that("an official run is detached: it goes on when the R process that started it ends", {
  skip_on_cran()
  local_home()
  out <- tempfile(fileext = ".txt")
  code <- sprintf(paste0(
    "p <- processx::process$new(file.path(R.home('bin'), 'Rscript'), ",
    "c('-e', 'Sys.sleep(4); writeLines(\"done\", \"%s\")'), cleanup = FALSE); ",
    "cat(p$get_pid())"), gsub("\\\\", "/", out))
  tmp <- file.path(tempdir(), "sleepers")
  dir.create(tmp, showWarnings = FALSE)
  f <- tempfile("detach", tmpdir = tmp, fileext = ".R")
  writeLines(code, f)
  pid <- as.integer(processx::run(file.path(R.home("bin"), "Rscript"), f,
                                  env = c("current", TMPDIR = tmp, TMP = tmp, TEMP = tmp))$stdout)
  # the parent has ended; the child goes on and finishes
  for (i in 1:40) if (!file.exists(out)) Sys.sleep(0.25)
  expect_true(file.exists(out))
  expect_false(is.na(pid))
  # run_batch() starts its process so
  expect_true(any(grepl("cleanup = FALSE", deparse(body(run_batch)), fixed = TRUE)))
})

test_that("a study runs one official run at a time", {
  local_home()
  px <- sleeper()
  on.exit(px$kill(), add = TRUE)
  .record_run(px, fake_study("S3"), "ard")
  expect_error(run_batch(fake_study("S3")), "An official run of S3 is going on")
})

test_that("no update while an official run goes on", {
  local_home()
  px <- sleeper()
  on.exit(px$kill(), add = TRUE)
  .record_run(px, fake_study("S4"), "tfl")
  # (in the console's language)
  expect_error(update_tflplanner(ask = FALSE), "S4")
  expect_error(withr::with_options(list(tflplanner.language = "en"), {
    local_mocked_bindings(.console_language = function() "en")
    update_tflplanner(ask = FALSE)
  }), "The official run of S4 is going on (started ", fixed = TRUE)
})

test_that("the app follows an official run started before it, and leaves it running", {
  skip_on_cran()
  local_home()
  create_study("LR", planner = add_output(new_planner(), "T1"))
  px <- sleeper()
  on.exit(px$kill(), add = TRUE)
  .record_run(px, open_study("LR"), "ard+tfl", before = "20261001_000000_all")
  shiny::testServer(server_for("LR"), {
    rv <- session$userData$rv
    session$flushReact()
    expect_false(is.null(rv$job))
    expect_identical(rv$job_kind, "official")
    expect_identical(rv$job_study, "LR")
    expect_identical(rv$job_batches_before, "20261001_000000_all")
    expect_match(rv$job_what, "the official run (started ", fixed = TRUE)
    # [Close] says it goes on
    session$setInputs(close_app = 1)
  })
  # the session has ended; the run goes on
  expect_true(px$is_alive())
  expect_identical(.live_runs()$study_id, "LR")
})

test_that("the app serves its version and process for the launcher", {
  d <- .serve_health()
  h <- readLines(file.path(d, "health.txt"))
  expect_identical(h[1L], paste("tflplanner", as.character(utils::packageVersion("tflplanner"))))
  expect_identical(h[2L], paste("pid", Sys.getpid()))
  expect_true("tflplanner-health" %in% names(shiny::resourcePaths()))
})

test_that("the launcher: the three cases of a taken port, each logged", {
  l <- readLines(system.file("launcher", "launch.R", package = "tflplanner"), encoding = "UTF-8")
  txt <- paste(l, collapse = "\n")
  expect_match(txt, "already running (same version ", fixed = TRUE)
  expect_match(txt, "古いアプリが動いています。止めて起動し直しますか？", fixed = TRUE)
  expect_match(txt, "is taken by another program", fixed = TRUE)
  expect_match(txt, "starting on port ", fixed = TRUE)
  # no update while a run goes on
  expect_match(txt, "Not updated: an official run is going on", fixed = TRUE)
})
