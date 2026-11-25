# Coverage of R/live_runs.R: the records of official runs that go on by
# themselves -- empty home, a record that cannot be read, a process that
# is gone.  Records live in a home in a temporary folder; no run is made.

test_that("a study id's record file name keeps only safe characters", {
  home <- withr_tempdir()
  f <- .live_file("S 1/../x", home)
  expect_identical(basename(f), "S_1_.._x.dcf")
  expect_identical(dirname(f), file.path(home, "live_runs"))
})

test_that("a home with no record has no run, with the columns the screens read", {
  home <- withr_tempdir()
  runs <- .live_runs(home)
  expect_identical(nrow(runs), 0L)
  expect_identical(names(runs), c("study_id", "path", "pid", "started", "what"))
  expect_null(.live_run("S1", home))
})

test_that("a record that cannot be read, or has no pid, is removed and is no run", {
  home <- withr_tempdir()
  dir.create(.live_dir(home))
  bad <- file.path(.live_dir(home), "bad.dcf")
  nopid <- file.path(.live_dir(home), "nopid.dcf")
  writeLines("this line has no field name", bad)
  writeLines("study_id: X", nopid)
  expect_identical(nrow(.live_runs(home)), 0L)
  expect_false(file.exists(bad))
  expect_false(file.exists(nopid))
})

test_that("a study's record that cannot be read is removed and is no run", {
  home <- withr_tempdir()
  dir.create(.live_dir(home))
  f <- .live_file("S1", home)
  writeLines("this line has no field name", f)
  expect_null(.live_run("S1", home))
  expect_false(file.exists(f))
})

test_that("a record of a process that is gone is not alive", {
  expect_false(.run_alive(list(pid = "2147483646", created = "1")))
  expect_true(is.na(.proc_created(2147483646)))
})

test_that("a run attached from a record of a gone process is not alive and can be killed", {
  h <- .attach_run(list(pid = "2147483646", created = "1"))
  expect_false(h$is_alive())
  expect_true(is.na(h$get_exit_status()))
  expect_identical(h$get_pid(), 2147483646L)
  expect_no_error(h$kill())
})

test_that("the text of no run going on is empty; of two, both said", {
  expect_identical(.live_runs_text(data.frame(study_id = character(), started = character())), "")
  runs <- data.frame(study_id = c("A", "B"),
                     started = c("2026-01-01 10:15:00", "2026-01-01 11:20:00"))
  txt <- .live_runs_text(runs, t = function(s) s)
  expect_match(txt, "The official run of A is going on (started 10:15).", fixed = TRUE)
  expect_match(txt, "The official run of B is going on (started 11:20).", fixed = TRUE)
})
