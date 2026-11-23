# The Runs tab's ZIPs of a run folder (#790, Q-C6): the output ZIP, the
# all-files ZIP downloaded at or under the limit, made in the background as
# runs/<folder>.zip over it; the per-report RTF download and "Open
# output/tfl" are gone.

rz_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# a study of one report and a run folder with output, logs, code and data
rz_study <- function(name = "20260101_100000_all") {
  p <- add_output(new_planner(), "DM", process_code = "x <- 1")
  s <- create_study("RZ", planner = p)
  d <- file.path(s$path, "runs", name)
  put <- function(f, x = "x") {
    dir.create(dirname(file.path(d, f)), recursive = TRUE, showWarnings = FALSE)
    writeLines(x, file.path(d, f))
  }
  put("output/tfl/DM.rtf", "{\\rtf1 DM}")
  put("output/ard/ard_status.csv")
  put("logs/tfl/DM.log")
  put("code/programs/tfl/DM.R")
  put("data/adsl.csv")
  put("session_info.txt")
  utils::write.csv(data.frame(part = "tfl", program = "programs/tfl/DM.R", status = "OK",
                              errors = 0L, warnings = 0L, seconds = 1.5, note = ""),
                   file.path(d, "run.csv"), row.names = FALSE)
  writeLines(c("Batch : test", "Finished : 2026-01-01 10:00:30"), file.path(d, "batch.txt"))
  list(study = s, dir = d, name = name)
}

# the notifications the server shows, as a function that gives them so far
rz_notes <- function(env = parent.frame()) {
  got <- new.env()
  got$n <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      got$n <- c(got$n, paste(as.character(ui), collapse = ""))
      invisible("id")
    }, .package = "shiny", .env = env)
  function() got$n
}

# the data frame the runs table is last drawn from (it is drawn server side)
rz_tables <- function(env = parent.frame()) {
  got <- new.env()
  draw <- DT::datatable
  testthat::local_mocked_bindings(
    datatable = function(data, ...) {
      got$d <- data
      draw(data, ...)
    }, .package = "DT", .env = env)
  function() got$d
}

rz_entries <- function(f) sort(zip::zip_list(f)$filename)

test_that("a run folder's output ZIP: output/, logs/, run.csv and batch.txt", {
  rz_home()
  r <- rz_study()
  rz_notes()
  tab <- rz_tables()
  shiny::testServer(server_for("RZ"), {
    session$setInputs(nav = "results")
    # each row has the two links
    output$batches
    z <- tab()[["Download (ZIP)"]]
    expect_match(z, "{batch: '20260101_100000_all', what: 'output'}", fixed = TRUE)
    expect_match(z, "{batch: '20260101_100000_all', what: 'all'}", fixed = TRUE)
    session$setInputs(batch_zip_row = list(batch = r$name, what = "output"))
    f <- output$batch_zip_dl
    got <- rz_entries(f)
    n <- r$name
    expect_true(all(file.path(n, c("run.csv", "batch.txt", "output/tfl/DM.rtf",
                                   "output/ard/ard_status.csv", "logs/tfl/DM.log")) %in% got))
    expect_false(any(grepl(paste0("^", n, "/(code|data|session_info)"), got)))
    # a folder that is not on the list: nothing chosen
    session$setInputs(batch_zip_row = list(batch = "no-such-run", what = "all"))
    expect_true(all(file.path(n, "run.csv") %in% rz_entries(output$batch_zip_dl)))
  })
})

test_that("the all-files ZIP at or under the limit is downloaded", {
  rz_home()
  r <- rz_study()
  notes <- rz_notes()
  shiny::testServer(server_for("RZ"), {
    session$setInputs(nav = "results", batch_zip_row = list(batch = r$name, what = "all"))
    got <- rz_entries(output$batch_zip_dl)
    expect_true(all(file.path(r$name, c("code/programs/tfl/DM.R", "data/adsl.csv",
                                        "session_info.txt", "output/tfl/DM.rtf")) %in% got))
    expect_false(file.exists(paste0(r$dir, ".zip")))
    expect_length(notes(), 0L)
  })
})

test_that("over the limit the all-files ZIP is made in the background as runs/<folder>.zip", {
  rz_home()
  r <- rz_study()
  .set_config("zip_download_limit_mb", 1e-6)
  expect_identical(.zip_limit_mb(), 1e-6)
  notes <- rz_notes()
  shiny::testServer(server_for("RZ"), {
    session$setInputs(nav = "results", batch_zip_row = list(batch = r$name, what = "all"))
    expect_match(notes()[1L], "being made in the background as runs/20260101_100000_all.zip",
                 fixed = TRUE)
    # not downloaded
    expect_error(output$batch_zip_dl, "No run folder")
    key <- normalizePath(r$dir, "/", mustWork = FALSE)
    px <- .zip_jobs[[key]]
    expect_false(is.null(px))
    px$wait(30000)
    session$elapse(1500)
    expect_true(file.exists(paste0(r$dir, ".zip")))
    expect_true(file.path(r$name, "code/programs/tfl/DM.R") %in% rz_entries(paste0(r$dir, ".zip")))
    expect_match(notes()[2L], "The ZIP of 20260101_100000_all is ready", fixed = TRUE)
    expect_null(.zip_jobs[[key]])
    # one ZIP job per folder at a time
    assign(key, list(is_alive = function() TRUE), envir = .zip_jobs)
    on.exit(.batch_zip_bg_done(r$dir), add = TRUE)
    session$setInputs(batch_zip_row = list(batch = r$name, what = "all"))
    expect_match(notes()[3L], "is being made already", fixed = TRUE)
    expect_error(.batch_zip_bg(r$dir), "is being made")
  })
})

test_that("a bad or missing limit is 500 MB", {
  rz_home()
  expect_identical(.zip_limit_mb(), 500)
  .set_config("zip_download_limit_mb", "lots")
  expect_identical(.zip_limit_mb(), 500)
})

test_that("Open output/tfl and the per-report RTF download are gone", {
  html <- as.character(app_ui())
  expect_false(grepl('id="tfl_open"', html, fixed = TRUE))
  expect_false(grepl('id="rtf_download"', html, fixed = TRUE))
  expect_true(grepl('id="batch_zip_dl"', html, fixed = TRUE))
})
