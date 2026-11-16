# The branches of R/srv_data_run.R the other tests leave out: the data
# catalog filled when the Data tab opens, a dataset's first rows, the log
# of a report or of a run folder, the report set buttons, the checks before
# an official run, how a run ends, and the comparison of two runs.
# (Each test starts the whole app's server once: a test does several things.)

dr_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# a study of three reports; `files` are written to its data folders first,
# `ard`: DM and VS have an ARD program each
dr_study <- function(id = "DR", files = character(), ard = FALSE) {
  p <- new_planner()
  for (o in c("DM", "VS")) p <- add_output(p, o, process_code = "x <- 1")
  p <- add_output(p, "L1", type = "listing", at = "end")
  if (ard) {
    p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
    p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                    where = "SAFFL == \"Y\"")
    p$ard$analyses <- data.frame(output_id = c("DM", "VS"), analysis_id = c("AGE", "AGE"),
                                 method = "continuous", population_id = "SAF", by = "TRT01A",
                                 variables = "AGE")
    for (sh in names(p$ard)) p$ard[[sh]] <- .normalize_ard_sheet(p$ard[[sh]], sh)
  }
  s <- create_study(id, planner = p)
  for (f in files) {
    dir.create(dirname(file.path(s$path, f)), recursive = TRUE, showWarnings = FALSE)
    saveRDS(data.frame(USUBJID = c("1", "2", "3"), AGE = c(50, 61, 47)), file.path(s$path, f))
  }
  s
}

# a run folder as an official run leaves it (it ended 30 s after its name's time)
dr_batch <- function(s, name, status = "OK", excluded = NULL) {
  d <- file.path(s$path, "runs", name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(part = "tfl", program = "programs/tfl/DM.R", status = status,
                              errors = if (status == "OK") 0L else 2L, warnings = 0L,
                              seconds = 1.5, note = "", stringsAsFactors = FALSE),
                   file.path(d, "run.csv"), row.names = FALSE)
  end <- strptime(substr(name, 1, 15), "%Y%m%d_%H%M%S", tz = "UTC") + 30
  writeLines(c("Batch : test",
               if (!is.null(excluded)) paste("Excluded :", excluded),
               paste("Finished :", format(end, "%Y-%m-%d %H:%M:%S"))),
             file.path(d, "batch.txt"))
  d
}

# the notifications the server shows, as a function that gives them so far
dr_notes <- function(env = parent.frame()) {
  got <- new.env()
  got$n <- list()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "message") {
      got$n[[length(got$n) + 1L]] <- list(text = paste(as.character(ui), collapse = ""), type = type)
      invisible("id")
    }, .package = "shiny", .env = env)
  function() got$n
}

# the html of the modals the server shows, as a function that gives them so far
dr_modals <- function(env = parent.frame()) {
  got <- new.env()
  got$m <- character()
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) got$m <- c(got$m, paste(as.character(ui), collapse = "")),
    removeModal = function(session = NULL) invisible(NULL),
    .package = "shiny", .env = env)
  function() got$m
}

# the data frames the tables are drawn from (a table is drawn server side: its rows
# do not come with its output); the last one drawn, with its column names
dr_tables <- function(env = parent.frame()) {
  got <- new.env()
  got$d <- NULL
  draw <- DT::datatable
  testthat::local_mocked_bindings(
    datatable = function(data, ...) {
      got$d <- data
      draw(data, ...)
    }, .package = "DT", .env = env)
  function() got$d
}

# the ticks the server sends to the report list (the box is the browser's)
dr_ticks <- function(env = parent.frame()) {
  got <- new.env()
  got$s <- list()
  testthat::local_mocked_bindings(
    updateCheckboxGroupInput = function(session, inputId, ..., selected = NULL) {
      got$s[[inputId]] <- selected
    }, .package = "shiny", .env = env)
  function() got$s
}

# a process as the app asks of one (processx': is_alive(), get_exit_status(), kill())
dr_proc <- function(status = 0L) {
  st <- new.env()
  st$alive <- TRUE
  st$killed <- FALSE
  list(is_alive = function() st$alive, get_exit_status = function() status,
       kill = function() {
         st$killed <- TRUE
         st$alive <- FALSE
       }, state = st)
}

# ---- the data catalog ------------------------------------------------------------

test_that("the Data tab puts the files without a row into the catalog once, and shows a dataset's first rows", {
  skip_on_cran()
  dr_home()
  dr_study("DR", c("data/adam/adsl.rds", "data/sdtm/dm.rds"))
  notes <- dr_notes()
  shiny::testServer(server_for("DR"), {
    rv <- session$userData$rv
    expect_false(any(grepl("adsl", rv$p$ard$datasets$path)))
    session$setInputs(nav = "outputs")
    expect_false(any(grepl("adsl", rv$p$ard$datasets$path)))
    session$setInputs(nav = "data")
    expect_true(all(c("data/adam/adsl.rds", "data/sdtm/dm.rds") %in% rv$p$ard$datasets$path))
    expect_true("In the data catalog now: ADSL, DM" %in% vapply(notes(), function(x) x$text, ""))
    # a row the user took away is not put back by opening the tab again
    rv$p$ard$datasets <- rv$p$ard$datasets[rv$p$ard$datasets$path != "data/sdtm/dm.rds", ,
                                           drop = FALSE]
    session$setInputs(nav = "outputs")
    session$setInputs(nav = "data")
    expect_false("data/sdtm/dm.rds" %in% rv$p$ard$datasets$path)
    # no dataset chosen: a hint; one chosen: its size and first rows
    expect_match(output$data_dim$html, "Click a dataset in the list", fixed = TRUE)
    session$setInputs(hot_ard_datasets_select = list(select = list(r = 1L, c = 1L)))
    expect_match(output$data_dim$html, "3 rows x 2 columns", fixed = TRUE)
    expect_match(output$data_head, "USUBJID", fixed = TRUE)
    # the list drawn again without that row: the choice is left alone
    session$setInputs(hot_ard_datasets_select = list(select = list(r = 9L, c = 1L)))
    expect_error(output$data_head, class = "shiny.silent.error")
    expect_error(output$data_dim, class = "shiny.silent.error")
    # a row whose file is gone
    unlink(file.path(rv$study$path, "data/adam/adsl.rds"))
    session$setInputs(hot_ard_datasets_select = list(select = list(r = 1L, c = 1L)))
    expect_error(output$data_head, class = "shiny.silent.error")
  })
})

# ---- the log and the RTF ----------------------------------------------------------

test_that("the log: what to do with nothing chosen, a report not run, a run folder; the RTF download", {
  skip_on_cran()
  dr_home()
  s <- dr_study("DR")
  dr_batch(s, "20260101_100000_all", status = "OK")
  dr_notes()
  shiny::testServer(server_for("DR"), {
    session$setInputs(nav = "results")
    expect_match(output$log, "Choose a report to see what its last preview printed", fixed = TRUE)
    # no report chosen: no RTF
    expect_error(output$rtf_download, "No RTF")
    session$setInputs(status_rows_selected = 2L)
    expect_identical(output$log, "VS has not been run yet.")
    # a report built: its RTF is what the download gives
    d <- study_status(session$userData$rv$study)
    expect_identical(d$output_id[1L], "DM")
    session$setInputs(status_rows_selected = 1L)
    dir.create(dirname(d$rtf_path[1L]), recursive = TRUE, showWarnings = FALSE)
    writeLines("{rtf1 hello}", d$rtf_path[1L])
    expect_identical(readLines(output$rtf_download), "{rtf1 hello}")
    # a run folder chosen: its batch.txt and the programs it ran
    session$setInputs(batches_rows_selected = 1L)
    expect_match(output$log, "Finished : 2026-01-01 10:00:30", fixed = TRUE)
    expect_match(output$log, "programs/tfl/DM.R", fixed = TRUE)
    expect_match(output$log, "seconds", fixed = TRUE)
  })
})

# ---- the report sets --------------------------------------------------------------

test_that("the report set buttons: what they say with nothing to work on, a bad name, rename, delete, the ticks", {
  skip_on_cran()
  dr_home()
  dr_study("DR")
  notes <- dr_notes()
  ticks <- dr_ticks()
  shiny::testServer(server_for("DR"), {
    rv <- session$userData$rv
    last <- function() notes()[[length(notes())]]
    session$setInputs(nav = "runs", batch_reports = character(), batch_save = 1)
    expect_identical(last()$text, "Tick the reports of the report set first.")
    expect_identical(last()$type, "warning")
    session$setInputs(batch_rename = 1)
    expect_identical(last()$text, "Choose a report set first.")
    session$setInputs(batch_delete = 1)
    expect_identical(last()$text, "Choose a report set first.")
    # a name the sets cannot carry: said, and no set made
    n <- length(notes())
    suppressMessages(session$setInputs(batch_reports = c("DM", "VS"), batch_save_name = "Top/line",
                                       batch_save_name_ok = 1))
    expect_identical(last()$type, "error")
    expect_gt(length(notes()), n)
    expect_length(batch_sets(rv$p), 0L)
    session$setInputs(batch_save_name = "  Topline ", batch_save_name_ok = 2)
    expect_identical(batch_sets(rv$p), list(Topline = c("DM", "VS")))
    expect_match(last()$text, "The report set Topline: 2 reports", fixed = TRUE)
    # choosing a set ticks its reports, the full run every report
    session$setInputs(batch_set = "Topline")
    expect_identical(ticks()$batch_reports, c("DM", "VS"))
    expect_identical(output$batch_reports_n, "Reports in the run: 2 of 3 (1 left out)")
    session$setInputs(batch_reports = "DM")
    expect_match(output$batch_reports_n, "(not the report set Topline any more: the run has no name)",
                 fixed = TRUE)
    session$setInputs(batch_set = ".all", batch_reports = c("DM", "VS", "L1"))
    expect_identical(ticks()$batch_reports, c("DM", "VS", "L1"))
    expect_identical(output$batch_reports_n, "Reports in the run: all 3")
    # the buttons of the list: the shown reports ticked or unticked, all, none
    session$setInputs(batch_shown = list(mode = "remove", ids = list("VS", "L1"), n = 2))
    expect_identical(ticks()$batch_reports, "DM")
    session$setInputs(batch_none = 1)
    expect_length(ticks()$batch_reports, 0L)
    session$setInputs(batch_all = 1)
    expect_identical(ticks()$batch_reports, c("DM", "VS", "L1"))
    # a set renamed to the name another has: said; renamed, then deleted: its reports stay
    rv$p <- set_batch(rv$p, "Final", c("DM", "VS"))
    suppressMessages(session$setInputs(batch_set = "Topline", batch_rename_name = "Final",
                                       batch_rename_name_ok = 1))
    expect_identical(last()$type, "error")
    expect_match(last()$text, "exists already", fixed = TRUE)
    session$setInputs(batch_rename_name = "Interim", batch_rename_name_ok = 2)
    expect_setequal(names(batch_sets(rv$p)), c("Final", "Interim"))
    session$setInputs(batch_set = "Final", batch_delete_ok = 1)
    expect_identical(names(batch_sets(rv$p)), "Interim")
    expect_identical(nrow(rv$p$outputs), 3L)
    # the select lists the sets the study has
    expect_match(output$batch_reports_ui$html, "Interim", fixed = TRUE)
  })
})

# ---- the runs ---------------------------------------------------------------------

test_that("a preview run: started once, said while it goes on, how it ended; one that cannot start", {
  skip_on_cran()
  dr_home()
  dr_study("DR")
  asked <- list()
  px <- dr_proc(status = 0L)
  end_run <- function() px$state$alive <- FALSE
  fail <- FALSE
  testthat::local_mocked_bindings(run_study = function(study, output_id = NULL, wait = TRUE) {
    if (fail) stop("R is not found")
    asked[[length(asked) + 1L]] <<- list(ids = output_id, wait = wait)
    px
  })
  notes <- dr_notes()
  shiny::testServer(server_for("DR"), {
    rv <- session$userData$rv
    last <- function() notes()[[length(notes())]]
    session$setInputs(nav = "results", run_selected = 1)
    expect_identical(last()$text, "Choose a report")
    expect_length(asked, 0L)
    session$setInputs(run_all = 1)
    expect_length(asked, 1L)
    expect_null(asked[[1L]]$ids)
    expect_false(asked[[1L]]$wait)
    expect_identical(rv$job_kind, "preview")
    expect_match(output$job$html, "Running every report ...", fixed = TRUE)
    # one at a time
    session$setInputs(run_all = 2)
    expect_length(asked, 1L)
    expect_identical(last()$text, "A run is going on")
    expect_identical(last()$type, "warning")
    # it ends
    end_run()
    session$elapse(1100)
    expect_null(rv$job)
    expect_identical(last()$text, "Finished: every report")
    expect_null(output$job)
    # the reports chosen in the list: those only; this one ends with an error
    px <<- dr_proc(status = 1L)
    session$setInputs(status_rows_selected = c(1L, 3L), run_selected = 2)
    expect_identical(asked[[2L]]$ids, c("DM", "L1"))
    expect_identical(rv$job_what, "DM, L1")
    end_run()
    session$elapse(1100)
    expect_identical(last()$text, "Finished with errors: DM, L1")
    expect_identical(last()$type, "warning")
    # a run that cannot start: said, and no run
    fail <<- TRUE
    suppressMessages(session$setInputs(run_all = 3))
    expect_null(rv$job)
    expect_identical(last()$type, "error")
    expect_match(last()$text, "R is not found", fixed = TRUE)
  })
})

test_that("a preview is stopped with the session", {
  skip_on_cran()
  dr_home()
  dr_study("DR")
  px <- dr_proc()
  testthat::local_mocked_bindings(run_study = function(...) px)
  dr_notes()
  shiny::testServer(server_for("DR"), {
    session$setInputs(nav = "results", run_all = 1)
    expect_false(px$state$killed)
  })
  expect_true(px$state$killed)
})

test_that("the official run: nothing ticked is refused; reports left out; a report set; what it reads first", {
  skip_on_cran()
  dr_home()
  dr_study("DR", ard = TRUE)
  asked <- list()
  warn <- list(newer = character(), confirm = character())
  testthat::local_mocked_bindings(
    run_batch = function(study, parts, code = TRUE, data = FALSE, exclude = NULL, batch = NULL,
                         wait = TRUE, ...) {
      asked[[length(asked) + 1L]] <<- list(parts = parts, code = code, data = data,
                                           exclude = exclude, batch = batch, wait = wait)
      dr_proc()
    },
    .ai_run_warnings = function(p, ids) warn)
  notes <- dr_notes()
  modals <- dr_modals()
  shiny::testServer(server_for("DR"), {
    rv <- session$userData$rv
    last <- function() notes()[[length(notes())]]
    # nothing ticked
    session$setInputs(nav = "runs", batch_parts = "tfl", batch_code = FALSE, batch_data = FALSE,
                      batch_reports = character(), batch_run = 1)
    expect_identical(last()$text, "Choose at least one report for the run.")
    expect_length(asked, 0L)
    # a report unticked is left out
    session$setInputs(batch_reports = c("DM", "VS"), batch_run = 2)
    expect_length(asked, 1L)
    expect_identical(asked[[1L]]$parts, "tfl")
    expect_identical(asked[[1L]]$exclude, "L1")
    expect_null(asked[[1L]]$batch)
    expect_false(asked[[1L]]$code)
    expect_identical(rv$job_kind, "official")
    expect_match(rv$job_what, "^the official run: Reports +[(]1 left out[)]$")
    # one at a time
    session$setInputs(batch_run = 3)
    expect_length(asked, 1L)
    expect_identical(last()$text, "A run is going on")
    # a report set, when the ticks are its reports
    rv$job <- NULL
    rv$p <- set_batch(rv$p, "Topline", c("DM", "VS"))
    session$setInputs(batch_set = "Topline", batch_run = 4)
    expect_length(asked, 2L)
    expect_identical(asked[[2L]]$batch, "Topline")
    expect_null(asked[[2L]]$exclude)
    expect_match(rv$job_what, "(the report set Topline)", fixed = TRUE)
    # a run that leaves out a report another reads: said first, Cancel, then Run anyway
    rv$job <- NULL
    rv$p <- set_step_code(rv$p, "L1", "data",
      "data <- readRDS(file.path(path_ard, \"ard.rds\")) |> filter(output_id == \"DM\")")
    session$setInputs(batch_set = ".all", batch_parts = "all", batch_reports = c("VS", "L1"),
                      batch_run = 5)
    expect_length(asked, 2L)
    expect_match(modals()[1L], "A report the run makes reads an ARD it leaves out", fixed = TRUE)
    expect_match(modals()[1L], "L1 reads the ARD of DM", fixed = TRUE)
    session$setInputs(batch_need_cancel = 1)
    expect_length(asked, 2L)
    session$setInputs(batch_need_go = 1)
    expect_length(asked, 3L)
    expect_identical(asked[[3L]]$exclude, "DM")
    expect_match(rv$job_what, "^the official run: ARD, Reports +[(]1 left out[)]$")
    # the AI's reports with a changed draft or places to confirm: said first
    rv$job <- NULL
    warn <<- list(newer = "DM", confirm = c("VS", "L1"))
    session$setInputs(batch_parts = "tfl", batch_reports = c("DM", "VS", "L1"), batch_run = 6)
    expect_length(asked, 3L)
    expect_match(modals()[2L], "Their draft changed after they were adopted .*: DM\\.")
    expect_match(modals()[2L], "places the AI left to confirm .*: VS, L1\\.")
    session$setInputs(batch_ai_cancel = 1)
    expect_length(asked, 3L)
    session$setInputs(batch_ai_go = 1)
    expect_length(asked, 4L)
    expect_length(asked[[4L]]$exclude, 0L)
  })
})

test_that("an official run that ends is said from its run folder: how many programs, how long", {
  skip_on_cran()
  dr_home()
  s <- dr_study("DR")
  px <- dr_proc()
  end_run <- function() px$state$alive <- FALSE
  status <- "OK"
  folder <- "20260102_100000_report"
  testthat::local_mocked_bindings(run_batch = function(...) {
    dr_batch(s, folder, status = status)
    px
  })
  notes <- dr_notes()
  shiny::testServer(server_for("DR"), {
    rv <- session$userData$rv
    last <- function() notes()[[length(notes())]]
    session$setInputs(nav = "runs", batch_parts = "tfl", batch_run = 1)
    expect_identical(rv$job_batches_before, character())
    end_run()
    session$elapse(1100)
    expect_identical(last()$text,
                     "The official run is done: 1 of 1 programs without error (0:30, 20260102_100000_report).")
    expect_identical(last()$type, "message")
    expect_null(rv$job_batches_before)
    expect_null(rv$job)
    # one that ended with errors is a warning
    px <<- dr_proc(status = 1L)
    status <<- "ERROR"
    folder <<- "20260103_100000_report"
    session$setInputs(batch_run = 2)
    end_run()
    session$elapse(1100)
    expect_identical(last()$text,
                     "The official run ended with errors: 1 of 1 programs failed (0:30, 20260103_100000_report).")
    expect_identical(last()$type, "warning")
  })
})

# ---- the run folders -----------------------------------------------------------------

# three run folders as the official runs leave them: two alike, one different
dr_runs <- function(s) {
  d <- file.path(s$path, "runs")
  mk <- function(b, ard, rtf_time) {
    dir.create(file.path(d, b, "output", "ard"), recursive = TRUE)
    dir.create(file.path(d, b, "output", "tfl"), recursive = TRUE)
    saveRDS(ard, file.path(d, b, "output", "ard", "DM.rds"))
    writeLines(c("{rtf1", paste("Generated on:", rtf_time), "}"),
               file.path(d, b, "output", "tfl", "DM.rtf"))
    dr_batch(s, b)
  }
  mk("20260101_100000_all", 1:3, "01Jan2026 10:00")
  mk("20260102_100000_all", 1:3, "02Jan2026 11:00")
  mk("20260103_100000_all", 1:4, "03Jan2026 09:00")
  invisible(d)
}

test_that("comparing runs needs two; the table lists what changed, or everything; the CSV has it", {
  skip_on_cran()
  dr_home()
  s <- dr_study("DR")
  dr_batch(s, "20251231_100000_all")
  notes <- dr_notes()
  modals <- dr_modals()
  shiny::testServer(server_for("DR"), {
    session$setInputs(nav = "results", batch_compare = 1)
    expect_identical(notes()[[1L]]$text, "Two official runs are needed to compare.")
    expect_length(modals(), 0L)
  })
  dr_runs(s)
  dr_notes()
  modals <- dr_modals()
  tab <- dr_tables()
  shiny::testServer(server_for("DR"), {
    session$setInputs(nav = "results", batches_rows_selected = 1L, batch_compare = 1)
    # the newest run chosen, against the one before it: both in the dialog's lists
    expect_match(modals()[1L], "Compare official runs", fixed = TRUE)
    expect_match(modals()[1L], "20260102_100000_all", fixed = TRUE)
    session$setInputs(cmp_batch = "20260103_100000_all", cmp_against = "20260102_100000_all",
                      cmp_all = FALSE)
    expect_match(output$cmp_summary$html, "Results changed:", fixed = TRUE)
    expect_match(output$cmp_summary$html, "1 of 2: DM.rds", fixed = TRUE)
    output$cmp_table
    expect_identical(tab()$File, "output/ard/DM.rds")
    expect_identical(tab()$Compared, "different")
    session$setInputs(cmp_all = TRUE)
    output$cmp_table
    expect_true("same but the time" %in% tab()$Compared)
    expect_true("output/tfl/DM.rtf" %in% tab()$File)
    # a run that is not there: the reason, in place of the table
    session$setInputs(cmp_batch = "20260109_100000_all")
    expect_match(output$cmp_summary$html, "text-danger", fixed = TRUE)
    expect_match(output$cmp_summary$html, "No batch folder", fixed = TRUE)
    expect_error(output$cmp_table, class = "shiny.silent.error")
    session$setInputs(cmp_batch = "20260103_100000_all")
    csv <- utils::read.csv(output$cmp_csv)
    expect_identical(names(csv), c("kind", "file", "result"))
    expect_identical(csv$result[csv$file == "output/ard/DM.rds"], "different")
    # two runs that are the same one: nothing to compare
    session$setInputs(cmp_against = "20260103_100000_all")
    expect_match(output$cmp_summary$html, "Choose two different runs", fixed = TRUE)
    expect_identical(nrow(utils::read.csv(output$cmp_csv)), 0L)
  })
})

test_that("the runs table: state, what was left out (the first few); folders opened by the system", {
  skip_on_cran()
  dr_home()
  s <- dr_study("DR")
  dr_batch(s, "20260101_100000_all", status = "ERROR",
           excluded = paste0("programs/ard/", c("A", "B", "C", "D", "E"), ".R", collapse = ", "))
  dr_batch(s, "20260102_100000_report", excluded = "programs/tfl/L1.R")
  dir.create(file.path(s$path, "runs", "20260103_100000_ard"))
  opened <- character()
  withr::local_options(tflplanner.open_folder = function(p) opened <<- c(opened, p))
  testthat::local_mocked_bindings(.is_local_host = function(host) TRUE)
  notes <- dr_notes()
  tab <- dr_tables()
  shiny::testServer(server_for("DR"), {
    session$setInputs(nav = "results")
    output$batches
    d <- tab()
    expect_match(d[[1L]][1L], "20260103_100000_ard", fixed = TRUE)
    expect_match(d[["State"]][2L], "Done .*: 1 of 1 without error")
    expect_match(d[["State"]][3L], "1 errors (of 1)", fixed = TRUE)
    expect_identical(d[["Left out"]], c("", "L1.R", "A.R, B.R, C.R, ... (5 programs)"))
    expect_match(d[[1L]][1L], "Open its folder", fixed = TRUE)
    expect_match(paste(output$batch_open_ui$html, collapse = ""), "Open the run folder", fixed = TRUE)
    # a run folder's icon, the runs folder, the folder of the run chosen, the others
    session$setInputs(batch_open_row = "20260101_100000_all")
    expect_identical(basename(opened), "20260101_100000_all")
    expect_match(notes()[[1L]]$text, "Opened the folder 20260101_100000_all", fixed = TRUE)
    session$setInputs(batch_open_row = "no-such-run")
    expect_length(opened, 1L)
    session$setInputs(batch_open = 1)
    expect_identical(basename(opened[2L]), "runs")
    session$setInputs(batches_rows_selected = 3L, batch_open = 2)
    expect_identical(basename(opened[3L]), "20260101_100000_all")
    session$setInputs(tfl_open = 1)
    expect_identical(basename(opened[4L]), "tfl")
    session$setInputs(data_open = 1)
    expect_identical(basename(opened[5L]), "adam")
  })
})

test_that("on a server the run folders are not opened", {
  skip_on_cran()
  dr_home()
  s <- dr_study("DR")
  dr_batch(s, "20260101_100000_all")
  opened <- character()
  withr::local_options(tflplanner.open_folder = function(p) opened <<- c(opened, p))
  testthat::local_mocked_bindings(.is_local_host = function(host) FALSE)
  dr_notes()
  shiny::testServer(server_for("DR"), {
    session$setInputs(nav = "results", batch_open_row = "20260101_100000_all", batch_open = 1)
    expect_length(opened, 0L)
    expect_null(output$batch_open_ui)
  })
})
