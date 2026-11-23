# Leaving reports out of an official run, and named batches (#299 D6): a
# report's batches are the report list's `batches` column; programs/batch.R
# carries them as .batch_sets; the runner takes --batch <name> and
# --exclude=<program>.

bs_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# two reports with an ARD each, and a listing (no ARD program)
bs_planner <- function(ids = c("DM", "VS")) {
  p <- add_output(new_planner(), ids[1])
  p <- add_output(p, ids[2])
  p <- add_output(p, "L1", type = "listing", at = "end")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = ids, analysis_id = c("AGE", "AGE"),
    method = "continuous", population_id = "SAF", by = "TRT01A",
    variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("a named batch is the reports that say they belong to it", {
  p <- bs_planner()
  expect_identical(batch_sets(p), stats::setNames(list(), character()))
  p <- set_batch(p, "Topline", "DM")
  p <- set_batch(p, "Final", c("DM", "VS", "L1"))
  # a report in two batches
  expect_identical(p$outputs$batches[p$outputs$output_id == "DM"], "Topline | Final")
  expect_identical(batch_sets(p), list(Topline = "DM", Final = c("DM", "VS", "L1")))
  # overwritten: the name is these reports, and no other
  p <- set_batch(p, "Topline", c("VS", "L1"))
  expect_identical(batch_sets(p)$Topline, c("VS", "L1"))
  expect_identical(p$outputs$batches[p$outputs$output_id == "DM"], "Final")
  # renamed, removed (the reports stay)
  p <- rename_batch(p, "Topline", "Interim Analysis")
  expect_identical(names(batch_sets(p)), c("Final", "Interim Analysis"))
  expect_error(rename_batch(p, "Final", "Interim Analysis"), "exists already")
  p <- remove_batch(p, "Final")
  expect_identical(names(batch_sets(p)), "Interim Analysis")
  expect_identical(nrow(p$outputs), 3L)
  # what a batch cannot be
  expect_error(set_batch(p, "Topline", "AE"), "No report")
  expect_error(set_batch(p, "", "DM"), "Give the batch a name")
  expect_error(set_batch(p, "Top/line", "DM"), "letters, digits")
  expect_equal(nrow(batch_set_problems(p)), 0L)
  p$outputs$batches[1] <- "Top/line"
  expect_match(batch_set_problems(p)$problem, "cannot carry")
})

test_that("left out of a run: a report's ARD and report programs; who reads them", {
  p <- bs_planner()
  expect_setequal(.batch_exclude(p, "DM"), c("DM.R", report_info(p, "DM")$program))
  expect_identical(.batch_exclude(p, "other.R"), "other.R")
})

test_that("an official run of a named batch, or leaving reports out", {
  skip_on_cran()
  bs_home()
  p <- set_batch(bs_planner(), "Topline", c("DM", "L1"))
  p <- set_batch(p, "Final", c("DM", "VS", "L1"))
  s <- create_study("B1", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  o <- open_study("B1")
  code <- readLines(file.path(s$path, "programs", "batch.R"))
  expect_true(any(grepl("^\\.batch_sets <- list\\(", code)))
  expect_true(any(grepl("`Topline` = c(\"DM\", \"L1\")", code, fixed = TRUE)))

  # the ARD of the named batch: its folder and batch.txt name it
  b <- run_batch(o, "ard", batch = "Topline")
  expect_true(b$ok)
  expect_match(basename(b$batch), "_ard_Topline$")
  expect_identical(basename(b$result$program), "DM.R")
  txt <- readLines(file.path(b$batch, "batch.txt"))
  expect_true("Batch set : Topline" %in% txt)
  # what the batch left out (VS is not in Topline) is in the record too
  expect_true(any(grepl("^Excluded : .*VS\\.R", txt)))
  lb <- list_batches(o)
  expect_identical(lb$what[1], "ard")
  expect_identical(lb$set[1], "Topline")
  Sys.sleep(1.1)

  # left out: VS's ARD program does not run, and the record says so
  b2 <- run_batch(o, "ard", exclude = "VS")
  expect_identical(basename(b2$result$program), "DM.R")
  expect_true(any(grepl("^Excluded : .*VS\\.R", readLines(file.path(b2$batch, "batch.txt")))))
  expect_match(list_batches(o)$excluded[1], "VS.R", fixed = TRUE)
  Sys.sleep(1.1)

  # a batch with a report the programs do not have (batch.R older than the
  # list): said, and the rest runs
  bf <- file.path(s$path, "programs", "batch.R")
  code <- readLines(bf)
  code <- sub("`Topline` = c(\"DM\", \"L1\")", "`Topline` = c(\"DM\", \"L1\", \"GONE\")",
              code, fixed = TRUE)
  writeLines(code, bf)
  b3 <- run_batch(o, "ard", batch = "Topline")
  expect_true(b3$ok)
  expect_true(any(grepl("no program for GONE", b3$output, fixed = TRUE)))

  # a name the study does not have
  expect_error(run_batch(o, "ard", batch = "Nope"), "No batch named")
})

test_that("the runner takes an output id as a program name, and --exclude too", {
  skip_on_cran()
  bs_home()
  # DM's report has a program name of its own (t_dm), so the id is not a file name
  p <- bs_planner(c("DM", "VS"))
  p$sheets$report <- rbind(p$sheets$report, NA)
  p$sheets$report[nrow(p$sheets$report), c("output_id", "program")] <- c("DM", "t_dm")
  s <- create_study("BN", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  o <- open_study("BN")
  progs <- function(b) basename(b$result$program)
  expect_identical(report_info(o$planner, "DM")$program, "t_dm.R")

  # an id: the ARD program (DM.R) and the report program (t_dm.R) of the report
  b <- run_batch(o, c("ard", "tfl"), only = "DM")
  expect_identical(progs(b), c("DM.R", "t_dm.R"))
  Sys.sleep(1.1)
  # a file name still works, with or without .R (an official run makes the
  # ARD first, so the ARD program is named too)
  expect_identical(progs(run_batch(o, c("ard", "tfl"), only = c("DM.R", "t_dm"))),
                   c("DM.R", "t_dm.R"))
  Sys.sleep(1.1)
  expect_identical(progs(run_batch(o, "ard", only = "VS.R")), "VS.R")
  Sys.sleep(1.1)
  # the runner's own rule: Rscript programs/autoexec_all.R --exclude=<id>
  old <- setwd(s$path)
  out <- system2(file.path(R.home("bin"), "Rscript"),
                 c("programs/autoexec_all.R", "--exclude=DM"),
                 stdout = TRUE, stderr = TRUE)
  setwd(old)
  left <- grep("^Left out: ", out, value = TRUE)
  expect_length(left, 1L)
  expect_match(left, "DM.R", fixed = TRUE)
  expect_match(left, "t_dm.R", fixed = TRUE)
  Sys.sleep(1.1)
  # a name that matches nothing (a typing slip) stops, naming it, in --only
  # and in --exclude alike
  b <- run_batch(o, "ard", only = "NOPE")
  expect_false(b$ok)
  expect_true(any(grepl("The name 'NOPE' stands for no output.", b$output, fixed = TRUE)))
  Sys.sleep(1.1)
  b <- run_batch(o, c("ard", "tfl"), exclude = "DMX")
  expect_false(b$ok)
  expect_true(any(grepl("The name 'DMX' stands for no output.", b$output, fixed = TRUE)))
})

test_that("a report with no program of its own does not break batch.R", {
  skip_on_cran()
  p <- bs_planner(c("DM", "VS"))
  # VS's figure reads an ARD with no source, and it has no program
  reads <- data.frame(report = c("VS", "ZZ"), reads = c(NA, NA), kind = "source",
                      reason = c("no source", "no source"), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(.batch_report_reads = function(x) reads)
  code <- batch_code(p)
  expect_no_error(parse(text = code))
  expect_false(any(grepl("`NA`", code, fixed = TRUE)))
})

test_that("the batches column survives the SPEC round trip", {
  skip_on_cran()
  p <- set_batch(bs_planner(), "Topline", c("DM", "L1"))
  dir <- withr_tempdir()
  paths <- write_planner(p, dir)
  q <- read_planner(paths)
  expect_identical(batch_sets(q), batch_sets(p))
})

test_that("the report list shows a report's report sets; they are made on the Runs tab", {
  skip_on_cran()
  bs_home()
  p <- set_batch(bs_planner(), "Topline", "DM")
  create_study("B2", planner = p)
  # one report's names: checked, " | " between them, none NA
  q <- .set_report_batches(p, "VS", c("Final", " Topline ", ""))
  expect_identical(q$outputs$batches[q$outputs$output_id == "VS"], "Final | Topline")
  expect_identical(batch_sets(q)$Topline, c("DM", "VS"))
  expect_true(is.na(.set_report_batches(q, "VS", character())$outputs$batches[2]))
  expect_error(.set_report_batches(p, "VS", "Top/line"), "letters, digits")
  shiny::testServer(server_for("B2"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", target = "VS")
    # the list's column (the table is drawn server side: its data here)
    expect_match(paste(output$outputs, collapse = ""), "Report sets", fixed = TRUE)
    # the Runs tab makes a set: the shown reports ticked together (a filter),
    # saved under a name
    expect_setequal(.batch_ticks("DM", "VS", "add"), c("DM", "VS"))
    expect_identical(.batch_ticks(c("DM", "VS"), "DM", "remove"), "VS")
    session$setInputs(batch_shown = list(mode = "add", ids = list("VS"), n = 1))
    session$setInputs(batch_reports = "VS")
    session$setInputs(batch_save_name = "Interim", batch_save_name_ok = 1)
    expect_identical(batch_sets(rv$p)$Interim, "VS")
  })
})

test_that("the data each program reads, by the data catalog", {
  p <- bs_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE"), path = c("data/adam/adsl.rds", "data/adam/adae.rds")),
    "datasets")
  p$lf$listings <- data.frame(output_id = "L1", dataset = "ADAE")
  p <- use_imported_ard(p, "VS", "vs_cro.rds")
  d <- .batch_data_files(p)
  expect_identical(d[["programs/ard/DM.R"]], "data/adam/adsl.rds")
  # a listing reads its dataset; a table's report program the study ARD
  # (kept anyway), or the ARD taken in it reads
  expect_identical(d[["programs/tfl/L1.R"]], "data/adam/adae.rds")
  expect_length(d[["programs/tfl/DM.R"]], 0L)
  expect_identical(d[["programs/tfl/VS.R"]], "input/ard/vs_cro.rds")
  # batch.R carries them
  code <- batch_code(p)
  expect_true(any(startsWith(code, ".batch_data <- list(")))
  expect_true(any(grepl("`programs/tfl/L1.R` = c(\"data/adam/adae.rds\")", code, fixed = TRUE)))
})

test_that("an official run keeps the data its programs read, and the versions", {
  skip_on_cran()
  bs_home()
  s <- create_study("BD", planner = bs_planner())
  adsl <- file.path(s$path, "data", "adam", "adsl.rds")
  saveRDS(cards::ADSL, adsl)
  o <- open_study("BD")
  # what would be kept, before the run
  plan <- batch_data_plan(o, "ard")
  expect_identical(plan$path, "data/adam/adsl.rds")
  expect_identical(plan$size, file.size(adsl))
  expect_identical(plan$programs, "DM.R, VS.R")
  expect_identical(nrow(batch_data_plan(o, "tfl")), 0L)

  # off by default: no data, but the R and package versions
  b <- run_batch(o, "ard")
  expect_true(b$ok)
  expect_false(dir.exists(file.path(b$batch, "data")))
  txt <- readLines(file.path(b$batch, "batch.txt"))
  expect_true("Data     : not kept" %in% txt)
  si <- readLines(file.path(b$batch, "session_info.txt"))
  expect_true(any(grepl("^cards +[0-9]", si)))
  # sessionInfo()'s first line ("R version 4.6.1 ...", "R Under development ...")
  expect_true(R.version.string %in% si)
  expect_false(list_batches(o)$data[1])
  # its state: done, when, every program without error
  lb <- list_batches(o)
  expect_identical(lb$state[1], "done")
  expect_match(lb$finished[1], "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9:]{8}$")
  expect_match(.batch_state_text(lb[1, ]), "^Done [0-9]{2}:[0-9]{2}: 2 of 2 without error$")
  # a folder with no record of its end: not finished (the app says running
  # while its run goes on)
  dir.create(file.path(s$path, "runs", "20200101_000000_ard"))
  lb <- list_batches(o)
  expect_identical(lb$state[lb$batch == "20200101_000000_ard"], "incomplete")
  lb$state[lb$batch == "20200101_000000_ard"] <- "running"
  expect_identical(.batch_state_text(lb[lb$batch == "20200101_000000_ard", ],
                                     now = as.POSIXct("2020-01-01 00:01:05")),
                   "Running 1:05")
  unlink(file.path(s$path, "runs", "20200101_000000_ard"), recursive = TRUE)
  Sys.sleep(1.1)

  # kept: under its path in the study folder, its md5 in the manifest; the
  # study's renv.lock with the versions
  writeLines("{}", file.path(s$path, "renv.lock"))
  b2 <- run_batch(o, "ard", data = TRUE)
  expect_true(b2$ok)
  expect_true(file.exists(file.path(b2$batch, "data", "adam", "adsl.rds")))
  man <- utils::read.csv(file.path(b2$batch, "data_manifest.csv"), colClasses = "character")
  expect_identical(man$path, "data/adam/adsl.rds")
  expect_identical(man$md5, unname(tools::md5sum(adsl)))
  expect_identical(as.numeric(man$size), file.size(adsl))
  txt <- readLines(file.path(b2$batch, "batch.txt"))
  expect_match(grep("^Data", txt, value = TRUE), "kept (data/, 1 file(s)", fixed = TRUE)
  expect_true(file.exists(file.path(b2$batch, "renv.lock")))
  expect_true(list_batches(o)$data[1])

  # a batch.R written before: said, not run
  bf <- file.path(s$path, "programs", "batch.R")
  writeLines(gsub("--data", "--d-a-t-a", readLines(bf), fixed = TRUE), bf)
  expect_error(run_batch(o, "ard", data = TRUE), "keep the data")
})

test_that("the Runs tab says the data a run would keep, and warns when large", {
  skip_on_cran()
  bs_home()
  s <- create_study("BS", planner = bs_planner())
  saveRDS(data.frame(x = 1:10), file.path(s$path, "data", "adam", "adsl.rds"))
  old <- options(tflplanner.batch_data_warn_mb = 0)
  on.exit(options(old), add = TRUE)
  shiny::testServer(server_for("BS"), {
    session$setInputs(nav = "runs", batch_parts = "ard", batch_data = FALSE)
    expect_null(output$batch_data_size)
    session$setInputs(batch_data = TRUE)
    txt <- paste(output$batch_data_size$html, collapse = "")
    expect_match(txt, "Data kept: 1 files", fixed = TRUE)
    # over the limit (0 MB here): said
    expect_match(txt, "every run keeps its own copy", fixed = TRUE)
  })
  expect_identical(.mb_text(0.04), "41 KB")
  expect_identical(.mb_text(1.44), "1.4 MB")
  expect_identical(.mb_text(12.3), "12 MB")
  expect_identical(.mb_text(2048), "2.0 GB")
})

test_that("a time taken, and a failed run's state", {
  expect_identical(.elapsed_text(42), "0:42")
  expect_identical(.elapsed_text(725), "12:05")
  expect_identical(.elapsed_text(3729), "1:02:09")
  d <- data.frame(state = "failed", started = "2026-10-12 08:00:00",
                  finished = "2026-10-12 08:02:00", programs = 13L, errors = 2L)
  expect_identical(.batch_state_text(d), "2 errors (of 13), 08:02")
})

test_that("a batch folder is opened by the system's opener", {
  got <- NULL
  old <- options(tflplanner.open_folder = function(p) got <<- p)
  on.exit(options(old), add = TRUE)
  d <- withr_tempdir()
  expect_true(.open_folder(file.path(d, "runs", "20261012_000000_ard")))
  expect_identical(got, normalizePath(file.path(d, "runs", "20261012_000000_ard")))
})

# two batch folders as official runs leave them
cmp_runs <- function(d) {
  mk <- function(b, rtf, ard, data_md5 = NULL, code = NULL) {
    dir.create(file.path(d, b, "output", "tfl"), recursive = TRUE)
    dir.create(file.path(d, b, "output", "ard"), recursive = TRUE)
    writeLines(rtf, file.path(d, b, "output", "tfl", "T-1.rtf"))
    saveRDS(ard, file.path(d, b, "output", "ard", "T-1.rds"))
    writeLines(c("part,program,status", "tfl,programs/tfl/T-1.R,OK"),
               file.path(d, b, "run.csv"))
    writeLines("Finished : 2026-01-01 10:00:00", file.path(d, b, "batch.txt"))
    if (!is.null(data_md5)) {
      utils::write.csv(data.frame(path = names(data_md5), md5 = unname(data_md5)),
                       file.path(d, b, "data_manifest.csv"), row.names = FALSE)
    }
    # the folder is the code: snapshot.csv has the md5 of the files it copied
    if (length(code)) {
      md5 <- vapply(code, function(txt) {
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(txt, f)
        unname(tools::md5sum(f))
      }, "")
      utils::write.csv(data.frame(path = names(code), size = 1, md5 = unname(md5)),
                       file.path(d, b, "snapshot.csv"), row.names = FALSE)
    }
  }
  mk("20260101_100000_all", c("{\\rtf1", "Generated on: 01Jan2026  10:00", "}"), 1:3,
     c("data/adam/adsl.rds" = "aaa", "data/adam/adae.rds" = "bbb"),
     list("programs/tfl/T-1.R" = "x <- 1", "programs/tfl/T-2.R" = "y <- 2"))
  mk("20260102_100000_all", c("{\\rtf1", "Generated on: 02Jan2026  11:30", "}"), 1:4,
     c("data/adam/adsl.rds" = "aaa", "data/adam/adae.rds" = "ccc"),
     list("programs/tfl/T-1.R" = "x <- 1", "programs/tfl/T-2.R" = "y <- 3"))
  # a run of the reports alone, keeping nothing
  mk("20260103_100000_report", c("{\\rtf1", "Generated on: 03Jan2026  09:00", "}"), 1:4)
}

test_that("two official runs compared: their inputs where kept, their results", {
  bs_home()
  s <- create_study("CMP", planner = add_output(new_planner(), "T-1"))
  cmp_runs(file.path(s$path, "runs"))
  # a run that stopped in between (no record of its end): not compared with
  dir.create(file.path(s$path, "runs", "20260101_120000_all", "output"), recursive = TRUE)
  r <- compare_batches(s, "20260102_100000_all")
  # the run before of the same kind
  expect_identical(attr(r, "batches")[["against"]], "20260101_100000_all")
  res <- stats::setNames(r$result, paste(r$kind, r$file))
  expect_identical(res[["data data/adam/adae.rds"]], "changed")
  expect_false("data data/adam/adsl.rds" %in% names(res))
  expect_identical(res[["code programs/tfl/T-2.R"]], "changed")
  expect_identical(res[["output output/tfl/T-1.rtf"]], "same but the time")
  expect_identical(res[["output output/ard/T-1.rds"]], "different")
  expect_identical(attr(r, "changed")$inputs, c("data/adam/adae.rds", "programs/tfl/T-2.R"))
  expect_identical(attr(r, "changed")$outputs, "output/ard/T-1.rds")
  # a run that kept no data nor code: its inputs are not compared
  r2 <- compare_batches(s, "20260103_100000_report", "20260102_100000_all")
  expect_identical(r2$result[r2$kind %in% c("data", "code")], c("not kept", "not kept"))
  expect_identical(r2$result[r2$file == "output/ard/T-1.rds"], "identical")
  # no run of its kind before it: the one before
  expect_identical(.batch_previous(list_batches(s), "20260103_100000_report"),
                   "20260102_100000_all")
  expect_error(compare_batches(s, "20260101_100000_all"), "No earlier official run")
  expect_identical(.batch_compare_file(tempfile(), file.path(s$path, "runs", "20260101_100000_all",
                                                             "run.csv")), "new")

  # the Runs tab: the dialog says what changed, the table only that
  shiny::testServer(server_for("CMP"), {
    session$setInputs(nav = "results", batch_compare = 1)
    session$setInputs(cmp_batch = "20260102_100000_all", cmp_against = "20260101_100000_all",
                      cmp_all = FALSE)
    txt <- paste(output$cmp_summary$html, collapse = "")
    expect_match(txt, "adae.rds, T-2.R", fixed = TRUE)
    expect_match(txt, "1 of 2: T-1.rds", fixed = TRUE)
    session$setInputs(cmp_against = "20260102_100000_all")
    expect_match(paste(output$cmp_summary$html, collapse = ""), "Choose two different runs",
                 fixed = TRUE)
  })
})

test_that("two official runs of the sample compared after a data fix", {
  skip_on_cran()
  home <- withr_tempdir()
  withr::local_options(tflplanner.home = home)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(sample_study_copy())
  b1 <- run_batch(s, c("ard", "tfl"), data = TRUE, only = "T-14-1-1")
  expect_true(file.exists(file.path(b1$batch, "programs", "study_helpers.R")))
  expect_true(file.exists(file.path(b1$batch, "output", "tfl", "report_status.csv")))
  Sys.sleep(1.1)
  # as it is: nothing changed
  b2 <- run_batch(s, c("ard", "tfl"), data = TRUE, only = "T-14-1-1")
  r <- compare_batches(s, basename(b2$batch))
  expect_length(attr(r, "changed")$inputs, 0L)
  expect_length(attr(r, "changed")$outputs, 0L)
  Sys.sleep(1.1)
  # a data fix: that input, and the results that read it
  adsl <- file.path(s$path, "data", "adam", "adsl.rds")
  saveRDS(readRDS(adsl)[-1, ], adsl)
  b3 <- run_batch(s, c("ard", "tfl"), data = TRUE, only = "T-14-1-1")
  r3 <- compare_batches(s, basename(b3$batch))
  expect_identical(attr(r3, "changed")$inputs, "data/adam/adsl.rds")
  # (the study ARD; a table's data is not saved per report, #405)
  expect_true("output/ard/ard.rds" %in% attr(r3, "changed")$outputs)
  expect_false("output/ard/T-14-1-1.rds" %in% c(r3$file, attr(r3, "changed")$outputs))
})

test_that("the runs list of a study with no run yet is drawn (no run, no row)", {
  skip_on_cran()
  bs_home()
  create_study("B0", planner = bs_planner())
  shiny::testServer(server_for("B0"), {
    session$setInputs(nav = "outputs", target = "VS")
    expect_no_error(output$batches)
  })
})

test_that("the Runs tab has no preview buttons: an official run is the one way to run", {
  html <- as.character(app_ui("en"))
  expect_false(grepl("id=\"run_selected\"", html, fixed = TRUE))
  expect_false(grepl("id=\"run_all\"", html, fixed = TRUE))
  expect_false(grepl("Preview selected", html, fixed = TRUE))
  expect_true(grepl("id=\"batch_run\"", html, fixed = TRUE))
})

test_that("only declared reads are needs: a quoted id in a report's code is not", {
  p <- bs_planner()
  # L1's code names DM's rows of the study ARD: not found out at the start
  p <- set_step_code(p, "L1", "data",
    "data <- readRDS(file.path(path_ard, \"ard.rds\")) |> filter(output_id == \"DM\")")
  n <- .batch_report_needs(p)
  # a report's own ARD (it has analyses); a listing with none reads none
  expect_identical(n, list(DM = "DM", VS = "VS"))
  expect_false("L1" %in% names(n))
  # a selected L1 without DM is not refused
  expect_identical(nrow(.batch_missing_needs(n, c("L1", "VS"))), 0L)
})

test_that("batch.R keeps the study's own analysis functions (key source) with the ARD's code", {
  skip_on_cran()
  bs_home()
  p <- bs_planner()
  # a fictional function file, named by the study key `source`
  p$ard$study <- rbind(p$ard$study, data.frame(
    key = "source", value = "programs/ard/functions/ard_mine.R"))
  s <- create_study("BSRC", planner = p)
  fdir <- file.path(s$path, "programs", "ard", "functions")
  dir.create(fdir, recursive = TRUE, showWarnings = FALSE)
  writeLines("ard_mine <- function(data, ...) NULL", file.path(fdir, "ard_mine.R"))
  # read back from study.yml
  o <- open_study("BSRC")
  expect_identical(.split_bar(.study_value(o$planner$ard, "source", NA)),
                   "programs/ard/functions/ard_mine.R")
  support_of <- function(code) {
    ex <- parse(text = code, keep.source = FALSE)
    hit <- vapply(ex, function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
                    identical(e[[2L]], as.name(".batch_support")), NA)
    eval(ex[[which(hit)]][[3L]], baseenv())
  }
  sup <- support_of(batch_code(o$planner, root = o$path))
  expect_true("programs/ard/functions/ard_mine.R" %in% sup$ard)
  expect_false("programs/ard/functions/ard_mine.R" %in% sup$tfl)
  # the batch.R written in the study folder says the same
  sup <- support_of(readLines(file.path(s$path, "programs", "batch.R")))
  expect_true("programs/ard/functions/ard_mine.R" %in% sup$ard)
  # without the key: nothing more
  sup <- support_of(batch_code(bs_planner()))
  expect_false(any(grepl("functions/", sup$ard, fixed = TRUE)))
})
