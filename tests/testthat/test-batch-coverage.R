# Coverage of R/batch.R: the branches the other tests leave out (empty
# inputs, a missing folder, a run that stopped, the old-runner and
# argument errors).  Fictional studies in temporary folders; the runs here
# are stub programs or fake batch folders, never the real runner.

bc_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# a study folder that is only a path (list_batches() and compare_batches()
# read nothing else)
bc_fake <- function(env = parent.frame()) {
  d <- withr_tempdir(env)
  list(path = d, meta = list(study_id = "BC"))
}

# one batch folder: run.csv's status column, batch.txt's lines
bc_batch <- function(study, name, status = NULL, txt = NULL, files = character()) {
  d <- file.path(study$path, "runs", name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(status)) {
    utils::write.csv(data.frame(program = paste0("p", seq_along(status), ".R"),
                                status = status),
                     file.path(d, "run.csv"), row.names = FALSE)
  }
  if (!is.null(txt)) writeLines(txt, file.path(d, "batch.txt"))
  for (f in files) {
    dir.create(dirname(file.path(d, f)), recursive = TRUE, showWarnings = FALSE)
    writeLines("x", file.path(d, f))
  }
  invisible(d)
}

bc_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p <- add_output(p, "VS")
  p <- add_output(p, "L1", type = "listing", at = "end")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = "ADSL", path = "data/adam/adsl.rds"), "datasets")
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    "populations")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = c("DM", "VS"), analysis_id = "AGE", method = "continuous",
    population_id = "SAF", by = "TRT01A", variables = "AGE"), "analyses")
  p
}

# a saved study, opened (needs bc_home() first)
bc_make <- function(id, planner = bc_planner()) {
  create_study(id, planner = planner)
  open_study(id)
}

# ---- list_batches() ------------------------------------------------------------

test_that("a study with no runs folder lists no batch, with every column", {
  s <- bc_fake()
  lb <- list_batches(s)
  expect_identical(nrow(lb), 0L)
  expect_identical(names(lb), c("batch", "started", "what", "set", "programs",
                                "errors", "state", "finished", "kind", "code",
                                "data", "excluded", "path"))
})

test_that("a folder starting with a dot (the runner's lock) is not a batch", {
  s <- bc_fake()
  bc_batch(s, ".lock")
  bc_batch(s, "20260101_100000_all", status = "OK",
           txt = "Finished : 2026-01-01 10:00:05")
  expect_identical(list_batches(s)$batch, "20260101_100000_all")
})

test_that("batch folders are listed newest first, with when and what from the name", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_ard")
  bc_batch(s, "20260203_091500_report")
  bc_batch(s, "20260203_091500_all_Topline")
  lb <- list_batches(s)
  expect_identical(lb$batch, c("20260203_091500_report", "20260203_091500_all_Topline",
                               "20260101_100000_ard"))
  expect_identical(lb$started[1], "2026-02-03 09:15:00")
  expect_identical(lb$what, c("report", "all", "ard"))
})

test_that("a folder whose name is not a batch name has no start and no kind of run", {
  s <- bc_fake()
  bc_batch(s, "scratch")
  lb <- list_batches(s)
  expect_true(is.na(lb$started))
  expect_true(is.na(lb$what))
  expect_identical(lb$state, "incomplete")
})

test_that("a run with no record of its end is incomplete, not failed", {
  s <- bc_fake()
  # run.csv but no "Finished :" (still running, or stopped)
  bc_batch(s, "20260101_100000_all", status = c("OK", "ERROR"),
           txt = "Batch    : x")
  lb <- list_batches(s)
  expect_identical(lb$state, "incomplete")
  expect_true(is.na(lb$finished))
  # the counts are still read from run.csv
  expect_identical(lb$programs, 2L)
  expect_identical(lb$errors, 1L)
})

test_that("a finished run with a program not OK is failed; all OK is done", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_all", status = c("OK", "ERROR"),
           txt = "Finished : 2026-01-01 10:00:05")
  bc_batch(s, "20260102_100000_all", status = c("OK", "OK"),
           txt = "Finished : 2026-01-02 10:00:05")
  lb <- list_batches(s)
  expect_identical(lb$state[lb$batch == "20260101_100000_all"], "failed")
  expect_identical(lb$state[lb$batch == "20260102_100000_all"], "done")
  expect_identical(lb$finished[lb$batch == "20260102_100000_all"], "2026-01-02 10:00:05")
})

test_that("a run.csv that cannot be read gives no counts and an incomplete run", {
  s <- bc_fake()
  d <- bc_batch(s, "20260101_100000_all", txt = "Finished : 2026-01-01 10:00:05")
  file.create(file.path(d, "run.csv"))
  lb <- list_batches(s)
  expect_true(is.na(lb$programs))
  expect_true(is.na(lb$errors))
  expect_identical(lb$state, "incomplete")
})

test_that("batch.txt says a preview, a re-run, the batch set and what was left out", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_all", txt = "Kind     : preview")
  bc_batch(s, "20260102_100000_all", txt = "Re-run   : runs/x")
  bc_batch(s, "20260103_100000_all", txt = c("Batch set : Topline",
                                             "Excluded : VS.R, L1.R"))
  bc_batch(s, "20260104_100000_all", txt = c("Batch set : none", "Excluded : none"))
  lb <- list_batches(s)
  by <- function(col, b) lb[[col]][lb$batch == b]
  expect_identical(by("kind", "20260101_100000_all"), "preview")
  expect_identical(by("kind", "20260102_100000_all"), "rerun")
  expect_identical(by("kind", "20260103_100000_all"), "run")
  expect_identical(by("set", "20260103_100000_all"), "Topline")
  expect_identical(by("excluded", "20260103_100000_all"), "VS.R, L1.R")
  # "none" is no set and nothing left out
  expect_identical(by("set", "20260104_100000_all"), "")
  expect_identical(by("excluded", "20260104_100000_all"), "")
})

test_that("a name's _2 suffix (a run in the same second) is not a batch set", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_all_2")
  lb <- list_batches(s)
  expect_identical(lb$set, "")
  expect_identical(lb$what, "all")
})

test_that("a batch folder says whether it kept the code and the data", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_all", files = c("programs/a.R", "data/adam/x.rds"))
  bc_batch(s, "20260102_100000_all")
  lb <- list_batches(s)
  expect_identical(lb$code, c(FALSE, TRUE))
  expect_identical(lb$data, c(FALSE, TRUE))
})

# ---- compare_batches() ---------------------------------------------------------

test_that("comparing a batch folder that does not exist says so", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_all", status = "OK", txt = "Finished : 2026-01-01 10:00:05")
  expect_error(compare_batches(s, "20269999_000000_all"), "No batch folder")
})

test_that("comparing against a folder that is not a batch says there is no earlier run", {
  s <- bc_fake()
  bc_batch(s, "20260101_100000_all", status = "OK", txt = "Finished : 2026-01-01 10:00:05")
  bc_batch(s, "20260102_100000_all", status = "OK", txt = "Finished : 2026-01-02 10:00:05")
  expect_error(compare_batches(s, "20260102_100000_all", against = "nope"),
               "No earlier official run")
})

test_that("a result one run made and the other did not is new or not made", {
  s <- bc_fake()
  ok <- "Finished : 2026-01-01 10:00:05"
  bc_batch(s, "20260101_100000_all", status = "OK", txt = ok,
           files = c("output/tfl/old.rtf", "output/tfl/both.rtf"))
  bc_batch(s, "20260102_100000_all", status = "OK", txt = ok,
           files = c("output/tfl/new.rtf", "output/tfl/both.rtf"))
  r <- compare_batches(s, "20260102_100000_all")
  res <- stats::setNames(r$result[r$kind == "output"], r$file[r$kind == "output"])
  expect_identical(res[["output/tfl/old.rtf"]], "not made")
  expect_identical(res[["output/tfl/new.rtf"]], "new")
  expect_identical(res[["output/tfl/both.rtf"]], "identical")
  expect_setequal(attr(r, "changed")$outputs, c("output/tfl/old.rtf", "output/tfl/new.rtf"))
  expect_identical(attr(r, "batches")[["batch"]], "20260102_100000_all")
})

test_that("a data file only one run kept is added or removed", {
  s <- bc_fake()
  ok <- "Finished : 2026-01-01 10:00:05"
  mk <- function(b, paths) {
    d <- bc_batch(s, b, status = "OK", txt = ok)
    utils::write.csv(data.frame(path = paths, md5 = "m"),
                     file.path(d, "data_manifest.csv"), row.names = FALSE)
  }
  mk("20260101_100000_all", c("data/adam/a.rds", "data/adam/b.rds"))
  mk("20260102_100000_all", c("data/adam/b.rds", "data/adam/c.rds"))
  r <- compare_batches(s, "20260102_100000_all")
  d <- r[r$kind == "data", ]
  expect_identical(stats::setNames(d$result, d$file),
                   c("data/adam/a.rds" = "removed", "data/adam/c.rds" = "added"))
})

test_that("two runs with no output folder at all compare to no result rows", {
  s <- bc_fake()
  ok <- "Finished : 2026-01-01 10:00:05"
  bc_batch(s, "20260101_100000_all", status = "OK", txt = ok)
  bc_batch(s, "20260102_100000_all", status = "OK", txt = ok)
  r <- compare_batches(s, "20260102_100000_all")
  expect_identical(sum(r$kind == "output"), 0L)
  expect_identical(r$result[r$kind %in% c("data", "code")], c("not kept", "not kept"))
  expect_length(attr(r, "changed")$inputs, 0L)
})

# ---- .batch_previous() ---------------------------------------------------------

test_that("the run before is one that ended; the oldest run has none", {
  lb <- data.frame(batch = c("c_all", "b_all", "a_all"), what = "all",
                   state = c("done", "incomplete", "failed"),
                   stringsAsFactors = FALSE)
  # b_all stopped: skipped; a_all failed but ended: compared with
  expect_identical(.batch_previous(lb, "c_all"), "a_all")
  expect_true(is.na(.batch_previous(lb, "a_all")))
})

test_that("the run before prefers the same kind, else any kind that ended", {
  lb <- data.frame(batch = c("d_all", "c_ard", "b_all", "a_report"),
                   what = c("all", "ard", "all", "report"),
                   state = "done", stringsAsFactors = FALSE)
  expect_identical(.batch_previous(lb, "d_all"), "b_all")
  expect_identical(.batch_previous(lb, "c_ard"), "b_all")
  expect_identical(.batch_previous(lb, "b_all"), "a_report")
})

# ---- .batch_compare_file() / .batch_untimed() ----------------------------------

test_that("two results are compared by bytes, then by text, else different", {
  d <- withr_tempdir()
  f <- function(name, lines) {
    p <- file.path(d, name)
    writeLines(lines, p)
    p
  }
  a <- f("a.txt", "same")
  expect_identical(.batch_compare_file(a, f("b.txt", "same")), "identical")
  expect_identical(.batch_compare_file(a, f("c.txt", "other")), "different")
  # a file the later run did not make
  expect_identical(.batch_compare_file(a, file.path(d, "gone.txt")), "not made")
  # a file that is neither text nor rds and differs
  expect_identical(.batch_compare_file(f("a.bin", "1"), f("b.bin", "2")), "different")
})

test_that("an rds that differs in bytes but holds the same object is the same object", {
  d <- withr_tempdir()
  x <- file.path(d, "x.rds")
  y <- file.path(d, "y.rds")
  saveRDS(data.frame(a = 1:3), x, compress = FALSE)
  saveRDS(data.frame(a = 1:3), y, compress = "gzip")
  expect_identical(.batch_compare_file(x, y), "same object")
  saveRDS(data.frame(a = 4:6), y)
  expect_identical(.batch_compare_file(x, y), "different")
})

test_that("an rds that cannot be read is different, not an error", {
  d <- withr_tempdir()
  x <- file.path(d, "x.rds")
  y <- file.path(d, "y.RDS")
  writeLines("not an rds", x)
  writeLines("not an rds either", y)
  expect_identical(.batch_compare_file(x, y), "different")
})

test_that("a report, csv or txt the same but the date and time is the same but the time", {
  d <- withr_tempdir()
  w <- function(name, lines) {
    p <- file.path(d, name)
    writeLines(lines, p)
    p
  }
  expect_identical(.batch_compare_file(
    w("a.csv", "built,2026-01-01 10:00:05"), w("b.csv", "built,2026-02-02 11:30:00")),
    "same but the time")
  expect_identical(.batch_compare_file(
    w("a.txt", "on 01Jan2026  10:00"), w("b.txt", "on 15Feb2026  9:45")),
    "same but the time")
  expect_identical(.batch_compare_file(
    w("a.rtf", "{\\creatim\\yr2026\\mo1}x"), w("b.rtf", "{\\creatim\\yr2027\\mo2}x")),
    "same but the time")
  # a difference that is not a time stays a difference
  expect_identical(.batch_compare_file(
    w("c.txt", "total 10 at 2026-01-01 10:00"), w("d.txt", "total 11 at 2026-01-01 10:00")),
    "different")
})

test_that("the times a file prints are put aside, the rest kept", {
  f <- withr_tempdir()
  p <- file.path(f, "t.txt")
  writeLines(c("a 2026-10-12 08:49:26 b", "c 2026-10-12T08:49 d", "e 12Oct2026  08:49 f",
               "no time here"), p)
  expect_identical(.batch_untimed(p), c("a <time> b", "c <time> d", "e <time> f",
                                        "no time here"))
})

# ---- the texts of the runs table -----------------------------------------------

test_that("a time taken never reads negative", {
  expect_identical(.elapsed_text(-5), "0:00")
  expect_identical(.elapsed_text(0), "0:00")
  expect_identical(.elapsed_text(3600), "1:00:00")
})

test_that("a size is in KB under 1 MB, one decimal to 10 MB, whole MB to 1 GB, then GB", {
  expect_identical(.mb_text(1), "1.0 MB")
  expect_identical(.mb_text(10), "10 MB")
  expect_identical(.mb_text(1023), "1023 MB")
  expect_identical(.mb_text(1024), "1.0 GB")
  expect_identical(.mb_text(0), "0 KB")
})

test_that("a run's state text is running, done, failed or not finished, and is translated", {
  d <- data.frame(
    started = "2026-01-01 10:00:00",
    finished = c(NA, "2026-01-01 10:05:00", "2026-01-01 10:06:00", NA),
    state = c("running", "done", "failed", "incomplete"),
    programs = c(3L, 3L, 3L, 3L), errors = c(NA, 0L, 2L, NA),
    stringsAsFactors = FALSE)
  txt <- .batch_state_text(d, now = as.POSIXct("2026-01-01 10:01:05"))
  expect_identical(txt[1], "Running 1:05")
  expect_identical(txt[2], "Done 10:05: 3 of 3 without error")
  expect_identical(txt[3], "2 errors (of 3), 10:06")
  expect_identical(txt[4], "Not finished (stopped)")
  up <- .batch_state_text(d[4, ], t = toupper)
  expect_identical(up, "NOT FINISHED (STOPPED)")
})

# ---- needs of the reports ------------------------------------------------------

test_that("with no read there are no needs and no unresolved reads", {
  reads <- data.frame(report = character(), reads = character(), kind = character(),
                      reason = character(), stringsAsFactors = FALSE)
  expect_length(.batch_reads_needs(reads), 0L)
  expect_length(.batch_reads_unresolved(reads), 0L)
})

test_that("needs are the ids a report reads once each; unresolved are its reasons joined", {
  reads <- data.frame(
    report = c("F1", "F1", "F1", "F2"),
    reads = c("T1", "T1", "T2", NA),
    kind = "source",
    reason = c(NA, NA, NA, "no source"), stringsAsFactors = FALSE)
  expect_identical(.batch_reads_needs(reads), list(F1 = c("T1", "T2")))
  expect_identical(.batch_reads_unresolved(reads), c(F2 = "no source"))
  reads$reason[1:2] <- c("source report not found", "source not in analyses")
  expect_identical(.batch_reads_unresolved(reads)[["F1"]],
                   "source report not found; source not in analyses")
})

test_that("no missing need when nothing is needed or the needed is selected too", {
  expect_identical(nrow(.batch_missing_needs(list(), "T1")), 0L)
  expect_identical(names(.batch_missing_needs(list(), "T1")), c("report", "need"))
  needs <- list(F1 = c("T1", "T2"), F2 = "T3")
  # F2 is not selected: its needs do not count
  m <- .batch_missing_needs(needs, c("F1", "T1"))
  expect_identical(m$report, "F1")
  expect_identical(m$need, "T2")
  expect_identical(nrow(.batch_missing_needs(needs, character())), 0L)
})

test_that("the reads of tables are their own ARD; a listing reads none; no report, no row", {
  p <- bc_planner()
  r <- .batch_report_reads(p)
  expect_identical(names(r), c("report", "reads", "kind", "reason"))
  expect_identical(r$reads[r$kind == "own"], c("DM", "VS"))
  expect_false("L1" %in% r$report)
  expect_identical(nrow(.batch_report_reads(new_planner())), 0L)
})

test_that("leaving a report out leaves its two programs; an unknown name is kept as it is", {
  p <- bc_planner()
  ex <- .batch_exclude(p, c("DM", "DM", "mine.R"))
  expect_identical(anyDuplicated(ex), 0L)
  expect_true("mine.R" %in% ex)
  expect_true(report_info(p, "DM")$program %in% ex)
  expect_length(.batch_exclude(p, character()), 0L)
})

# ---- the data a run keeps ------------------------------------------------------

test_that("the data plan refuses a part that is not ard or tfl", {
  bc_home()
  s <- bc_make("BCP1")
  expect_error(batch_data_plan(s, "report"), "should be one of")
})

test_that("a data file that is not in the study folder has no size", {
  bc_home()
  s <- bc_make("BCP2")
  plan <- batch_data_plan(s, "ard")
  expect_identical(plan$path, "data/adam/adsl.rds")
  expect_true(is.na(plan$size))
  expect_identical(plan$programs, "DM.R, VS.R")
})

test_that("the data plan of chosen reports lists only their programs' data", {
  bc_home()
  s <- bc_make("BCP3")
  expect_identical(batch_data_plan(s, "ard", output_ids = "DM")$programs, "DM.R")
  # a listing has no ARD program: nothing from the ARD part
  none <- batch_data_plan(s, "ard", output_ids = "L1")
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), c("path", "size", "programs"))
})

test_that("no path means no data variable", {
  expect_identical(.batch_data_vars(list()), character())
  expect_identical(.batch_data_vars(list(a = character())), character())
})

test_that("the study's own functions are scanned for outside paths, sorted, R files only", {
  bc_home()
  d <- withr_tempdir()
  fun <- file.path(d, .study_fun_dir)
  dir.create(fun, recursive = TRUE)
  for (f in c("b.R", "a.r", "notes.txt")) writeLines("x", file.path(fun, f))
  code <- .batch_check_code(d)
  own <- code[startsWith(code, paste0(.study_fun_dir, "/"))]
  expect_identical(own, file.path(.study_fun_dir, c("a.r", "b.R")))
  # with none: only the company's and the user's setup
  d2 <- withr_tempdir()
  expect_identical(.batch_check_code(d2), c(.company_setup_path(), .study_user_path()))
})

# ---- the generated programs ----------------------------------------------------

test_that("the autoexec for both parts says how to run it, and runs the ARD then the reports", {
  code <- autoexec_all_code(date = as.Date("2026-03-04"))
  expect_true(any(grepl("Rscript programs/autoexec_all.R", code, fixed = TRUE)))
  expect_true(any(grepl("--no-code", code, fixed = TRUE)))
  expect_true(any(grepl("--data", code, fixed = TRUE)))
  expect_true(any(grepl("Generated  : tflplanner .*, 2026-03-04$", code)))
  expect_true("run_batch(c(\"ard\", \"tfl\"), commandArgs(trailingOnly = TRUE))" %in% code)
})

test_that("an autoexec body of one part names that part alone", {
  body <- .autoexec_body("ard")
  expect_true("run_batch(\"ard\", commandArgs(trailingOnly = TRUE))" %in% body)
  expect_true(any(grepl("source(\"programs/batch.R\")", body, fixed = TRUE)))
  expect_true(any(grepl("study.yml", body, fixed = TRUE)))
})

test_that("the runner program is made for a planner with one report, with the date in its banner", {
  p <- add_output(new_planner(), "T-14-1-1", type = "table")
  code <- batch_code(p, date = as.Date("2026-05-06"))
  expect_type(code, "character")
  expect_true(any(grepl("programs/batch.R", code, fixed = TRUE)))
  expect_true(any(grepl("2026-05-06", code, fixed = TRUE)))
  expect_true(any(grepl("T-14-1-1", code, fixed = TRUE)))
})

test_that("the runner program is made when the study.yml cannot be read", {
  d <- withr_tempdir()
  writeLines("a: [b", file.path(d, "study.yml"))
  p <- add_output(new_planner(), "T-1", type = "table")
  expect_no_error(batch_code(p, root = d))
})

test_that("the figure setup uses the base libraries, with or without designs", {
  expect_identical(.fig_setup_libs(NULL), c("ggplot2", "patchwork", "dplyr"))
  expect_identical(.fig_setup_libs(new_planner()), c("ggplot2", "patchwork", "dplyr"))
})

test_that("a save writes the runner, the setups and the autoexec files, then none again", {
  bc_home()
  s <- bc_make("BCW")
  root <- s$path
  out <- .save_batch_programs(s$planner, root)
  expect_identical(nrow(out), 5L)
  expect_identical(basename(out$file),
                   c("batch.R", "fig_setup.R", "report_setup.R", "autoexec_all.R",
                     "autoexec_report.R"))
  expect_true(all(file.exists(out$file)))
  # nothing changed: nothing written
  again <- .save_batch_programs(s$planner, root)
  expect_true(all(again$status == "unchanged"))
  # a program removed is written again
  unlink(file.path(root, "programs", "autoexec_all.R"))
  third <- .save_batch_programs(s$planner, root)
  expect_identical(third$status[basename(third$file) == "autoexec_all.R"], "written")
})

# ---- run_batch(): what it refuses, and what it hands back ----------------------

# a study whose autoexec is a stub: it prints its arguments, then does what
# `body` says; batch.R is a comment that names the flags the runner knows
# (or not, as an earlier tflplanner's did not)
bc_stub <- function(id, body = "", flags = "--exclude= --batch --data") {
  s <- bc_make(id)
  prog <- file.path(s$path, "programs", "ard", .ard_autoexec_file)
  dir.create(dirname(prog), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("cat('ARGS:', commandArgs(trailingOnly = TRUE), '\\n')", body), prog)
  writeLines(paste("#", flags), file.path(s$path, "programs", .batch_file))
  s
}

test_that("a run of the reports alone is refused", {
  bc_home()
  s <- bc_stub("BCR1")
  expect_error(run_batch(s, "tfl"), "ARD only")
})

test_that("a run of a study that was not saved says to save it first", {
  bc_home()
  s <- bc_stub("BCR2")
  unlink(file.path(s$path, "programs", "ard", .ard_autoexec_file))
  expect_error(run_batch(s, "ard"), "save it first")
  unlink(file.path(s$path, "programs", .autoexec_all_file))
  expect_error(run_batch(s, c("ard", "tfl")), "autoexec_all.R yet")
})

test_that("a runner written by an earlier tflplanner cannot leave reports out or run a batch", {
  bc_home()
  s <- bc_stub("BCR3", flags = "nothing here")
  expect_error(run_batch(s, "ard", exclude = "DM"), "earlier tflplanner")
  s$planner <- set_batch(s$planner, "Topline", "DM")
  expect_error(run_batch(s, "ard", batch = "Topline"), "earlier tflplanner")
  expect_error(run_batch(s, "ard", data = TRUE), "earlier tflplanner")
})

test_that("a batch name the study does not have is refused with the names it has", {
  bc_home()
  s <- bc_stub("BCR4")
  s$planner <- set_batch(s$planner, "Topline", "DM")
  expect_error(run_batch(s, "ard", batch = "Nope"), "Topline")
})

test_that("a run hands the arguments to the autoexec and returns what it printed", {
  skip_on_cran()
  bc_home()
  s <- bc_stub("BCR5")
  s$planner <- set_batch(s$planner, "Topline", "DM")
  b <- run_batch(s, "ard", code = FALSE, data = TRUE, only = "DM", exclude = "VS",
                 batch = "Topline")
  expect_true(b$ok)
  args <- grep("^ARGS:", b$output, value = TRUE)
  expect_match(args, "--no-code", fixed = TRUE)
  expect_match(args, "--data", fixed = TRUE)
  expect_match(args, "DM", fixed = TRUE)
  expect_match(args, "--exclude=VS.R", fixed = TRUE)
  expect_match(args, "--batch Topline", fixed = TRUE)
  # the stub made no batch folder: no folder, no result
  expect_null(b$batch)
  expect_null(b$result)
})

test_that("a run whose program stops is not ok, and its message is in the output", {
  skip_on_cran()
  bc_home()
  s <- bc_stub("BCR6", body = "stop('a made-up stop')")
  b <- run_batch(s, "ard")
  expect_false(b$ok)
  expect_true(any(grepl("a made-up stop", b$output, fixed = TRUE)))
})

test_that("a run returns the batch folder it made and its run.csv", {
  skip_on_cran()
  bc_home()
  body <- c(
    "d <- file.path('runs', '20260101_100000_ard')",
    "dir.create(d, recursive = TRUE)",
    "write.csv(data.frame(program = 'DM.R', status = 'OK'), file.path(d, 'run.csv'), row.names = FALSE)",
    "writeLines('Finished : 2026-01-01 10:00:05', file.path(d, 'batch.txt'))")
  s <- bc_stub("BCR7", body = body)
  b <- run_batch(s, "ard")
  expect_true(b$ok)
  expect_identical(basename(b$batch), "20260101_100000_ard")
  expect_identical(b$result$status, "OK")
  expect_identical(list_batches(s)$state, "done")
})

test_that("a run that is not waited for returns its process", {
  skip_on_cran()
  bc_home()
  s <- bc_stub("BCR8")
  px <- run_batch(s, "ard", wait = FALSE)
  expect_s3_class(px, "process")
  px$wait()
  expect_identical(px$get_exit_status(), 0L)
})

test_that("a second run of the same study is refused while the first goes on", {
  skip_on_cran()
  bc_home()
  s <- bc_stub("BCR9", body = "Sys.sleep(30)")
  px <- run_batch(s, "ard", wait = FALSE)
  on.exit(px$kill(), add = TRUE)
  expect_error(run_batch(s, "ard"), "is going on")
})
