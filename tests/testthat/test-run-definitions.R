# Run definitions (slice B1): a study's official runs are saved definitions
# (p$runs, spec/runs.yml); the API, its name rule, the planner parts, the
# older forms moved in, and an official run of a definition.

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

test_that("run definitions: set, update, rename, remove", {
  p <- bs_planner()
  expect_identical(nrow(run_definitions(p)), 0L)
  p <- set_run_definition(p, "Topline", "DM")
  p <- set_run_definition(p, "Final", "all")
  rd <- run_definitions(p)
  expect_identical(rd$name, c("Topline", "Final"))
  expect_identical(rd$reports, c(1L, 3L))
  expect_identical(rd$mode, c("all", "all"))
  expect_identical(rd$copy_data, c(FALSE, FALSE))
  # the ids are kept in the report list's order, whatever order they come in
  p <- set_run_definition(p, "Topline", c("L1", "DM"))
  expect_identical(p$runs[[1]]$reports, c("DM", "L1"))
  # an argument left NULL stays; the options are set
  p <- set_run_definition(p, "Topline", mode = "ard", copy_data = TRUE)
  expect_identical(p$runs[[1]]$reports, c("DM", "L1"))
  expect_identical(run_definitions(p)$mode[1], "ard")
  expect_true(run_definitions(p)$copy_data[1])
  # renamed, removed (the reports stay)
  p <- rename_run_definition(p, "Topline", "Interim_Analysis")
  expect_identical(run_definitions(p)$name, c("Interim_Analysis", "Final"))
  expect_error(rename_run_definition(p, "Final", "interim_analysis"), "exists already")
  expect_error(rename_run_definition(p, "Nope", "X"), "No run definition")
  p <- remove_run_definition(p, "Final")
  expect_identical(run_definitions(p)$name, "Interim_Analysis")
  expect_identical(nrow(p$outputs), 3L)
  expect_error(remove_run_definition(p, "Final"), "No run definition")
})

test_that("a run definition's name: the rule, the case, and what a new one needs", {
  p <- bs_planner()
  expect_error(set_run_definition(p, "", "DM"), "Give the run definition a name")
  expect_error(set_run_definition(p, "Top/line", "DM"), "name")
  expect_error(set_run_definition(p, "Top line", "DM"), "name")
  expect_error(set_run_definition(p, "1st", "DM"), "name")
  expect_error(set_run_definition(p, strrep("a", 41), "DM"), "name")
  expect_error(set_run_definition(p, "Topline"), "needs its reports")
  expect_error(set_run_definition(p, "Topline", "AE"), "No report")
  p <- set_run_definition(p, "Topline", "DM")
  # the same name but for the case is another definition: refused
  expect_error(set_run_definition(p, "topline", "VS"), "ignoring case")
  # ... but renaming it so is the same definition
  q <- rename_run_definition(p, "Topline", "topline")
  expect_identical(run_definitions(q)$name, "topline")
  expect_identical(q$runs[[1]]$reports, "DM")
  # `all` is a definition's reports: every report, now and later
  p <- set_run_definition(p, "Final", "all")
  p <- add_output(p, "AE", at = "end")
  expect_identical(run_definitions(p)$reports, c(1L, 4L))
  expect_identical(.report_run_names(p, c("DM", "AE")), c("Topline | Final", "Final"))
})

test_that("an older name that breaks the rule: updated and kept, a Review row, not a new one", {
  p <- bs_planner()
  p$runs <- list(list(name = "Top line", reports = "DM"))
  # (an older name passes an update; its Review row asks for a rename)
  p <- set_run_definition(p, "Top line", c("DM", "VS"))
  expect_identical(p$runs[[1]]$reports, c("DM", "VS"))
  rows <- .rules_runs(p)
  expect_true(any(vapply(rows$args, function(a) "Top line" %in% a, NA)))
  # the save rule: a name new in this session that breaks it is refused ...
  root <- withr_tempdir()
  expect_identical(nrow(.runs_save_problems(p, root)), 1L)
  # ... one the disk has already passes
  dir.create(file.path(root, "spec"))
  writeLines(c("runs:", "  - name: \"Top line\"", "    reports: all"),
             file.path(root, "spec", "runs.yml"))
  expect_identical(nrow(.runs_save_problems(p, root)), 0L)
  # a rename to a name that keeps the rule fixes it
  p <- rename_run_definition(p, "Top line", "Topline")
  expect_identical(nrow(.runs_save_problems(p, root)), 0L)
})

test_that("rename_output and remove_output carry the ids of the definitions", {
  p <- bs_planner()
  p <- set_run_definition(p, "Topline", c("DM", "L1"))
  p <- set_run_definition(p, "Final", "all")
  p <- rename_output(p, "DM", "DM2")
  expect_identical(p$runs[[1]]$reports, c("DM2", "L1"))
  expect_identical(p$runs[[2]]$reports, "all")
  p <- remove_output(p, "L1")
  expect_identical(p$runs[[1]]$reports, "DM2")
  expect_identical(run_definitions(p)$reports, c(1L, 2L))
})

test_that("the planner's fields are all in the state, the parts and the merge", {
  # A new field of the planner must be named here, and then it has to be in
  # the state, in the parts, and put back by the merge: this fails while any
  # of them does not carry it.
  part_of <- c(sheets = "sheet:", study = "study keys", outputs = "report list",
               setup = "setup", ard = "ard:", lf = "lf:", fig_designs = "fig:",
               runs = "runs")
  p <- bs_planner()
  p <- set_run_definition(p, "Topline", c("DM", "L1"), mode = "ard")
  expect_setequal(setdiff(names(new_planner()), "meta"), names(part_of))
  s <- list(planner = p, meta = list())
  parts <- .study_parts(s)
  ps <- .planner_state(p)
  for (f in names(part_of)) {
    expect_true(f %in% names(ps), info = paste("the state has no", f))
    # (a figure's design is a part when the study has one)
    if (f != "fig_designs") {
      expect_true(any(startsWith(names(parts), part_of[[f]])),
                  info = paste("the parts have no", f))
    }
  }
  # the merge puts each part back where it was
  for (k in setdiff(names(parts), grep("^meta:", names(parts), value = TRUE))) {
    q <- .set_study_part(list(planner = new_planner(), meta = list()), k, parts[[k]])
    expect_identical(.study_parts(q)[[k]], parts[[k]], info = k)
  }
  expect_identical(.part_labels("runs"), "run definitions")
  # the state restores the definitions
  back <- .planner_from_state(list(planner = ps))
  expect_identical(.runs_bare(back$runs), .runs_bare(p$runs))
  expect_identical(.run_def_ids(back, back$runs[[1]]), c("DM", "L1"))
})

test_that("the older form moves into run definitions: the state, the workbook", {
  p <- bs_planner()
  ps <- .planner_state(p)
  ps$runs <- NULL
  ps$outputs$batches <- list("Topline | Final", "Final", NULL)
  q <- .planner_from_state(list(planner = ps))
  expect_identical(vapply(q$runs, function(d) d$name, ""), c("Topline", "Final"))
  expect_identical(q$runs[[2]]$reports, c("DM", "VS"))
  expect_identical(attr(q$runs, "migrated"), 2L)
  # the Review tab says so
  rows <- .rules_runs(q)
  expect_match(rows$template, "set(s) of the earlier form", fixed = TRUE)
  expect_identical(rows$args[[1]], "2")
  # a state with run definitions of its own is not migrated again
  ps$runs <- .runs_to_state(q$runs)
  expect_null(attr(.planner_from_state(list(planner = ps))$runs, "migrated"))
  # the workbook of an older planner: the column is not written
  dir <- withr_tempdir()
  paths <- write_planner(p, dir)
  expect_false("batches" %in% names(read_planner(paths)$outputs))
})

test_that("batch.R carries the definitions (with their problem) and the stamp", {
  p <- bs_planner()
  p <- set_run_definition(p, "Topline", c("DM", "L1"))
  man <- .batch_runs_manifest(p)
  expect_identical(man[[1]]$name, "Topline")
  expect_identical(man[[1]]$reports, c("DM", "L1"))
  expect_identical(man[[1]]$mode, "all")
  expect_false(man[[1]]$copy_data)
  expect_identical(man[[1]]$problem, "")
  # a definition with a problem still goes in, with its problem
  p$runs <- c(p$runs, list(list(name = "Top line", reports = "ZZ")))
  man <- .batch_runs_manifest(p)
  expect_match(man[[2]]$problem, "ZZ")
  # the code parses; it names the definition and no older sets
  code <- batch_code(set_run_definition(bs_planner(), "Topline", "DM"))
  expect_no_error(parse(text = code))
  expect_true(any(grepl("Topline", code, fixed = TRUE)))
  expect_false(any(grepl("^\\.batch_sets <- list", code)))
  # the stamp is the spec's: the same spec, the same stamp
  root <- withr_tempdir()
  expect_identical(.batch_stamp(root), .batch_stamp(root))
})

test_that("an official run of a definition, and the runs it leaves", {
  skip_on_cran()
  bs_home()
  # (the ARD programs only: the study has no report code to run)
  p <- set_run_definition(bs_planner(), "Topline", c("DM", "L1"), mode = "ard")
  p <- set_run_definition(p, "Final", "all", mode = "ard")
  s <- create_study("B1", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  o <- open_study("B1")
  expect_true(file.exists(file.path(s$path, "spec", "runs.yml")))

  b <- run_batch(o, "Topline", wait = TRUE)
  expect_true(b$ok)
  expect_match(basename(b$batch), "^batch_[0-9]{8}-[0-9]{6}_Topline$")
  expect_identical(basename(b$result$program), "DM.R")
  txt <- readLines(file.path(b$batch, "batch.txt"))
  expect_true("Run      : Topline" %in% txt)
  lb <- list_batches(o)
  expect_identical(lb$definition[1], "Topline")
  expect_identical(lb$state[1], "done")
  Sys.sleep(1.1)

  # a definition of every report; VS is in it, too
  b2 <- run_batch(o, "Final", wait = TRUE)
  expect_true(b2$ok)
  expect_true("VS.R" %in% basename(b2$result$program))

  # a name the study does not have; none given
  expect_error(run_batch(o, "Nope"), "No run definition named")
  expect_error(run_batch(o), "name")
})

test_that("a run of the ARD only, and a report whose program has a name of its own", {
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
  # an id in a definition: the ARD program (DM.R) and the report program (t_dm.R)
  b <- run_def(o, "OneReport", "DM")
  expect_identical(progs(b), c("DM.R", "t_dm.R"))
  Sys.sleep(1.1)
  # the ARD only: the mode of the definition
  b <- run_def(open_study("BN"), "ArdOnly", c("DM", "VS"), mode = "ard")
  expect_identical(progs(b), c("DM.R", "VS.R"))
  expect_match(basename(b$batch), "_ArdOnly$")
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

test_that("the run definitions survive a save and an open of the study", {
  skip_on_cran()
  bs_home()
  p <- set_run_definition(bs_planner(), "Topline", c("DM", "L1"), mode = "ard")
  s <- create_study("B2", planner = p)
  o <- open_study("B2")
  expect_identical(.runs_bare(o$planner$runs), .runs_bare(p$runs))
  txt <- readLines(file.path(s$path, "spec", "runs.yml"))
  expect_true(any(grepl("Topline", txt, fixed = TRUE)))
  # the report list shows the names, one report's definitions together
  expect_identical(.report_run_names(o$planner, c("DM", "VS", "L1")), c("Topline", "", "Topline"))
  # the Runs tab: the ticked reports saved under a name (the new API's rule)
  shiny::testServer(server_for("B2"), {
    rv <- session$userData$rv
    session$setInputs(nav = "results")
    expect_setequal(.batch_ticks("DM", "VS", "add"), c("DM", "VS"))
    expect_identical(.batch_ticks(c("DM", "VS"), "DM", "remove"), "VS")
    session$setInputs(batch_reports = "VS")
    session$setInputs(batch_save_name = "Interim", batch_save_name_ok = 1)
    expect_identical(rv$p$runs[[2]]$name, "Interim")
    expect_identical(rv$p$runs[[2]]$reports, "VS")
    # a name that breaks the rule is refused, the list as it was
    session$setInputs(batch_save_name = "Bad name", batch_save_name_ok = 2)
    expect_length(rv$p$runs, 2L)
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
  b <- run_def(o, "ArdOnly", "all", mode = "ard")
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
  b2 <- run_def(o, "ArdData", "all", mode = "ard", copy_data = TRUE)
  expect_true(b2$ok)
  expect_true(file.exists(file.path(b2$batch, "data", "adam", "adsl.rds")))
  man <- utils::read.csv(file.path(b2$batch, "data_manifest.csv"), colClasses = "character")
  expect_identical(man$path, "data/adam/adsl.rds")
  expect_identical(man$md5, unname(tools::md5sum(adsl)))
  expect_identical(as.numeric(man$size), file.size(adsl))
  txt <- readLines(file.path(b2$batch, "batch.txt"))
  expect_match(grep("^Data +:", txt, value = TRUE), "kept (data/, 1 file(s)", fixed = TRUE)
  expect_true(file.exists(file.path(b2$batch, "renv.lock")))
  expect_true(list_batches(o)$data[1])
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
    # the folder is the code: the programs it ran, as files
    for (rel in names(code)) {
      dir.create(dirname(file.path(d, b, rel)), recursive = TRUE, showWarnings = FALSE)
      writeLines(code[[rel]], file.path(d, b, rel))
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
  expect_identical(res[["code tfl/T-2.R"]], "changed")
  expect_identical(res[["output output/tfl/T-1.rtf"]], "same but the time")
  expect_identical(res[["output output/ard/T-1.rds"]], "different")
  expect_identical(attr(r, "changed")$inputs, c("data/adam/adae.rds", "tfl/T-2.R"))
  expect_identical(attr(r, "changed")$outputs, "output/ard/T-1.rds")
  # a run that kept no data nor code: its inputs are not compared
  r2 <- compare_batches(s, "20260103_100000_report", "20260102_100000_all")
  expect_identical(r2$result[r2$kind %in% c("data", "code")], c("not kept", "not kept"))
  expect_identical(r2$result[r2$file == "output/ard/T-1.rds"], "identical")
  # no run of its kind before it: the one before
  expect_identical(.batch_previous(list_batches(s), "20260103_100000_report"),
                   "20260102_100000_all")
  expect_error(compare_batches(s, "20260101_100000_all"), "No earlier official run")

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
  b1 <- run_def(s, "One", "T-14-1-1", copy_data = TRUE)
  expect_true(file.exists(file.path(b1$batch, "programs", "study_helpers.R")))
  expect_true(file.exists(file.path(b1$batch, "output", "tfl", "report_status.csv")))
  Sys.sleep(1.1)
  # as it is: nothing changed
  b2 <- run_def(s, "One", "T-14-1-1", copy_data = TRUE)
  r <- compare_batches(s, basename(b2$batch))
  expect_length(attr(r, "changed")$inputs, 0L)
  expect_length(attr(r, "changed")$outputs, 0L)
  Sys.sleep(1.1)
  # a data fix: that input, and the results that read it
  adsl <- file.path(s$path, "data", "adam", "adsl.rds")
  saveRDS(readRDS(adsl)[-1, ], adsl)
  b3 <- run_def(s, "One", "T-14-1-1", copy_data = TRUE)
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
