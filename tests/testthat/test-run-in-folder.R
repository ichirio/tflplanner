# An official run executes inside its own batch folder (tflspec#916): the
# manifest tflplanner writes into programs/batch.R, the study left as it was,
# and the batch folders the runs table lists.  (slow: its tests write study
# folders and run Rscript -- run on CI and locally with NOT_CRAN=true)
skip_on_cran()

# (test files do not share functions: these are test-batch-sets.R's)
bs_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

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

# a study with the one dataset the reports of bs_planner() read
rif_study <- function(id, env = parent.frame(), data = NULL) {
  bs_home(env)
  s <- create_study(id, planner = bs_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  open_study(id)
}

test_that("batch.R's manifest: an official run, its data folders, and the code it scans", {
  o <- rif_study("RIF1")
  f <- file.path(o$path, "programs", "batch.R")
  e <- new.env()
  sys.source(f, envir = e)
  expect_identical(e$.batch_kind, "run")
  expect_identical(e$.batch_paths, list())
  # a catalog path -> the variable the generated code reads it through
  expect_identical(e$.batch_data_vars[["data/adam/adsl.rds"]], "path_adam")
  expect_identical(e$.batch_data_dirs,
                   c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other"))
  # the study's own code, not the generated helpers
  expect_true(all(c("programs/company_setup.R", "programs/study_user.R") %in% e$.batch_check_code))
  expect_false("programs/study_helpers.R" %in% e$.batch_check_code)
})

test_that("the data variable of a catalog path is tflspec's rule: a path outside maps to none", {
  # (a ./ start and a backslash are one path; a full path no variable holds)
  d <- list(P = c("./data/adam/a.rds", "data\\sdtm\\b.rds", "D:/secure/c.rds",
                  "data/other/d.csv", "input/ard/x.rds"))
  v <- .batch_data_vars(d)
  expect_identical(unname(v), c("path_adam", "path_sdtm", "", "path_other", ""))
})

test_that("an official run leaves the study's working output as it was", {
  skip_if_not_installed("cards")
  o <- rif_study("RIF2")
  u <- update_study_ard(o, "DM")
  expect_true(u$ok)
  tree <- function() {
    f <- list.files(file.path(o$path, "output"), recursive = TRUE, full.names = TRUE)
    paste(f, unname(tools::md5sum(f)))
  }
  before <- tree()
  b <- run_batch(o, "ard")
  expect_true(b$ok)
  expect_identical(tree(), before)
  # what it made is in its own folder, which holds the programs it ran
  expect_true(file.exists(file.path(b$batch, "output", "ard", "ard.rds")))
  expect_true(file.exists(file.path(b$batch, "programs", "ard", "DM.R")))
  expect_match(paste(readLines(file.path(b$batch, "batch.txt")), collapse = "\n"),
               "Verified : yes", fixed = TRUE)
  lb <- list_batches(o)
  expect_identical(lb$kind[1], "run")
  expect_true(lb$code[1])
})

test_that("an official run refuses a data path outside the study, and makes no folder", {
  skip_if_not_installed("cards")
  bs_home()
  out <- file.path(withr_tempdir(), "secure")
  dir.create(out)
  saveRDS(cards::ADSL, file.path(out, "adsl.rds"))
  p <- bs_planner()
  p$ard$datasets$path <- paste0(normalizePath(out, "/"), "/adsl.rds")
  s <- create_study("RIF3", planner = p)
  o <- open_study("RIF3")
  b <- run_batch(o, "ard")
  expect_false(b$ok)
  expect_null(b$batch)
  txt <- paste(b$output, collapse = "\n")
  expect_match(txt, "The run was not started", fixed = TRUE)
  expect_match(txt, "outside the study folder", fixed = TRUE)
  expect_identical(nrow(list_batches(o)), 0L)
  # a preview of the same ARD still reads the file where it is
  u <- update_study_ard(o, "DM")
  expect_true(u$ok)
})

test_that("list_batches lists real batch folders: not .lock, _2 is no set, kind is said", {
  bs_home()
  s <- create_study("RIF4", planner = bs_planner())
  d <- file.path(s$path, "runs")
  mk <- function(name, txt = character()) {
    dir.create(file.path(d, name), recursive = TRUE)
    writeLines(c("Finished : 2026-01-01 10:00:00", txt), file.path(d, name, "batch.txt"))
    writeLines(c("part,program,status", "ard,programs/ard/DM.R,OK"),
               file.path(d, name, "run.csv"))
  }
  dir.create(file.path(d, ".lock"), recursive = TRUE)
  mk("20260101_100000_all", "Batch set : none")
  mk("20260101_100000_all_2", "Batch set : none")
  mk("20260102_100000_ard_Topline", "Batch set : Topline")
  mk("20260103_100000_report", "Kind     : preview")
  mk("20260104_100000_all", "Re-run   : 2026-01-05 09:00:00")
  lb <- list_batches(s)
  expect_false(any(startsWith(lb$batch, ".")))
  expect_identical(nrow(lb), 5L)
  row <- function(b) lb[lb$batch == b, ]
  expect_identical(row("20260101_100000_all_2")$set, "")
  expect_identical(row("20260101_100000_all_2")$what, "all")
  expect_identical(row("20260102_100000_ard_Topline")$set, "Topline")
  expect_identical(row("20260101_100000_all")$kind, "run")
  expect_identical(row("20260103_100000_report")$kind, "preview")
  expect_identical(row("20260104_100000_all")$kind, "rerun")
})

test_that("the generated study setup reads the data folders through the data root", {
  bs_home()
  s <- create_study("RIF5")
  txt <- readLines(file.path(s$path, "programs", "study_setup.R"), encoding = "UTF-8")
  root <- grep("^[.]tfl_data_root <- ", txt)
  adam <- grep("^path_adam +<- ", txt)
  expect_length(root, 1L)
  expect_true(root < adam)
  expect_match(txt[adam], "file.path(.tfl_data_root, \"data/adam\")", fixed = TRUE)
  # the other folders stay relative
  expect_true(any(grepl("^path_spec +<- \"spec\"$", txt)))
  # the option the programs are written with holds the relative folders
  expect_identical(unname(.study_path_vars()[["path_adam"]]), "data/adam")
})
