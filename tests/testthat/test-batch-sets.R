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
bs_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p <- add_output(p, "VS")
  p <- add_output(p, "L1", type = "listing", at = "end")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "VS"), analysis_id = c("AGE", "AGE"),
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
  # a report whose own code reads another report's rows of the study ARD
  p$outputs$data_code[p$outputs$output_id == "L1"] <-
    "data <- readRDS(file.path(path_ard, \"ard.rds\")) |> filter(output_id == \"DM\")"
  reads <- .batch_ard_reads(p)
  expect_identical(reads$DM, "DM")
  expect_identical(reads$L1, "DM")
  need <- .batch_excluded_needs(p, "DM")
  expect_identical(need$report, "L1")
  expect_identical(need$needs, "DM")
  expect_equal(nrow(.batch_excluded_needs(p, "VS")), 0L)
})

test_that("an official run of a named batch, or leaving reports out", {
  skip_if_not_installed("cards")
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

test_that("the batches column survives the SPEC round trip", {
  p <- set_batch(bs_planner(), "Topline", c("DM", "L1"))
  dir <- withr_tempdir()
  paths <- write_planner(p, dir)
  q <- read_planner(paths)
  expect_identical(batch_sets(q), batch_sets(p))
})

test_that("the report list shows a report's batches and edits them", {
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
    expect_match(paste(output$outputs, collapse = ""), "Batches", fixed = TRUE)
    session$setInputs(report_batches = 1)
    session$setInputs(report_batches_pick = c("Topline", "Interim"), report_batches_ok = 1)
    expect_identical(rv$p$outputs$batches[rv$p$outputs$output_id == "VS"], "Topline | Interim")
    expect_identical(batch_sets(rv$p)$Interim, "VS")
  })
})
