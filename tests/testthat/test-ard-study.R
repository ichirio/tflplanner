local_home2 <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

ard_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p <- add_output(p, "L1", type = "listing")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "DM"), analysis_id = c("BIGN", "AGE"),
    method = c("categorical", "continuous"), population_id = "SAF",
    by = c(NA, "TRT01A"), variables = c("TRT01A", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("a report's analyses are edited like the other sheets", {
  p <- ard_planner()
  p <- add_output(p, "AE")
  expect_equal(nrow(ard_rows(p, "analyses", "DM")), 2)
  rows <- ard_rows(p, "analyses", "AE")
  rows$output_id <- NULL
  rows[1, ] <- NA
  rows$analysis_id[1] <- "ANY"
  rows$method[1] <- "subjects"
  p <- set_ard_rows(p, "analyses", "AE", rows)
  expect_equal(p$ard$analyses$output_id, c("DM", "DM", "AE"))
  q <- copy_output(p, "DM", "DM2")
  expect_equal(nrow(ard_rows(q, "analyses", "DM2")), 2)
  q <- rename_output(q, "DM2", "DM3")
  expect_equal(nrow(ard_rows(q, "analyses", "DM3")), 2)
  q <- remove_output(q, "DM3")
  expect_identical(q$ard, p$ard)
})

test_that("a table the ARD definition serves reads its part of the study ARD", {
  p <- ard_planner()
  code <- data_lines(p, "DM")
  expect_true(any(grepl("readRDS(\"output/ard/ard.rds\")", code, fixed = TRUE)))
  expect_true("data <- ard_normalize(ard)" %in% code)
  expect_true(any(grepl("still to be written", data_lines(p, "L1"))))
  p$outputs$data_code[1] <- "ard <- my_ard()"
  expect_false(any(grepl("ard.rds", data_lines(p, "DM"))))
  expect_false(any(grepl("make_ard", autoexec_code(p))))
})

test_that("the definition is saved, reopened and run", {
  skip_if_not_installed("cards")
  skip_on_cran()
  local_home2()
  s <- create_study("A1", planner = ard_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  expect_true(file.exists(file.path(s$path, "spec", "ard_spec.xlsx")))
  for (f in c("ard_setup.R", "DM.R", "autoexec_ard.R")) {
    expect_true(file.exists(file.path(s$path, "programs", "ard", f)))
  }
  expect_true(file.exists(file.path(s$path, "programs", "tfl", "DM.R")))
  s2 <- save_study(s)
  expect_true(all(s2$files$status[grepl("ard", s2$files$file)] == "unchanged"))
  o <- open_study("A1")
  expect_identical(o$planner$ard, s$planner$ard)

  r <- run_ard(o, "DM")
  expect_null(r$error)
  expect_setequal(unique(r$ard$analysis_id), c("BIGN", "AGE"))
  v <- ard_view(r$ard)
  expect_true(all(c("analysis_id", "variable", "stat_name", "stat") %in%
                    names(v)))
  expect_false(file.exists(file.path(s$path, "output", "ard", "ard.rds")))
  # the study ARD from the saved programs, each with its log
  r <- run_study_ard(o)
  expect_true(r$ok)
  expect_true(file.exists(file.path(s$path, "output", "ard", "ard.rds")))
  expect_equal(ard_status(o)$state, "built")
  log <- readLines(file.path(s$path, "logs", "ard", "DM.log"))
  expect_true(any(grepl("cards::ard_continuous", log, fixed = TRUE)))
  expect_true(any(grepl("run by", log, fixed = TRUE)))

  # the reports: their programs in programs/tfl, logs in logs/tfl
  st <- run_study(o, "DM")
  expect_equal(st$status[st$output_id == "DM"], "error")   # no table spec
  expect_true(file.exists(file.path(s$path, "logs", "tfl", "DM.log")))

  # a failing ARD program leaves the study ARD and records its error
  o2 <- o
  o2$planner$ard$analyses$method[2] <- "cards::ard_nope"
  o2 <- save_study(o2)
  u <- update_study_ard(o2, "DM")
  expect_false(u$ok)
  expect_match(u$error, "ard_nope")
  expect_equal(ard_status(o2)$state, "error")
  expect_true(file.exists(file.path(s$path, "output", "ard", "ard.rds")))

  bad <- o
  bad$planner$ard$analyses$method[2] <- "cards::ard_nope"
  r <- run_ard(bad, "DM")
  expect_null(r$ard)
  expect_match(r$error, "ard_nope")

  # registering a folder reads its ard_spec.xlsx
  unregister_study("A1")
  g <- register_study(s$path)
  expect_identical(g$planner$ard, o2$planner$ard)
})
