flow_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

two_output_study <- function(id = "F1") {
  p <- add_output(new_planner(), "DM")
  p <- add_output(p, "SEX")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = "ADSL", path = "data/adam/adsl.rds"), "datasets")
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    "populations")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = c("DM", "SEX"), analysis_id = c("AGE", "SEX"),
    method = c("continuous", "categorical"), population_id = "SAF",
    by = "TRT01A", variables = c("AGE", "SEX")), "analyses")
  s <- create_study(id, planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  s
}

test_that("the study ARD grows output by output, and says where it stands", {
  skip_if_not_installed("cards")
  skip_on_cran()
  flow_home()
  s <- two_output_study()
  expect_equal(ard_status(s)$state, c("not built", "not built"))
  # the preview's ARD (#428): made for the report alone, in a folder of its
  # own -- the study ARD is not written
  expect_equal(ard_preview_state(s, "DM"), "missing")
  m <- fetch_ard(s, "DM")
  expect_identical(m$made, "here")
  expect_equal(m$variables$variable, "AGE")
  expect_false(file.exists(file.path(s$path, "output", "ard", "ard.rds")))
  expect_equal(ard_status(s)$state, c("not built", "not built"))
  expect_equal(ard_preview_state(s, "DM"), "current")
  expect_identical(unique(ard_raw(s, "DM", rows = TRUE)$output_id), "DM")
  expect_false("output_id" %in% names(ard_raw(s, "DM")))
  # the preview normalizes the ARD read as the report's item says
  expect_identical(.preview_normalized(s$planner, "DM", ard_raw(s, "DM")), ard_data(s, "DM"))
  # what it is made from changes: outdated
  s2 <- s
  s2$planner$ard$analyses$label[1] <- "Age"
  expect_equal(ard_preview_state(s2, "DM"), "outdated")

  update_study_ard(s, "DM")
  st <- ard_status(s)
  expect_equal(st$state, c("built", "not built"))
  a <- readRDS(file.path(s$path, "output", "ard", "ard.rds"))
  expect_equal(unique(a$output_id), "DM")
  expect_equal(fetch_ard(s, "DM")$variables$variable, "AGE")
  # (the study ARD's rows, made from the definition as it is now: read)
  expect_identical(ard_info(s, "DM")$made, "study")

  update_study_ard(s, "SEX")
  a2 <- readRDS(file.path(s$path, "output", "ard", "ard.rds"))
  expect_setequal(unique(a2$output_id), c("DM", "SEX"))
  # DM's rows were left as they were
  expect_equal(nrow(a2[a2$output_id == "DM", ]), nrow(a[a$output_id == "DM", ]))

  # the definition moves on: outdated; a failed update: error, ARD kept
  s$planner$ard$analyses$statistics[1] <- "N | mean"
  expect_equal(ard_status(s)$state[1], "outdated")
  s$planner$ard$analyses$method[2] <- "cards::ard_nope"
  s <- save_study(s)
  update_study_ard(s, "SEX")
  expect_equal(ard_status(s)$state[2], "error")
  a3 <- readRDS(file.path(s$path, "output", "ard", "ard.rds"))
  expect_identical(a3, a2)

  # the whole study at once records every output
  s$planner$ard$analyses$method[2] <- "categorical"
  run_ard(s)
  expect_equal(ard_status(s)$state, c("built", "built"))
})

test_that("two people saving different parts both keep their changes", {
  skip_if_not_installed("cards")
  flow_home()
  s <- two_output_study("F2")
  ard_person <- open_study("F2")
  table_person <- open_study("F2")
  base_a <- ard_person
  base_t <- table_person

  ard_person$planner$ard$analyses$label[1] <- "Age"
  save_study(ard_person, base = base_a)
  table_person$planner$sheets$titles <- .normalize_sheet(data.frame(
    output_id = "DM", line = "3", center = "Demographics"), "titles")
  save_study(table_person, base = base_t)

  now <- open_study("F2")
  expect_equal(now$planner$ard$analyses$label[1], "Age")
  expect_equal(now$planner$sheets$titles$center, "Demographics")

  # the same part changed by both: the second save stops
  a <- open_study("F2")
  b <- open_study("F2")
  base <- a
  a$planner$ard$analyses$label[1] <- "Age (years)"
  save_study(a, base = base)
  b$planner$ard$analyses$label[1] <- "Age, years"
  err <- tryCatch(save_study(b, base = base), tflplanner_conflict = function(e) e)
  expect_s3_class(err, "tflplanner_conflict")
  expect_equal(err$parts, "ard:analyses")
  expect_equal(open_study("F2")$planner$ard$analyses$label[1], "Age (years)")
})
