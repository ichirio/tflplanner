# A report's ARD takes the code lists it inherits from its layers (the study
# defaults, a pattern), as tflspec's helper gives them (tflspec #655): the
# programs, builds and hashes all read them through .study_codelists().
# Fictional data (cards::ADSL).

skip_on_cran()
skip_if_not("tfl_report_codelists" %in% getNamespaceExports("tflspec"))

cl_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

cl_trt <- "TRT01A: [{value: Placebo}, {value: Xanomeline Low Dose}, {value: Xanomeline High Dose}]"

cl_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "DM"), analysis_id = c("GROUPN", "AGE"),
    method = c("categorical", "continuous"), population_id = "SAF",
    by = c(NA, "TRT01A"), variables = c("TRT01A", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

# A study of the YAML spec, in step with its files, with: DM extending the
# defaults (code list SEX there); DM2 extending the pattern pp (TRT01A and
# SEX there); DM3 extending the defaults, with a list SEX of its own; and
# spec/codelists.yml (lists to copy from, no report's) if `file`.
cl_study <- function(id, file = TRUE) {
  p <- cl_planner()
  a <- p$ard$analyses
  a[3, ] <- NA
  a[3, c("output_id", "analysis_id", "method", "population_id", "variables")] <-
    c("DM", "SEX", "categorical", "SAF", "SEX")
  p$ard$analyses <- .normalize_ard_sheet(a, "analyses")
  p <- copy_output(p, "DM", "DM2")
  p <- copy_output(p, "DM", "DM3")
  p <- set_codelist(p, "DM3", data.frame(variable = "SEX", value = c("F", "M"),
                                         label = c("OwnF", "OwnM"), order = c("1", "2")))
  s <- create_study(id, planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  sp <- file.path(s$path, "spec")
  cat("codelists:\n  SEX: [{value: F, label: DefF}, {value: M, label: DefM}]\n",
      file = file.path(sp, "defaults.yml"), append = TRUE)
  dir.create(file.path(sp, "patterns"))
  writeLines(c("codelists:", paste0("  ", cl_trt),
               "  SEX: [{value: F, label: PatF}, {value: M, label: PatM}]"),
             file.path(sp, "patterns", "pp.yml"))
  f <- file.path(sp, "outputs", "DM2.yml")
  writeLines(sub("^  - defaults$", "  - patterns/pp", readLines(f)), f)
  if (file) writeLines("ANRIND: [{value: LOW, label: Low}, {value: HIGH, label: High}]",
                       file.path(sp, "codelists.yml"))
  # (taken in as an open does it; open_study() itself stops on the file's
  # rows while the sheets' programs refuse them, a gap not in this change)
  sf <- .spec_load(s$path)
  s$planner <- .planner_from_spec(sf$study, sf$outputs)
  s <- .spec_attach(s)
  .write_state(s, tflplanner_home())
  s
}

cl_ard <- function(s, id) .ard_spec_code(.ard_spec(s$planner$ard), id,
                                         codelists = .study_codelists(s$planner))
cl_labels <- function(cl, id, var) cl$label[cl$output_id == id & cl$variable == var]

test_that("a study with spec/codelists.yml: no row without a report, and the ARD programs write", {
  cl_home()
  s <- cl_study("CL1")
  # the sheet keeps the file's rows (tflspec #651); the reports do not
  expect_true(anyNA(s$planner$sheets$codelists$output_id))
  cl <- .study_codelists(s$planner)
  expect_false(anyNA(cl$output_id))
  expect_false("ANRIND" %in% cl$variable)
  s2 <- save_study(s)
  expect_null(s2$ard_problem)
  for (id in c("DM", "DM2", "DM3")) {
    expect_true(file.exists(file.path(s$path, "programs", "ard", paste0(id, ".R"))))
  }
  expect_false(any(grepl("ANRIND", readLines(file.path(s$path, "programs", "ard", "DM.R")))))
})

test_that("a report takes its layers' lists: defaults, a pattern; its own list wins", {
  cl_home()
  s <- cl_study("CL2")
  cl <- .study_codelists(s$planner)
  expect_identical(cl_labels(cl, "DM", "SEX"), c("DefF", "DefM"))
  expect_identical(sort(unique(cl$variable[cl$output_id == "DM"])), "SEX")
  expect_identical(cl_labels(cl, "DM2", "SEX"), c("PatF", "PatM"))
  expect_identical(cl$value[cl$output_id == "DM2" & cl$variable == "TRT01A"],
                   c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  # (DM3 has the defaults' list too: its own is the one)
  expect_identical(cl_labels(cl, "DM3", "SEX"), c("OwnF", "OwnM"))
  # the sheet's columns, character like its rows
  expect_identical(names(cl), names(s$planner$sheets$codelists))
  expect_true(all(vapply(cl, is.character, NA)))
  # the programs
  prog <- cl_ard(s, "DM")
  expect_true("cl_sex <- c(F = \"DefF\", M = \"DefM\")" %in% prog)
  expect_true("cl_trt01a <- c(\"Placebo\", \"Xanomeline Low Dose\", \"Xanomeline High Dose\")" %in%
                cl_ard(s, "DM2"))
  expect_true("cl_sex <- c(F = \"OwnF\", M = \"OwnM\")" %in% cl_ard(s, "DM3"))
  expect_false(any(grepl("^cl_trt01a <- ", prog)))
})

test_that("an own list edited in the sheet wins at once; one removed there does not come back from the spec", {
  cl_home()
  s <- cl_study("CL3")
  p <- s$planner
  # DM: an own list, not yet taken into the spec (the sheet is ahead)
  p <- set_codelist(p, "DM", data.frame(variable = "SEX", value = c("M", "F"),
                                        label = c("EditM", "EditF"), order = c("1", "2")))
  expect_false(identical(.planner_body(p), p$spec$base))
  cl <- .study_codelists(p)
  expect_identical(cl_labels(cl, "DM", "SEX"), c("EditM", "EditF"))
  # DM3: the own list removed in the sheet: nothing for DM3's SEX, not the
  # spec's copy of it (nor the defaults')
  p$sheets$codelists <- p$sheets$codelists[!p$sheets$codelists$output_id %in% "DM3", , drop = FALSE]
  expect_identical(nrow(sheet_rows(p, "codelists", "DM3")), 0L)
  cl <- .study_codelists(p)
  expect_false("DM3" %in% cl$output_id)
  expect_identical(cl_labels(cl, "DM2", "SEX"), c("PatF", "PatM"))
})

test_that("a report that cannot be resolved does not stop the lists: the sheet's rows stand", {
  cl_home()
  s <- cl_study("CL4")
  p <- s$planner
  p$spec$outputs[["DM2"]]$extends <- "patterns/nothing_here"
  expect_error(tflspec::tfl_report_codelists(p$spec$study, p$spec$outputs))
  cl <- expect_no_error(.study_codelists(p))
  expect_identical(unique(cl$output_id), "DM3")
})

test_that("the lists are asked of tflspec once for the same spec", {
  cl_home()
  s <- cl_study("CL5")
  p <- s$planner
  a <- .study_codelists(p)
  k <- .report_codelists_cache$value
  expect_identical(.study_codelists(p), a)
  expect_identical(.report_codelists_cache$value, k)
  # another list in a layer: asked again
  p$spec$study$defaults$codelists$SEX[[1L]]$label <- "Girls"
  expect_identical(cl_labels(.study_codelists(p), "DM", "SEX")[1L], "Girls")
})

test_that("the ARD built from the layers' lists is built, not outdated (programs, tflspec's build, the batch's hash)", {
  skip_if_not_installed("cards")
  cl_home()
  s <- save_study(cl_study("CL6"))
  # the ARD program of the planner
  u <- update_study_ard(s, "DM2")
  expect_true(u$ok)
  expect_identical(ard_status(s)$state[ard_status(s)$output_id == "DM2"], "built")
  # the study ARD tflspec builds from the spec, with the lists it takes from
  # the layers: its fingerprint is the one the planner reads
  sp <- s$planner$spec$study
  suppressMessages(tflspec::tfl_build_ard(sp, dir = s$path, output_id = "DM", save = TRUE))
  expect_identical(ard_status(s)$state[ard_status(s)$output_id == "DM"], "built")
  # batch_code()'s hash, as it writes it
  spec <- structure(s$planner$ard, class = "tfl_ard_spec")
  st <- .read_ard_status(s)
  h <- tflspec::tfl_ard_spec_hash(spec, "DM", dir = s$path,
                                  codelists = .study_codelists(s$planner))
  expect_identical(h, st$definition[st$output_id == "DM"])
  # (and as it would be with the sheet's rows only)
  own <- s$planner$sheets$codelists
  own <- own[!is.na(own$output_id), , drop = FALSE]
  expect_false(identical(h, tflspec::tfl_ard_spec_hash(spec, "DM", dir = s$path, codelists = own)))
})

test_that("no spec: the sheet's lists as they are", {
  p <- cl_planner()
  expect_null(.study_codelists(p))
  p <- set_codelist(p, "DM", data.frame(variable = "SEX", value = c("F", "M"), order = c("1", "2")))
  expect_null(p$spec)
  expect_identical(.study_codelists(p), p$sheets$codelists)
  # a spec of no report: the same
  p$spec <- list(study = tflspec::tfl_read_study_spec(withr_tempdir()), outputs = list(),
                 base = .planner_body(p))
  expect_identical(.study_codelists(p), p$sheets$codelists)
})

test_that("the code lists' screen reads the sheet's rows of its report: the layers' lists are not shown", {
  cl_home()
  s <- cl_study("CL7")
  p <- s$planner
  # (R/codelists.R's grid: sheet_rows(rv$p, "codelists", report()))
  expect_identical(nrow(sheet_rows(p, "codelists", "DM")), 0L)
  expect_identical(nrow(sheet_rows(p, "codelists", "DM2")), 0L)
  expect_identical(sheet_rows(p, "codelists", "DM3")$label, c("OwnF", "OwnM"))
  expect_gt(sum(.study_codelists(p)$output_id == "DM"), 0L)
})
