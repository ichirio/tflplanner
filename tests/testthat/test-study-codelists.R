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
  # (a pattern with a page: tflspec's sheets name a pattern by its page
  # rows, and a pattern with none is refused by the report sheet)
  writeLines(c("codelists:", paste0("  ", cl_trt),
               "  SEX: [{value: F, label: PatF}, {value: M, label: PatM}]",
               "page:", "  titles:", "    1: {center: Pattern title}"),
             file.path(sp, "patterns", "pp.yml"))
  f <- file.path(sp, "outputs", "DM2.yml")
  writeLines(sub("^  - defaults$", "  - patterns/pp", readLines(f)), f)
  if (file) writeLines(cl_file_text, file.path(sp, "codelists.yml"))
  # (taken in as an open does it: the files changed outside are read)
  open_study(id)
}

cl_file_text <- "ANRIND: [{value: LOW, label: Low}, {value: HIGH, label: High}]"

# A study of one report (DM) in step with its files, nothing in the layers
cl_plain <- function(id) {
  s <- create_study(id, planner = cl_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  save_study(s)
}

# spec/codelists.yml added outside the app, with a change to DM's file (an
# open then checks the programs of the reports it changes)
cl_outside <- function(s) {
  writeLines(cl_file_text, file.path(s$path, "spec", "codelists.yml"))
  f <- file.path(s$path, "spec", "outputs", "DM.yml")
  x <- readLines(f)
  writeLines(sub("^id: DM$", "id: DM\nlabel: Changed outside", x), f)
  invisible(s)
}

# the planner with an edit the spec has not taken in (the view ahead)
cl_ahead <- function(p, id = "DM") {
  p$outputs$description[p$outputs$output_id == id] <- "Ahead of the spec"
  p
}
# a program without its banner (the date, the description)
cl_body <- function(x) x[!grepl("^#", x)]

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

test_that("an own list edited in the sheet wins at once; one removed there is the layer's again, or none", {
  cl_home()
  s <- cl_study("CL3")
  p <- s$planner
  # DM: an own list, not yet taken into the spec (the sheet is ahead)
  p <- set_codelist(p, "DM", data.frame(variable = "SEX", value = c("M", "F"),
                                        label = c("EditM", "EditF"), order = c("1", "2")))
  expect_false(identical(.planner_body(p), p$spec$base))
  cl <- .study_codelists(p)
  expect_identical(cl_labels(cl, "DM", "SEX"), c("EditM", "EditF"))
  # DM3: the own list removed in the sheet: the report has none of its own
  # any more, so the defaults' list is its list again (not the spec's copy
  # of the own one)
  p$sheets$codelists <- p$sheets$codelists[!p$sheets$codelists$output_id %in% "DM3", , drop = FALSE]
  expect_identical(nrow(sheet_rows(p, "codelists", "DM3")), 0L)
  cl <- .study_codelists(p)
  expect_identical(cl_labels(cl, "DM3", "SEX"), c("DefF", "DefM"))
  expect_identical(cl_labels(cl, "DM2", "SEX"), c("PatF", "PatM"))
  # a report removed from the sheets has no layer rows either
  q <- p
  q$outputs <- q$outputs[q$outputs$output_id != "DM2", , drop = FALSE]
  expect_false("DM2" %in% .study_codelists(q)$output_id)
})

test_that("an own list with no layer behind it, removed in the sheet, gives no rows and does not come back from the spec", {
  cl_home()
  s <- cl_study("CL3B")
  p <- set_codelist(s$planner, "DM3", data.frame(variable = "AGEGR1", value = c("a", "b"),
                                                 label = c("A", "B"), order = c("1", "2")))
  p <- save_study(`$<-`(s, "planner", p))$planner
  expect_identical(cl_labels(.study_codelists(p), "DM3", "AGEGR1"), c("A", "B"))
  expect_identical(.planner_body(p), p$spec$base)
  p$sheets$codelists <- p$sheets$codelists[!(p$sheets$codelists$output_id %in% "DM3" &
                                               p$sheets$codelists$variable %in% "AGEGR1"), , drop = FALSE]
  expect_false(identical(.planner_body(p), p$spec$base))
  expect_false("AGEGR1" %in% .study_codelists(p)$variable)
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

# ---- spec/codelists.yml: lists to copy from, no report's ---------------------
# The sheet keeps its rows (no output_id) for the round trip; tflspec's
# tfl_table_spec() refuses such a row, so every call that hands the sheets
# to tflspec leaves them out (.codelists_of_reports()).

test_that(".codelists_of_reports() drops the rows with no report and nothing else", {
  expect_null(.codelists_of_reports(NULL))
  d <- data.frame(output_id = c(NA, "DM"), variable = c("A", "SEX"), value = c("x", "F"))
  expect_identical(.codelists_of_reports(d)$variable, "SEX")
  e <- d[0, ]
  expect_identical(.codelists_of_reports(e), e)
  expect_identical(.codelists_of_reports(d["variable"]), d["variable"])
})

test_that("a study with spec/codelists.yml added outside the app opens, taking the file in", {
  cl_home()
  s <- cl_plain("CL10")
  cl_outside(s)
  o <- open_study("CL10")
  expect_identical(o$spec$status, "adopted")
  expect_null(o$spec$problems)
  expect_true(anyNA(o$planner$sheets$codelists$output_id))
})

test_that("a program is written with the view ahead of the spec, like the one from the spec", {
  cl_home()
  s <- cl_study("CL11")
  p <- s$planner
  expect_true(anyNA(p$sheets$codelists$output_id))
  expect_false(is.null(.code_spec(p)))
  ahead <- cl_ahead(p)
  expect_null(.code_spec(ahead))
  for (id in c("DM", "DM2", "DM3")) {
    a <- program_code(p, id)
    b <- expect_no_error(program_code(cl_ahead(p, id), id))
    expect_false(any(grepl("does not hold", b, fixed = TRUE)))
    # the inherited lists are the ARD program's: neither table program has
    # a line for them (the sheets path with them would write plan_levels()
    # for a variable the ARD does not read, which the spec path does not).
    # (DM2 extends a pattern with no normalize_ard(): its two paths differ
    # there, not in the lists)
    if (id != "DM2") expect_identical(cl_body(b), cl_body(a))
  }
})

test_that("the sheets path takes no inherited list a variable the ARD does not read would write", {
  cl_home()
  s <- cl_study("CL12")
  p <- s$planner
  cat("  AGEGR1: [{value: '<65', label: Young}, {value: '>=65', label: Old}]\n",
      file = file.path(s$path, "spec", "defaults.yml"), append = TRUE)
  p <- open_study("CL12")$planner
  ahead <- cl_ahead(p)
  expect_true("AGEGR1" %in% .study_codelists(p)$variable)
  expect_identical(cl_body(program_code(ahead, "DM")), cl_body(program_code(p, "DM")))
  expect_false(any(grepl("agegr1", program_code(ahead, "DM"), ignore.case = TRUE)))
})

test_that("the ARS reads a report's inherited lists: a condition on a label is the data's value", {
  skip_if_not_installed("cards")
  cl_home()
  s <- cl_study("CL13")
  p <- s$planner
  i <- which(p$ard$analyses$output_id == "DM2" & p$ard$analyses$analysis_id == "AGE")
  p$ard$analyses$where[i] <- "SEX == \"PatF\""
  s$planner <- p
  txt <- paste(utils::capture.output(utils::str(.ars_build(s)$ars, max.level = 99)),
               collapse = "\n")
  # (PatF is the pattern's label of F, a list DM2 has from its layer only)
  expect_true(grepl("SEX == \\\"F\\\"", txt, fixed = TRUE))
  expect_false(grepl("PatF", txt, fixed = TRUE))
})

test_that("the study is saved after an edit, and spec/codelists.yml is left as it is", {
  cl_home()
  s <- cl_plain("CL14")
  cl_outside(s)
  o <- open_study("CL14")
  cp <- file.path(s$path, "spec", "codelists.yml")
  md5 <- unname(tools::md5sum(cp))
  p <- cl_ahead(o$planner)
  o$planner <- p
  # the edit taken into the spec (the bridge), then saved
  y <- expect_no_error(.spec_sync(p, o$meta))
  expect_identical(unname(tools::md5sum(cp)), md5)
  o2 <- expect_no_error(save_study(o))
  expect_identical(unname(tools::md5sum(cp)), md5)
  # written back from the last save, and a save over the files
  expect_no_error(write_spec(o2))
  expect_identical(unname(tools::md5sum(cp)), md5)
  expect_no_error(save_study(o2, spec = "overwrite"))
  expect_identical(unname(tools::md5sum(cp)), md5)
  # (and it still reads, with its rows)
  expect_true(anyNA(open_study("CL14")$planner$sheets$codelists$output_id))
})

test_that("a file added outside is not removed by a save over the files; a study without it gets none", {
  cl_home()
  s <- cl_plain("CL15")
  cp <- file.path(s$path, "spec", "codelists.yml")
  expect_false(file.exists(cp))
  write_spec(s)
  save_study(s, spec = "overwrite")
  expect_false(file.exists(cp))
  # added outside after the last save: the save over the files leaves it
  writeLines(cl_file_text, cp)
  md5 <- unname(tools::md5sum(cp))
  save_study(s, spec = "overwrite")
  expect_identical(unname(tools::md5sum(cp)), md5)
})

test_that("rows of spec/codelists.yml are no old format; report-less rows of a planner without the file are", {
  cl_home()
  s <- cl_study("CL16")
  expect_true(anyNA(s$planner$sheets$codelists$output_id))
  expect_true(.codelists_file() %in% names(s$planner$spec$text))
  expect_false("codelists" %in% .old_format(s$planner))
  # the same rows, the planner without that file (an old workbook's)
  p <- s$planner
  p$spec <- NULL
  expect_true("codelists" %in% .old_format(p))
  q <- s$planner
  q$spec$text[[.codelists_file()]] <- NULL
  expect_true("codelists" %in% .old_format(q))
})

test_that("a re-saved spec file with only its line ends changed is no change to the study", {
  cl_home()
  s <- cl_study("CL17")
  f <- file.path(s$path, "spec", "outputs", "DM.yml")
  # (the other line ends than the file has, so its bytes do change: on
  # Windows writeLines() has already written these with CRLF)
  raw <- readBin(f, "raw", file.size(f))
  x <- readLines(f)
  eol <- if (as.raw(13L) %in% raw) "\n" else "\r\n"
  writeBin(charToRaw(paste0(paste(x, collapse = eol), eol)), f)
  expect_false(identical(readBin(f, "raw", file.size(f)), raw))
  o <- open_study("CL17")
  expect_identical(o$spec$status, "touched")
  expect_null(o$spec$parts)
})

test_that("a real change to spec/codelists.yml is still a change", {
  cl_home()
  s <- cl_study("CL18")
  writeLines("ANRIND: [{value: LOW, label: Lower}, {value: HIGH, label: High}]",
             file.path(s$path, "spec", "codelists.yml"))
  o <- open_study("CL18")
  rows <- o$planner$sheets$codelists
  expect_identical(rows$label[is.na(rows$output_id) & rows$value == "LOW"], "Lower")
  expect_identical(o$spec$status, "adopted")
  expect_true("sheet:codelists" %in% o$spec$parts)
})

test_that("the other readers of the sheets leave the rows with no report out too", {
  cl_home()
  s <- cl_study("CL18")
  expect_true(anyNA(s$planner$sheets$codelists$output_id))
  expect_false(anyNA(.study_table_spec(s)$codelists$output_id))
  # (preview_pages() builds the table spec first: it stops later, on no data)
  msg <- tryCatch(preview_pages(s$planner, "DM", data.frame()),
                  error = function(e) conditionMessage(e))
  expect_false(grepl("has no `output_id`", msg, fixed = TRUE))
})
