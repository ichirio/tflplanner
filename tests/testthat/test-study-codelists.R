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
# a left-over spec/codelists.yml (read by no one since tflspec #659) if
# `file`.
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
  # (a pattern holds no page: that is a page configuration's, tflspec PP-1b)
  writeLines(c("codelists:", paste0("  ", cl_trt),
               "  SEX: [{value: F, label: PatF}, {value: M, label: PatM}]"),
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
# a table program without its plan_labels() step: tflspec writes one merged
# step from the study's code lists when the program is generated from the
# spec (tflspec #796); the view ahead of the spec (the sheets path) has none
cl_unlabel <- function(x) {
  i <- grep("  plan_labels(", x, fixed = TRUE)
  if (!length(i)) return(x)
  j <- if (endsWith(x[i[1L]], ")")) i[1L] else i[1L] + which(x[-seq_len(i[1L])] == "  )")[1L]
  x <- x[-(i[1L]:j)]
  x[i[1L] - 1L] <- sub(" |>", "", x[i[1L] - 1L], fixed = TRUE)
  x
}

cl_ard <- function(s, id) .ard_spec_code(.ard_spec(s$planner$ard), id,
                                         codelists = .study_codelists(s$planner))
cl_labels <- function(cl, id, var) cl$label[cl$output_id == id & cl$variable == var]

test_that("a left-over spec/codelists.yml is ignored: its lists reach no report and no sheet", {
  cl_home()
  s <- cl_study("CL1")
  cp <- file.path(s$path, "spec", "codelists.yml")
  expect_true(file.exists(cp))
  # (the planner's spec has not read it: tflspec says so, once)
  expect_false(file.path("spec", "codelists.yml") %in% names(s$planner$spec$text))
  expect_false(anyNA(s$planner$sheets$codelists$output_id))
  expect_false("ANRIND" %in% s$planner$sheets$codelists$variable)
  cl <- .study_codelists(s$planner)
  expect_false(anyNA(cl$output_id))
  expect_false("ANRIND" %in% cl$variable)
  s2 <- save_study(s)
  expect_null(s2$ard_problem)
  for (id in c("DM", "DM2", "DM3")) {
    expect_true(file.exists(file.path(s$path, "programs", "ard", paste0(id, ".R"))))
  }
  expect_false(any(grepl("ANRIND", readLines(file.path(s$path, "programs", "ard", "DM.R")))))
  # (nor the ARS)
  txt <- paste(utils::capture.output(utils::str(.ars_build(s2)$ars, max.level = 99)),
               collapse = "
")
  expect_false(grepl("ANRIND", txt, fixed = TRUE))
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

# ---- spec/codelists.yml: left over, ignored -----------------------------------
# A row with no report can only be an old workbook's now; tflspec's
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

test_that("spec/codelists.yml added outside the app is not taken in; the file changed with it is", {
  cl_home()
  s <- cl_plain("CL10")
  cl_outside(s)
  o <- open_study("CL10")
  expect_identical(o$spec$status, "adopted")
  expect_null(o$spec$problems)
  expect_identical(o$spec$files$file, file.path("spec", "outputs", "DM.yml"))
  expect_false(any(grepl("codelists", o$spec$parts)))
  expect_identical(o$planner$outputs$description[o$planner$outputs$output_id == "DM"], "Changed outside")
  expect_false("ANRIND" %in% o$planner$sheets$codelists$variable)
  expect_false(anyNA(o$planner$sheets$codelists$output_id))
})

test_that("a program is written with the view ahead of the spec, like the one from the spec", {
  cl_home()
  s <- cl_study("CL11")
  p <- s$planner
  expect_false(anyNA(p$sheets$codelists$output_id))
  expect_false(is.null(.code_spec(p)))
  ahead <- cl_ahead(p)
  expect_null(.code_spec(ahead))
  for (id in c("DM", "DM2", "DM3")) {
    a <- program_code(p, id)
    b <- expect_no_error(program_code(cl_ahead(p, id), id))
    expect_false(any(grepl("does not hold", b, fixed = TRUE)))
    # the inherited lists are the ARD program's (set_levels()): neither
    # table program has a line for them; the labels reach the table through
    # one plan_labels() (the sheets path has the lists its sheets hold: an
    # own list, not the inherited ones)
    # (DM2 extends a pattern with no normalize_ard(): its two paths differ
    # there, not in the lists)
    if (id != "DM2") expect_identical(cl_body(cl_unlabel(b)), cl_body(cl_unlabel(a)))
    if (id == "DM") {
      expect_true(any(grepl("plan_labels(SEX = c(F = \"DefF\", M = \"DefM\"))", a, fixed = TRUE)))
    }
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
  expect_identical(cl_body(program_code(ahead, "DM")),
                   cl_body(cl_unlabel(program_code(p, "DM"))))
  expect_false(any(grepl("agegr1", program_code(ahead, "DM"), ignore.case = TRUE)))
  # (the spec path's plan_labels() has the list of a variable the ARD does
  # not read: a label no cell uses)
  expect_true(any(grepl("Young", program_code(p, "DM"), fixed = TRUE)))
})

test_that("the ARS reads a report's inherited lists: a condition on a label stops", {
  skip_if_not_installed("cards")
  # tflspec #796: the data hold the codes, so a `where` written with a label
  # stops in the ARS as in the ARD program, and says the code to write
  cl_home()
  s <- cl_study("CL13")
  p <- s$planner
  i <- which(p$ard$analyses$output_id == "DM2" & p$ard$analyses$analysis_id == "AGE")
  p$ard$analyses$where[i] <- "SEX == \"PatF\""
  s$planner <- p
  # (PatF is the pattern's label of F, a list DM2 has from its layer only)
  expect_error(.ars_build(s), "SEX: write \"F\" for \"PatF\"", fixed = TRUE)
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
  # (and still no list of it in a report's)
  expect_false("ANRIND" %in% open_study("CL14")$planner$sheets$codelists$variable)
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

test_that("rows with no report are an old workbook's: the old format; a left-over file's lists are not", {
  cl_home()
  s <- cl_study("CL16")
  expect_false(anyNA(s$planner$sheets$codelists$output_id))
  expect_false("codelists" %in% .old_format(s$planner))
  # a planner of a workbook with a row of no report
  p <- s$planner
  p$spec <- NULL
  p$sheets$codelists[nrow(p$sheets$codelists) + 1L, ] <- NA
  p$sheets$codelists[nrow(p$sheets$codelists), c("variable", "value")] <- c("ANRIND", "LOW")
  expect_true("codelists" %in% .old_format(p))
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

test_that("a change to a left-over spec/codelists.yml is no change to the study", {
  cl_home()
  s <- cl_study("CL18")
  cp <- file.path(s$path, "spec", "codelists.yml")
  writeLines("ANRIND: [{value: LOW, label: Lower}, {value: HIGH, label: High}]", cp)
  md5 <- unname(tools::md5sum(cp))
  o <- open_study("CL18")
  expect_identical(o$spec$status, "same")
  expect_null(o$spec$parts)
  expect_false("Lower" %in% o$planner$sheets$codelists$label)
  expect_false(anyNA(o$planner$sheets$codelists$output_id))
  expect_identical(unname(tools::md5sum(cp)), md5)
  # removed outside: no change either
  unlink(cp)
  expect_identical(open_study("CL18")$spec$status, "same")
})

test_that("the other readers of the sheets leave an old workbook's rows with no report out too", {
  cl_home()
  s <- cl_study("CL19")
  s$planner$sheets$codelists[nrow(s$planner$sheets$codelists) + 1L, ] <- NA
  s$planner$sheets$codelists[nrow(s$planner$sheets$codelists), c("variable", "value")] <-
    c("ANRIND", "LOW")
  expect_true(anyNA(s$planner$sheets$codelists$output_id))
  expect_false(anyNA(.study_table_spec(s)$codelists$output_id))
  # (preview_pages() builds the table spec first: it stops later, on no data)
  msg <- tryCatch(preview_pages(s$planner, "DM", data.frame()),
                  error = function(e) conditionMessage(e))
  expect_false(grepl("has no `output_id`", msg, fixed = TRUE))
})

# ---- a study saved before tflspec #659 ----------------------------------------
# Its record lists spec/codelists.yml, and the folder still has the file.

# the messages of `expr` (tflspec's, once a read) by their text
cl_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  msgs
}

test_that("a study recorded with spec/codelists.yml opens as it was: no change, no parts, no error", {
  cl_home()
  s <- cl_study("CL20")
  cp <- file.path(s$path, "spec", "codelists.yml")
  md5 <- unname(tools::md5sum(cp))
  rel <- file.path("spec", "codelists.yml")
  # the record of the earlier version: the file in its fingerprints
  f <- .state_file("CL20")
  st <- .read_state("CL20")
  expect_false(rel %in% names(st$files))
  st$files[[rel]] <- list(md5 = md5, size = file.size(cp))
  writeLines(.json(st), f)
  expect_true(rel %in% names(.read_state("CL20")$files))
  before <- s$planner$sheets$codelists
  msgs <- cl_messages(o <- open_study("CL20"))
  expect_identical(o$spec$status, "same")
  expect_null(o$spec$parts)
  expect_null(o$spec$problems)
  expect_identical(o$planner$sheets$codelists, before)
  expect_false(anyNA(o$planner$sheets$codelists$output_id))
  expect_identical(.study_codelists(o$planner), .study_codelists(s$planner))
  # (tflspec says once per open that the file is not read)
  expect_identical(sum(grepl("codelists.yml", msgs, fixed = TRUE)), 1L)
  # a repeat open and a save are quiet too
  msgs2 <- cl_messages(o2 <- open_study("CL20"))
  expect_identical(o2$spec$status, "same")
  expect_identical(o2$planner$sheets$codelists, before)
  expect_identical(sum(grepl("codelists.yml", msgs2, fixed = TRUE)), 1L)
  msgs3 <- cl_messages(o3 <- save_study(o2))
  expect_identical(sum(grepl("codelists.yml", msgs3, fixed = TRUE)), 0L)
  expect_null(o3$ard_problem)
  expect_identical(o3$planner$sheets$codelists, before)
  expect_identical(unname(tools::md5sum(cp)), md5)
  expect_identical(open_study("CL20")$spec$status, "same")
  expect_false(rel %in% names(.read_state("CL20")$files))
})

test_that("an import takes no spec/codelists.yml in: alone it is no definition file, in a folder it is left out", {
  cl_home()
  s <- cl_study("CL21")
  d <- withr_tempdir()
  one <- file.path(d, "lists.yml")
  writeLines(cl_file_text, one)
  expect_true(is.na(.import_place(one, character())))
  expect_error(preview_spec_import("CL21", one), "No definition file")
  # a copy of the study folder (an export) with the file in it
  dir.create(file.path(d, "copy", "spec"), recursive = TRUE)
  file.copy(file.path(s$path, "study.yml"), file.path(d, "copy", "study.yml"))
  writeLines(cl_file_text, file.path(d, "copy", "spec", "codelists.yml"))
  expect_true(is.na(.import_place(file.path(d, "copy", "spec", "codelists.yml"), character(),
                                  "spec/codelists.yml")))
  stage <- .stage_import(s$path, file.path(d, "copy"), output_ids(s$planner))
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  expect_false("codelists.yml" %in% basename(attr(stage, "placed")))
})
