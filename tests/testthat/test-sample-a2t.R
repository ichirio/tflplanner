# A2-T (tflspec#984, canon 11-15: the analysis set lives on the data):
# SAMPLE-01's tables say their set on the data rows that make their
# subjects, not in `common: population`.  The impact gate: every report's
# ARD program (its definition hash in it), its report program and its
# page's set line are the same as with the sets in `common:`, the form
# before (built here from the sample's own files).

# The sets the sample's reports had in `common: population` before A2-T
# (F-14-2-4 until tflplanner#858)
a2t_sets <- c(`T-14-1-1` = "SAF", `T-14-1-1C` = "SAF", `T-14-1-1S` = "SAF", `T-14-1-2` = "SAF",
              `T-14-1-3` = "SAF", `T-14-1-4` = "SCRF", `T-14-1-5` = "ENR", `T-14-1-6` = "ENR",
              `T-14-2-1` = "SAF", `T-14-2-2` = "SAF", `T-14-2-3` = "SAF", `T-14-3-1` = "SAF",
              `T-14-3-1C` = "SAF", `F-14-2-4` = "SAF")

a2t_sample_dir <- function() system.file("sample", "SAMPLE-01", package = "tflplanner")

# A report in the form before: `set` in `common: population`, taken off
# the data rows that make its subjects (of ADSL, of no other's subjects,
# no code) -- the rows tflspec gives `common`'s set to
a2t_old_form <- function(o, set) {
  strip <- function(rows) lapply(rows, function(r) {
    if (is.list(r) && identical(r[["from"]], "ADSL") && is.null(r[["subjects_in"]]) &&
        is.null(r[["code"]]) && identical(r[["population"]], set)) {
      r[["population"]] <- NULL
    }
    r
  })
  if (is.list(o$data) && is.null(names(o$data))) o$data <- strip(o$data)
  if (is.list(o$ard$analysis_data)) o$ard$analysis_data <- strip(o$ard$analysis_data)
  o$common <- c(list(population = set), o$common)
  o
}

test_that("no report of the sample writes its analysis set in `common:` (A2-T); T-14-1-1C keeps its treatment", {
  fs <- list.files(a2t_sample_dir(), pattern = "[.]ya?ml$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(fs), 19L)
  in_common <- function(x) {
    is.list(x) && ((is.list(x[["common"]]) && !is.null(x[["common"]][["population"]])) ||
                     any(vapply(x, in_common, NA)))
  }
  has <- vapply(fs, function(f) in_common(yaml::read_yaml(f, handlers = list(r = function(x) x))), NA)
  expect_identical(basename(fs[has]), character())
  o <- yaml::read_yaml(file.path(a2t_sample_dir(), "spec", "outputs", "T-14-1-1C.yml"),
                       handlers = list(r = function(x) x))
  expect_identical(o$common, list(treatment = "TRT01A"))
})

test_that("SAMPLE-01's ARD programs, report programs and page set lines are the same with the sets in `common:` (the impact gate, A2-T)", {
  skip_on_cran()
  skip_if_not("tfl_set_report_set" %in% getNamespaceExports("tflspec"),
              "this tflspec reads a table's set from `common:` only (tflspec#984)")
  root <- withr::local_tempdir()
  for (k in c("new", "old")) {
    dir.create(file.path(root, k))
    expect_true(file.copy(a2t_sample_dir(), file.path(root, k), recursive = TRUE))
  }
  new <- file.path(root, "new", "SAMPLE-01")
  old <- file.path(root, "old", "SAMPLE-01")
  od <- file.path(old, "spec", "outputs")
  for (id in names(a2t_sets)) {
    f <- file.path(od, paste0(id, ".yml"))
    o <- tflspec::tfl_read_output(f)
    # (each set read where it moved to: the report's data)
    expect_identical(tflspec::tfl_report_set(o), a2t_sets[[id]], info = id)
    tflspec::tfl_write_output(a2t_old_form(o, a2t_sets[[id]]), f)
  }
  sn <- tflspec::tfl_read_study_spec(new)
  so <- tflspec::tfl_read_study_spec(old)
  on <- tflspec::tfl_read_outputs(new)
  oo <- tflspec::tfl_read_outputs(old)
  expect_length(on, 19L)
  expect_identical(names(oo), names(on))
  # (the old form read as the old form: the set in `common:`)
  expect_identical(oo[["T-14-1-5"]]$common$population, "ENR")
  same <- function(f) tryCatch(f(), error = function(e) paste("error:", conditionMessage(e)))
  # (the page's {OUTPUT_POPULATION}: the set's label; NA none)
  set_line <- function(o, st) {
    v <- tflspec::tfl_report_tokens(o, study = st)
    if ("OUTPUT_POPULATION" %in% names(v)) v[["OUTPUT_POPULATION"]] else NA_character_
  }
  no_tok <- function(o) {
    if (is.list(o$page$tokens)) o$page$tokens$OUTPUT_POPULATION <- NULL
    o
  }
  for (id in names(on)) {
    expect_identical(same(function() tflspec::tfl_ard_code(sn, id, save = TRUE)),
                     same(function() tflspec::tfl_ard_code(so, id, save = TRUE)), info = id)
    expect_identical(same(function() tflspec::tfl_report_code(on[[id]], study = sn)),
                     same(function() tflspec::tfl_report_code(oo[[id]], study = so)), info = id)
    expect_identical(set_line(on[[id]], sn), set_line(oo[[id]], so), info = id)
    # (and the line the set gives when the page names none)
    expect_identical(set_line(no_tok(on[[id]]), sn), set_line(no_tok(oo[[id]]), so), info = id)
  }
  expect_identical(set_line(no_tok(on[["T-14-1-5"]]), sn), "Screened Set")
})
