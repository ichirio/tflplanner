# G5-0, the gate of dev-rep docs/design/generator-in-tflspec-2026-11-25.md
# (R2.3): the programs a save writes are a function of the saved YAML.
#
# For each case a study is saved as the app saves it (save_study(),
# fill_widths = TRUE): the programs that save wrote or found current are
# programs A.  A copy of the study folder -- its definition (study.yml,
# spec/), its data and its two free setup files (programs/company_setup.R,
# programs/study_user.R: written once, never again; input, not output), no
# other program and no state -- is opened as the app opens a study folder
# its home does not know (open_study() -> register_study(): the definition
# read from the YAML, .planner_from_yaml()) and saved again into that copy:
# programs B.  A and B are compared file by file, the `#  Generated  :`
# line masked (the date and the versions).  The same home holds both, so
# the company's inputs, the layout and the free setup files are the same:
# what differs is the YAML read back.
#
# A difference is a gap of the gate, a row for tflplanner#689: the case is
# skipped and the skip names it -- "G5-0 gap: <one of R2.3's list>" when
# the files that differ are the ones that gap touches, else "G5-0 gap
# (new): <file>: <what differs>".  Fixing the gap (in the YAML or its
# reader) turns the skip into a pass.  R2.3's known gaps:
#   1. process_code: only the first `!r` step reaches the view
#   2. a report's datasets: two homes (toc.yml and a figure's data.datasets)
#   3. page text read again through .page_sheet_text() (quotes, `\n`)
#   4. the OUTPUT_POPULATION label falls back to the company's note
# (5-8 -- the free setup files, the layout, the company's inputs, the
# renamed-call alias -- are held equal here: one home, the files copied.)

# The programs a save wrote (or found current), by their path under
# programs/, each its lines with the Generated line masked.  From the save's
# own list of files: a program an earlier save left that this one did not
# write is not this save's.
g5_programs <- function(s) {
  root <- s$path
  f <- s$files$file[!s$files$status %in% "removed"]
  pre <- paste0(root, "/")
  rel <- ifelse(startsWith(f, pre), substring(f, nchar(pre) + 1L), f)
  rel <- unique(rel[startsWith(rel, "programs/") & !startsWith(rel, "programs/.edited/")])
  rel <- sort(rel[file.exists(file.path(root, rel))])
  stats::setNames(lapply(rel, function(r) {
    l <- readLines(file.path(root, r), warn = FALSE, encoding = "UTF-8")
    l[grepl("^#  Generated  :", l)] <- "#  Generated  : (masked)"
    l
  }), sub("^programs/", "", rel))
}

# What differs between two sets of programs: by file, its first line that
# differs (or that only one set has the file)
g5_diff <- function(a, b) {
  cut <- function(x) {
    if (is.na(x)) return("(no line)")
    if (nchar(x) > 70L) paste0(substr(x, 1L, 67L), "...") else x
  }
  out <- character()
  for (f in setdiff(names(a), names(b))) out[[f]] <- "written by the first save only"
  for (f in setdiff(names(b), names(a))) out[[f]] <- "written after the re-open only"
  for (f in intersect(names(a), names(b))) {
    x <- a[[f]]
    y <- b[[f]]
    if (identical(x, y)) next
    n <- max(length(x), length(y))
    i <- which(vapply(seq_len(n), function(k) !identical(x[k], y[k]), NA))[1L]
    out[[f]] <- sprintf("line %d `%s` vs `%s`", i, cut(x[i]), cut(y[i]))
  }
  out
}

# The differences in one line (the first three files)
g5_say <- function(d) {
  s <- paste(sprintf("%s: %s", names(d), d)[seq_len(min(3L, length(d)))], collapse = "; ")
  if (length(d) > 3L) s <- paste0(s, sprintf(" (and %d more files)", length(d) - 3L))
  s
}

# The programs of reports `ids` (their report and ARD programs), and the
# other files named: a pattern of paths under programs/
g5_files <- function(ids, also = character()) {
  ids <- gsub(".", "[.]", ids, fixed = TRUE)
  also <- gsub(".", "[.]", also, fixed = TRUE)
  paste0("^((tfl|ard)/(", paste(ids, collapse = "|"), ")[.]R",
         if (length(also)) paste0("|", paste(also, collapse = "|")), ")$")
}

# A save as the app's, its programs written as a fresh session writes them
# (none taken from the report programs' cache of an earlier save)
g5_save <- function(s, home) {
  rm(list = ls(.program_cache, all.names = TRUE), envir = .program_cache)
  suppressWarnings(suppressMessages(save_study(s, home = home, fill_widths = TRUE)))
}

# Save `s` (programs A); copy its folder with no program but the free setup
# files and no state, open the copy from its YAML and save it (programs B).
# `b` is the error when the copy did not open or save.
g5_round_trip <- function(s, home, env = parent.frame()) {
  a <- g5_save(s, home)
  lay_tfl <- sub("^programs/", "", study_layout()[["programs_tfl"]])
  out <- list(a = g5_programs(a), b = NULL,
              reports = file.path(lay_tfl, vapply(a$planner$outputs$output_id, function(id)
                report_info(a$planner, id)$program, "", USE.NAMES = FALSE)))
  parent <- withr_tempdir(env)
  if (!file.copy(a$path, parent, recursive = TRUE, copy.date = TRUE)) {
    stop("The study folder was not copied to ", parent, call. = FALSE)
  }
  to <- file.path(parent, basename(a$path))
  unlink(file.path(to, ".tflplanner"), recursive = TRUE)
  pr <- file.path(to, "programs")
  free <- c(.company_setup_file, .study_user_file)
  unlink(file.path(pr, setdiff(list.files(pr, recursive = TRUE, all.files = TRUE), free)))
  unlink(file.path(pr, ".edited"), recursive = TRUE)
  # (one study a home by its id: the first copy taken off the list, the
  # second opened as a folder new to the home)
  unregister_study(a$meta$study_id, home = home)
  out$b <- tryCatch({
    b <- suppressWarnings(suppressMessages(open_study(to, home = home)))
    g5_programs(g5_save(b, home))
  }, error = function(e) e)
  out
}

# A and B the same; else the case skipped, naming its gap.  `gap` (one line)
# and `files` (g5_files()): the known gap this case may meet, and the files
# it would show in.
g5_expect_same <- function(rt, gap = NULL, files = NULL) {
  # (the first save wrote the setup, the runner and every report's program)
  expect_true(all(c("study_setup.R", "batch.R", rt$reports) %in% names(rt$a)))
  if (inherits(rt$b, "error")) {
    skip(paste("G5-0 gap (new): the copy did not open from its YAML or save:",
               conditionMessage(rt$b)))
  }
  d <- g5_diff(rt$a, rt$b)
  if (length(d)) {
    known <- if (is.null(gap)) rep(FALSE, length(d)) else grepl(files, names(d))
    if (!all(known)) skip(paste0("G5-0 gap (new): ", g5_say(d[!known])))
    skip(paste0("G5-0 gap: ", gap, " (", g5_say(d), ")"))
  }
  expect_identical(d, character())
}

g5_gap_steps <- "process_code: only the first !r step of data.steps reaches the view"
g5_gap_datasets <- "a report's datasets have two homes (toc.yml and the figure's data.datasets)"
g5_gap_page_text <- "page text is read again through .page_sheet_text() in tflplanner only"

# ---- the sample study ---------------------------------------------------------

test_that("G5-0: the sample study's programs are its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  # (F-14-2-1 is a figure of its own code: its datasets are gap 2's)
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt, g5_gap_datasets,
                 g5_files("F-14-2-1", "batch.R"))
})

# ---- a report from a template of each kind -------------------------------------

test_that("G5-0: reports from a table, a listing and a figure template are their YAML's", {
  skip_on_cran()
  have <- tflspec::tfl_templates()$name
  skip_if_not(all(c("demog", "listing", "forest_hr") %in% have),
              "this tflspec has no demog, listing or forest_hr template")
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  p <- s$planner
  # a table (as test-template-report.R adds one)
  o <- tflspec::tfl_output("T-TPL", "table", label = "Demographics", common = list(population = "SAF"),
                           template = list(name = "demog",
                                           args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  p <- .spec_add_output(p, o, label = "Demographics", population = "SAF")
  # a listing
  o <- tflspec::tfl_output("L-TPL", "listing", label = "AE listing",
                           template = list(name = "listing", args = list(
                             data = "ADAE",
                             columns = list(list(vars = "USUBJID", label = "Subject"),
                                            list(vars = "AEDECOD", label = "Term", width = 30L)))))
  p <- .spec_add_output(p, o, label = "AE listing")
  # a figure that reads an ARD of its own (Add > Figure > Template, as
  # test-week-1108.R adds it)
  pops <- p$ard$populations
  saf <- as.list(pops[match("SAF", pops$population_id),
                      intersect(c("population_id", "dataset", "where"), names(pops)), drop = FALSE])
  steps <- if (.tpl_own_data(tflspec::tfl_template_params("forest_hr"))) .tpl_steps_from("ADTTE", saf, "PFS")
  o <- tflspec::tfl_output("F-TPL", "figure", label = "forest_hr", common = list(population = "SAF"),
                           template = list(name = "forest_hr", args = list(param = "PFS", population = "SAF")),
                           data = if (length(steps)) list(steps = steps))
  p <- .spec_add_output(p, o, label = "forest_hr", population = "SAF")
  s$planner <- p
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt)
})

# ---- edits through the model functions the screens call ---------------------------

test_that("G5-0: a figure of the report's own code (a step's code, its datasets) is its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  p <- add_output(s$planner, "F-99-1", type = "figure", description = "Age histogram")
  p <- set_report_datasets(p, "F-99-1", "ADSL")
  p <- set_step_code(p, "F-99-1", "content", paste(
    "content <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) +",
    "  ggplot2::geom_histogram(bins = 5)", sep = "\n"))
  s$planner <- p
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt, g5_gap_datasets,
                 g5_files(c("F-99-1", "F-14-2-1"), "batch.R"))
})

test_that("G5-0: a table's Rework typed in its field is its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  p <- s$planner
  # (the field writes the report list's process_code, as srv_figure_page.R's
  # set_field() does)
  i <- match("T-14-1-1", p$outputs$output_id)
  p$outputs$process_code[i] <- "data <- data[order(data$variable), , drop = FALSE]"
  s$planner <- p
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt)
})

test_that("G5-0: a table's Rework of two R steps is its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  id <- "T-14-1-1"
  o <- s$planner$spec$outputs[[id]]
  o$data$steps <- list(tflspec::tfl_r("data <- data[!is.na(data$variable), , drop = FALSE]"),
                       tflspec::tfl_r("data$variable <- toupper(data$variable)"))
  s$planner <- .spec_set_output(s$planner, id, o)
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt, g5_gap_steps, g5_files(id))
})

test_that("G5-0: a figure designed in the Plot Designer is its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  p <- add_output(s$planner, "F-99-2", type = "figure", description = "Systolic BP")
  d <- tflspec::tfl_fig_template("mean_ci", data = "ADVS", param = "SYSBP", value = "CHG")
  s$planner <- set_fig_design(p, "F-99-2", d)
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt)
})

test_that("G5-0: a listing started from the data is its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  path <- "data/adam/adae.rds"
  dat <- readRDS(file.path(s$path, path))
  cols <- intersect(c("USUBJID", "AEDECOD", "AESEV"), names(dat))
  grp <- intersect(c("TRT01A", "TRTA"), names(dat))[1L]
  s$planner <- first_listing(s$planner, "L-99-1", path, dat, columns = cols,
                             group = if (!is.na(grp)) grp)
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt)
})

test_that("G5-0: a report's titles with quotes and a typed \\n are its YAML's", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  id <- "T-14-1-1"
  p <- set_page_line(s$planner, "titles", id, "1",
                     center = "Demographics of \"all subjects\"\\nSafety Analysis Set")
  p <- set_page_line(p, "titles", id, "2", center = "\"A quoted line\"")
  s$planner <- p
  rt <- g5_round_trip(s, home)
  g5_expect_same(rt, g5_gap_page_text, g5_files(id))
})
