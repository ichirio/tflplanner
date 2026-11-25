# The run definitions in the sync and the save (slice B1, R2.4): spec/runs.yml
# is derived from p$runs at the end of every sync; a file edited by hand is
# kept when it says the same; a file that does not parse blocks its own save
# and is never replaced; a study of the older form is moved in the same sync;
# two sessions, the draft and the history keep the definitions.

rs_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

rs_planner <- function() {
  p <- add_output(add_output(new_planner(), "DM"), "VS")
  add_output(p, "L1", type = "listing", at = "end")
}

rs_runs_file <- function(s) file.path(s$path, "spec", "runs.yml")

test_that("a save writes spec/runs.yml; no definition, no file", {
  rs_home()
  s <- create_study("RS1", planner = rs_planner())
  expect_false(file.exists(rs_runs_file(s)))
  s$planner <- set_run_definition(s$planner, "Topline", c("DM", "L1"), mode = "ard")
  s <- suppressMessages(save_study(s))
  expect_true(file.exists(rs_runs_file(s)))
  back <- tflspec::tfl_read_runs(s$path)
  expect_identical(vapply(unclass(back), function(d) d$name, ""), "Topline")
  # the last definition removed: the file goes
  s$planner <- remove_run_definition(s$planner, "Topline")
  s <- suppressMessages(save_study(s))
  expect_false(file.exists(rs_runs_file(s)))
})

test_that("a clean save after a card edit refreshes batch.R's stamp, and writes nothing else", {
  rs_home()
  s <- create_study("RS2", planner = set_run_definition(rs_planner(), "Topline", "DM"))
  stamp <- function() grep("^[.]batch_stamp <- ", readLines(file.path(s$path, "programs", "batch.R")),
                           value = TRUE)
  expect_identical(stamp(), paste0(".batch_stamp <- \"", tflspec::tfl_spec_stamp(s$path, "spec"), "\""))
  # the spec changes by hand (a file the sync does not write): the next save
  # of the study, with nothing changed in the planner, makes batch.R again
  cat("# a comment\n", file = file.path(s$path, "spec", "defaults.yml"), append = TRUE)
  expect_false(identical(stamp(), paste0(".batch_stamp <- \"", tflspec::tfl_spec_stamp(s$path, "spec"), "\"")))
  o <- open_study("RS2")
  suppressMessages(save_study(o))
  expect_identical(stamp(), paste0(".batch_stamp <- \"", tflspec::tfl_spec_stamp(s$path, "spec"), "\""))
})

test_that("runs.yml edited by hand: kept as it is when it says the same, read in when it does not", {
  rs_home()
  s <- create_study("RS3", planner = set_run_definition(rs_planner(), "Topline", c("DM", "L1")))
  f <- rs_runs_file(s)
  # the same definitions, written by hand with a comment and another layout
  writeLines(c("# the study's run definitions (mine)", "runs:",
               "  - name: Topline", "    reports: [DM, L1]"), f)
  o <- open_study("RS3")
  expect_identical(run_def_ids(o$planner, "Topline"), c("DM", "L1"))
  suppressMessages(save_study(o))
  expect_identical(readLines(f)[1], "# the study's run definitions (mine)")
  # another definition added by hand: the study has it when it is opened again
  writeLines(c("runs:", "  - name: Topline", "    reports: [DM, L1]",
               "  - name: Final", "    reports: all"), f)
  o <- open_study("RS3")
  expect_identical(.run_names(o$planner), c("Topline", "Final"))
  # a report in a definition that the study does not have: a Review row, and
  # the save is not refused (it is an error row, not a name)
  writeLines(c("runs:", "  - name: Topline", "    reports: [DM, GONE]"), f)
  o <- open_study("RS3")
  rows <- .rules_runs(o$planner)
  expect_true(any(rows$level == "error"))
  expect_no_error(suppressMessages(save_study(o)))
})

test_that("a runs.yml that does not parse: the study cannot be read, its text is never replaced", {
  rs_home()
  s <- create_study("RS4", planner = set_run_definition(rs_planner(), "Topline", "DM"))
  f <- rs_runs_file(s)
  bad <- c("runs: [", "  - name: Topline")
  writeLines(bad, f)
  expect_error(.runs_read(s$path), "cannot be read")
  expect_error(.planner_from_yaml(s$path), "cannot be read")
  # in the sync, a text that does not parse is kept, whatever the planner has
  want <- .runs_text_want(list(list(name = "X", reports = "all")), bad, "DM")
  expect_identical(want, bad)
  expect_identical(readLines(f), bad)
})

test_that("the sync never gives the runs file a NULL text: no unlink of a runs file", {
  rs_home()
  s <- create_study("RS5", planner = set_run_definition(rs_planner(), "Topline", "DM"))
  p <- s$planner
  expect_true(.runs_rel() %in% names(p$spec$text))
  # the sync of a planner that changed something else
  p <- add_output(p, "AE", at = "end")
  q <- .spec_sync(p, p$spec$meta)
  expect_false(.runs_rel() %in% attr(q, "changed") && is.null(q$spec$text[[.runs_rel()]]))
  expect_false(is.null(q$spec$text[[.runs_rel()]]))
  # a study with no definition: no text at all (and no file written)
  p2 <- remove_run_definition(s$planner, "Topline")
  q2 <- .spec_sync(p2, p2$spec$meta)
  expect_null(q2$spec$text[[.runs_rel()]])
  # the save of it removes the file it wrote itself; and never any other
  s$planner <- p2
  s <- suppressMessages(save_study(s))
  expect_false(file.exists(rs_runs_file(s)))
  expect_true(file.exists(file.path(s$path, "spec", "toc.yml")))
})

test_that("the older form is moved in one sync: runs.yml written, toc.yml loses `sets:`", {
  rs_home()
  s <- create_study("RS6", planner = rs_planner())
  toc <- file.path(s$path, "spec", "toc.yml")
  txt <- readLines(toc)
  expect_true(any(grepl("id: DM", txt, fixed = TRUE)))
  # an older study: its reports say their sets in toc.yml
  out <- character()
  for (l in txt) {
    out <- c(out, l)
    if (grepl("^ *- id: DM", l)) out <- c(out, "    sets: [Topline, Final]")
    if (grepl("^ *- id: VS", l)) out <- c(out, "    sets:", "      - Final")
  }
  writeLines(out, toc)
  r <- .runs_read(s$path)
  expect_identical(attr(r, "migrated"), 2L)
  expect_identical(vapply(r, function(d) d$name, ""), c("Topline", "Final"))
  # opened: the definitions are the planner's, not on disk yet
  o <- open_study("RS6")
  expect_identical(run_def_ids(o$planner, "Final"), c("DM", "VS"))
  expect_false(file.exists(rs_runs_file(s)))
  expect_identical(attr(o$planner$runs, "migrated"), 2L)
  rows <- .rules_runs(o$planner)
  expect_match(rows$template[1], "of the earlier form", fixed = TRUE)
  # saved: runs.yml is written and toc.yml has no `sets:` left, in the one save
  o <- suppressMessages(save_study(o))
  expect_true(file.exists(rs_runs_file(s)))
  expect_false(any(grepl("sets:", readLines(toc), fixed = TRUE)))
  expect_null(attr(o$planner$runs, "migrated"))
  expect_identical(run_def_ids(open_study("RS6")$planner, "Final"), c("DM", "VS"))
  expect_null(attr(open_study("RS6")$planner$runs, "migrated"))
})

test_that("two sessions: a part each merges; the same part both changed is a conflict", {
  rs_home()
  s <- create_study("RS7", planner = rs_planner())
  base <- open_study("RS7")
  # session 1 saves a definition
  one <- open_study("RS7")
  one$planner <- set_run_definition(one$planner, "Topline", "DM")
  suppressMessages(save_study(one))
  # session 2 (opened before) changes the report list only: the definition is merged in
  two <- base
  two$planner <- add_output(two$planner, "AE", at = "end")
  r <- suppressMessages(save_study(two, base = base))
  expect_identical(.run_names(r$planner), "Topline")
  expect_true("AE" %in% r$planner$outputs$output_id)
  expect_true(file.exists(rs_runs_file(r)))
  # both change the definitions: the one that saves second is told
  base2 <- open_study("RS7")
  a <- open_study("RS7")
  a$planner <- set_run_definition(a$planner, "Final", "all")
  suppressMessages(save_study(a))
  b <- base2
  b$planner <- set_run_definition(b$planner, "Interim", "DM")
  expect_error(suppressMessages(save_study(b, base = base2)), "runs", class = "tflplanner_conflict")
})

test_that("the definitions come back from the state, the draft and an older state", {
  home <- rs_home()
  s <- create_study("RS8", planner = rs_planner())
  s$planner <- set_run_definition(s$planner, "Topline", c("DM", "L1"), mode = "ard", copy_data = TRUE)
  # the draft (unsaved changes) keeps them
  .write_draft(s)
  d <- .read_draft("RS8")
  expect_identical(.runs_bare(d$planner$runs), .runs_bare(s$planner$runs))
  # a state of before the run definitions (no `runs`, the report list's
  # `batches`): moved in, said
  st <- .state_of(s)
  st$planner$runs <- NULL
  st$planner$outputs$batches <- list("Topline", "Topline", NULL)
  old <- .study_from_state(st)
  expect_identical(run_def_ids(old$planner, "Topline"), c("DM", "VS"))
  expect_identical(attr(old$planner$runs, "migrated"), 1L)
  # through JSON, as the files hold them
  st2 <- jsonlite::fromJSON(.json(st), simplifyVector = FALSE)
  expect_identical(run_def_ids(.study_from_state(st2)$planner, "Topline"), c("DM", "VS"))
  # and the state's own definitions through JSON: one report stays an array
  s$planner <- set_run_definition(s$planner, "One", "DM")
  st3 <- jsonlite::fromJSON(.json(.state_of(s)), simplifyVector = FALSE)
  back <- .study_from_state(st3)
  expect_identical(back$planner$runs[[2]]$reports, "DM")
  expect_identical(.runs_bare(back$planner$runs), .runs_bare(s$planner$runs))
})

test_that("an export of the older form, imported: its sets become definitions", {
  rs_home()
  s <- create_study("RS9", planner = rs_planner())
  src <- withr_tempdir()
  file.copy(file.path(s$path, "study.yml"), src)
  file.copy(file.path(s$path, "spec"), src, recursive = TRUE)
  toc <- file.path(src, "spec", "toc.yml")
  out <- character()
  for (l in readLines(toc)) {
    out <- c(out, l)
    if (grepl("^ *- id: L1", l)) out <- c(out, "    sets: [Topline]")
  }
  writeLines(out, toc)
  unlink(file.path(src, "spec", "runs.yml"))
  p <- .planner_from_yaml(src)
  expect_identical(run_def_ids(p, "Topline"), "L1")
  expect_identical(attr(p$runs, "migrated"), 1L)
})
