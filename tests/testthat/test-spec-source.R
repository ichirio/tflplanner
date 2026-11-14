# The SPEC is the source (#274): the definition files -- the YAML spec --
# are read on open when they changed outside tflplanner, a save never writes
# over such a change.

# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()
skip_if_quick()  # slow: not in the quick check of a PR into next (helper.R)

spec_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

spec_study <- function(id = "SP-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  p <- set_sheet_rows(p, "titles", "T-2", data.frame(line = "1", center = "Other"))
  suppressMessages(create_study(id, planner = p))
}

# a report's file of the YAML spec, and a person's edit of it in a text
# editor: its first title
report_file <- function(s, id = "T-1") file.path(s$path, "spec", "outputs", paste0(id, ".yml"))
edit_title <- function(s, value, id = "T-1") {
  f <- report_file(s, id)
  o <- tflspec::tfl_read_output(f)
  o$page$titles[["1"]]$center <- value
  tflspec::tfl_write_output(o, f)
}
break_file <- function(f) writeLines(c("id: [", "kind"), f)
history_n <- function(home, id) {
  length(list.files(file.path(.store_dir(id, home), "history")))
}

test_that("a save records each definition file's fingerprint", {
  home <- spec_home()
  s <- spec_study()
  st <- .read_state("SP-1", home)
  expect_identical(st$format, 2L)
  expect_setequal(names(st$files), .spec_files_yaml(s$path))
  expect_true(all(c("study.yml", "spec/toc.yml", "spec/outputs/T-1.yml",
                    "spec/outputs/T-2.yml") %in% names(st$files)))
  expect_identical(st$files[["spec/outputs/T-1.yml"]]$md5,
                   unname(tools::md5sum(report_file(s))))
  expect_true(all(spec_status("SP-1")$status == "same"))
})

test_that("an untouched study opens from its state, without reading its files", {
  spec_home()
  spec_study()
  local_mocked_bindings(.planner_from_yaml = function(...) stop("read"))
  o <- open_study("SP-1")
  expect_identical(o$spec$status, "same")
})

test_that("a direct edit that reads is taken in on open", {
  home <- spec_home()
  s <- spec_study()
  n <- history_n(home, "SP-1")
  edit_title(s, "New title")
  expect_identical(spec_status("SP-1")$status[
    spec_status("SP-1")$file == "spec/outputs/T-1.yml"], "changed")
  expect_message(o <- open_study("SP-1"), "changed outside tflplanner")
  expect_identical(o$spec$status, "adopted")
  expect_identical(o$spec$files$file, "spec/outputs/T-1.yml")
  expect_true("sheet:titles" %in% o$spec$parts)
  expect_identical(o$spec$outputs, "T-1")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "New title")
  # the copy follows: the next open is the same, the old state in history
  expect_identical(open_study("SP-1")$spec$status, "same")
  expect_identical(history_n(home, "SP-1"), n + 1L)
  # and a save keeps the edit
  o2 <- save_study(o)
  expect_identical(sheet_rows(open_study("SP-1")$planner, "titles", "T-1")$center,
                   "New title")
})

test_that("a direct edit that does not read: open as saved, no save, write back or reload", {
  home <- spec_home()
  s <- spec_study()
  good <- tempfile(fileext = ".yml")
  edit_title(s, "Fixed title")
  file.copy(report_file(s), good)
  break_file(report_file(s))
  o <- suppressMessages(open_study("SP-1"))
  expect_identical(o$spec$status, "invalid")
  expect_true(any(grepl("T-1.yml", o$spec$problems$file, fixed = TRUE)))
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "Old title")
  # a save is stopped, whatever it changes
  o$planner$outputs$description[1] <- "changed in the app"
  expect_error(save_study(o), class = "tflplanner_spec_changed")
  expect_error(reload_from_spec("SP-1"), class = "tflplanner_spec_invalid")
  # fixed in the editor, then reloaded: taken in
  file.copy(good, report_file(s), overwrite = TRUE)
  r <- reload_from_spec("SP-1")
  expect_identical(r$spec$status, "adopted")
  expect_identical(sheet_rows(r$planner, "titles", "T-1")$center, "Fixed title")
  # broken again, then written back from the last save: the rejected file
  # is kept in spec/.rejected/
  break_file(report_file(s))
  w <- write_spec("SP-1")
  rej <- list.files(file.path(s$path, "spec", ".rejected"))
  expect_length(rej, 1L)
  expect_match(rej, "^T-1_[0-9-]+\\.yml$")
  o <- open_study("SP-1")
  expect_identical(o$spec$status, "same")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "Fixed title")
})

test_that("a figure's design taken out of its file, or put back, is taken in", {
  spec_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_fig_design(p, "F-1", tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  s <- suppressMessages(create_study("SP-2", planner = p))
  f <- report_file(s, "F-1")
  with_design <- readLines(f)
  o <- tflspec::tfl_read_output(f)
  o$template <- NULL
  o$content <- list()
  o$data$steps <- NULL
  for (k in c("plot", "plots", "compose", "ggplot2_version")) o[[k]] <- NULL
  tflspec::tfl_write_output(o, f)
  r <- suppressMessages(open_study("SP-2"))
  expect_identical(r$spec$status, "adopted")
  expect_null(fig_design(r$planner, "F-1"))
  writeLines(with_design, f)
  r <- suppressMessages(open_study("SP-2"))
  expect_identical(r$spec$status, "adopted")
  expect_identical(fig_design(r$planner, "F-1")$template, "km_simple")
})

test_that("a file written again with the same meaning is touched: no notice, no history", {
  home <- spec_home()
  s <- spec_study()
  n <- history_n(home, "SP-1")
  # the same report, written with its keys in another order
  y <- yaml::read_yaml(report_file(s))
  writeLines(yaml::as.yaml(y[rev(names(y))]), report_file(s))
  skip_if(all(spec_status("SP-1")$status == "same"), "the same bytes")
  expect_no_message(o <- open_study("SP-1"))
  expect_identical(o$spec$status, "touched")
  expect_identical(history_n(home, "SP-1"), n)
  expect_identical(open_study("SP-1")$spec$status, "same")
})

test_that("a state of an earlier version reads the files once, an earlier edit kept", {
  home <- spec_home()
  s <- spec_study()
  f <- .state_file("SP-1", home)
  st <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  st$files <- NULL
  st$format <- 1L
  writeLines(.json(st), f)
  # unchanged: read once, recorded, nothing to say
  expect_no_message(o <- open_study("SP-1"))
  expect_identical(o$spec$status, "same")
  expect_false(is.null(.read_state("SP-1", home)$files))
  # an edit made before the upgrade is taken in, not written over
  st$files <- NULL
  writeLines(.json(st), f)
  edit_title(s, "Edited before")
  o <- suppressMessages(open_study("SP-1"))
  expect_identical(o$spec$status, "adopted")
  expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "Edited before")
})

test_that("a moved study folder matches its record", {
  home <- spec_home()
  s <- spec_study()
  to <- file.path(home, "moved")
  dir.create(to)
  file.rename(s$path, file.path(to, "SP-1"))
  o <- open_study(file.path(to, "SP-1"))
  expect_identical(o$spec$status, "same")
  expect_identical(.read_state("SP-1", home)$path,
                   normalizePath(file.path(to, "SP-1"), "/"))
})

test_that("an edit made while the study was off the list is taken in on register", {
  home <- spec_home()
  s <- spec_study()
  unregister_study("SP-1")
  edit_title(s, "While away")
  r <- suppressMessages(register_study(s$path))
  expect_identical(r$spec$status, "adopted")
  expect_identical(sheet_rows(r$planner, "titles", "T-1")$center, "While away")
  # one that does not read: the kept state comes back, marked
  unregister_study("SP-1")
  break_file(report_file(s))
  r <- suppressMessages(register_study(s$path))
  expect_identical(r$spec$status, "invalid")
  expect_identical(sheet_rows(r$planner, "titles", "T-1")$center, "While away")
})

test_that("two sessions: another's save passes the guard, an outside edit does not", {
  spec_home()
  s <- spec_study()
  a <- open_study("SP-1")
  b <- open_study("SP-1")
  b$planner <- set_sheet_rows(b$planner, "titles", "T-2",
                              data.frame(line = "1", center = "B's"))
  save_study(b, base = open_study("SP-1"))
  a2 <- a
  a2$planner$outputs$description[1] <- "A's"
  saved <- save_study(a2, base = a)
  expect_identical(saved$planner$outputs$description[1], "A's")
  expect_identical(sheet_rows(saved$planner, "titles", "T-2")$center, "B's")
  # an edit in the editor after A opened: A's save stops
  a <- open_study("SP-1")
  edit_title(s, "Outside")
  a$planner$outputs$description[1] <- "A again"
  expect_error(save_study(a, base = a), class = "tflplanner_spec_changed")
})

test_that("a save writes the files whose part changed, and reads none", {
  spec_home()
  s <- spec_study()
  local_mocked_bindings(.planner_from_yaml = function(...) stop("read"))
  s$planner$outputs$description[1] <- "report list only"
  # the report list is the TOC: written, not read; the reports' files not
  out <- save_study(s)
  st <- function(f) out$files$status[basename(out$files$file) == f]
  expect_identical(st("toc.yml"), "written")
  expect_identical(st("T-2.yml"), "unchanged")
  # a study field only: study.yml, not a report's file
  out$meta$title <- "a title"
  out <- save_study(out)
  expect_identical(st("T-2.yml"), "unchanged")
  expect_identical(.read_meta(s$path)$title, "a title")
})

test_that("a figure design is a part: merged, or a conflict", {
  spec_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- add_output(p, "T-1", type = "table")
  s <- suppressMessages(create_study("SP-3", planner = p))
  base <- open_study("SP-3")
  other <- base
  other$planner <- set_fig_design(other$planner, "F-1",
                                  tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  save_study(other, base = base)
  mine <- base
  mine$planner$outputs$description[mine$planner$outputs$output_id == "T-1"] <- "mine"
  m <- save_study(mine, base = base)
  expect_identical(fig_design(m$planner, "F-1")$template, "km_simple")
  # the same design changed by both: a conflict
  b2 <- open_study("SP-3")
  x <- b2
  x$planner <- set_fig_design(x$planner, "F-1", NULL)
  save_study(x, base = b2)
  y <- b2
  y$planner <- set_fig_design(y$planner, "F-1",
                              tflspec::tfl_fig_template("km_simple", data = "ADTTE2"))
  expect_error(save_study(y, base = b2), class = "tflplanner_conflict")
})

test_that("a report taken out in the app: the file the save wrote goes", {
  spec_home()
  s <- spec_study("SP-4")
  expect_true(file.exists(report_file(s, "T-2")))
  o <- open_study("SP-4")
  o$planner <- remove_output(o$planner, "T-2")
  o <- save_study(o)
  expect_false(file.exists(report_file(s, "T-2")))
  expect_true(file.exists(report_file(s, "T-1")))
  expect_identical(o$files$status[basename(o$files$file) == "T-2.yml"], "removed")
})

# ---- unsaved changes merged with reloaded files keep a report's spec ----------
# (the app's "Load from the definition files" over unsaved changes: the study
# as last saved `base`, the unsaved one `mine`, the files read again `now`,
# then a save)

merge_study <- function(home) {
  suppressMessages(create_sample_study(run = FALSE, home = home))
}
# a person's edit of the data definition, outside the app
edit_data_yml <- function(s) {
  f <- file.path(s$path, "spec", "data.yml")
  writeLines(sub("label: Screened Set", "label: Enrolled Set", readLines(f), fixed = TRUE), f)
}
template_report <- function(id = "T-TPL", args = list(by = "TRT01A", categorical = "SEX")) {
  tflspec::tfl_output(id, "table", label = "Demographics", population = "SAF",
                      template = list(name = "demog", args = args))
}
# the app's reload: the files read, `mine` merged over them, then a save
merge_and_save <- function(base, mine, home) {
  now <- suppressMessages(reload_from_spec(base$meta$study_id, home))
  m <- .merge_parts(base, mine, now)
  if (length(m$conflicts)) return(list(m = m))
  saved <- suppressMessages(save_study(m$study, base = now, fill_widths = TRUE, home = home))
  list(m = m, saved = saved, now = now)
}
data_label <- function(s) {
  grep("Enrolled Set", readLines(file.path(s$path, "spec", "data.yml")), fixed = TRUE, value = TRUE)
}

test_that("a new template report kept over an outside change to the files", {
  home <- spec_home()
  base <- merge_study(home)
  mine <- base
  mine$planner <- .spec_add_output(base$planner, template_report(), label = "Demographics",
                                   population = "SAF")
  edit_data_yml(base)
  r <- merge_and_save(base, mine, home)
  expect_length(r$m$conflicts, 0L)
  o <- tflspec::tfl_read_output(report_file(base, "T-TPL"))
  expect_identical(o$template$name, "demog")
  expect_identical(o$template$args$by, "TRT01A")
  expect_length(o$content, 0L)
  expect_false(any(grepl("^content:\\s*$", readLines(report_file(base, "T-TPL")))))
  # the outside change is in too, and the view still has the report
  expect_length(data_label(base), 1L)
  expect_false(is.null(.tpl_of(r$saved$planner, "T-TPL")))
  expect_true("T-TPL" %in% r$saved$planner$outputs$output_id)
})

test_that("an existing template report's unsaved argument is kept over an outside change", {
  home <- spec_home()
  s <- merge_study(home)
  s$planner <- .spec_add_output(s$planner, template_report(), label = "Demographics",
                                population = "SAF")
  base <- suppressMessages(save_study(s, home = home))
  mine <- base
  mine$planner <- .spec_set_output(base$planner, "T-TPL", .tpl_output(
    base$planner, "T-TPL", list(by = "TRT01A", continuous = "AGE")))
  edit_data_yml(base)
  r <- merge_and_save(base, mine, home)
  expect_length(r$m$conflicts, 0L)
  o <- tflspec::tfl_read_output(report_file(base, "T-TPL"))
  expect_identical(o$template$name, "demog")
  expect_identical(o$template$args$continuous, "AGE")
  expect_identical(o$template$args$by, "TRT01A")
  expect_length(data_label(base), 1L)
})

test_that("a non-template report's unsaved cell edit is still kept", {
  home <- spec_home()
  base <- merge_study(home)
  id <- "T-14-1-2"
  mine <- base
  mine$planner <- set_sheet_rows(mine$planner, "titles", id,
                                 data.frame(line = "1", center = "Mine title"))
  edit_data_yml(base)
  r <- merge_and_save(base, mine, home)
  expect_length(r$m$conflicts, 0L)
  expect_identical(tflspec::tfl_read_output(report_file(base, id))$page$titles[["1"]]$center,
                   "Mine title")
  expect_length(data_label(base), 1L)
})

test_that("a report whose file changed outside, untouched in the app, takes the file's", {
  home <- spec_home()
  base <- merge_study(home)
  s <- base
  mine <- base
  mine$planner <- .spec_add_output(base$planner, template_report(), label = "Demographics",
                                   population = "SAF")
  edit_title(s, "Outside title", id = "T-14-1-2")
  r <- merge_and_save(base, mine, home)
  expect_length(r$m$conflicts, 0L)
  expect_identical(tflspec::tfl_read_output(report_file(base, "T-14-1-2"))$page$titles[["1"]]$center,
                   "Outside title")
  expect_identical(r$m$study$planner$spec$outputs[["T-14-1-2"]]$page$titles[["1"]]$center,
                   "Outside title")
  expect_identical(tflspec::tfl_read_output(report_file(base, "T-TPL"))$template$name, "demog")
})

test_that("a report changed in the files and in the app is still a conflict", {
  home <- spec_home()
  base <- merge_study(home)
  id <- "T-14-1-2"
  mine <- base
  mine$planner <- set_sheet_rows(mine$planner, "titles", id,
                                 data.frame(line = "1", center = "Mine title"))
  edit_title(base, "Outside title", id = id)
  r <- merge_and_save(base, mine, home)
  expect_true("sheet:titles" %in% r$m$conflicts)
  # (the files' spec of the report is not replaced by the app's)
  expect_identical(r$m$study$planner$spec$outputs[[id]]$page$titles[["1"]]$center, "Outside title")
})

test_that("a report taken out in the app stays out over an outside change", {
  home <- spec_home()
  base <- merge_study(home)
  mine <- base
  mine$planner <- remove_output(base$planner, "T-14-1-2")
  edit_data_yml(base)
  r <- merge_and_save(base, mine, home)
  expect_length(r$m$conflicts, 0L)
  expect_false("T-14-1-2" %in% r$saved$planner$outputs$output_id)
  expect_false(file.exists(report_file(base, "T-14-1-2")))
  expect_true(file.exists(report_file(base, "T-14-1-3")))
  expect_length(data_label(base), 1L)
})

test_that("a report the files added stays, with the unsaved report kept", {
  home <- spec_home()
  base <- merge_study(home)
  mine <- base
  mine$planner <- set_sheet_rows(base$planner, "titles", "T-14-1-3",
                                 data.frame(line = "1", center = "Mine title"))
  o <- tflspec::tfl_read_output(report_file(base, "T-14-1-3"))
  o$id <- "T-NEW"
  tflspec::tfl_write_output(o, report_file(base, "T-NEW"))
  toc <- file.path(base$path, "spec", "toc.yml")
  writeLines(sub("^outputs:", "outputs:
  - id: T-NEW
    kind: table", readLines(toc)), toc)
  r <- merge_and_save(base, mine, home)
  expect_length(r$m$conflicts, 0L)
  expect_true("T-NEW" %in% r$saved$planner$outputs$output_id)
  expect_true(file.exists(report_file(base, "T-NEW")))
  expect_identical(tflspec::tfl_read_output(report_file(base, "T-14-1-3"))$page$titles[["1"]]$center,
                   "Mine title")
})

# ---- "Keep my unsaved changes" in a conflict keeps a template report's spec ----
# (do_reload(): the study of "keep mine" = the merge with the conflicting
# parts taken from the session; .keep_mine_outputs() carries the spec too)

# a template report saved, then changed in the app (a title and an argument)
# and in its file (a title and another argument): the titles sheet conflicts
conflict_setup <- function(home, id = "T-TPL") {
  s <- merge_study(home)
  s$planner <- .spec_add_output(s$planner, template_report(), label = "Demographics",
                                population = "SAF")
  base <- suppressMessages(save_study(s, home = home))
  mine <- base
  mine$planner <- .spec_set_output(base$planner, id, .tpl_output(
    base$planner, id, list(by = "TRT01A", continuous = "AGE")))
  mine$planner <- set_sheet_rows(mine$planner, "titles", id,
                                 data.frame(line = "1", center = "Mine title"))
  f <- report_file(base, id)
  o <- tflspec::tfl_read_output(f)
  o$page$titles[["1"]]$center <- "Outside title"
  o$template$args$continuous <- "WEIGHT"
  tflspec::tfl_write_output(o, f)
  now <- suppressMessages(reload_from_spec(base$meta$study_id, home))
  m <- .merge_parts(base, mine, now)
  expect_true("sheet:titles" %in% m$conflicts)
  mine2 <- m$study
  for (k in m$conflicts) mine2 <- .set_study_part(mine2, k, .study_parts(mine)[[k]])
  list(base = base, mine = mine, now = now, m = m, mine2 = mine2)
}
save_choice <- function(study, now, home) {
  suppressMessages(save_study(study, base = now, fill_widths = TRUE, home = home))
}

test_that("keeping my changes in a conflict keeps the template report's template", {
  home <- spec_home()
  x <- conflict_setup(home)
  # without the carry the spec is the files' (the bug)
  keep <- .keep_mine_outputs(x$mine2, x$mine, x$m$study, x$m$conflicts)
  expect_identical(.tpl_of(keep$planner, "T-TPL")$args$continuous, "AGE")
  expect_identical(.tpl_of(keep$planner, "T-TPL")$name, "demog")
  save_choice(keep, x$now, home)
  o <- tflspec::tfl_read_output(report_file(x$base, "T-TPL"))
  expect_identical(o$template$name, "demog")
  expect_identical(o$template$args$continuous, "AGE")
  expect_identical(o$template$args$by, "TRT01A")
  expect_length(o$content, 0L)
})

test_that("keeping the files' version of a conflict has the file's arguments", {
  home <- spec_home()
  x <- conflict_setup(home)
  save_choice(x$m$study, x$now, home)
  o <- tflspec::tfl_read_output(report_file(x$base, "T-TPL"))
  expect_identical(o$template$name, "demog")
  expect_identical(o$template$args$continuous, "WEIGHT")
  expect_identical(o$page$titles[["1"]]$center, "Outside title")
})

test_that("a plain report in a conflict is kept as before", {
  home <- spec_home()
  base <- merge_study(home)
  id <- "T-14-1-2"
  mine <- base
  mine$planner <- set_sheet_rows(mine$planner, "titles", id,
                                 data.frame(line = "1", center = "Mine title"))
  edit_title(base, "Outside title", id = id)
  now <- suppressMessages(reload_from_spec(base$meta$study_id, home))
  m <- .merge_parts(base, mine, now)
  mine2 <- m$study
  for (k in m$conflicts) mine2 <- .set_study_part(mine2, k, .study_parts(mine)[[k]])
  keep <- .keep_mine_outputs(mine2, mine, m$study, m$conflicts)
  expect_identical(keep, mine2)
})

test_that("a report outside the conflict keeps what the merge gave it", {
  home <- spec_home()
  x <- conflict_setup(home)
  # a second template report changed only in the app: .merge_spec_outputs() took it
  s <- x$base
  s$planner <- .spec_add_output(s$planner, template_report("T-TP2"), label = "Demographics",
                                population = "SAF")
  base <- suppressMessages(save_study(s, home = home))
  mine <- base
  mine$planner <- .spec_set_output(base$planner, "T-TP2", .tpl_output(
    base$planner, "T-TP2", list(by = "TRT01A", continuous = "HEIGHT")))
  mine$planner <- set_sheet_rows(mine$planner, "titles", "T-TPL",
                                 data.frame(line = "1", center = "Mine title"))
  f <- report_file(base, "T-TPL")
  o <- tflspec::tfl_read_output(f)
  o$page$titles[["1"]]$center <- "Outside title"
  tflspec::tfl_write_output(o, f)
  now <- suppressMessages(reload_from_spec(base$meta$study_id, home))
  m <- .merge_parts(base, mine, now)
  expect_identical(.tpl_of(m$study$planner, "T-TP2")$args$continuous, "HEIGHT")
  mine2 <- m$study
  for (k in m$conflicts) mine2 <- .set_study_part(mine2, k, .study_parts(mine)[[k]])
  keep <- .keep_mine_outputs(mine2, mine, m$study, m$conflicts)
  expect_identical(.tpl_of(keep$planner, "T-TP2")$args$continuous, "HEIGHT")
  expect_identical(keep$planner$spec$outputs[["T-TP2"]], m$study$planner$spec$outputs[["T-TP2"]])
})
