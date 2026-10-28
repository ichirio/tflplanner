# The study's programs, run as an official run runs them (#e2e, 10/29): a
# study saved (save_study()), its programs run in a child R (run_batch())
# that attaches only what programs/study_setup.R attaches and finds the
# packages through R_LIBS -- every kind of report the app makes: the
# sample's tables, figures and listing; tables and a listing started from
# the data; one report set's run; a report adopted from the AI's draft;
# every template of tflspec.  A report that fails for a known reason of
# tflspec's (its message below) is skipped, so the test runs it again once
# tflspec is fixed; any other failure fails.
#
# Slow (several minutes): not on CRAN, not on CI.

# tflspec's known failures, by their message (none now: the four found on
# 10/29 -- tfl_tpl() without tflspec::, PARAMN, ancova's endsWith(), a
# record-level grade taken from ADSL -- were fixed in tflspec 45b466be and
# eb4d6e05; a new one goes here while tflspec fixes it)
e2e_known <- character()

e2e_skip <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("rtfreporter")
}

# the child R finds this session's packages (a library set by .libPaths()
# is not the child's otherwise)
e2e_libs <- function(env = parent.frame()) {
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), .local_envir = env)
}

# a study of tflspec's example ADaM, registered in the test's home
e2e_study <- function(id, datasets, home) {
  d <- file.path(studies_root(home), id)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  dat <- list()
  for (x in datasets) {
    e <- new.env()
    utils::data(list = x, package = "tflspec", envir = e)
    dat[[x]] <- e[[x]]
    saveRDS(e[[x]], file.path(d, "adam", paste0(x, ".rds")))
  }
  invisible(tflspec::tfl_ai_study_init(d, adam = "adam", study_id = id, overwrite = TRUE))
  invisible(suppressMessages(register_study(d, home = home)))
  list(path = d, data = dat)
}

# each report's status after the run, and the first error its logs say
e2e_results <- function(s, ids) {
  st <- study_status(s)
  st <- st[st$output_id %in% ids, c("output_id", "status"), drop = FALSE]
  st$error <- vapply(st$output_id, function(id) {
    lg <- list.files(s$path, paste0("^", id, "[.]log$"), recursive = TRUE, full.names = TRUE)
    msg <- ""
    for (g in lg) {
      x <- readLines(g, warn = FALSE)
      e <- grep("Error|error", x)
      if (length(e)) msg <- paste(msg, paste(x[e[1]:min(length(x), e[1] + 3L)], collapse = " "))
    }
    # (the run's logs keep the error's first line only: its ARD program
    # run again for the whole message)
    if (!st$status[match(id, st$output_id)] %in% c("ok", "current", "done")) {
      f <- file.path("programs", "ard", paste0(id, ".R"))
      if (file.exists(file.path(s$path, f))) {
        out <- tryCatch(processx::run(file.path(R.home("bin"), "Rscript"), f, wd = s$path,
                                      env = c("current", R_LIBS = Sys.getenv("R_LIBS")),
                                      error_on_status = FALSE, timeout = 300),
                        error = function(e) list(stdout = "", stderr = conditionMessage(e)))
        out <- c(out$stdout, out$stderr)
        msg <- paste(msg, paste(out, collapse = " "))
      }
    }
    msg
  }, "", USE.NAMES = FALSE)
  st
}

# every report ok; one failing for a known reason of tflspec's skipped
e2e_expect_ok <- function(res) {
  ok <- res$status %in% c("ok", "current", "done")
  known <- vapply(res$error, function(m) {
    k <- names(e2e_known)[vapply(e2e_known, function(p) grepl(p, m), NA)]
    if (length(k)) k[1L] else NA_character_
  }, "", USE.NAMES = FALSE)
  for (i in which(!ok & is.na(known))) {
    fail(sprintf("%s: %s (%s)", res$output_id[i], res$status[i], trimws(res$error[i])))
  }
  expect_true(any(ok) || any(!is.na(known)))
  if (any(!ok & !is.na(known))) {
    skip(sprintf("tflspec, known: %s", paste(sprintf("%s (%s)", res$output_id[!ok & !is.na(known)],
                                                     known[!ok & !is.na(known)]), collapse = ", ")))
  }
}

test_that("the sample study's official run makes every report", {
  e2e_skip()
  home <- local_home()
  e2e_libs()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  s <- suppressMessages(save_study(s, home = home, fill_widths = TRUE))
  invisible(run_batch(s, c("ard", "tfl"), wait = TRUE))
  res <- e2e_results(s, s$planner$outputs$output_id)
  expect_identical(nrow(res), nrow(s$planner$outputs))
  e2e_expect_ok(res)
})

test_that("tables and a listing started from the data run; a report set's run makes its own", {
  e2e_skip()
  home <- local_home()
  e2e_libs()
  st <- e2e_study("FIRST-1", c("adsl", "adae"), home)
  s <- suppressMessages(open_study("FIRST-1", home = home))
  p <- s$planner
  p <- first_table(p, "F-DM", "adam/adsl.rds", st$data$adsl, population = "SAFFL", group = "TRT01A",
                   variables = c("AGE", "SEX", "RACE"))
  p <- first_hierarchy_table(p, "F-AE", "adam/adae.rds", st$data$adae, "adam/adsl.rds", st$data$adsl,
                             population = "SAFFL", group = "TRT01A", where = "TRTEMFL == \"Y\"",
                             total = "Total")
  p <- first_listing(p, "F-LAE", "adam/adae.rds", st$data$adae, columns = c("USUBJID", "AEDECOD", "AESEV"),
                     group = "TRT01A")
  p <- .set_report_batches(p, "F-DM", "Topline")
  s$planner <- p
  s <- suppressMessages(save_study(s, home = home, fill_widths = TRUE))
  # the report set alone: its programs only
  before <- list_batches(s)$batch
  invisible(run_batch(s, c("ard", "tfl"), batch = "Topline", wait = TRUE))
  b <- setdiff(list_batches(s)$batch, before)
  expect_length(b, 1L)
  r <- utils::read.csv(file.path(s$path, "runs", b, "run.csv"), stringsAsFactors = FALSE)
  expect_setequal(r$program, c("programs/ard/F-DM.R", "programs/tfl/F-DM.R"))
  expect_true(all(r$status == "OK"))
  # every report
  invisible(run_batch(s, c("ard", "tfl"), wait = TRUE))
  e2e_expect_ok(e2e_results(s, c("F-DM", "F-AE", "F-LAE")))
})

test_that("a hierarchy by worst grade started from the data runs", {
  e2e_skip()
  home <- local_home()
  e2e_libs()
  st <- e2e_study("FIRST-2", c("adsl", "adae"), home)
  s <- suppressMessages(open_study("FIRST-2", home = home))
  s$planner <- first_hierarchy_table(s$planner, "F-AEGR", "adam/adae.rds", st$data$adae, "adam/adsl.rds",
                                     st$data$adsl, population = "SAFFL", group = "TRT01A",
                                     where = "TRTEMFL == \"Y\"", grade = "AESEV")
  s <- suppressMessages(save_study(s, home = home, fill_widths = TRUE))
  invisible(run_batch(s, c("ard", "tfl"), wait = TRUE))
  e2e_expect_ok(e2e_results(s, "F-AEGR"))
})

test_that("a report adopted from the AI's draft is in the run and runs", {
  e2e_skip()
  home <- local_home()
  e2e_libs()
  st <- e2e_study("AI-2", c("adsl"), home)
  s <- suppressMessages(open_study("AI-2", home = home))
  s$planner <- first_table(s$planner, "F-DM", "adam/adsl.rds", st$data$adsl, population = "SAFFL",
                           group = "TRT01A", variables = c("AGE", "SEX"))
  s <- suppressMessages(save_study(s, home = home))
  # the AI's draft of another report: that report's file under a new id
  o <- tflspec::tfl_read_output(file.path(st$path, "spec", "outputs", "F-DM.yml"))
  o$id <- "A-DM"
  dir.create(file.path(st$path, "ai", "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
  tflspec::tfl_write_output(o, file.path(st$path, "ai", "spec", "outputs", "A-DM.yml"))
  invisible(suppressMessages(tflspec::tfl_ai_adopt(st$path, "A-DM")))
  s <- suppressMessages(reload_from_spec("AI-2", home = home))
  expect_true("A-DM" %in% s$planner$outputs$output_id)
  s <- suppressMessages(save_study(s, home = home, fill_widths = TRUE))
  invisible(run_batch(s, c("ard", "tfl"), wait = TRUE))
  e2e_expect_ok(e2e_results(s, c("F-DM", "A-DM")))
})

test_that("every template of tflspec makes its report in an official run", {
  e2e_skip()
  home <- local_home()
  e2e_libs()
  st <- e2e_study("TPL-ALL", c("adsl", "adae", "adlb", "adtte", "adrs", "advs", "adex", "adtr"), home)
  s <- suppressMessages(open_study("TPL-ALL", home = home))
  dat <- st$data
  first <- function(x) as.character(unique(x[!is.na(x)]))[1L]
  # (a visit with the most changes from baseline of the parameter: an
  # ANCOVA needs subjects in each arm)
  lb <- dat$adlb[dat$adlb$PARAMCD %in% first(dat$adlb$PARAMCD) & !is.na(dat$adlb$CHG) &
                   !is.na(dat$adlb$AVISIT) & dat$adlb$AVISIT != "BASELINE", ]
  visit <- names(sort(table(as.character(lb$AVISIT)), decreasing = TRUE))[1L]
  # what the dialog's fields would hold, as typed
  typed <- list(population = "SAF", hierarchy = c("AEBODSYS", "AEDECOD"), by = "TRT01A", visit = visit,
                params = as.character(unique(dat$adlb$PARAMCD))[1:2], variable = "AGE",
                columns = "USUBJID\nAEDECOD\nAESEV")
  param_of <- list(ancova = first(dat$adlb$PARAMCD), mean_ci = first(dat$adlb$PARAMCD),
                   mean_se = first(dat$adlb$PARAMCD), forest_hr = first(dat$adtte$PARAMCD),
                   waterfall = first(dat$adtr$PARAMCD))
  data_of <- list(ancova = "ADLB", listing = "ADAE", param_change = "ADLB")
  # (km_table and stat_block read an ARD defined in the ARD step: not from
  # the template alone)
  tp <- tflspec::tfl_templates()
  tp <- tp[!tp$name %in% c("km_table", "stat_block"), , drop = FALSE]
  pops <- s$planner$ard$populations
  saf <- as.list(pops[match("SAF", pops$population_id), intersect(c("population_id", "dataset", "where"),
                                                                    names(pops)), drop = FALSE])
  p <- s$planner
  ids <- character()
  for (i in seq_len(nrow(tp))) {
    n <- tp$name[i]
    prm <- tflspec::tfl_template_params(n)
    args <- list()
    for (x in .tpl_shown(prm)) {
      if (!isTRUE(x$required)) next
      v <- switch(x$name, param = param_of[[n]], data = data_of[[n]], typed[[x$name]])
      args[[x$name]] <- .tpl_value(v, .tpl_type(x), x)
    }
    if (identical(n, "summary")) args <- c(args, list(continuous = "AGE", categorical = "SEX"))
    pop <- if (!is.null(args$population)) "SAF" else NA_character_
    steps <- NULL
    if (.tpl_own_data(prm)) {
      steps <- .tpl_steps_from("ADTTE", saf, first(dat$adtte$PARAMCD))
      pop <- "SAF"
    }
    id <- paste0("X-", gsub("_", "-", n))
    o <- tflspec::tfl_output(id, tp$kind[i], label = n, population = pop,
                             template = list(name = n, args = args),
                             data = if (length(steps)) list(steps = steps))
    p <- .spec_add_output(p, o, label = n, population = pop)
    ids <- c(ids, id)
  }
  if ("X-forest-hr" %in% ids) p <- set_fig_ard_source(p, "X-forest-hr", "own")
  s$planner <- p
  s <- suppressWarnings(suppressMessages(save_study(s, home = home, fill_widths = TRUE)))
  invisible(run_batch(s, c("ard", "tfl"), wait = TRUE))
  res <- e2e_results(s, ids)
  expect_identical(sort(res$output_id), sort(ids))
  e2e_expect_ok(res)
})
