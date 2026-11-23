# A change of the study's settings and its save do the work of the study,
# not of each report again (#738): a title, the ADaM levels.

# The ADaM levels applied from tflspec's example define.xml, saved
speed_define <- function(s) {
  f <- system.file("extdata", "define", "define.xml", package = "tflspec")
  testthat::skip_if_not(nzchar(f), "tflspec's example define.xml is not installed")
  testthat::skip_if_not_installed("xml2")
  dir.create(file.path(s$path, study_layout()[["adam"]]), recursive = TRUE, showWarnings = FALSE)
  file.copy(f, file.path(s$path, study_layout()[["adam"]], "define.xml"), overwrite = TRUE)
  invisible()
}

# The caches of the spec and the programs emptied: a run starts as a new
# R session would
speed_fresh_caches <- function() {
  for (e in .spec_base_cache$entries) unlink(e$was$dir, recursive = TRUE)
  .spec_base_cache$entries <- NULL
  for (k in c("out", "p", "meta")) .spec_sync_cache[[k]] <- NULL
  .ard_spec_cache$key <- .ard_spec_cache$spec <- NULL
  rm(list = ls(.program_cache, all.names = TRUE), envir = .program_cache)
  invisible()
}

# The four actions of #738 on a study in the app's server: the short title
# changed, saved; the ADaM levels (applied and saved first) cleared, saved.
# `count()` gives what was counted so far.  Gives the counts of each action.
speed_actions <- function(id, count, reset) {
  out <- list()
  suppressWarnings(shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(adam_levels_path = "data/adam/define.xml")
    session$setInputs(adam_levels_apply = 1L)
    session$setInputs(save = 1L)
    expect_identical(.study_adam_levels(rv$p), "define_xml")
    step <- function(name, ...) {
      reset()
      session$setInputs(...)
      out[[name]] <<- count()
    }
    step("title", m_short_title = "Another short title")
    step("title_save", save = 2L)
    step("levels", adam_levels_clear = 1L)
    step("levels_save", save = 3L)
    expect_identical(.study_adam_levels(rv$p), "none")
    expect_false(session$userData$dirty())
  }))
  out
}

# The files a save writes, by md5 (study.yml without the time it was saved)
speed_md5 <- function(root) {
  f <- list.files(root, recursive = TRUE, all.files = TRUE)
  f <- f[grepl("^(spec|programs)/|^study\\.yml$", f) & !grepl("(^|/)\\.edited/|^spec/\\.rejected/", f)]
  md5 <- stats::setNames(unname(tools::md5sum(file.path(root, f))), f)
  yml <- readLines(file.path(root, "study.yml"), encoding = "UTF-8", warn = FALSE)
  md5[["study.yml"]] <- paste(yml[!startsWith(yml, "updated:")], collapse = "\n")
  md5[order(names(md5))]
}

test_that("a settings change and its save bridge no report and count the same for any number of reports", {
  skip_on_cran()
  skip_if_quick()
  skip_if_no_define_levels()
  local_home()
  n <- new.env()
  bridge <- .spec_bridge
  ard <- .ard_spec
  prog <- program_code
  local_mocked_bindings(
    .spec_bridge = function(...) {
      n$bridge <- (n$bridge %||% 0L) + 1L
      bridge(...)
    },
    .ard_spec = function(...) {
      n$ard <- (n$ard %||% 0L) + 1L
      ard(...)
    },
    program_code = function(...) {
      n$prog <- (n$prog %||% 0L) + 1L
      prog(...)
    },
    .env = environment())
  count <- function() c(bridge = n$bridge %||% 0L, ard = n$ard %||% 0L, prog = n$prog %||% 0L)
  reset <- function() rm(list = ls(n), envir = n)
  # the sample (19 reports) and a study of two
  big <- suppressMessages(sample_study_copy())
  speed_define(big)
  p2 <- add_output(add_output(new_planner(), "T1", description = "one"), "T2", description = "two")
  small <- suppressMessages(create_study("SPEED2", planner = p2))
  speed_define(small)
  expect_gt(nrow(big$planner$outputs), 10L)
  res <- lapply(c(big = big$meta$study_id, small = "SPEED2"), function(id) {
    speed_fresh_caches()
    speed_actions(id, count, reset)
  })
  for (a in c("title", "title_save", "levels", "levels_save")) {
    # no report's files bridged again: the study's settings reach study.yml alone
    expect_identical(res$big[[a]][["bridge"]], 0L, label = paste(a, "bridges"))
    # the ARD definition read as often for 2 reports as for 19
    expect_identical(res$big[[a]][["ard"]], res$small[[a]][["ard"]], label = paste(a, "ARD reads"))
  }
  # the title is not in any report's program: none written again
  expect_identical(res$big$title[["prog"]], 0L)
  expect_identical(res$big$title_save[["prog"]], 0L)
})

test_that("the files a save writes are the same with the study-only bridge as with the whole one", {
  skip_on_cran()
  skip_if_quick()
  skip_if_no_define_levels()
  local_home()
  s1 <- suppressMessages(sample_study_copy())
  speed_define(s1)
  speed_fresh_caches()
  speed_actions(s1$meta$study_id, function() NULL, function() NULL)
  fast <- speed_md5(s1$path)
  # the same actions on another copy, as before #738: each view bridged whole
  # (the core of a view is the view itself), no base derived, the ARD
  # definition read each time
  unregister_study(s1$meta$study_id)
  unlink(s1$path, recursive = TRUE)
  s2 <- suppressMessages(sample_study_copy())
  speed_define(s2)
  speed_fresh_caches()
  local_mocked_bindings(.planner_core = function(body) body,
                        .spec_bridge_base_near = function(key) NULL,
                        .ard_spec_last = function(ard) .ard_spec(ard),
                        .env = environment())
  speed_actions(s2$meta$study_id, function() NULL, function() NULL)
  whole <- speed_md5(s2$path)
  expect_identical(names(fast), names(whole))
  expect_identical(fast, whole)
})

test_that("the study's own settings and the ADaM levels row change study.yml alone in the bridge", {
  skip_on_cran()
  skip_if_quick()
  skip_if_no_define_levels()
  local_home()
  s <- suppressMessages(sample_study_copy())
  speed_define(s)
  p1 <- .adam_levels_apply(s$planner, "data/adam/define.xml", s$path, t = identity)
  p2 <- .adam_levels_clear(p1, s$path)
  p2$study[["treatment"]] <- "TRT01A"
  p2$study[["sd_missing"]] <- "-"
  b1 <- .planner_body(p1)
  b2 <- .planner_body(p2)
  expect_false(identical(b1, b2))
  expect_identical(.planner_core(b1), .planner_core(b2))
  w1 <- .spec_bridge(b1, s$meta)
  w2 <- .spec_bridge(b2, s$meta)
  on.exit(unlink(c(w1$dir, w2$dir), recursive = TRUE), add = TRUE)
  expect_identical(names(w1$text), names(w2$text))
  differ <- names(w1$text)[!vapply(names(w1$text), function(r) identical(w1$text[[r]], w2$text[[r]]), NA)]
  expect_identical(differ, .study_file)
  # and study.yml is what the study-only bridge writes
  o2 <- .spec_bridge_study(b2, s$meta)
  on.exit(unlink(o2$dir, recursive = TRUE), add = TRUE)
  expect_identical(o2$text[[.study_file]], w2$text[[.study_file]])
})
