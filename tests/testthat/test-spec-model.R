# The study's model is its YAML spec; the sheets are a view of it (#407 B1)

model_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))
}

spec_md5 <- function(root) {
  f <- .spec_files_yaml(root)
  stats::setNames(unname(tools::md5sum(file.path(root, f))), f)
}

changed_files <- function(a, b) {
  k <- union(names(a), names(b))
  k[!vapply(k, function(x) identical(a[x], b[x]), NA)]
}

test_that("an opened study holds its spec, in step with the sheets' view", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  sp <- s$planner$spec
  expect_s3_class(sp$study, "tfl_study_spec")
  expect_setequal(names(sp$outputs), s$planner$outputs$output_id)
  expect_true(all(vapply(sp$outputs, inherits, NA, "tfl_output")))
  expect_identical(sp$where[["T-14-1-1"]], "spec/outputs/T-14-1-1.yml")
  expect_true(all(c(.study_file, "spec/toc.yml") %in% names(sp$text)))
  expect_true(.same_planner(sp$base, s$planner))
  # the view made from the spec is the sheets the screens read
  v <- .planner_from_spec(sp$study, sp$outputs)
  expect_identical(v$sheets$tables, s$planner$sheets$tables)
  expect_identical(v$outputs, .planner_body(s$planner)$outputs)
  # the spec is not part of what is compared or kept
  expect_null(.state_of(s)$planner$spec)
  expect_true(.same_planner(s$planner, .planner_body(s$planner)))
})

test_that("opened and saved with no change, no spec file is written", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), character())
  y <- s$files[grepl("\\.ya?ml$", s$files$file), ]
  expect_true(all(y$status == "unchanged"))
})

test_that("an edit of one report's sheet rows writes that report's file only", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  id <- "T-14-1-1"
  d <- sheet_rows(s$planner, "tables", id)
  d$output_id <- NULL
  d$note <- "an edit"
  s$planner <- set_sheet_rows(s$planner, "tables", id, d)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), "spec/outputs/T-14-1-1.yml")
  # the spec in memory follows, in step again
  expect_true(.same_planner(s$planner$spec$base, s$planner))
  expect_identical(s$planner$spec$text[["spec/outputs/T-14-1-1.yml"]],
                   readLines(file.path(s$path, "spec/outputs/T-14-1-1.yml"), encoding = "UTF-8"))
  # and reads back as saved
  s2 <- suppressMessages(open_study("SAMPLE-01", home = home))
  expect_identical(sheet_rows(s2$planner, "tables", id)$note[1L], "an edit")
})

test_that("what the sheets cannot hold stays in a report the edits do not touch", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  # a page key with no sheet, written straight into one report's spec
  o <- s$planner$spec$outputs[["T-14-3-1"]]
  o$page$kept_key <- "kept"
  s$planner <- .spec_set_output(s$planner, "T-14-3-1", o)
  expect_identical(s$planner$spec$outputs[["T-14-3-1"]]$page$kept_key, "kept")
  # an edit of another report through its sheets
  d <- sheet_rows(s$planner, "tables", "T-14-1-1")
  d$output_id <- NULL
  d$note <- "another edit"
  s$planner <- set_sheet_rows(s$planner, "tables", "T-14-1-1", d)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_setequal(changed_files(m0, spec_md5(s$path)),
                  c("spec/outputs/T-14-1-1.yml", "spec/outputs/T-14-3-1.yml"))
  back <- tflspec::tfl_read_output(file.path(s$path, "spec/outputs/T-14-3-1.yml"))
  expect_identical(back$page$kept_key, "kept")
  # the programs as before
  expect_identical(program_code(s$planner, "T-14-3-1"),
                   program_code(.planner_body(s$planner), "T-14-3-1"))
})

test_that("an edit of a report's sheets keeps what only its spec holds (#415)", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  id <- "T-14-3-1"
  rel <- "spec/outputs/T-14-3-1.yml"
  # a content argument and a page key with no sheet, written into the spec
  o <- s$planner$spec$outputs[[id]]
  k <- which(vapply(o$content, function(i) i$fn, "") == "table_plan")
  o$content[[k]]$args$kept_arg <- "kept"
  o$page$kept_key <- "kept"
  s$planner <- .spec_set_output(s$planner, id, o)
  # (a) the same report's ARD sheet and page sheet edited, then saved
  a <- ard_rows(s$planner, "analyses", id)
  a$output_id <- NULL
  a$label <- "An edited label"
  s$planner <- set_ard_rows(s$planner, "analyses", id, a)
  f <- sheet_rows(s$planner, "footer", id)
  f$output_id <- NULL
  f$left <- "An edited footnote."
  s$planner <- set_sheet_rows(s$planner, "footer", id, f)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), rel)
  back <- tflspec::tfl_read_output(file.path(s$path, rel))
  # the edits are in
  expect_identical(back$ard$analyses[[1L]]$label, "An edited label")
  expect_identical(back$page$footer[["1"]]$left, "An edited footnote.")
  # and what only the spec held is kept
  expect_identical(back$content[[k]]$args$kept_arg, "kept")
  expect_identical(back$page$kept_key, "kept")
  # the other parts as they were
  expect_identical(back$content[-k], o$content[-k])
  expect_identical(back$codelists, o$codelists)
  # saved again with no change: no file written
  m1 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m1, spec_md5(s$path)), character())
})

test_that("a draft keeps what only the spec holds, and gives it back (#415)", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  id <- "T-14-3-1"
  rel <- "spec/outputs/T-14-3-1.yml"
  # a draft of nothing changed holds no spec
  expect_null(.draft_spec_of(s))
  o <- s$planner$spec$outputs[[id]]
  k <- which(vapply(o$content, function(i) i$fn, "") == "table_plan")
  o$content[[k]]$args$kept_arg <- "kept"
  s$planner <- .spec_set_output(s$planner, id, o)
  # then the same report's page sheet edited (not yet in the spec)
  f <- sheet_rows(s$planner, "footer", id)
  f$output_id <- NULL
  f$left <- "An edited footnote."
  s$planner <- set_sheet_rows(s$planner, "footer", id, f)
  .write_draft(s, home)
  d <- .read_draft("SAMPLE-01", home)
  expect_false(is.null(d$planner$spec))
  expect_identical(d$planner$spec$outputs[[id]]$content[[k]]$args$kept_arg, "kept")
  # taken back (as the app does) and saved: both are in the file
  d$planner <- .study_spec_keys(d$planner)
  m0 <- spec_md5(d$path)
  d <- suppressMessages(save_study(d, home = home))
  expect_identical(changed_files(m0, spec_md5(d$path)), rel)
  back <- tflspec::tfl_read_output(file.path(d$path, rel))
  expect_identical(back$content[[k]]$args$kept_arg, "kept")
  expect_identical(back$page$footer[["1"]]$left, "An edited footnote.")
  # a report removed in the spec stays removed
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  s$planner <- .spec_set_output(s$planner, "T-14-1-2", NULL)
  .write_draft(s, home)
  d <- .read_draft("SAMPLE-01", home)
  expect_false("T-14-1-2" %in% names(d$planner$spec$outputs))
  expect_false("T-14-1-2" %in% d$planner$outputs$output_id)
})

test_that("a three-way merge keeps the spec's own keys and takes the edits", {
  spec <- list(a = 1, b = list(x = 1, y = 2), only = "s")
  was <- list(a = 1, b = list(x = 1))
  now <- list(b = list(x = 3), c = 4)
  # a removed, b$x changed, c added, b$y and only kept
  expect_identical(.merge3(spec, was, now), list(b = list(x = 3, y = 2), c = 4, only = "s"))
  # unchanged by the edits: the spec's
  expect_identical(.merge3(spec, was, was), spec)
  # a sequence changed: the edits' whole
  expect_identical(.merge3(list(1, 2), list(1), list(3)), list(3))
})

test_that("a report written into the spec has its view made again", {
  s <- model_sample()
  id <- "T-14-1-1"
  o <- s$planner$spec$outputs[[id]]
  p <- .spec_set_output(s$planner, id, o)
  # the same report: the same view
  expect_identical(.planner_body(p)$sheets, .planner_body(s$planner)$sheets)
  expect_identical(p$outputs, s$planner$outputs)
  # its population changed in the spec: the report list says it
  o$population <- "SAF"
  p <- .spec_set_output(s$planner, id, o)
  expect_identical(.spec_output(p, id)$population, "SAF")
  expect_true(.same_planner(p$spec$base, p))
  expect_error(.spec_set_output(.planner_body(s$planner), id, o), "no YAML spec")
})

test_that("a planner with no spec is written whole, and gets one", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s$planner <- .planner_body(s$planner)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_false(is.null(s$planner$spec))
  expect_setequal(names(s$planner$spec$outputs), s$planner$outputs$output_id)
  # the sample's files are what the bridge writes of it
  expect_identical(changed_files(m0, spec_md5(s$path)), character())
})

test_that("a report's items say where each argument comes from (#417)", {
  s <- model_sample()
  p <- s$planner
  x <- .spec_items(p, "T-14-1-1")
  fns <- vapply(x$items, function(it) it$fn, "")
  expect_true(all(c("table_plan", "normalize_ard", "plan_cells") %in% fns))
  expect_identical(vapply(x$own, function(it) it$fn, "")[1L], "table_plan")
  # a named-dots verb key by key; any other verb the layer's whole
  pc <- x$items[[match("plan_cells", fns)]]
  expect_identical(pc$arg_from[["default"]], "defaults")
  expect_identical(pc$arg_from[["continuous"]], "own")
  expect_identical(pc$below$fn, "plan_cells")
  st <- x$items[[match("plan_stub", fns)]]
  expect_true(all(st$arg_from == "defaults"))
  tp <- x$items[[match("table_plan", fns)]]
  expect_true(all(tp$arg_from == "own"))
  expect_null(tp$below)
  # T-14-3-1's normalization is its own
  y <- .spec_items(p, "T-14-3-1")
  na <- Filter(function(it) identical(it$fn, "normalize_ard"), y$items)[[1L]]
  expect_identical(na$from, "own")
  expect_true("hierarchy" %in% names(na$arg_from))
  # no spec, no such report, or edits of the sheets not in it yet: NULL
  expect_null(.spec_items(.planner_body(p), "T-14-1-1"))
  expect_null(.spec_items(p, "T-99"))
  d <- sheet_rows(p, "tables", "T-14-1-1")
  d$output_id <- NULL
  d$note <- "an edit"
  expect_null(.spec_items(set_sheet_rows(p, "tables", "T-14-1-1", d), "T-14-1-1"))
})
