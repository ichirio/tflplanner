# A save of the study defaults writes back every block of spec/defaults.yml it
# read: the blocks the screen does not edit as they were, in their order; only
# the edited block changes; a key nobody knows is kept (#794)

skip_if_quick()  # slow: not in the quick check of a PR into next (helper.R)

# The sample study, its spec/defaults.yml given a style block (figure_style:),
# an options: block and a block nobody knows, beside its codelists:,
# adam_meta: and styles: blocks.  Returns the study, its home and the
# defaults.yml path.
keep_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(sample_study_copy(home))
  f <- file.path(s$path, "spec", "defaults.yml")
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  extra <- c("figure_style:", "  base_size: 10", "  palette: [grey20, grey60]",
             "options:", "  digits: 2",
             "fictional_unknown_block:", "  note: kept as it is",
             "  items: [1, 2, 3]")
  cl <- grep("^codelists:", lines)[1L]
  # (the comment lines above codelists: stay with it)
  while (cl > 1L && grepl("^#", lines[cl - 1L])) cl <- cl - 1L
  writeLines(append(lines, extra, after = cl - 1L), f, useBytes = TRUE)
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  list(study = s, home = home, file = f)
}

top_keys <- function(f) {
  l <- readLines(f, encoding = "UTF-8", warn = FALSE)
  sub(":.*$", "", grep("^[A-Za-z_][A-Za-z0-9_]*:", l, value = TRUE))
}

test_that("a save of one edited field of the study defaults keeps every other block (#794)", {
  skip_on_cran()
  k <- keep_sample()
  before <- yaml::read_yaml(k$file)
  keys_before <- top_keys(k$file)
  expect_true(all(c("content", "codelists", "adam_meta", "styles", "figure_style",
                    "options", "fictional_unknown_block") %in% names(before)))
  # one field edited: the study's rounding (plan_digits() in content:)
  s <- k$study
  s$planner <- .spec_set_rounding(s$planner, "r")
  s <- suppressMessages(save_study(s, home = k$home))
  after <- yaml::read_yaml(k$file)
  # every block, in its order
  expect_identical(top_keys(k$file), keys_before)
  # the blocks not edited: as they were
  for (b in setdiff(names(before), "content")) {
    expect_identical(after[[b]], before[[b]], label = b)
  }
  # the edited block: only the rounding changed
  fn <- function(x) vapply(x, function(it) names(it)[1L], "")
  expect_identical(fn(after$content), fn(before$content))
  at <- which(fn(before$content) == "plan_digits")
  expect_identical(after$content[[at]]$plan_digits$rounding, "r")
  expect_identical(after$content[-at], before$content[-at])
})

test_that("a write of the study defaults with nothing edited gives the same blocks back (#794)", {
  skip_on_cran()
  k <- keep_sample()
  before <- yaml::read_yaml(k$file)
  p <- .spec_ensure(k$study$planner)
  rel <- file.path("spec", "defaults.yml")
  q <- .spec_set_study_part(p, "defaults")
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(q$spec$text[[rel]], f, useBytes = TRUE)
  expect_identical(yaml::read_yaml(f), before)
  expect_identical(top_keys(f), top_keys(k$file))
})
