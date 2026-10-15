test_that("a study folder's format: the YAML spec, the workbooks, or nothing", {
  root <- withr_tempdir()
  expect_identical(.study_format(root), "none")
  dir.create(file.path(root, "spec"))
  file.create(file.path(root, "spec", .table_file))
  expect_identical(.study_format(root), "sheets")
  dir.create(file.path(root, "spec", "outputs"))
  expect_identical(.study_format(root), "yaml")
  # a YAML file is the study whole; the workbooks and designs as before
  expect_identical(.spec_part_of(c("spec/outputs/T-1.yml", "spec/toc.yml", "study.yml",
                                   "spec/table_spec.xlsx", "spec/figures/F-1.yml")),
                   c("yaml", "yaml", "meta", "books", "fig:F-1"))
})

# The sample study, made in a home of its own: its definition the YAML spec
yaml_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))
}

test_that("the sample's YAML spec reads back to the same programs", {
  s <- yaml_sample()
  expect_true("spec/toc.yml" %in% .spec_files(s$path))
  p <- .study_spec_keys(.planner_from_yaml(s$path))
  for (id in s$planner$outputs$output_id) {
    expect_identical(program_code(p, id), program_code(s$planner, id), info = id)
  }
  expect_identical(p$setup, s$planner$setup)
  expect_identical(p$outputs, s$planner$outputs)
})

test_that("a YAML study saves the files whose text changed, and keeps study.yml's keys", {
  s <- yaml_sample()
  root <- s$path
  home <- getOption("tflplanner.home")
  # saved as read: the spec's files unchanged but study.yml (its time)
  s <- suppressMessages(save_study(s, home = home))
  f <- s$files
  spec <- f[startsWith(normalizePath(f$file, "/", FALSE), normalizePath(file.path(root, "spec"), "/")) |
              basename(f$file) == .study_file, ]
  expect_identical(spec$status[basename(spec$file) != .study_file],
                   rep("unchanged", sum(basename(spec$file) != .study_file)))
  for (id in s$planner$outputs$output_id) {
    expect_identical(.program_state(s$planner, id, file.path(root, study_layout()[["programs_tfl"]],
                                                             report_info(s$planner, id)$program)),
                     "current", info = id)
  }
  # one report's description changed: its file (and study.yml) written
  i <- match("T-14-1-1", s$planner$outputs$output_id)
  s$planner$outputs$description[i] <- "Demographics (changed)"
  s <- suppressMessages(save_study(s, home = home))
  w <- basename(s$files$file[s$files$status == "written" &
                               grepl("\\.yml$", s$files$file)])
  expect_setequal(intersect(w, c("T-14-1-1.yml", "toc.yml", .study_file)), w)
  expect_true("toc.yml" %in% w || "T-14-1-1.yml" %in% w)
  # study.yml keeps the spec's keys and setup, and the study's fields
  st <- tflspec::tfl_read_study_spec(root)
  expect_identical(st$study$keys$id, "USUBJID")
  expect_false(is.null(st$study$setup))
  expect_identical(.read_meta(root)$study_id, "SAMPLE-01")
  expect_false(is.na(.read_meta(root)$created))
  # and it reads back as saved
  expect_identical(.planner_from_yaml(root)$outputs$description[i], "Demographics (changed)")
})
