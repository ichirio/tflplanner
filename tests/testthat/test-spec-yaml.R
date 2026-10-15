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

test_that("the sample's definition as YAML reads back to the same programs", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  # its spec/ as the YAML spec (tflspec's converter), in a copy
  root <- file.path(withr_tempdir(), "study")
  dir.create(root)
  file.copy(list.files(s$path, full.names = TRUE, all.files = TRUE, no.. = TRUE), root,
            recursive = TRUE)
  y <- file.path(withr_tempdir(), "y")
  tflspec::tfl_convert_sheets_to_yaml(root, y, overwrite = TRUE)
  unlink(file.path(root, "spec"), recursive = TRUE)
  dir.create(file.path(root, "spec"))
  file.copy(list.files(file.path(y, "spec"), full.names = TRUE), file.path(root, "spec"),
            recursive = TRUE)
  file.copy(file.path(y, "study.yml"), file.path(root, "study.yml"), overwrite = TRUE)
  expect_identical(.study_format(root), "yaml")
  expect_true("spec/toc.yml" %in% .spec_files(root))
  p <- .study_spec_keys(.planner_from_yaml(root))
  for (id in s$planner$outputs$output_id) {
    expect_identical(program_code(p, id), program_code(s$planner, id), info = id)
  }
  expect_identical(p$setup, s$planner$setup)
  expect_identical(p$outputs, s$planner$outputs)
})
