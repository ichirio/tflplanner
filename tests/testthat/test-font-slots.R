test_that("a font per script is read and shown as the sheets say it", {
  expect_identical(.slots_parse("Courier New"), "Courier New")
  s <- .slots_parse("latin = Courier New | east_asian = MS Mincho")
  expect_identical(s, list(latin = "Courier New", east_asian = "MS Mincho"))
  expect_identical(.slots_show(s), "latin = Courier New | east_asian = MS Mincho")
  expect_identical(.slots_show("Arial"), "Arial")
  expect_error(.slots_parse("latin = | east_asian"), "per script")
  v <- .pgs_value("latin = Arial | east_asian = MS Mincho", "font")
  expect_identical(v, list(list(name = list(latin = "Arial", east_asian = "MS Mincho"))))
  expect_identical(.pgs_show(v, "font"), "latin = Arial | east_asian = MS Mincho")
  expect_identical(.pgs_value("Arial", "font"), list(list(name = "Arial")))
  expect_identical(.pgs_value("east_asian = ja", "slots"), list(east_asian = "ja"))
  expect_identical(.pgs_show(list(east_asian = "ja"), "slots"), "east_asian = ja")
})

test_that("a font per script survives the YAML round trip", {
  v <- .pgs_value("latin = Arial | east_asian = MS Mincho", "font")
  back <- yaml::yaml.load(yaml::as.yaml(list(font_table = v)))
  expect_identical(.pgs_show(back$font_table, "font"), "latin = Arial | east_asian = MS Mincho")
})

test_that("the study's font and language reach report_setup.R", {
  local_home()
  s <- create_study("NEW-J")
  s$planner <- set_study_page_value(s$planner, "font",
                                    "latin = Courier New | east_asian = MS Mincho")
  s$planner <- set_study_page_value(s$planner, "font_lang", "east_asian = ja")
  expect_identical(study_page_value(s$planner, "font_lang"), "east_asian = ja")
  expect_true(.uses_report_setup(s$planner))
  s <- save_study(s)
  set <- readLines(file.path(s$path, "programs", "tfl", "report_setup.R"))
  expect_true(any(grepl('rtfreporter.font = c(latin = "Courier New", east_asian = "MS Mincho")',
                        set, fixed = TRUE)))
  expect_true(any(grepl('rtfreporter.font_lang = c(east_asian = "ja")', set, fixed = TRUE)))
})

test_that("a new study made in the Japanese app keeps Courier New only", {
  local_home()
  .set_config("language", "ja")
  s <- create_study("NEW-JA")
  expect_true(is.na(study_page_value(s$planner, "font")))
  expect_true(is.na(study_page_value(s$planner, "font_lang")))
  set <- readLines(file.path(s$path, "programs", "tfl", "report_setup.R"))
  expect_false(any(grepl("rtfreporter.font", set, fixed = TRUE)))
  s <- suppressMessages(create_sample_study(run = FALSE, study_id = "SMP-JA"))
  expect_true(is.na(study_page_value(s$planner, "font")))
  expect_true(is.na(study_page_value(s$planner, "font_lang")))
})
