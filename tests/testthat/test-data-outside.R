# (slow: its tests write study folders -- run on CI and locally with
# NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

# A study whose ADaM is kept outside the study folder, by its full path
# (#430): the sample study, its data/adam moved to a folder of its own
outside_study <- function(env = parent.frame(), outside = TRUE) {
  base <- withr_tempdir(env)
  home <- file.path(base, "home")
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(base, "ws"), language = "en"))
  src <- system.file("sample", "SAMPLE-01", package = "tflplanner")
  path <- file.path(base, "ws", "SAMPLE-01")
  dir.create(path, recursive = TRUE)
  files <- list.files(src, recursive = TRUE, all.files = TRUE)
  files <- files[!grepl(if (outside) "^data/|[.]Rproj$" else "[.]Rproj$", files)]
  for (f in files) {
    dir.create(file.path(path, dirname(f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(src, f), file.path(path, f))
  }
  if (!outside) return(list(path = path, home = home))
  adam <- file.path(base, "secure", "adam")
  dir.create(adam, recursive = TRUE)
  file.copy(list.files(file.path(src, "data", "adam"), full.names = TRUE), adam)
  adam <- normalizePath(adam, "/")
  yml <- file.path(path, "spec", "data.yml")
  y <- readLines(yml, encoding = "UTF-8")
  writeLines(sub("path: data/adam/", paste0("path: ", adam, "/"), y, fixed = TRUE), yml,
             useBytes = TRUE)
  list(path = path, adam = adam, home = home)
}

test_that("a catalog path is a file of the study folder, or the path written whole", {
  expect_identical(.data_file("C:/st/S1", "data/adam/adsl.rds"), "C:/st/S1/data/adam/adsl.rds")
  expect_identical(.data_file("C:/st/S1", "D:/secure/adam/adsl.rds"), "D:/secure/adam/adsl.rds")
  expect_identical(.data_file("C:/st/S1", "D:\\secure\\adam\\adsl.rds"), "D:/secure/adam/adsl.rds")
  expect_identical(.data_file("/st/S1", "/secure/adam/adsl.rds"), "/secure/adam/adsl.rds")
  expect_identical(.data_file("C:/st/S1", "\\\\server\\adam\\adsl.rds"), "//server/adam/adsl.rds")
  expect_identical(.data_file("C:/st/S1", c(NA, "")), c(NA_character_, NA_character_))
  # outside: written whole and not under the folder, or going up out of it
  expect_identical(
    .data_outside("C:/st/S1", c("data/adam/adsl.rds", "D:/secure/adsl.rds",
                                "C:/st/S1/data/adam/adsl.rds", "../adam/adsl.rds",
                                "C:/st/S10/adsl.rds", NA)),
    c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))
  # the Data tab: a file outside goes under its level's folder
  expect_identical(.dataset_pill(c("D:/secure/adsl.rds", "../x/dm.xpt", "D:/o/a.csv"),
                                 c("ADaM", "SDTM", NA)),
                   c("adam", "sdtm", "other"))
})

test_that("a study with its ADaM outside the study folder reads it (#430)", {
  st <- outside_study()
  s <- suppressMessages(register_study(st$path))
  # the RStudio project the folder had not
  expect_true(file.exists(file.path(st$path, "SAMPLE-01.Rproj")))
  ds <- s$planner$ard$datasets
  expect_true(all(startsWith(ds$path, st$adam)))
  expect_true(all(.data_outside(s$path, ds$path)))
  expect_true(all(file.exists(.data_file(s$path, ds$path))))
  # opened again: the same paths
  s <- open_study("SAMPLE-01")
  expect_identical(s$planner$ard$datasets$path, ds$path)
  # D01 (a dataset with no file) says nothing; the facts of the data are read
  expect_null(.rule_d01(s))
  f <- review_facts(s, refresh = TRUE)
  expect_true(all(c("ADSL", "ADAE") %in% names(f$datasets)))
  expect_identical(f$datasets$ADSL$n, 200L)
  # the ARS check's columns, the counts of the data choices
  cols <- .ars_columns(s, s$planner$ard)
  expect_true("USUBJID" %in% cols$ADSL)
  expect_true(length(.an_data_counts(s)) > 0L)
  # a file outside that is not there is still missing
  file.rename(file.path(st$adam, "advs.rds"), file.path(st$adam, "advs-gone.rds"))
  expect_identical(.rule_d01(s)$row[1L], "ADVS")
  file.rename(file.path(st$adam, "advs-gone.rds"), file.path(st$adam, "advs.rds"))
  # the official run keeps it (data/external/)
  plan <- batch_data_plan(s)
  expect_true(all(!is.na(plan$size)))
})

test_that("the programs read the path as written, and a save changes nothing (#430)", {
  # the same study with its data in the study folder (a home of its own)
  tw <- outside_study(outside = FALSE)
  ref <- suppressMessages(register_study(tw$path, home = tw$home))$planner
  st <- outside_study()
  s <- suppressMessages(register_study(st$path, home = st$home))
  p <- s$planner
  d <- as.Date("2026-01-01")
  code <- function(p) lapply(stats::setNames(p$outputs$output_id, p$outputs$output_id),
                             program_code, x = p, date = d)
  mine <- code(p)
  # the one difference: the path, written whole instead of under path_adam
  ref <- lapply(code(ref), gsub, pattern = 'file[.]path[(]path_adam, "([^"]+)"[)]',
                replacement = paste0('"', st$adam, '/\\1"'))
  expect_identical(mine, ref)
  expect_true(any(grepl(st$adam, unlist(mine), fixed = TRUE)))
  ard <- ard_program_code(.ard_spec(p$ard), "T-14-1-1", date = d, dir = s$path,
                          codelists = .study_codelists(p))
  expect_true(any(grepl(st$adam, ard, fixed = TRUE)))
  # a save with nothing changed: no file changes
  s2 <- suppressMessages(save_study(s))
  md5 <- function() {
    f <- list.files(st$path, recursive = TRUE, full.names = TRUE)
    tools::md5sum(f[!grepl("[.]tflplanner|/logs?/|draft", f)])
  }
  m1 <- md5()
  suppressMessages(save_study(s2))
  expect_identical(md5(), m1)
  # the spec keeps the full path
  y <- readLines(file.path(st$path, "spec", "data.yml"), encoding = "UTF-8")
  expect_true(any(grepl(paste0("path: ", st$adam, "/adsl.rds"), y, fixed = TRUE)))
})
