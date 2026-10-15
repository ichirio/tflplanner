# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

sample_planner <- function() {
  d <- system.file("extdata", "ard-spec", package = "tflspec")
  read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
}

# a fresh home and study root for one test
local_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

test_that("setup makes the home and remembers where studies go", {
  home <- local_home()
  expect_true(dir.exists(file.path(home, "state")))
  expect_equal(studies_root(), normalizePath(file.path(home, "ws"), "/"))
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws2")))
  expect_match(studies_root(), "ws2$")
})

test_that("a home in the temporary folder is not remembered for later sessions", {
  # the tests write no user settings (helper.R): R_user_dir() is temporary
  expect_true(.in_temp(tools::R_user_dir("tflplanner", "config")))
  old <- options(tflplanner.home = NULL)
  on.exit(options(old), add = TRUE)
  unlink(.pointer_file())
  home <- file.path(withr_tempdir(), "home")
  expect_message(setup_tflplanner(home = home, studies_root = file.path(home, "ws")),
                 "in this R session only")
  # this session's home, not the next one's
  expect_false(file.exists(.pointer_file()))
  expect_identical(tflplanner_home(), normalizePath(home, "/", mustWork = FALSE))
  # a folder outside it is remembered
  expect_false(.in_temp("C:/studies/tflplanner_home"))
  expect_false(.in_temp("/home/u/tflplanner"))
  expect_true(.in_temp(file.path(tempdir(), "x")))
  expect_true(.in_temp(file.path(tempdir(), "not", "made", "yet")))
  # a folder named two ways is one (macOS: /var is /private/var)
  if (.Platform$OS.type != "windows") {
    expect_identical(.real_path("/private/tmp/tflplanner-none/a"),
                     .real_path("/tmp/tflplanner-none/a"))
  }
})

test_that("a new study has its layout and is registered", {
  home <- local_home()
  s <- create_study("ABC-101", title = "A phase 2 study", phase = "2")
  for (d in study_layout()) expect_true(dir.exists(file.path(s$path, d)))
  expect_true(file.exists(file.path(s$path, "study.yml")))
  expect_true(file.exists(file.path(s$path, "ABC-101.Rproj")))
  expect_true(file.exists(file.path(s$path, "spec", "toc.yml")))
  expect_true(file.exists(file.path(home, "state", "ABC-101",
                                    "state.json")))
  expect_error(create_study("ABC-101"), "already registered")
  expect_error(create_study("bad id"), "study id")

  l <- list_studies()
  expect_equal(l$study_id, "ABC-101")
  expect_equal(l$title, "A phase 2 study")
  expect_true(l$folder)
  expect_equal(tflplanner_config()$last_study, "ABC-101")
})

test_that("a study whose workbooks are gone opens as it was last saved", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  s$planner <- set_step_code(s$planner, "DM", "data", "data <- normalize_ard(my_ard)\n# 2")
  s$planner$outputs$description[2] <- "有害事象"
  s$planner$setup <- "library(cards)"
  s$planner$sheets$variables$label[1] <- "Age"
  s$meta$title <- "Title"
  s <- save_study(s)

  # the definition's files gone: they do not read, so the study opens as last saved
  # (and cannot be saved until they are written back, #274)
  unlink(file.path(s$path, "spec"), recursive = TRUE)
  o <- suppressMessages(open_study("S1"))
  expect_identical(o$spec$status, "invalid")
  expect_identical(o$planner$sheets, s$planner$sheets)
  expect_identical(o$planner$outputs, s$planner$outputs)
  expect_identical(o$planner$setup, s$planner$setup)
  expect_identical(o$planner$study, s$planner$study)
  expect_equal(o$meta$title, "Title")
  expect_equal(o$path, s$path)
})

test_that("each save that changes the study keeps the previous state", {
  home <- local_home()
  s <- create_study("S1", planner = sample_planner())
  hist <- file.path(home, "state", "S1", "history")
  expect_length(list.files(hist), 0)
  s <- save_study(s)
  expect_length(list.files(hist), 0)              # nothing changed
  s$planner$outputs$description[1] <- "changed"
  s <- save_study(s)
  expect_length(list.files(hist), 1)
})

test_that("an existing folder is registered from its workbooks", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  unregister_study("S1")
  expect_equal(nrow(list_studies()), 0)
  expect_true(dir.exists(s$path))
  expect_error(open_study("S1"), "not registered")

  r <- open_study(s$path)                         # registers it
  expect_equal(nrow(list_studies()), 1)
  expect_identical(r$planner$sheets, s$planner$sheets)

  # a moved folder is followed when opened from where it is
  to <- file.path(dirname(s$path), "moved")
  file.rename(s$path, to)
  expect_false(list_studies()$folder)
  m <- open_study(to)
  expect_equal(m$path, normalizePath(to, "/"))
  expect_true(list_studies()$folder)
})

test_that("a study's files export, and import into another study", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  d <- withr_tempdir()
  export_spec_files("S1", d)
  expect_true(file.exists(file.path(d, "spec", "toc.yml")))
  expect_true(file.exists(file.path(d, .flat_xlsx)))
  e <- create_study("S2")
  e <- import_spec_files("S2", d)
  # (the study's own ID and fields kept, the definition's settings taken)
  expect_identical(e$meta$study_id, "S2")
  expect_identical(e$planner$outputs, s$planner$outputs)
  expect_identical(program_code(e$planner, "DM"), program_code(s$planner, "DM"))
  expect_equal(unname(e$planner$study[["output_path"]]), "output/tfl")
})

test_that("saving writes the programs from the definition, an edited one too", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  # the YAML spec's files, the programs, batch.R, the two autoexec programs,
  # fig_setup.R, report_setup.R, study_setup.R, study_helpers.R
  expect_equal(sum(s$files$status == "written"), length(.spec_files_yaml(s$path)) + 5 + 7)
  s0 <- save_study(s)
  expect_true(all(s0$files$status == "unchanged"))
  f <- file.path(s$path, "programs", "tfl", "DM.R")
  expect_equal(study_status(s)$program_state, rep("current", 5))

  # an untouched program follows the definition
  s$planner <- set_step_code(s$planner, "DM", "data", "data <- normalize_ard(my_ard)")
  expect_equal(study_status(s)$program_state[1], "generated")
  expect_equal(study_status(s)$status[1], "unsaved")
  s <- save_study(s)
  expect_true("data <- normalize_ard(my_ard)" %in% readLines(f))
  expect_equal(study_status(s)$program_state[1], "current")

  # nothing changed: nothing is rewritten
  s2 <- save_study(s)
  expect_true(all(s2$files$status == "unchanged"))

  # a hand edit: written again from the definition, the edited one kept
  # in programs/.edited/ (a program is never edited: the definition is)
  writeLines(c(readLines(f), "# mine"), f)
  expect_equal(study_status(s)$program_state[1], "edited")
  s <- save_study(s)
  expect_false("# mine" %in% readLines(f))
  expect_equal(s$files$status[basename(s$files$file) == "DM.R"], "rewritten")
  expect_equal(study_status(s)$program_state[1], "current")
  bk <- list.files(file.path(s$path, "programs", ".edited"), full.names = TRUE)
  expect_length(bk, 1L)
  expect_match(basename(bk), "^tfl_DM_[0-9]{8}-[0-9]{6}[.]R$")
  expect_equal(utils::tail(readLines(bk), 1), "# mine")
  # an ARD program too
  s$planner$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = "ADSL", level = "adam", path = "data/adam/adsl.rds"), "datasets")
  s$planner$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "DM", analysis_id = "A1", method = "continuous",
    dataset = "ADSL", variables = "AGE", stringsAsFactors = FALSE), "analyses")
  s <- save_study(s)
  fa <- file.path(s$path, "programs", "ard", "DM.R")
  writeLines(c(readLines(fa), "# mine too"), fa)
  s <- save_study(s)
  expect_false("# mine too" %in% readLines(fa))
  expect_equal(s$files$status[s$files$file == fa], "rewritten")
  expect_true(any(grepl("^ard_DM_",
                        list.files(file.path(s$path, "programs", ".edited")))))
})

test_that("a study runs end to end and reports what it produced", {
  skip_if_not_installed("cards")
  skip_on_cran()
  local_home()
  p <- sample_planner()
  for (id in c("AE", "ORR", "LB", "PK")) p <- remove_output(p, id)
  p$setup <- "library(cards)"
  p <- set_step_code(p, "DM", "data", paste(
    "adsl <- readRDS(\"data/adam/adsl.rds\")",
    "ard <- ard_stack(",
    "  adsl, .by = TRT01A,",
    "  ard_continuous(variables = c(AGE, WEIGHTBL),",
    "                 statistic = ~ continuous_summary_fns(",
    "                   c(\"N\", \"mean\", \"sd\", \"median\", \"min\", \"max\"))),",
    "  ard_categorical(variables = c(AGEGR1, SEX, RACE)),",
    "  .total_n = TRUE)",
    "data <- normalize_ard(ard)", sep = "\n"))
  p <- add_output(p, "L1", type = "listing")
  s <- create_study("S2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  expect_equal(study_files(s, "data")$file, "adsl.rds")
  expect_equal(nrow(read_data_head(study_files(s)$path, n = 5)), 5)

  expect_equal(study_status(s)$status, c("not run", "todo"))
  st <- run_study(s)
  expect_equal(st$status, c("ok", "todo"))
  expect_true(file.exists(file.path(s$path, "output", "tfl", "DM.rtf")))
  expect_true(file.exists(file.path(s$path, "output", "ard", "DM.rds")))
  expect_true(file.exists(file.path(s$path, "logs", "preview", "DM.log")))

  # saving an unchanged study leaves the RTF current
  s <- save_study(s)
  expect_equal(study_status(s)$status[1], "ok")

  # a definition changed after the RTF was made
  Sys.sleep(1.1)
  s$planner$sheets$variables$label[1] <- "Age"
  s <- save_study(s)
  expect_equal(study_status(s)$status[1], "outdated")

  # a change to one report leaves the others current: the workbooks are
  # rewritten, but only that report's program changes
  st <- run_study(s, "DM")
  expect_equal(st$status[1], "ok")
  s$planner <- copy_output(s$planner, "DM", "DM2")
  s <- save_study(s)
  st <- run_study(s, "DM2")
  expect_equal(study_status(s)$status[study_status(s)$output_id %in% c("DM", "DM2")],
               c("ok", "ok"))
  Sys.sleep(1.1)
  i <- which(s$planner$outputs$output_id == "DM2")
  s$planner$outputs$description[i] <- "Another description"
  s <- save_study(s)
  ss <- study_status(s)
  expect_equal(ss$status[ss$output_id == "DM2"], "outdated")
  expect_equal(ss$status[ss$output_id == "DM"], "ok")

  # a file the program sources (the figure setup) changes: its reports too
  f <- file.path(s$path, "programs", "tfl", "DM.R")
  extra <- file.path(s$path, "programs", "tfl", "extra.R")
  writeLines("x <- 1", extra)
  writeLines(c(readLines(f), 'source("programs/tfl/extra.R")'), f)
  Sys.sleep(1.1)
  st <- run_study(s, "DM")
  expect_equal(st$status[1], "ok")
  Sys.sleep(1.1)
  writeLines("x <- 2", extra)
  expect_equal(study_status(s)$status[1], "outdated")

  # a program that fails
  writeLines("stop('boom')", file.path(s$path, "programs", "tfl", "DM.R"))
  st <- run_study(s, "DM")
  expect_equal(st$status[1], "error")
})

test_that("every study folder has its note on the study tab", {
  lay <- study_layout()
  notes <- .study_folder_notes(names(lay))
  expect_length(notes, length(lay))
  expect_true(all(nzchar(notes)))
  # a folder with no note yet shows none instead of breaking the tab
  expect_identical(.study_folder_notes(c("adam", "new_one"))[2], "")
  # and every note is translated
  tr <- utils::read.csv(system.file("i18n", "strings.csv", package = "tflplanner"),
                        encoding = "UTF-8", check.names = FALSE)
  expect_true(all(notes %in% tr$en))
})

test_that("a study folder registered again keeps its designed figures", {
  local_home()
  p <- add_output(new_planner(), "F-1", type = "figure")
  p <- set_fig_design(p, "F-1", tflspec::tfl_fig_template("km_simple", data = "ADTTE"))
  s <- create_study("FD", planner = p)
  expect_true(file.exists(file.path(s$path, "spec/outputs/F-1.yml")))
  unregister_study("FD")
  s2 <- register_study(s$path)
  expect_identical(fig_design(s2$planner, "F-1")$template, "km_simple")
  # saved again: the design stays in its report's file
  save_study(s2)
  expect_identical(fig_design(.planner_from_yaml(s$path), "F-1")$template, "km_simple")
})

test_that("unregistering and registering again gives the study back as it was", {
  home <- local_home()
  body <- function(id) {
    st <- .read_state(id, home)
    st$saved <- NULL
    st$tflplanner <- NULL
    .json(st)
  }
  s <- suppressMessages(create_study("UR-1", title = "Kept"))
  s$planner <- add_output(s$planner, "T-1", type = "table")
  s$planner <- set_sheet_rows(s$planner, "titles", "T-1",
                              data.frame(line = "1", center = "My title"))
  s$planner$setup <- "library(dplyr)"
  s <- suppressMessages(save_study(s, home = home))
  s$planner$outputs$description[1] <- "second save"
  s <- suppressMessages(save_study(s, home = home))
  before <- body("UR-1")
  hist <- list.files(file.path(.store_dir("UR-1", home), "history"))
  expect_true(length(hist) >= 1L)
  # an unsaved change, kept as a draft
  d <- s
  d$planner$outputs$description[1] <- "not saved yet"
  .write_draft(d, home)
  unregister_study("UR-1")
  expect_false(dir.exists(.store_dir("UR-1", home)))
  expect_true(file.exists(file.path(s$path, ".tflplanner", "state.json")))
  expect_true(dir.exists(s$path))
  r <- register_study(s$path)
  expect_identical(body("UR-1"), before)
  expect_identical(list.files(file.path(.store_dir("UR-1", home), "history")),
                   hist)
  expect_identical(.read_draft("UR-1", home)$planner$outputs$description[1],
                   "not saved yet")
  expect_false(dir.exists(file.path(s$path, ".tflplanner")))
  expect_identical(r$planner$setup, "library(dplyr)")
})

test_that("a study whose spec/ changed after unregistering comes back from spec/", {
  home <- local_home()
  s <- suppressMessages(create_study("UR-2"))
  s$planner <- add_output(s$planner, "T-1", type = "table", description = "kept")
  s <- suppressMessages(save_study(s, home = home))
  unregister_study("UR-2")
  # the report list (the TOC) edited while the study was off the list
  f <- file.path(s$path, "spec", "toc.yml")
  y <- yaml::read_yaml(f)
  y$outputs[[1L]]$label <- "edited"
  writeLines(yaml::as.yaml(y), f)
  r <- suppressMessages(register_study(s$path))
  expect_identical(r$planner$outputs$output_id, "T-1")
  expect_identical(r$planner$outputs$description, "edited")
  # the kept state is in the history, not lost
  expect_true(length(list.files(file.path(.store_dir("UR-2", home), "history"))) >= 1L)
  expect_false(dir.exists(file.path(s$path, ".tflplanner")))
})

# --- the home and the study folders (#359) -----------------------------

test_that("the home is never inside the study folders, nor inside a study", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = NULL)
  on.exit(options(old), add = TRUE)
  # a new home: its study folders inside it
  h1 <- file.path(home, "h1")
  suppressMessages(setup_tflplanner(home = h1))
  expect_identical(studies_root(h1), normalizePath(file.path(h1, "studies"), "/"))
  # the home inside the study folders: refused, nothing written
  h2 <- file.path(home, "studies", "tflplanner_home")
  expect_error(setup_tflplanner(home = h2, studies_root = file.path(home, "studies")),
               "inside .+ where new study folders go")
  expect_false(dir.exists(h2))
  # nor the same folder
  expect_error(setup_tflplanner(home = h1, studies_root = h1), "where new study folders go")
  # beside each other: kept
  suppressMessages(setup_tflplanner(home = h1, studies_root = file.path(home, "elsewhere")))
  expect_match(studies_root(h1), "elsewhere$")
  # inside a study's folder
  st <- file.path(home, "ST")
  dir.create(st)
  writeLines("study_id: ST", file.path(st, "study.yml"))
  expect_error(setup_tflplanner(home = file.path(st, "app"), studies_root = file.path(home, "s2")),
               "inside the study folder")
  # the problem as the app says it
  expect_match(.error_view(.home_problem(h2, file.path(home, "studies")), identity),
               "^The home ")
})

test_that("a study's state at the old place is moved to <home>/state, never deleted", {
  home <- local_home()
  s <- create_study("S1", planner = sample_planner())
  # as before 0.0.2.9200: <home>/studies/<ID>
  old <- file.path(home, "studies", "S1")
  dir.create(dirname(old), showWarnings = FALSE)
  file.rename(file.path(home, "state", "S1"), old)
  # a study folder that was its own state's folder keeps its files
  both <- file.path(home, "studies", "S2")
  dir.create(both)
  file.copy(file.path(old, "state.json"), both)
  writeLines("study_id: S2", file.path(both, "study.yml"))
  rm(list = ls(.moved_homes), envir = .moved_homes)
  expect_true("S1" %in% list_studies()$study_id)
  expect_true(file.exists(file.path(home, "state", "S1", "state.json")))
  expect_false(dir.exists(old))
  expect_true(file.exists(file.path(home, "state", "S2", "state.json")))
  expect_true(file.exists(file.path(both, "study.yml")))
  # one already at the new place stays where it was
  dir.create(file.path(home, "studies", "S3"))
  dir.create(file.path(home, "state", "S3"))
  writeLines("{}", file.path(home, "studies", "S3", "state.json"))
  writeLines("{}", file.path(home, "state", "S3", "state.json"))
  rm(list = ls(.moved_homes), envir = .moved_homes)
  expect_message(.move_old_stores(home), "is not moved")
  expect_true(file.exists(file.path(home, "studies", "S3", "state.json")))
})

test_that("one call makes the whole layout; a home that uses workspace keeps it", {
  old <- options(tflplanner.home = NULL)
  on.exit(options(old), add = TRUE)
  h <- file.path(withr_tempdir(), "tflplanner_home")
  local_mocked_bindings(create_sample_study = function(root, home) {
    dir.create(file.path(root, "SAMPLE-01"), recursive = TRUE)
    invisible(NULL)
  })
  suppressMessages(setup_tflplanner(home = h, sample = TRUE))
  expect_true(dir.exists(file.path(h, "state")))
  expect_true(dir.exists(file.path(h, "studies", "SAMPLE-01")))
  expect_identical(studies_root(h), normalizePath(file.path(h, "studies"), "/"))
  # a home set up before, its study folders at <home>/workspace: kept
  h2 <- file.path(withr_tempdir(), "older")
  dir.create(h2)
  yaml::write_yaml(list(studies_root = file.path(h2, "workspace")), file.path(h2, "config.yml"))
  suppressMessages(setup_tflplanner(home = h2, language = "ja"))
  expect_match(studies_root(h2), "workspace$")
})

test_that("a new study's place: never one the home is in; a drive's root joined right", {
  home <- local_home()
  # the joins: a drive's root, a drive alone, a trailing slash
  expect_identical(.study_path("Q:/", "S-1", home = home), "Q:/S-1")
  expect_identical(.study_path("Q:", "S-1", home = home), "Q:/S-1")
  expect_identical(.study_path("Q:/work/", "S-1", home = home), "Q:/work/S-1")
  # blank: the one set up
  expect_identical(.study_path("", "S-1", home = home), file.path(studies_root(home), "S-1"))
  # the home inside the place: refused, nothing made
  up <- dirname(home)
  expect_error(create_study("S-2", root = up), "the home .+ is inside it")
  expect_false(dir.exists(file.path(up, "S-2")))
  expect_error(create_sample_study(root = up, run = FALSE, study_id = "SAMPLE-09"),
               "the home .+ is inside it")
  expect_false(dir.exists(file.path(up, "SAMPLE-09")))
  # inside a study's folder: refused
  s <- create_study("S-3")
  expect_error(create_study("S-4", root = s$path), "inside the study folder")
  # a study folder the home is in: not registered
  fake <- dirname(home)
  writeLines("study_id: FAKE", file.path(fake, "study.yml"))
  on.exit(unlink(file.path(fake, "study.yml")), add = TRUE)
  expect_error(register_study(fake), "is inside the study folder")
})

test_that("a folder that cannot be made says which and why; the app says it in its words", {
  f <- tempfile()
  writeLines("x", f)
  expect_error(.make_dir(file.path(f, "sub")), "^Could not make the folder .+sub \\(.+\\)\\.$")
  ja <- function(x) tr(x, "ja")
  expect_identical(.error_view("Could not make the folder C:/x/y (Permission denied).", ja),
                   "\u30d5\u30a9\u30eb\u30c0\u3092\u4f5c\u308c\u307e\u305b\u3093\u3067\u3057\u305f\uff1aC:/x/y\uff08Permission denied\uff09")
  m <- tryCatch(.study_path("C:/", "S", home = "C:/tflplanner_home"), error = conditionMessage)
  expect_match(.error_view(m, ja), "^C:/ ")
  expect_match(.error_view(m, ja), "C:/tflplanner_home", fixed = TRUE)
  # the English app: the message as it is
  expect_identical(.error_view(m), m)
})

test_that("the new-study dialog says a place the home is in, under the field", {
  local_home()
  create_study("S1")
  shiny::testServer(server_for("S1"), {
    session$setInputs(new_study = 1)
    session$setInputs(ns_id = "NEW-1", ns_root = dirname(tflplanner_home()))
    expect_match(output$ns_id_check$html, "is inside it", fixed = TRUE)
    session$setInputs(ns_ok = 1)
    expect_false("NEW-1" %in% list_studies()$study_id)
  })
})
