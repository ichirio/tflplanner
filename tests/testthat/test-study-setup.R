# The study's setup (#268, #910): three files -- programs/company_setup.R
# (the company's, copied), programs/study_setup.R (generated on every save;
# it runs the other two, the company's first and the user's last, once per
# program) and programs/study_user.R (the user's) -- and study_setup.R
# sourced by ard_setup.R, report_setup.R and fig_setup.R.  A study made
# before has one study_setup.R of three marked parts: its first save splits
# it.

# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

setup_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "DM"), analysis_id = c("BIGN", "AGE"),
    method = c("categorical", "continuous"), population_id = "SAF",
    by = c(NA, "TRT01A"), variables = c("TRT01A", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

setup_file <- function(s) file.path(s$path, "programs", "study_setup.R")
company_file <- function(s) file.path(s$path, "programs", "company_setup.R")
user_file <- function(s) file.path(s$path, "programs", "study_user.R")
bytes <- function(f) readBin(f, "raw", file.size(f))
setup_bytes <- function(s) {
  f <- setup_file(s)
  readBin(f, "raw", file.size(f))
}
# the text of one part (1 standard, 2 study, 3 user), its marker included
setup_part <- function(s, k) {
  p <- .split_study_setup(.read_bytes(setup_file(s)))
  p[[k]]
}
# The study's data, where the setup reads it (with tflspec#794 its part 2
# loads the catalog's datasets as it runs): a small file for each
# readRDS(file.path(path_<folder>, "<name>.rds")) line, in its folder
give_data <- function(s) {
  txt <- readLines(setup_file(s), warn = FALSE)
  m <- regmatches(txt, regexec("readRDS[(]file.path[(]path_([a-z_]+), \"([^\"]+)\"[)][)]",
                               txt))
  for (x in Filter(length, m)) {
    d <- file.path(s$path, study_layout()[[x[2L]]])
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    saveRDS(data.frame(USUBJID = "01", TRT01A = "A"), file.path(d, x[3L]))
  }
  invisible(s)
}
add_to_part3 <- function(s, lines) {
  cat(paste0(lines, "\n", collapse = ""), file = user_file(s), append = TRUE)
}
# A study_setup.R in the form before #910, its three parts as given: part 2
# a tflplanner part with its checksum (of `code`), `edit` a line put in
# after the checksum was taken
old_setup <- function(s, part1, code, part3, edit = NULL, extra = NULL) {
  p2 <- c(.setup_marker_study, paste("#  Checksum   :", .body_hash(code)), code, edit)
  txt <- paste0(paste(c(.setup_marker_standard(as.Date("2026-10-08")), part1, p2,
                        .setup_marker_user, part3, extra), collapse = "\n"), "\n")
  unlink(c(company_file(s), user_file(s)))
  writeBin(charToRaw(enc2utf8(txt)), setup_file(s))
  txt
}

test_that("a new study gets its three setup files; study_setup.R is generated", {
  local_home()
  s <- create_study("ABC-101", title = "A phase 2 study", phase = "2")
  f <- setup_file(s)
  expect_true(all(file.exists(c(company_file(s), f, user_file(s)))))
  # the company's: the standard's library() lines, under a line saying when
  co <- readLines(company_file(s))
  expect_match(co[1L], "^# The company's code [(]programs/company_setup.R[)]")
  expect_match(co[2L], paste0("0\\.1.* on ", format(Sys.Date())))
  expect_true(all(c("library(cards)", "library(dplyr)", "library(rtfreporter)") %in% co))
  # the user's: comments only
  expect_true(all(grepl("^#|^$", readLines(user_file(s)))))
  # the generated one: a banner, its checksum, the layout and the study
  txt <- readLines(f)
  expect_false(any(grepl("^# ==== ", txt)))
  expect_true(any(grepl("^#  Generated  : tflplanner ", txt)))
  expect_true(any(grepl("^#  Checksum   : [0-9a-f]{32}$", txt)))
  expect_false("library(tflspec)" %in% c(co, txt))
  lay <- study_layout()
  for (k in names(lay)) {
    expect_true(any(grepl(sprintf("^path_%s +<- \"%s\"$", k, lay[[k]]), txt)), label = k)
  }
  expect_true(any(grepl("^study_id +<- \"ABC-101\"$", txt)))
  expect_true(any(grepl("^study_title +<- \"A phase 2 study\"$", txt)))
  expect_true(any(grepl("^study_phase +<- \"2\"$", txt)))
  expect_true(any(grepl("^study_compound +<- NA_character_$", txt)))
  # the company's runs before the study's lines, the user's after them
  co_at <- grep("source(.tfl_free[[\"company\"]]", txt, fixed = TRUE)
  us_at <- grep("source(.tfl_free[[\"user\"]]", txt, fixed = TRUE)
  id_at <- grep("^study_id ", txt)
  expect_true(co_at < id_at && id_at < us_at)
  # it runs, and defines them
  give_data(s)
  e <- new.env()
  withr::with_dir(s$path, sys.source(f, envir = e, toplevel.env = e))
  expect_identical(e$path_adam, "data/adam")
  expect_identical(e$study_id, "ABC-101")
  expect_identical(e$study_title, "A phase 2 study")
  expect_true(is.na(e$study_compound))
  # saved again unchanged: not written; the free files never are
  s2 <- save_study(s)
  expect_identical(s2$files$status[s2$files$file == f], "unchanged")
  expect_false(any(s2$files$file %in% c(company_file(s), user_file(s))))
})

test_that("the company's code and the user's run once per program, again when changed", {
  local_home()
  s <- create_study("ABC-110")
  f <- setup_file(s)
  count <- function(v) sprintf("%s <- if (exists(\"%s\", inherits = FALSE)) %s + 1 else 1", v, v, v)
  # the company's sees nothing of the study yet, and its path_adam is
  # overwritten; the user's study_title wins
  writeLines(c(count("n_company"), "saw_id <- exists(\"study_id\", inherits = FALSE)",
               "path_adam <- \"mine\""), company_file(s))
  writeLines(c(count("n_user"), "study_title <- \"mine\""), user_file(s))
  give_data(s)
  e <- new.env()
  src <- function() withr::with_dir(s$path, sys.source(f, envir = e, toplevel.env = e))
  src()
  expect_identical(c(e$n_company, e$n_user), c(1, 1))
  expect_false(e$saw_id)
  expect_identical(e$path_adam, "data/adam")
  expect_identical(e$study_title, "mine")
  # sourced again in the same program (a figure report): not again
  src()
  expect_identical(c(e$n_company, e$n_user), c(1, 1))
  # a file changed: both run again
  cat("# changed\n", file = user_file(s), append = TRUE)
  src()
  expect_identical(c(e$n_company, e$n_user), c(2, 2))
  # by hand
  rm(".tfl_free_ran", envir = e)
  src()
  expect_identical(c(e$n_company, e$n_user), c(3, 3))
  # another environment (another program's): they run there
  e2 <- new.env()
  withr::with_dir(s$path, sys.source(f, envir = e2, toplevel.env = e2))
  expect_identical(c(e2$n_company, e2$n_user), c(1, 1))
})

test_that("saving writes study_setup.R again and never the company's or the user's file", {
  local_home()
  s <- create_study("ABC-102", title = "First")
  f <- setup_file(s)
  # the user's own lines, with odd spacing, a non-ASCII character and a CRLF line
  own <- paste0("options(digits = 4)\n  # indented  \nmy_label <- \"caf\u00e9\"\n",
                "my_fun <- function(x) {\r\n  x + 1\n}\n\n  \n")
  writeBin(charToRaw(enc2utf8(paste0("library(cards)\n", own))), company_file(s))
  writeBin(charToRaw(enc2utf8(own)), user_file(s))
  co <- bytes(company_file(s))
  us <- bytes(user_file(s))
  s$meta$title <- "Second"
  s$meta$compound <- "XY-1"
  s <- save_study(s)
  expect_identical(s$files$status[s$files$file == f], "written")
  expect_identical(bytes(company_file(s)), co)
  expect_identical(bytes(user_file(s)), us)
  txt <- readLines(f)
  expect_true("study_title       <- \"Second\"" %in% txt)
  expect_true("study_compound    <- \"XY-1\"" %in% txt)
  expect_false(dir.exists(file.path(s$path, "programs", ".edited")))
  give_data(s)
  e <- new.env()
  withr::with_dir(s$path, sys.source(f, envir = e, toplevel.env = e))
  expect_identical(e$my_fun(1), 2)
  expect_identical(e$study_title, "Second")
})

test_that("a hand edit of study_setup.R is backed up to programs/.edited/ and undone", {
  local_home()
  s <- create_study("ABC-103")
  f <- setup_file(s)
  txt <- readLines(f)
  txt <- sub("^path_adam( +)<- .*$", "path_adam\\1<- \"elsewhere\"", txt)
  writeLines(txt, f)
  edited <- readLines(f)
  s <- save_study(s)
  expect_identical(s$files$status[s$files$file == f], "rewritten")
  bak <- list.files(file.path(s$path, "programs", ".edited"),
                    "^study_setup_.*[.]R$", full.names = TRUE)
  expect_length(bak, 1L)
  expect_identical(readLines(bak), edited)
  expect_true(any(grepl("^path_adam +<- \"data/adam\"$", readLines(f))))
})

test_that("an old study_setup.R is split byte for byte, once", {
  local_home()
  s <- create_study("ABC-111")
  f <- setup_file(s)
  part1 <- c("library(cards)", "options(digits = 4)\r", "# caf\u00e9")
  part3 <- c("my_fun <- function(x) x + 1", "", "  ")
  txt <- old_setup(s, part1, c("# the study, then", "x_old <- 1"), part3)
  lines <- strsplit(txt, "(?<=\n)", perl = TRUE)[[1L]]
  m2 <- grep(.setup_marker[["study"]], lines)
  m3 <- grep(.setup_marker[["user"]], lines)
  want1 <- paste(lines[seq_len(m2 - 1L)], collapse = "")
  want3 <- paste(lines[seq(m3, length(lines))], collapse = "")
  s <- save_study(s)
  expect_identical(s$files$status[match(c(company_file(s), user_file(s), f), s$files$file)],
                   rep("split", 3L))
  expect_null(s$setup_note)
  expect_identical(bytes(company_file(s)), charToRaw(enc2utf8(want1)))
  expect_identical(bytes(user_file(s)), charToRaw(enc2utf8(want3)))
  now <- readLines(f)
  expect_false(any(grepl("^# ==== ", now)))
  expect_true(any(grepl("^#  Generated  :", now)))
  # the old one kept
  expect_length(list.files(file.path(s$path, "programs", ".edited"), "^study_setup_"), 1L)
  # it runs: the user's function there
  give_data(s)
  e <- new.env()
  withr::with_dir(s$path, sys.source(f, envir = e, toplevel.env = e))
  expect_identical(e$my_fun(1), 2)
  # saved again: nothing split, nothing written
  s <- save_study(s)
  expect_false(any(s$files$status == "split"))
  expect_identical(s$files$status[s$files$file == f], "unchanged")
})

test_that("an old study_setup.R that cannot be split is kept as it was, and says why", {
  local_home()
  s <- create_study("ABC-112")
  f <- setup_file(s)
  code <- c("# the study", "x_old <- 1")
  kept <- function(txt, why) {
    s2 <- save_study(s)
    expect_identical(s2$files$status[s2$files$file == f], "kept")
    expect_identical(s2$setup_note, why)
    expect_identical(bytes(f), charToRaw(enc2utf8(txt)))
    s2
  }
  # its tflplanner part edited by hand
  txt <- old_setup(s, "library(cards)", code, "mine <- 1", edit = "x_old <- 2")
  kept(txt, "its tflplanner part was edited by hand")
  expect_false(any(file.exists(c(company_file(s), user_file(s)))))
  expect_true(length(list.files(file.path(s$path, "programs", ".edited"), "^study_setup_")) >= 1L)
  # a marker line repeated
  txt <- old_setup(s, "library(cards)", code, "mine <- 1", extra = .setup_marker_user)
  kept(txt, "its marker lines are missing, repeated or out of order")
  # study_user.R there already, with other content
  txt <- old_setup(s, "library(cards)", code, "mine <- 1")
  writeLines("theirs <- 1", user_file(s))
  kept(txt, "company_setup.R or study_user.R is there already, with other content")
  expect_identical(readLines(user_file(s)), "theirs <- 1")
  # the old file is still read as the study's setup
  expect_true("cards" %in% .study_setup_packages(s$path))
})

test_that("each setup file sources study_setup.R first; programs no library()", {
  local_home()
  s <- create_study("ABC-104", planner = setup_planner())
  src <- "source(\"programs/study_setup.R\")"
  first_code <- function(f) {
    x <- readLines(f)
    x[!grepl("^\\s*(#|$)", x)][1L]
  }
  for (f in c("programs/ard/ard_setup.R", "programs/tfl/report_setup.R",
              "programs/tfl/fig_setup.R")) {
    expect_identical(first_code(file.path(s$path, f)), src, label = f)
  }
  expect_identical(.source_study_setup(), src)
  # a report program: no library() of its own, the study.yml check, then
  # report_setup.R (always written, always sourced)
  prog <- readLines(file.path(s$path, "programs", "tfl", "DM.R"))
  expect_false(any(grepl("^library\\(", prog)))
  chk <- grep("file.exists(\"study.yml\")", prog, fixed = TRUE)
  rs <- grep("source(\"programs/tfl/report_setup.R\")", prog, fixed = TRUE)
  expect_length(chk, 1L)
  expect_length(rs, 1L)
  expect_true(chk < rs)
  # a study with no tokens or font of its own sources it too
  y <- add_output(new_planner(), "T-1", type = "table")
  expect_match(paste(program_code(y, "T-1"), collapse = "\n"),
               "source(\"programs/tfl/report_setup.R\")", fixed = TRUE)
  expect_match(paste(report_setup_code(y), collapse = "\n"), src, fixed = TRUE)
  # the official run keeps the setup with its code
  expect_match(paste(batch_code(s$planner), collapse = "\n"),
               "\"programs/study_setup.R\"", fixed = TRUE)
})

test_that("setup_code: read as written, indentation kept, checked", {
  b <- .builtin_standards()
  expect_true("setup_code" %in% .standard_sheets())
  expect_true("setup_code" %in% .standards_readme()$sheet)
  f <- tempfile(fileext = ".xlsx")
  standards_template(f)
  expect_true("setup_code" %in% readxl::excel_sheets(f))
  expect_identical(read_standards(f)$setup_code, b$setup_code)

  write_std <- function(code, note = NULL) {
    s <- b
    s$setup_code <- data.frame(code = code, note = note %||% NA_character_,
                               stringsAsFactors = FALSE)
    g <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(s, g)
    g
  }
  code <- c("library(cards)", "fmt2 <- function(x) {",
            "    formatC(x, digits = 2, format = \"f\")", "}", NA,
            "study_label <- \"{STUDY_ID}\"  ")
  r <- read_standards(write_std(code))$setup_code
  expect_identical(r$code, c(code[1:4], "", code[6]))
  expect_identical(setup_code("ABC-1", list(setup_code = r))[6],
                   "study_label <- \"ABC-1\"  ")
  # a workbook without the sheet keeps the built-in one
  s <- b
  s$setup_code <- NULL
  g <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(s, g)
  expect_identical(read_standards(g)$setup_code, b$setup_code)

  # not R: refused when the standards are set up
  local_home()
  expect_error(setup_tflplanner(standards = write_std(c("x <- function(", "y"))),
               "setup_code` is not R code")
  q <- "library(\u201cdplyr\u201d)"
  expect_error(setup_tflplanner(standards = write_std(c("library(cards)", q))),
               "curly quotes.*line\\(s\\) 2 \\(Excel row\\(s\\) 3\\)")
  expect_error(.check_setup_code("x <- \u2018a\u2019"), "straight quotes")
  # installed, a new study's company_setup.R is a copy of them
  suppressMessages(setup_tflplanner(standards = write_std(code)))
  st <- create_study("ABC-105")
  txt <- readLines(company_file(st))
  expect_true("    formatC(x, digits = 2, format = \"f\")" %in% txt)
  expect_true("study_label <- \"ABC-105\"  " %in% txt)
  # the standards changing later: a finished study's company_setup.R stays
  suppressMessages(setup_tflplanner(standards = write_std("library(dplyr)")))
  st <- save_study(st)
  expect_identical(readLines(company_file(st)), txt)
})

test_that("an existing study gets its setup files on its first save", {
  local_home()
  s <- create_study("ABC-106", planner = setup_planner())
  f <- setup_file(s)
  # as an earlier tflplanner left it: no setup files, report programs
  # with their own library() lines, ARD status without the setup
  unlink(c(company_file(s), f, user_file(s)))
  dm <- file.path(s$path, "programs", "tfl", "DM.R")
  old <- readLines(dm)
  writeLines(c(old[1:2], "library(rtfreporter)", "library(tflspec)", old[-(1:2)]),
             dm)
  st <- data.frame(output_id = "DM", definition = tflspec::tfl_ard_spec_hash(
    structure(s$planner$ard, class = "tfl_ard_spec"), "DM", dir = s$path,
    codelists = .study_codelists(s$planner)),
    built = "2026-10-01 10:00:00", rows = "10", error = "",
    stringsAsFactors = FALSE)
  .write_ard_status(s, st)
  expect_equal(ard_status(s)$state, "built")
  s <- save_study(s)
  expect_identical(s$files$status[match(c(company_file(s), f, user_file(s)), s$files$file)],
                   rep("written", 3L))
  expect_true("library(rtfreporter)" %in% readLines(company_file(s)))
  # an output built before the setup was recorded is not outdated by it
  expect_equal(ard_status(s)$state, "built")
  # the program edited by hand was backed up and written again
  expect_false(any(grepl("^library\\(", readLines(dm))))
})

test_that("a change to study_setup.R marks the ARD and the reports outdated", {
  local_home()
  s <- create_study("ABC-107", planner = setup_planner())
  # as the programs record a build: the setup's fingerprint
  h <- .study_setup_hash(s$path)
  expect_match(h, "^([0-9a-f]{32}[+]){2}[0-9a-f]{32}$")
  withr::with_dir(s$path, expect_identical(eval(parse(text = .setup_hash_code())), h))
  def <- tflspec::tfl_ard_spec_hash(structure(s$planner$ard, class = "tfl_ard_spec"),
                                    "DM", dir = s$path,
                                    codelists = .study_codelists(s$planner))
  .write_ard_status(s, data.frame(output_id = "DM", definition = def,
                                  built = "2026-10-08 10:00:00", rows = 10L,
                                  error = "", setup = h,
                                  stringsAsFactors = FALSE))
  # the report: its RTF, and the record its program leaves
  info <- report_info(s$planner, "DM")
  rtf <- file.path(s$path, info$file)
  dir.create(dirname(rtf), recursive = TRUE, showWarnings = FALSE)
  writeLines("{\\rtf1}", rtf)
  rec <- report_setup_code(s$planner)
  rec <- rec[seq(grep("^record_report <- function", rec),
                 length(rec) - 1L)]
  withr::with_dir(s$path, {
    eval(parse(text = rec))
    record_report("DM")
  })
  # (a report records report_setup.R too)
  expect_identical(.report_setup_recorded(s$path, "DM"), .report_setup_hash(s$path))
  expect_match(.report_setup_hash(s$path), "^([0-9a-f]{32}[+]){2}[0-9a-f]{32}[|]([0-9a-f]{32})?$")
  expect_equal(ard_status(s)$state, "built")
  expect_equal(study_status(s)$status, "ok")
  expect_equal(.report_run_light(s)$status, "ok")
  # study_user.R changed by the user: both outdated
  add_to_part3(s, "options(digits = 3)")
  expect_equal(ard_status(s)$state, "outdated")
  expect_equal(.report_run_light(s)$status, "outdated")
  expect_equal(study_status(s)$status, "outdated")
})

test_that("a sample built by its programs records the setup, and goes stale with it", {
  skip_on_cran()
  local_home()
  s <- create_study("ABC-108", planner = setup_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  u <- update_study_ard(s, "DM")
  expect_true(u$ok)
  st <- .read_ard_status(s)
  # (the ARD program records the three files: company, setup, user)
  expect_identical(.ard_recorded_setup(st), .study_setup_hash(s$path))
  expect_equal(ard_status(s)$state, "built")
  add_to_part3(s, "# a comment of mine")
  expect_equal(ard_status(s)$state, "outdated")
  u <- update_study_ard(s, "DM")
  expect_equal(ard_status(s)$state, "built")
})

test_that("the parts are found with lines before part 1 or a marker repeated", {
  m <- c(.setup_marker_standard(as.Date("2026-10-08")), .setup_marker_study,
         .setup_marker_user)
  txt <- paste0(paste(c("# my preamble", m[1], "library(cards)", m[2], "x <- 1",
                        m[3], "y <- 2", m[2]), collapse = "\n"), "\n")
  p <- .split_study_setup(txt)
  expect_identical(p$standard, paste0("# my preamble\n", m[1], "\nlibrary(cards)\n"))
  expect_identical(p$study, paste0(m[2], "\nx <- 1\n"))
  expect_identical(p$user, paste0(m[3], "\ny <- 2\n", m[2], "\n"))
  expect_identical(paste0(p$standard, p$study, p$user), txt)
  expect_null(.split_study_setup(paste0(m[1], "\n", m[3], "\n")))
  expect_null(.split_study_setup(""))
})

test_that("a study gets programs/study_helpers.R, which its setup sources", {
  local_home()
  s <- create_study("ABC-107", planner = setup_planner())
  f <- file.path(s$path, "programs", "study_helpers.R")
  expect_true(file.exists(f))
  txt <- readLines(f)
  # tflspec's functions, after the banner
  expect_true(all(tflspec::tfl_helpers_code() %in% txt))
  expect_match(txt[2L], "programs/study_helpers.R", fixed = TRUE)
  expect_true("source(\"programs/study_helpers.R\")" %in% readLines(setup_file(s)))
  # the setup defines them, from the study folder (source(): where the
  # programs run, the global environment)
  fns <- c("set_levels", "tag_ard", "fmt_ard", "fmt_pvalue", "keep_stats",
           "save_ard")
  withr::defer(suppressWarnings(rm(list = fns, envir = globalenv())))
  give_data(s)
  e <- new.env()
  withr::with_dir(s$path, sys.source(setup_file(s), envir = e, toplevel.env = e))
  for (fn in fns) {
    expect_true(is.function(get0(fn, globalenv(), inherits = FALSE)), label = fn)
  }
  # saved again: unchanged
  s2 <- save_study(s)
  expect_identical(s2$files$status[s2$files$file == f], "unchanged")
})

test_that("a change to report_setup.R (the rounding) marks the reports outdated, not the ARD", {
  local_home()
  s <- create_study("ABC-109", planner = setup_planner())
  def <- tflspec::tfl_ard_spec_hash(structure(s$planner$ard, class = "tfl_ard_spec"),
                                    "DM", dir = s$path,
                                    codelists = .study_codelists(s$planner))
  .write_ard_status(s, data.frame(output_id = "DM", definition = def,
                                  built = "2026-10-08 10:00:00", rows = 10L,
                                  error = "", setup = .study_setup_hash(s$path),
                                  stringsAsFactors = FALSE))
  # report_setup.R, then the RTF (a program newer than its RTF is outdated
  # anyway: report_setup.R keeps its time below)
  rs <- file.path(s$path, .report_setup_path())
  dir.create(dirname(rs), recursive = TRUE, showWarnings = FALSE)
  writeLines("options(rtfreporter.rounding = \"sas\")", rs)
  then <- Sys.time() - 3600
  Sys.setFileTime(rs, then)
  info <- report_info(s$planner, "DM")
  rtf <- file.path(s$path, info$file)
  dir.create(dirname(rtf), recursive = TRUE, showWarnings = FALSE)
  writeLines("{\\rtf1}", rtf)
  rec <- report_setup_code(s$planner)
  rec <- rec[seq(grep("^record_report <- function", rec), length(rec) - 1L)]
  withr::with_dir(s$path, {
    eval(parse(text = rec))
    record_report("DM")
  })
  expect_identical(.report_setup_recorded(s$path, "DM"), .report_setup_hash(s$path))
  expect_equal(study_status(s)$status, "ok")
  # the study's rounding changed: report_setup.R written again
  writeLines("options(rtfreporter.rounding = \"r\")", rs)
  Sys.setFileTime(rs, then)
  expect_equal(study_status(s)$status, "outdated")
  expect_match(study_status(s)$why, "setup", fixed = TRUE)
  expect_equal(.report_run_light(s)$status, "outdated")
  expect_equal(ard_status(s)$state, "built")
  # a record of study_setup.R alone (made before): held to it alone
  expect_false(.report_setup_changed(.study_setup_hash(s$path), s$path))
  expect_true(.report_setup_changed(paste0(.study_setup_hash(s$path), "|x"), s$path))
})

test_that("the study's rounding is set once, in study_setup.R (tflspec#791)", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(sample_study_copy())
  s$planner <- .spec_set_rounding(s$planner, "sas")
  s <- suppressMessages(save_study(s))
  part2 <- readLines(file.path(s$path, .study_setup_path()), encoding = "UTF-8")
  rnd <- "options(rtfreporter.rounding = \"sas\", cards.round_type = \"round-half-up\")"
  expect_true(rnd %in% trimws(part2))
  # the input data (tflspec#794), where tflspec writes it: after the rounding,
  # through the folder variables study_setup.R defines, not the catalog's paths
  if (any(grepl("readRDS", tflspec::tfl_study_setup_code(s$planner$spec$study)))) {
    # (an object of its own, or study$adam_org's, read when first used: tflspec#814)
    expect_true(any(grepl("readRDS(file.path(path_adam, \"adsl.rds\"))", part2, fixed = TRUE)))
    expect_gt(grep("readRDS", part2)[1L], match(rnd, trimws(part2)))
    expect_false(any(grepl("readRDS(\"data/", part2, fixed = TRUE)))
    # and it runs from the study folder, the sample's data there
    # (in the global environment, as a program sources it: the helpers'
    # adam_codes() reads `study` there; what it made is removed again)
    before <- ls(globalenv(), all.names = TRUE)
    withr::defer(rm(list = setdiff(ls(globalenv(), all.names = TRUE), before), envir = globalenv()))
    withr::with_dir(s$path, {
      sys.source(file.path(s$path, .study_setup_path()), envir = globalenv(), toplevel.env = globalenv())
      expect_true(is.data.frame(get0("adsl", globalenv())) || is.data.frame(study$adam$adsl))
    })
  }
  # the ARD and report setups source it and do not set the rounding again
  ard <- readLines(file.path(s$path, study_layout()[["programs_ard"]], .ard_setup_file))
  expect_false(any(grepl("cards.round_type", ard, fixed = TRUE)))
  rep <- file.path(s$path, .report_setup_path())
  if (file.exists(rep)) {
    expect_false(any(grepl("rtfreporter.rounding", readLines(rep), fixed = TRUE)))
  }
  # the program as study_setup_code() gives it: with the study's spec, or none
  expect_true(rnd %in% trimws(study_setup_code(s$meta, study = s$planner$spec$study)))
  expect_false(any(grepl("rounding", study_setup_code(s$meta), fixed = TRUE)))
  # the study's rounding changed: study_setup.R says the new one, once
  s$planner <- .spec_set_rounding(s$planner, "r")
  s <- suppressMessages(save_study(s))
  setup <- readLines(file.path(s$path, .study_setup_path()), encoding = "UTF-8")
  expect_identical(sum(grepl("rtfreporter.rounding = \"r\"", setup, fixed = TRUE)), 1L)
  expect_false(any(grepl("rtfreporter.rounding = \"sas\"", setup, fixed = TRUE)))
})

test_that("the setup reads the datasets a report uses, not every dataset of the catalog", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(sample_study_copy())
  p <- s$planner
  # a dataset of the catalog no report uses (its file is not on disk)
  p$spec$study$data$datasets <- c(p$spec$study$data$datasets,
                                  list(list(dataset = "ADZZ", path = "data/adam/adzz.rds")))
  # the datasets read: `adsl <- readRDS(...)`, or (tflspec#814)
  # `delayedAssign("adsl", readRDS(...), assign.env = study$adam_org)`,
  # over one line or several: the parsed code
  read <- function(x) {
    reads <- function(e) any(grepl("readRDS(", deparse(e), fixed = TRUE))
    unlist(lapply(as.list(parse(text = x)), function(e) {
      if (!is.call(e)) return(NULL)
      f <- as.character(e[[1L]])
      if (identical(f, "<-") && reads(e[[3L]])) return(as.character(e[[2L]]))
      if (identical(f, "delayedAssign") && reads(e[[3L]])) return(as.character(e[[2L]]))
      NULL
    }))
  }
  all <- read(tflspec::tfl_study_setup_code(p$spec$study))
  now <- read(tflspec::tfl_study_setup_code(.study_rounding_source(p)))
  expect_true("adzz" %in% all)
  expect_false("adzz" %in% now)
  # the others as the catalog has them, ADSL first
  used <- tolower(.reports_datasets(p))
  expect_identical(now, all[all %in% used])
  expect_identical(now[1L], "adsl")
})
