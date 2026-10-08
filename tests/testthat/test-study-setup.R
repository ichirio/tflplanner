# programs/study_setup.R (#268): one study-wide setup in three parts --
# the company standard (copied), tflplanner's (rewritten on save), the
# user's (never touched) -- sourced by ard_setup.R, report_setup.R and
# fig_setup.R.

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
setup_bytes <- function(s) {
  f <- setup_file(s)
  readBin(f, "raw", file.size(f))
}
# the text of one part (1 standard, 2 study, 3 user), its marker included
setup_part <- function(s, k) {
  p <- .split_study_setup(.read_bytes(setup_file(s)))
  p[[k]]
}
add_to_part3 <- function(s, lines) {
  cat(paste0(lines, "\n", collapse = ""), file = setup_file(s), append = TRUE)
}

test_that("a new study gets programs/study_setup.R with its three parts", {
  local_home()
  s <- create_study("ABC-101", title = "A phase 2 study", phase = "2")
  f <- setup_file(s)
  expect_true(file.exists(f))
  txt <- readLines(f)
  m <- grep("^# ==== ", txt)
  expect_length(m, 3L)
  expect_match(txt[m[1]], paste0("^# ==== tflplanner: company standard \\(copied ",
                                 format(Sys.Date()), ", standards .*0\\.1"))
  expect_match(txt[m[2]], "tflplanner: study", fixed = TRUE)
  expect_match(txt[m[3]], "your study: edit freely below", fixed = TRUE)
  expect_identical(m[1], 1L)
  expect_identical(m[3], length(txt))         # part 3 empty
  # part 1: the standard's library() lines
  expect_true(all(c("library(cards)", "library(rtfreporter)", "library(tflspec)")
                  %in% txt[seq(m[1], m[2])]))
  # part 2: the layout and the study, as variables
  p2 <- txt[seq(m[2], m[3])]
  expect_true(any(grepl("^#  Checksum   : [0-9a-f]{32}$", p2)))
  lay <- study_layout()
  for (k in names(lay)) {
    expect_true(any(grepl(sprintf("^path_%s +<- \"%s\"$", k, lay[[k]]), p2)),
                label = k)
  }
  expect_true(any(grepl("^study_id +<- \"ABC-101\"$", p2)))
  expect_true(any(grepl("^study_title +<- \"A phase 2 study\"$", p2)))
  expect_true(any(grepl("^study_phase +<- \"2\"$", p2)))
  expect_true(any(grepl("^study_compound +<- NA_character_$", p2)))
  # it runs, and defines them
  e <- new.env()
  sys.source(f, envir = e, toplevel.env = e)
  expect_identical(e$path_adam, "data/adam")
  expect_identical(e$study_id, "ABC-101")
  expect_identical(e$study_title, "A phase 2 study")
  expect_true(is.na(e$study_compound))
  # saved again unchanged: not written
  s2 <- save_study(s)
  expect_identical(s2$files$status[s2$files$file == f], "unchanged")
})

test_that("saving rewrites part 2 only: parts 1 and 3 stay byte for byte", {
  local_home()
  s <- create_study("ABC-102", title = "First")
  f <- setup_file(s)
  # the user's own lines in parts 1 and 3, with odd spacing, a non-ASCII
  # character and a CRLF line
  lines <- readLines(f)
  m <- grep("^# ==== ", lines)
  own1 <- c("options(digits = 4)", "  # indented  ", "my_label <- \"caf\u00e9\"")
  txt <- paste0(paste(c(lines[1L], own1, lines[-1L]), collapse = "\n"), "\n")
  txt <- paste0(txt, "my_fun <- function(x) {\r\n  x + 1\n}\n\n  \n")
  writeBin(charToRaw(enc2utf8(txt)), f)
  p1 <- setup_part(s, "standard")
  p3 <- setup_part(s, "user")
  s$meta$title <- "Second"
  s$meta$compound <- "XY-1"
  s <- save_study(s)
  expect_identical(s$files$status[s$files$file == f], "written")
  expect_identical(charToRaw(setup_part(s, "standard")), charToRaw(p1))
  expect_identical(charToRaw(setup_part(s, "user")), charToRaw(p3))
  p2 <- setup_part(s, "study")
  expect_match(p2, "study_title       <- \"Second\"", fixed = TRUE)
  expect_match(p2, "study_compound    <- \"XY-1\"", fixed = TRUE)
  expect_false(dir.exists(file.path(s$path, "programs", ".edited")))
  # the file still runs: part 3 after part 2
  e <- new.env()
  sys.source(f, envir = e, toplevel.env = e)
  expect_identical(e$my_fun(1), 2)
  expect_identical(e$study_title, "Second")
})

test_that("a hand edit of part 2 is backed up to programs/.edited/ and undone", {
  local_home()
  s <- create_study("ABC-103")
  f <- setup_file(s)
  add_to_part3(s, "mine <- 1")
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
  now <- readLines(f)
  expect_true(any(grepl("^path_adam +<- \"data/adam\"$", now)))
  expect_identical(utils::tail(now, 1L), "mine <- 1")
  # a file whose markers are gone: kept in .edited/, made again
  writeLines("library(dplyr)", f)
  Sys.sleep(1.1)   # a backup of its own (the time in its name)
  s <- save_study(s)
  expect_identical(s$files$status[s$files$file == f], "rewritten")
  expect_length(list.files(file.path(s$path, "programs", ".edited"),
                           "^study_setup_"), 2L)
  expect_length(grep("^# ==== ", readLines(f)), 3L)
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
  # installed, a new study's part 1 is a copy of them
  suppressMessages(setup_tflplanner(standards = write_std(code)))
  st <- create_study("ABC-105")
  txt <- readLines(setup_file(st))
  expect_true("    formatC(x, digits = 2, format = \"f\")" %in% txt)
  expect_true("study_label <- \"ABC-105\"  " %in% txt)
  # the standards changing later: a finished study's part 1 stays
  suppressMessages(setup_tflplanner(standards = write_std("library(dplyr)")))
  st <- save_study(st)
  expect_identical(readLines(setup_file(st)), txt)
})

test_that("an existing study gets study_setup.R on its first save", {
  local_home()
  s <- create_study("ABC-106", planner = setup_planner())
  f <- setup_file(s)
  # as an earlier tflplanner left it: no study_setup.R, report programs
  # with their own library() lines, ARD status without the setup
  unlink(f)
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
  expect_identical(s$files$status[s$files$file == f], "written")
  txt <- readLines(f)
  m <- grep("^# ==== ", txt)
  expect_length(m, 3L)
  expect_true("library(rtfreporter)" %in% txt[seq(m[1], m[2])])
  expect_identical(m[3], length(txt))
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
  expect_match(h, "^[0-9a-f]{32}$")
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
  expect_identical(.report_setup_recorded(s$path, "DM"), h)
  expect_equal(ard_status(s)$state, "built")
  expect_equal(study_status(s)$status, "ok")
  expect_equal(.report_run_light(s)$status, "ok")
  # part 3 changed by the user: both outdated
  add_to_part3(s, "options(digits = 3)")
  expect_equal(ard_status(s)$state, "outdated")
  expect_equal(.report_run_light(s)$status, "outdated")
  expect_equal(study_status(s)$status, "outdated")
})

test_that("a sample built by its programs records the setup, and goes stale with it", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("ABC-108", planner = setup_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  u <- update_study_ard(s, "DM")
  expect_true(u$ok)
  st <- .read_ard_status(s)
  expect_identical(st$setup, .study_setup_hash(s$path))
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
