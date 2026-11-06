# A page pattern's and the defaults' own lists (code lists, content) are kept
# when an edit of their page rows goes through the sheets, and a pattern
# with no page rows keeps its `@<name>` row of the report sheet.  The marker
# tests need the bridge of tflspec #669 (its sheets say a pattern with no
# page).

skip_on_cran()

lk_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# A study of two reports of one analysis of SEX: DM (the defaults) and DM2
# (extends the pattern pp);
# defaults.yml with a code list and two content items, patterns/pp.yml with
# a code list and (`page`) a page title; opened as the files say it
lk_study <- function(id, page = TRUE) {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(output_id = "DM", analysis_id = "SEX",
                               method = "categorical", population_id = "SAF",
                               variables = "SEX")
  for (n in names(p$ard)) p$ard[[n]] <- .normalize_ard_sheet(p$ard[[n]], n)
  s <- create_study(id, planner = copy_output(p, "DM", "DM2"))
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  sp <- file.path(s$path, "spec")
  writeLines(c("content:", "  - normalize_ard: {}", "  - table_plan: {}", "codelists:",
               "  SEX: [{value: F, label: DefF}, {value: M, label: DefM}]"),
             file.path(sp, "defaults.yml"))
  dir.create(file.path(sp, "patterns"), showWarnings = FALSE)
  writeLines(c("codelists:", "  SEX: [{value: F, label: PatF}, {value: M, label: PatM}]",
               if (page) c("page:", "  titles:", "    1: {center: Pattern title}")),
             file.path(sp, "patterns", "pp.yml"))
  f <- file.path(sp, "outputs", "DM2.yml")
  writeLines(sub("^  - defaults$", "  - patterns/pp", readLines(f)), f)
  open_study(id)
}

lk_file <- function(s, ...) readLines(file.path(s$path, "spec", ...), encoding = "UTF-8")

# the labels of SEX in the spec in memory: the pattern's (id "DM2", which
# extends pp) or the defaults' ("DM").  (.study_codelists() reads the sheet,
# the reports' own lists: the layers' come with tflspec::tfl_report_codelists().)
lk_label <- function(p, id) {
  st <- .spec_study(p)
  l <- if (identical(id, "DM2")) st$patterns$pp else st$defaults
  vapply(l$codelists$SEX, function(e) e$label, "")
}

# a title row of a layer's, edited through the sheet
lk_title <- function(p, id, text) {
  r <- sheet_rows(p, "titles", id)
  if (!nrow(r)) {
    r <- data.frame(output_id = id, line = "1", center = text, stringsAsFactors = FALSE)
  } else r$center <- text
  set_sheet_rows(p, "titles", id, r)
}

test_that("a pattern's title edited through the sheet keeps its code lists", {
  lk_home()
  s <- lk_study("LK1")
  expect_equal(lk_label(s$planner, "DM2"), c("PatF", "PatM"))
  s$planner <- lk_title(s$planner, "@pp", "Edited title")
  s2 <- save_study(s)
  x <- lk_file(s, "patterns", "pp.yml")
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("PatF", x, fixed = TRUE)))
  expect_true(any(grepl("Edited title", x, fixed = TRUE)))
  # (the spec in memory says what the file says)
  expect_equal(lk_label(s2$planner, "DM2"), c("PatF", "PatM"))
  expect_equal(s2$planner$spec$text[["spec/patterns/pp.yml"]], x)
  s3 <- open_study("LK1")
  expect_identical(s3$spec$status, "same")
  expect_equal(lk_label(s3$planner, "DM2"), c("PatF", "PatM"))
  expect_equal(sheet_rows(s3$planner, "titles", "@pp")$center, "Edited title")
})

test_that("the defaults' title edited through the sheet keeps their code lists and content", {
  lk_home()
  s <- lk_study("LK2")
  expect_equal(lk_label(s$planner, "DM"), c("DefF", "DefM"))
  s$planner <- lk_title(s$planner, NA_character_, "Default title")
  s2 <- save_study(s)
  x <- lk_file(s, "defaults.yml")
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("DefF", x, fixed = TRUE)))
  expect_true(any(grepl("table_plan", x, fixed = TRUE)))
  expect_true(any(grepl("Default title", x, fixed = TRUE)))
  expect_equal(lk_label(s2$planner, "DM"), c("DefF", "DefM"))
  expect_equal(s2$planner$spec$text[["spec/defaults.yml"]], x)
  s3 <- open_study("LK2")
  expect_identical(s3$spec$status, "same")
  expect_equal(lk_label(s3$planner, "DM"), c("DefF", "DefM"))
})

test_that("the page screen's writer keeps a pattern's lists", {
  lk_home()
  s <- lk_study("LK3")
  p <- .pgs_write(s$planner, "@pp", list(titles = list(`1` = list(center = "Own"))))
  s$planner <- p
  save_study(s)
  x <- lk_file(s, "patterns", "pp.yml")
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("Own", x, fixed = TRUE)))
  expect_equal(lk_label(p, "DM2"), c("PatF", "PatM"))
})

test_that("a pattern whose last page content is taken away keeps its marker row", {
  lk_home()
  s <- lk_study("LK4")
  s$planner <- .pgs_write(s$planner, "@pp", list())
  d <- s$planner$sheets$report
  expect_true("@pp" %in% d$output_id)
  expect_identical(d$pattern[d$output_id == "DM2"], "@pp")
  x <- s$planner$spec$text[["spec/patterns/pp.yml"]]
  expect_true(any(grepl("^codelists:", x)))
  expect_false(any(grepl("^page:", x)))
  save_study(s)
  expect_false(any(grepl("^page:", lk_file(s, "patterns", "pp.yml"))))
  s2 <- open_study("LK4")
  expect_identical(s2$spec$status, "same")
  expect_true("@pp" %in% s2$planner$sheets$report$output_id)
  # another edit, saved: the sheets still work
  s2$planner <- .pgs_write(s2$planner, "DM", list(titles = list(`1` = list(center = "Own"))))
  s3 <- save_study(s2)
  expect_true("@pp" %in% s3$planner$sheets$report$output_id)
  sh <- .planner_sheets(s3$planner, s3$meta)
  expect_s3_class(tflspec::tfl_table_spec(sh$table), "tfl_table_spec")
  expect_false("@pp" %in% output_ids(s3$planner))
  expect_equal(lk_label(s3$planner, "DM2"), c("PatF", "PatM"))
  # and the pattern can have a page again
  s3$planner <- .pgs_write(s3$planner, "@pp", list(titles = list(`1` = list(center = "Back"))))
  s4 <- save_study(s3)
  x <- lk_file(s4, "patterns", "pp.yml")
  expect_true(any(grepl("Back", x, fixed = TRUE)))
  expect_true(any(grepl("^codelists:", x)))
  s5 <- open_study("LK4")
  expect_identical(s5$spec$status, "same")
  expect_equal(sheet_rows(s5$planner, "titles", "@pp")$center, "Back")
})

test_that("a study saved before the marker rows opens with the pattern's row", {
  home <- lk_home()
  s <- lk_study("LK5", page = FALSE)
  expect_true("@pp" %in% s$planner$sheets$report$output_id)
  # (the state as an earlier version wrote it: no row for the pattern)
  d <- s$planner$sheets$report
  s$planner$sheets$report <- d[!d$output_id %in% "@pp", , drop = FALSE]
  s$planner <- .spec_rebase(s$planner, s$meta)
  .write_state(s, home)
  s2 <- open_study("LK5")
  expect_true("@pp" %in% s2$planner$sheets$report$output_id)
  sh <- .planner_sheets(s2$planner, s2$meta)
  expect_s3_class(tflspec::tfl_table_spec(sh$table), "tfl_table_spec")
  expect_false("@pp" %in% output_ids(s2$planner))
  s2$planner <- lk_title(s2$planner, "@pp", "Now a page")
  s3 <- save_study(s2)
  expect_true(any(grepl("^codelists:", lk_file(s3, "patterns", "pp.yml"))))
})

test_that("a pattern renamed with rename_page_pattern() keeps its code lists", {
  lk_home()
  s <- lk_study("LK6")
  s$planner <- rename_page_pattern(s$planner, "pp", "qq")
  s2 <- save_study(s)
  expect_false(file.exists(file.path(s$path, "spec", "patterns", "pp.yml")))
  x <- lk_file(s, "patterns", "qq.yml")
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("PatF", x, fixed = TRUE)))
  expect_true(any(grepl("Pattern title", x, fixed = TRUE)))
  expect_identical(names(s2$planner$spec$study$patterns), "qq")
  expect_true(any(grepl("patterns/qq", lk_file(s, "outputs", "DM2.yml"), fixed = TRUE)))
  s3 <- open_study("LK6")
  expect_identical(s3$spec$status, "same")
})
