# The study's own lists (code lists, content) are kept when an edit of a page
# configuration's rows goes through the sheets: a page configuration is a
# style set of defaults.yml `styles:` (title lines live there, not in a
# pattern), and the code lists live in defaults.yml and in a legacy pattern
# that holds them only.  A configuration that differs from the default by no
# row keeps its `@<name>` row of the report sheet, as it is a known name.

skip_on_cran()

lk_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# A study of two reports of one analysis of SEX: DM (the defaults) and DM2
# (extends the legacy pattern pp, which holds a code list only, and uses the
# page configuration Wide); defaults.yml has a code list, two content items and
# the configurations Std (the study's default) and Wide (a title of its own;
# with `wide = FALSE` the same page as Std); opened as the files say it
lk_study <- function(id, wide = TRUE, pattern = TRUE, style = TRUE) {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = 'SAFFL == "Y"')
  p$ard$analyses <- data.frame(output_id = "DM", analysis_id = "SEX",
                               method = "categorical", population_id = "SAF",
                               variables = "SEX")
  for (n in names(p$ard)) p$ard[[n]] <- .normalize_ard_sheet(p$ard[[n]], n)
  s <- create_study(id, planner = copy_output(p, "DM", "DM2"))
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  sp <- file.path(s$path, "spec")
  writeLines(c("content:", "  - normalize_ard: {}", "  - table_plan: {}", "codelists:",
               "  SEX: [{value: F, label: DefF}, {value: M, label: DefM}]",
               "styles:",
               "  Std:", "    page:", "      titles:", "        first:",
               "          '1': {center: Std title}",
               "  Wide:", "    page:", "      titles:", "        first:",
               paste0("          '1': {center: ", if (wide) "Wide title" else "Std title", "}")),
             file.path(sp, "defaults.yml"))
  f <- file.path(s$path, "study.yml")
  x <- readLines(f)
  writeLines(append(x, "  style: Std", after = which(x == "settings:")), f)
  dir.create(file.path(sp, "patterns"), showWarnings = FALSE)
  writeLines(c("codelists:", "  SEX: [{value: F, label: PatF}, {value: M, label: PatM}]"),
             file.path(sp, "patterns", "pp.yml"))
  f <- file.path(sp, "outputs", "DM2.yml")
  x <- readLines(f)
  if (pattern) x <- sub("^  - defaults$", "  - patterns/pp", x)
  if (style) x <- append(x, "style: Wide", after = which(x == "kind: table"))
  writeLines(x, f)
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

test_that("a page configuration's title edited through the sheet keeps the code lists", {
  lk_home()
  s <- lk_study("LK1")
  expect_equal(lk_label(s$planner, "DM2"), c("PatF", "PatM"))
  pat <- lk_file(s, "patterns", "pp.yml")
  s$planner <- lk_title(s$planner, "@Wide", "Edited title")
  s2 <- save_study(s)
  x <- lk_file(s, "defaults.yml")
  expect_true(any(grepl("Edited title", x, fixed = TRUE)))
  expect_false(any(grepl("Wide title", x, fixed = TRUE)))
  # (the study's default configuration, the defaults' code list and content stay)
  expect_true(any(grepl("Std title", x, fixed = TRUE)))
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("DefF", x, fixed = TRUE)))
  expect_true(any(grepl("table_plan", x, fixed = TRUE)))
  # (the pattern's code list: its file is as it was, and the spec in memory says so)
  expect_identical(lk_file(s, "patterns", "pp.yml"), pat)
  expect_equal(lk_label(s2$planner, "DM2"), c("PatF", "PatM"))
  expect_equal(lk_label(s2$planner, "DM"), c("DefF", "DefM"))
  expect_equal(s2$planner$spec$text[["spec/defaults.yml"]], x)
  s3 <- open_study("LK1")
  expect_identical(s3$spec$status, "same")
  expect_equal(lk_label(s3$planner, "DM2"), c("PatF", "PatM"))
  expect_equal(sheet_rows(s3$planner, "titles", "@Wide")$center, "Edited title")
})

test_that("the default configuration's title edited through the sheet keeps the defaults' code lists, content and the other configurations", {
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
  expect_false(any(grepl("Std title", x, fixed = TRUE)))
  expect_true(any(grepl("Wide title", x, fixed = TRUE)))
  expect_equal(lk_label(s2$planner, "DM"), c("DefF", "DefM"))
  expect_equal(lk_label(s2$planner, "DM2"), c("PatF", "PatM"))
  expect_equal(s2$planner$spec$text[["spec/defaults.yml"]], x)
  s3 <- open_study("LK2")
  expect_identical(s3$spec$status, "same")
  expect_equal(lk_label(s3$planner, "DM"), c("DefF", "DefM"))
})

test_that("the page screen's writer keeps the lists, and names a configuration that is not the study's", {
  lk_home()
  s <- lk_study("LK3")
  p <- .pgs_write(s$planner, "@Wide", list(titles = list(`first:1` = list(center = "Own"))))
  s$planner <- p
  save_study(s)
  x <- lk_file(s, "defaults.yml")
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("DefF", x, fixed = TRUE)))
  expect_true(any(grepl("Own", x, fixed = TRUE)))
  expect_true(any(grepl("PatF", lk_file(s, "patterns", "pp.yml"), fixed = TRUE)))
  expect_equal(lk_label(p, "DM2"), c("PatF", "PatM"))
  # a built-in configuration is read only; a name that is neither the study's nor a
  # built-in one is said to be unknown, not called a built-in
  pg <- list(titles = list(`1` = list(center = "X")))
  expect_error(.pgs_write(p, "@tfl_small", pg), "built-in one")
  expect_error(.pgs_write(p, "@pp", pg), "no page configuration pp")
  msg <- tryCatch(.pgs_write(p, "@pp", pg), error = conditionMessage)
  expect_false(grepl("is a built-in", msg, fixed = TRUE))
})

test_that("a legacy pattern that holds code lists only keeps its marker row", {
  lk_home()
  s <- lk_study("LK4", style = FALSE)
  d <- s$planner$sheets$report
  # (a pattern with no page rows: its row of the report sheet is the marker)
  expect_true("@pp" %in% d$output_id)
  expect_identical(d$pattern[d$output_id == "DM2"], "@pp")
  expect_true(any(grepl("^codelists:", s$planner$spec$text[["spec/patterns/pp.yml"]])))
  expect_false(any(grepl("^page:", s$planner$spec$text[["spec/patterns/pp.yml"]])))
  # an edit of another report's page: the pattern's file and row stay
  s$planner <- .pgs_write(s$planner, "DM", list(titles = list(`1` = list(center = "Own"))))
  s2 <- save_study(s)
  expect_false(any(grepl("^page:", lk_file(s, "patterns", "pp.yml"))))
  expect_true(any(grepl("PatF", lk_file(s, "patterns", "pp.yml"), fixed = TRUE)))
  expect_true("@pp" %in% s2$planner$sheets$report$output_id)
  s3 <- open_study("LK4")
  expect_identical(s3$spec$status, "same")
  expect_true("@pp" %in% s3$planner$sheets$report$output_id)
  sh <- .planner_sheets(s3$planner, s3$meta)
  expect_s3_class(tflspec::tfl_table_spec(sh$table), "tfl_table_spec")
  expect_false("@pp" %in% output_ids(s3$planner))
  expect_equal(lk_label(s3$planner, "DM2"), c("PatF", "PatM"))
})

test_that("a configuration that differs from the default by no row keeps its marker row", {
  lk_home()
  # (Wide has the default's page: the sheets have no row of it but the marker)
  s <- lk_study("LK7", pattern = FALSE, wide = FALSE)
  d <- s$planner$sheets$report
  expect_true("@Wide" %in% d$output_id)
  expect_identical(d$pattern[d$output_id == "DM2"], "@Wide")
  expect_false(.pattern_in_view(s$planner, "@Wide", setdiff(.pattern_sheets, "report")))
  expect_true(any(grepl("^codelists:", s$planner$spec$text[["spec/defaults.yml"]])))
  sh <- .planner_sheets(s$planner, s$meta)
  expect_s3_class(tflspec::tfl_table_spec(sh$table), "tfl_table_spec")
  # the page screen gives it a line of its own, then takes it away again
  s$planner <- .pgs_write(s$planner, "@Wide", list(titles = list(`first:1` = list(center = "Own"))))
  s2 <- save_study(s)
  x <- lk_file(s, "defaults.yml")
  expect_true(any(grepl("Own", x, fixed = TRUE)))
  expect_true(any(grepl("DefF", x, fixed = TRUE)))
  expect_true(any(grepl("table_plan", x, fixed = TRUE)))
  s2$planner <- .pgs_write(s2$planner, "@Wide",
                           list(titles = list(`first:1` = list(center = "Std title"))))
  s3 <- save_study(s2)
  x <- lk_file(s, "defaults.yml")
  expect_false(any(grepl("Own", x, fixed = TRUE)))
  expect_true(any(grepl("^  Wide:", x)))
  expect_true("@Wide" %in% s3$planner$sheets$report$output_id ||
                .pattern_in_view(s3$planner, "@Wide"))
  s4 <- open_study("LK7")
  expect_identical(s4$spec$status, "same")
  expect_true(.pattern_in_view(s4$planner, "@Wide"))
  # another edit, saved: the sheets still work, the lists are kept
  s4$planner <- .pgs_write(s4$planner, "DM", list(titles = list(`1` = list(center = "Dm"))))
  s5 <- save_study(s4)
  expect_true(.pattern_in_view(s5$planner, "@Wide"))
  sh <- .planner_sheets(s5$planner, s5$meta)
  expect_s3_class(tflspec::tfl_table_spec(sh$table), "tfl_table_spec")
  expect_false("@Wide" %in% output_ids(s5$planner))
  expect_equal(lk_label(s5$planner, "DM"), c("DefF", "DefM"))
  expect_equal(lk_label(s5$planner, "DM2"), c("PatF", "PatM"))
})

test_that("a study saved before the marker rows opens with the legacy pattern's row", {
  home <- lk_home()
  s <- lk_study("LK5", style = FALSE)
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
  x <- lk_file(s3, "defaults.yml")
  expect_true(any(grepl("Now a page", x, fixed = TRUE)))
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("PatF", lk_file(s3, "patterns", "pp.yml"), fixed = TRUE)))
})

test_that("a configuration renamed with rename_page_pattern() keeps the code lists", {
  lk_home()
  s <- lk_study("LK6", pattern = FALSE)
  s$planner <- rename_page_pattern(s$planner, "Wide", "Narrow")
  s2 <- save_study(s)
  x <- lk_file(s, "defaults.yml")
  expect_true(any(grepl("^  Narrow:", x)))
  expect_false(any(grepl("^  Wide:", x)))
  expect_true(any(grepl("Wide title", x, fixed = TRUE)))
  expect_true(any(grepl("^codelists:", x)))
  expect_true(any(grepl("DefF", x, fixed = TRUE)))
  expect_true(any(grepl("PatF", lk_file(s, "patterns", "pp.yml"), fixed = TRUE)))
  expect_true(any(grepl("style: Narrow", lk_file(s, "outputs", "DM2.yml"), fixed = TRUE)))
  expect_equal(lk_label(s2$planner, "DM2"), c("PatF", "PatM"))
  s3 <- open_study("LK6")
  expect_identical(s3$spec$status, "same")
})
