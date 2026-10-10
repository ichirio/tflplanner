# The review of a study (#288 phase 1): tflspec's rules on the study's
# definition, the report list's and the study folder's own, the facts of
# the data kept in the store, the messages in the app's language.

.rv_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE))
}

.has <- function(r, rule, output_id = NULL, sheet = NULL, row = NULL, field = NULL) {
  k <- r$rule == rule
  if (!is.null(output_id)) k <- k & (if (is.na(output_id)) is.na(r$output_id) else
    r$output_id %in% output_id)
  if (!is.null(sheet)) k <- k & r$sheet == sheet
  if (!is.null(row)) k <- k & r$row == row
  if (!is.null(field)) k <- k & r$field == field
  any(k)
}

test_that("the sample study has no error and nothing to set by hand", {
  skip_on_cran()
  s <- .rv_sample()
  r <- study_review(s, data = "read", lang = "en")
  expect_s3_class(r, "tfl_review")
  expect_false(any(r$level %in% c("error", "hand")),
               info = paste(r$rule, r$output_id, r$message, collapse = "\n"))
  # its checks, seen: the code list values no record has (T-14-1-5/6
  # count too since they read the enrolled set itself, not a data made by
  # code; their ETHNIC's NOT REPORTED / UNKNOWN as T-14-1-1's), the KM figure's advice, the medians figure's ARD (T-14-2-2's)
  # not made yet
  expect_identical(as.integer(table(r$rule)[c("C02", "F02", "F07")]), c(15L, 2L, 1L))
  expect_identical(sort(unique(r$rule)), c("C02", "F02", "F07"))
  # without the data: no data rule, and no row it would not have with them
  r0 <- study_review(s, data = "none", lang = "en")
  expect_identical(sort(unique(r0$rule)), c("F02", "F07"))
})

test_that("the report list's rules (R01-R08) and the analyses' (A11-A13)", {
  local_home()
  p <- add_output(new_planner(), "T1", type = "table", description = "one",
                  population = "SAF")
  p <- add_output(p, "T2", type = "table", description = "two", population = "FAS")
  p <- add_output(p, "L1", type = "listing", description = "a listing")
  p <- add_output(p, "F1", type = "figure", description = "a figure")
  p$ard$populations <- .normalize_ard_sheet(
    data.frame(population_id = c("SAF", "ENR"), dataset = "ADSL",
               where = c("SAFFL == \"Y\"", "ENRLFL == \"Y\"")), "populations")
  p$ard$datasets <- .normalize_ard_sheet(
    data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"), "datasets")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = c("T1", "T1", "T9"), analysis_id = c("AGE", "SEX", "X"),
    method = "continuous", dataset = "ADSL",
    population_id = c("SAF", "ENR", "SAF"), by = "TRT01A",
    variables = c("AGE", "AGE", "AGE")), "analyses")
  p$outputs$datasets[p$outputs$output_id == "T1"] <- "ADSL | ADXX"
  p <- set_sheet_rows(p, "tokens", "T1", data.frame(name = "OUTPUT_TITLE", value = "Age"))
  r <- review_problems(p, lang = "en")
  # T2: no title, no analyses, its analysis set unknown
  expect_true(.has(r, "R01", "T2", "titles"))
  expect_false(.has(r, "R01", "T1"))
  expect_true(.has(r, "R05", "T2", "_tflplanner", "T2", "population"))
  expect_true(.has(r, "R06", "T2", "analyses"))
  # a listing with no columns, a figure with no design
  expect_true(.has(r, "R06", "L1", "listing_cols"))
  expect_true(.has(r, "R06", "F1", "design"))
  # a dataset the list names that the catalog does not have
  expect_true(.has(r, "R08", "T1", "_tflplanner", "T1", "datasets"))
  expect_match(r$message[r$rule == "R08"], "ADXX", fixed = TRUE)
  # analyses of a report the list does not have; another analysis set
  expect_true(.has(r, "A11", "T9", "analyses", "X"))
  expect_true(.has(r, "A12", "T1", "analyses", "SEX", "population_id"))
  # R04: a table with no analysis set and analyses that name none
  p2 <- p
  p2$outputs$population[p2$outputs$output_id == "T1"] <- NA
  p2$ard$analyses$population_id <- NA
  expect_true(.has(review_problems(p2, lang = "en"), "R04", "T1"))
  # ... unless its analyses are its own code (it reads what it needs)
  p2$ard$analyses$method <- "custom"
  p2$ard$analyses$code <- "x"
  expect_false(.has(review_problems(p2, lang = "en"), "R04", "T1"))
  # R02 / R07: an id twice; two reports writing one file
  p3 <- p
  p3$outputs <- rbind(p3$outputs, p3$outputs[p3$outputs$output_id == "T1", ])
  expect_true(.has(review_problems(p3, lang = "en"), "R02", "T1"))
  p4 <- set_sheet_rows(p, "report", "T2", data.frame(file = "T1.rtf"))
  expect_true(.has(review_problems(p4, lang = "en"), "R07", "T2", "report", "T2", "file"))
  # output_id narrows
  expect_true(all(review_problems(p, output_id = "T2", lang = "en")$output_id == "T2"))
})

test_that("a dataset with no file: once for the study and under each report reading it", {
  skip_on_cran()
  s <- .rv_sample()
  file.remove(file.path(s$path, "data", "adam", "adae.rds"))
  r <- study_review(s, data = "none", lang = "en")
  expect_true(.has(r, "D01", NA, "datasets", "ADAE", "path"))
  expect_true(.has(r, "D01", "T-14-3-1", "datasets", "ADAE"))
  expect_true(.has(r, "D01", "L-16-2-7", "datasets", "ADAE"))
  expect_false(.has(r, "D01", "T-14-1-1"))
  expect_identical(unique(r$level[r$rule == "D01"]), "error")
})

test_that("the data rules on the sample: a misspelt column, a value the code list lacks", {
  skip_on_cran()
  s <- .rv_sample()
  a <- s$planner$ard$analyses
  a$by[a$output_id == "T-14-1-1" & a$analysis_id == "CONT"] <- "TRT01X"
  s$planner$ard$analyses <- a
  cl <- s$planner$sheets$codelists
  s$planner$sheets$codelists <- cl[!(cl$output_id %in% "T-14-1-1" & cl$variable %in% "SEX" &
                                       cl$value %in% "M"), , drop = FALSE]
  r <- study_review(s, data = "read", lang = "en")
  expect_true(.has(r, "A01", "T-14-1-1", "analyses", "CONT", "by"))
  expect_true(.has(r, "C03", "T-14-1-1", "codelists", "SEX", "value"))
  expect_identical(r$level[r$rule == "C03"][1L], "error")
  expect_match(r$hint[r$rule == "C03"][1L], "program will stop", fixed = TRUE)
})

test_that("the facts are kept: read once, a changed file or condition read again alone", {
  skip_on_cran()
  s <- .rv_sample()
  # cached, nothing kept yet: no file read, each dataset not reviewed yet
  local({
    local_mocked_bindings(read_data_head = function(...) stop("read"))
    r <- study_review(s, data = "cached", lang = "en")
    expect_setequal(r$row[r$rule == "D02"], c("ADSL", "ADAE", "ADVS", "ADTTE"))
    expect_false(.has(r, "F03"))
    expect_identical(nrow(study_review(s, data = "none", lang = "en")[0, ]), 0L)
  })
  f1 <- review_facts(s)
  expect_s3_class(f1, "tfl_data_facts")
  expect_setequal(names(f1$datasets), c("ADSL", "ADAE", "ADVS", "ADTTE"))
  expect_identical(attr(f1, "not_read"), character())
  # the second time: no file read
  read <- character()
  real <- read_data_head
  local_mocked_bindings(read_data_head = function(path, ...) {
    read <<- c(read, basename(path))
    real(path, ...)
  })
  f2 <- review_facts(s)
  expect_identical(read, character())
  expect_identical(f2$datasets, f1$datasets)
  # a file changed: that dataset again (and the analysis set's, for its
  # subjects), the others not
  advs <- file.path(s$path, "data", "adam", "advs.rds")
  d <- readRDS(advs)
  saveRDS(d[-1L, ], advs)
  review_facts(s)
  expect_true("advs.rds" %in% read)
  expect_false(any(c("adae.rds", "adtte.rds") %in% read))
  # a new condition on ADAE: ADAE again, not ADVS
  read <- character()
  ad <- s$planner$ard$analysis_data
  ad$where[ad$output_id %in% "T-14-3-1" & ad$data_id %in% "adae_saf"] <- "TRTEMFL == \"Y\" & AESER == \"Y\""
  s$planner$ard$analysis_data <- ad
  review_facts(s)
  expect_true("adae.rds" %in% read)
  expect_false(any(c("advs.rds", "adtte.rds") %in% read))
  # refresh: everything again
  read <- character()
  review_facts(s, refresh = TRUE)
  expect_true(all(c("adsl.rds", "adae.rds", "advs.rds", "adtte.rds") %in% read))
})

test_that("each rule has its words in Japanese, with as many values", {
  cat <- tflspec::tfl_review_rules()
  d <- utils::read.csv(system.file("i18n", "strings.csv", package = "tflplanner"),
                       stringsAsFactors = FALSE, encoding = "UTF-8")
  n_args <- function(x) lengths(regmatches(x, gregexpr("%([0-9]+[$])?s", x)))
  for (k in c(cat$message, cat$hint[nzchar(cat$hint)])) {
    i <- match(k, d$en)
    expect_false(is.na(i), info = k)
    if (!is.na(i)) expect_identical(n_args(d$ja[i]), n_args(k), info = k)
  }
  # a message in Japanese, its English kept
  p <- add_output(new_planner(), "T2", type = "table")
  r <- review_problems(p, lang = "ja")
  i <- which(r$rule == "R01")
  expect_match(r$message[i], "T2 にタイトルがありません", fixed = TRUE)
  expect_identical(r$message_en[i], "T2 has no title.")
  expect_match(r$hint[i], "タイトル", fixed = TRUE)
})

test_that("a study of 200 reports is reviewed without its data in a few seconds", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  s <- .make_big_study(200L, root = file.path(home, "ws"), home = home)
  t0 <- Sys.time()
  r <- study_review(s, data = "none", ard = FALSE, lang = "en")
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  expect_s3_class(r, "tfl_review")
  expect_lt(dt, 8)
})

test_that("a figure's advice is in the app's language, by its sentence", {
  local_home()
  p <- add_output(new_planner(), "F1", type = "figure")
  p <- set_fig_design(p, "F1", tflspec::tfl_fig_template("km_simple"))
  r <- review_problems(p, lang = "ja")
  i <- which(r$rule == "F02")
  expect_true(length(i) >= 1L)
  expect_match(r$message[i[1L]], "リスク集合", fixed = TRUE)
  expect_match(r$message_en[i[1L]], "number at risk", fixed = TRUE)
  expect_match(r$hint[i[1L]], "提案", fixed = TRUE)
})

test_that("the review takes rows with a column more or less than its own", {
  a <- .rv_row("R01", "T1", args = "T1")
  b <- .rv_row("R02", "T2", args = "T2")
  b$extra <- "more"
  r <- .review_bind(list(a, b))
  expect_identical(nrow(r), 2L)
  expect_true("extra" %in% names(r))
  expect_true(is.na(r$extra[r$rule == "R01"]))
  expect_identical(r$extra[r$rule == "R02"], "more")
})

test_that("every sentence a review row can carry beyond its rule's has its Japanese", {
  skip_if_not("tfl_review_templates" %in% getNamespaceExports("tflspec"))
  tp <- tflspec::tfl_review_templates()
  ja <- vapply(tp$template, tr, "", lang = "ja")
  expect_identical(tp$template[ja == tp$template], character(0))
  # each one's values go in: as many as the English takes
  for (i in seq_len(nrow(tp))) {
    n <- lengths(regmatches(tp$template[i], gregexpr("%s", tp$template[i], fixed = TRUE)))
    a <- paste0("v", seq_len(n))
    m <- do.call(sprintf, c(list(ja[[i]]), as.list(a)))
    expect_true(all(vapply(a, grepl, NA, x = m, fixed = TRUE)), info = tp$template[i])
  }
  # and the app's own (the figure's ARD, F04-F07)
  own <- c("%s is not a table of the study",
           "%s is not a table of the study (deleted, or renamed by hand)",
           "the design reads an ARD, but the figure has none: choose it in step 2",
           "%s has no analysis %s (any more): the figure prints from it",
           "the ARD of %s is not made yet: make it first (its step 2)")
  expect_false(any(vapply(own, tr, "", lang = "ja") == own))
})

test_that("a row's own sentence is translated and filled with its values", {
  r <- .rv_row("F06", "F-1", "report", "", "analysis_id", args = c("T-1", "KM"),
               template = "%s has no analysis %s (any more): the figure prints from it")
  expect_identical(r$message, "T-1 has no analysis KM (any more): the figure prints from it")
  j <- .review_language(.review_bind(list(r)), "ja")
  expect_match(j$message, "^T-1 \u306b\u89e3\u6790 KM")
  expect_identical(j$message_en, r$message)
  # a row without the column (tflspec before 0.0.24.9078): the rule's
  r$template <- NULL
  expect_identical(.review_language(.review_bind(list(r)), "ja")$message, r$message)
})

test_that("P02: a program calling a function tflspec no longer has", {
  skip_if_not("P02" %in% tflspec::tfl_review_rules()$rule)
  root <- withr_tempdir()
  dir.create(file.path(root, "programs", "ard"), recursive = TRUE)
  dir.create(file.path(root, "programs", ".edited"))
  put <- function(f, ...) writeLines(c(...), file.path(root, "programs", f))
  put("study_helpers.R", "set_levels <- function(x, ...) x")
  put("study_setup.R", "library(dplyr)", "source(\"programs/study_helpers.R\")")
  # generated now: the setup sources the helpers -- nothing to say
  put("ard/T-1.R", "source(\"programs/study_setup.R\")", "library(tflspec)", "d <- set_levels(d)")
  # tflspec:: stops whatever is sourced
  put("ard/T-2.R", "source(\"programs/study_setup.R\")", "tflspec::save_ard(ard, \"T-2\")")
  # an old program: tflspec attached, no helpers
  put("old.R", "library(tflspec)", "d <- set_levels(d)", "fmt_ard(a)")
  # attached, no helpers, but sourced by another: to check
  put("part.R", "library(\"tflspec\")", "keep_stats(a)")
  put("run.R", "source(\"programs/part.R\")")
  # its own function, and copies in .edited: not looked at
  put("own.R", "library(tflspec)", "tag_ard <- function(x) x", "tag_ard(1)")
  put(".edited/T-1.R", "library(tflspec)", "set_levels(d)")
  p <- new_planner()
  p <- add_output(p, "T-2", type = "table")
  r <- .rule_p02(list(path = root, planner = p))
  expect_setequal(r$row, c("programs/ard/T-2.R", "programs/old.R", "programs/part.R"))
  expect_identical(r$output_id[r$row == "programs/ard/T-2.R"], "T-2")
  expect_true(is.na(r$output_id[r$row == "programs/old.R"]))
  expect_identical(r$level[match(c("programs/ard/T-2.R", "programs/old.R", "programs/part.R"), r$row)],
                   c("error", "error", "check"))
  expect_identical(r$message[r$row == "programs/old.R"],
                   "programs/old.R calls set_levels(), fmt_ard(), which tflspec no longer has.")
  expect_identical(r$area, rep("program", 3L))
  j <- .review_language(r, "ja")
  expect_false(any(j$message == j$message_en))
  # a file that does not parse is passed over
  put("broken.R", "library(tflspec)", "set_levels(")
  expect_false("programs/broken.R" %in% .rule_p02(list(path = root, planner = p))$row)
})
