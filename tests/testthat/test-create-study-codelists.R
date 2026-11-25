# A new study takes the company standards' code lists: create_study() with
# no planner writes them into spec/defaults.yml `codelists:` (tflspec's
# study$codelists), as it copies the company's analysis sets and data
# catalog.  A company with none writes no key; an existing study never takes
# them.  Each list is compared with the company's own rows (its codes in
# their order, their labels), worked out here from standard_codelists().

skip_on_cran()

csc_defaults <- function(s) {
  readLines(file.path(s$path, "spec", "defaults.yml"), encoding = "UTF-8", warn = FALSE)
}

test_that("a new study's defaults.yml has the company standards' code lists, in their order", {
  local_home()
  std <- standard_codelists()
  sets <- unique(std$set)
  expect_true(all(c("SEX", "RACE", "ETHNIC") %in% sets))
  s <- create_study("CSC-1")
  x <- csc_defaults(s)
  expect_identical(sum(grepl("^codelists:", x)), 1L)
  y <- yaml::read_yaml(file.path(s$path, "spec", "defaults.yml"))
  expect_identical(names(y$codelists), sets)
  # as tflspec reads them: each list's codes in their order, with labels
  cl <- tflspec:::.study_codelists_read(y$codelists)
  expect_identical(names(cl), sets)
  for (k in sets) {
    r <- std[std$set == k & !is.na(std$value) & nzchar(trimws(std$value)), , drop = FALSE]
    o <- suppressWarnings(as.numeric(r$order))
    r <- r[order(is.na(o), o), , drop = FALSE]
    lab <- ifelse(is.na(r$label) | !nzchar(trimws(r$label)), r$value, r$label)
    # (a list whose labels are its codes: the codes alone)
    want <- if (all(lab == r$value)) r$value else stats::setNames(lab, r$value)
    expect_identical(cl[[k]], want, label = k)
  }
  # the study as tflspec reads its folder, and the planner's spec in memory
  expect_identical(tflspec::tfl_read_study_spec(s$path)$codelists, cl)
  expect_identical(s$planner$spec$study$codelists, cl)
  # (the study's lists are codes and labels: `under` and `note` not written)
  blk <- x[.study_block_range(x, "codelists")]
  expect_true(length(blk) > length(sets))
  expect_false(any(grepl("^[ -]*(under|note):", blk)))
  expect_false(any(grepl("sponsor's terms", x, fixed = TRUE)))
  # opened again: as it was saved, the lists in it
  o <- open_study("CSC-1")
  expect_identical(o$spec$status, "same")
  expect_identical(o$planner$spec$study$codelists, cl)
  # a save that changes defaults.yml (the font) keeps the block
  o$planner <- set_study_page_value(o$planner, "font", "Arial")
  o2 <- save_study(o)
  expect_identical(sum(grepl("^codelists:", csc_defaults(o2))), 1L)
  expect_identical(tflspec::tfl_read_study_spec(o2$path)$codelists, cl)
})

test_that("a study made from a planner has that planner's code lists, not the company's", {
  home <- local_home()
  # a planner with no lists: the new study has none
  s <- create_study("CSC-2", planner = add_output(new_planner(), "T-1"))
  expect_false(any(grepl("^codelists:", csc_defaults(s))))
  expect_length(tflspec::tfl_read_study_spec(s$path)$codelists, 0L)
  # a study made under a company with one list of its own (fictional arms)
  b <- .builtin_standards()
  own <- b$codelists[rep(1L, 2L), , drop = FALSE]
  own[] <- lapply(own, function(v) v[NA_integer_])
  own$set <- "ARMS"
  own$variable <- "TRT01P"
  own$value <- c("P", "X")
  own$label <- c("Placebo", "Drug X")
  own$order <- c("1", "2")
  b$codelists <- own
  f <- file.path(withr_tempdir(), "arms.xlsx")
  writexl::write_xlsx(b, f)
  suppressMessages(setup_tflplanner(standards = f))
  a <- create_study("CSC-2A")
  la <- tflspec::tfl_read_study_spec(a$path)$codelists
  expect_identical(names(la), "ARMS")
  # the company's lists again (the built-in ones), none of them ARMS
  unlink(file.path(home, "standards", "company_standards.xlsx"))
  expect_false("ARMS" %in% standard_codelists()$set)
  # a study made from that study's planner: its lists, not the company's
  s2 <- create_study("CSC-2B", planner = open_study("CSC-2A")$planner)
  y <- yaml::read_yaml(file.path(s2$path, "spec", "defaults.yml"))
  expect_identical(names(y$codelists), "ARMS")
  expect_identical(tflspec::tfl_read_study_spec(s2$path)$codelists, la)
  expect_identical(s2$planner$spec$study$codelists, la)
})

test_that("a create that stops on the company's code lists leaves no folder, and the same create works once they are fixed", {
  home <- local_home()
  # a company whose SEX list has the code F twice
  bad <- .builtin_standards()
  sex <- which(bad$codelists$set == "SEX")
  bad$codelists <- bad$codelists[c(seq_len(nrow(bad$codelists)), sex[1L]), , drop = FALSE]
  f <- file.path(withr_tempdir(), "dup.xlsx")
  writexl::write_xlsx(bad, f)
  suppressMessages(setup_tflplanner(standards = f))
  # (a code twice is made an error by the company standards' check; stood
  # in for here around standard_codelists(), which the create calls)
  real <- standard_codelists
  local_mocked_bindings(standard_codelists = function(...) {
    cl <- real(...)
    dup <- duplicated(cl[c("set", "value")]) & !is.na(cl$value)
    if (any(dup)) {
      stop("The code list ", cl$set[dup][1L], " has the code ", cl$value[dup][1L], " twice.",
           call. = FALSE)
    }
    cl
  })
  path <- .study_path(studies_root(home), "CSC-4", home)
  expect_error(create_study("CSC-4"), "has the code .* twice")
  expect_false(file.exists(path))
  expect_null(.read_state("CSC-4", home))
  # the company's lists fixed (the built-in ones, F once): the same create works
  unlink(file.path(home, "standards", "company_standards.xlsx"))
  s <- create_study("CSC-4")
  expect_identical(normalizePath(s$path, "/"), normalizePath(path, "/"))
  y <- yaml::read_yaml(file.path(s$path, "spec", "defaults.yml"))
  expect_identical(names(y$codelists), unique(standard_codelists()$set))
})

test_that("a company with no code lists writes no codelists:, and an existing study never takes them", {
  home <- local_home()
  f <- file.path(withr_tempdir(), "nolists.xlsx")
  b <- .builtin_standards()
  b$codelists <- b$codelists[0L, ]
  writexl::write_xlsx(b, f)
  suppressMessages(setup_tflplanner(standards = f))
  expect_identical(nrow(standard_codelists()), 0L)
  s <- create_study("CSC-3")
  expect_false(any(grepl("^codelists:", csc_defaults(s))))
  expect_length(s$planner$spec$study$codelists, 0L)
  # the company's lists again (the built-in ones): the study made before
  # them, opened and saved, takes none
  unlink(file.path(home, "standards", "company_standards.xlsx"))
  expect_true(nrow(standard_codelists()) > 0L)
  o <- open_study("CSC-3")
  expect_length(o$planner$spec$study$codelists, 0L)
  o$planner <- set_study_page_value(o$planner, "font", "Arial")
  o2 <- save_study(o)
  expect_false(any(grepl("^codelists:", csc_defaults(o2))))
  expect_length(tflspec::tfl_read_study_spec(o2$path)$codelists, 0L)
})

test_that("the block: a list a set in the sheet's order, its values by order, a value with no label alone", {
  cl <- data.frame(
    set = c("B", "A", "A", "B", "A", NA),
    variable = c("BV", "AV", "AV", "BV", "AV", "CV"),
    value = c("y", "a2", "a1", "x", NA, "c1"),
    label = c("Why", NA, "First", "Ex", "Lost", "C one"),
    order = c("2", "2", "1", "1", "3", "1"),
    under = NA_character_, note = c("a note", NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE)
  # (standard_codelists() gives a missing set its variable)
  cl$set[is.na(cl$set)] <- cl$variable[is.na(cl$set)]
  block <- .std_codelists_block(cl)
  expect_true(any(grepl("^codelists:", block)))
  expect_false(any(grepl("note", block, fixed = TRUE)))
  y <- yaml::yaml.load(paste(block, collapse = "\n"))$codelists
  expect_identical(names(y), c("B", "A", "CV"))
  expect_identical(y$B, list(list(value = "x", label = "Ex"), list(value = "y", label = "Why")))
  # a row with no value is no code; a1 before a2, a2 written alone
  expect_identical(y$A, list(list(value = "a1", label = "First"), "a2"))
  read <- .defaults_block_read(block)$codelists
  expect_identical(read$A, c(a1 = "First", a2 = "a2"))
  expect_identical(.std_codelists_block(cl[0L, ]), character())
  expect_identical(.std_codelists_block(NULL), character())
})

test_that("create_study(home = ) takes the analysis sets, data catalog and code lists from that home alone", {
  home_a <- local_home()
  # home B: a company with one analysis set, one dataset, one code list and
  # its own subject id
  b <- .builtin_standards()
  b$populations <- b$populations[b$populations$population_id == "SAF", , drop = FALSE]
  b$populations$population_id <- "BSAF"
  b$datasets <- b$datasets[b$datasets$dataset == "ADSL", , drop = FALSE]
  b$codelists <- b$codelists[b$codelists$set == "SEX", , drop = FALSE]
  b$settings$value[b$settings$key == "subject_id"] <- "SUBJID"
  f <- file.path(withr_tempdir(), "home-b.xlsx")
  writexl::write_xlsx(b, f)
  home_b <- withr_tempdir()
  # (home B set up, then home A the default again)
  options(tflplanner.home = home_b)
  suppressMessages(setup_tflplanner(studies_root = file.path(home_b, "ws"), standards = f))
  options(tflplanner.home = home_a)
  expect_true("RACE" %in% standard_codelists()$set)
  expect_identical(unique(standard_codelists(home = home_b)$set), "SEX")
  # a study of home B, made while home A is the default: all B's
  s <- create_study("TWO-B", home = home_b)
  expect_identical(s$planner$ard$populations$population_id, "BSAF")
  expect_identical(s$planner$ard$datasets$dataset, "ADSL")
  expect_identical(s$planner$ard$study$value[s$planner$ard$study$key == "id"], "SUBJID")
  y <- yaml::read_yaml(file.path(s$path, "spec", "defaults.yml"))
  expect_identical(names(y$codelists), "SEX")
  # a study of home A: A's (the built-in standards)
  a <- create_study("TWO-A")
  expect_true(all(c("SAF", "FAS") %in% a$planner$ard$populations$population_id))
  expect_false("BSAF" %in% a$planner$ard$populations$population_id)
  expect_identical(a$planner$ard$study$value[a$planner$ard$study$key == "id"], "USUBJID")
  ya <- yaml::read_yaml(file.path(a$path, "spec", "defaults.yml"))
  expect_true(all(c("SEX", "RACE", "ETHNIC") %in% names(ya$codelists)))
})
