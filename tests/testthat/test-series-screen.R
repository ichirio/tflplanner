# The Template series card (R/srv_series.R, R/series_ui.R).  tflspec's
# series functions are replaced: through tflspec itself
# (local_mocked_bindings(.package = "tflspec")) once tflspec has them, and
# until then through tflplanner's lookup of them (.series_fn()).

local_series <- function(fns, env = parent.frame()) {
  ns <- asNamespace("tflspec")
  if (all(vapply(names(fns), exists, NA, envir = ns, inherits = FALSE))) {
    do.call(testthat::local_mocked_bindings, c(fns, list(.package = "tflspec", .env = env)))
  } else {
    testthat::local_mocked_bindings(
      .series_available = function() TRUE,
      .series_fn = function(name) fns[[name]],
      .env = env)
  }
}

# a series store and a study folder: `st$block` is study.yml's
# series_settings, `st$calls` what was asked of tflspec
fake_series <- function(dir) {
  st <- new.env()
  st$block <- list(tflspec_v1 = list(population_safety = "SAF2"))
  st$calls <- list()
  st$changed <- FALSE
  vals <- list(tflspec_v1 = list(
    style = "tfl_standard", population_safety = "SAF",
    formats = list(N = 0L, mean = 2L, p = "xx.x%"), total = FALSE,
    sd_missing = "-", sponsor = structure(list(key = "sponsor.name"), class = "tfl_study_ref"),
    conf_level = structure(list(value = 0.95), class = "tfl_fixed_ref")),
    site_v2 = list(style = "site"))
  fns <- list(
    tfl_series_list = function(dir = NULL) {
      data.frame(name = c("site_v2", "tflspec_v1"), label = c("Site", "tflspec templates"),
                 released = c("2026-10-01", "2026-11-17"),
                 path = c("/home/standards/series/site_v2", "/pkg/series/tflspec_v1"),
                 source = c("home", "package"), stringsAsFactors = FALSE)
    },
    tfl_series_read = function(series, dir = NULL) {
      list(name = series, label = series, released = "2026-11-17", needs = NULL,
           fixes = list(), values = vals[[series]], path = paste0("/x/", series))
    },
    tfl_series_settings = function(study, series = NULL) {
      if (is.null(series)) st$block else st$block[[series]]
    },
    tfl_series_settings_set = function(study_dir, series, values) {
      st$calls <- c(st$calls, list(list(series = series, values = values)))
      bad <- setdiff(names(values), names(vals[[series]]))
      if (length(bad)) stop("series_settings.", series, ": unknown key '", bad[1L], "'.", call. = FALSE)
      st$block[[series]] <- values
      invisible(study_dir)
    },
    tfl_pin_series = function(study_dir, reports = NULL) {
      dir.create(file.path(study_dir, "spec"), showWarnings = FALSE)
      yaml::write_yaml(list(tflspec_v1 = list(path = "/pkg/series/tflspec_v1", series = "abc123",
                                              templates = list(demog = "def456"))),
                       file.path(study_dir, "spec", "series.lock"))
      invisible(study_dir)
    },
    tfl_series_check_lock = function(study_dir) {
      if (st$changed) warning("tflspec_v1/templates/demog.yml changed since it was pinned.", call. = FALSE)
      if (st$changed) message("series tflspec_v1 is read from /new (lock: /pkg/series/tflspec_v1).")
      invisible(TRUE)
    })
  list(st = st, fns = fns, dir = dir)
}

series_app <- function(dir, notes) {
  function(input, output, session) {
    rv <- .series_server(input, output, session, study_dir = shiny::reactive(dir),
                   notify = function(msg, type = "message") {
                     notes$all <- c(notes$all, paste0(type, ": ", msg))
                   })
  }
}

html_of <- function(x) paste(as.character(x$html), collapse = "")

test_that("a value is shown as one line of YAML and reads back the same", {
  for (v in list("tfl_standard", "-", "{n} ({p:.1f%})", 0.95, 30L, TRUE, "a, b",
                 c("defaults", "more"), list(N = 0L, mean = 2L, "AGE:mean" = 1L), list())) {
    back <- .series_yaml_load(.series_flow(v))
    if (is.character(v) && length(v) > 1L) back <- unlist(back)
    expect_equal(back, v, info = .series_flow(v))
  }
  expect_identical(.series_flow(list(N = 0L, mean = 2L)), "{N: 0, mean: 2}")
  expect_identical(.series_yaml_load("{N: 0, y: yes, t: true}"), list(N = 0L, y = "yes", t = TRUE))
  # the tags as written; an edit starts from the value without its tag
  study <- structure(list(key = "sponsor.name"), class = "tfl_study_ref")
  fixed <- structure(list(value = list(N = 0L)), class = "tfl_fixed_ref")
  expect_identical(.series_flow(study), "!study sponsor.name")
  expect_identical(.series_flow(structure(list(key = "style"), class = "tfl_series_ref")), "!series style")
  expect_identical(.series_flow(fixed), "!fixed {N: 0}")
  expect_identical(.series_flow(structure(list(), class = "tfl_unset")), "!unset")
  expect_identical(.series_flow(list(a = fixed)), "{a: !fixed {N: 0}}")
  expect_identical(.series_edit_text(fixed), "{N: 0}")
  expect_identical(.series_edit_text(study), "")
  expect_identical(.series_edit_text("x"), "x")
})

test_that("the series folder is the home's when the home is not R_user_dir()", {
  withr::local_options(tflspec.series_dir = NULL)
  home <- withr::local_tempdir()
  expect_identical(.series_dir_option(home), file.path(home, "standards", "series"))
  expect_identical(getOption("tflspec.series_dir"), file.path(home, "standards", "series"))
  # back at R_user_dir(): the option this function set is taken away
  expect_null(.series_dir_option(tools::R_user_dir("tflplanner", "data")))
  expect_null(getOption("tflspec.series_dir"))
  # an option the user set stays
  withr::local_options(tflspec.series_dir = "/site/series")
  expect_identical(.series_dir_option(home), "/site/series")
  expect_identical(getOption("tflspec.series_dir"), "/site/series")
})

test_that("an older tflspec: the card only says it has no template series", {
  testthat::local_mocked_bindings(.series_available = function() FALSE)
  notes <- new.env()
  shiny::testServer(series_app(withr::local_tempdir(), notes), {
    h <- html_of(output$series_card)
    expect_match(h, "has no template series")
    expect_false(grepl("series_list", h, fixed = TRUE))
  })
})

test_that("the card lists the series and shows the study's series with its values", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    expect_match(html_of(output$series_card), "series_list")
    # the study's block names tflspec_v1: it is the one chosen
    session$flushReact()
    v <- html_of(output$series_values)
    expect_match(v, "Values of tflspec_v1")
    expect_match(v, "{N: 0, mean: 2, p: xx.x%}", fixed = TRUE)
    expect_match(v, "!study sponsor.name", fixed = TRUE)
    expect_match(v, "!fixed 0.95", fixed = TRUE)
    # the study's value beside its key, as an input
    expect_match(v, 'id="series_edit_2"', fixed = TRUE)
    expect_match(v, "SAF2", fixed = TRUE)
    expect_match(v, "Back to the series value", fixed = TRUE)
    expect_match(v, "Change for this study", fixed = TRUE)
    # the list: where each was found, in words
    d <- .series_list_table(f$fns$tfl_series_list())
    expect_identical(d[[4]], c("tflplanner's home (standards/series)", "In tflspec (package)"))
    # another row chosen
    # a !fixed value is changed from its value; a !study reference from nothing
    session$setInputs(series_change = list(key = "conf_level", n = 1))
    session$setInputs(series_change = list(key = "sponsor", n = 2))
    expect_identical(rv$draft$conf_level, "0.95")
    expect_identical(rv$draft$sponsor, "")
    session$setInputs(series_list_rows_selected = 1L)
    expect_match(html_of(output$series_values), "Values of site_v2")
  })
})

test_that("change, edit, back and save go through tfl_series_settings_set()", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    # formats (row 3) copied in for editing, as the series has it
    session$setInputs(series_change = list(key = "formats", n = 1))
    expect_identical(rv$draft$formats, "{N: 0, mean: 2, p: xx.x%}")
    expect_match(html_of(output$series_values), "unsaved", fixed = TRUE)
    session$setInputs(series_edit_3 = "{N: 0, mean: 1}", series_edit_2 = "SAF3")
    session$setInputs(series_save = 1L)
    got <- f$st$calls[[1L]]
    expect_identical(got$series, "tflspec_v1")
    expect_identical(got$values, list(population_safety = "SAF3", formats = list(N = 0L, mean = 1L)))
    expect_match(notes$all[length(notes$all)], "^message: Saved")
    expect_false(grepl("unsaved", html_of(output$series_values), fixed = TRUE))
    # both back to the series: the block removed (values = NULL)
    session$setInputs(series_back = list(key = "formats", n = 2))
    session$setInputs(series_back = list(key = "population_safety", n = 3))
    expect_identical(rv$draft, stats::setNames(list(), character()))
    session$setInputs(series_save = 2L)
    expect_null(f$st$calls[[2L]]$values)
    expect_null(f$st$block$tflspec_v1)
  })
})

test_that("tflspec's error is shown as it is, and a value that is not YAML stops before it", {
  f <- fake_series(withr::local_tempdir())
  f$st$block$tflspec_v1$old_key <- "x"
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    # a key of the block the series does not have is shown, not dropped
    expect_match(html_of(output$series_values), "(not in this series)", fixed = TRUE)
    session$setInputs(series_save = 1L)
    expect_identical(notes$all[length(notes$all)],
                     "error: series_settings.tflspec_v1: unknown key 'old_key'.")
    n <- length(f$st$calls)
    session$setInputs(series_back = list(key = "old_key", n = 1))
    session$setInputs(series_edit_2 = "[unclosed")
    session$setInputs(series_save = 2L)
    expect_match(notes$all[length(notes$all)], "^error: A value is not YAML:")
    expect_identical(length(f$st$calls), n)
  })
})

test_that("the pins: the button writes spec/series.lock, its pins and the lock's warnings are shown", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    expect_match(html_of(output$series_lock), "No spec/series.lock yet", fixed = TRUE)
    session$setInputs(series_pin = 1L)
    expect_true(file.exists(file.path(f$dir, "spec", "series.lock")))
    l <- html_of(output$series_lock)
    expect_match(l, "abc123", fixed = TRUE)
    expect_match(l, "def456", fixed = TRUE)
    expect_match(l, "spec/series.lock as written", fixed = TRUE)
    expect_match(notes$all[length(notes$all)], "^message: Pinned")
    f$st$changed <- TRUE
    session$setInputs(series_pin = 2L)
    l <- html_of(output$series_lock)
    expect_match(l, "demog.yml changed since it was pinned", fixed = TRUE)
    expect_match(l, "is read from /new", fixed = TRUE)
  })
})

test_that("the Study tab has the card, and its texts have their Japanese", {
  expect_match(as.character(app_ui("en")), 'id="series_card"', fixed = TRUE)
  r_dir <- test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(r_dir, "srv_series.R")), "source not available (installed package)")
  code <- unlist(lapply(file.path(r_dir, c("srv_series.R", "series_ui.R")), readLines,
                        encoding = "UTF-8", warn = FALSE))
  lits <- unlist(regmatches(code, gregexpr('\\bt\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE)))
  lits <- unique(eval(parse(text = paste0("c(", paste(sub("^t", "", lits), collapse = ","), ")"))))
  expect_gt(length(lits), 20L)
  s <- .strings()
  expect_identical(setdiff(lits, s$en), character())
  expect_false(any(grepl("水準", s$ja[s$en %in% lits])))
})

test_that("the app's server starts the card on the open study", {
  local_home()
  two_studies()
  f <- fake_series(NULL)
  local_series(f$fns)
  suppressWarnings(shiny::testServer(server_for("S1"), {
    session$flushReact()
    expect_match(html_of(output$series_card), "series_list", fixed = TRUE)
    expect_match(html_of(output$series_values), "Values of tflspec_v1", fixed = TRUE)
    expect_match(html_of(output$series_lock), "No spec/series.lock yet", fixed = TRUE)
  }))
})
