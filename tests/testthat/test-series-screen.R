# The Template series card (R/srv_series.R, R/series_ui.R): the series, the
# study's values in its copy of a series, and the study's copies with their
# state (tflspec#879, canon 2026-11-19).  tflspec's functions are replaced
# through tflspec itself (local_mocked_bindings(.package = "tflspec")) when
# it has them, else through tflplanner's lookup of them (.series_fn()).

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

# a series store and a study folder with its copy of tflspec_v1 (not of
# site_v2): `st$copy` the copy's values, `st$status` its copies' state,
# `st$calls` what was asked of tflspec
fake_series <- function(dir) {
  st <- new.env()
  st$calls <- list()
  vals <- list(tflspec_v1 = list(
    style = "tfl_standard", population_safety = "SAF",
    formats = list(N = 0L, mean = 2L, p = "xx.x%"), total = FALSE,
    sd_missing = "-", sponsor = structure(list(key = "sponsor.name"), class = "tfl_study_ref"),
    conf_level = structure(list(value = 0.95), class = "tfl_fixed_ref")),
    site_v2 = list(style = "site"))
  # (the study changed one value in its copy)
  st$copy <- list(tflspec_v1 = utils::modifyList(vals$tflspec_v1, list(population_safety = "SAF2")))
  if (!is.null(dir)) {
    dir.create(file.path(dir, "spec", "series", "tflspec_v1"), recursive = TRUE, showWarnings = FALSE)
    writeLines("name: tflspec_v1", file.path(dir, "spec", "series", "tflspec_v1", "series.yml"))
  }
  st$status <- data.frame(
    series = "tflspec_v1", template = c(NA, "ae_soc_pt", "demog", "km_simple"),
    file = "x", from = c("/pkg/series/tflspec_v1/series.yml", "/pkg/series/tflspec_v1/templates/ae_soc_pt.yml",
                         "/pkg/series/tflspec_v1/templates/demog.yml", "/pkg/series/tflspec_v1/templates/km_simple.yml"),
    copied_at = "2026-11-20 10:00:00",
    status = c("same", "source_changed", "study_changed", "source_missing"), stringsAsFactors = FALSE)
  # what a value means: a text tflplanner translates, one it does not; the
  # other keys have none, and site_v2 none at all
  docs <- list(tflspec_v1 = c(
    count_cell = "The cell of a count and its percent; % means ×100; no % sign is printed (e.g. 12 (34.5)).",
    style = "A site's own words."))
  fns <- list(
    tfl_series_list = function(dir = NULL) {
      data.frame(name = c("site_v2", "tflspec_v1"), label = c("Site", "tflspec templates"),
                 released = c("2026-10-01", "2026-11-17"),
                 path = c("/home/standards/series/site_v2", "/pkg/series/tflspec_v1"),
                 source = c("home", "package"), stringsAsFactors = FALSE)
    },
    # as tflspec: `dir` looked in first (the study's copies), then the sources
    tfl_series_read = function(series, dir = NULL) {
      v <- if (!is.null(dir) && !is.null(st$copy[[series]])) st$copy[[series]] else vals[[series]]
      list(name = series, label = series, released = "2026-11-17", needs = NULL,
           fixes = list(), values = v, descriptions = docs[[series]],
           path = paste0("/x/", series))
    },
    tfl_series_values_set = function(study_dir, series, values) {
      st$calls <- c(st$calls, list(list(fn = "set", series = series, values = values)))
      if (any(names(values) == "bad_key")) stop("tfl_series_values_set(): bad_key is refused.", call. = FALSE)
      # as tflspec: each value set in the copy, a NULL takes its key out
      cp <- st$copy[[series]]
      for (k in names(values)) {
        v <- values[[k]]
        # (a value written as its YAML text: read back as tflspec reads it)
        if (inherits(v, "verbatim")) {
          v <- if (v %in% c("true", "false")) identical(unclass(v), "true") else vals[[series]][[k]]
        }
        cp[k] <- list(v)
      }
      st$copy[[series]] <- cp[!vapply(cp, is.null, NA)]
      invisible(file.path(study_dir, "spec", "series", series, "series.yml"))
    },
    tfl_template_status = function(study_dir, dir = NULL) st$status,
    tfl_template_diff = function(study_dir, template) {
      st$calls <- c(st$calls, list(list(fn = "diff", template = template)))
      c("--- copy", "+++ source", " params:", "-  by: TRT01P", "+  by: TRT01A", " end: 1")
    },
    tfl_recopy_templates = function(study_dir, templates) {
      st$calls <- c(st$calls, list(list(fn = "recopy", template = templates)))
      i <- match(templates, paste0(st$status$series, "/", st$status$template))
      if (st$status$status[i] %in% c("study_changed", "both_changed")) {
        warning("the study's edits are lost", call. = FALSE)
      }
      st$status$status[i] <- "same"
      data.frame(series = "tflspec_v1", template = sub("^.*/", "", templates), file = "x",
                 from = "y", md5 = "z", status = "recopied", stringsAsFactors = FALSE)
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

# the dialogs shown (as text) and how many were taken away
watch_modals <- function(env = parent.frame()) {
  shown <- new.env()
  shown$modals <- character()
  shown$removed <- 0L
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) {
      shown$modals <- c(shown$modals, paste(as.character(ui), collapse = ""))
      invisible()
    },
    removeModal = function(session = NULL) {
      shown$removed <- shown$removed + 1L
      invisible()
    },
    .package = "shiny", .env = env)
  shown
}

html_of <- function(x) paste(as.character(x$html), collapse = "")
calls_of <- function(st, fn) Filter(function(x) identical(x$fn, fn), st$calls)

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
  # what tfl_series_values_set() is given: a tag and a flag as YAML text
  expect_identical(yaml::as.yaml(list(a = .series_value_out(study))), "a: !study sponsor.name\n")
  expect_identical(yaml::as.yaml(list(a = .series_value_out(TRUE))), "a: true\n")
  expect_identical(.series_value_out(list(N = 0L)), list(N = 0L))
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

test_that("the card lists the series and shows the study's copied series with its own values", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    expect_match(html_of(output$series_card), "series_list")
    # the study has a copy of tflspec_v1: it is the one chosen
    session$flushReact()
    v <- html_of(output$series_values)
    expect_match(v, "Defaults shared by the tflspec_v1 series' templates (values:)", fixed = TRUE)
    expect_match(v, "{N: 0, mean: 2, p: xx.x%}", fixed = TRUE)
    expect_match(v, "!study sponsor.name", fixed = TRUE)
    expect_match(v, "!fixed 0.95", fixed = TRUE)
    # what each value means, beside its key; a key without one: an empty cell
    expect_match(v, "<th>Description</th>", fixed = TRUE)
    expect_match(v, "A site's own words.", fixed = TRUE)
    expect_match(v, '<td class="text-muted"></td>', fixed = TRUE)
    expect_match(v, "This study's value (its copy's values:)", fixed = TRUE)
    # the copy's value that is not the source's: the study's own, as an input
    expect_identical(names(rv$saved), "population_safety")
    expect_match(v, 'id="series_edit_2"', fixed = TRUE)
    expect_match(v, "SAF2", fixed = TRUE)
    expect_match(v, "Back to the series value", fixed = TRUE)
    expect_match(v, "Change for this study", fixed = TRUE)
    expect_match(v, "tfl_series_values_set", fixed = TRUE)
    # the list: where each was found, in words
    d <- .series_list_table(f$fns$tfl_series_list())
    expect_identical(d[[4]], c("tflplanner's home (standards/series)", "In tflspec (package)"))
    # a !fixed value is changed from its value; a !study reference from nothing
    session$setInputs(series_change = list(key = "conf_level", n = 1))
    session$setInputs(series_change = list(key = "sponsor", n = 2))
    expect_identical(rv$draft$conf_level, "0.95")
    expect_identical(rv$draft$sponsor, "")
    # a series the study has no copy of: its values, nothing to save
    session$setInputs(series_list_rows_selected = 1L)
    v <- html_of(output$series_values)
    expect_match(v, "Defaults shared by the site_v2 series' templates")
    expect_match(v, "This study has no copy of this series yet", fixed = TRUE)
    expect_false(grepl("series_save", v, fixed = TRUE))
    session$setInputs(series_save = 1L)
    expect_identical(calls_of(f$st, "set"), list())
  })
})

test_that("change, edit, back and save go through tfl_series_values_set() into the copy", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    # formats (row 3) taken in for editing, as the series has it
    session$setInputs(series_change = list(key = "formats", n = 1))
    expect_identical(rv$draft$formats, "{N: 0, mean: 2, p: xx.x%}")
    expect_match(html_of(output$series_values), "unsaved", fixed = TRUE)
    session$setInputs(series_edit_3 = "{N: 0, mean: 1}", series_edit_2 = "SAF3")
    session$setInputs(series_save = 1L)
    got <- calls_of(f$st, "set")[[1L]]
    expect_identical(got$series, "tflspec_v1")
    expect_identical(got$values, list(population_safety = "SAF3", formats = list(N = 0L, mean = 1L)))
    expect_match(notes$all[length(notes$all)], "^message: Saved this study's values for tflspec_v1")
    expect_false(grepl("unsaved", html_of(output$series_values), fixed = TRUE))
    expect_setequal(names(rv$saved), c("population_safety", "formats"))
    # both back to the series: the source's values written into the copy
    session$setInputs(series_back = list(key = "formats", n = 2))
    session$setInputs(series_back = list(key = "population_safety", n = 3))
    expect_identical(rv$draft, stats::setNames(list(), character()))
    session$setInputs(series_save = 2L)
    got <- calls_of(f$st, "set")[[2L]]$values
    expect_identical(got[c("population_safety", "formats")],
                     list(population_safety = "SAF", formats = list(N = 0L, mean = 2L, p = "xx.x%")))
    expect_identical(f$st$copy$tflspec_v1$population_safety, "SAF")
    expect_identical(rv$saved, stats::setNames(list(), character()))
  })
})

test_that("only the study's own values are its draft: one change saves one key, a flag as true", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    session$setInputs(series_change = list(key = "total", n = 1))
    session$setInputs(series_edit_4 = "true")
    session$setInputs(series_save = 1L)
    got <- calls_of(f$st, "set")[[1L]]$values
    expect_identical(names(got), c("population_safety", "total"))
    expect_identical(unclass(got$total), "true")
    expect_true(f$st$copy$tflspec_v1$total)
    expect_setequal(names(rv$saved), c("population_safety", "total"))
    # one taken back: only it changes in the copy
    session$setInputs(series_back = list(key = "population_safety", n = 2))
    session$setInputs(series_save = 2L)
    expect_identical(f$st$copy$tflspec_v1$population_safety, "SAF")
    expect_true(f$st$copy$tflspec_v1$total)
    expect_identical(names(rv$saved), "total")
  })
})

test_that("tflspec's error is shown as it is, and a value that is not YAML stops before it", {
  f <- fake_series(withr::local_tempdir())
  f$st$copy$tflspec_v1$bad_key <- "x"
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    # a key of the copy the source does not have is shown, not dropped
    expect_match(html_of(output$series_values), "(not in this series)", fixed = TRUE)
    session$setInputs(series_save = 1L)
    expect_identical(notes$all[length(notes$all)], "error: tfl_series_values_set(): bad_key is refused.")
    n <- length(f$st$calls)
    session$setInputs(series_edit_2 = "[unclosed")
    session$setInputs(series_save = 2L)
    expect_match(notes$all[length(notes$all)], "^error: A value is not YAML:")
    expect_identical(length(f$st$calls), n)
  })
})

test_that("the study's copies: each one's state in words, differences and a copy again offered where it is not the source's", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    h <- html_of(output$series_copies)
    expect_match(h, "This study's copies (spec/series/)", fixed = TRUE)
    for (x in c("tflspec_v1/series.yml", "tflspec_v1/ae_soc_pt", "tflspec_v1/demog", "tflspec_v1/km_simple",
                "Same as the source", "The source has changed", "Changed in this study",
                "The source is missing", "2026-11-20 10:00:00", "/pkg/series/tflspec_v1/templates/demog.yml")) {
      expect_match(h, x, fixed = TRUE, info = x)
    }
    # the buttons: on the two templates whose copy is not the source's and
    # whose source is there
    expect_identical(lengths(regmatches(h, gregexpr("Show the differences", h, fixed = TRUE))), 2L)
    expect_identical(lengths(regmatches(h, gregexpr("Copy again from the source", h, fixed = TRUE))), 2L)
    expect_match(h, 'data-key="tflspec_v1/ae_soc_pt"', fixed = TRUE)
    expect_match(h, 'data-key="tflspec_v1/demog"', fixed = TRUE)
    expect_false(grepl('data-key="tflspec_v1/km_simple"', h, fixed = TRUE))
  })
  # the words for each state
  expect_identical(.series_status_label(c("same", "both_changed", "other")),
                   c("Same as the source", "Changed in both the source and this study", "other"))
  expect_match(as.character(.series_copies_ui(NULL)), "This study has no copies yet", fixed = TRUE)
})

test_that("the differences in a dialog; a copy again goes at once, or asks first when the study's edits would go", {
  f <- fake_series(withr::local_tempdir())
  local_series(f$fns)
  notes <- new.env()
  shown <- watch_modals()
  shiny::testServer(series_app(f$dir, notes), {
    session$flushReact()
    session$setInputs(series_diff = list(key = "tflspec_v1/demog", n = 1))
    expect_identical(calls_of(f$st, "diff")[[1L]]$template, "tflspec_v1/demog")
    m <- shown$modals[length(shown$modals)]
    expect_match(m, "Differences of tflspec_v1/demog", fixed = TRUE)
    expect_match(m, "Changed in this study", fixed = TRUE)
    expect_match(m, '<div class="rp-diff-copy">-  by: TRT01P</div>', fixed = TRUE)
    expect_match(m, '<div class="rp-diff-source">+  by: TRT01A</div>', fixed = TRUE)
    expect_match(m, "Copy again from the source", fixed = TRUE)
    # source changed only: copied again at once
    session$setInputs(series_recopy = list(key = "tflspec_v1/ae_soc_pt", n = 2))
    expect_identical(calls_of(f$st, "recopy")[[1L]]$template, "tflspec_v1/ae_soc_pt")
    expect_identical(notes$all[length(notes$all)], "message: Copied tflspec_v1/ae_soc_pt again from the source.")
    expect_identical(f$st$status$status[2L], "same")
    expect_false(grepl('data-key="tflspec_v1/ae_soc_pt"', html_of(output$series_copies), fixed = TRUE))
    # the study's own edits: asked first, nothing copied yet
    n <- length(shown$modals)
    session$setInputs(series_recopy = list(key = "tflspec_v1/demog", n = 3))
    expect_identical(length(calls_of(f$st, "recopy")), 1L)
    expect_identical(length(shown$modals), n + 1L)
    expect_match(shown$modals[n + 1L], "Copy tflspec_v1/demog again from the source?", fixed = TRUE)
    expect_match(shown$modals[n + 1L], "loses them", fixed = TRUE)
    session$setInputs(series_recopy_ok = 1L)
    expect_identical(calls_of(f$st, "recopy")[[2L]]$template, "tflspec_v1/demog")
    expect_identical(notes$all[length(notes$all)], "message: Copied tflspec_v1/demog again from the source.")
    expect_identical(f$st$status$status[3L], "same")
  })
  # no differences said as such
  expect_match(as.character(.series_diff_ui(c("--- copy", "+++ source", " a: 1"))), "No differences.", fixed = TRUE)
})

test_that("the Study tab has the card, and its texts have their Japanese", {
  expect_match(as.character(app_ui("en")), 'id="series_card"', fixed = TRUE)
  r_dir <- test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(r_dir, "srv_series.R")), "source not available (installed package)")
  code <- unlist(lapply(file.path(r_dir, c("srv_series.R", "series_ui.R", "template_report.R")), readLines,
                        encoding = "UTF-8", warn = FALSE))
  lits <- unlist(regmatches(code, gregexpr('\\bt\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE)))
  lits <- unique(eval(parse(text = paste0("c(", paste(sub("^t", "", lits), collapse = ","), ")"))))
  expect_gt(length(lits), 30L)
  s <- .strings()
  expect_identical(setdiff(lits, s$en), character())
  # (each with its Japanese: "unsaved" was English on the ja screen)
  expect_identical(s$en[s$en %in% lits & (is.na(s$ja) | !nzchar(s$ja))], character())
  expect_false(any(grepl("水準", s$ja[s$en %in% lits])))
  # the two actions in the user's words
  expect_identical(tr("Show the differences", "ja"), "差分を見る")
  expect_identical(tr("Copy again from the source", "ja"),
                   "ソースから取り直す")
})

test_that("the app's server starts the card on the open study", {
  local_home()
  two_studies()
  f <- fake_series(NULL)
  local_series(f$fns)
  suppressWarnings(shiny::testServer(server_for("S1"), {
    session$flushReact()
    expect_match(html_of(output$series_card), "series_list", fixed = TRUE)
    # (the series the study's copies are of; S1's folder has no copy of it
    # itself here: nothing to save)
    v <- html_of(output$series_values)
    expect_match(v, "Defaults shared by the tflspec_v1 series' templates (values:)", fixed = TRUE)
    expect_match(v, "This study has no copy of this series yet", fixed = TRUE)
    expect_match(html_of(output$series_copies), "tflspec_v1/demog", fixed = TRUE)
    # the template cards read from the open study
    expect_identical(.tpl_study_dir(), session$userData$rv$study$path)
  }))
})

# ---- with tflspec's own functions ----------------------------------------------------------

has_copies <- function() {
  all(c("tfl_copy_templates", "tfl_template_status", "tfl_series_values_set") %in%
        getNamespaceExports("tflspec"))
}

test_that("a template copied into a study: said once, the cards read the copy, the card lists and changes it", {
  skip_if_not(has_copies(), "this tflspec has no study copies (tflspec#879)")
  dir <- withr::local_tempdir()
  withr::defer(.tpl_study_dir(NULL))
  withr::defer(.arg_defs_reset())
  msg <- .tpl_copy_into_study(dir, "tflspec_v1/demog")
  expect_match(msg, "Copied the template tflspec_v1/demog into this study", fixed = TRUE)
  f <- .tpl_copy_file(dir, "tflspec_v1/demog")
  expect_true(file.exists(f))
  expect_true(file.exists(file.path(dir, "spec", "series", "tflspec_v1", "series.yml")))
  # a second time: there already, nothing said
  expect_null(.tpl_copy_into_study(dir, "tflspec_v1/demog"))
  # (a template of no series, or no study: nothing to copy)
  expect_null(.tpl_copy_into_study(dir, "demog"))
  expect_null(.tpl_copy_into_study(NULL, "tflspec_v1/demog"))
  # the cards: the study's copy once the study is open, its edits too
  lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
  at <- grep("^  by:$", lines)[1L]
  doc <- grep("^    description:", lines)
  doc <- doc[doc > at][1L]
  lines[doc] <- "    description: The arm the study's copy says."
  writeLines(lines, f, useBytes = TRUE)
  .arg_defs_reset()
  .tpl_study_dir(NULL)
  expect_false(identical(.tpl_params("tflspec_v1/demog")$by$doc, "The arm the study's copy says."))
  .tpl_study_dir(dir)
  expect_identical(.tpl_params("tflspec_v1/demog")$by$doc, "The arm the study's copy says.")
  # the card: the copy's state, its differences
  notes <- new.env()
  shown <- watch_modals()
  shiny::testServer(series_app(dir, notes), {
    session$flushReact()
    h <- html_of(output$series_copies)
    expect_match(h, "Changed in this study", fixed = TRUE)
    expect_match(h, 'data-key="tflspec_v1/demog"', fixed = TRUE)
    session$setInputs(series_diff = list(key = "tflspec_v1/demog", n = 1))
    expect_match(shown$modals[length(shown$modals)], "The arm the study&#39;s copy says.", fixed = TRUE)
    # a value of the study's copy, saved, read back
    expect_match(html_of(output$series_values), "Defaults shared by the tflspec_v1 series' templates (values:)", fixed = TRUE)
    session$setInputs(series_change = list(key = "total_label", n = 2))
    k <- union(names(tflspec::tfl_series_read("tflspec_v1")$values), names(rv$draft))
    do.call(session$setInputs, stats::setNames(list("All arms"), paste0("series_edit_", match("total_label", k))))
    session$setInputs(series_save = 1L)
    expect_identical(tflspec::tfl_series_read("tflspec_v1", dir = .series_copy_root(dir))$values$total_label,
                     "All arms")
    expect_identical(names(rv$saved), "total_label")
    # copied again, asked first: the study's edit of the template is gone
    session$setInputs(series_recopy = list(key = "tflspec_v1/demog", n = 3))
    session$setInputs(series_recopy_ok = 1L)
    expect_match(html_of(output$series_copies), "Same as the source", fixed = TRUE)
    expect_false(identical(.tpl_params("tflspec_v1/demog")$by$doc, "The arm the study's copy says."))
  })
})

test_that("the values' descriptions are in the app's language when tflplanner has them (user check 2026-11-19)", {
  ja <- function(x) tr(x, "ja")
  docs <- c(count_cell = "The cell of a count and its percent; % means ×100; no % sign is printed (e.g. 12 (34.5)).",
            max_rows = "The most rows a page holds.", style = "A site's own words.")
  h <- as.character(.series_values_ui(
    list(count_cell = "{n} ({p:.1f%})", max_rows = 30L, style = "s", total = FALSE),
    list(), list(), ja, docs))
  expect_match(h, "<th>説明</th>", fixed = TRUE)
  # count_cell: the % is x100, no sign printed
  expect_match(h, "記号は出ない（例: 12 (34.5)）", fixed = TRUE)
  expect_match(h, "1 ページ", fixed = TRUE)
  # a text tflplanner has no Japanese for: as written; a key with none: empty
  expect_match(h, "A site's own words.", fixed = TRUE)
  expect_match(h, '<td class="text-muted"></td>', fixed = TRUE)
  # no descriptions at all (an older tflspec): the column is there, empty
  h <- as.character(.series_values_ui(list(total = FALSE), list(), list(), identity, NULL))
  expect_match(h, "<th>Description</th>", fixed = TRUE)
  # the heading
  expect_identical(sprintf(ja("Defaults shared by the %s series' templates (values:)"), "tflspec_v1"),
                   "tflspec_v1 シリーズのテンプレートに共通の既定値 (values:)")
})
