# GUI check 1 (tflplanner#717): the template card's switch no default writes,
# a column field that lists the template's data, the ARD result and the
# builder note that follow the report open, and what [Make the ARD] reads again;
# below, each of the issue's eight items on today's screens.

skip_on_cran()

# a study with tflspec's fictional ADaM data and a series template report
gc717_study <- function(home, id, args) {
  skip_if_not_installed("cards")
  need <- c("tfl_arg_defs", "tfl_ai_study_init")
  skip_if_not(all(need %in% getNamespaceExports("tflspec")), "this tflspec has no series templates")
  skip_if_not(tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/tflspec_summary")),
                       error = function(e) FALSE), "this tflspec has no series templates")
  d <- file.path(studies_root(home), id)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adtte")) {
    saveRDS(getExportedValue("tflspec", x), file.path(d, "adam", paste0(x, ".rds")))
  }
  invisible(tflspec::tfl_ai_study_init(d, adam = "adam", study_id = id))
  s <- suppressMessages(open_study(d, home = home))
  # (copied into the study first, as the Add dialog does)
  invisible(tflspec::tfl_copy_templates(d, "tflspec_v1/tflspec_summary"))
  o <- tflspec::tfl_output("T-717-1", "table", template = list(name = "tflspec_v1/tflspec_summary", args = args))
  s$planner <- suppressWarnings(.spec_add_output(s$planner, o))
  suppressMessages(save_study(s))
  id
}

test_that("a series template's columns are the data row's dataset's, not every dataset's", {
  skip_if_not(tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/tflspec_summary")),
                       error = function(e) FALSE), "this tflspec has no series templates")
  prm <- .tpl_params("tflspec_v1/tflspec_summary")
  data <- list(ADSL = data.frame(USUBJID = 1, TRT01A = "A", AGE = 1),
               ADAE = data.frame(USUBJID = 1, AEDECOD = "x"))
  expect_identical(.tpl_vars_dataset(prm$by, data), "ADSL")
  expect_identical(.tpl_vars_dataset(prm$continuous, data), "ADSL")
  expect_identical(.tpl_columns(data, .tpl_vars_dataset(prm$by, data)), c("USUBJID", "TRT01A", "AGE"))
  # (a parameter that names no data, or a dataset the study lacks: none, every column)
  expect_null(.tpl_vars_dataset(prm$total, data))
  expect_null(.tpl_vars_dataset(prm$by, list(ADAE = data$ADAE)))
  expect_setequal(.tpl_columns(data, NULL), c("USUBJID", "TRT01A", "AGE", "AEDECOD"))
})

test_that("a template card's switch with no default is drawn off: its first FALSE is no edit", {
  home <- local_home()
  id <- gc717_study(home, "G717", list(by = "TRT01A", continuous = "AGE"))
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(target = "T-717-1", nav = "make", step = "content", content_nav = "content")
    expect_match(tpl_cards_html(output), "tpl_tflspec_v1_tflspec_summary__total", fixed = TRUE)
    before <- .tpl_of(rv$p, "T-717-1")$args
    session$setInputs(`tpl_tflspec_v1_tflspec_summary__total` = FALSE)
    expect_identical(.tpl_of(rv$p, "T-717-1")$args, before)
    expect_false("total" %in% names(.tpl_of(rv$p, "T-717-1")$args))
    # (a change to on is one)
    session$setInputs(`tpl_tflspec_v1_tflspec_summary__total` = TRUE)
    expect_true(isTRUE(.tpl_of(rv$p, "T-717-1")$args$total))
  })
})

test_that("the ARD result kept for one report is not shown under another", {
  local_home()
  s <- create_study("GC3")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  adsl <- cards::ADSL
  saveRDS(adsl, file.path(s$path, "data/adam/adsl.rds"))
  for (id in c("T1", "T2")) {
    s$planner <- first_table(s$planner, id, "data/adam/adsl.rds", adsl, "SAFFL", "ARM", c("AGE", "SEX"))
  }
  save_study(s)
  shiny::testServer(server_for("GC3"), {
    session$setInputs(nav = "make", step = "ard", target = "T1", ard_preview = 1)
    info <- function() paste(output$ard_run_info$html, collapse = "")
    expect_match(info(), "T1: [0-9]+ rows", perl = TRUE)
    session$setInputs(target = "T2")
    expect_false(grepl("T1: ", info(), fixed = TRUE))
    expect_match(info(), "Update preview to see the ARD", fixed = TRUE)
    session$setInputs(target = "T1")
    expect_match(info(), "T1: [0-9]+ rows", perl = TRUE)
  })
})

# ---- the eight items of #717, each on today's screens ------------------------------------
# (the sample study, driven through testServer() as a user would)

# what the server said and showed, instead of the screen: the notes, the
# dialogs (as text) and the progress notices opened and closed
gc717_watch <- function(env = parent.frame()) {
  shown <- new.env()
  shown$notes <- list()
  shown$modals <- character()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown$notes[[length(shown$notes) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                      type = type)
      invisible("id")
    },
    showModal = function(ui, session = NULL) {
      shown$modals <- c(shown$modals, paste(as.character(ui), collapse = ""))
      invisible()
    },
    .package = "shiny", .env = env)
  shown
}
gc717_text <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = "")))
gc717_sample <- function(id = "G717S", env = parent.frame()) {
  skip_if_quick()  # slow: the sample study and a server each
  home <- local_home(env)
  suppressMessages(create_sample_study(run = FALSE, home = home, study_id = id))
  id
}
# the progress notices a session opens and closes (withProgress(): its
# "...making its ARD" detail), recorded on the session itself
gc717_progress <- function(session) {
  log <- new.env()
  log$open <- character()
  log$closed <- character()
  unlockBinding("sendProgress", session)
  session$sendProgress <- function(type, message) {
    if (identical(type, "open")) log$open <- c(log$open, message$id)
    if (identical(type, "close")) log$closed <- c(log$closed, message$id)
    invisible()
  }
  log
}

test_that("#717 item 1: a series template's missing required arguments give one note naming them, and no ARD run", {
  # (the built-in demog's: test-tpl-args-missing-toast.R, #813.  This is the
  # same rule for the series template the screen check used; it holds once
  # tflspec says which arguments tflspec_v1/demog needs -- the PR's open point)
  skip_if_quick()
  home <- local_home()
  id <- gc717_study(home, "G717A", list(by = "TRT01A"))
  p <- open_study(id)$planner
  e <- tryCatch(tflspec::tfl_resolve_output(p$spec$outputs[["T-717-1"]], p$spec$study), error = identity)
  skip_if_not(inherits(e, "tflspec_template_args_missing"),
              "tflspec_v1/demog with `by` only is no tflspec_template_args_missing error (tflplanner#717 open point)")
  shown <- gc717_watch()
  ran <- 0L
  local_mocked_bindings(fetch_ard = function(...) {
    ran <<- ran + 1L
    stop("not to be run")
  })
  shiny::testServer(server_for(id), {
    session$setInputs(nav = "make", step = "ard", target = "T-717-1", ard_preview = 1)
    errs <- Filter(function(n) identical(n$type, "error"), shown$notes)
    expect_length(errs, 1L)
    for (a in e$args) expect_match(errs[[1L]]$text, sprintf("(%s)", a), fixed = TRUE)
    expect_identical(ran, 0L)
  })
})

test_that("#717 item 2: Add from a series template writes only what was filled in; the untouched switch is no edit", {
  id <- gc717_sample("G717B")
  shown <- gc717_watch()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    nm <- "tflspec_v1/demog"
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = nm,
                      modal_id = "ZZ-717", modal_desc = "", modal_section = "",
                      mtpl_tflspec_v1_demog__population = "SAF", mtpl_tflspec_v1_demog__by = "TRT01A",
                      mtpl_tflspec_v1_demog__continuous = "AGE",
                      # (the dialog's switch, left as drawn: off)
                      mtpl_tflspec_v1_demog__total = FALSE)
    suppressMessages(session$setInputs(add_ok = 1))
    expect_false(is.null(.tpl_of(rv$p, "T-ZZ-717")))
    expect_false("total" %in% names(.tpl_of(rv$p, "T-ZZ-717")$args))
    # the card, drawn and echoed back by the page: still no edit, no "This table", no Revert
    session$setInputs(target = "T-ZZ-717", nav = "make", step = "ard", content_nav = "content")
    html <- suppressMessages(tpl_cards_html(output))
    expect_match(html, "tpl_tflspec_v1_demog__total", fixed = TRUE)
    session$setInputs(`tpl_tflspec_v1_demog__total` = FALSE)
    expect_false("total" %in% names(.tpl_of(rv$p, "T-ZZ-717")$args))
    html <- suppressMessages(tpl_cards_html(output))
    at <- regexpr("tpl_tflspec_v1_demog__total", html, fixed = TRUE)
    total <- substr(html, at, at + 1500L)
    expect_false(grepl("rp-tpl-badge-own", total, fixed = TRUE))
    expect_false(grepl("tpl_revert', 'total'", html, fixed = TRUE))
  })
})

test_that("#717 items 3 and 4: the right pane and the analysis-data form follow the report open (sample study)", {
  id <- gc717_sample("G717C")
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    # (the screen check's two reports: both read adsl_saf)
    a <- "T-14-1-1"
    b <- "T-14-1-2"
    session$setInputs(nav = "make", step = "ard", target = a, ard_preview = 1)
    info <- function() gc717_text(output$ard_run_info$html)
    expect_match(info(), paste0(a, ": [0-9]+ rows"))
    open_report(session, b)
    expect_false(grepl(paste0(a, ": "), info(), fixed = TRUE))
    open_report(session, a)
    expect_match(info(), paste0(a, ": [0-9]+ rows"))
    # the analysis-data form: opened on one report, an edit not saved, then
    # the other report: its form is gone, and what the page sends on (the
    # form's fields, a Save) writes nothing
    session$setInputs(ard_adata_pick = "adsl_saf")
    expect_match(paste(output$adata_detail$html, collapse = ""), "adata_id", fixed = TRUE)
    expect_match(gc717_text(output$adata_detail$html), a, fixed = TRUE)
    session$setInputs(adata_distinct = "USUBJID")
    open_report(session, b)
    expect_false(grepl("adata_id", paste(output$adata_detail$html, collapse = ""), fixed = TRUE))
    before <- rv$p$ard
    session$setInputs(adata_distinct = c("USUBJID", "AGE"), adata_id = "zz_717")
    session$setInputs(adata_save = 1)
    expect_identical(rv$p$ard, before)
    # (b's own form is b's)
    session$setInputs(ard_adata_pick = "adsl_saf")
    txt <- gc717_text(output$adata_detail$html)
    expect_match(txt, b, fixed = TRUE)
  })
})

test_that("#717 item 5: after the ARD is made, the content step reads it (no 'no ARD yet')", {
  id <- gc717_sample("G717D")
  shiny::testServer(server_for(id), {
    tb <- "T-14-1-1C"
    session$setInputs(nav = "make", target = tb, step = "content", content_nav = "content")
    expect_match(gc717_text(output$builder_note$html), "has no ARD yet", fixed = TRUE)
    session$setInputs(step = "ard", ard_preview = 1)
    session$setInputs(step = "content")
    expect_false(grepl("has no ARD yet", gc717_text(output$builder_note$html), fixed = TRUE))
  })
})

test_that("#717 item 6: no English from the series templates in the Japanese UI (titles, argument help)", {
  ser <- .tpl_series_templates()
  skip_if(!nrow(ser), "this tflspec has no series templates")
  # every series template's title in the Add dialog, every argument's (i)
  for (nm in ser$name) {
    d <- .tpl_desc_of(nm, ser)
    if (nzchar(d)) expect_false(identical(tr(d, "ja"), d), info = nm)
    for (prm in .tpl_params(nm)) {
      h <- .tpl_doc_shown(prm, function(x) tr(x, "ja"), "ja")
      if (nzchar(h)) expect_false(identical(h, prm$doc), info = paste(nm, prm$name))
    }
  }
  # tflspec's Japanese help (`help: {en, ja}`) is the one shown in Japanese
  prm <- list(name = "x", doc = "An analysis id for each variable; empty for the variable's name.",
              doc_ja = "\u30c6\u30b9\u30c8")
  expect_identical(.tpl_doc_shown(prm, function(x) tr(x, "ja"), "ja"), "\u30c6\u30b9\u30c8")
  expect_identical(.tpl_doc_shown(prm, identity, "en"), prm$doc)
  defs <- tflspec::tfl_arg_defs(template = ser$name[1L])
  defs$help_ja[1L] <- "\u30d8\u30eb\u30d7"
  expect_identical(.tpl_params_from_defs(defs)[[1L]]$doc_ja, "\u30d8\u30eb\u30d7")
})

test_that("#717 item 6: the Add dialog in Japanese shows no series template's English title", {
  id <- gc717_sample("G717E")
  shown <- gc717_watch()
  local_mocked_bindings(tflplanner_language = function() "ja")
  ser <- .tpl_series_templates()
  shiny::testServer(server_for(id), {
    session$setInputs(add = 1)
    dlg <- shown$modals[length(shown$modals)]
    for (d in setdiff(ser$label, "")) expect_false(grepl(d, dlg, fixed = TRUE), info = d)
  })
})

test_that("#717 item 7: a series template's column field lists its data's columns only (demog: ADSL)", {
  id <- gc717_sample("G717F")
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    data <- .tpl_data_cols(rv$study$path, rv$p)
    skip_if(!all(c("ADSL", "ADAE") %in% names(data)), "the sample has no ADSL and ADAE")
    only_ae <- setdiff(names(data$ADAE), names(data$ADSL))
    prm <- .tpl_params("tflspec_v1/demog")
    for (a in c("by", "continuous", "categorical")) {
      ds <- .tpl_vars_dataset(prm[[a]], data)
      expect_identical(ds, "ADSL", info = a)
      expect_length(intersect(.tpl_columns(data, ds), only_ae), 0L)
    }
  })
})

test_that("#717 item 8: the notice of making the ARD is closed when the work ends, also on an error", {
  id <- gc717_sample("G717G")
  test_frame <- environment()  # (a mock inside testServer() is undone with this test)
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    pg <- gc717_progress(session)
    session$setInputs(nav = "make", step = "ard", target = "T-14-1-1C", ard_preview = 1)
    expect_gt(length(pg$open), 0L)
    expect_setequal(pg$closed, pg$open)
    # again, made to fail: no notice is left either, and none piles up
    n <- length(pg$open)
    testthat::local_mocked_bindings(fetch_ard = function(...) stop("made to fail"),
                                    .package = "tflplanner", .env = test_frame)
    suppressWarnings(session$setInputs(ard_preview = 2))
    expect_setequal(pg$closed, pg$open)
    session$setInputs(ard_preview = 3)
    expect_setequal(pg$closed, pg$open)
  })
})
