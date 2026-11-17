# GUI check 1 (tflplanner#717): the template card's switch no default writes,
# a column field that lists the template's data, the ARD result and the
# builder note that follow the report open, and what [Make the ARD] reads again.

skip_on_cran()

# a study with tflspec's fictional ADaM data and a series template report
gc717_study <- function(home, id, args) {
  skip_if_not_installed("cards")
  need <- c("tfl_arg_defs", "tfl_ai_study_init")
  skip_if_not(all(need %in% getNamespaceExports("tflspec")), "this tflspec has no series templates")
  skip_if_not(tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")),
                       error = function(e) FALSE), "this tflspec has no series templates")
  d <- file.path(studies_root(home), id)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adtte")) {
    saveRDS(getExportedValue("tflspec", x), file.path(d, "adam", paste0(x, ".rds")))
  }
  invisible(tflspec::tfl_ai_study_init(d, adam = "adam", study_id = id))
  s <- suppressMessages(open_study(d, home = home))
  o <- tflspec::tfl_output("T-717-1", "table", template = list(name = "tflspec_v1/demog", args = args))
  s$planner <- suppressWarnings(.spec_add_output(s$planner, o))
  suppressMessages(save_study(s))
  id
}

test_that("a series template's columns are the data row's dataset's, not every dataset's", {
  skip_if_not(tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")),
                       error = function(e) FALSE), "this tflspec has no series templates")
  prm <- .tpl_params("tflspec_v1/demog")
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
    expect_match(paste(as.character(output$tpl_card$html), collapse = ""), "tpl_tflspec_v1/demog__total", fixed = TRUE)
    before <- .tpl_of(rv$p, "T-717-1")$args
    session$setInputs(`tpl_tflspec_v1/demog__total` = FALSE)
    expect_identical(.tpl_of(rv$p, "T-717-1")$args, before)
    expect_false("total" %in% names(.tpl_of(rv$p, "T-717-1")$args))
    # (a change to on is one)
    session$setInputs(`tpl_tflspec_v1/demog__total` = TRUE)
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

test_that("making a report's ARD reads it again for the builder and the content step", {
  local_home()
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$analyses <- data.frame(output_id = "DM", analysis_id = "A1",
                               method = "cards::ard_summary", variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  create_study("GC5", planner = p)
  # (the program's run and the rows are the ones of the ARD made; its meta is there already)
  local_mocked_bindings(
    update_study_ard = function(...) list(ok = TRUE, output = "made"),
    study_ard_rows = function(...) data.frame(analysis_id = "A1", stat = 1),
    ard_preview_state = function(...) "current")
  shiny::testServer(server_for("GC5"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "DM")
    v <- rv$ard_ver
    session$setInputs(ard_make_study = 1)
    expect_gt(rv$ard_ver, v)
  })
})
