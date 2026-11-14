# The branches of R/srv_first_table.R the other tests leave out: the report
# list's copy, rename, delete, move and sort buttons, and the data folder's
# grid, note and upload -- driven through shiny::testServer() on the whole
# app, as the other server tests do.

skip_if_quick()  # slow: not in the quick check of a PR into next (helper.R)

# what the server showed, instead of the screen: the notes and the dialogs
# (a dialog's title and body as text)
watch_screen <- function(env = parent.frame()) {
  shown <- new.env()
  shown$notes <- list()
  shown$modals <- list()
  shown$removed <- 0L
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown$notes[[length(shown$notes) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                      type = type)
      invisible("id")
    },
    showModal = function(ui, session = NULL) {
      shown$modals[[length(shown$modals) + 1L]] <- as.character(ui)
      invisible()
    },
    removeModal = function(session = NULL) {
      shown$removed <- shown$removed + 1L
      invisible()
    },
    .package = "shiny", .env = env)
  shown
}

note_texts <- function(shown) vapply(shown$notes, function(n) n$text, "")
last_modal <- function(shown) shown$modals[[length(shown$modals)]]

three_reports_study <- function(id = "FT") {
  p <- add_output(new_planner(), "T-1", description = "one", at = "end")
  p <- add_output(p, "T-2", description = "two", at = "end")
  p <- add_output(p, "T-3", description = "three", at = "end")
  create_study(id, planner = p)
}

ids <- function(p) p$outputs$output_id

# ---- copy ------------------------------------------------------------------------------

test_that("Copy with no report chosen says to choose one and asks for no ID", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", copy = 1)
    expect_identical(note_texts(shown), "Choose a report")
    expect_identical(shown$notes[[1L]]$type, "warning")
    expect_length(shown$modals, 0L)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
  })
})

test_that("Copy asks for the new ID, and the copy goes beside its original and is chosen", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", target = "T-2", copy = 1)
    expect_match(last_modal(shown), "Copy T-2", fixed = TRUE)
    # the ID offered is the original's with _2
    expect_match(last_modal(shown), 'value="T-2_2"', fixed = TRUE)
    session$setInputs(modal_id = "  T-2b ", copy_ok = 1)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-2b", "T-3"))
    expect_identical(rv$p$outputs$description[ids(rv$p) == "T-2b"], "two")
    expect_gte(shown$removed, 1L)
  })
})

test_that("Copy to an ID that exists changes nothing and says why", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", target = "T-2", copy = 1)
    suppressMessages(session$setInputs(modal_id = "T-3", copy_ok = 1))
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
    expect_true(any(grepl("already exists", note_texts(shown), fixed = TRUE)))
    expect_identical(shown$notes[[length(shown$notes)]]$type, "error")
  })
})

# ---- rename ----------------------------------------------------------------------------

test_that("Rename asks for the new ID, and the report keeps its place and its content under it", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", rename = 1)
    expect_identical(note_texts(shown), "Choose a report")
    session$setInputs(target = "T-2", rename = 2)
    expect_match(last_modal(shown), "Rename T-2", fixed = TRUE)
    expect_match(last_modal(shown), 'value="T-2"', fixed = TRUE)
    session$setInputs(modal_id = "T-20", rename_ok = 1)
    expect_identical(ids(rv$p), c("T-1", "T-20", "T-3"))
    expect_identical(rv$p$outputs$description[2L], "two")
  })
})

test_that("Rename to an ID that exists changes nothing and says why", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", target = "T-1", rename = 1)
    suppressMessages(session$setInputs(modal_id = "T-3", rename_ok = 1))
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
    expect_true(any(grepl("already exists", note_texts(shown), fixed = TRUE)))
  })
})

# ---- delete ----------------------------------------------------------------------------

test_that("Delete asks first, names the report, and removes it only on OK", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", remove = 1)
    expect_identical(note_texts(shown), "Choose a report")
    expect_length(shown$modals, 0L)
    session$setInputs(target = "T-2", remove = 2)
    expect_match(last_modal(shown), "Delete T-2", fixed = TRUE)
    expect_match(last_modal(shown), "program and outputs stay on disk", fixed = TRUE)
    expect_false(grepl("print numbers from its ARD", last_modal(shown), fixed = TRUE))
    # nothing is deleted until it is confirmed
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
    session$setInputs(remove_ok = 1)
    expect_identical(ids(rv$p), c("T-1", "T-3"))
  })
})

test_that("Delete warns of the figures that read the table's ARD", {
  local_home()
  p <- add_output(new_planner(), "T-1", at = "end")
  p <- add_output(p, "F-1", type = "figure", at = "end")
  p$sheets$report$ard_source <- ifelse(p$sheets$report$output_id == "F-1", "table:T-1", NA)
  create_study("FT", planner = p)
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    session$setInputs(nav = "outputs", target = "T-1", remove = 1)
    expect_match(last_modal(shown), "Delete T-1", fixed = TRUE)
    expect_match(last_modal(shown), "F-1 print numbers from its ARD", fixed = TRUE)
  })
})

# ---- move ------------------------------------------------------------------------------

test_that("the up and down arrows move the chosen report one place, and stop at the ends", {
  local_home()
  three_reports_study()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    # no report chosen: nothing moves
    session$setInputs(nav = "outputs", up = 1, down = 1)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
    session$setInputs(target = "T-2", up = 2)
    expect_identical(ids(rv$p), c("T-2", "T-1", "T-3"))
    expect_identical(rv$p$outputs$description, c("two", "one", "three"))
    # at the top already
    session$setInputs(up = 3)
    expect_identical(ids(rv$p), c("T-2", "T-1", "T-3"))
    session$setInputs(down = 2)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
    session$setInputs(down = 3)
    expect_identical(ids(rv$p), c("T-1", "T-3", "T-2"))
    # at the bottom already
    session$setInputs(down = 4)
    expect_identical(ids(rv$p), c("T-1", "T-3", "T-2"))
  })
})

# ---- sort ------------------------------------------------------------------------------

test_that("Sort by ID with the reports in order already says so and asks nothing", {
  local_home()
  three_reports_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", sort_ids = 1)
    expect_identical(note_texts(shown), "The reports are in their ids' order already.")
    expect_length(shown$modals, 0L)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-3"))
  })
})

test_that("Sort by ID says how many reports move before it moves them", {
  local_home()
  p <- add_output(new_planner(), "T-10", at = "end")
  p <- add_output(p, "T-2", at = "end")
  p <- add_output(p, "T-1", at = "end")
  create_study("FT", planner = p)
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", sort_ids = 1)
    expect_match(last_modal(shown), "Sort the reports by ID?", fixed = TRUE)
    # T-10 T-2 T-1 -> T-1 T-2 T-10: T-2 stays
    expect_match(last_modal(shown), "2 of 3 reports move", fixed = TRUE)
    session$setInputs(sort_ids_ok = 1)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-10"))
    expect_gte(shown$removed, 1L)
  })
})

# ---- the data folder ---------------------------------------------------------------------

# a catalog with one dataset row, drawn as the Data tab draws it
catalog_study <- function(id = "FT") {
  p <- add_output(new_planner(), "T-1", at = "end")
  p <- set_ard_rows(p, "datasets", "",
                    data.frame(dataset = "ADSL", level = "ADaM",
                               path = "data/adam/adsl.rds", derive = NA_character_,
                               stringsAsFactors = FALSE))
  create_study(id, planner = p)
}

# what the datasets grid sends back for a change of one cell, or for a row
# added (a blank one at the end)
datasets_grid_event <- function(session, output, key = NULL, set = NULL, add_row = FALSE) {
  w <- jsonlite::fromJSON(output[["hot_ard_datasets"]], simplifyVector = FALSE)$x
  hdr <- unlist(w$colHeaders)
  data <- w$data
  ch <- list()
  event <- "afterChange"
  if (!is.null(set)) {
    old <- data[[1L]][[match(names(set), hdr)]]
    data[[1L]][[match(names(set), hdr)]] <- set[[1L]]
    ch <- list(list(0L, names(set), old, set[[1L]]))
  }
  if (add_row) {
    data[[length(data) + 1L]] <- as.list(rep("", length(hdr)))
    event <- "afterCreateRow"
  }
  session$setInputs(hot_ard_datasets = list(
    data = data,
    changes = list(event = event, changes = if (length(ch)) ch else NULL),
    params = list(planner_key = key %||% w$planner_key, rClass = "data.frame",
                  rColHeaders = as.list(hdr), rColClasses = w$rColClasses,
                  rDataDim = list(length(data), length(hdr)))))
}

test_that("the datasets grid shows the catalog row with its file's size, and the folder's pill", {
  local_home()
  catalog_study()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "data", data_pill = "adam")
    w <- jsonlite::fromJSON(output[["hot_ard_datasets"]], simplifyVector = FALSE)$x
    hdr <- unlist(w$colHeaders)
    expect_true("size_kb" %in% hdr)
    expect_identical(w$data[[1L]][[match("dataset", hdr)]], "ADSL")
    # the file is not there
    expect_identical(w$data[[1L]][[match("size_kb", hdr)]], "no file")
    # the folder is said under the grid
    expect_match(output$data_folder_where$html, "data/adam/", fixed = TRUE)
  })
})

test_that("a dataset row with a file shows its size in KB, and a file outside the study folder is said", {
  local_home()
  catalog_study()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    dir.create(file.path(rv$study$path, "data", "adam"), recursive = TRUE, showWarnings = FALSE)
    saveRDS(data.frame(USUBJID = "S-1"), file.path(rv$study$path, "data", "adam", "adsl.rds"))
    elsewhere <- withr_tempdir()
    saveRDS(data.frame(USUBJID = "S-2"), file.path(elsewhere, "adae.rds"))
    rv$p <- set_ard_rows(rv$p, "datasets", "",
                         data.frame(dataset = c("ADSL", "ADAE"), level = "ADaM",
                                    path = c("data/adam/adsl.rds",
                                             file.path(elsewhere, "adae.rds")),
                                    derive = NA_character_, stringsAsFactors = FALSE))
    session$setInputs(nav = "data", data_pill = "adam")
    w <- jsonlite::fromJSON(output[["hot_ard_datasets"]], simplifyVector = FALSE)$x
    hdr <- unlist(w$colHeaders)
    size <- vapply(w$data, function(r) r[[match("size_kb", hdr)]], "")
    expect_match(size[[1L]], "^[0-9]+[.][0-9]$")
    expect_match(size[[2L]], "^[0-9]+[.][0-9]$")
    where <- vapply(w$data, function(r) r[[match("where", hdr)]], "")
    expect_identical(where, c("", "outside the study folder"))
    # and the folder outside is named under the grid
    expect_match(output$catalog_files_note$html, "Outside the study folder:", fixed = TRUE)
    expect_match(output$catalog_files_note$html, normalizePath(elsewhere, "/", mustWork = FALSE),
                 fixed = TRUE)
  })
})

test_that("an edit in the datasets grid is written to the catalog, a stale grid's is not", {
  local_home()
  catalog_study()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "data", data_pill = "adam")
    # a grid drawn for another folder (its key is not this one's): ignored
    datasets_grid_event(session, output, key = "datasets|sdtm|0|0|0",
                        set = c(dataset = "OLD"))
    expect_identical(ard_rows(rv$p, "datasets", "")$dataset, "ADSL")
    datasets_grid_event(session, output, set = c(dataset = "ADSL2"))
    ds <- ard_rows(rv$p, "datasets", "")
    expect_identical(ds$dataset, "ADSL2")
    expect_identical(ds$path, "data/adam/adsl.rds")
    # the display-only columns are not written back
    expect_false(any(c("size_kb", "where") %in% names(rv$p$ard$datasets)))
  })
})

test_that("a row added in the ADaM folder's grid is of level ADaM; an unreadable change is refused", {
  local_home()
  catalog_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "data", data_pill = "adam")
    datasets_grid_event(session, output, add_row = TRUE)
    ds <- ard_rows(rv$p, "datasets", "")
    expect_identical(ds$dataset[1L], "ADSL")
    if (nrow(ds) > 1L) expect_identical(ds$level[-1L], rep("ADaM", nrow(ds) - 1L))
    # a change the grid library cannot rebuild the table from: said, the
    # catalog as it was
    before <- rv$p$ard$datasets
    key <- jsonlite::fromJSON(output[["hot_ard_datasets"]], simplifyVector = FALSE)$x$planner_key
    session$setInputs(hot_ard_datasets = list(
      data = list(list("x")),
      changes = list(event = "afterChange", changes = list(list(0L, "dataset", "a", "b"))),
      params = list(planner_key = key, rClass = "data.frame",
                    rColHeaders = list("a", "b", "c"), rColClasses = list(a = "character"),
                    rDataDim = list(1L, 3L))))
    expect_true(any(grepl("could not be taken in", note_texts(shown), fixed = TRUE)))
    expect_identical(rv$p$ard$datasets, before)
  })
})

test_that("a grid event with no change that is not a row added or removed writes nothing", {
  local_home()
  catalog_study()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "data", data_pill = "adam")
    before <- rv$p
    datasets_grid_event(session, output)
    expect_identical(rv$p, before)
  })
})

test_that("the note under the grid names the files with no row, and the dataset names that are there twice", {
  local_home()
  catalog_study()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    dir.create(file.path(rv$study$path, "data", "adam"), recursive = TRUE, showWarnings = FALSE)
    saveRDS(data.frame(a = 1), file.path(rv$study$path, "data", "adam", "adae.rds"))
    saveRDS(data.frame(a = 1), file.path(rv$study$path, "data", "adam", "adsl.rds"))
    rv$p <- set_ard_rows(rv$p, "datasets", "",
                         data.frame(dataset = c("ADSL", "ADSL"), level = "ADaM",
                                    path = c("data/adam/adsl.rds", NA),
                                    derive = NA_character_, stringsAsFactors = FALSE))
    session$setInputs(nav = "data", data_pill = "adam", data_refresh = 1)
    h <- output$catalog_files_note$html
    # (Refresh makes the rows: the file with no row yet is named)
    expect_match(h, "A dataset name is in the catalog twice: ADSL", fixed = TRUE)
  })
})

test_that("files added in the Data tab are copied into the folder and get a catalog row", {
  local_home()
  catalog_study()
  shown <- watch_screen()
  shiny::testServer(server_for("FT"), {
    rv <- session$userData$rv
    session$setInputs(nav = "data", data_pill = "adam")
    up <- file.path(withr_tempdir(), "upload")
    dir.create(up)
    saveRDS(data.frame(USUBJID = "S-1"), file.path(up, "adae.rds"))
    writeLines("x", file.path(up, "readme.txt"))
    session$setInputs(data_upload = data.frame(
      name = c("adae.rds", "readme.txt"),
      datapath = file.path(up, c("adae.rds", "readme.txt")),
      stringsAsFactors = FALSE))
    dest <- file.path(rv$study$path, "data", "adam")
    expect_true(file.exists(file.path(dest, "adae.rds")))
    expect_true(file.exists(file.path(dest, "readme.txt")))
    expect_true(any(grepl("2 files added to data/adam", note_texts(shown), fixed = TRUE)))
    # the data file has its row now, the text file does not
    ds <- ard_rows(rv$p, "datasets", "")
    expect_true("ADAE" %in% ds$dataset)
    expect_identical(ds$path[ds$dataset == "ADAE"], "data/adam/adae.rds")
    expect_false(any(grepl("readme", ds$path)))
    expect_true(any(grepl("In the data catalog now: ADAE", note_texts(shown), fixed = TRUE)))
  })
})
