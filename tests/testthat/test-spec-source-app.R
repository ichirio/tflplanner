# The app and the definition files changed outside it (#274 phase 2)

# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

spec_app_study <- function() {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  suppressMessages(create_study("SA-1", planner = p))
}

# an edit of T-1's file (spec/outputs/T-1.yml) in a text editor: its title
t1_file <- function(path) file.path(path, "spec", "outputs", "T-1.yml")
set_title <- function(path, value) {
  f <- t1_file(path)
  o <- tflspec::tfl_read_output(f)
  o$page$titles[["1"]]$center <- value
  tflspec::tfl_write_output(o, f)
}

test_that("files that do not read: the card, no save, write back", {
  local_home()
  s <- spec_app_study()
  writeLines(c("id: [", "kind"), t1_file(s$path))
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(rv$spec$status, "invalid")
    session$setInputs(nav = "study")
    h <- output$spec_card$html
    expect_match(h, "spec/outputs/T-1.yml", fixed = TRUE)
    expect_match(h, "spec_writeback", fixed = TRUE)
    # a save stops: nothing written, the fingerprints as they were
    before <- .read_state("SA-1")$files
    rv$p$outputs$description[1] <- "changed"
    session$setInputs(save = 1)
    expect_identical(.read_state("SA-1")$files, before)
    # written back from the last save: the study reads, the change unsaved
    session$setInputs(spec_writeback = 1)
    expect_identical(rv$spec$status, "same")
    expect_true(all(spec_status("SA-1")$status == "same"))
    expect_identical(rv$p$outputs$description[1], "changed")
    expect_length(list.files(file.path(s$path, "spec", ".rejected")), 1L)
  })
})

test_that("Reading the definition files again takes them in, unsaved changes merged", {
  local_home()
  s <- spec_app_study()
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(rv$spec$status, "same")
    # an unsaved change of another part, then an edit of the file
    rv$p$outputs$description[2] <- "mine"
    set_title(s$path, "From the file")
    session$setInputs(spec_reload = 1)
    expect_identical(rv$spec$status, "adopted")
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "From the file")
    expect_identical(rv$p$outputs$description[2], "mine")
    # saved: both
    session$setInputs(save = 1)
    o <- open_study("SA-1")
    expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "From the file")
    expect_identical(o$planner$outputs$description[2], "mine")
  })
})

test_that("a save after an outside edit asks to load first; the same part changed: keep which", {
  local_home()
  s <- spec_app_study()
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    rv$p <- set_sheet_rows(rv$p, "titles", "T-1",
                           data.frame(line = "1", center = "Mine"))
    set_title(s$path, "Theirs")
    session$setInputs(save = 1)
    # not saved: the file keeps the outside edit
    st <- spec_status("SA-1")
    expect_identical(st$status[st$file == "spec/outputs/T-1.yml"], "changed")
    session$setInputs(spec_reload_m = 1)
    expect_false(is.null(rv$spec_merge))
    session$setInputs(merge_mine = 1)
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "Mine")
    expect_null(rv$spec_merge)
  })
})

test_that("a draft made before the files changed is merged on open", {
  home <- local_home()
  s <- spec_app_study()
  d <- open_study("SA-1")
  d$planner$outputs$description[2] <- "drafted"
  .write_draft(d)
  set_title(s$path, "Edited while closed")
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(rv$spec$status, "adopted")
    # asked first (never silently); all parts taken in, then the draft offered
    expect_null(rv$draft)
    session$setInputs(adopt_parts = rv$spec$parts, adopt_ok = 1)
    # offered back: the draft over the files' version
    expect_false(is.null(rv$draft))
    session$setInputs(draft_restore = 1)
    expect_identical(rv$p$outputs$description[2], "drafted")
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "Edited while closed")
  })
})

test_that("Import in the app: the parts shown, the ones checked taken in, a broken copy refused", {
  home <- local_home()
  s <- spec_app_study()
  out <- file.path(home, "copy")
  export_spec_files("SA-1", out)
  g <- t1_file(out)
  set_title(out, "From the copy")
  up <- function(f) data.frame(name = basename(f), size = file.size(f),
                               type = "", datapath = f, stringsAsFactors = FALSE)
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    session$setInputs(spec_import = up(g))
    expect_true(rv$import$preview$ok)
    expect_identical(rv$import$preview$parts, "sheet:titles")
    session$setInputs(import_parts = "sheet:titles", import_ok = 1)
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "From the copy")
    # what was taken in stays on the Study tab
    expect_identical(rv$spec$status, "imported")
    expect_match(rv$spec$backup, "^spec/[.]backup/")
    expect_identical(open_study("SA-1")$spec$status, "same")
    expect_true(dir.exists(file.path(s$path, "spec", ".backup")))
    # a copy that does not read: nothing changes
    bad <- file.path(home, "T-1.yml")
    writeLines(c("id: [", "kind"), bad)
    rec <- .read_state("SA-1")$files
    session$setInputs(spec_import = up(bad))
    expect_false(rv$import$preview$ok)
    expect_identical(.read_state("SA-1")$files, rec)
  })
})

test_that("a change made outside the app is asked about on open: leave it, or take parts in", {
  local_home()
  s <- spec_app_study()
  set_title(s$path, "Outside")
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(rv$spec$status, "adopted")
    # nothing decided yet: the dialog is up, the files untouched
    expect_length(list.files(file.path(s$path, "spec", ".backup")), 0L)
    # leave the study as it was: the title is the old one again, and the
    # changed file is kept in spec/.backup
    session$setInputs(adopt_leave = 1)
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "Old title")
    expect_identical(rv$spec$status, "imported")
    expect_length(list.files(file.path(s$path, "spec", ".backup")), 1L)
    expect_true(all(spec_status("SA-1")$status == "same"))
  })
  expect_identical(sheet_rows(open_study("SA-1")$planner, "titles", "T-1")$center,
                   "Old title")
})

test_that("Restore from a backup: listed, shown, the study as it is now kept first", {
  home <- local_home()
  s <- spec_app_study()
  out <- file.path(home, "copy")
  export_spec_files("SA-1", out)
  set_title(out, "Second")
  import_spec_files("SA-1", out)
  expect_identical(nrow(list_spec_backups("SA-1")), 1L)
  b <- list_spec_backups("SA-1")$name
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "Second")
    session$setInputs(restore_open = 1)
    session$setInputs(restore_pick = b, restore_show = 1)
    expect_identical(rv$import$backup, b)
    expect_identical(rv$import$preview$parts, "sheet:titles")
    session$setInputs(import_parts = "sheet:titles", import_ok = 1)
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "Old title")
    # the study before the restore is kept too
    expect_identical(nrow(list_spec_backups("SA-1")), 2L)
  })
})
