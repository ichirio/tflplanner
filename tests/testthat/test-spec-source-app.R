# The app and the definition files changed outside it (#274 phase 2)

spec_app_study <- function() {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  suppressMessages(create_study("SA-1", planner = p))
}

set_title <- function(path, value) {
  f <- file.path(path, "spec", "report_spec.xlsx")
  wb <- openxlsx::loadWorkbook(f)
  d <- openxlsx::read.xlsx(wb, "titles")
  d$center[1] <- value
  openxlsx::writeData(wb, "titles", d)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
}

test_that("files that do not read: the card, no save, write back", {
  local_home()
  s <- spec_app_study()
  writeLines("not a workbook", file.path(s$path, "spec", "report_spec.xlsx"))
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(rv$spec$status, "invalid")
    session$setInputs(nav = "study")
    h <- output$spec_card$html
    expect_match(h, "spec/report_spec.xlsx", fixed = TRUE)
    expect_match(h, "spec_writeback", fixed = TRUE)
    # a save stops: nothing written, the fingerprints as they were
    before <- .read_state("SA-1")$files
    rv$p$outputs$description[1] <- "changed"
    session$setInputs(save = 1)
    expect_identical(.read_state("SA-1")$files, before)
    # written back from the last save: the study reads, the change unsaved
    session$setInputs(spec_writeback = 1)
    expect_identical(rv$spec$status, "same")
    expect_identical(spec_status("SA-1")$status, rep("same", 3L))
    expect_identical(rv$p$outputs$description[1], "changed")
    expect_length(list.files(file.path(s$path, "spec", ".rejected")), 1L)
  })
})

test_that("Load from the definition files takes them in, unsaved changes merged", {
  local_home()
  s <- spec_app_study()
  shiny::testServer(server_for("SA-1"), {
    rv <- session$userData$rv
    expect_identical(rv$spec$status, "same")
    # an unsaved change of another part, then an Excel edit
    rv$p$outputs$description[2] <- "mine"
    set_title(s$path, "From Excel")
    session$setInputs(spec_load = 1)
    expect_identical(rv$spec$status, "adopted")
    expect_identical(sheet_rows(rv$p, "titles", "T-1")$center, "From Excel")
    expect_identical(rv$p$outputs$description[2], "mine")
    # saved: both
    session$setInputs(save = 1)
    o <- open_study("SA-1")
    expect_identical(sheet_rows(o$planner, "titles", "T-1")$center, "From Excel")
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
    expect_identical(st$status[st$file == "spec/report_spec.xlsx"], "changed")
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
  g <- file.path(out, "spec", "report_spec.xlsx")
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
    expect_identical(open_study("SA-1")$spec$status, "same")
    expect_true(dir.exists(file.path(s$path, "spec", ".backup")))
    # a copy that does not read: nothing changes
    bad <- file.path(home, "report_spec.xlsx")
    writeLines("not a workbook", bad)
    rec <- .read_state("SA-1")$files
    session$setInputs(spec_import = up(bad))
    expect_false(rv$import$preview$ok)
    expect_identical(.read_state("SA-1")$files, rec)
  })
})
