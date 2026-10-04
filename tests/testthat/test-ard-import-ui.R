test_that("the record lists the reports that use each ARD taken in", {
  rep <- data.frame(output_id = c("T1", "T2", "T3"),
                    ard_source = c("import:a.rds", NA, "import:a.rds"))
  expect_identical(.imports_used_by(rep, "a.rds"), c("T1", "T3"))
  expect_identical(.imports_used_by(rep, "b.rds"), character())
  expect_identical(.imports_used_by(data.frame(output_id = "T1"), "a.rds"), character())
  imp <- data.frame(import_id = c("IMP001", "IMP002"), file = c("a.rds", "b.rds"))
  v <- .imports_view(imp, rep)
  expect_identical(v$import_id, c("IMP002", "IMP001"))
  expect_identical(v$used_by, c("", "T1, T3"))
})

test_that("ARDs are taken in, used, compared, replaced and removed on the ARD tab", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("IM")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  adsl <- as.data.frame(cards::ADSL)
  saveRDS(adsl, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", adsl,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  save_study(s)
  run_ard(s)
  # the report's own ARD, written out as another program would hand it over
  own <- study_ard_rows(s, "T1")
  skip_if(is.null(own) || !nrow(own))
  f1 <- file.path(tempdir(), "t1_cro.rds")
  f2 <- file.path(tempdir(), "t1_cro_v2.rds")
  saveRDS(own, f1)
  saveRDS(own, f2)
  shiny::testServer(server_for("IM"), {
    rv <- session$userData$rv
    session$setInputs(nav = "ard", target = "T1")
    # take it in, for T1, and use it
    session$setInputs(imp_new = 1)
    session$setInputs(imp_file = data.frame(name = "t1_cro.rds", datapath = f1,
                                            stringsAsFactors = FALSE),
                      imp_source = "CRO-A", imp_outputs = "T1", imp_use_it = TRUE)
    session$setInputs(imp_do = 1)
    d <- ard_imports(rv$study)
    expect_identical(d$file, "t1_cro.rds")
    expect_identical(d$source, "CRO-A")
    expect_identical(d$state, "in use")
    expect_identical(.ard_import_of(rv$p, "T1"), "t1_cro.rds")
    expect_identical(.imports_view(ard_imports(rv$study), rv$p$sheets$report)$used_by, "T1")
    expect_no_error(output$imp_list)
    # the mark above the ARD definition, with Compare (T1 has its own rows)
    h <- output$ard_kind_note$html
    expect_match(h, "uses an ARD taken in (t1_cro.rds)", fixed = TRUE)
    expect_match(h, "imp_compare_cur", fixed = TRUE)
    # the same rows as its own: they agree
    s2 <- rv$study; s2$planner <- rv$p
    expect_true(cards::is_ard_equal(compare_imported_ard(s2, "T1")))
    session$setInputs(imp_list_rows_selected = 1L, imp_compare = 1)
    expect_false(session$isClosed())
    # replaced: T1 now uses the new file, the old one is removed but kept
    session$setInputs(imp_replace = 1)
    session$setInputs(imp_file = data.frame(name = "t1_cro_v2.rds", datapath = f2,
                                            stringsAsFactors = FALSE),
                      imp_source = "CRO-A", imp_outputs = "T1")
    session$setInputs(imp_do = 2)
    d <- ard_imports(rv$study)
    expect_identical(d$state, c("removed", "in use"))
    expect_identical(.ard_import_of(rv$p, "T1"), "t1_cro_v2.rds")
    # removed, T1 back to its own ARD definition
    session$setInputs(imp_list_rows_selected = 1L)  # the newest first
    session$setInputs(imp_remove = 1)
    session$setInputs(imp_remove_back = TRUE, imp_remove_ok = 1)
    d <- ard_imports(rv$study)
    expect_identical(d$state, c("removed", "removed"))
    expect_null(.ard_import_of(rv$p, "T1"))
    expect_false(grepl("taken in", output$ard_kind_note$html %||% "", fixed = TRUE))
  })
})
