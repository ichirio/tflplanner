test_that("a model's formula is written from columns", {
  expect_identical(.fb_formula("AVAL", c("TRTA", "BASE")), "AVAL ~ TRTA + BASE")
  expect_identical(.fb_formula("AVAL", c("TRTA", "SEX"), interaction = TRUE),
                   "AVAL ~ TRTA + SEX + TRTA:SEX")
  # the response is not a term of itself; nothing to write without both
  expect_identical(.fb_formula("AVAL", c("AVAL", "TRTA")), "AVAL ~ TRTA")
  expect_null(.fb_formula("", "TRTA"))
  expect_null(.fb_formula("AVAL", character()))
})

test_that("the data choices say how many subjects (and records) each reads", {
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("CN")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data/adam/adae.rds"))
  s$planner$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE"), level = "adam",
    path = c("data/adam/adsl.rds", "data/adam/adae.rds")), "datasets")
  s$planner$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""), "populations")
  n <- .an_data_counts(s)
  saf <- sum(cards::ADSL$SAFFL == "Y")
  expect_identical(n[["|SAF"]], sprintf("%d subjects", saf))
  ae <- cards::ADAE[cards::ADAE$USUBJID %in% cards::ADSL$USUBJID[cards::ADSL$SAFFL == "Y"], ]
  expect_identical(n[["ADAE|SAF"]], sprintf("%d records, %d subjects", nrow(ae),
                                            length(unique(ae$USUBJID))))
  ch <- .an_data_choices(c("ADSL", "ADAE"), s$planner$ard$populations,
                         words = list(with = "%s x %s (%s)", alone = "%s (%s)", none = "-",
                                      count = "%s: %s"), counts = n)
  expect_true(any(grepl(sprintf("pop_saf: %d subjects", saf), names(ch), fixed = TRUE)))
  s$planner <- add_output(s$planner, "T1", type = "table")
  s$planner <- set_ard_rows(s$planner, "analyses", "T1", data.frame(
    analysis_id = "A1", method = "continuous", population_id = "SAF", variables = "AGE"))
  save_study(s)
  shiny::testServer(server_for("CN"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_ol_pick = "A1")
    expect_match(output$ard_stat_ui$html, sprintf("pop_saf: %d subjects", saf), fixed = TRUE)
  })
})

test_that("a regression's form: its fitting function chosen, its formula written from columns", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("RG")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = "ADSL", level = "adam", path = "data/adam/adsl.rds"), "datasets")
  s$planner <- add_output(s$planner, "T1", type = "table")
  s$planner <- set_ard_rows(s$planner, "analyses", "T1", data.frame(
    analysis_id = "M1", method = "cardx::ard_regression", dataset = "ADSL",
    by = "ARM", args = "formula = AGE ~ ARM, method = \"lm\""))
  save_study(s)
  shiny::testServer(server_for("RG"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_ol_pick = "M1")
    h <- output$ard_an_args$html
    expect_match(h, "Write the formula", fixed = TRUE)
    expect_match(h, "survival::coxph", fixed = TRUE)
  })
})
