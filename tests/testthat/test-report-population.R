# A report's analysis set: one value (the report list's, the TOC's, step
# 2's), the data of its subjects made, the analyses moved with it

pop_planner <- function() {
  p <- add_output(new_planner(), "T1")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE"), path = c("data/adam/adsl.rds", "data/adam/adae.rds")), "datasets")
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = c("SAF", "ITT"), dataset = "ADSL",
    where = c("SAFFL == \"Y\"", "ITTFL == \"Y\"")), "populations")
  p
}

test_that("a report's analysis set makes the data of its subjects, once", {
  p <- pop_planner()
  expect_true(is.na(report_population(p, "T1")))
  p <- set_report_population(p, "T1", "SAF")
  expect_identical(report_population(p, "T1"), "SAF")
  expect_identical(attr(p, "made"), "adsl_saf")
  ad <- .adata_rows(p)
  expect_identical(ad$from, "ADSL")
  expect_identical(ad$population_id, "SAF")
  # another report of the same set: the data there is used
  p <- add_output(p, "T2")
  p <- set_report_population(p, "T2", "SAF")
  expect_length(attr(p, "made"), 0L)
  expect_identical(.adata_rows(p)$data_id, "adsl_saf")
  # one the study has not
  expect_error(set_report_population(p, "T1", "PP"), "No analysis set")
  # none: the value goes, the data stays
  q <- set_report_population(p, "T1", NA)
  expect_true(is.na(report_population(q, "T1")))
  expect_identical(.adata_rows(q)$data_id, "adsl_saf")
  expect_silent(suppressWarnings(.ard_spec(p$ard)))
})

test_that("a report's analysis set changed: its analyses move to the new set's data", {
  p <- set_report_population(pop_planner(), "T1", "SAF")
  p <- set_analysis_data(p, "adae_saf", from = "ADAE", subjects = "adsl_saf",
                         where = "TRTEMFL == \"Y\"")
  p <- set_analysis_data(p, "adae_ser", from = "adae_saf", where = "AESER == \"Y\"")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T1", analysis_id = c("A1", "A2", "A3"), method = "categorical",
    data = c("adae_saf", NA, "adae_ser"), denominator = c("adsl_saf", NA, NA),
    dataset = c(NA, "ADSL", NA), population_id = c(NA, "SAF", NA),
    variables = c("AEDECOD", "SEX", "AEDECOD")), "analyses")
  q <- set_report_population(p, "T1", "ITT")
  a <- q$ard$analyses
  expect_identical(a$data[1L], "adae_itt")
  expect_identical(a$denominator[1L], "adsl_itt")
  expect_identical(a$population_id[2L], "ITT")
  # made from another analysis data: left, and said
  expect_identical(a$data[3L], "adae_ser")
  expect_identical(attr(q, "left"), "adae_ser")
  expect_setequal(attr(q, "made"), c("adsl_itt", "adae_itt"))
  ad <- .adata_rows(q)
  r <- ad[ad$data_id == "adae_itt", ]
  expect_identical(r$subjects, "adsl_itt")
  expect_identical(r$where, "TRTEMFL == \"Y\"")
  # the old ones stay (the study's)
  expect_true(all(c("adsl_saf", "adae_saf") %in% ad$data_id))
  # back: the data there are used again, none made
  q2 <- set_report_population(q, "T1", "SAF")
  expect_length(attr(q2, "made"), 0L)
  expect_identical(q2$ard$analyses$data[1L], "adae_saf")
  expect_identical(q2$ard$analyses$denominator[1L], "adsl_saf")
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
})

test_that("an analysis set's label, flag and a flag that disagrees", {
  p <- pop_planner()
  expect_identical(.pop_flag("SAFFL == \"Y\""), "SAFFL")
  expect_identical(.pop_flag("EFFFL %in% \"Y\""), "EFFFL")
  expect_true(is.na(.pop_flag("SAFFL == \"Y\" & AGE > 65")))
  # the company standards' label (their note), else the flag's
  expect_identical(.pop_label(p, "SAF"), "Safety set")
  d <- data.frame(ITTFL = structure("Y", label = "Intent-To-Treat Population Flag"))
  expect_identical(.pop_label(p, "ITT", d), "Intent-To-Treat Population Flag")
  expect_identical(.pop_label(p, "ITT"), "ITT")
  adsl <- data.frame(USUBJID = c("a", "b", "c"), SAFFL = c("Y", "Y", "N"))
  adae <- data.frame(USUBJID = c("a", "a", "b", "c"), SAFFL = c("Y", "Y", "Y", "N"))
  expect_identical(.flag_disagrees(adae, adsl, "SAFFL"), 0L)
  adae$SAFFL[3L] <- "N"
  expect_identical(.flag_disagrees(adae, adsl, "SAFFL"), 1L)
  expect_true(is.na(.flag_disagrees(adae[c("USUBJID")], adsl, "SAFFL")))
})

test_that("the report list's analysis set: the report's, else its analyses'", {
  p <- set_report_population(pop_planner(), "T1", "SAF")
  p <- add_output(p, "T2")
  p <- set_analysis_data(p, "adae_saf", from = "ADAE", subjects = "adsl_saf")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T2", analysis_id = "A1", method = "categorical", data = "adae_saf",
    variables = "AEDECOD"), "analyses")
  r <- .report_rows(p)
  expect_identical(r$population, c("SAF", "SAF"))
  p <- set_report_population(p, "T1", "ITT")
  expect_identical(.report_rows(p)$population[1L], "ITT")
  # a copy keeps it
  p <- copy_output(p, "T1", "T3")
  expect_identical(report_population(p, "T3"), "ITT")
})

test_that("a report's analysis set is saved with the study", {
  local_home()
  p <- set_report_population(pop_planner(), "T1", "SAF")
  s <- create_study("RP", planner = p)
  s2 <- open_study("RP")
  expect_identical(report_population(s2$planner, "T1"), "SAF")
  expect_identical(.adata_rows(s2$planner)$data_id, "adsl_saf")
})

test_that("step 2: the report's analysis set chosen, its data made, 2-1 starts from it", {
  skip_if_not_installed("cards")
  local_home()
  p <- pop_planner()
  s <- create_study("RQ", planner = p)
  d <- cards::ADSL
  d$ITTFL <- "Y"
  saveRDS(d, file.path(s$path, "data", "adam", "adsl.rds"))
  ae <- cards::ADAE
  ae$SAFFL <- "N"
  saveRDS(ae, file.path(s$path, "data", "adam", "adae.rds"))
  shiny::testServer(server_for("RQ"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    h <- output$ard_report_pop$html
    expect_match(h, "ard_report_pop_pick", fixed = TRUE)
    session$setInputs(ard_report_pop_pick = "ITT")
    rv <- session$userData$rv
    expect_identical(report_population(rv$p, "T1"), "ITT")
    expect_true("adsl_itt" %in% .adata_rows(rv$p)$data_id)
    # a new data starts kept to the report's set's subjects
    session$setInputs(ard_adata_new = 1)
    f <- output$adata_detail$html
    expect_match(f, "Keep to the subjects of adsl_itt", fixed = TRUE)
    # ADAE's SAFFL that is not ADSL's: not the set, said
    session$setInputs(adata_from = "ADAE", adata_subj_on = FALSE, adata_pop = "pop:SAF")
    cond <- session$userData$adata_cond
    cond$value("SAFFL == \"Y\"")
    cond$key(shiny::isolate(cond$key()) + 1L)
    session$flushReact()
    expect_match(output$adata_pop_note$html, "is not ADSL's for", fixed = TRUE)
    session$setInputs(adata_id = "adae_flag", adata_label = "", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL, adata_code = "")
    session$setInputs(adata_save = 1)
    r <- .adata_rows(rv$p)
    r <- r[r$data_id == "adae_flag", ]
    expect_true(is.na(r$population_id))
    expect_match(r$where, "SAFFL", fixed = TRUE)
  })
})
