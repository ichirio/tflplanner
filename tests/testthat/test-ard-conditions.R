test_that("the study ARD's errors and warnings are listed, and a row opens its analysis", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("CO")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  adsl <- cards::ADSL
  saveRDS(adsl, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", adsl,
                           "SAFFL", "ARM", c("AGE", "SEX"))
  # a t-test across three arms: cardx keeps the error in the ARD
  a <- ard_rows(s$planner, "analyses", "T1")
  a$output_id <- NULL
  a[nrow(a) + 1L, ] <- NA
  a$analysis_id[nrow(a)] <- "TTEST"
  a$method[nrow(a)] <- "ttest"
  a$dataset[nrow(a)] <- "ADSL"
  a$population_id[nrow(a)] <- "SAF"
  a$by[nrow(a)] <- "ARM"
  a$variables[nrow(a)] <- "AGE"
  s$planner <- set_ard_rows(s$planner, "analyses", "T1", a)
  save_study(s)
  expect_identical(nrow(study_ard_conditions(s)), 0L)
  r <- run_ard(s)
  expect_null(r$error)
  d <- study_ard_conditions(s)
  expect_true(any(d$analysis_id == "TTEST" & d$level == "error"))
  expect_identical(d$level[1], "error")
  shiny::testServer(server_for("CO"), {
    rv <- session$userData$rv
    session$setInputs(nav = "ard", target = "T1")
    expect_match(output$ard_cond_badge$html, "errors 1", fixed = TRUE)
    expect_no_error(output$ard_conds)
    session$setInputs(ard_cond_errors = TRUE)
    e <- d[d$level == "error", , drop = FALSE]
    i <- which(e$analysis_id == "TTEST")[1]
    session$setInputs(ard_conds_rows_selected = i)
    expect_match(output$ard_stat_ui$html, "Analysis TTEST of T1", fixed = TRUE)
  })
})
