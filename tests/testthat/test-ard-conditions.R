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
  expect_identical(unique(d$source), "")
  # an ARD taken in for another report: its errors too, said where from
  s <- open_study(s$path)
  s$planner <- add_output(s$planner, "T2", type = "table")
  save_study(s)
  own <- study_ard_rows(s, "T1")
  f <- file.path(tempdir(), "cro_t2.rds")
  own$output_id <- "T2"
  saveRDS(own, f)
  row <- import_ard(s, f, output_id = "T2", source = "CRO")
  s$planner <- use_imported_ard(s$planner, "T2", row$file)
  save_study(s)
  d2 <- study_ard_conditions(s)
  expect_true(any(d2$output_id == "T2" & d2$source == row$file & d2$level == "error"))
  shiny::testServer(server_for("CO"), {
    rv <- session$userData$rv
    session$setInputs(nav = "ard", target = "T1")
    expect_match(output$ard_cond_badge$html, "errors 2", fixed = TRUE)
    expect_no_error(output$ard_conds)
    session$setInputs(ard_cond_errors = TRUE)
    e <- d[d$level == "error", , drop = FALSE]
    i <- which(e$analysis_id == "TTEST")[1]
    session$setInputs(ard_conds_rows_selected = i)
    expect_match(output$ard_stat_ui$html, "Analysis TTEST of T1", fixed = TRUE)
    # the report's own, after Preview
    session$setInputs(target = "T1", ard_preview = 1)
    expect_match(output$ard_run_info$html, "Errors and warnings inside the analyses", fixed = TRUE)
    expect_match(output$ard_run_info$html, "not in the ARD", fixed = TRUE)
  })
})
