# The CDISC ARS export dialog (R/ars_ui.R): its checks shown before the
# files, the profile chosen there, a row's link to its report.

test_that("the ARS dialog shows the checks, switches profile and writes what it shows", {
  local_home()
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = "DM", analysis_id = "AGE", method = "continuous",
    population_id = "SAF", by = "TRT01A", variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  create_study("AR", planner = p)
  suppressWarnings(shiny::testServer(server_for("AR"), {
    session$setInputs(ars_open = 1)
    h <- output$ars_summary$html
    expect_match(h, "ars_goto", fixed = TRUE)
    expect_match(h, "purpose", fixed = TRUE)
    expect_match(h, ">DM<", fixed = TRUE)
    expect_match(basename(output$ars_json), "^AR_ars[.]json$")
    # siera: built again, and its files named so
    session$setInputs(ars_profile = "siera")
    f <- output$ars_json
    expect_match(basename(f), "^AR_ars_siera[.]json$")
    expect_identical(jsonlite::fromJSON(f, simplifyVector = FALSE)$id, "AR")
    z <- output$ars_zip
    expect_setequal(zip::zip_list(z)$filename,
                    c("AR_ars_siera.json", "AR_ars_siera.xlsx", "ars_check.csv"))
    # a row's link: no error, and a row out of range is ignored
    session$setInputs(ars_goto = 1L)
    session$setInputs(ars_goto = 9999L)
    expect_true(TRUE)
  }))
})
