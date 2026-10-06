# The forms of step 2 keep what they cannot show (design 13-2): a value the
# fields have no place for is shown and written back as it was

test_that("2-2: arguments the fields cannot show come back as they were", {
  f <- .ard_form_fields(.ard_method_call("cards::ard_tabulate", .std_ard_methods()))
  args <- "denominator = dplyr::filter(population, AGE > 65), fmt_fn = list(n = 0)"
  pa <- .ard_args_parse(args, f)
  expect_true(nzchar(pa$other))
  back <- .ard_args_build(pa$values, f, pa$other)
  expect_true(.ard_args_same(back, args))
})

test_that("2-2: a data the study has no choice for is a choice, as it is", {
  w <- list(with = "%s x %s (%s)", alone = "%s (%s)", none = "(none)")
  po <- data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  ch <- .an_data_choices("ADSL", po, now = "ADXX|SAF", words = w)
  expect_true("ADXX|SAF" %in% unlist(ch))
})

test_that("2-1: an analysis data the form cannot show is saved back unchanged", {
  skip_if_not_installed("cards")
  local_home()
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = c("ADSL", "ADAE"),
                               path = c("data/adam/adsl.rds", "data/adam/adae.rds"))
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p <- set_analysis_data(p, "adsl_saf", from = "ADSL", where = "SAFFL == \"Y\"")
  # written by hand in the sheet: a condition rows cannot hold, columns not
  # in the data, a derive
  odd <- "AESER == \"Y\" | (AETOXGR %in% c(\"3\", \"4\") & !is.na(AEACN))"
  p <- set_analysis_data(p, "adae_odd", from = "ADAE", subjects = "adsl_saf",
                         where = odd, add = "TRT01A | NOT_A_COLUMN",
                         derive = "Q = 1 | R = Q + 1", keep = "AEDECOD | ODD",
                         distinct = "USUBJID | AEDECOD")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "DM", analysis_id = "A1", method = "cards::ard_tabulate",
    data = "adae_odd", variables = "AEDECOD"), "analyses")
  before <- .adata_rows(p)
  s <- create_study("KU", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data", "adam", "adae.rds"))
  shiny::testServer(server_for("KU"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_pick = "adae_odd")
    f <- output$adata_detail$html
    # what the browser would send back for the fields as drawn
    session$setInputs(adata_id = "adae_odd", adata_label = "Odd AEs", adata_from = "ADAE",
                      adata_subj_on = TRUE,
                      adata_add = c("TRT01A", "NOT_A_COLUMN"),
                      adata_derive = "Q = 1 | R = Q + 1",
                      adata_keep = c("AEDECOD", "ODD"),
                      adata_distinct = c("USUBJID", "AEDECOD"), adata_code = "")
    session$setInputs(adata_save = 1)
    after <- .adata_rows(session$userData$rv$p)
    a1 <- after[after$data_id == "adae_odd", ]
    b1 <- before[before$data_id == "adae_odd", ]
    # saved (the label it was given), the rest as it was
    expect_identical(a1$label, "Odd AEs")
    a1$label <- b1$label <- NULL
    rownames(a1) <- rownames(b1) <- NULL
    expect_identical(a1, b1)
  })
})
