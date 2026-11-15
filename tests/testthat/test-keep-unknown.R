# The forms of step 1 keep what they cannot show (design 13-2): a value the
# fields have no place for is shown and written back as it was

test_that("1-2: arguments the fields cannot show come back as they were", {
  f <- .ard_form_fields(.ard_method_call("cards::ard_tabulate", .std_ard_methods()))
  args <- "denominator = dplyr::filter(population, AGE > 65), fmt_fn = list(n = 0)"
  pa <- .ard_args_parse(args, f)
  expect_true(nzchar(pa$other))
  back <- .ard_args_build(pa$values, f, pa$other)
  expect_true(.ard_args_same(back, args))
})

test_that("1-2: a data the study has no choice for is a choice, as it is", {
  w <- list(with = "%s x %s (%s)", alone = "%s (%s)", none = "(none)")
  po <- data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  ch <- .an_data_choices("ADSL", po, now = "ADXX|SAF", words = w)
  expect_true("ADXX|SAF" %in% unlist(ch))
})

test_that("1-1: an analysis data the form cannot show is saved back unchanged", {
  local_home()
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = c("ADSL", "ADAE"),
                               path = c("data/adam/adsl.rds", "data/adam/adae.rds"))
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p <- set_analysis_data(p, "DM", "adsl_saf", from = "ADSL", where = "SAFFL == \"Y\"")
  # written by hand in the sheet: a condition rows cannot hold, columns not
  # in the data, a derive
  odd <- "AESER == \"Y\" | (AETOXGR %in% c(\"3\", \"4\") & !is.na(AEACN))"
  p <- set_analysis_data(p, "DM", "adae_odd", from = "ADAE", subjects = "adsl_saf",
                         where = odd, add = "TRT01A | NOT_A_COLUMN",
                         derive = "Q = 1 | R = Q + 1", keep = "AEDECOD | ODD",
                         distinct = "USUBJID | AEDECOD")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "DM", analysis_id = "A1", method = "cards::ard_tabulate",
    data = "adae_odd", variables = "AEDECOD"), "analyses")
  before <- .adata_rows(p, "DM")
  s <- create_study("KU", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data", "adam", "adae.rds"))
  shiny::testServer(server_for("KU"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_pick = "adae_odd")
    f <- output$adata_detail$html
    # what the browser would send back for the fields as drawn
    session$setInputs(adata_id = "adae_odd", adata_from = "ADAE",
                      adata_subj_on = TRUE,
                      adata_add = c("TRT01A", "NOT_A_COLUMN"),
                      adata_derive = "Q = 1 | R = Q + 1",
                      adata_keep = c("AEDECOD", "ODD"),
                      adata_distinct = c("USUBJID", "AEDECOD"), adata_code = "")
    session$setInputs(adata_save = 1)
    after <- .adata_rows(session$userData$rv$p, "DM")
    a1 <- after[after$data_id == "adae_odd", ]
    b1 <- before[before$data_id == "adae_odd", ]
    # saved, the rest as it was (a data row has no label: a sheet an older
    # tflspec made may still carry the empty column)
    a1$label <- b1$label <- NULL
    rownames(a1) <- rownames(b1) <- NULL
    expect_identical(a1, b1)
  })
})

test_that("1-1: the card writes arrange and then into the spec and reads them back", {
  local_home()
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = c("ADSL", "ADAE"),
                               path = c("data/adam/adsl.rds", "data/adam/adae.rds"))
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  th <- "dplyr::mutate(N = 1) |> dplyr::filter(N > 0)"
  p <- set_analysis_data(p, "DM", "adae_s", from = "ADAE", population_id = "SAF",
                         arrange = "USUBJID | desc(ASTDT)", keep = "AEDECOD", then = th)
  ad <- .adata_rows(p, "DM")
  expect_identical(ad$arrange, "USUBJID | desc(ASTDT)")
  expect_identical(ad$then, th)
  # written with code: the card writes neither (whole-row code has none)
  s <- create_study("KU2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data", "adam", "adae.rds"))
  shiny::testServer(server_for("KU2"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_pick = "adae_s")
    f <- output$adata_detail$html
    expect_match(f, "adata_arrange", fixed = TRUE)
    expect_match(f, "adata_then", fixed = TRUE)
    expect_match(f, "desc(ASTDT)", fixed = TRUE)
    # what the browser sends back for the fields as drawn: saved as it was
    session$setInputs(adata_id = "adae_s", adata_from = "ADAE", adata_subj_on = FALSE,
                      adata_add = NULL, adata_derive = "",
                      adata_arrange = c("USUBJID", "desc(ASTDT)"),
                      adata_keep = "AEDECOD", adata_distinct = NULL,
                      adata_then = th, adata_code = "")
    session$setInputs(adata_save = 1)
    a1 <- .adata_rows(session$userData$rv$p, "DM")
    expect_identical(a1$arrange[a1$data_id == "adae_s"], "USUBJID | desc(ASTDT)")
    expect_identical(a1$then[a1$data_id == "adae_s"], th)
  })
  # the form closes on saving: each change is a form of its own
  save_with <- function(arrange, then, code = "") {
    out <- NULL
    shiny::testServer(server_for("KU2"), {
      session$setInputs(nav = "make", step = "ard", target = "DM")
      session$setInputs(ard_adata_pick = "adae_s")
      session$setInputs(adata_id = "adae_s", adata_from = "ADAE", adata_subj_on = FALSE,
                        adata_add = NULL, adata_derive = "", adata_arrange = arrange,
                        adata_keep = "AEDECOD", adata_distinct = NULL,
                        adata_then = then, adata_code = code)
      session$setInputs(adata_save = 1)
      a <- .adata_rows(session$userData$rv$p, "DM")
      out <<- a[a$data_id == "adae_s", ]
    })
    out
  }
  # changed: the sort and the R follow
  a2 <- save_with(c("AEDECOD", "desc(AESEQ)"), "")
  expect_identical(a2$arrange, "AEDECOD | desc(AESEQ)")
  expect_true(is.na(a2$then))
  # whole-row code: arrange and then are not written with it
  a3 <- save_with("USUBJID", th, "dplyr::filter(adae, TRUE)")
  expect_true(is.na(a3$arrange))
  expect_true(is.na(a3$then))
  expect_match(a3$code, "filter", fixed = TRUE)
})
