# The ARD tab's analysis data (tflspec's sheet analysis_data): 1. the data a
# report reads, made, changed, named; 2. an analysis reads one

adata_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "DM"), analysis_id = c("BIGN", "AGE"),
    method = c("cards::ard_tabulate", "cards::ard_summary"),
    population_id = "SAF", where = c(NA, "AGE >= 18"),
    by = c(NA, "TRT01A"), variables = c("TRT01A", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("analysis data are added, renamed, named from a report and removed", {
  p <- adata_planner()
  expect_identical(.adata_suggest(p, "ADSL", "SAF"), "adsl_saf")
  # a report's data given a name: its analyses read it; a condition all of
  # them have (here only AGE's: BIGN has none) stays with the analyses
  q <- name_analysis_data(p, "DM", NA, "SAF", "adsl_saf")
  a <- q$ard$analyses
  expect_identical(a$data, c("adsl_saf", "adsl_saf"))
  expect_true(all(is.na(a$population_id)))
  expect_identical(a$where, c(NA, "AGE >= 18"))
  ad <- .adata_rows(q)
  expect_identical(ad$from, "ADSL")
  expect_identical(ad$population_id, "SAF")
  expect_identical(.adata_of_report(q, "DM"), "adsl_saf")
  # the same data under a new name: the analyses follow
  q <- set_analysis_data(q, "adsl_safety", from = "ADSL", population_id = "SAF",
                         old = "adsl_saf")
  expect_identical(q$ard$analyses$data, c("adsl_safety", "adsl_safety"))
  # a data made from it; used ones are not removed
  q <- set_analysis_data(q, "adsl_old", from = "adsl_safety", where = "AGE >= 65")
  expect_identical(.adata_pop(.adata_rows(q), "adsl_old"), "SAF")
  expect_identical(.adata_dataset(.adata_rows(q), "adsl_old"), "ADSL")
  expect_error(remove_analysis_data(q, "adsl_safety"), "is used")
  q <- remove_analysis_data(q, "adsl_old")
  expect_identical(.adata_rows(q)$data_id, "adsl_safety")
  expect_error(set_analysis_data(q, "adsl_safety", from = "ADSL"), "already")
  # the engine takes it
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
})

test_that("the form's data choice: an analysis data first, written as `data`", {
  p <- name_analysis_data(adata_planner(), "DM", NA, "SAF", "adsl_saf")
  ad <- .adata_rows(p)
  r <- p$ard$analyses[2L, ]
  expect_identical(.an_data_value_row(r), "@adsl_saf")
  ch <- .an_data_choices(p$ard$datasets$dataset, p$ard$populations,
                         now = "@adsl_saf",
                         words = list(with = "%s x %s (%s)", alone = "%s (%s)",
                                      none = "(none)"),
                         adata = ad, first = "adsl_saf")
  expect_identical(unname(ch[1L]), "@adsl_saf")
  expect_match(names(ch)[1L], "adsl_saf", fixed = TRUE)
  expect_match(names(ch)[1L], "ADSL × SAF", fixed = TRUE)
  sp <- .an_data_split("@adsl_saf", ad)
  expect_identical(sp, list(dataset = "ADSL", pop = "SAF", data = "adsl_saf"))
  a <- .an_data_write(p$ard$analyses, 2L, .an_data_split("ADSL|SAF", ad))
  expect_true(is.na(a$data[2L]))
  expect_identical(a$population_id[2L], "SAF")
  a <- .an_data_write(a, 2L, sp)
  expect_identical(a$data[2L], "adsl_saf")
  expect_true(is.na(a$population_id[2L]))
})

test_that("the ARD tab shows a report's analysis data and makes one", {
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("AD", planner = adata_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("AD"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    h <- output$ard_adata$html
    # no named data yet: the analysis set's own data, to be named
    expect_match(h, "pop_saf", fixed = TRUE)
    expect_match(h, "ard_adata_name", fixed = TRUE)
    session$setInputs(ard_adata_name = "|SAF")
    session$setInputs(adata_from = "ADSL", adata_pop_keep = TRUE, adata_subj = "", adata_where = "",
                      adata_add = NULL, adata_derive = "", adata_distinct = NULL,
                      adata_id = "adsl_saf", adata_label = "Safety set")
    session$setInputs(adata_preview = 1)
    expect_match(output$adata_preview_out$html, "subjects", fixed = TRUE)
    session$setInputs(adata_save = 1)
    rv <- session$userData$rv
    expect_identical(rv$p$ard$analyses$data, c("adsl_saf", "adsl_saf"))
    h <- output$ard_adata$html
    expect_match(h, "Read by BIGN, AGE", fixed = TRUE)
    expect_match(h, "254 subjects", fixed = TRUE)
  })
})

test_that("a suggested name is not the program's own; the choices come in groups", {
  p <- adata_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADVS"), path = c("a.rds", "b.rds")), "datasets")
  # advs_saf is the program's name for ADVS x SAF: not suggested
  expect_identical(.adata_suggest(p, "ADVS", "SAF"), "advs_saf_1")
  expect_identical(.adata_suggest(p, "ADVS", "SAF", "AVISIT == \"Week 24\""), "advs_week24")
  q <- name_analysis_data(p, "DM", NA, "SAF", "adsl_saf")
  ad <- .adata_rows(q)
  w <- list(with = "%s x %s (%s)", alone = "%s (%s)", none = "(none)",
            groups = c("mine", "others", "rest"))
  ch <- .an_data_choices(q$ard$datasets$dataset, q$ard$populations, now = "@adsl_saf",
                         words = w, adata = ad, first = "adsl_saf")
  expect_identical(names(ch), c("mine", "rest"))
  expect_identical(unname(ch$mine), "@adsl_saf")
  expect_false("|" %in% ch$rest)
  # counts: one row a subject is said
  cw <- list(records = "%d records, %d subjects", one_row = "%d subjects, one row")
  expect_identical(.adata_count_words(data.frame(USUBJID = c("a", "b")), cw), "2 subjects, one row")
  expect_identical(.adata_count_words(data.frame(USUBJID = c("a", "a")), cw), "2 records, 1 subjects")
  # the preview: the columns the data names first
  d <- data.frame(STUDYID = 1, AVAL = 2, USUBJID = "a", AVISIT = "W", TRT01A = "A")
  expect_identical(.adata_preview_cols(d, list(add = "TRT01A", where = "AVISIT == \"W\"")),
                   c("USUBJID", "TRT01A", "AVISIT", "STUDYID", "AVAL"))
})

test_that("2-1: a data kept to the subjects of another", {
  p <- adata_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE"), path = c("data/adam/adsl.rds", "data/adam/adae.rds")), "datasets")
  p <- set_analysis_data(p, "adsl_old", from = "ADSL", population_id = "SAF",
                         where = "AGE >= 65")
  p <- set_analysis_data(p, "adae_old", from = "ADAE", subjects = "adsl_old",
                         where = "TRTEMFL == \"Y\"", add = "TRT01A", keep = "TRT01A | AEDECOD")
  ad <- .adata_rows(p)
  expect_identical(.adata_pop(ad, "adae_old"), "SAF")
  expect_identical(.adata_subjects_of(ad, "adae_old"), "adsl_old")
  # the subjects data is used by the data kept to it: not deleted
  expect_error(remove_analysis_data(p, "adsl_old"), "is used")
  # a new name is followed by the data that keep its subjects
  q <- set_analysis_data(p, "adsl_65", from = "ADSL", population_id = "SAF",
                         where = "AGE >= 65", old = "adsl_old")
  expect_identical(.adata_rows(q)$subjects[2L], "adsl_65")
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
  # an analysis set's opposite keeps a blank flag
  expect_identical(.cond_not('SAFFL == "Y"'), '!(SAFFL %in% "Y")')
  expect_identical(.cond_not("AGE >= 65"), "!((AGE >= 65) %in% TRUE)")
  d <- data.frame(SAFFL = c("Y", "N", NA))
  expect_identical(nrow(subset(d, eval(str2lang(.cond_not('SAFFL == "Y"'))))), 2L)
})

test_that("a data of one row a subject made later: the others are kept to it, above them", {
  p <- adata_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE", "ADLB"), path = c("a.rds", "b.rds", "c.rds")), "datasets")
  p <- set_analysis_data(p, "adae_teae", from = "ADAE", where = "TRTEMFL == \"Y\"")
  p <- set_analysis_data(p, "adlb_alt", from = "ADLB", population_id = "SAF")
  p <- set_analysis_data(p, "adae_ser", from = "adae_teae", where = "AESER == \"Y\"")
  p <- set_analysis_data(p, "adsl_saf", from = "ADSL", where = "SAFFL == \"Y\"")
  expect_identical(.adata_subject_level(p), "adsl_saf")
  q <- .adata_keep_to(p, "adsl_saf", c("adae_teae", "adlb_alt", "adae_ser"))
  # adlb_alt has its analysis set; adae_ser follows adae_teae (made from it)
  expect_identical(attr(q, "kept"), "adae_teae")
  ad <- .adata_rows(q)
  expect_identical(ad$data_id, c("adsl_saf", "adae_teae", "adlb_alt", "adae_ser"))
  expect_identical(ad$subjects[2L], "adsl_saf")
  expect_identical(.adata_subjects_of(ad, "adae_ser"), "adsl_saf")
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
  expect_identical(.adata_subject_key(q), "USUBJID")
})

test_that("2-1: one list, a new analysis data, kept to another's subjects", {
  skip_if_not_installed("cards")
  local_home()
  p <- adata_planner()
  p$ard$analyses <- p$ard$analyses[0L, ]
  s <- create_study("AN", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("AN"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    h <- output$ard_adata$html
    expect_match(h, "ard_adata_new", fixed = TRUE)
    expect_match(h, "None yet", fixed = TRUE)
    # a new analysis data: the name first; an analysis set's condition put in
    session$setInputs(ard_adata_new = 1)
    f <- output$adata_detail$html
    expect_lt(regexpr("adata_id", f), regexpr("adata_from", f))
    expect_match(f, "adata_short", fixed = TRUE)
    session$setInputs(adata_id = "adsl_saf", adata_label = "", adata_from = "ADSL",
                      adata_subj = "", adata_where = "SAFFL == \"Y\"", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL)
    session$setInputs(adata_save = 1)
    rv <- session$userData$rv
    ad <- .adata_rows(rv$p)
    expect_identical(ad$data_id, "adsl_saf")
    expect_identical(ad$where, "SAFFL == \"Y\"")
    expect_true(is.na(ad$population_id))
    # another kept to its subjects
    session$setInputs(ard_adata_new = 2)
    session$setInputs(adata_id = "adsl_old", adata_label = "", adata_from = "ADSL",
                      adata_subj_on = TRUE, adata_where = "AGE >= 65", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL)
    session$setInputs(adata_save = 2)
    ad <- .adata_rows(rv$p)
    expect_identical(ad$subjects[ad$data_id == "adsl_old"], "adsl_saf")
    # a click marks it, and the buttons name it
    session$setInputs(ard_adata_pick = "adsl_old")
    h <- output$ard_adata$html
    expect_match(h, "Copy adsl_old", fixed = TRUE)
    expect_match(h, "aria-selected=\"true\"", fixed = TRUE)
    # a first analysis is added at once (no question about subjects)
    session$setInputs(ard_an_new = 1)
    a <- rv$p$ard$analyses
    expect_identical(sum(a$output_id == "DM"), 1L)
  })
})
