# The ARD tab's analysis data (tflspec's sheet analysis_data): 1. the data a
# report reads, made, changed, named; 2. an analysis reads one

adata_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "DM"), analysis_id = c("GROUPN", "AGE"),
    method = c("cards::ard_tabulate", "cards::ard_summary"),
    population_id = "SAF", where = c(NA, "AGE >= 18"),
    by = c(NA, "TRT01A"), variables = c("TRT01A", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("analysis data are added, renamed, named from a report and removed", {
  p <- adata_planner()
  expect_identical(.adata_suggest(p, "DM", "ADSL", "SAF"), "adsl_saf")
  # a report's data given a name: its analyses read it; a condition all of
  # them have (here only AGE's: GROUPN has none) stays with the analyses
  q <- name_analysis_data(p, "DM", NA, "SAF", "adsl_saf")
  a <- q$ard$analyses
  expect_identical(a$data, c("adsl_saf", "adsl_saf"))
  expect_true(all(is.na(a$population_id)))
  expect_identical(a$where, c(NA, "AGE >= 18"))
  ad <- .adata_rows(q, "DM")
  expect_identical(ad$from, "ADSL")
  expect_identical(ad$population_id, "SAF")
  expect_identical(.adata_of_report(q, "DM"), "adsl_saf")
  # the same data under a new name: the analyses follow
  q <- set_analysis_data(q, "DM", "adsl_safety", from = "ADSL", population_id = "SAF",
                         old = "adsl_saf")
  expect_identical(q$ard$analyses$data, c("adsl_safety", "adsl_safety"))
  # a data made from it; used ones are not removed
  q <- set_analysis_data(q, "DM", "adsl_old", from = "adsl_safety", where = "AGE >= 65")
  expect_identical(.adata_pop(.adata_rows(q, "DM"), "adsl_old"), "SAF")
  expect_identical(.adata_dataset(.adata_rows(q, "DM"), "adsl_old"), "ADSL")
  expect_error(remove_analysis_data(q, "DM", "adsl_safety"), "is used")
  q <- remove_analysis_data(q, "DM", "adsl_old")
  expect_identical(.adata_rows(q, "DM")$data_id, "adsl_safety")
  expect_error(set_analysis_data(q, "DM", "adsl_safety", from = "ADSL"), "already")
  # the engine takes it
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
})

test_that("the form's data choice: an analysis data first, written as `data`", {
  p <- name_analysis_data(adata_planner(), "DM", NA, "SAF", "adsl_saf")
  ad <- .adata_rows(p, "DM")
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
    # chosen as the others: its mark, and its settings below
    expect_match(output$ard_adata$html, "aria-selected=\"true\"", fixed = TRUE)
    expect_match(output$adata_detail$html, "adata_id", fixed = TRUE)
    session$setInputs(adata_from = "ADSL", adata_pop = "SAF", adata_subj = "",
                      adata_add = NULL, adata_derive = "", adata_distinct = NULL,
                      adata_id = "adsl_saf", adata_label = "Safety set")
    session$setInputs(adata_preview = 1)
    expect_match(output$adata_preview_out$html, "subjects", fixed = TRUE)
    session$setInputs(adata_save = 1)
    rv <- session$userData$rv
    expect_identical(rv$p$ard$analyses$data, c("adsl_saf", "adsl_saf"))
    h <- output$ard_adata$html
    expect_match(h, "Read by GROUPN, AGE", fixed = TRUE)
    expect_match(h, "254 subjects", fixed = TRUE)
  })
})

test_that("a suggested name is not the program's own; the choices come in groups", {
  p <- adata_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADVS"), path = c("a.rds", "b.rds")), "datasets")
  # advs_saf is the program's name for ADVS x SAF: not suggested
  expect_identical(.adata_suggest(p, "DM", "ADVS", "SAF"), "advs_saf_1")
  expect_identical(.adata_suggest(p, "DM", "ADVS", "SAF", "AVISIT == \"Week 24\""), "advs_week24")
  q <- name_analysis_data(p, "DM", NA, "SAF", "adsl_saf")
  ad <- .adata_rows(q, "DM")
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

test_that("1-1: a data kept to the subjects of another", {
  p <- adata_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE"), path = c("data/adam/adsl.rds", "data/adam/adae.rds")), "datasets")
  p <- set_analysis_data(p, "DM", "adsl_old", from = "ADSL", population_id = "SAF",
                         where = "AGE >= 65")
  p <- set_analysis_data(p, "DM", "adae_old", from = "ADAE", subjects = "adsl_old",
                         where = "TRTEMFL == \"Y\"", add = "TRT01A", keep = "TRT01A | AEDECOD")
  ad <- .adata_rows(p, "DM")
  expect_identical(.adata_pop(ad, "adae_old"), "SAF")
  expect_identical(.adata_subjects_of(ad, "adae_old"), "adsl_old")
  # the subjects data is used by the data kept to it: not deleted
  expect_error(remove_analysis_data(p, "DM", "adsl_old"), "is used")
  # a new name is followed by the data that keep its subjects
  q <- set_analysis_data(p, "DM", "adsl_65", from = "ADSL", population_id = "SAF",
                         where = "AGE >= 65", old = "adsl_old")
  expect_identical(.adata_rows(q, "DM")$subjects[2L], "adsl_65")
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
  p <- set_analysis_data(p, "DM", "adae_teae", from = "ADAE", where = "TRTEMFL == \"Y\"")
  p <- set_analysis_data(p, "DM", "adlb_alt", from = "ADLB", population_id = "SAF")
  p <- set_analysis_data(p, "DM", "adae_ser", from = "adae_teae", where = "AESER == \"Y\"")
  p <- set_analysis_data(p, "DM", "adsl_saf", from = "ADSL", where = "SAFFL == \"Y\"")
  expect_identical(.adata_subject_level(p, "DM"), "adsl_saf")
  q <- .adata_keep_to(p, "DM", "adsl_saf", c("adae_teae", "adlb_alt", "adae_ser"))
  # adlb_alt has its analysis set; adae_ser follows adae_teae (made from it)
  expect_identical(attr(q, "kept"), "adae_teae")
  ad <- .adata_rows(q, "DM")
  expect_identical(ad$data_id, c("adsl_saf", "adae_teae", "adlb_alt", "adae_ser"))
  expect_identical(ad$subjects[2L], "adsl_saf")
  expect_identical(.adata_subjects_of(ad, "adae_ser"), "adsl_saf")
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
  expect_identical(.adata_subject_key(q), "USUBJID")
})

test_that("1-1: one list, a new analysis data, kept to another's subjects", {
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
    # what it is made from and its analysis set first, the name after them
    expect_lt(regexpr("adata_from", f), regexpr("adata_pop", f))
    expect_lt(regexpr("adata_pop", f), regexpr("adata_id", f))
    expect_match(f, "adata_cond", fixed = TRUE)
    # the words: a filter; the condition as R, the whole data as R
    expect_match(f, "Filter (a condition)", fixed = TRUE)
    expect_match(f, "Write this analysis data whole as R (code)", fixed = TRUE)
    expect_false(grepl("Rows kept", f, fixed = TRUE))
    # columns added from the subjects' data: not while it is made from the
    # analysis set's own data (ADSL), and said what they do
    expect_match(f, "indexOf(input.adata_from) &lt; 0", fixed = TRUE)
    expect_match(f, "[&quot;ADSL&quot;]", fixed = TRUE)
    expect_match(f, "Nothing chosen: nothing added", fixed = TRUE)
    session$setInputs(adata_label = "", adata_from = "ADSL", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL)
    # the condition (as the builder gives it): the name after it
    cond <- session$userData$adata_cond
    cond$value("SAFFL == \"Y\"")
    cond$key(shiny::isolate(cond$key()) + 1L)
    session$flushReact()
    session$setInputs(adata_id = "adsl_saf")
    session$setInputs(adata_save = 1)
    rv <- session$userData$rv
    ad <- .adata_rows(rv$p, "DM")
    expect_identical(ad$data_id, "adsl_saf")
    # the analysis set's condition as it is: its population_id
    expect_identical(ad$population_id, "SAF")
    expect_true(is.na(ad$where))
    # another kept to its subjects
    session$setInputs(ard_adata_new = 2)
    session$setInputs(adata_id = "adsl_old", adata_label = "", adata_from = "ADSL",
                      adata_subj_on = TRUE, adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL)
    session$setInputs(adata_save = 2)
    ad <- .adata_rows(rv$p, "DM")
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

test_that("1-1's sheet: the report's rows replaced as a whole; a `from` the form has no choice for is kept", {
  # a report's rows replaced as a whole, in their place in the sheet
  ad <- .normalize_ard_sheet(data.frame(output_id = c("A", "B", "B", "C"),
                                        data_id = c("a", "b", "c", "d"), from = "ADSL"), "analysis_data")
  p <- add_output(new_planner(), "B")
  p$ard$analysis_data <- ad
  d <- data.frame(data_id = c("b", "x"), from = c("ADAE", "ADLB"))
  out <- .adata_rows(.adata_set_rows(p, "B", d))
  expect_identical(out$data_id, c("a", "b", "x", "d"))
  expect_identical(out$output_id, c("A", "B", "B", "C"))
  expect_identical(out$from, c("ADSL", "ADAE", "ADLB", "ADSL"))
  # a report with none yet: at the end
  out2 <- .adata_rows(.adata_set_rows(p, "D", d[1L, ]))
  expect_identical(out2$output_id, c("A", "B", "B", "C", "D"))
  skip_if_not_installed("cards")
  local_home()
  p <- adata_planner()
  p <- set_analysis_data(p, "DM", "adsl_saf", from = "ADSL", where = "SAFFL == \"Y\"")
  p$ard$analyses$data <- "adsl_saf"
  p$ard$analyses$population_id <- NA
  # written in the sheet by hand: a dataset the catalog does not have
  p$ard$analysis_data$from <- "ADSL2"
  s <- create_study("SH", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("SH"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    expect_false(is.null(output$hot_adata_report))
    session$setInputs(ard_adata_pick = "adsl_saf")
    f <- output$adata_detail$html
    expect_match(f, "ADSL2 (as written in the sheet", fixed = TRUE)
    expect_match(f, "<option value=\"ADSL2\" selected>", fixed = TRUE)
    # the right of step 1 follows what is open: the analysis data's rows and
    # its preview, then 1-2's again when it closes
    expect_match(output$ard_spec_pane$html, "hot_adata_report", fixed = TRUE)
    expect_match(output$ard_result_pane$html, "adata_preview", fixed = TRUE)
    session$setInputs(adata_close = 1)
    expect_match(output$ard_spec_pane$html, "hot_ard_analyses", fixed = TRUE)
    expect_match(output$ard_result_pane$html, "ard_preview", fixed = TRUE)
  })
})

test_that("a data written as R: the code the program has as a start; saved, the fields left blank", {
  p <- adata_planner()
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADAE"), path = c("data/adam/adsl.rds", "data/adam/adae.rds")), "datasets")
  p <- set_analysis_data(p, "DM", "adsl_saf", from = "ADSL", where = "SAFFL == \"Y\"")
  p <- set_analysis_data(p, "DM", "adae_teae", from = "ADAE", subjects = "adsl_saf",
                         where = "TRTEMFL == \"Y\"")
  v <- .adata_code_start(p, "DM", "adae_teae")
  # (one condition in brackets or not, as tflspec writes it)
  expect_match(v, "^adae_teae <- filter\\(adae, USUBJID %in% adsl_saf\\$USUBJID & \\(?TRTEMFL == \"Y\"\\)?\\)\nadae_teae$")
  q <- set_analysis_data(p, "DM", "adae_teae", from = "ADAE", code = v, old = "adae_teae")
  ad <- .adata_rows(q, "DM")
  expect_identical(ad$code[2L], v)
  expect_silent(suppressWarnings(.ard_spec(q$ard)))
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("CO", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data", "adam", "adae.rds"))
  shiny::testServer(server_for("CO"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_pick = "adae_teae")
    expect_match(output$adata_detail$html, "adata_code", fixed = TRUE)
    session$setInputs(adata_id = "adae_teae", adata_label = "", adata_from = "ADAE", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL,
                      adata_code = "dplyr::filter(adae, AESER == \"Y\")")
    session$setInputs(adata_save = 1)
    rv <- session$userData$rv
    ad <- .adata_rows(rv$p, "DM")
    r <- ad[ad$data_id == "adae_teae", ]
    expect_identical(r$code, "dplyr::filter(adae, AESER == \"Y\")")
    # written as R: the fields that make a data are not used
    expect_true(is.na(r$subjects))
    expect_true(is.na(r$where))
    # made by it
    d <- .adata_make(rv$p, s$path, "DM", "adae_teae")
    expect_true(all(d$AESER == "Y"))
  })
})

test_that("1-2: an analysis written as code (custom) from what its fields make", {
  skip_if_not_installed("cards")
  local_home()
  p <- adata_planner()
  s <- create_study("AC", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("AC"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_ol_pick = "AGE")
    expect_match(output$ard_stat_ui$html, "ard_an_as_code", fixed = TRUE)
    session$setInputs(ard_an_as_code = 1)
    session$setInputs(ard_an_as_code_ok = 1)
    rv <- session$userData$rv
    a <- rv$p$ard$analyses
    r <- a[a$analysis_id == "AGE", ]
    expect_identical(r$method, "custom")
    expect_match(r$code, "cards::ard_summary(data", fixed = TRUE)
    # the definition takes it, and the form shows its code now
    expect_silent(suppressWarnings(.ard_spec(rv$p$ard)))
    session$setInputs(ard_ol_pick = "AGE")
    # (a second click closes the form: nothing, now -- no empty frame)
    expect_false(isTRUE(grepl("ard_an_as_code\"", output$ard_stat_ui$html, fixed = TRUE)))
  })
})

test_that("step 1 says the report's analyses and the study's; the Data tab explains its sheets", {
  skip_if_not_installed("cards")
  local_home()
  p <- adata_planner()
  p$ard$analyses <- rbind(p$ard$analyses, transform(p$ard$analyses[1L, ], output_id = "OTHER"))
  s <- create_study("RV", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("RV"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    expect_match(output$ard_check$html, "This report's analyses: 2 (the study's: 3)", fixed = TRUE)
    expect_match(output$ard_2_2_head$html, "1-2 Analyses", fixed = TRUE)
    for (sh in c("datasets", "populations")) {
      expect_false(is.null(output[[paste0("help_ard_", sh)]]))
    }
    # the analysis data are each report's (1-1): the Data tab has no grid of them
    expect_error(output$hot_ard_analysis_data)
  })
})

test_that("a data's name from what it is made from and its condition", {
  po <- data.frame(population_id = c("SAF", "ITT"), where = c("SAFFL == \"Y\"", "ITTFL == \"Y\""))
  expect_identical(.adata_name_from("ADSL", "SAFFL == \"Y\"", po), "adsl_saf")
  expect_identical(.adata_name_from("ADSL", "EFFFL %in% \"Y\""), "adsl_eff")
  expect_identical(.adata_name_from("ADAE", "TRTEMFL == \"Y\""), "adae_trtem")
  expect_true(is.na(.adata_name_from("ADSL", "AGE >= 65", po)))
  expect_true(is.na(.adata_name_from("ADSL", NA, po)))
})

test_that("1-1's analysis set: the condition's first row, from the study's sets and the data's flags", {
  expect_identical(.population_id_for("PPROTFL"), "PP")
  expect_identical(.population_id_for("SAFFL", "SAF"), "SAF2")
  expect_identical(.adata_name_from("ADAE", NA, pop = "SAF"), "adae_saf")
  # the first term: put in, replaced (an analysis set's), taken out
  known <- c('SAFFL == "Y"', 'PPROTFL == "Y"')
  expect_identical(.cond_set_first(NA, 'SAFFL == "Y"', known), 'SAFFL == "Y"')
  expect_identical(.cond_set_first('SAFFL == "Y" & AGE >= 65', 'PPROTFL == "Y"', known),
                   'PPROTFL == "Y" & AGE >= 65')
  expect_identical(.cond_set_first('AGE >= 65', 'SAFFL == "Y"', known), 'SAFFL == "Y" & AGE >= 65')
  expect_identical(.cond_set_first('SAFFL == "Y" & (A == 1 | B == 2)', NA, known), "(A == 1 | B == 2)")
  expect_true(is.na(.cond_set_first('SAFFL == "Y"', NA, known)))
  expect_identical(.cond_set_first("not R (", 'SAFFL == "Y"', known), "not R (")
  expect_identical(.cond_first('SAFFL == "Y" & AGE >= 65'), 'SAFFL == "Y"')
  skip_if_not_installed("cards")
  local_home()
  p <- adata_planner()
  s <- create_study("PO", planner = p)
  d <- cards::ADSL
  d$PPROTFL <- ifelse(d$SAFFL == "Y", "Y", "N")
  saveRDS(d, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("PO"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_new = 1)
    f <- output$adata_detail$html
    # a new data: the study's first analysis set, as the condition's first row
    expect_match(f, "<option value=\"pop:SAF\" selected>", fixed = TRUE)
    cond <- session$userData$adata_cond
    expect_identical(shiny::isolate(cond$value()), 'SAFFL == "Y"')
    # the data's flags that are no analysis set: offered as well
    expect_match(f, "flag:PPROTFL", fixed = TRUE)
    expect_false(grepl("flag:\"", f, fixed = TRUE))
    session$setInputs(adata_from = "ADSL", adata_pop = "flag:PPROTFL")
    expect_identical(shiny::isolate(cond$value()), 'PPROTFL == "Y"')
    # saved: the condition, no population_id
    session$setInputs(adata_id = "adsl_pp", adata_label = "", adata_subj = "", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL, adata_code = "")
    session$flushReact()
    session$setInputs(adata_save = 1)
    ad <- .adata_rows(session$userData$rv$p, "DM")
    r <- ad[ad$data_id == "adsl_pp", ]
    expect_identical(r$where, 'PPROTFL == "Y"')
    expect_true(is.na(r$population_id))
    # an analysis set's row as it is, and another: population_id, and where
    # the other; the same data again is said to be there already
    session$setInputs(ard_adata_new = 2)
    session$setInputs(adata_from = "ADSL", adata_pop = "pop:SAF")
    cond$value('SAFFL == "Y" & AGE >= 65')
    cond$key(shiny::isolate(cond$key()) + 1L)
    session$setInputs(adata_id = "adsl_65", adata_label = "", adata_subj = "", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL, adata_code = "")
    session$flushReact()
    session$setInputs(adata_save = 2)
    r <- .adata_rows(session$userData$rv$p, "DM")
    r <- r[r$data_id == "adsl_65", ]
    expect_identical(r$population_id, "SAF")
    expect_identical(r$where, "AGE >= 65")
    session$setInputs(ard_adata_new = 3)
    session$setInputs(adata_from = "ADSL", adata_pop = "pop:SAF")
    cond$value('SAFFL == "Y" & AGE >= 65')
    cond$key(shiny::isolate(cond$key()) + 1L)
    session$setInputs(adata_id = "adsl_65_1", adata_add = NULL, adata_derive = "",
                      adata_keep = NULL, adata_distinct = NULL, adata_code = "")
    session$flushReact()
    expect_match(output$adata_same$html, "adsl_65, already there", fixed = TRUE)
    # the row changed ("!="): a condition like the others, no population_id
    cond$value('SAFFL != "Y"')
    cond$key(shiny::isolate(cond$key()) + 1L)
    session$setInputs(adata_id = "adsl_notsaf")
    session$flushReact()
    session$setInputs(adata_save = 3)
    r <- .adata_rows(session$userData$rv$p, "DM")
    r <- r[r$data_id == "adsl_notsaf", ]
    expect_match(r$where, "SAFFL", fixed = TRUE)
    expect_true(is.na(r$population_id))
  })
})

test_that("an analysis set's condition taken out of a condition, and put in", {
  po <- data.frame(population_id = c("SAF", "ITT"), where = c('SAFFL == "Y"', 'ITTFL == "Y"'))
  expect_identical(.cond_take_pop('SAFFL == "Y"', po, "SAF"), list(pop = "SAF", where = NA_character_))
  expect_identical(.cond_take_pop('SAFFL == "Y" & AGE >= 65', po, "SAF"),
                   list(pop = "SAF", where = "AGE >= 65"))
  # changed, or not first: a condition as it is
  expect_identical(.cond_take_pop('SAFFL == "N"', po, "SAF"), list(pop = NA_character_, where = 'SAFFL == "N"'))
  expect_identical(.cond_take_pop('AGE >= 65 & SAFFL == "Y"', po, "SAF")$pop, NA_character_)
  expect_identical(.cond_take_pop('SAFFL == "Y"', po, NA)$pop, NA_character_)
  expect_identical(.cond_take_pop("not R (", po, "SAF")$where, "not R (")
  expect_identical(.cond_put_pop("AGE >= 65", 'SAFFL == "Y"'), 'SAFFL == "Y" & AGE >= 65')
  expect_identical(.cond_put_pop(NA, 'SAFFL == "Y"'), 'SAFFL == "Y"')
  expect_true(is.na(.cond_put_pop("not R (", 'SAFFL == "Y"')))
  expect_true(is.na(.cond_put_pop("AGE >= 65", NA)))
  ad <- data.frame(data_id = c("adsl_saf", "adsl_x"), from = "ADSL", population_id = c("SAF", "SAF"),
                   subjects = NA, where = c(NA, "AGE>=65"), add = c(NA, "TRT01A"))
  expect_identical(.adata_same_as(ad, "ADSL", "SAF", NA, NA), "adsl_saf")
  expect_identical(.adata_same_as(ad, "ADSL", "SAF", NA, NA, but = "adsl_saf"), NA_character_)
  # one with more (add) is not the same
  expect_identical(.adata_same_as(ad, "ADSL", "SAF", NA, "AGE >= 65"), NA_character_)
  expect_identical(.adata_same_as(ad, "ADAE", "SAF", NA, NA), NA_character_)
})

test_that("1-1: a population_id the sheet has is the condition's first row", {
  skip_if_not_installed("cards")
  local_home()
  p <- adata_planner()
  p <- set_analysis_data(p, "DM", "adsl_old", from = "ADSL", population_id = "SAF", where = "AGE >= 65")
  s <- create_study("PL", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("PL"), {
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_pick = "adsl_old")
    expect_false(grepl("Also kept to the population", output$adata_detail$html, fixed = TRUE))
    expect_identical(shiny::isolate(session$userData$adata_cond$value()), 'SAFFL == "Y" & AGE >= 65')
    session$setInputs(adata_id = "adsl_old", adata_label = "Old", adata_from = "ADSL", adata_subj = "",
                      adata_add = NULL, adata_derive = "", adata_keep = NULL, adata_distinct = NULL,
                      adata_code = "")
    session$setInputs(adata_save = 1)
    ad <- .adata_rows(session$userData$rv$p, "DM")
    expect_identical(ad$population_id[ad$data_id == "adsl_old"], "SAF")
    expect_identical(ad$where[ad$data_id == "adsl_old"], "AGE >= 65")
    expect_identical(ad$label[ad$data_id == "adsl_old"], "Old")
  })
})


test_that("1-1 lists only the analysis data the report's analyses read (every sample report)", {
  skip_if_not_installed("cards")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  p <- s$planner
  ad <- .adata_rows(p)$data_id
  tabs <- unique(p$ard$analyses$output_id)
  shiny::testServer(server_for(s$meta$study_id), {
    session$setInputs(nav = "make", step = "ard")
    for (id in tabs) {
      session$setInputs(target = id)
      h <- output$ard_adata$html
      # (a row's click names it: Shiny.setInputValue('ard_adata_pick', "<id>"))
      shown <- ad[vapply(ad, function(d)
        grepl(paste0("ard_adata_pick&#39;, &quot;", d, "&quot;"), h, fixed = TRUE), NA)]
      expect_setequal(shown, .adata_of_report(p, id))
    }
    # the demographics table reads the safety set only
    session$setInputs(target = "T-14-1-1")
    expect_identical(.adata_of_report(p, "T-14-1-1"), "adsl_saf")
  })
  # every table reads analysis data of its analysis set: the safety set,
  # the screen failures' (T-14-1-4), the enrolled subjects' (T-14-1-5 and
  # T-14-1-6, race), none for the study information (T-14-0-1)
  pops <- vapply(tabs, function(id) report_population(p, id), "")
  own <- c("T-14-0-1", "T-14-1-4", "T-14-1-5", "T-14-1-6")
  expect_identical(unname(pops[own]), c(NA_character_, "SCRF", "ENR", "ENR"))
  expect_true(all(pops[setdiff(tabs, own)] %in% "SAF"))
})
