# A report's analysis set: one value (the report list's, the TOC's, step
# 2's), the data of its subjects made, the analyses moved with it

# (slow: its tests write study folders or start the app -- run on CI
# and locally with NOT_CRAN=true, not in CRAN's check)
skip_on_cran()

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
  ad <- .adata_rows(p, "T1")
  expect_identical(ad$from, "ADSL")
  expect_identical(ad$population_id, "SAF")
  # another report of the same set: its own adsl_saf (an analysis data is
  # a report's)
  p <- add_output(p, "T2")
  p <- set_report_population(p, "T2", "SAF")
  expect_identical(attr(p, "made"), "adsl_saf")
  expect_identical(.adata_rows(p, "T2")$data_id, "adsl_saf")
  expect_identical(.adata_rows(p)$output_id, c("T1", "T2"))
  # one the study has not
  expect_error(set_report_population(p, "T1", "PP"), "No population")
  # none: the value goes, the data stays
  q <- set_report_population(p, "T1", NA)
  expect_true(is.na(report_population(q, "T1")))
  expect_identical(.adata_rows(q, "T1")$data_id, "adsl_saf")
  expect_silent(suppressWarnings(.ard_spec(p$ard)))
})

test_that("a report's analysis set changed: its analyses move to the new set's data", {
  p <- set_report_population(pop_planner(), "T1", "SAF")
  p <- set_analysis_data(p, "T1", "adae_saf", from = "ADAE", subjects_in = "adsl_saf",
                         where = "TRTEMFL == \"Y\"")
  p <- set_analysis_data(p, "T1", "adae_ser", from = "adae_saf", where = "AESER == \"Y\"")
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
  ad <- .adata_rows(q, "T1")
  r <- ad[ad$data_id == "adae_itt", ]
  expect_identical(r$subjects_in, "adsl_itt")
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
  expect_identical(.pop_label(p, "SAF"), "Safety Analysis Set")
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
  # T2's own: its adsl_saf and the AEs kept to it (no set of its own)
  p <- set_analysis_data(p, "T2", "adsl_saf", from = "ADSL", population_id = "SAF")
  p <- set_analysis_data(p, "T2", "adae_saf", from = "ADAE", subjects_in = "adsl_saf")
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

test_that("step 1: the report's analysis set chosen, its data made, 1-1 starts from it", {
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
    session$setInputs(ard_report_pop_pick_by = list(report = "T1", value = "ITT"))
    rv <- session$userData$rv
    expect_identical(report_population(rv$p, "T1"), "ITT")
    expect_true("adsl_itt" %in% .adata_rows(rv$p, "T1")$data_id)
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
    session$setInputs(adata_id = "adae_flag", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL, adata_code = "")
    r <- .adata_rows(rv$p, "T1")
    r <- r[r$data_id == "adae_flag", ]
    expect_true(is.na(r$population_id))
    expect_match(r$where, "SAFFL", fixed = TRUE)
  })
})

test_that("a TOC's population, matched to the study's analysis sets", {
  p <- pop_planner()
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = c("SAF", "ITT", "EFF"), dataset = "ADSL",
    where = c("SAFFL == \"Y\"", "ITTFL == \"Y\"", "EFFFL == \"Y\"")), "populations")
  expect_identical(.pop_match(p, "saf"), "SAF")
  expect_identical(.pop_match(p, "Safety Population"), "SAF")
  expect_identical(.pop_match(p, "Safety Analysis Set"), "SAF")
  expect_identical(.pop_match(p, "Intent-to-Treat Population"), "ITT")
  expect_identical(.pop_match(p, "Efficacy Population (EFF)"), "EFF")
  # a flag's label (the data's)
  d <- data.frame(ITTFL = structure("Y", label = "Randomised Subjects Flag"))
  expect_identical(.pop_match(p, "Randomised subjects flag", d), "ITT")
  expect_true(is.na(.pop_match(p, "Per-Protocol")))
  expect_true(is.na(.pop_match(p, NA)))
})

test_that("a TOC taken in: the reports' analysis sets, their data made", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title,Population",
               "T1,Table,Demographics,Safety Population",
               "T2,Table,AEs,Intent-to-treat",
               "T3,Table,Other,Completers"), f)
  p <- pop_planner()
  tp <- toc_populations(p, f, "No.", "Population")
  expect_identical(tp$output_id, c("T1", "T2", "T3"))
  expect_identical(tp$population_id, c("SAF", "ITT", NA))
  expect_identical(nrow(toc_populations(p, f, "No.", "Nope")), 0L)
  sp <- tflspec::tfl_read_toc(f, map = c(output_id = "No.", type = "Kind", title = "Title",
                                         population = "Population"))
  ch <- toc_changes(p, sp)
  pops <- stats::setNames(tp$population_id, tp$output_id)
  q <- toc_apply(p, sp, ch, populations = pops[!is.na(pops)])
  expect_identical(report_population(q, "T1"), "SAF")
  expect_identical(report_population(q, "T2"), "ITT")
  expect_true(is.na(report_population(q, "T3")))
  expect_setequal(attr(q, "made"), c("adsl_saf", "adsl_itt"))
  # taken in again with another set: the report's changes (its analyses with it)
  q2 <- toc_apply(q, sp, toc_changes(q, sp), populations = c(T1 = "ITT"))
  expect_identical(report_population(q2, "T1"), "ITT")
  # T1's own adsl_itt (T2's is T2's: an analysis data is a report's)
  expect_identical(attr(q2, "made"), "adsl_itt")
  expect_true("adsl_itt" %in% .adata_rows(q2, "T1")$data_id)
})

test_that("the Add dialog gives a new report its analysis set", {
  local_home()
  s <- create_study("RA", planner = pop_planner())
  shiny::testServer(server_for("RA"), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "table", modal_first = FALSE,
                      modal_id = "9", modal_desc = "x", modal_pop = "ITT")
    # (the ID typed after the type's prefix: T-9)
    session$setInputs(add_ok = 1)
    expect_identical(report_population(rv$p, "T-9"), "ITT")
    expect_true("adsl_itt" %in% .adata_rows(rv$p, "T-9")$data_id)
  })
})

test_that("a TOC's datasets: kept, a listing's default, a table's analysis data made", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title,Population,Data",
               "N1,Table,AEs,Safety Population,\"ADSL, ADAE\"",
               "L1,Listing,AE listing,,ADAE",
               "N2,Table,Demog,Safety Population,ADSL"), f)
  p <- pop_planner()
  m <- c(output_id = "No.", type = "Kind", title = "Title", population = "Population",
         datasets = "Data")
  sp <- tflspec::tfl_read_toc(f, map = m)
  tp <- toc_populations(p, f, "No.", "Population")
  pops <- stats::setNames(tp$population_id, tp$output_id)
  q <- toc_apply(p, sp, toc_changes(p, sp), populations = pops, make_data = TRUE)
  expect_identical(q$outputs$datasets, c(NA, "ADSL | ADAE", "ADAE", "ADSL"))
  expect_setequal(attr(q, "made"), c("adsl_saf", "adae_saf"))
  r <- .adata_rows(q, "N1")
  expect_identical(r$subjects_in[r$data_id == "adae_saf"], "adsl_saf")
  expect_identical(lf_rows(q, "listings", "L1")$dataset, "ADAE")
  expect_identical(.report_toc_data(q, "N1"), c(ADAE = "adae_saf", "adsl_saf"))
  # the report list: the TOC's datasets until the definition names its own
  expect_identical(.report_rows(q)$datasets[3L], "ADAE")
  # taken in again: not made again unless asked
  q$ard$analysis_data <- q$ard$analysis_data[q$ard$analysis_data$data_id != "adae_saf", ]
  q2 <- toc_apply(q, sp, toc_changes(q, sp), populations = pops, make_data = TRUE)
  expect_false("adae_saf" %in% .adata_rows(q2, "N1")$data_id)
  q3 <- toc_apply(q, sp, toc_changes(q, sp), populations = pops, make_data = TRUE, again = TRUE)
  expect_true("adae_saf" %in% .adata_rows(q3, "N1")$data_id)
  # without make_data: kept, nothing made
  q4 <- toc_apply(p, sp, toc_changes(p, sp))
  expect_identical(q4$outputs$datasets[2L], "ADSL | ADAE")
  expect_length(.adata_rows(q4)$data_id, 0L)
})

test_that("analysis data copied from another report: the same names, what they read along", {
  p <- add_output(set_report_population(pop_planner(), "T1", "SAF"), "T2")
  p <- set_analysis_data(p, "T1", "adae_saf", from = "ADAE", subjects_in = "adsl_saf",
                         where = "TRTEMFL == \"Y\"")
  # adae_saf alone asked for: adsl_saf (its subjects) comes along
  q <- import_analysis_data(p, "T1", "T2", "adae_saf")
  expect_setequal(attr(q, "copied"), c("adsl_saf", "adae_saf"))
  r <- .adata_rows(q, "T2")
  expect_identical(r$data_id, c("adsl_saf", "adae_saf"))
  expect_identical(r$subjects_in[2L], "adsl_saf")
  # T1's are as they were
  expect_identical(.adata_rows(q, "T1"), .adata_rows(p, "T1"))
  # again: the names T2 has get _1, and what they read follows the new name
  q2 <- import_analysis_data(q, "T1", "T2", "adae_saf")
  r2 <- .adata_rows(q2, "T2")
  expect_identical(r2$data_id, c("adsl_saf", "adae_saf", "adsl_saf_1", "adae_saf_1"))
  expect_identical(r2$subjects_in[4L], "adsl_saf_1")
  expect_error(import_analysis_data(p, "T1", "T2", "nope"), "No analysis data nope")
  # in use in a report is that report's: T2 may delete its copy though T1 reads its own
  expect_identical(.adata_rows(remove_analysis_data(q, "T2", "adae_saf"), "T2")$data_id, "adsl_saf")
})

test_that("the condition builder shows a variable on one line, the whole on hover", {
  expect_match(.cond_js, "tflCondRender", fixed = TRUE)
  expect_match(.cond_js, "title=", fixed = TRUE)
  h <- as.character(condition_builder_ui("x"))
  expect_match(h, "text-overflow: ellipsis", fixed = TRUE)
  # the chosen one beside selectize's input (a block took a line of its
  # own: the box twice as tall until opened); the options still a line each
  expect_match(h, ".selectize-input .item.tfl-cond-one { display: inline-block;", fixed = TRUE)
  expect_match(h, ".selectize-dropdown .option.tfl-cond-one { display: block; }", fixed = TRUE)
})

test_that("an analysis set's label: the sheet's own first, then the company standards' note", {
  p <- .standard_planner("LB-1")
  expect_true("label" %in% names(p$ard$populations))
  # a new study copies the company's note into the label
  expect_identical(p$ard$populations$label[p$ard$populations$population_id == "SAF"],
                   "Safety Analysis Set")
  expect_identical(.pop_label(p, "SAF"), "Safety Analysis Set")
  # the sheet's label wins
  p$ard$populations$label[p$ard$populations$population_id == "SAF"] <- "Safety Population"
  expect_identical(.pop_label(p, "SAF"), "Safety Population")
  # a blank label: the note as before
  p$ard$populations$label[p$ard$populations$population_id == "SAF"] <- NA
  expect_identical(.pop_label(p, "SAF"), "Safety Analysis Set")
})

test_that("step 1: a value the population select sends for another report is not written (#418)", {
  local_home()
  p <- pop_planner()
  p <- add_output(p, "T2")
  p <- set_report_population(p, "T1", "SAF")
  attr(p, "made") <- attr(p, "left") <- NULL
  create_study("RR", planner = p)
  shiny::testServer(server_for("RR"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    expect_match(output$ard_report_pop$html, "data-tfl-report=\"T1\"", fixed = TRUE)
    # A -> B -> A quickly: the select drawn for T2 (no set) sends "" after
    # T1 is chosen again
    session$setInputs(target = "T2")
    session$setInputs(target = "T1")
    session$setInputs(ard_report_pop_pick = "")
    expect_identical(report_population(rv$p, "T1"), "SAF")
    session$setInputs(ard_report_pop_pick_by = list(report = "T2", value = ""))
    expect_identical(report_population(rv$p, "T1"), "SAF")
    expect_true(is.na(report_population(rv$p, "T2")))
    # the user's change, on the report it was drawn for: written
    session$setInputs(ard_report_pop_pick_by = list(report = "T1", value = "ITT"))
    expect_identical(report_population(rv$p, "T1"), "ITT")
    session$setInputs(ard_report_pop_pick_by = list(report = "T1", value = ""))
    expect_true(is.na(report_population(rv$p, "T1")))
  })
})

test_that("step 1: a population not in the populations sheet stays a choice (#418)", {
  local_home()
  p <- pop_planner()
  p$outputs$population <- "FAS"
  create_study("RS", planner = p)
  shiny::testServer(server_for("RS"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    h <- output$ard_report_pop$html
    expect_match(h, "<option value=\"FAS\" selected>", fixed = TRUE)
  })
})

# (tflspec's .study_set_ids(): a tflspec without it knows no study's sets)
has_study_set_ids <- function() {
  !is.null(get0(".study_set_ids", envir = asNamespace("tflspec"), mode = "function", inherits = FALSE))
}

test_that("the sets a report is offered are the study's registered ones; none registered, none offered and said (review M3)", {
  p <- pop_planner()
  # no YAML spec, no data catalog: the sets are not known -- the sheet's, as before
  r <- .study_set_rows(p)
  expect_identical(r$population_id, c("SAF", "ITT"))
  expect_false(attr(r, "none"))
  expect_identical(.study_sets_none_label(r), "(not chosen)")
  expect_identical(.study_sets_none_label(r, none = "(none)"), "(none)")
  # a row with no id is no set
  q <- p
  q$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = c("SAF", NA), dataset = "ADSL", where = c("SAFFL == \"Y\"", "XXFL == \"Y\"")),
    "populations")
  expect_identical(.study_set_rows(q)$population_id, "SAF")
  # no data catalog and no set: not known to have none
  q$ard$populations <- p$ard$populations[0, , drop = FALSE]
  expect_false(attr(.study_set_rows(q), "none"))
  skip_if_not(has_study_set_ids())
  # a data catalog: its sets as the planner holds them (a set added in the
  # sheet and not saved yet is the study's too)
  q <- p
  q$spec <- list(study = list(data = list(datasets = list(list(dataset = "ADSL", path = "data/adam/adsl.rds")),
                                          populations = list(list(id = "SAF")))))
  r <- .study_set_rows(q)
  expect_identical(r$population_id, c("SAF", "ITT"))
  expect_false(attr(r, "none"))
  # a data catalog that names none: none offered, and the "none" item says so
  q$ard$populations <- p$ard$populations[0, , drop = FALSE]
  r <- .study_set_rows(q)
  expect_identical(nrow(r), 0L)
  expect_true(attr(r, "none"))
  expect_match(.study_sets_none_label(r), "register one in spec/data.yml populations", fixed = TRUE)
})

test_that("step 1 of a study that registers no set offers none and says so (review M3)", {
  skip_if_not(has_study_set_ids())
  local_home()
  p <- pop_planner()
  p$ard$populations <- p$ard$populations[0, , drop = FALSE]
  create_study("RN", planner = p)
  shiny::testServer(server_for("RN"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    h <- output$ard_report_pop$html
    expect_match(h, "register one in spec/data.yml populations", fixed = TRUE)
    expect_false(grepl("<option value=\"SAF\"", h, fixed = TRUE))
  })
})

test_that("a listing's analysis set is on its row too: what a save writes onto its data (codex review 3B)", {
  p <- add_output(pop_planner(), "L1", type = "listing")
  p <- set_lf_rows(p, "listings", "L1", data.frame(dataset = "ADAE", population_id = "SAF"))
  q <- set_report_population(p, "L1", "ITT")
  expect_identical(lf_rows(q, "listings", "L1")$population_id, "ITT")
  expect_identical(report_population(q, "L1"), "ITT")
  q2 <- set_report_population(q, "L1", NA)
  expect_true(is.na(lf_rows(q2, "listings", "L1")$population_id))
  # a table has no such row: its column alone
  expect_identical(report_population(set_report_population(q, "T1", "SAF"), "T1"), "SAF")
  expect_error(set_report_population(q, c("T1", "L1"), "SAF"), "One report")
})

test_that("a listing's set has one reader: its row's own, else the report list's (review S5)", {
  p <- add_output(pop_planner(), "L1", type = "listing")
  # the row's set, the report list's none: the row's (the data's own set wins, as tflspec reads it)
  p <- set_lf_rows(p, "listings", "L1", data.frame(dataset = "ADAE", population_id = "SAF"))
  expect_identical(report_population(p, "L1"), "SAF")
  expect_identical(.listing_row_set(p, "L1"), "SAF")
  # the row's none, the report list's SAF: the report list's, and the sheets
  # the program is written from have it on the row
  p <- set_lf_rows(p, "listings", "L1", data.frame(dataset = "ADAE"))
  p$outputs$population <- ifelse(p$outputs$output_id == "L1", "ITT", NA_character_)
  expect_true(is.na(.listing_row_set(p, "L1")))
  expect_identical(report_population(p, "L1"), "ITT")
  lf <- .lf_with_report_set(p, "L1")
  expect_identical(lf$listings$population_id[lf$listings$output_id == "L1"], "ITT")
  # a row of an analysis data: that data's set, nothing added
  p <- set_lf_rows(p, "listings", "L1", data.frame(data = "adae_x"))
  lf <- .lf_with_report_set(p, "L1")
  expect_true(is.na(lf$listings$population_id[lf$listings$output_id == "L1"]))
  # a table: the report list's alone
  expect_true(is.na(.listing_row_set(p, "T1")))
  expect_true(is.na(report_population(p, "T1")))
})

test_that("a TOC taken in moves a report's set where its data keeps it, its toc line with it, and says so (codex review 3B)", {
  local_home()
  s <- suppressMessages(sample_study_copy())
  p <- s$planner
  expect_false(is.null(p$spec))
  expect_identical(lf_rows(p, "listings", "L-16-2-7")$population_id, "SAF")
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Title",
               "L-16-2-7,Listing,Severe TEAEs",
               "F-14-2-3,Figure,KM curves",
               "T-14-1-1,Table,Demographics",
               "T-14-1-2,Table,Disposition",
               "T-99,Table,A new one"), f)
  sp <- tflspec::tfl_read_toc(f, map = c(output_id = "No.", type = "Kind", title = "Title"))
  pops <- c(`L-16-2-7` = "SCRF", `F-14-2-3` = "SCRF", `T-14-1-1` = "SCRF",
            `T-14-1-2` = report_population(p, "T-14-1-2"), `T-99` = "SAF")
  q <- toc_apply(p, sp, toc_changes(p, sp), populations = pops)
  toc_pop <- function(study, id) {
    e <- Filter(function(e) identical(e$id, id), study$toc$outputs)
    if (length(e)) e[[1L]]$population
  }
  # the listing: its row, its data's set, its toc line (L-16-2-7 kept SAF before)
  expect_identical(lf_rows(q, "listings", "L-16-2-7")$population_id, "SCRF")
  o <- q$spec$outputs[["L-16-2-7"]]
  expect_identical(o$data$population, "SCRF")
  expect_null(o$common$population)
  expect_identical(toc_pop(q$spec$study, "L-16-2-7"), "SCRF")
  expect_identical(report_population(q, "L-16-2-7"), "SCRF")
  # a figure's set on its data, a table's on its rows, never in common: (A2-T)
  f3 <- q$spec$outputs[["F-14-2-3"]]
  expect_identical(f3$data$population, "SCRF")
  expect_null(f3$common$population)
  expect_identical(toc_pop(q$spec$study, "F-14-2-3"), "SCRF")
  t1 <- q$spec$outputs[["T-14-1-1"]]
  expect_identical(tflspec::tfl_report_set(t1), "SCRF")
  expect_null(t1$common$population)
  expect_identical(toc_pop(q$spec$study, "T-14-1-1"), "SCRF")
  # the view's edits taken in, as a save takes them: the sets stay
  now <- .spec_now(q)
  expect_identical(now$outputs[["L-16-2-7"]]$data$population, "SCRF")
  expect_identical(now$outputs[["F-14-2-3"]]$data$population, "SCRF")
  expect_identical(toc_pop(now$study, "L-16-2-7"), "SCRF")
  # the summary: a line for each report already there whose set moved
  mv <- attr(q, "set_moved")
  expect_setequal(mv$output_id, c("L-16-2-7", "F-14-2-3", "T-14-1-1"))
  expect_identical(mv$from[mv$output_id == "L-16-2-7"], "SAF")
  expect_true(all(mv$to == "SCRF"))
  lines <- .toc_set_lines(mv)
  expect_length(lines, 3L)
  expect_true("L-16-2-7: set SAF -> SCRF" %in% lines)
  # the same set (T-14-1-2) and a new report (T-99): no line
  expect_false(any(c("T-14-1-2", "T-99") %in% mv$output_id))
  expect_identical(report_population(q, "T-99"), "SAF")
  # taken in again with the same sets: nothing moves, no line
  q2 <- toc_apply(q, sp, toc_changes(q, sp), populations = pops)
  expect_null(attr(q2, "set_moved"))
  expect_length(.toc_set_lines(attr(q2, "set_moved")), 0L)
})

test_that("a template that takes the set as an argument: every write of the report's set moves the argument with it (review H5)", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  p <- s$planner
  other <- setdiff(as.character(stats::na.omit(p$ard$populations$population_id)), "SAF")[1L]
  skip_if(is.na(other), "the sample study has one population")
  # forest_hr (a figure: its set on its data) and tflspec_v1/tflspec_hierarchy
  # (a table: its set in common:), each with a shown `population`
  tpls <- c(`F-H5-1` = "forest_hr", `T-H5-2` = "tflspec_v1/tflspec_hierarchy")
  if ("tfl_copy_templates" %in% getNamespaceExports("tflspec")) {
    invisible(tryCatch(tflspec::tfl_copy_templates(s$path, tpls[["T-H5-2"]]), error = function(e) NULL))
  }
  tpls <- tpls[vapply(tpls, function(nm) .tpl_pop_is_arg(.tpl_params(nm)), NA)]
  skip_if(!length(tpls), "this tflspec has no template that takes the set as an argument")
  set_of <- function(p, id) .o_population(p$spec$outputs[[id]])
  arg_of <- function(p, id) .tpl_of(p, id)$args$population
  agree <- function(p, id, want) {
    expect_identical(set_of(p, id), want, info = id)
    expect_identical(arg_of(p, id), want, info = id)
    expect_identical(report_population(p, id), want, info = id)
  }
  for (id in names(tpls)) {
    kind <- if (startsWith(id, "F-")) "figure" else "table"
    # (the argument alone: the report's set is its template's set argument, A2-T)
    # (a figure's group has no default: the report says it, PRINCIPLES 1.14)
    args <- c(list(population = "SAF"), if (identical(kind, "figure")) list(group = "TRT01A"))
    o <- tflspec::tfl_output(id, kind, label = id,
                             template = list(name = tpls[[id]], args = args))
    p <- .spec_add_output(p, o, label = id)
    expect_identical(set_of(p, id), "SAF", info = id)
    # a write of its template (no set given): the argument's set taken as
    # the report's, in both places
    p <- .write_report_population(p, id, output = p$spec$outputs[[id]])
    expect_identical(set_of(p, id), "SAF", info = id)
    expect_identical(arg_of(p, id), "SAF", info = id)
    # set_report_population() -- the report list, step 1: both move
    p <- set_report_population(p, id, other)
    agree(p, id, other)
    p <- set_report_population(p, id, "SAF")
    agree(p, id, "SAF")
    # (none: both taken out -- the report's set is then its template's own
    # default, canon 11-15 (1), as a report of that template that says none)
    q <- set_report_population(p, id, NA)
    bare <- tflspec::tfl_output(id, kind, label = id, template = list(name = tpls[[id]]))
    expect_identical(set_of(q, id), .o_population(bare), info = id)
    expect_null(arg_of(q, id))
  }
  # a figure's set on its data, a table's its template's argument, never in
  # common: (A2-T), the argument beside it
  if ("F-H5-1" %in% names(tpls)) expect_identical(p$spec$outputs[["F-H5-1"]]$data$population, "SAF")
  if ("T-H5-2" %in% names(tpls)) {
    expect_identical(arg_of(p, "T-H5-2"), "SAF")
    expect_null(p$spec$outputs[["T-H5-2"]]$common$population)
  }
  # a TOC taken in: both move
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  kinds <- ifelse(startsWith(names(tpls), "F-"), "Figure", "Table")
  writeLines(c("No.,Kind,Title", paste0(names(tpls), ",", kinds, ",", names(tpls))), f)
  sp <- tflspec::tfl_read_toc(f, map = c(output_id = "No.", type = "Kind", title = "Title"))
  pops <- stats::setNames(rep(other, length(tpls)), names(tpls))
  q <- toc_apply(p, sp, toc_changes(p, sp), populations = pops)
  for (id in names(tpls)) agree(q, id, other)
  # (the view's edits taken in, as a save takes them: they still agree)
  now <- .spec_now(q)
  for (id in names(tpls)) {
    expect_identical(.o_population(now$outputs[[id]]), other, info = id)
    expect_identical(.tpl_args_of(now$outputs[[id]]$template)$population, other, info = id)
  }
})

test_that("a report whose template takes no set argument gets none from the setter (review H5)", {
  o <- list(id = "F1", kind = "figure", template = list(name = "no/such_template", args = list(by = "TRT01A")))
  expect_false(.o_takes_set_arg(o))
  w <- .o_write_set(o, "SAF")
  expect_identical(w$data$population, "SAF")
  expect_null(w$template$args$population)
  # (a figure designer's template, a name: no template SPEC)
  expect_false(.o_takes_set_arg(list(id = "F2", kind = "figure", template = "forest_hr")))
  expect_true(is.na(.o_arg_population(o)))
})
