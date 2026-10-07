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
  expect_error(set_report_population(p, "T1", "PP"), "No analysis set")
  # none: the value goes, the data stays
  q <- set_report_population(p, "T1", NA)
  expect_true(is.na(report_population(q, "T1")))
  expect_identical(.adata_rows(q, "T1")$data_id, "adsl_saf")
  expect_silent(suppressWarnings(.ard_spec(p$ard)))
})

test_that("a report's analysis set changed: its analyses move to the new set's data", {
  p <- set_report_population(pop_planner(), "T1", "SAF")
  p <- set_analysis_data(p, "T1", "adae_saf", from = "ADAE", subjects = "adsl_saf",
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
  # T2's own: its adsl_saf and the AEs kept to it (no set of its own)
  p <- set_analysis_data(p, "T2", "adsl_saf", from = "ADSL", population_id = "SAF")
  p <- set_analysis_data(p, "T2", "adae_saf", from = "ADAE", subjects = "adsl_saf")
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
    session$setInputs(adata_id = "adae_flag", adata_label = "", adata_add = NULL,
                      adata_derive = "", adata_keep = NULL, adata_distinct = NULL, adata_code = "")
    session$setInputs(adata_save = 1)
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
  expect_identical(.pop_match(p, "Safety set"), "SAF")
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
                      modal_id = "T9", modal_desc = "x", modal_pop = "ITT")
    session$setInputs(add_ok = 1)
    expect_identical(report_population(rv$p, "T9"), "ITT")
    expect_true("adsl_itt" %in% .adata_rows(rv$p, "T9")$data_id)
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
  expect_identical(r$subjects[r$data_id == "adae_saf"], "adsl_saf")
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
  p <- set_analysis_data(p, "T1", "adae_saf", from = "ADAE", subjects = "adsl_saf",
                         where = "TRTEMFL == \"Y\"")
  # adae_saf alone asked for: adsl_saf (its subjects) comes along
  q <- import_analysis_data(p, "T1", "T2", "adae_saf")
  expect_setequal(attr(q, "copied"), c("adsl_saf", "adae_saf"))
  r <- .adata_rows(q, "T2")
  expect_identical(r$data_id, c("adsl_saf", "adae_saf"))
  expect_identical(r$subjects[2L], "adsl_saf")
  # T1's are as they were
  expect_identical(.adata_rows(q, "T1"), .adata_rows(p, "T1"))
  # again: the names T2 has get _1, and what they read follows the new name
  q2 <- import_analysis_data(q, "T1", "T2", "adae_saf")
  r2 <- .adata_rows(q2, "T2")
  expect_identical(r2$data_id, c("adsl_saf", "adae_saf", "adsl_saf_1", "adae_saf_1"))
  expect_identical(r2$subjects[4L], "adsl_saf_1")
  expect_error(import_analysis_data(p, "T1", "T2", "nope"), "No analysis data nope")
  # in use in a report is that report's: T2 may delete its copy though T1 reads its own
  expect_identical(.adata_rows(remove_analysis_data(q, "T2", "adae_saf"), "T2")$data_id, "adsl_saf")
})

test_that("the condition builder shows a variable on one line, the whole on hover", {
  expect_match(.cond_js, "tflCondRender", fixed = TRUE)
  expect_match(.cond_js, "title=", fixed = TRUE)
  h <- as.character(condition_builder_ui("x"))
  expect_match(h, "text-overflow: ellipsis", fixed = TRUE)
})
