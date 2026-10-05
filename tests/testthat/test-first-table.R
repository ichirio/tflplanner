test_that("data files go into the data catalog once, with name and level", {
  p <- new_planner()
  files <- data.frame(folder = c("data/adam", "data/adam", "data/sdtm", "data/other"),
                      file = c("adsl.rds", "notes.txt", "dm.xpt", "x.rds"),
                      stringsAsFactors = FALSE)
  p2 <- catalog_add_files(p, files)
  expect_identical(attr(p2, "added"), c("ADSL", "DM"))
  ds <- ard_rows(p2, "datasets")
  expect_identical(ds$level, c("ADaM", "SDTM"))
  expect_identical(ds$path, c("data/adam/adsl.rds", "data/sdtm/dm.xpt"))
  # again: nothing more
  expect_length(attr(catalog_add_files(p2, files), "added"), 0L)
})

test_that("first_table() writes every sheet a summary table needs", {
  d <- data.frame(SAFFL = c("Y", "Y"), TRT01A = c("B", "A"), TRT01AN = c(1, 2),
                  AGE = c(50, 60),
                  SEX = c("F", "M"), TRTSDT = as.Date(c("2020-01-01", "2020-01-02")),
                  stringsAsFactors = FALSE)
  attr(d$AGE, "label") <- "Age"
  p <- first_table(new_planner(), "T-DM", "data/adam/adsl.rds", d,
                   population = "SAFFL", group = "TRT01A",
                   variables = c("AGE", "SEX"), description = "Demographics",
                   stack = FALSE)
  expect_true("T-DM" %in% p$outputs$output_id)
  expect_identical(report_info(p, "T-DM")$type, "table")
  expect_identical(ard_rows(p, "datasets")$dataset, "ADSL")
  po <- ard_rows(p, "populations")
  expect_identical(po$population_id, "SAF")
  expect_identical(po$where, "SAFFL == \"Y\"")
  an <- ard_rows(p, "analyses", "T-DM")
  # the N per group first (column headers), then one analysis per call:
  # the numbers together, the counts together
  expect_identical(an$analysis_id, c("BIGN", "CONT", "CAT"))
  # written as the cards functions (the same analyses as the keywords)
  expect_identical(an$method, c("cards::ard_tabulate", "cards::ard_summary",
                                "cards::ard_tabulate"))
  expect_identical(an$by, c(NA, "TRT01A", "TRT01A"))
  expect_identical(an$variables, c("TRT01A", "AGE", "SEX"))
  expect_identical(an$population_id, c("SAF", "SAF", "SAF"))
  tb <- sheet_rows(p, "tables", "T-DM")
  expect_identical(tb$cols, "TRT01A")
  expect_identical(tb$rows, "group = variable")
  vr <- sheet_rows(p, "variables", "T-DM")
  # the groups in the order of TRT01AN, then the rows with the data's labels
  expect_identical(vr$variable, c("TRT01A", "AGE", "SEX"))
  expect_identical(vr$levels[1], "B | A")
  expect_identical(vr$order, c(NA, "1", "2"))
  expect_identical(vr$label, c(NA, "Age", NA))
  # a second table on the same data reuses the dataset and the analysis set
  p <- first_table(p, "T-2", "data/adam/adsl.rds", d, "SAFFL", "TRT01A", "AGE")
  expect_identical(nrow(ard_rows(p, "populations")), 1L)
  expect_identical(nrow(ard_rows(p, "datasets")), 1L)
  expect_error(first_table(p, "T-2", "data/adam/adsl.rds", d, "SAFFL", "TRT01A", "AGE"),
               "has analyses already")
  expect_error(first_table(p, "T-3", "data/adam/adsl.rds", d, "SAFFL", "TRT01A", "TRTSDT"),
               "Dates")
})

test_that("Add > Table from the data: data file to a previewed table in one form", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("F1")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  shiny::testServer(server_for("F1"), {
    rv <- session$userData$rv
    # Refresh on the Data tab: the file joins the data catalog
    session$setInputs(data_refresh = 1)
    expect_true("ADSL" %in% ard_rows(rv$p, "datasets")$dataset)
    session$setInputs(add = 1, modal_type = "table", modal_first = TRUE,
                      modal_id = "T-DM", modal_desc = "Demographics")
    session$setInputs(mf_data = "data/adam/adsl.rds")
    expect_match(output$mf_cols$html, "SAFFL")
    session$setInputs(mf_pop = "SAFFL", mf_group = "TRT01A",
                      mf_vars = c("AGE", "SEX"))
    session$setInputs(add_ok = 1)
    expect_true("T-DM" %in% rv$p$outputs$output_id)
    expect_identical(nrow(ard_rows(rv$p, "analyses", "T-DM")), 3L)
    # saved, previewed and read: the builder can start
    s2 <- open_study("F1")
    st <- ard_status(s2)
    expect_identical(st$state[st$output_id == "T-DM"], "built")
    expect_false(is.null(ard_info(s2, "T-DM")))
  })
})

test_that("dates, times and their flags are not offered as rows or groups", {
  cols <- list(AGE = c(1, 2), RFSTDTC = c("2014-01-02", "2014-02-03"),
               TRTSDT = as.Date("2014-01-01"), TRTSTMF = c("H", "M"),
               SEX = c("F", "M"), TRT01A = c("A", "B"), TRTETMF = c("H", NA))
  attr(cols$AGE, "label") <- "Age"
  r <- .row_choices(cols)
  expect_identical(unname(r), c("AGE", "SEX", "TRT01A"))
  expect_identical(names(r)[1], "AGE — Age (numbers)")
  expect_identical(unname(.group_choices(cols)), "TRT01A")
})

test_that("read_data_head() keeps the columns' labels", {
  f <- tempfile(fileext = ".rds")
  d <- data.frame(AGE = 1:3)
  attr(d$AGE, "label") <- "Age"
  saveRDS(d, f)
  expect_identical(attr(read_data_head(f, 2L)$AGE, "label"), "Age")
})

test_that("a row of the analyses grid is edited as a form", {
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("AF")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  adsl <- cards::ADSL
  attr(adsl$AGE, "label") <- "Age"
  saveRDS(adsl, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", adsl,
                           "SAFFL", "TRT01A", c("AGE", "SEX"), stack = FALSE)
  save_study(s)
  shiny::testServer(server_for("AF"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    # the first analysis (BIGN) until a row is clicked
    expect_match(output$ard_stat_ui$html, "Analysis BIGN of T1")
    expect_match(output$ard_stat_ui$html, "What to compute", fixed = TRUE)
    # a click on the grid's second row: CONT (AGE)
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 2L)))
    h <- output$ard_stat_ui$html
    expect_match(h, "Analysis CONT of T1")
    st_env <- session$userData$st_env
    id <- function(x) paste0("st", st_env$n, "_", x)
    v <- output$ard_an_vars$html
    expect_match(v, "AGE \u2014 Age")
    # a continuous analysis offers numbers only as its variables
    vars_part <- sub("_strata-label.*", "", sub(".*_vars-label", "", v))
    expect_false(grepl('value="SEX"', vars_part))
    expect_true(grepl('value="HEIGHTBL"', vars_part))
    inp <- list()
    inp[[id("id")]] <- "CONT"
    inp[[id("label")]] <- "Age (years)"
    inp[[id("fn_pick")]] <- "continuous"
    # the data: one choice of dataset x analysis set, named as the program
    # names it
    expect_match(h, "ADSL × SAF (pop_saf: ", fixed = TRUE)
    expect_false(grepl("(the analysis set's", h, fixed = TRUE))
    # the wizard wrote the dataset out: the same data as the analysis set's
    expect_match(h, 'value="ADSL|SAF" selected', fixed = TRUE)
    expect_false(grepl('value="|SAF"', h, fixed = TRUE))
    inp[[id("data")]] <- "|SAF"
    inp[[id("by")]] <- "TRT01A"
    inp[[id("vars")]] <- c("AGE", "HEIGHTBL")
    inp[[id("where")]] <- "AGE >= 18"
    inp[[id("pick")]] <- c("N", "mean", "sd")
    inp[[id("strata")]] <- "SEX"
    inp[[id("den")]] <- ""
    do.call(session$setInputs, inp)
    session$setInputs(ard_stat_apply = 1)
    a <- ard_rows(rv$p, "analyses", "T1")
    r <- a[a$analysis_id == "CONT", ]
    expect_identical(r$label, "Age (years)")
    expect_true(is.na(r$dataset))
    expect_identical(r$population_id, "SAF")
    # strata and the denominator are the row's columns (blank: none)
    expect_identical(r$strata, "SEX")
    expect_true(is.na(r$denominator))
    expect_identical(r$variables, "AGE | HEIGHTBL")
    expect_identical(r$where, "AGE >= 18")
    expect_identical(r$statistics, "N | mean | sd")
    # a subset that is not R is refused
    inp2 <- list(); inp2[[id("where")]] <- "AGE >="
    do.call(session$setInputs, inp2)
    session$setInputs(ard_stat_apply = 2)
    a <- ard_rows(rv$p, "analyses", "T1")
    expect_identical(a$where[a$analysis_id == "CONT"], "AGE >= 18")
    # a new analysis
    session$setInputs(ard_an_new = 1)
    expect_true("A1" %in% ard_rows(rv$p, "analyses", "T1")$analysis_id)
  })
})

test_that("a study made without the company defaults can take them, missing ones only", {
  local_home()
  p <- new_planner()
  p <- set_ard_rows(p, "populations", "", data.frame(
    population_id = "SAF", dataset = "ADSL", where = "MYFL == \"Y\""))
  p2 <- add_standard_defaults(p, "X1")
  expect_true(all(c("layout", "col_header", "columns", "populations",
                    "datasets") %in% attr(p2, "added")))
  expect_true(any(sheet_rows(p2, "col_header", NA)$text %in% "(N={n})"))
  # the study's own analysis set stays as it was
  po <- ard_rows(p2, "populations")
  expect_identical(po$where[po$population_id == "SAF"], "MYFL == \"Y\"")
  # again: nothing to add
  expect_length(attr(add_standard_defaults(p2, "X1"), "added"), 0L)
})

test_that("blank statistics show the method's defaults", {
  expect_identical(.method_default_stats("continuous")[1:3], c("N", "mean", "sd"))
  expect_identical(.method_default_stats("result"), character())
})

test_that("another study with unsaved changes asks: save and open, or open without saving", {
  local_home()
  two_studies()
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    rv$p$outputs$description[1] <- "changed"
    session$setInputs(studies_dbl = match("S2", list_studies()$study_id))
    session$setInputs(open_discard = 1)
    expect_identical(rv$study$meta$study_id, "S2")
    # the changes were kept as S1's draft
    expect_false(is.null(.read_draft("S1")))
  })
})

test_that("a template says whether the study's data can draw it", {
  expect_true(.fig_data_ok("ADTTE", c("ADSL", "ADTTE")))
  expect_false(.fig_data_ok("ADTR + ADRS", c("ADSL", "ADRS")))
  expect_true(.fig_data_ok("ADLB / ADVS + ADSL", c("adsl", "advs")))
  expect_false(.fig_data_ok("ADLB / ADVS + ADSL", c("ADSL", "ADAE")))
  expect_true(.fig_data_ok("(your estimates)", character()))
  expect_true(.fig_data_ok(NA, character()))
  expect_identical(.fig_first_data("ADLB / ADVS + ADSL", c("ADSL", "ADVS")), "ADVS")
  tp <- data.frame(template = c("km", "ae"), kind = c("km", "ae_dot"),
                   label = c("KM", "AE dots"), category = c("Efficacy", "Safety"),
                   data = c("ADTTE", "ADAE + ADSL"), stringsAsFactors = FALSE)
  w <- list(label = identity, missing = "no data", other = "Other", empty = "Empty")
  tc <- .fig_template_choices(tp, c("ADSL", "ADAE"), w)
  expect_identical(names(tc$choices), c("Efficacy", "Safety", "Other"))
  expect_identical(tc$off, "km")
  expect_identical(tc$first, "ae")
  # of the drawable ones, the one using most of the study's data
  tp3 <- rbind(tp, data.frame(template = "sw", kind = "swimmer", label = "Swimmer",
                              category = "Efficacy", data = "ADSL"))
  tp3 <- tp3[c(3, 1, 2), ]
  expect_identical(.fig_template_choices(tp3, c("ADSL", "ADAE"), w)$first, "ae")
  # a tflspec without the columns: grouped by kind, nothing greyed
  tc2 <- .fig_template_choices(tp[c("template", "kind", "label")], character(), w)
  expect_identical(names(tc2$choices)[1:2], c("km", "ae_dot"))
  expect_length(tc2$off, 0L)
  # no columns read: no choices, not an error
  expect_length(.group_choices(list()), 0L)
  expect_length(.labelled(NULL, list()), 0L)
})

test_that("first_listing() writes the listing and its columns from the data", {
  d <- data.frame(TRTA = "A", USUBJID = "1", AEDECOD = "X", ASTDT = as.Date("2020-01-01"),
                  stringsAsFactors = FALSE)
  attr(d$AEDECOD, "label") <- "Preferred Term"
  p <- first_listing(new_planner(), "L-AE", "data/adam/adae.rds", d,
                     columns = c("USUBJID", "AEDECOD", "ASTDT"), group = "TRTA")
  expect_identical(report_info(p, "L-AE")$type, "listing")
  l <- lf_rows(p, "listings", "L-AE")
  expect_identical(l$dataset, "ADAE")
  expect_identical(l$sort, "TRTA | USUBJID | ASTDT")
  lc <- lf_rows(p, "listing_cols", "L-AE")
  expect_identical(lc$vars, c("TRTA", "USUBJID", "AEDECOD", "ASTDT"))
  expect_identical(lc$label[3], "Preferred Term")
  # widths from the data and the header's longest word
  expect_identical(lc$width[3], "9")
  expect_error(first_listing(p, "L-AE", "data/adam/adae.rds", d, "USUBJID"),
               "has columns already")
})

test_that("Add > Listing from the data: a listing shown in one form", {
  skip_on_cran()
  local_home()
  s <- create_study("L1")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADAE, file.path(s$path, "data/adam/adae.rds"))
  shiny::testServer(server_for("L1"), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "listing", modal_first_l = TRUE,
                      modal_id = "L-AE", modal_desc = "AEs")
    session$setInputs(ml_data = "data/adam/adae.rds")
    expect_match(output$ml_cols$html, "AEDECOD")
    session$setInputs(ml_group = "TRTA", ml_cols_pick = c("USUBJID", "AEDECOD", "AESEV"))
    session$setInputs(add_ok = 1, target = "L-AE", nav = "make", step = "content", content_nav = "content")
    expect_identical(lf_rows(rv$p, "listing_cols", "L-AE")$vars,
                     c("TRTA", "USUBJID", "AEDECOD", "AESEV"))
    h <- output$lf_preview_out$html
    expect_false(grepl("alert-danger", h))
    # the preview is shown, not the invitation to preview
    expect_false(grepl("Preview the listing to see", h))
  })
})

test_that("a new figure starts from a template the study's data can draw", {
  skip_on_cran()
  skip_if_not("data" %in% names(tflspec::tfl_fig_templates()))
  local_home()
  s <- create_study("G1")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data/adam/adae.rds"))
  s$planner <- add_output(s$planner, "F-AGE", type = "figure")
  save_study(s)
  shiny::testServer(server_for("G1"), {
    session$setInputs(target = "F-AGE", nav = "make", step = "content", content_nav = "content")
    h <- output$pd_body$html
    # ADTTE templates are listed but cannot be chosen, and say why
    expect_match(h, "needs data this study has not got")
    expect_match(h, 'value="km_risk_table" disabled')
    # the first step: a template or empty, both the designer's layers
    expect_match(h, "First step")
    expect_match(h, "pd_empty")
    # the note of a new figure, and the user code below, folded
    expect_match(output$pd_note$html, "A new figure")
    expect_match(output$lf_fig_box$html, "User code")
    expect_false(grepl("<details[^>]*open", output$lf_fig_box$html))
    # a drawable template's form: no R error
    session$setInputs(pd_from = "template", pd_tpl = "ae_dot_incidence",
                      pd_tpl_data = "ADAE")
    expect_false(grepl("attempt to set", output$pd_tpl_more$html))
    # applied: the designer, its Spec named, and no user code
    session$setInputs(pd_start = 1)
    d <- fig_design(session$userData$rv$p, "F-AGE")
    expect_true(length(d$layers) > 0L)
    h <- output$pd_body$html
    expect_match(h, "spec/figures/F-AGE.yml", fixed = TRUE)
    expect_match(h, "Spec (YAML)", fixed = TRUE)
    expect_match(h, "pd_retpl")
    expect_error(output$lf_fig_box)
    # a template again: asked first, then the design is replaced
    session$setInputs(pd_retpl = 1)
    session$setInputs(pd_tpl = "", pd_retpl_ok = 1)
    expect_length(fig_design(session$userData$rv$p, "F-AGE")$layers, 0L)
  })
})

test_that("a figure starts empty, and a figure written by hand shows its two ways", {
  skip_on_cran()
  local_home()
  s <- create_study("G2")
  s$planner <- add_output(s$planner, "F-E", type = "figure")
  s$planner <- add_output(s$planner, "F-U", type = "figure")
  s$planner$outputs$data_code[s$planner$outputs$output_id == "F-U"] <-
    "plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram()"
  save_study(s)
  shiny::testServer(server_for("G2"), {
    session$setInputs(target = "F-U", nav = "make", step = "content", content_nav = "content")
    # written by hand: keep the code (a user-code report) or the designer
    expect_match(output$pd_note$html, "written by hand. Two ways", fixed = TRUE)
    expect_match(output$pd_note$html, "fig_to_user", fixed = TRUE)
    expect_match(output$lf_fig_box$html, "<details[^>]*open")
    session$setInputs(target = "F-E")
    session$setInputs(pd_empty = 1)
    d <- fig_design(session$userData$rv$p, "F-E")
    expect_identical(d$data[[1L]]$step, "read")
    expect_length(d$layers, 0L)
  })
})

test_that("a report's ARD can be given the subjects per group", {
  p <- set_ard_rows(new_planner(), "analyses", "T1", data.frame(
    analysis_id = "AGE", method = "continuous", dataset = "ADSL",
    population_id = "SAF", by = "TRT01A", variables = "AGE"))
  expect_false(.has_group_n(p, "T1", "TRT01A"))
  p <- add_group_n(p, "T1", "TRT01A")
  a <- ard_rows(p, "analyses", "T1")
  expect_identical(a$analysis_id, c("BIGN", "AGE"))
  expect_identical(a$variables[1], "TRT01A")
  expect_true(is.na(a$by[1]))
  expect_identical(a$population_id[1], "SAF")
  expect_true(.has_group_n(p, "T1", "TRT01A"))
})

test_that("the data of an analysis is one choice of dataset and analysis set", {
  po <- data.frame(population_id = c("SAF", "ITT"), dataset = c("ADSL", "ADSL"))
  w <- list(with = "%s x %s (%s)", alone = "%s alone (%s)", none = "(none)")
  ch <- .an_data_choices(c("ADSL", "ADAE"), po, "|SAF", w)
  expect_identical(unname(ch), c("|SAF", "ADAE|SAF", "|ITT", "ADAE|ITT",
                                 "ADSL|", "ADAE|"))
  expect_identical(names(ch)[1:2], c("ADSL x SAF (pop_saf)", "ADAE x SAF (adae_saf)"))
  expect_identical(names(ch)[5], "ADSL alone (adsl)")
  # a choice the analysis has that is none of these is kept
  ch2 <- .an_data_choices("ADSL", po, "ADVS|SAF", w)
  expect_true("ADVS|SAF" %in% ch2)
  expect_identical(.an_data_choices("ADSL", po, "|", w)[[1]], "|")
  # the analysis set's dataset written out takes the place of the blank one
  ch3 <- .an_data_choices(c("ADSL", "ADAE"), po, "ADSL|SAF", w)
  expect_identical(unname(ch3)[1], "ADSL|SAF")
  expect_false("|SAF" %in% ch3)
  expect_identical(.an_data_split("ADAE|SAF"), list(dataset = "ADAE", pop = "SAF", data = NA_character_))
  expect_identical(.an_data_split("|SAF"), list(dataset = NA_character_, pop = "SAF", data = NA_character_))
  expect_identical(.an_data_value(NA, "SAF"), "|SAF")
})

test_that("the wizard puts the numbers in one analysis and the counts in another", {
  d <- data.frame(SAFFL = "Y", TRT01A = c("A", "B"), AGE = c(50, 60),
                  SEX = c("F", "M"), BMIBL = c(20.1, 25.3), RACE = c("X", "Y"))
  p <- first_table(new_planner(), "T-M", "data/adam/adsl.rds", d, "SAFFL",
                   "TRT01A", c("AGE", "SEX", "BMIBL", "RACE"), stack = FALSE)
  an <- ard_rows(p, "analyses", "T-M")
  expect_identical(an$analysis_id, c("BIGN", "CONT", "CAT"))
  expect_identical(an$variables, c("TRT01A", "AGE | BMIBL", "SEX | RACE"))
  # the rows keep the order they were chosen in
  vr <- sheet_rows(p, "variables", "T-M")
  expect_identical(vr$order[match(c("AGE", "SEX", "BMIBL", "RACE"), vr$variable)],
                   c("1", "2", "3", "4"))
  # only counts: no CONT
  p2 <- first_table(new_planner(), "T-C", "data/adam/adsl.rds", d, "SAFFL",
                    "TRT01A", c("SEX", "RACE"), stack = FALSE)
  expect_identical(ard_rows(p2, "analyses", "T-C")$analysis_id, c("BIGN", "CAT"))
})

test_that("the wizard runs the analyses together (ard_stack), unless a subject has no group", {
  d <- data.frame(SAFFL = "Y", TRT01A = c("A", "B"), AGE = c(50, 60),
                  SEX = c("F", "M"), RACE = c("X", "Y"))
  p <- first_table(new_planner(), "T-S", "data/adam/adsl.rds", d, "SAFFL",
                   "TRT01A", c("AGE", "SEX", "RACE"), description = "Demographics")
  an <- ard_rows(p, "analyses", "T-S")
  expect_identical(an$analysis_id, c("STACK", "CONT", "CAT"))
  expect_identical(an$parent, c(NA, "STACK", "STACK"))
  expect_identical(an$method[1], "cards::ard_stack")
  expect_identical(an$label[1], "Demographics")
  expect_identical(c(an$dataset[1], an$population_id[1], an$by[1]), c("ADSL", "SAF", "TRT01A"))
  expect_true(all(is.na(an$dataset[2:3])) && all(is.na(an$by[2:3])))
  expect_identical(an$args[1], ".total_n = TRUE")
  expect_identical(attr(p, "group_missing"), 0L)
  # the stack counts the subjects per group: the column headers' N
  expect_true(.has_group_n(p, "T-S", "TRT01A"))
  # a subject of the analysis set with no group: one by one
  d$TRT01A[2] <- NA
  p2 <- first_table(new_planner(), "T-S", "data/adam/adsl.rds", d, "SAFFL",
                    "TRT01A", c("AGE", "SEX"))
  expect_identical(ard_rows(p2, "analyses", "T-S")$analysis_id, c("BIGN", "CONT", "CAT"))
  expect_identical(attr(p2, "group_missing"), 1L)
})
