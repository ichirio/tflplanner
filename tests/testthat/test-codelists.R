# A report's code lists: every row a report's; copied from another report
# or from the company standards; step 1's editor

cl_planner <- function() {
  p <- add_output(new_planner(), "T1")
  p <- add_output(p, "T2")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds",
                               derive = "AGEGRP = ifelse(AGE < 65, \"<65\", \">=65\")")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("T1", "T1"), analysis_id = c("SEX", "AGE"),
    method = c("categorical", "continuous"), population_id = "SAF",
    by = "TRT01A", variables = c("SEX", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("a code list is a report's: set, copied from another report", {
  p <- cl_planner()
  sex <- data.frame(variable = "SEX", value = c("F", "M"), label = c("Female", "Male"),
                    order = c("1", "2"))
  expect_error(set_codelist(p, NA, sex), "a report's")
  expect_error(set_codelist(p, "T9", sex), "a report's")
  p <- set_codelist(p, "T1", sex)
  expect_identical(sheet_rows(p, "codelists", "T1")$label, c("Female", "Male"))
  expect_identical(nrow(sheet_rows(p, "codelists", NA)), 0L)
  # a value again: replaced; the others kept
  p <- set_codelist(p, "T1", data.frame(variable = c("SEX", "RACE"), value = c("F", "ASIAN")))
  d <- sheet_rows(p, "codelists", "T1")
  expect_identical(paste(d$variable, d$value, d$label),
                   c("SEX M Male", "SEX F NA", "RACE ASIAN NA"))
  # into T2: its own rows; T1's stay
  q <- import_codelist(p, "T1", "T2", "SEX")
  expect_identical(sheet_rows(q, "codelists", "T2")$value, c("M", "F"))
  expect_identical(sheet_rows(q, "codelists", "T1"), sheet_rows(p, "codelists", "T1"))
  q <- set_codelist(q, "T2", data.frame(variable = "SEX", value = "M", label = "Men"))
  expect_identical(sheet_rows(q, "codelists", "T1")$label[1L], "Male")
  expect_error(import_codelist(p, "T1", "T2", "AGEGR1"), "no code list of")
  expect_error(import_codelist(p, "T2", "T1"), "no code lists to copy")
  # a report copied, renamed, removed: its code lists with it
  q <- copy_output(p, "T1", "T3")
  expect_identical(nrow(sheet_rows(q, "codelists", "T3")), 3L)
  q <- remove_output(rename_output(q, "T3", "T4"), "T4")
  expect_identical(q$sheets$codelists, p$sheets$codelists)
})

test_that("the company standards' code lists, and the variables a report uses", {
  local_home()
  s <- standard_codelists()
  expect_true(all(c("SEX", "RACE", "ETHNIC", "AESEV", "AESER", "AEREL", "AEOUT",
                    "EOSSTT") %in% s$set))
  expect_identical(standard_codelists("AESEV")$value, c("MILD", "MODERATE", "SEVERE"))
  expect_identical(names(s), c("set", "variable", "value", "label", "order", "note"))
  # a company's workbook without set (and label, order, note): read, set = variable
  f <- withr::local_tempfile(fileext = ".xlsx")
  std <- .builtin_standards()
  std$codelists <- data.frame(variable = c("ARM", "ARM"), value = c("A", "B"))
  writexl::write_xlsx(std, f)
  r <- read_standards(f)$codelists
  expect_identical(names(r), names(s))
  # the variables: what the ARD reads, what is derived, what the table shows
  p <- cl_planner()
  p <- set_sheet_rows(p, "variables", "T1", data.frame(variable = "AGEGR1", label = "Age group"))
  v <- .codelist_vars(p, "T1")
  expect_true(all(c("TRT01A", "SEX", "AGE", "AGEGRP", "AGEGR1") %in% v))
  expect_false("variable" %in% v)
  expect_identical(.codelist_vars(p, NULL), character())
})

test_that("1-1: the report's code lists, copied in, a file read", {
  skip_on_cran()
  local_home()
  p <- cl_planner()
  p <- set_analysis_data(p, "T1", "adsl_saf", from = "ADSL", population_id = "SAF")
  p <- set_codelist(p, "T2", data.frame(variable = c("TRT01A", "TRT01A", "PARAMCD"),
                                        value = c("Placebo", "Drug", "ALT"),
                                        label = c("Placebo", "Drug X", "ALT")))
  p <- set_codelist(p, "T1", data.frame(variable = "COUNTRY", value = "JPN"))
  create_study("CL", planner = p)
  shiny::testServer(server_for("CL"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_adata_pick = "adsl_saf")
    session$setInputs(adata_id = "adsl_saf", adata_add = "TRT01A", adata_keep = NULL)
    # the copy dialog straight from part 4: T2's choices preselect what
    # this data has
    session$setInputs(cl21_copy = 1, cl21_from = "T2")
    expect_match(output$cl21_which$html, "TRT01A: Placebo, Drug", fixed = TRUE)
    session$setInputs(cl21_pick = "TRT01A", cl21_as_var = "TRT01A", cl21_copy_ok = 1)
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "TRT01A"], c("Placebo", "Drug"))
    # part 4 shows what the program makes of TRT01A's values
    expect_match(output$adata_cl$html, "Drug \u2192 Drug X", fixed = TRUE)
    # the editor: this data's columns (TRT01A), COUNTRY hidden and said so
    session$setInputs(cl21_open = 1)
    expect_false(grepl("COUNTRY", paste(output$cl21_hot, collapse = ""), fixed = TRUE))
    expect_match(output$cl21_hidden$html, "1", fixed = TRUE)
    session$setInputs(cl21_all = TRUE)
    expect_true(grepl("COUNTRY", paste(output$cl21_hot, collapse = ""), fixed = TRUE))
    # the company standards' SEX, as SEX
    session$setInputs(cl21_copy = 2, cl21_from = ".std")
    session$setInputs(cl21_pick = "SEX", cl21_as_var = "SEX", cl21_copy_ok = 2)
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "SEX"], c("F", "M", "U"))
    # a file read into T1
    f <- withr::local_tempfile(fileext = ".csv")
    writeLines(c("variable,value,label", "RACE,ASIAN,Asian"), f)
    session$setInputs(cl21_file = data.frame(name = "cl.csv", datapath = f))
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$label[d$variable == "RACE"], "Asian")
    expect_identical(nrow(sheet_rows(rv$p, "codelists", NA)), 0L)
    session$setInputs(cl21_done = 1)
  })
})

test_that("a study of the old format: said once, and why step 2 is empty", {
  skip_on_cran()
  local_home()
  p <- cl_planner()
  p$ard$analysis_data <- .normalize_ard_sheet(data.frame(
    output_id = "T1", data_id = "adsl_saf", from = "ADSL", population_id = "SAF"),
    "analysis_data")
  expect_length(.old_format(p), 0L)
  old <- p
  old$ard$analysis_data$output_id <- NA_character_
  old$sheets$codelists <- .normalize_sheet(data.frame(
    output_id = NA_character_, variable = "SEX", value = "F"), "codelists")
  expect_identical(.old_format(old), c("analysis_data", "codelists"))
  # a sheet of nothing but a blank row is not old
  blank <- p
  blank$sheets$codelists <- .normalize_sheet(data.frame(output_id = NA_character_,
                                                        variable = NA_character_), "codelists")
  expect_length(.old_format(blank), 0L)
  # a study saved before: its analysis data without a report
  create_study("OLD", planner = p)
  s <- open_study("OLD")
  s$planner$ard$analysis_data$output_id <- NA_character_
  .write_state(s, tflplanner_home())
  shiny::testServer(server_for("OLD"), {
    rv <- session$userData$rv
    expect_identical(.old_format(rv$p), "analysis_data")
    session$setInputs(nav = "make", step = "ard", target = "T1")
    expect_match(output$ard_adata$html, "An old format", fixed = TRUE)
    expect_false(grepl("None yet", output$ard_adata$html, fixed = TRUE))
    expect_match(output$ard_2_2_head$html, "An old format", fixed = TRUE)
    chk <- output$ard_check$html
    expect_match(chk, "An old format", fixed = TRUE)
    expect_match(chk, "<details>", fixed = TRUE)
  })
})

test_that("a drag list's levels show their code list's text, the values given back", {
  cl <- data.frame(variable = c("SEX", "SEX"), value = c("F", "M"), label = c("Female", NA))
  x <- .levels_with_labels(c("F", "M", "U"), "SEX", cl)
  expect_identical(names(x), c("F", "M", "U"))
  expect_match(as.character(x[[1L]]), "text-muted small ms-1\">Female", fixed = TRUE)
  expect_false(grepl("text-muted", as.character(x[[2L]]), fixed = TRUE))
  # nothing to show: the values as they are
  expect_identical(.levels_with_labels(c("A", "B"), "ARM", cl), c("A", "B"))
  expect_identical(.levels_with_labels(character(), "SEX", cl), character())
})

test_that("1-1 part 4: the values a list does not have, added from there", {
  skip_on_cran()
  local_home()
  p <- cl_planner()
  p <- set_analysis_data(p, "T1", "adsl_saf", from = "ADSL", population_id = "SAF",
                         derive = "OLD = ifelse(AGE >= 75, \"Y\", \"N\")")
  p <- set_codelist(p, "T1", data.frame(variable = c("SEX", "OLD", "OLD", "COUNTRY"),
                                        value = c("F", "Y", "N", "JPN"),
                                        label = c("Female", "75 or older", "Under 75", NA)))
  s <- create_study("CD", planner = p)
  dir.create(file.path(s$path, "data", "adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(USUBJID = c("1", "2", "3"), SEX = c("F", "M", NA), AGE = c(70, 80, 60),
                     SAFFL = "Y"),
          file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("CD"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_adata_pick = "adsl_saf")
    expect_match(output$adata_detail$html, "Column definitions", fixed = TRUE)
    session$setInputs(adata_id = "adsl_saf", adata_from = "ADSL", adata_add = NULL, adata_keep = NULL)
    h <- output$adata_cl$html
    # the columns of this data: SEX (ADSL's) and OLD (made), not COUNTRY
    expect_match(h, "F \u2192 Female", fixed = TRUE)
    expect_match(h, "Y \u2192 75 or older, N \u2192 Under 75", fixed = TRUE)
    expect_false(grepl("COUNTRY", h, fixed = TRUE))
    # M: ADSL has it, the list not (a blank is no value)
    expect_match(h, "SEX: the data has values its code list does not", fixed = TRUE)
    expect_match(h, "would stop): M<", fixed = TRUE)
    session$setInputs(adata_cl_add = "SEX")
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "SEX"], c("F", "M"))
    expect_identical(d$label[d$variable == "SEX"], c("Female", "M"))
    expect_false(grepl("would stop", output$adata_cl$html, fixed = TRUE))
    # nothing added to another report
    expect_identical(nrow(sheet_rows(rv$p, "codelists", "T2")), 0L)
    # step 2's table points to 1-1
    session$setInputs(cl3_open = 1)
  })
})

test_that("a report's code lists, as lines and the values they lack", {
  cl <- data.frame(variable = c("SEX", "SEX", "ARM"), value = c("M", "F", "A"),
                   label = c("Male", "Female", "A"), order = c("2", "1", NA))
  l <- .codelist_lines(cl)
  expect_identical(vapply(l, `[[`, "", "variable"), c(SEX = "SEX", ARM = "ARM"))
  expect_identical(l$SEX$text, "F \u2192 Female, M \u2192 Male")
  expect_identical(l$ARM$text, "A")
  expect_named(.codelist_lines(cl, "ARM"), "ARM")
  expect_identical(.codelist_lines(cl[0, ]), list())
  d <- data.frame(SEX = c("F", "U", "X", "", NA), ARM = factor("A", levels = c("A", "B")),
                  AGE = 1)
  expect_identical(.codelist_missing(cl, d), list(SEX = c("U", "X")))
  expect_identical(.codelist_missing(cl, NULL), list())
  # added after the others, each printing as itself
  p <- set_codelist(cl_planner(), "T1", cl[1:2, ])
  q <- .codelist_add_values(p, "T1", "SEX", c("U", "X"))
  r <- sheet_rows(q, "codelists", "T1")
  expect_identical(r$value, c("M", "F", "U", "X"))
  expect_identical(r$order, c("2", "1", "3", "4"))
  expect_identical(r$label[3:4], c("U", "X"))
})

test_that("the code list of `variable` moves to the variables' labels", {
  p <- cl_planner()
  p <- set_sheet_rows(p, "variables", "T1", data.frame(
    variable = c("SEX", "AGE"), label = c(NA, "Age"), order = c("1", "2")))
  p <- set_codelist(p, "T1", data.frame(variable = c("variable", "variable", "variable", "SEX"),
                                        value = c("SEX", "AGE", "RACE", "F"),
                                        label = c("Sex", "not this", "Race", "Female")))
  q <- .move_heading_rows(p)
  v <- sheet_rows(q, "variables", "T1")
  # a label of its own wins; a variable not on the sheet gets a row
  expect_identical(v$label[v$variable == "SEX"], "Sex")
  expect_identical(v$label[v$variable == "AGE"], "Age")
  expect_identical(v$label[v$variable == "RACE"], "Race")
  # the code lists keep the values' rows only
  cl <- sheet_rows(q, "codelists", "T1")
  expect_identical(cl$variable, "SEX")
  expect_identical(attr(q, "moved_headings"), 3L)
  # none to move: the planner as it was
  expect_identical(.move_heading_rows(q), structure(q, moved_headings = 3L))
  expect_null(attr(.move_heading_rows(cl_planner()), "moved_headings"))
})

test_that("step 2: a variable's label is the variables sheet's, the code lists give none", {
  p <- cl_planner()
  p <- set_sheet_rows(p, "variables", "T1", data.frame(
    variable = c("SEX", "AGE"), label = c(NA, "Age"), order = c("1", "2")))
  p <- set_codelist(p, "T1", data.frame(variable = "SEX", value = "F", label = "Female"))
  st <- builder_read(p, "T1")
  v <- st$variables
  # SEX: no label of its own, and no hint; AGE: its own
  expect_true(is.na(v$label[v$variable == "SEX"]))
  expect_true(is.na(v$hint[v$variable == "SEX"]))
  expect_identical(v$label[v$variable == "AGE"], "Age")
  # written back unchanged: the variables sheet as it was
  q <- builder_write(p, "T1", st, was = st)
  expect_identical(sheet_rows(q, "variables", "T1")$label, sheet_rows(p, "variables", "T1")$label)
})

test_that("a listing's and a figure's data: their code lists, where the data is made", {
  skip_on_cran()
  local_home()
  s <- create_study("CF")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  saveRDS(cards::ADAE, file.path(s$path, "data/adam/adae.rds"))
  s$planner <- add_output(s$planner, "L-AE", type = "listing")
  s$planner <- set_lf_rows(s$planner, "listings", "L-AE", data.frame(
    type = "multiline", dataset = "ADAE"))
  s$planner <- add_output(s$planner, "F-AE", type = "figure")
  s$planner <- set_fig_design(s$planner, "F-AE", list(
    data = list(list(step = "read", dataset = "ADAE")), stats = list(),
    plot = list(), layers = list(list(layer = "histogram", x = "AESTDY"))))
  for (id in c("L-AE", "F-AE")) {
    s$planner <- set_codelist(s$planner, id, data.frame(
      variable = "AESEV", value = c("MILD", "MODERATE"), label = c("Mild", "Moderate")))
  }
  save_study(s)
  shiny::testServer(server_for("CF"), {
    rv <- session$userData$rv
    # the listing: after the condition, before the order
    session$setInputs(target = "L-AE", nav = "make", step = "content", content_nav = "content")
    expect_match(output$lf_form$html, "lf_cl", fixed = TRUE)
    h <- output$lf_cl$html
    expect_match(h, "MILD \u2192 Mild, MODERATE \u2192 Moderate", fixed = TRUE)
    expect_match(h, "AESEV: the data has values its code list does not", fixed = TRUE)
    expect_match(h, "SEVERE", fixed = TRUE)
    session$setInputs(lf_cl_add = "AESEV")
    expect_identical(sheet_rows(rv$p, "codelists", "L-AE")$value,
                     c("MILD", "MODERATE", "SEVERE"))
    # the figure: the data steps' last, the code lists
    session$setInputs(target = "F-AE")
    expect_match(output$pd_stack$html, "Code lists", fixed = TRUE)
    session$setInputs(pd_act = list(op = "sel", sec = "codelists", i = 1L, n = 1))
    expect_match(output$pd_form$html, "pd_cl", fixed = TRUE)
    h <- output$pd_cl$html
    expect_match(h, "MILD \u2192 Mild", fixed = TRUE)
    expect_match(h, "would stop): SEVERE", fixed = TRUE)
    session$setInputs(pd_cl_add = "AESEV")
    expect_identical(sheet_rows(rv$p, "codelists", "F-AE")$value,
                     c("MILD", "MODERATE", "SEVERE"))
    # the listing's list as it was: each report's own
    expect_identical(nrow(sheet_rows(rv$p, "codelists", "L-AE")), 3L)
  })
})
