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
  expect_setequal(.codelist_ard_vars(p, "T1"), c("TRT01A", "SEX", "AGE"))
  expect_identical(.codelist_vars(p, NULL), character())
})

test_that("step 1: the report's code lists, the ones it uses, copied in", {
  skip_on_cran()
  local_home()
  p <- cl_planner()
  p <- set_codelist(p, "T2", data.frame(variable = c("TRT01A", "TRT01A", "PARAMCD"),
                                        value = c("Placebo", "Drug", "ALT"),
                                        label = c("Placebo", "Drug X", "ALT")))
  p <- set_codelist(p, "T1", data.frame(variable = "COUNTRY", value = "JPN"))
  create_study("CL", planner = p)
  shiny::testServer(server_for("CL"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "codelist", target = "T1")
    # the variables T1 uses only: COUNTRY is hidden, and said so
    expect_match(output$cl_hidden$html, "1", fixed = TRUE)
    expect_false(grepl("COUNTRY", paste(output$cl_hot, collapse = ""), fixed = TRUE))
    session$setInputs(cl_all = TRUE)
    expect_true(grepl("COUNTRY", paste(output$cl_hot, collapse = ""), fixed = TRUE))
    # copy T2's TRT01A: T2's choices preselect what T1 uses
    session$setInputs(cl_copy = 1, cl_from = "T2")
    expect_match(output$cl_which$html, "TRT01A: Placebo, Drug", fixed = TRUE)
    session$setInputs(cl_pick = "TRT01A", cl_as_var = "TRT01A", cl_copy_ok = 1)
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "TRT01A"], c("Placebo", "Drug"))
    # the company standards' SEX, as SEX
    session$setInputs(cl_from = ".std", cl_pick = "SEX", cl_as_var = "SEX", cl_copy_ok = 2)
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "SEX"], c("F", "M", "U"))
    # step 1's result: what uses each
    r <- output$codelist_effective
    expect_match(r, "used_by", fixed = TRUE)
    # a file read into T1
    f <- withr::local_tempfile(fileext = ".csv")
    writeLines(c("variable,value,label", "RACE,ASIAN,Asian"), f)
    session$setInputs(cl_file = data.frame(name = "cl.csv", datapath = f))
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$label[d$variable == "RACE"], "Asian")
    expect_identical(nrow(sheet_rows(rv$p, "codelists", NA)), 0L)
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
