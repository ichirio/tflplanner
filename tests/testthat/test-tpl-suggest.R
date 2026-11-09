# Suggested values for a template's required variable-set arguments, from the
# data (inst/extdata/tpl_suggest.yml, R/tpl_suggest.R).  Fictional data:
# Example Pharma's study.

example_adsl <- function() {
  data.frame(USUBJID = paste0("EX-", 1:4), AGE = c(40, 52, 61, 33),
             SEX = c("F", "M", "F", "M"), RACE = c("WHITE", "ASIAN", "WHITE", "BLACK"),
             WEIGHTBL = c(60, 80, 72, 90), AGEGR1 = c("<65", "<65", "<65", "<65"),
             stringsAsFactors = FALSE)
}

test_that(".tpl_suggest keeps the candidates the data has, in candidate order", {
  cols <- c("RACE", "SEX", "AGE", "WEIGHTBL", "OTHER")
  expect_identical(.tpl_suggest("demog", "continuous", cols), c("AGE", "WEIGHTBL"))
  expect_identical(.tpl_suggest("demog", "categorical", cols), c("SEX", "RACE"))
  expect_identical(.tpl_suggest("stf_t02_demog", "categorical", cols, list = "stf"), c("SEX", "RACE"))
  expect_identical(.tpl_suggest("stf_t02_demog", "continuous", c("BMIBL", "AGE"), list = "stf"), "AGE")
  expect_identical(.tpl_suggest("ae_soc_pt", "hierarchy", c("AEDECOD", "AEBODSYS")), c("AEBODSYS", "AEDECOD"))
  expect_identical(.tpl_suggest("forest_hr", "subgroups", c("RACE", "SEX")), c("SEX", "RACE"))
})

test_that(".tpl_suggest: a single-value argument gets the first present; none or unknown is character()", {
  expect_identical(.tpl_suggest("ae_pt_freq", "term", c("AEBODSYS", "AEDECOD")), "AEDECOD")
  expect_identical(.tpl_suggest("ae_pt_freq", "term", "X"), character())
  expect_identical(.tpl_suggest("demog", "continuous", "X"), character())
  expect_identical(.tpl_suggest("no_such_template", "continuous", "AGE"), character())
  expect_identical(.tpl_suggest("demog", "no_such_arg", "AGE"), character())
  expect_identical(.tpl_suggest("demog", "continuous", "AGE", list = "nope"), character())
})

test_that(".tpl_suggest_for takes the dataset's columns as the dialog does", {
  data <- list(ADSL = example_adsl(), ADAE = data.frame(AEDECOD = "x", AEBODSYS = "y"))
  expect_identical(.tpl_suggest_for("demog", "continuous", data, list(data = "ADSL"),
                                    .tpl_params("demog")), c("AGE", "WEIGHTBL"))
  expect_identical(.tpl_suggest_for("ae_soc_pt", "hierarchy", data, list(data = "ADAE"),
                                    .tpl_params("ae_soc_pt")), c("AEBODSYS", "AEDECOD"))
  expect_identical(.tpl_suggest_for("km", "arm", data), character())
})

test_that("by: the column variable is the study's treatment setting, else nothing; the list name is one constant", {
  st <- list(study = list(settings = list(treatment = "TRT01A")))
  expect_identical(.tpl_suggest("km", "by", "AGE", study = st), "TRT01A")
  expect_identical(.tpl_suggest("demog", "by", "AGE", study = st), "TRT01A")
  expect_identical(.tpl_suggest("demog", "by", "AGE"), character())
  expect_identical(.tpl_suggest("demog", "by", "AGE", study = list(study = list(settings = list()))), character())
  expect_identical(.tpl_suggest_list, "max")
})
