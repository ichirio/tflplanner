# Extracted from test-template-report-gaps.R:168

# prequel ----------------------------------------------------------------------
tpl_prm <- function(...) utils::modifyList(list(name = "x"), list(...))
tpl_sample_planner <- function() {
  .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
}
tpl_demog_output <- function(id = "T-99") {
  tflspec::tfl_output(id, "table", population = "SAF",
                      template = list(name = "demog", args = list(population = "SAF")))
}

# test -------------------------------------------------------------------------
expect_identical(.tpl_show(NULL), "NULL")
expect_identical(.tpl_show("n (%)"), "n (%)")
expect_identical(.tpl_show(c("a", "b")), "c(\"a\", \"b\")")
expect_identical(.tpl_show(c(x = "a")), "c(x = \"a\")")
expect_identical(.tpl_show(1:3), "1:3")
