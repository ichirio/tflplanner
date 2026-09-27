spec_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

toy_spec <- function(analyses) {
  s <- list(
    study = spec_df(list(key = "id", value = "USUBJID")),
    datasets = spec_df(list(dataset = "ADSL", path = "adsl.rds"),
                       list(dataset = "ADAE", path = "adae.rds")),
    populations = spec_df(list(population_id = "SAF", dataset = "ADSL",
                               where = "SAFFL == \"Y\"",
                               derive = "TRTA = TRT01A")),
    analyses = analyses)
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) {
      if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  ard_spec(s)
}

test_that("a definition is checked", {
  expect_error(toy_spec(spec_df(list(output_id = "T1", analysis_id = "A",
                                     method = "nope"))), "unknown method")
  expect_error(toy_spec(spec_df(
    list(output_id = "T1", analysis_id = "A", method = "categorical"),
    list(output_id = "T1", analysis_id = "A", method = "categorical"))),
    "repeated")
  expect_error(toy_spec(spec_df(list(output_id = "T1", analysis_id = "A",
                                     method = "categorical",
                                     population_id = "ITT"))),
               "ITT")
  expect_error(toy_spec(spec_df(list(output_id = "T1", analysis_id = "A",
                                     method = "custom"))), "code")
})

test_that("the definition becomes code and the code the study ARD", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  dir <- withr_tempdir()
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  x <- toy_spec(spec_df(
    list(output_id = "DM", analysis_id = "BIGN", method = "categorical",
         population_id = "SAF", variables = "TRT01A"),
    list(output_id = "DM", analysis_id = "AGE", method = "continuous",
         population_id = "SAF", by = "TRT01A", variables = "AGE",
         statistics = "N | mean | sd"),
    list(output_id = "AE", analysis_id = "TEAE", method = "hierarchical",
         dataset = "ADAE", population_id = "SAF", where = "TRTEMFL == \"Y\"",
         by = "TRTA", variables = "AEBODSYS | AEDECOD",
         args = "over_variables = TRUE"),
    list(output_id = "CI", analysis_id = "SEX", method = "proportion_ci",
         population_id = "SAF", by = "TRT01A", variables = "SEX",
         args = "method = \"wilson\""),
    list(output_id = "X", analysis_id = "T", method = "custom",
         population_id = "SAF",
         code = "cardx::ard_stats_t_test(\n  data[data$TRT01A != \"Placebo\", ], by = TRT01A, variables = AGE)")))
  code <- ard_spec_code(x)
  expect_silent(parse(text = code))
  expect_true(any(grepl("cards::ard_stack_hierarchical", code, fixed = TRUE)))
  expect_true(any(grepl("denominator = population", code, fixed = TRUE)))

  a <- build_ard(x, dir = dir)
  expect_true(file.exists(file.path(dir, "output", "ard", "ard.rds")))
  expect_setequal(unique(a$output_id), c("DM", "AE", "CI", "X"))
  expect_true(all(c("output_id", "analysis_id", "population_id") %in%
                    names(a)))
  dm <- ard_for(a, "DM")
  expect_false("output_id" %in% names(dm))
  expect_setequal(unique(dm$stat_name), c("n", "N", "p", "mean", "sd"))
  # the population's subjects only
  n <- a$stat[a$output_id == "DM" & a$analysis_id == "BIGN" &
                a$stat_name == "N"][[1L]]
  expect_equal(n, sum(cards::ADSL$SAFFL == "Y"))
  expect_true(any(a$variable[a$output_id == "AE"] == "AEDECOD"))
  expect_true("conf.low" %in% a$stat_name[a$output_id == "CI"])
  expect_true("p.value" %in% a$stat_name[a$output_id == "X"])

  # the same outputs as one part of the whole
  only <- build_ard(x, dir = dir, output_id = "DM", save = FALSE)
  expect_equal(unique(only$output_id), "DM")
})

test_that("a workbook reads back as written", {
  skip_if_not_installed("cards")
  x <- toy_spec(spec_df(list(output_id = "DM", analysis_id = "BIGN",
                             method = "categorical", population_id = "SAF",
                             variables = "TRT01A")))
  f <- file.path(withr_tempdir(), "ard_spec.xlsx")
  writexl::write_xlsx(c(list(`_README` = data.frame(a = 1)), unclass(x)), f)
  y <- read_ard_spec(f)
  expect_equal(ard_spec_code(y, save = FALSE)[-2], ard_spec_code(x, save = FALSE)[-2])
})

test_that("statistics of the catalog and formats become stat_fmt", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  dir <- withr_tempdir()
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  x <- toy_spec(spec_df(
    list(output_id = "DM", analysis_id = "AGE", method = "continuous",
         population_id = "SAF", by = "TRT01A", variables = "AGE | BMIBL",
         statistics = "N | mean | cv | geo_mean | sd",
         formats = "mean=xx.xx | BMIBL:sd=3"),
    list(output_id = "DM", analysis_id = "CI", method = "mean_ci",
         population_id = "SAF", by = "TRT01A", variables = "AGE",
         statistics = "estimate | conf.low | conf.high"),
    list(output_id = "DM", analysis_id = "SEX", method = "categorical",
         population_id = "SAF", by = "TRT01A", variables = "SEX")))
  code <- ard_spec_code(x)
  expect_true(any(grepl(".tfl_stats[c(\"cv\", \"geo_mean\")]", code,
                        fixed = TRUE)))
  expect_false(any(grepl("`p5` = function", code, fixed = TRUE)))
  a <- build_ard(x, dir = dir)
  v <- ard_view(a)
  pick <- function(id, var, s) v$stat_fmt[v$analysis_id == id &
                                             v$variable == var &
                                             v$stat_name == s][1L]
  expect_equal(pick("AGE", "AGE", "mean"), "75.21")
  expect_equal(pick("AGE", "AGE", "sd"), "8.59")
  expect_equal(pick("AGE", "BMIBL", "sd"), "3.672")
  expect_equal(pick("AGE", "AGE", "cv"), "11.4")
  expect_setequal(unique(v$stat_name[v$analysis_id == "CI"]),
                  c("estimate", "conf.low", "conf.high"))
  expect_equal(pick("CI", "AGE", "estimate"), "75.2")
  expect_equal(pick("SEX", "SEX", "p"), "61.6")

  bad <- x
  bad$analyses$statistics[1] <- "N | nonsense"
  bad$analyses$formats[3] <- "p=x.y"
  expect_error(ard_spec(unclass(bad)), "no continuous statistic nonsense")
  expect_error(ard_spec(unclass(bad)), "p")
})

test_that("every computed statistic of the catalog is a function of x", {
  st <- ard_statistics("continuous")
  x <- c(2.1, 3.4, 5.9, 4.2, 3.3)
  for (i in which(!is.na(st$fun))) {
    f <- eval(parse(text = st$fun[i]))
    expect_true(is.numeric(f(x)) && length(f(x)) == 1L, label = st$statistic[i])
  }
  expect_equal(eval(parse(text = st$fun[st$statistic == "geo_mean"]))(c(1, 100)), 10)
})
