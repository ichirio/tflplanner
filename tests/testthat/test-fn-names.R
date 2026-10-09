# Analyses written with the cards function a keyword calls (#142): the same
# analysis for the app's checks, and the company's list shows only its own

test_that("a keyword's function stands for the keyword", {
  local_home()
  expect_identical(.method_kw(c("cards::ard_tabulate", "categorical",
                                "cards::ard_total_n", "cardx::ard_stats_anova",
                                "custom")),
                   c("categorical", "categorical", "total_n",
                     "cardx::ard_stats_anova", "custom"))
})

test_that("the company's list holds what it added or changed", {
  local_home()
  # the built-in standards: only subjects and custom code (no function)
  expect_setequal(.company_keywords()$method, c("subjects", "custom"))
  # a row that names a keyword keeps it in the list
  expect_true("categorical" %in% .company_keywords("categorical")$method)
  # a keyword the company changed or added is its own
  std <- company_standards()
  m <- std$ard_methods
  m$statistics[m$method == "continuous"] <- "N | mean"
  m <- rbind(m, m[m$method == "categorical", ])
  m$method[nrow(m)] <- "cat_pct"
  testthat::local_mocked_bindings(.std_ard_methods = function() {
    m[] <- lapply(m, function(v) ifelse(is.na(v), "", v))
    m
  })
  expect_setequal(.company_keywords()$method,
                  c("continuous", "cat_pct", "subjects", "custom"))
})

test_that("a group's N written as cards::ard_tabulate is found", {
  local_home()
  a <- data.frame(analysis_id = c("GROUPN", "TOTAL"),
                  method = c("cards::ard_tabulate", "cards::ard_total_n"),
                  dataset = "ADSL", population_id = "SAF",
                  variables = c("TRT01A", NA), by = NA, parent = NA,
                  args = NA, where = NA, stringsAsFactors = FALSE)
  r <- a[1L, ]
  r$by <- "TRT01A"
  expect_identical(.stack_group_n(a, r), 1L)
  n <- .stack_n_rows(a, data.frame(by = "TRT01A", dataset = "ADSL",
                                   population_id = "SAF", where = NA,
                                   args = ".by_stats = TRUE, .total_n = TRUE",
                                   stringsAsFactors = FALSE))
  # GROUPN and TOTAL are there already: not made again
  expect_identical(nrow(n), 0L)
})
