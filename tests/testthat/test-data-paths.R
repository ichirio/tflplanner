test_that("a dataset delivered in another form is found: .sas7bdat, .xpt, .rds, .parquet (#515)", {
  d <- tempfile("dp"); dir.create(file.path(d, "data", "adam"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.create(file.path(d, "data", "adam", c("adsl.xpt", "adsl.sas7bdat", "adae.xpt", "adlb.rds")))
  expect_identical(.dataset_delivered(d, "data/adam/adsl.rds"), "data/adam/adsl.sas7bdat")
  expect_identical(.dataset_delivered(d, "data/adam/adae.rds"), "data/adam/adae.xpt")
  expect_identical(.dataset_delivered(d, "data/adam/adlb.rds"), "data/adam/adlb.rds")
  expect_identical(.dataset_delivered(d, "data/adam/advs.rds"), "data/adam/advs.rds")
  p <- new_planner()
  p$ard$datasets <- data.frame(dataset = c("ADSL", "ADAE", "ADLB", "ADVS"),
                               path = paste0("data/adam/", c("adsl", "adae", "adlb", "advs"), ".rds"),
                               stringsAsFactors = FALSE)
  r <- .datasets_as_delivered(p, d)
  expect_identical(r$changed$dataset, c("ADSL", "ADAE"))
  expect_identical(r$planner$ard$datasets$path[1:2], c("data/adam/adsl.sas7bdat", "data/adam/adae.xpt"))
  expect_identical(nrow(.datasets_as_delivered(r$planner, d)$changed), 0L)
})
