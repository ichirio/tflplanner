# SAMPLE-01 has one source: its committed files, edited by hand (#879).
# data-raw/make-sample-data.R makes only the four ADaM data files; these
# tests keep the data, spec/data.yml and the README in step with the YAML.

sample_dir <- function() system.file("sample", "SAMPLE-01", package = "tflplanner")

test_that("every dataset of spec/data.yml has its file", {
  d <- yaml::read_yaml(file.path(sample_dir(), "spec", "data.yml"))$datasets
  expect_gt(length(d), 0L)
  for (x in d) {
    expect_true(file.exists(file.path(sample_dir(), x$path)),
                info = paste(x$dataset, x$path))
  }
})

test_that("the four ADaM files are what data-raw/make-sample-data.R makes", {
  skip_on_cran()
  skip_if_not_installed("tflspec", "0.0.24.9168")
  # the same steps as the script (data-raw is not in the built package)
  strip <- function(d) {
    for (k in names(d)) attr(d[[k]], "format.sas") <- NULL
    d
  }
  advs <- as.data.frame(tflspec::advs)
  advs <- advs[advs$PARAMCD == "SYSBP", ]
  advs$TRTA <- structure(as.character(advs$TRT01A), label = "Actual Treatment")
  advs <- advs[c("STUDYID", "USUBJID", "TRTA", "TRT01P", "TRT01A", "PARAMCD", "PARAM", "AVISIT", "AVISITN",
                 "ADT", "ADY", "AVAL", "BASE", "CHG", "ABLFL", "ONTRTFL")]
  rownames(advs) <- NULL
  made <- list(adsl = strip(as.data.frame(tflspec::adsl)),
               adae = strip(as.data.frame(tflspec::adae)),
               advs = strip(advs),
               adtte = strip(as.data.frame(tflspec::adtte)))
  for (k in names(made)) {
    f <- file.path(sample_dir(), "data", "adam", paste0(k, ".rds"))
    expect_true(file.exists(f), info = k)
    # (variable labels left out: tflspec's own labels change without the data)
    expect_equal(readRDS(f), made[[k]], ignore_attr = "label", info = k)
  }
})

test_that("every output of the sample is in the README's table", {
  ids <- sub("[.]yml$", "", list.files(file.path(sample_dir(), "spec", "outputs"),
                                       pattern = "[.]yml$"))
  expect_gt(length(ids), 0L)
  readme <- readLines(file.path(sample_dir(), "README.md"), warn = FALSE)
  tbl <- grep("^[|] *[TLF]-", readme, value = TRUE)
  rows <- regmatches(tbl, regexpr("[TLF]-[0-9]+-[0-9]+-[0-9]+[A-Z]*", tbl))
  expect_setequal(rows, ids)
})
