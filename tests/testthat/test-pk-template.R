run_saved_program <- function(s, id, dir = "tfl") {
  owd <- setwd(s$path)
  on.exit(setwd(owd), add = TRUE)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  file.path("programs", dir, paste0(id, ".R")),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, out = paste(out, collapse = "\n"))
}

test_that("a pk_conc report's program makes its records and runs: the subjects on top, the ARD summary under them", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  # a small fictional ADPC: 12 safety-set subjects at 4 nominal times
  set.seed(1)
  adsl <- readRDS(file.path(s$path, "data/adam/adsl.rds"))
  adsl <- adsl[adsl$SAFFL == "Y", ][1:12, c("USUBJID", "TRT01A", "AGE", "SEX")]
  adpc <- merge(adsl, data.frame(ATPTN = c(0, 1, 2, 4), ATPT = paste(c(0, 1, 2, 4), "h")), by = NULL)
  adpc$PARAMCD <- "CONC"
  adpc$PARAM <- "Concentration (ng/mL)"
  adpc$NFRLT <- adpc$ATPTN
  adpc$AVAL <- round(stats::runif(nrow(adpc), 5, 50), 1)
  saveRDS(adpc, file.path(s$path, "data/adam/adpc.rds"))
  p <- s$planner
  p$ard$datasets <- rbind(p$ard$datasets, data.frame(dataset = "ADPC", level = "ADaM",
                                                     path = "data/adam/adpc.rds", derive = NA_character_))
  o <- tflspec::tfl_output("X-PK", "table", label = "pk", population = "SAF",
                           template = list(name = "pk_conc", args = list(population = "SAF")))
  s$planner <- .spec_add_output(p, o, label = "pk", population = "SAF")
  s <- suppressWarnings(suppressMessages(save_study(s, home = home)))
  prog <- readLines(file.path(s$path, "programs", "tfl", "X-PK.R"), warn = FALSE)
  expect_true(any(grepl("^data <-", prog)))
  a <- run_saved_program(s, "X-PK", "ard")
  expect_identical(a$status, 0L, info = a$out)
  r <- run_saved_program(s, "X-PK")
  expect_identical(r$status, 0L, info = r$out)
  rtf <- list.files(s$path, "X-PK.*\\.rtf$", recursive = TRUE, full.names = TRUE)
  expect_length(rtf, 1L)
  txt <- paste(readLines(rtf, warn = FALSE), collapse = "\n")
  expect_match(txt, adsl$USUBJID[1L], fixed = TRUE)
  expect_match(txt, "Summary Statistics", fixed = TRUE)
})
