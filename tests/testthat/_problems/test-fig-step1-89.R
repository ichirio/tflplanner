# Extracted from test-fig-step1.R:89

# prequel ----------------------------------------------------------------------
skip_on_cran()

# test -------------------------------------------------------------------------
d <- tflspec::tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE", pop = "SAFFL",
                                 group = "TRT01A")
i <- which(vapply(d$data, function(p) identical(p$step, "time_unit"), NA))
make <- function(d, codelists = TRUE) .fig_design_script(d, "F1")
d$data[[i]]$unit <- "days"
expect_match(.piece_code(make(d), d, list(sec = "data", i = i), make), "in days already", fixed = TRUE)
