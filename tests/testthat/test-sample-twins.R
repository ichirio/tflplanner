# SAMPLE-01's twins: the same report written as a template SPEC (T) and
# as a code SPEC (C).  Their ARDs are the same but for output_id: the
# analysis ids (the canon's: BIGN, an id a variable or a depth), groups,
# variables, statistics and values.

twin_ard <- function(p, s, id) {
  run <- suppressWarnings(.ard_spec_code(.ard_spec(p$ard), output_id = id, save = FALSE,
                                         dir = s$path, codelists = .study_codelists(p)))
  env <- new.env(parent = globalenv())
  old <- setwd(s$path)
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(suppressMessages(eval(parse(text = run, keep.source = FALSE), envir = env)))
  env$ard
}

# an ARD as rows to compare: output_id and the functions dropped, the list
# columns as text, in one order
twin_rows <- function(a) {
  a <- as.data.frame(a)
  a <- a[setdiff(names(a), c("output_id", "fmt_fn", "warning", "error"))]
  for (k in names(a)) if (is.list(a[[k]])) a[[k]] <- vapply(a[[k]], function(v) paste(format(v), collapse = "|"), "")
  a <- a[do.call(order, unname(as.list(a))), , drop = FALSE]
  rownames(a) <- NULL
  a
}

test_that("SAMPLE-01's template and code twins make the same ARD (DM, AE)", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ok <- tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")), error = function(e) FALSE)
  skip_if(!isTRUE(ok), "this tflspec has no series templates (tflspec#824)")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  p <- s$planner
  pairs <- list(DM = c("T-14-1-1T", "T-14-1-1C"), AE = c("T-14-3-1T", "T-14-3-1C"))
  ids <- list(DM = c("AGE", "AGEGR1", "BIGN", "ECOGBL", "RACE", "SEX"), AE = c("AE_ANY", "AE_L1", "AE_L2", "BIGN"))
  for (k in names(pairs)) {
    tw <- pairs[[k]]
    a <- twin_rows(twin_ard(p, s, tw[[1L]]))
    b <- twin_rows(twin_ard(p, s, tw[[2L]]))
    expect_identical(sort(unique(a$analysis_id)), ids[[k]], info = k)
    expect_identical(sort(names(a)), sort(names(b)), info = k)
    expect_equal(a, b[names(a)], ignore_attr = TRUE, info = k)
  }
})
