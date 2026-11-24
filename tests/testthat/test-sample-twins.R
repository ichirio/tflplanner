# SAMPLE-01's twins: the same report written as a template SPEC (T) and
# as a code SPEC (C) -- T on the basic templates, tflspec_summary (DM) and
# tflspec_hierarchy (AE; its ids, row label and sort given by the report).
# Their ARDs are the same but for output_id: the analysis ids (the canon's: BIGN, an id a variable or a depth), groups,
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

# the template twins are not in the sample: written into the study's spec
# (their files and lines of spec/toc.yml) and the study read again
twin_add_templates <- function(s, home) {
  ids <- c("T-14-1-1T", "T-14-3-1T")
  toc_file <- file.path(s$path, "spec", "toc.yml")
  toc <- yaml::read_yaml(toc_file)
  for (id in ids) {
    f <- testthat::test_path("fixtures", "twins", paste0(id, ".yml"))
    expect_true(file.copy(f, file.path(s$path, "spec", "outputs", paste0(id, ".yml"))))
    o <- yaml::read_yaml(f, handlers = list(r = function(x) x))
    toc$outputs <- c(toc$outputs, list(list(id = id, kind = o$kind, label = o$label,
                                            population = o$common$population)))
  }
  yaml::write_yaml(toc, toc_file)
  # (the study reads only its own copies of the templates, as the Add
  # dialog makes them)
  invisible(tflspec::tfl_copy_templates(s$path))
  suppressMessages(reload_from_spec(s, home = home))
}

test_that("SAMPLE-01's template and code twins make the same ARD (DM, AE)", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ok <- tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/tflspec_hierarchy")),
                 error = function(e) FALSE)
  skip_if(!isTRUE(ok), "this tflspec has no series templates (tflspec#824)")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  s <- twin_add_templates(s, home)
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
