# A local_*() helper called inside testServer()'s code is not undone when its
# test ends: that code has no frame of its own that ends, so the mock, the
# option or the file stays for the test files after it (tflspec's check runs
# them all in one R: a fetch_ard() mocked to fail broke test-run-end.R).
# Inside testServer(), give the helper the test's frame (.env = or
# .local_envir =).

scope_fns <- c("local_mocked_bindings", "local_options", "local_envvar", "local_tempfile",
               "local_tempdir", "local_dir", "local_seed", "local_language", "local_locale",
               "local_reproducible_output", "local_par", "local_timezone")

# the local_*() calls in `e` left without a frame, inside a testServer()
scope_leaks <- function(e, inside = FALSE) {
  if (!is.call(e)) return(character())
  f <- e[[1L]]
  nm <- if (is.name(f)) {
    as.character(f)
  } else if (is.call(f) && is.name(f[[1L]]) && as.character(f[[1L]]) %in% c("::", ":::")) {
    as.character(f[[3L]])
  } else {
    ""
  }
  # (code in quote() is data: the check's own examples below)
  if (identical(nm, "quote")) return(character())
  out <- character()
  if (inside && nm %in% scope_fns && !any(c(".env", ".local_envir") %in% names(e))) out <- nm
  # (only testServer()'s code, the { } block: its app argument is read in
  # the test's frame)
  is_ts <- identical(nm, "testServer")
  args <- as.list(e)[-1L]
  for (i in seq_along(args)) {
    a <- args[[i]]
    if (!is.call(a)) next
    out <- c(out, scope_leaks(a, inside || (is_ts && identical(a[[1L]], as.name("{")))))
  }
  out
}

test_that("a local_*() helper inside testServer() is undone with its test", {
  files <- list.files(testthat::test_path(), "^(test|helper)-.*[.]R$", full.names = TRUE)
  bad <- unlist(lapply(files, function(f) {
    found <- unlist(lapply(parse(f, keep.source = FALSE, encoding = "UTF-8"), scope_leaks))
    if (length(found)) paste0(basename(f), ": ", found) else character()
  }))
  expect(length(bad) == 0L,
         paste0("inside testServer(), give these the test's frame (.env = / .local_envir =): ",
                paste(bad, collapse = "; ")))
  # the check itself finds one
  expect_identical(scope_leaks(quote(shiny::testServer(app, {
    testthat::local_mocked_bindings(f = function() 1)
  }))), "local_mocked_bindings")
  expect_identical(scope_leaks(quote(shiny::testServer(app, {
    withr::local_options(x = 1, .local_envir = test_frame)
  }))), character())
  expect_identical(scope_leaks(quote(shiny::testServer(app(withr::local_tempdir()), {
    x <- 1
  }))), character())
})
