# The package's R code must be ASCII: R CMD check warns ("checking R files
# for non-ASCII characters") on any other byte, and PR checks do not run it.
# Write such a character as an escape, e.g. "⚠".

# Lines of a file holding a byte above 127, as "file:line".
non_ascii_lines <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "bytes")
  bad <- vapply(lines, function(l) any(charToRaw(l) > as.raw(127L)),
                logical(1), USE.NAMES = FALSE)
  if (any(bad)) paste0(basename(file), ":", which(bad)) else character()
}

test_that("every file in R/ is ASCII", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir))  # not there in an installed check
  files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  expect_true(length(files) > 0L)
  bad <- unlist(lapply(files, non_ascii_lines))
  expect_true(length(bad) == 0L,
              info = paste("non-ASCII at", paste(bad, collapse = ", ")))
})

test_that("the non-ASCII check finds a warning sign", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f))
  writeBin(c(charToRaw("x <- \""), as.raw(c(0xe2, 0x9a, 0xa0)),
             charToRaw("\"\ny <- 1\n")), f)
  expect_identical(non_ascii_lines(f), paste0(basename(f), ":1"))
})
