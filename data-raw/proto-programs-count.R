# PROTOTYPE (tflspec#1032, tflplanner c/proto-programs-planner): the Gate 2
# counts, as tflspec's inst/dev/proto-programs/count.R takes them.
# Executable lines are lines with a code token (getParseData(), comments
# and blank lines left out).  Run from the package folder:
#   Rscript data-raw/proto-programs-count.R <base ref> [<head ref>]
# (head: the working tree).  It prints the executable lines removed and
# added (by git diff -U0, a line counted when it has a code token in its
# version), the top-level functions removed, added and changed, and the
# test_that() blocks removed, added and changed.

args <- commandArgs(TRUE)
base <- if (length(args)) args[1L] else "origin/next"
head <- if (length(args) > 1L) args[2L] else NA_character_

git <- function(...) system2("git", c(...), stdout = TRUE)
read_at <- function(ref, f) {
  if (is.na(ref)) return(if (file.exists(f)) readLines(f, warn = FALSE) else character())
  out <- suppressWarnings(system2("git", c("show", paste0(ref, ":", f)), stdout = TRUE,
                                  stderr = FALSE))
  if (!is.null(attr(out, "status"))) character() else out
}

# the lines of `text` with a code token; and its top-level functions with
# their executable lines and their text
parse_info <- function(text) {
  if (!length(text)) return(list(lines = integer(), funs = list()))
  ex <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) stop("does not parse")
  pd <- utils::getParseData(ex)
  code <- unique(pd$line1[pd$terminal & pd$token != "COMMENT"])
  # (a token spanning lines, a string: its first line)
  funs <- list()
  srcs <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && as.character(e[[1L]]) %in% c("<-", "=") && is.name(e[[2L]]) &&
        is.call(e[[3L]]) && identical(e[[3L]][[1L]], as.name("function"))) {
      r <- srcs[[i]]
      ln <- r[1L]:r[3L]
      funs[[as.character(e[[2L]])]] <- list(exec = sum(code %in% ln),
                                            text = paste(text[ln], collapse = "\n"))
    }
  }
  list(lines = code, funs = funs)
}

# the files of `dir` that differ between base and head (untracked ones too)
changed_in <- function(dir) {
  d <- if (is.na(head)) git("diff", "--name-only", base, "--", dir) else
    git("diff", "--name-only", base, head, "--", dir)
  u <- if (is.na(head)) git("ls-files", "--others", "--exclude-standard", "--", dir) else character()
  unique(c(d, u))
}

files_at <- function(ref, dir) {
  if (is.na(ref)) return(list.files(dir, "[.][Rr]$", full.names = TRUE))
  f <- git("ls-tree", "--name-only", paste0(ref, ":", dir))
  file.path(dir, f[grepl("[.][Rr]$", f)])
}

count_dir <- function(dir) {
  fs <- intersect(union(files_at(base, dir), files_at(head, dir)), changed_in(dir))
  rem <- add <- 0L
  per <- list()
  for (f in fs) {
    a <- read_at(base, f)
    b <- read_at(head, f)
    if (identical(a, b)) next
    ia <- parse_info(a)
    ib <- parse_info(b)
    d <- if (is.na(head)) git("diff", "-U0", base, "--", f) else git("diff", "-U0", base, head, "--", f)
    hunks <- grep("^@@", d, value = TRUE)
    r <- 0L
    s <- 0L
    for (h in hunks) {
      m <- regmatches(h, regexec("^@@ -([0-9]+)(,([0-9]+))? \\+([0-9]+)(,([0-9]+))? @@", h))[[1L]]
      a0 <- as.integer(m[2L]); an <- if (nzchar(m[4L])) as.integer(m[4L]) else 1L
      b0 <- as.integer(m[5L]); bn <- if (nzchar(m[7L])) as.integer(m[7L]) else 1L
      if (an) r <- r + sum(seq(a0, length.out = an) %in% ia$lines)
      if (bn) s <- s + sum(seq(b0, length.out = bn) %in% ib$lines)
    }
    if (!length(hunks) && !length(a)) s <- length(ib$lines)
    if (!length(hunks) && !length(b)) r <- length(ia$lines)
    rem <- rem + r
    add <- add + s
    per[[f]] <- list(removed = r, added = s, a = ia$funs, b = ib$funs)
  }
  list(removed = rem, added = add, per = per)
}

cat("Base:", base, " head:", if (is.na(head)) "working tree" else head, "\n\n")
x <- count_dir("R")
cat("== R/: executable lines removed", x$removed, ", added", x$added,
    ", net", x$added - x$removed, "\n\n")
cat("file | removed | added\n")
for (f in names(x$per)) cat(f, "|", x$per[[f]]$removed, "|", x$per[[f]]$added, "\n")
fa <- unlist(lapply(x$per, function(p) lapply(p$a, `[[`, "exec")))
fb <- unlist(lapply(x$per, function(p) lapply(p$b, `[[`, "exec")))
ta <- unlist(lapply(x$per, function(p) lapply(p$a, `[[`, "text")))
tb <- unlist(lapply(x$per, function(p) lapply(p$b, `[[`, "text")))
gone <- setdiff(names(fa), names(fb))
new <- setdiff(names(fb), names(fa))
chg <- intersect(names(fa), names(fb))
chg <- chg[ta[chg] != tb[chg]]
cat("\nFunctions removed (", length(gone), "):\n", sep = "")
for (g in gone) cat("  ", g, fa[[g]], "\n")
cat("Functions added (", length(new), "):\n", sep = "")
for (g in new) cat("  ", g, fb[[g]], "\n")
cat("Functions changed (", length(chg), "):\n", sep = "")
for (g in chg) cat("  ", g, fa[[g]], "->", fb[[g]], "\n")

# test_that() blocks: by description, per file
blocks <- function(text) {
  if (!length(text)) return(character())
  ex <- tryCatch(parse(text = text, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) return(character())
  out <- character()
  srcs <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && identical(e[[1L]], as.name("test_that")) && is.character(e[[2L]])) {
      r <- srcs[[i]]
      out[[e[[2L]]]] <- paste(text[r[1L]:r[3L]], collapse = "\n")
    }
  }
  out
}
tf <- intersect(union(files_at(base, "tests/testthat"), files_at(head, "tests/testthat")),
                changed_in("tests/testthat"))
tr <- ta2 <- tc <- 0L
cat("\n== tests/testthat: file | blocks removed | added | changed\n")
for (f in tf) {
  a <- read_at(base, f)
  b <- read_at(head, f)
  if (identical(a, b)) next
  ba <- blocks(a)
  bb <- blocks(b)
  r <- length(setdiff(names(ba), names(bb)))
  s <- length(setdiff(names(bb), names(ba)))
  k <- intersect(names(ba), names(bb))
  cc <- sum(ba[k] != bb[k])
  tr <- tr + r; ta2 <- ta2 + s; tc <- tc + cc
  cat(f, "|", r, "|", s, "|", cc, "\n")
}
cat("total | ", tr, "|", ta2, "|", tc, "\n")
