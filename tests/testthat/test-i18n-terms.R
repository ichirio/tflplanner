# One Japanese word per idea on the screens (the owner's decision,
# 2026-11-27): no ja cell of inst/i18n/strings/*.csv, nor a ja note of
# inst/ard_search/fn_keywords.csv, uses a word the decision dropped, but for
# the rows below (each with why). The words are \u escapes (R code is ASCII).

# the word dropped = the word the screens use
.ja_terms <- c(
  "\u65e2\u5b9a" = "\u30c7\u30d5\u30a9\u30eb\u30c8",
  "\u30b5\u30d6\u30b0\u30eb\u30fc\u30d7" = "\u90e8\u5206\u96c6\u56e3",
  "\u8a18\u9332" = "\u30ec\u30b3\u30fc\u30c9",
  "\u96c6\u56e3" = "\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3",
  "\u6bcd\u96c6\u56e3" = "\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3",
  "\u5206\u6790" = "\u89e3\u6790",
  "\u884c\u898b\u51fa\u3057" = "\u884c\u30e9\u30d9\u30eb",
  "\u30b9\u30bf\u30d6" = "\u884c\u30e9\u30d9\u30eb"
)

# Longer words that hold a dropped word but are another term: taken out
# before the check
.ja_terms_whole <- c(
  "\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3" = "the word used for the analysis set",
  "\u90e8\u5206\u96c6\u56e3" = "the word used for a subgroup",
  "\u5206\u6563\u5206\u6790" = "ANOVA (and ANCOVA), a fixed statistical term"
)

# Rows that keep a dropped word: "file|en" = c(word, why)
.ja_terms_kept <- list(
  "ard_own.csv|Saved at once, with the other unsaved changes: the record of ARDs taken in and the reports must agree." =
    c("\u8a18\u9332", "the log of what was taken in, not a data record"),
  "ard_own.csv|Who made it is blank: the record will not say." =
    c("\u8a18\u9332", "the log of what was taken in, not a data record"),
  "ard_own.csv|The file and its record stay in input/ard/ (marked taken out), so what a report was made from is not forgotten." =
    c("\u8a18\u9332", "the log of what was taken in, not a data record"),
  "ard_own.csv|The study's copy and the company's file differ (no record of when it was copied)." =
    c("\u8a18\u9332", "the log of a copy, not a data record"),
  "toc.csv|Saved at once, with the other unsaved changes: the record of TOCs taken in and the reports must agree." =
    c("\u8a18\u9332", "the log of what was taken in, not a data record"),
  "setup-wizard.csv|remember the home folder in %s" =
    c("\u8a18\u9332", "the verb: to note down a setting"),
  "common.csv|fn-note:cards::ard_formals" =
    c("\u8a18\u9332", "the verb: the ARD records a value"),
  "common.csv|arg-hint:The name the row count is given in the variable column (default ..row_count..)." =
    c("\u8a18\u9332", "the verb: the ARD records a value"),
  "common.csv|arg-hint:The text the ARD records as the method." =
    c("\u8a18\u9332", "the verb: the ARD records a value"),
  "common.csv|arg-hint:The variable name the statistics are recorded under." =
    c("\u8a18\u9332", "the verb: the ARD records a value"),
  "common.csv|arg-hint:The context the ARD records (default identity)." =
    c("\u8a18\u9332", "the verb: the ARD records a value")
)

.ja_cells <- function() {
  fs <- list.files(system.file("i18n", "strings", package = "tflplanner"), "[.]csv$", full.names = TRUE)
  rows <- lapply(fs, function(f) {
    d <- utils::read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character())
    data.frame(id = paste(basename(f), d$en, sep = "|"), ja = d$ja, stringsAsFactors = FALSE)
  })
  k <- utils::read.csv(system.file("ard_search", "fn_keywords.csv", package = "tflplanner"),
                       fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character())
  rows[[length(rows) + 1L]] <- data.frame(
    id = paste("fn_keywords.csv", k$fn, k$lang, k$keyword, sep = "|"), ja = k$note_ja,
    stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

.ja_dropped_words <- function(ja) {
  for (w in names(.ja_terms_whole)) ja <- gsub(w, "", ja, fixed = TRUE)
  names(.ja_terms)[vapply(names(.ja_terms), grepl, logical(1), x = ja, fixed = TRUE)]
}

test_that("the screens' Japanese uses one word per idea", {
  d <- .ja_cells()
  expect_gt(nrow(d), 1000L)
  bad <- character()
  for (i in seq_len(nrow(d))) {
    w <- .ja_dropped_words(d$ja[[i]])
    kept <- .ja_terms_kept[[d$id[[i]]]]
    w <- setdiff(w, kept[1L])
    if (length(w)) {
      bad <- c(bad, sprintf("%s: %s (use %s)", d$id[[i]], paste(w, collapse = ", "),
                            paste(.ja_terms[w], collapse = ", ")))
    }
  }
  expect_identical(bad, character())
})

test_that("each kept row still exists, still has its word, and says why", {
  d <- .ja_cells()
  for (id in names(.ja_terms_kept)) {
    kept <- .ja_terms_kept[[id]]
    expect_length(kept, 2L)
    expect_true(kept[1L] %in% names(.ja_terms), info = id)
    expect_true(nzchar(kept[2L]), info = id)
    expect_true(id %in% d$id, info = id)
    expect_true(kept[1L] %in% .ja_dropped_words(d$ja[d$id == id]), info = id)
  }
})
