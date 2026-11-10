# The template card's origin badges: tflspec's tfl_tpl_args() says where each
# argument's value comes from; the card shows one badge per field.

.badge_study <- function() {
  skip_if_not(is.function(tryCatch(getExportedValue("tflspec", "tfl_tpl_args"), error = function(e) NULL)))
  d <- tempfile("badge-study")
  dir.create(file.path(d, "spec"), recursive = TRUE)
  writeLines(c("study_id: X", "spec_version: 1", "settings:", "  rounding: sas"),
             file.path(d, "study.yml"))
  writeLines(c("content:", "  - plan_blanks:", "      where: between_groups"),
             file.path(d, "spec", "defaults.yml"))
  tflspec::tfl_read_study_spec(d)
}

test_that("demog: each of the four badges is on the right argument", {
  st <- .badge_study()
  rows <- .tpl_args_rows("demog", st, "SAF")
  expect_s3_class(rows, "data.frame")
  args <- list(by = "TRT01A", total = TRUE)
  kind <- function(a) .tpl_badge_of(a, args, rows)$kind
  # the report's own
  expect_identical(kind("by"), "own")
  expect_identical(kind("total"), "own")
  # the study standard's (and where in it)
  expect_identical(kind("rounding"), "study")
  expect_identical(.tpl_badge_of("rounding", args, rows)$key, "settings.rounding")
  expect_identical(kind("blanks"), "study")
  expect_identical(.tpl_badge_of("blanks", args, rows)$key, "plan_blanks.where")
  # the value for when the study standard has none
  expect_identical(kind("max_rows"), "fallback")
  # the template's own default (a study key it does not set, no value of its own; no key at all)
  expect_identical(kind("blanks_first"), "default")
  expect_identical(kind("total_label"), "default")
  # the report's value over the study's: still the report's
  expect_identical(.tpl_badge_of("rounding", list(rounding = "r"), rows)$kind, "own")
  # ... and it carries the study's key and value, for the note under it
  b <- .tpl_badge_of("rounding", list(rounding = "r"), rows)
  expect_identical(b$key, "settings.rounding")
  expect_identical(as.character(b$study), "sas")
  # a key the old lookup misses: given in the report, the study has plan_cells.default
  d <- tempfile("badge-cells")
  dir.create(file.path(d, "spec"), recursive = TRUE)
  writeLines(c("study_id: X", "spec_version: 1"), file.path(d, "study.yml"))
  writeLines(c("content:", "  - plan_cells:", "      default: \"{n} ({pct}%)\""), file.path(d, "spec", "defaults.yml"))
  rows2 <- .tpl_args_rows("demog", tflspec::tfl_read_study_spec(d), "SAF")
  b2 <- .tpl_badge_of("count_cell", list(count_cell = "{n}"), rows2)
  expect_identical(b2$kind, "own")
  expect_identical(b2$key, "plan_cells.default")
  expect_false(is.null(b2$study))
})

test_that("the badge is a small pill with the kind's class and text", {
  tr_en <- function(x) tr(x, "en")
  tr_ja <- function(x) tr(x, "ja")
  h <- function(b, f = tr_en, k = "table") as.character(.tpl_badge_tag(b, f, k))
  expect_match(h(list(kind = "own")), "rp-tpl-badge-own")
  expect_match(h(list(kind = "own")), "This table")
  expect_match(h(list(kind = "own"), k = "listing"), "This report")
  expect_match(h(list(kind = "study", key = "settings.style")), "Study standard (settings.style)", fixed = TRUE)
  expect_match(h(list(kind = "study", key = "x.y")), "rp-tpl-badge-study")
  expect_match(h(list(kind = "report")), "rp-tpl-badge-study")
  expect_match(h(list(kind = "report")), "This report's analysis set", fixed = TRUE)
  expect_match(h(list(kind = "fallback")), "rp-tpl-badge-fallback")
  expect_match(h(list(kind = "fallback", key = "plan_blanks.max_rows")),
               "When the study standard (plan_blanks.max_rows) has none", fixed = TRUE)
  expect_match(h(list(kind = "fallback", key = "a.b"), tr_ja), "試験標準（a.b）に無い時の値")
  expect_match(h(list(kind = "default")), "rp-tpl-badge-default")
  expect_match(h(list(kind = "default")), "Template default")
  expect_match(h(list(kind = "default"), tr_ja), "\u30c6\u30f3\u30d7\u30ec\u30fc\u30c8\u306e\u65e2\u5b9a\u5024")
  expect_match(h(list(kind = "fallback"), tr_ja), "\u8a66\u9a13\u6a19\u6e96\u306b\u7121\u3044\u6642\u306e\u5024")
  expect_match(h(list(kind = "report"), tr_ja), "\u3053\u306e\u5e33\u7968\u306e\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3")
})

test_that("tflspec cannot say: no rows, no badge (the card keeps its old text)", {
  expect_null(.tpl_args_rows("demog", list(not = "a study spec"), "SAF"))
  expect_null(.tpl_args_rows("no_such_template", NULL, NULL))
  expect_null(.tpl_badge_of("by", list(), NULL))
})

test_that("no fading of a field at a default; only the empty placeholder is light", {
  expect_false(grepl("rp-tpl-default", .tpl_css, fixed = TRUE))
  expect_true(grepl(".selectize-input .item[data-value=''] { color: #8a8f98; }", .tpl_css, fixed = TRUE))
})
