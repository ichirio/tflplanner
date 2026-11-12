# The four-steps report screen (provisional): its model from the YAML in the
# target shape, and the screen drawn from it

fs_study_dir <- function() testthat::test_path("fixtures", "foursteps")
fs_model_of <- function(id) {
  st <- .fs_study(fs_study_dir())
  .fs_model(.fs_reports(fs_study_dir())[[id]], st)
}
fs_html <- function(m, lang = "en") as.character(.fs_screen_ui(m, function(x) tr(x, lang)))

test_that("the screen is off unless asked for", {
  old <- options(tflplanner.foursteps = NULL)
  on.exit(options(old))
  if (!nzchar(Sys.getenv("TFLPLANNER_FOURSTEPS"))) expect_false(.fs_on())
  options(tflplanner.foursteps = TRUE)
  expect_true(.fs_on())
})

test_that("a template report: common, the four steps' values with their origins", {
  m <- fs_model_of("T-14-3-1")
  expect_identical(m$template, "ae_soc_pt")
  expect_identical(m$common$population$value, "SAF")
  expect_identical(m$common$population_label$value, "Safety Analysis Set")
  expect_identical(m$common$population_label$origin, "study")
  d <- stats::setNames(m$steps$data$rows, vapply(m$steps$data$rows, function(r) r$name, ""))
  expect_identical(d$subset$origin, "own")
  expect_identical(d$subset$value, "SEX == \"F\"")
  expect_identical(d$dataset$origin, "default")
  expect_identical(vapply(m$steps$data$datasets, function(x) x$id, ""), c("adsl_sub", "adae_sub"))
  pb <- stats::setNames(m$steps$content$pagebreak, vapply(m$steps$content$pagebreak, function(r) r$name, ""))
  expect_identical(pb$max_rows$value, "40")
  expect_identical(pb$max_rows$origin, "own")
  expect_identical(pb$split$value, "group_force")
  # the demographics split is fixed by the template; max_rows follows the configuration
  m2 <- fs_model_of("T-14-1-1")
  pb2 <- stats::setNames(m2$steps$content$pagebreak, vapply(m2$steps$content$pagebreak, function(r) r$name, ""))
  expect_identical(pb2$split$origin, "fixed")
  expect_identical(m2$style$name, "tfl_standard")
  expect_identical(m2$style$chosen_by, "study")
})

test_that("a band lists the resolved lines in print order with their sources", {
  m <- fs_model_of("T-14-3-1")
  fn <- m$steps$page$bands$footnotes$items
  # the template's line 1 stays, its line 2 is taken out, the report adds 3
  expect_identical(vapply(fn, function(i) i$src, ""), c("template", "own"))
  expect_identical(vapply(fn, function(i) i$num, 0L), c(1L, 3L))
  ft <- m$steps$page$bands$footer$items
  expect_identical(vapply(ft, function(i) i$src, ""), "config")
  hd <- m$steps$page$bands$header$items
  expect_length(hd, 5L)
  expect_true(all(vapply(hd, function(i) i$src, "") == "config"))
  # the titles are the report's own list
  expect_identical(vapply(m$steps$page$bands$titles$items, function(i) i$src, ""), c("own", "own"))
  # a setting the report overrides says so; the rest is the configuration's
  s <- stats::setNames(m$steps$page$settings, vapply(m$steps$page$settings, function(r) r$name, ""))
  expect_identical(s[["rtf_page: orientation"]]$origin, "own")
  expect_identical(s[["rtf_page: margin_left_in"]]$origin, "config")
  expect_identical(s[["rtf_page: margin_left_in"]]$config, "tfl_small")
})

test_that("a code SPEC report: its dataset definitions, analyses and verbs; the same bands", {
  m <- fs_model_of("T-14-3-2")
  expect_null(m$template)
  expect_identical(m$steps$data$ids, c("adsl_sub", "adae_sub"))
  expect_identical(m$steps$data$id_roles, c("analysis set", "records"))
  expect_identical(m$style$name, "acme_wide")
  expect_identical(m$configs$acme_wide$source, "study")
  ft <- m$steps$page$bands$footer$items
  expect_identical(vapply(ft, function(i) i$src, ""), c("own", "config"))
  hd <- m$steps$page$bands$header$items
  expect_length(hd, 2L)
  expect_length(m$steps$content$verbs, 2L)
})

test_that("the screen draws the common block and the four steps", {
  for (id in c("T-14-1-1", "T-14-3-1", "T-14-3-2")) {
    h <- fs_html(fs_model_of(id))
    expect_match(h, "Common", fixed = TRUE)
    for (s in c("1 Analysis data", "2 ARD", "3 Content", "4 Page and output")) expect_match(h, s, fixed = TRUE)
    expect_match(h, "Page configuration", fixed = TRUE)
    expect_match(h, "adsl_sub (analysis set)", fixed = TRUE)
  }
  h <- fs_html(fs_model_of("T-14-3-1"))
  expect_match(h, "Page configuration tfl_small", fixed = TRUE)
  expect_match(h, "Page breaks", fixed = TRUE)
  expect_match(h, "Back to the template", fixed = TRUE)
  expect_false(grepl("Back to the template", fs_html(fs_model_of("T-14-3-2")), fixed = TRUE))
})

test_that("the screen speaks Japanese", {
  h <- fs_html(fs_model_of("T-14-3-1"), "ja")
  expect_match(h, "\u30da\u30fc\u30b8\u306e\u8a2d\u5b9a", fixed = TRUE)
  expect_match(h, "\u6539\u30da\u30fc\u30b8\u306e\u65b9\u6cd5", fixed = TRUE)
  expect_false(grepl("Page configuration", h, fixed = TRUE))
})
