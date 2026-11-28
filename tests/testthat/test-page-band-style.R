# The titles' and footnotes' style on the page screen: rtfreporter's
# arguments, written into the report's YAML (#472)

band_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(sample_study_copy(home))$planner
}

test_that("rtf_titles / rtf_footnotes have their style's fields, the size first", {
  for (fn in c("rtf_titles", "rtf_footnotes")) {
    args <- vapply(.pgs_fields[[fn]], `[[`, "", "arg")
    expect_identical(args, c("font_size_half_points", "row_height_twips", "markup", "font", "align"))
  }
  # (a figure's alignment is its content's tfl_image: tflspec#874)
  expect_null(.pgs_fields$rtf_figures)
})

test_that("the titles' card: the size shown, the rest folded until the page says them", {
  skip_on_cran()
  p <- band_sample()
  h <- as.character(.page_form_ui(p, "T-14-3-1", identity))
  ti <- substring(h, regexpr("(rtf_titles())", h, fixed = TRUE))
  expect_match(ti, "data-fn=\"rtf_titles\" data-arg=\"font_size_half_points\"", fixed = TRUE)
  expect_match(ti, "More settings (4)", fixed = TRUE)
  q <- .pgs_set_arg(p, "T-14-3-1", "rtf_titles", "align", .pgs_value("left", "choice"))
  h <- as.character(.page_form_ui(q, "T-14-3-1", identity))
  ti <- substring(h, regexpr("(rtf_titles())", h, fixed = TRUE))
  expect_match(ti, "More settings (3)", fixed = TRUE)
  expect_lt(regexpr("data-fn=\"rtf_titles\" data-arg=\"align\"", ti, fixed = TRUE),
            regexpr("More settings", ti, fixed = TRUE))
})

test_that("the titles' card in Japanese: the content cards' fold word, the size, the part named", {
  skip_on_cran()
  p <- band_sample()
  ja <- function(x) tr(x, "ja")
  h <- as.character(.page_form_ui(p, "T-14-3-1", ja))
  ti <- substring(h, regexpr("(rtf_titles())", h, fixed = TRUE))
  # (the fold: the content cards' fold word, "More settings")
  expect_match(ti, "\u8a73\u7d30\u8a2d\u5b9a (4)", fixed = TRUE)
  expect_identical(ja(.spec_more_label), "\u8a73\u7d30\u8a2d\u5b9a")
  for (en in c("Font size (pt)", "+ Add a title line for this report only",
               "(Choose a value to insert: report number, page number, ...)")) {
    expect_false(identical(ja(en), en), info = en)
    expect_match(ti, ja(en), fixed = TRUE, info = en)
  }
  expect_false(grepl("Not in the code", ti, fixed = TRUE))
})

test_that("an argument set is in the report's YAML (tflspec writes it into the program)", {
  skip_on_cran()
  p <- band_sample()
  q <- .pgs_set_arg(p, "T-14-3-1", "rtf_footnotes", "markup", .pgs_value("none", "choice"))
  q <- .pgs_set_arg(q, "T-14-3-1", "rtf_footnotes", "row_height_twips", .pgs_value("300", "number"))
  sp <- .pgs_spec(q)
  pg <- sp$outputs[["T-14-3-1"]]$page$rtf_footnotes
  expect_identical(pg$markup, "none")
  expect_identical(pg$row_height_twips, 300L)
})
