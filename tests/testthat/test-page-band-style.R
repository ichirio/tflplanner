# The titles' and footnotes' style and the figure's alignment on the page
# screen: rtfreporter's arguments, written into the report's YAML (#472)

band_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))$planner
}

test_that("rtf_titles / rtf_footnotes have their style's fields, the size first", {
  for (fn in c("rtf_titles", "rtf_footnotes")) {
    args <- vapply(.pgs_fields[[fn]], `[[`, "", "arg")
    expect_identical(args, c("font_size_half_points", "row_height_twips", "markup", "font", "align"))
  }
  expect_true("align" %in% vapply(.pgs_fields$rtf_figures, `[[`, "", "arg"))
})

test_that("the titles' card: the size shown, the rest folded until the page says them", {
  skip_on_cran()
  p <- band_sample()
  h <- as.character(.page_form_ui(p, "T-14-3-1", identity))
  ti <- substring(h, regexpr("(rtf_titles())", h, fixed = TRUE))
  expect_match(ti, "data-fn=\"rtf_titles\" data-arg=\"font_size_half_points\"", fixed = TRUE)
  expect_match(ti, "Not in the code (4)", fixed = TRUE)
  q <- .pgs_set_arg(p, "T-14-3-1", "rtf_titles", "align", .pgs_value("left", "choice"))
  h <- as.character(.page_form_ui(q, "T-14-3-1", identity))
  ti <- substring(h, regexpr("(rtf_titles())", h, fixed = TRUE))
  expect_match(ti, "Not in the code (3)", fixed = TRUE)
  expect_lt(regexpr("data-fn=\"rtf_titles\" data-arg=\"align\"", ti, fixed = TRUE),
            regexpr("Not in the code", ti, fixed = TRUE))
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
