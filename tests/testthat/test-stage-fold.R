# [Collapse all] / [Expand all] at the top of each step of the report
# screen (#835): one helper for every step (R/stage_fold.R).  What the
# buttons do in a browser: test-e2e-fold.R.

test_that("the fold bar: two buttons, the same words in every step, in Japanese too", {
  for (st in c("data", "ard", "content", "page")) {
    html <- as.character(.stage_fold_bar(st, "en"))
    expect_match(html, sprintf("class=\"rp-fold-bar\" data-fold-stage=\"%s\"", st), fixed = TRUE)
    expect_match(html, "data-fold=\"close\">Collapse all</button>", fixed = TRUE)
    expect_match(html, "data-fold=\"open\">Expand all</button>", fixed = TRUE)
  }
  ja <- as.character(.stage_fold_bar("content", "ja"))
  expect_match(ja, "すべてたたむ", fixed = TRUE)
  expect_match(ja, "すべて開く", fixed = TRUE)
})

test_that("the report screen has the bar once in each step, at its top", {
  html <- as.character(app_ui("en"))
  count <- function(p) lengths(regmatches(html, gregexpr(p, html, fixed = TRUE)))
  expect_identical(count("class=\"rp-fold-bar\""), 4L)
  for (st in c("data", "ard", "content", "page")) {
    # the step is the bar's scope; the bar its first child
    expect_identical(count(sprintf("data-fold-stage=\"%s\"", st)), 2L)
    expect_match(html, sprintf(paste0("rp-fold-scope\" data-fold-stage=\"%s\">\\s*",
                                      "<div class=\"rp-fold-bar\" data-fold-stage=\"%s\">"), st, st))
  }
  expect_identical(count(">Collapse all</button>"), 4L)
  expect_identical(count(">Expand all</button>"), 4L)
  # its style and script are on the page once
  expect_identical(count(".rp-fold-closed > :not(:first-child)"), 1L)
  expect_identical(count("$(document).on('click', '.rp-fold-bar [data-fold]'"), 1L)
  ja <- as.character(app_ui("ja"))
  expect_identical(lengths(regmatches(ja, gregexpr("すべてたたむ", ja, fixed = TRUE))), 4L)
})

test_that("a collapsed card is its title line: a builder card's head, another card's title", {
  # a builder card closed: no rp-card-open, its body hidden by the cards' style
  card <- as.character(builder_card("c1", "rtf_title", label = "Titles", open = FALSE,
                                    body = shiny::p("the fields")))
  expect_false(grepl("rp-card-open", card, fixed = TRUE))
  expect_match(card, "class=\"rp-card-head\"", fixed = TRUE)
  expect_match(.builder_cards_css, ".rp-card:not(.rp-card-open) .rp-card-body { display: none; }",
               fixed = TRUE)
  # another card folded: all but its title (a card header, an h6) hidden,
  # its title line as the builder card's head (padding, the closed arrow)
  expect_match(.stage_fold_css, ".rp-fold-closed > :not(:first-child) { display: none !important; }",
               fixed = TRUE)
  expect_match(.stage_fold_css, "padding: .35rem .6rem", fixed = TRUE)
  expect_match(.builder_cards_css, "padding: .35rem .6rem", fixed = TRUE)
  expect_match(.stage_fold_css, "content: '\\25b8'", fixed = TRUE)
  expect_match(.builder_cards_css, "content: '\\25b8'", fixed = TRUE)
  # the script: only hides (no Shiny input), and leaves open a card with an
  # error or a value just changed
  expect_false(grepl("Shiny.setInputValue", .stage_fold_js, fixed = TRUE))
  expect_match(.stage_fold_js, "rp-card-error", fixed = TRUE)
  expect_match(.stage_fold_js, "data-fold-changed", fixed = TRUE)
})
