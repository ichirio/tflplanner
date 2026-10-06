# A heading's help as an (i) with a tooltip, not a paragraph on the page

test_that("the (i) is focusable, named by its help, and carries a tooltip", {
  h <- as.character(help_tip("Some long help."))
  expect_match(h, 'tabindex="0"', fixed = TRUE)
  expect_match(h, 'aria-label="Some long help."', fixed = TRUE)
  expect_match(h, "bslib-tooltip", fixed = TRUE)
  expect_null(help_tip(""))
  w <- as.character(with_tip("Heading", "Help"))
  expect_match(w, "Heading", fixed = TRUE)
  expect_match(w, "rp-tip", fixed = TRUE)
  expect_match(as.character(about_tip("Help", "ja")), "\u3053\u306e\u753b\u9762\u306b\u3064\u3044\u3066",
               fixed = TRUE)
})

test_that("the long explanations are help now, not paragraphs", {
  html <- as.character(app_ui())
  for (x in c("Reports are made in this order", "ARD functions of one",
              "Right-click to add or delete rows", "An official run makes a batch folder")) {
    # in an (i)'s help (its aria-label), not in a paragraph of its own
    expect_match(html, paste0('aria-label="', x), fixed = TRUE, info = x)
    expect_false(grepl(paste0("<p class=\"[^\"]*text-muted[^\"]*\">", x), html), info = x)
  }
})
