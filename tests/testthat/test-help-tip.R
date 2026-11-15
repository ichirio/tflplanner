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
  for (x in c("Reports are made in this order", "Your own ARD functions",
              "Right-click to add or delete rows", "An official run makes a run folder")) {
    # in an (i)'s help (its aria-label), not in a paragraph of its own
    expect_match(html, paste0('aria-label="', x), fixed = TRUE, info = x)
    expect_false(grepl(paste0("<p class=\"[^\"]*text-muted[^\"]*\">", x), html), info = x)
  }
})

test_that("a part's heading carries its help; a click or a tap pins the (i) open", {
  h <- as.character(pane_head("Datasets", "Help text."))
  expect_match(h, "<h6", fixed = TRUE)
  expect_match(h, 'aria-label="Help text."', fixed = TRUE)
  html <- as.character(app_ui())
  # no "About this" lines left: each part has a heading with its (i)
  expect_false(grepl(">About this<", html, fixed = TRUE))
  # the sheet note is in a part's (i) on the Data tab, not a line of its own
  expect_match(html, 'aria-label="The data files of the folder', fixed = TRUE)
  # the pinning script is on the page, for 2-x's (i) too
  expect_match(html, "rp-pinned", fixed = TRUE)
  expect_match(html, ".rp-tip, .rp-hint", fixed = TRUE)
})

test_that("a tip's text is text: a <placeholder> in it shows as it is", {
  h <- as.character(help_tip("runs/<date>_<time>/ and <b>x</b>"))
  expect_match(h, "runs/&lt;date&gt;_&lt;time&gt;/", fixed = TRUE)
  expect_match(h, "&lt;b&gt;x&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<b>x</b>", h, fixed = TRUE))
  w <- as.character(with_tip("Heading", "(adsl_<set>)"))
  expect_match(w, "adsl_&lt;set&gt;", fixed = TRUE)
  # the app's own tip with one
  html <- as.character(app_ui("ja"))
  expect_match(html, "runs/&lt;", fixed = TRUE)
})
